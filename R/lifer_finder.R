EBIRD_API_BASE <- "https://api.ebird.org/v2"
LOOKBACK_DAYS <- 3L
MAX_CANDIDATES <- 40L
ORIGIN_LAT <- 45.6124
ORIGIN_LNG <- -122.6784

SEARCH_ZONES <- data.frame(
  zone = c(
    "Portland / Vancouver",
    "Southwest Washington",
    "Columbia Gorge",
    "Willamette Valley",
    "Northern Coast"
  ),
  lat = c(45.6124, 46.1382, 45.7054, 44.9429, 45.4562),
  lng = c(-122.6784, -122.9382, -121.5215, -123.0351, -123.8440),
  radius_km = c(50, 50, 50, 50, 50),
  stringsAsFactors = FALSE
)

empty_observations <- function() {
  data.frame(
    speciesCode = character(), comName = character(), sciName = character(),
    category = character(),
    locId = character(), locName = character(), obsDt = character(),
    lat = numeric(), lng = numeric(), howMany = numeric(),
    obsValid = logical(), obsReviewed = logical(), locationPrivate = logical(),
    subId = character(), stringsAsFactors = FALSE
  )
}

empty_hotspots <- function() {
  data.frame(
    locId = character(), locName = character(), lat = numeric(), lng = numeric(),
    latestObsDt = character(), numSpeciesAllTime = numeric(), zone = character(),
    stringsAsFactors = FALSE
  )
}

normalize_text <- function(x) {
  tolower(trimws(gsub("[[:space:]]+", " ", as.character(x))))
}

read_life_list <- function(path) {
  if (!file.exists(path)) stop("Life-list file not found: ", path)
  x <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  needed <- c("Category", "Common Name", "Scientific Name", "Countable")
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop("The eBird export is missing: ", paste(missing, collapse = ", "))

  countable <- suppressWarnings(as.integer(x[["Countable"]]))
  keep <- normalize_text(x[["Category"]]) == "species" & !is.na(countable) & countable == 1L
  out <- unique(data.frame(
    common_name = trimws(x[["Common Name"]][keep]),
    scientific_name = trimws(x[["Scientific Name"]][keep]),
    common_key = normalize_text(x[["Common Name"]][keep]),
    scientific_key = normalize_text(x[["Scientific Name"]][keep]),
    stringsAsFactors = FALSE
  ))
  rownames(out) <- NULL
  out
}

life_list_keys <- function(life_list) {
  list(
    scientific = unique(life_list$scientific_key[nzchar(life_list$scientific_key)]),
    common = unique(life_list$common_key[nzchar(life_list$common_key)])
  )
}

mark_potential_lifers <- function(observations, life_list) {
  if (!nrow(observations)) {
    observations$is_lifer <- logical()
    return(observations)
  }
  keys <- life_list_keys(life_list)
  sci <- normalize_text(observations$sciName)
  common <- normalize_text(observations$comName)
  observations$is_lifer <- !(sci %in% keys$scientific | common %in% keys$common)
  observations
}

parse_observation_time <- function(x, tz = "America/Los_Angeles") {
  x <- as.character(x)
  out <- as.POSIXct(rep(NA_character_, length(x)), tz = tz)
  for (fmt in c("%Y-%m-%d %H:%M", "%Y-%m-%d")) {
    missing <- is.na(out)
    out[missing] <- as.POSIXct(x[missing], format = fmt, tz = tz)
  }
  out
}

haversine_miles <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  3958.7613 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

api_cache <- new.env(parent = emptyenv())

cache_get <- function(key, ttl_seconds = 1200) {
  if (!exists(key, envir = api_cache, inherits = FALSE)) return(NULL)
  item <- get(key, envir = api_cache, inherits = FALSE)
  if (difftime(Sys.time(), item$stored_at, units = "secs") > ttl_seconds) {
    rm(list = key, envir = api_cache)
    return(NULL)
  }
  item$value
}

cache_set <- function(key, value) {
  assign(key, list(stored_at = Sys.time(), value = value), envir = api_cache)
  value
}

ebird_get <- function(path, query = list(), api_key = Sys.getenv("EBIRD_API_KEY"), use_cache = TRUE) {
  if (!nzchar(api_key)) stop("EBIRD_API_KEY is missing from .Renviron.")
  query <- query[!vapply(query, is.null, logical(1))]
  cache_key <- paste(path, paste(names(query), unlist(query), sep = "=", collapse = "&"), sep = "?")
  if (use_cache) {
    cached <- cache_get(cache_key)
    if (!is.null(cached)) return(cached)
  }

  request <- httr2::request(paste0(EBIRD_API_BASE, path)) |>
    httr2::req_headers(`X-eBirdApiToken` = api_key, `User-Agent` = "Portland-Lifer-Finder/1.0") |>
    httr2::req_url_query(!!!query) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 1)

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(e) stop("eBird request failed: ", conditionMessage(e), call. = FALSE)
  )
  result <- httr2::resp_body_json(response, simplifyVector = TRUE)
  if (use_cache) cache_set(cache_key, result) else result
}

standardize_frame <- function(x, template) {
  if (is.null(x) || !length(x)) return(template[0, , drop = FALSE])
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (nm in setdiff(names(template), names(x))) x[[nm]] <- NA
  x[, names(template), drop = FALSE]
}

fetch_zone_hotspots <- function(zone_row, api_key) {
  result <- ebird_get(
    "/ref/hotspot/geo",
    list(lat = zone_row$lat, lng = zone_row$lng, dist = zone_row$radius_km,
         back = LOOKBACK_DAYS, fmt = "json"),
    api_key
  )
  out <- standardize_frame(result, empty_hotspots())
  out$zone <- rep(zone_row$zone, nrow(out))
  out
}

fetch_zone_observations <- function(zone_row, api_key, notable = FALSE, back_days = LOOKBACK_DAYS) {
  path <- if (notable) "/data/obs/geo/recent/notable" else "/data/obs/geo/recent"
  result <- ebird_get(
    path,
    list(lat = zone_row$lat, lng = zone_row$lng, dist = zone_row$radius_km,
         back = back_days, hotspot = "true", detail = "full",
         includeProvisional = "true", maxResults = 10000),
    api_key
  )
  out <- standardize_frame(result, empty_observations())
  out$zone <- rep(zone_row$zone, nrow(out))
  out
}

fetch_zone_species_observations <- function(zone_row, species_code, api_key, back_days = LOOKBACK_DAYS) {
  result <- ebird_get(
    paste0("/data/obs/geo/recent/", utils::URLencode(species_code, reserved = TRUE)),
    list(
      lat = zone_row$lat,
      lng = zone_row$lng,
      dist = zone_row$radius_km,
      back = back_days,
      hotspot = "true",
      detail = "full",
      includeProvisional = "true",
      maxResults = 10000
    ),
    api_key
  )
  standardize_frame(result, empty_observations())
}

fetch_hotspot_observations <- function(loc_id, api_key, back_days = LOOKBACK_DAYS) {
  result <- ebird_get(
    paste0("/data/obs/", utils::URLencode(loc_id, reserved = TRUE), "/recent"),
    list(back = back_days, detail = "full", includeProvisional = "true", maxResults = 10000),
    api_key
  )
  standardize_frame(result, empty_observations())
}

choose_candidates <- function(hotspots, zone_observations, life_list, max_candidates = MAX_CANDIDATES) {
  if (!nrow(hotspots)) return(hotspots)
  hotspots <- hotspots[!is.na(hotspots$locId) & nzchar(hotspots$locId), , drop = FALSE]
  hotspots$numSpeciesAllTime <- suppressWarnings(as.numeric(hotspots$numSpeciesAllTime))
  hotspots$latest_time <- parse_observation_time(hotspots$latestObsDt)
  hotspots <- hotspots[order(
    hotspots$locId,
    -ifelse(is.na(hotspots$numSpeciesAllTime), -1, hotspots$numSpeciesAllTime)
  ), , drop = FALSE]
  hotspots <- hotspots[!duplicated(hotspots$locId), , drop = FALSE]

  zone_observations <- mark_potential_lifers(zone_observations, life_list)
  lifer_hits <- zone_observations[zone_observations$is_lifer & nzchar(zone_observations$locId), , drop = FALSE]
  hit_counts <- if (nrow(lifer_hits)) {
    aggregate(speciesCode ~ locId, lifer_hits, function(x) length(unique(x)))
  } else {
    data.frame(locId = character(), speciesCode = integer(), stringsAsFactors = FALSE)
  }
  names(hit_counts)[names(hit_counts) == "speciesCode"] <- "preliminary_lifers"
  hotspots <- merge(hotspots, hit_counts, by = "locId", all.x = TRUE, sort = FALSE)
  hotspots$preliminary_lifers[is.na(hotspots$preliminary_lifers)] <- 0L

  zone_top <- do.call(rbind, lapply(split(hotspots, hotspots$zone), function(z) {
    z <- z[order(-z$preliminary_lifers, -z$numSpeciesAllTime, -as.numeric(z$latest_time)), , drop = FALSE]
    utils::head(z, 8)
  }))
  zone_top <- zone_top[!duplicated(zone_top$locId), , drop = FALSE]
  zone_top <- zone_top[order(
    -zone_top$preliminary_lifers,
    -zone_top$numSpeciesAllTime,
    -as.numeric(zone_top$latest_time)
  ), , drop = FALSE]
  utils::head(zone_top, max_candidates)
}

make_observation_key <- function(loc_id, species_code) paste(loc_id, species_code, sep = "|")

summarize_hotspot <- function(observations, hotspot_row, life_list, notable_keys = character()) {
  observations <- mark_potential_lifers(observations, life_list)
  observations$obs_time <- parse_observation_time(observations$obsDt)
  observations$is_notable <- make_observation_key(observations$locId, observations$speciesCode) %in% notable_keys
  observations <- observations[order(-as.numeric(observations$obs_time)), , drop = FALSE]
  observations <- observations[!duplicated(observations$speciesCode), , drop = FALSE]
  lifers <- observations[observations$is_lifer, , drop = FALSE]

  latest <- if (nrow(observations) && any(!is.na(observations$obs_time))) {
    max(observations$obs_time, na.rm = TRUE)
  } else {
    as.POSIXct(NA)
  }
  lat <- suppressWarnings(as.numeric(hotspot_row$lat[1]))
  lng <- suppressWarnings(as.numeric(hotspot_row$lng[1]))
  summary <- data.frame(
    locId = hotspot_row$locId[1], hotspot = hotspot_row$locName[1], zone = hotspot_row$zone[1],
    potential_lifers = nrow(lifers),
    notable_lifers = if (nrow(lifers)) sum(lifers$is_notable, na.rm = TRUE) else 0L,
    recent_species = nrow(observations), latest_report = latest, lat = lat, lng = lng,
    approx_miles = if (!is.na(lat) && !is.na(lng)) haversine_miles(ORIGIN_LAT, ORIGIN_LNG, lat, lng) else NA_real_,
    stringsAsFactors = FALSE
  )
  list(summary = summary, observations = observations, lifers = lifers)
}

rank_hotspots <- function(summaries) {
  if (!nrow(summaries)) return(summaries)
  summaries <- summaries[order(
    -summaries$potential_lifers, -summaries$notable_lifers,
    -as.numeric(summaries$latest_report), -summaries$recent_species,
    summaries$approx_miles
  ), , drop = FALSE]
  summaries$rank <- seq_len(nrow(summaries))
  summaries <- summaries[, c("rank", setdiff(names(summaries), "rank")), drop = FALSE]
  rownames(summaries) <- NULL
  summaries
}

empty_species_locations <- function() {
  data.frame(
    speciesCode = character(), common_name = character(), scientific_name = character(),
    locId = character(), hotspot = character(), zone = character(),
    obs_time = as.POSIXct(character()), how_many = numeric(), is_notable = logical(),
    lat = numeric(), lng = numeric(), approx_miles = numeric(),
    stringsAsFactors = FALSE
  )
}

build_species_locations <- function(details, ranking) {
  if (!length(details) || !nrow(ranking)) return(empty_species_locations())
  rows <- lapply(names(details), function(loc_id) {
    entry <- details[[loc_id]]
    if (is.null(entry) || is.null(entry$lifers)) return(NULL)
    lifers <- entry$lifers
    if (!nrow(lifers)) return(NULL)
    site <- ranking[ranking$locId == loc_id, , drop = FALSE]
    if (!nrow(site)) return(NULL)
    data.frame(
      speciesCode = lifers$speciesCode,
      common_name = lifers$comName,
      scientific_name = lifers$sciName,
      locId = loc_id,
      hotspot = site$hotspot[1],
      zone = site$zone[1],
      obs_time = lifers$obs_time,
      how_many = lifers$howMany,
      is_notable = lifers$is_notable,
      lat = site$lat[1],
      lng = site$lng[1],
      approx_miles = site$approx_miles[1],
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(empty_species_locations())
  out <- do.call(rbind, rows)
  out <- out[order(out$speciesCode, -as.numeric(out$obs_time)), , drop = FALSE]
  out <- out[!duplicated(make_observation_key(out$locId, out$speciesCode)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

summarize_species <- function(species_locations) {
  if (!nrow(species_locations)) {
    return(data.frame(
      speciesCode = character(), common_name = character(), scientific_name = character(),
      reporting_hotspots = integer(), latest_report = as.POSIXct(character()),
      notable = logical(), closest_hotspot = character(), nearest_miles = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  pieces <- split(species_locations, species_locations$speciesCode)
  rows <- lapply(pieces, function(x) {
    x <- x[order(x$approx_miles, -as.numeric(x$obs_time)), , drop = FALSE]
    data.frame(
      speciesCode = x$speciesCode[1],
      common_name = x$common_name[1],
      scientific_name = x$scientific_name[1],
      reporting_hotspots = length(unique(x$locId)),
      latest_report = max(x$obs_time, na.rm = TRUE),
      notable = any(x$is_notable, na.rm = TRUE),
      closest_hotspot = x$hotspot[1],
      nearest_miles = x$approx_miles[1],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(
    -as.numeric(out$latest_report), -out$notable,
    -out$reporting_hotspots, out$nearest_miles, out$common_name
  ), , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  out <- out[, c("rank", setdiff(names(out), "rank")), drop = FALSE]
  rownames(out) <- NULL
  out
}

build_area_lifer_species <- function(observations, life_list) {
  observations <- mark_potential_lifers(observations, life_list)
  observations <- observations[
    observations$is_lifer & !is.na(observations$speciesCode) & nzchar(observations$speciesCode),
    , drop = FALSE
  ]
  if (!nrow(observations)) {
    return(data.frame(
      rank = integer(), speciesCode = character(), common_name = character(),
      scientific_name = character(), latest_report = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }
  observations$obs_time <- parse_observation_time(observations$obsDt)
  pieces <- split(observations, observations$speciesCode)
  rows <- lapply(pieces, function(x) {
    latest_index <- which.max(as.numeric(x$obs_time))
    data.frame(
      speciesCode = x$speciesCode[latest_index],
      common_name = x$comName[latest_index],
      scientific_name = x$sciName[latest_index],
      latest_report = x$obs_time[latest_index],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(-as.numeric(out$latest_report), out$common_name), , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  out <- out[, c("rank", setdiff(names(out), "rank")), drop = FALSE]
  rownames(out) <- NULL
  out
}

build_hotspot_details <- function(observations) {
  if (!nrow(observations)) {
    return(data.frame(
      locId = character(), hotspot = character(), obs_time = as.POSIXct(character()),
      how_many = numeric(), lat = numeric(), lng = numeric(), approx_miles = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  observations <- observations[
    !is.na(observations$locId) & nzchar(observations$locId) &
      !(observations$locationPrivate %in% TRUE),
    , drop = FALSE
  ]
  if (!nrow(observations)) return(build_hotspot_details(empty_observations()))
  observations$obs_time <- parse_observation_time(observations$obsDt)
  observations <- observations[order(-as.numeric(observations$obs_time)), , drop = FALSE]
  observations <- observations[!duplicated(observations$locId), , drop = FALSE]
  out <- data.frame(
    locId = observations$locId,
    hotspot = observations$locName,
    obs_time = observations$obs_time,
    how_many = observations$howMany,
    lat = suppressWarnings(as.numeric(observations$lat)),
    lng = suppressWarnings(as.numeric(observations$lng)),
    approx_miles = haversine_miles(
      ORIGIN_LAT, ORIGIN_LNG,
      suppressWarnings(as.numeric(observations$lat)),
      suppressWarnings(as.numeric(observations$lng))
    ),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

build_other_lifers <- function(observations, life_list, exclude_species_code = NULL) {
  if (!nrow(observations)) {
    return(data.frame(
      speciesCode = character(), common_name = character(),
      latest_report = as.POSIXct(character()), stringsAsFactors = FALSE
    ))
  }
  if ("category" %in% names(observations)) {
    observations <- observations[
      is.na(observations$category) | normalize_text(observations$category) == "species",
      , drop = FALSE
    ]
  }
  observations <- mark_potential_lifers(observations, life_list)
  observations <- observations[
    observations$is_lifer & !is.na(observations$speciesCode) &
      nzchar(observations$speciesCode),
    , drop = FALSE
  ]
  if (!is.null(exclude_species_code) && nzchar(exclude_species_code)) {
    observations <- observations[observations$speciesCode != exclude_species_code, , drop = FALSE]
  }
  if (!nrow(observations)) return(build_other_lifers(empty_observations(), life_list))
  observations$obs_time <- parse_observation_time(observations$obsDt)
  observations <- observations[order(-as.numeric(observations$obs_time)), , drop = FALSE]
  observations <- observations[!duplicated(observations$speciesCode), , drop = FALSE]
  out <- data.frame(
    speciesCode = observations$speciesCode,
    common_name = observations$comName,
    latest_report = observations$obs_time,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

count_potential_lifers <- function(observations, life_list) {
  nrow(build_other_lifers(observations, life_list, exclude_species_code = NULL))
}
