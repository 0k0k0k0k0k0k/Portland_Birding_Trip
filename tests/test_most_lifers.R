source("app.R")

make_observations <- function(codes, loc_id) {
  if (!length(codes)) return(empty_observations())
  data.frame(
    speciesCode = codes,
    comName = paste("Test lifer", codes),
    sciName = paste("Avis", codes),
    category = "species",
    locId = loc_id,
    locName = paste("Hotspot", loc_id),
    obsDt = "2026-06-20 08:00",
    lat = 45.6,
    lng = -122.7,
    howMany = 1,
    obsValid = TRUE,
    obsReviewed = FALSE,
    locationPrivate = FALSE,
    subId = paste0("S", seq_along(codes)),
    stringsAsFactors = FALSE
  )
}

spots <- data.frame(
  locId = c("A", "B", "C", "FAIL"),
  hotspot = c("Hotspot A", "Hotspot B", "Hotspot C", "Broken hotspot"),
  obs_time = as.POSIXct(c(
    "2026-06-20 08:00", "2026-06-19 08:00",
    "2026-06-18 08:00", "2026-06-20 09:00"
  ), tz = "America/Los_Angeles"),
  how_many = 1,
  lat = 45.6,
  lng = -122.7,
  approx_miles = c(4, 2, 1, 3),
  stringsAsFactors = FALSE
)

observations_by_hotspot <- list(
  A = make_observations(c("selected", "a2"), "A"),
  B = make_observations(c("selected", "b2"), "B"),
  C = empty_observations(),
  D = make_observations("d1", "D")
)

original_fetch <- fetch_hotspot_observations
on.exit(assign("fetch_hotspot_observations", original_fetch, envir = .GlobalEnv), add = TRUE)
calls <- character()
assign("fetch_hotspot_observations", function(loc_id, api_key, back_days) {
  calls <<- c(calls, loc_id)
  if (identical(loc_id, "FAIL")) stop("mock failure")
  observations_by_hotspot[[loc_id]]
}, envir = .GlobalEnv)

original_zone_species <- fetch_zone_species_observations
on.exit(assign("fetch_zone_species_observations", original_zone_species, envir = .GlobalEnv), add = TRUE)
assign("fetch_zone_species_observations", function(zone_row, species_code, api_key, back_days) {
  rbind(
    make_observations("selected", "A"),
    make_observations("selected", "B")
  )
}, envir = .GlobalEnv)

old_key <- Sys.getenv("EBIRD_API_KEY", unset = NA_character_)
on.exit({
  if (is.na(old_key)) Sys.unsetenv("EBIRD_API_KEY") else Sys.setenv(EBIRD_API_KEY = old_key)
}, add = TRUE)
Sys.setenv(EBIRD_API_KEY = "test-key")

shiny::testServer(server, {
  session$setInputs(lookback = "3")
  load_species_hotspots("selected")
  stopifnot(identical(unique(calls), c("A", "B")))
  stopifnot(all(c("A|3", "B|3") %in% names(hotspot_observation_cache())))
  calls <<- character()
  hotspot_observation_cache(list())

  hotspot_results(spots)
  session$setInputs(shortlist_toggle = "A")
  stopifnot(identical(shortlist()$locId, "A"))
  hotspot_observation_cache(list("A|3" = observations_by_hotspot$A))

  load_all_hotspot_lifers()
  stopifnot(identical(calls, c("B", "C", "FAIL")))
  stopifnot(all(c("A|3", "B|3", "C|3") %in% names(hotspot_observation_cache())))
  stopifnot("FAIL|3" %in% hotspot_count_failures())

  session$setInputs(hotspot_sort = "lifers")
  ranked <- sorted_hotspots()
  stopifnot(identical(ranked$locId, c("B", "A", "C", "FAIL")))
  stopifnot(identical(ranked$lifer_count, c(2L, 2L, 0L, NA_integer_)))
  load_all_hotspot_lifers()
  stopifnot(identical(calls, c("B", "C", "FAIL")))

  session$setInputs(hotspot_sort = "distance")
  stopifnot(identical(sorted_hotspots()$locId, c("C", "B", "FAIL", "A")))
  session$setInputs(hotspot_sort = "recent")
  stopifnot(identical(sorted_hotspots()$locId, c("FAIL", "A", "B", "C")))

  extra <- rbind(spots[spots$locId == "A", ], transform(spots[1, ], locId = "D", hotspot = "Hotspot D"))
  hotspot_results(extra)
  session$setInputs(shortlist_toggle = "D")
  stopifnot(identical(shortlist()$locId, c("A", "D")))
  stopifnot(identical(current_report_hotspots()$locId, c("A", "D")))
  load_all_hotspot_lifers()
  stopifnot(tail(calls, 1) == "D")

  hotspot_results(spots[spots$locId == "A", , drop = FALSE])
  stopifnot(identical(current_report_hotspots()$locId, "A"))

  session$setInputs(lookback = "2")
  stopifnot(length(hotspot_observation_cache()) == 0L)
  stopifnot(length(hotspot_count_failures()) == 0L)
  stopifnot(identical(shortlist()$locId, c("A", "D")))
})

cat("Most-lifers server tests passed.\n")
