empty_shortlist <- function() {
  data.frame(
    locId = character(), hotspot = character(),
    obs_time = as.POSIXct(character()), approx_miles = numeric(),
    stringsAsFactors = FALSE
  )
}

toggle_shortlist_hotspot <- function(shortlist, hotspot_row) {
  if (is.null(shortlist) || !nrow(shortlist)) shortlist <- empty_shortlist()
  loc_id <- as.character(hotspot_row$locId[1])
  if (loc_id %in% shortlist$locId) {
    return(shortlist[shortlist$locId != loc_id, , drop = FALSE])
  }
  added <- data.frame(
    locId = loc_id, hotspot = as.character(hotspot_row$hotspot[1]),
    obs_time = hotspot_row$obs_time[1],
    approx_miles = suppressWarnings(as.numeric(hotspot_row$approx_miles[1])),
    stringsAsFactors = FALSE
  )
  out <- rbind(shortlist, added)
  rownames(out) <- NULL
  out
}

latest_observation_time <- function(observations) {
  if (is.null(observations) || !nrow(observations)) return(as.POSIXct(NA))
  times <- parse_observation_time(observations$obsDt)
  if (!any(!is.na(times))) return(as.POSIXct(NA))
  max(times, na.rm = TRUE)
}

build_comparison_data <- function(hotspots, observations_by_hotspot, life_list,
                                  failed_loc_ids = character()) {
  if (is.null(hotspots) || !nrow(hotspots)) stop("Choose at least one hotspot.")
  hotspots <- hotspots[!duplicated(hotspots$locId), , drop = FALSE]
  rows <- vector("list", nrow(hotspots))
  lifer_rows <- list()
  for (i in seq_len(nrow(hotspots))) {
    loc_id <- hotspots$locId[i]
    observations <- observations_by_hotspot[[loc_id]]
    available <- !loc_id %in% failed_loc_ids && !is.null(observations)
    lifers <- if (available) build_other_lifers(observations, life_list, NULL) else NULL
    if (available && nrow(lifers)) {
      lifer_rows[[loc_id]] <- data.frame(
        locId = loc_id, speciesCode = lifers$speciesCode,
        common_name = lifers$common_name, latest_report = lifers$latest_report,
        stringsAsFactors = FALSE
      )
    }
    rows[[i]] <- data.frame(
      locId = loc_id, hotspot = hotspots$hotspot[i],
      approx_miles = suppressWarnings(as.numeric(hotspots$approx_miles[i])),
      latest_report = if (available) latest_observation_time(observations) else as.POSIXct(NA),
      available = available,
      total_lifers = if (available) nrow(lifers) else NA_integer_,
      stringsAsFactors = FALSE
    )
  }
  summary <- do.call(rbind, rows)
  presence <- if (length(lifer_rows)) do.call(rbind, lifer_rows) else data.frame(
    locId = character(), speciesCode = character(), common_name = character(),
    latest_report = as.POSIXct(character()), stringsAsFactors = FALSE
  )
  if (nrow(presence)) {
    coverage <- aggregate(locId ~ speciesCode + common_name, presence, function(x) length(unique(x)))
    names(coverage)[3] <- "hotspot_count"
    presence <- merge(presence, coverage, by = c("speciesCode", "common_name"), all.x = TRUE, sort = FALSE)
    unique_presence <- presence[presence$hotspot_count == 1L, , drop = FALSE]
    unique_counts <- if (nrow(unique_presence)) {
      aggregate(speciesCode ~ locId, unique_presence, length)
    } else {
      data.frame(locId = character(), speciesCode = integer())
    }
    names(unique_counts)[2] <- "unique_lifers"
    summary <- merge(summary, unique_counts, by = "locId", all.x = TRUE, sort = FALSE)
    summary$unique_lifers[is.na(summary$unique_lifers) & summary$available] <- 0L
  } else {
    summary$unique_lifers <- ifelse(summary$available, 0L, NA_integer_)
  }
  summary <- summary[order(
    !summary$available,
    -ifelse(is.na(summary$total_lifers), -1, summary$total_lifers),
    summary$approx_miles, -as.numeric(summary$latest_report)
  ), , drop = FALSE]
  summary$report_order <- seq_len(nrow(summary))
  if (nrow(presence)) {
    presence$hotspot_order <- match(presence$locId, summary$locId)
    presence <- presence[order(presence$hotspot_count, presence$common_name, presence$hotspot_order), , drop = FALSE]
  }
  available_ids <- summary$locId[summary$available]
  list(
    hotspots = summary,
    presence = presence,
    pairs = build_hotspot_pairs(available_ids, presence, summary),
    failed_loc_ids = summary$locId[!summary$available]
  )
}

build_hotspot_pairs <- function(loc_ids, presence, summary) {
  empty <- data.frame(
    loc_id_a = character(), loc_id_b = character(), hotspot_a = character(),
    hotspot_b = character(), combined_lifers = integer(), shared_lifers = integer(),
    only_a = character(), only_b = character(), combined_miles = numeric(),
    latest_report = as.POSIXct(character()), stringsAsFactors = FALSE
  )
  if (length(loc_ids) < 2L) return(empty)
  rows <- lapply(utils::combn(loc_ids, 2L, simplify = FALSE), function(ids) {
    a <- presence[presence$locId == ids[1], , drop = FALSE]
    b <- presence[presence$locId == ids[2], , drop = FALSE]
    set_a <- unique(a$speciesCode)
    set_b <- unique(b$speciesCode)
    site_a <- summary[summary$locId == ids[1], , drop = FALSE]
    site_b <- summary[summary$locId == ids[2], , drop = FALSE]
    latest <- suppressWarnings(max(c(site_a$latest_report, site_b$latest_report), na.rm = TRUE))
    if (!is.finite(as.numeric(latest))) latest <- as.POSIXct(NA)
    data.frame(
      loc_id_a = ids[1], loc_id_b = ids[2],
      hotspot_a = site_a$hotspot[1], hotspot_b = site_b$hotspot[1],
      combined_lifers = length(union(set_a, set_b)),
      shared_lifers = length(intersect(set_a, set_b)),
      only_a = paste(sort(unique(a$common_name[a$speciesCode %in% setdiff(set_a, set_b)])), collapse = ", "),
      only_b = paste(sort(unique(b$common_name[b$speciesCode %in% setdiff(set_b, set_a)])), collapse = ", "),
      combined_miles = if (anyNA(c(site_a$approx_miles, site_b$approx_miles))) Inf else sum(c(site_a$approx_miles, site_b$approx_miles)),
      latest_report = latest, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$combined_lifers, out$combined_miles, -as.numeric(out$latest_report)), , drop = FALSE]
  rownames(out) <- NULL
  utils::head(out, 5L)
}

format_report_time <- function(x) {
  if (!length(x) || is.na(x)) return("—")
  format(x, "%b %d, %I:%M %p")
}

write_comparison_report <- function(comparison, file, title, window_label,
                                    highlighted_species_code = NULL,
                                    highlighted_species_name = NULL,
                                    generated_at = Sys.time()) {
  h <- comparison$hotspots
  p <- comparison$presence
  species <- if (nrow(p)) unique(p[, c("speciesCode", "common_name", "hotspot_count"), drop = FALSE]) else data.frame(
    speciesCode = character(), common_name = character(), hotspot_count = integer()
  )
  species <- species[order(species$hotspot_count, species$common_name), , drop = FALSE]
  summary_table <- htmltools::tags$table(
    class = "summary-table",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th("Hotspot"), htmltools::tags$th("Distance"),
      htmltools::tags$th("Lifers"), htmltools::tags$th("Only here"),
      htmltools::tags$th("Latest report")
    )),
    htmltools::tags$tbody(lapply(seq_len(nrow(h)), function(i) htmltools::tags$tr(
      htmltools::tags$td(h$hotspot[i]),
      htmltools::tags$td(if (is.na(h$approx_miles[i])) "—" else paste0(round(h$approx_miles[i]), " mi")),
      htmltools::tags$td(if (h$available[i]) h$total_lifers[i] else "Unavailable"),
      htmltools::tags$td(if (h$available[i]) h$unique_lifers[i] else "—"),
      htmltools::tags$td(format_report_time(h$latest_report[i]))
    )))
  )
  matrix_table <- if (!nrow(species)) {
    htmltools::tags$p(class = "muted", "No potential lifers were reported at the available hotspots.")
  } else htmltools::tags$div(
    class = "matrix-wrap",
    htmltools::tags$table(
      class = "matrix-table",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th(class = "species-col", "Potential lifer"),
        lapply(h$hotspot, htmltools::tags$th)
      )),
      htmltools::tags$tbody(lapply(seq_len(nrow(species)), function(i) {
        sp <- species[i, ]
        row_class <- if (!is.null(highlighted_species_code) && identical(sp$speciesCode, highlighted_species_code)) "highlight" else NULL
        htmltools::tags$tr(
          class = row_class,
          htmltools::tags$th(class = "species-col", sp$common_name),
          lapply(h$locId, function(loc_id) {
            hit <- p[p$locId == loc_id & p$speciesCode == sp$speciesCode, , drop = FALSE]
            htmltools::tags$td(if (nrow(hit)) format_report_time(hit$latest_report[1]) else "")
          })
        )
      }))
    )
  )
  unique_section <- lapply(seq_len(nrow(h)), function(i) {
    unique_here <- p[p$locId == h$locId[i] & p$hotspot_count == 1L, , drop = FALSE]
    htmltools::tags$div(class = "report-card", htmltools::tags$h3(h$hotspot[i]),
      if (!h$available[i]) htmltools::tags$p(class = "muted", "Observations unavailable.")
      else if (!nrow(unique_here)) htmltools::tags$p(class = "muted", "No lifers unique to this shortlist.")
      else htmltools::tags$p(paste(sort(unique(unique_here$common_name)), collapse = ", "))
    )
  })
  pair_section <- if (!nrow(comparison$pairs)) {
    htmltools::tags$p(class = "muted", "Choose at least two available hotspots to compare pairs.")
  } else lapply(seq_len(nrow(comparison$pairs)), function(i) {
    pair <- comparison$pairs[i, ]
    htmltools::tags$div(class = "pair-card",
      htmltools::tags$h3(paste(pair$hotspot_a, "+", pair$hotspot_b)),
      htmltools::tags$p(class = "pair-total", paste(pair$combined_lifers, "combined lifers ·", pair$shared_lifers, "shared")),
      htmltools::tags$p(htmltools::tags$b(paste0("Only at ", pair$hotspot_a, ": ")), if (nzchar(pair$only_a)) pair$only_a else "None"),
      htmltools::tags$p(htmltools::tags$b(paste0("Only at ", pair$hotspot_b, ": ")), if (nzchar(pair$only_b)) pair$only_b else "None")
    )
  })
  full_lists <- lapply(seq_len(nrow(h)), function(i) {
    site_lifers <- p[p$locId == h$locId[i], , drop = FALSE]
    site_lifers <- site_lifers[order(site_lifers$common_name), , drop = FALSE]
    htmltools::tags$section(class = "site-list", htmltools::tags$h3(h$hotspot[i]),
      if (!h$available[i]) htmltools::tags$p(class = "warning", "Observations unavailable for this hotspot.")
      else if (!nrow(site_lifers)) htmltools::tags$p(class = "muted", "No potential lifers reported.")
      else htmltools::tags$ul(lapply(seq_len(nrow(site_lifers)), function(j) htmltools::tags$li(
        htmltools::tags$span(site_lifers$common_name[j]),
        htmltools::tags$time(format_report_time(site_lifers$latest_report[j]))
      )))
    )
  })
  css <- "*{box-sizing:border-box}body{margin:0;background:#f4f5f2;color:#24352a;font:13px/1.4 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}main{max-width:1180px;margin:auto;padding:18px}h1{font-size:24px;margin:0 0 4px}h2{font-size:17px;margin:24px 0 8px}h3{font-size:13px;margin:0 0 6px}p{margin:4px 0}.subtitle,.muted{color:#68746c}.warning{color:#8a4e12;font-weight:650}.wide-warning{padding:8px 10px;background:#fff4d8;border-radius:6px}table{width:100%;border-collapse:collapse;background:white}th,td{border:1px solid #dfe4df;padding:6px 7px;text-align:left;vertical-align:top}thead th{background:#e7eee8}.summary-table th:not(:first-child),.summary-table td:not(:first-child){white-space:nowrap}.matrix-wrap{overflow-x:auto;-webkit-overflow-scrolling:touch;border:1px solid #dfe4df}.matrix-table{min-width:720px;border:0}.matrix-table th,.matrix-table td{font-size:11px;min-width:116px}.matrix-table .species-col{position:sticky;left:0;z-index:1;min-width:170px;background:#f8faf8}.matrix-table thead .species-col{z-index:2;background:#e7eee8}.matrix-table tr.highlight th,.matrix-table tr.highlight td{background:#fff3bf;font-weight:700}.card-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:8px}.report-card,.pair-card,.site-list{background:white;border:1px solid #dfe4df;border-radius:7px;padding:10px;break-inside:avoid}.pair-card,.site-list{margin-bottom:8px}.pair-total{color:#2a5235;font-weight:700}.site-list ul{list-style:none;margin:0;padding:0;columns:2;column-gap:18px}.site-list li{display:flex;justify-content:space-between;gap:10px;padding:3px 0;break-inside:avoid;border-bottom:1px solid #eef0ee}.site-list time{color:#68746c;white-space:nowrap;font-size:11px}footer{margin-top:24px;color:#68746c;font-size:11px}@media(max-width:640px){main{padding:12px 9px}h1{font-size:20px}.summary-table{font-size:11px}.summary-table th,.summary-table td{padding:5px}.site-list ul{columns:1}}@media print{@page{size:landscape;margin:.45in}body{background:white;font-size:10px}main{max-width:none;padding:0}.matrix-wrap{overflow:visible}.matrix-table{min-width:0}.matrix-table th,.matrix-table td{font-size:8px;min-width:0;padding:3px}.matrix-table .species-col{position:static;min-width:0}}"
  report <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title(title), htmltools::tags$style(htmltools::HTML(css))
    ),
    htmltools::tags$body(htmltools::tags$main(
      htmltools::tags$h1(title),
      htmltools::tags$p(class = "subtitle", paste(window_label, "· Generated", format(generated_at, "%b %d, %Y at %I:%M %p"))),
      if (!is.null(highlighted_species_name)) htmltools::tags$p(class = "subtitle", paste("Highlighted species:", highlighted_species_name)),
      if (nrow(h) > 10L) htmltools::tags$p(class = "wide-warning", "This report includes more than ten hotspots. Swipe the matrix sideways on a phone or print in landscape orientation."),
      if (length(comparison$failed_loc_ids)) htmltools::tags$p(class = "warning", paste(length(comparison$failed_loc_ids), "hotspot observation list(s) were unavailable; the report continues with the remaining locations.")),
      htmltools::tags$h2("Hotspot summary"), summary_table,
      htmltools::tags$h2("Species by hotspot"), matrix_table,
      htmltools::tags$h2("Lifers found at only one selected hotspot"),
      htmltools::tags$div(class = "card-grid", unique_section),
      htmltools::tags$h2("Best hotspot pairs"), pair_section,
      htmltools::tags$h2("Complete lifer lists"), full_lists,
      htmltools::tags$footer("Based on recent eBird reports in the selected time window. A report does not guarantee that a species will still be present.")
    ))
  )
  htmltools::save_html(report, file = file, background = "white")
  invisible(file)
}
