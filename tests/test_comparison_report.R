source(file.path("R", "lifer_finder.R"))
source(file.path("R", "comparison_report.R"))

life <- read_life_list(file.path("data", "ebird_world_life_list.csv"))

make_report_observations <- function(codes, names, loc_id, times = NULL) {
  if (is.null(times)) times <- rep("2026-06-20 08:00", length(codes))
  data.frame(
    speciesCode = codes, comName = names, sciName = paste("Avis", codes),
    category = "species", locId = loc_id, locName = paste("Hotspot", loc_id),
    obsDt = times, lat = 45.6, lng = -122.7, howMany = 1,
    obsValid = TRUE, obsReviewed = FALSE, locationPrivate = FALSE,
    subId = paste0(loc_id, seq_along(codes)), stringsAsFactors = FALSE
  )
}

seen <- life[1, ]
obs_a <- make_report_observations(
  c("x", "y", "seen", "hybrid"),
  c("Lifer X", "Lifer Y", seen$common_name, "Hybrid bird"), "A"
)
obs_a$sciName[3] <- seen$scientific_name
obs_a$category[4] <- "hybrid"
obs_b <- make_report_observations(
  c("y", "z"), c("Lifer Y", "Lifer Z"), "B",
  c("2026-06-19 09:00", "2026-06-20 10:00")
)

spots <- data.frame(
  locId = c("A", "B", "C"), hotspot = c("Alpha Marsh", "Beta Woods", "Broken Site"),
  obs_time = as.POSIXct(c("2026-06-20 08:00", "2026-06-20 10:00", "2026-06-18 08:00"), tz = "America/Los_Angeles"),
  approx_miles = c(4, 2, 1), stringsAsFactors = FALSE
)

shortlist <- empty_shortlist()
shortlist <- toggle_shortlist_hotspot(shortlist, spots[1, ])
stopifnot(identical(shortlist$locId, "A"))
shortlist <- toggle_shortlist_hotspot(shortlist, spots[1, ])
stopifnot(nrow(shortlist) == 0L)

comparison <- build_comparison_data(
  spots,
  list(A = obs_a, B = obs_b, C = NULL),
  life,
  failed_loc_ids = "C"
)
summary <- comparison$hotspots
stopifnot(summary$total_lifers[summary$locId == "A"] == 2L)
stopifnot(summary$total_lifers[summary$locId == "B"] == 2L)
stopifnot(summary$unique_lifers[summary$locId == "A"] == 1L)
stopifnot(summary$unique_lifers[summary$locId == "B"] == 1L)
stopifnot(!summary$available[summary$locId == "C"])
stopifnot(nrow(comparison$presence) == 4L)
stopifnot(nrow(comparison$pairs) == 1L)
stopifnot(comparison$pairs$combined_lifers[1] == 3L)
stopifnot(comparison$pairs$shared_lifers[1] == 1L)
pair_exclusives <- paste(comparison$pairs$only_a[1], comparison$pairs$only_b[1])
stopifnot(grepl("Lifer X", pair_exclusives, fixed = TRUE))
stopifnot(grepl("Lifer Z", pair_exclusives, fixed = TRUE))

one <- build_comparison_data(spots[1, ], list(A = obs_a), life)
stopifnot(nrow(one$pairs) == 0L)
stopifnot(inherits(try(build_comparison_data(spots[0, ], list(), life), silent = TRUE), "try-error"))

report_file <- tempfile(fileext = ".html")
write_comparison_report(
  comparison, report_file, "Test hotspot comparison", "Previous 3 days",
  highlighted_species_code = "y", highlighted_species_name = "Lifer Y",
  generated_at = as.POSIXct("2026-06-20 12:00", tz = "America/Los_Angeles")
)
html <- paste(readLines(report_file, warn = FALSE), collapse = "\n")
stopifnot(grepl("Species by hotspot", html, fixed = TRUE))
stopifnot(grepl("Best hotspot pairs", html, fixed = TRUE))
stopifnot(grepl("Lifer Y", html, fixed = TRUE))
stopifnot(grepl("class=\"highlight\"", html, fixed = TRUE))
stopifnot(grepl("@media print", html, fixed = TRUE))
stopifnot(grepl("width=device-width", html, fixed = TRUE))
stopifnot(!grepl(Sys.getenv("EBIRD_API_KEY"), html, fixed = TRUE) || !nzchar(Sys.getenv("EBIRD_API_KEY")))

many_spots <- do.call(rbind, lapply(seq_len(11L), function(i) {
  transform(spots[1, ], locId = paste0("M", i), hotspot = paste("Hotspot", i))
}))
many_obs <- setNames(lapply(seq_len(11L), function(i) obs_a), many_spots$locId)
many <- build_comparison_data(many_spots, many_obs, life)
wide_file <- tempfile(fileext = ".html")
write_comparison_report(many, wide_file, "Wide report", "Previous 3 days")
wide_html <- paste(readLines(wide_file, warn = FALSE), collapse = "\n")
stopifnot(grepl("more than ten hotspots", wide_html, fixed = TRUE))

cat("Comparison report tests passed.\n")
