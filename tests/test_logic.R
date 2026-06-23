source(file.path("R", "lifer_finder.R"))

life <- read_life_list(file.path("data", "ebird_world_life_list.csv"))
stopifnot(nrow(life) == 330L)
stopifnot(all(nzchar(life$scientific_name)))

raw <- utils::read.csv(
  file.path("data", "ebird_world_life_list.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
stopifnot(sum(raw[["Category"]] != "species" | raw[["Countable"]] != 1) == 30L)

seen_sci <- life$scientific_name[1]
seen_common <- life$common_name[1]
mock <- data.frame(
  speciesCode = c("seen1", "new1", "new2"),
  comName = c(seen_common, "Imaginary Lifer", "Second Lifer"),
  sciName = c(seen_sci, "Avis imaginaria", "Avis secunda"),
  locId = c("L1", "L1", "L2"),
  locName = c("One", "One", "Two"),
  obsDt = c("2026-06-19 08:00", "2026-06-19 09:00", "2026-06-18 10:00"),
  lat = c(45.6, 45.6, 45.7), lng = c(-122.7, -122.7, -122.8),
  howMany = c(1, 2, 1), obsValid = TRUE,
  obsReviewed = c(FALSE, TRUE, FALSE), locationPrivate = FALSE,
  subId = c("S1", "S2", "S3"), stringsAsFactors = FALSE
)

marked <- mark_potential_lifers(mock, life)
stopifnot(identical(marked$is_lifer, c(FALSE, TRUE, TRUE)))

hotspot_one <- data.frame(
  locId = "L1", locName = "One", lat = 45.6, lng = -122.7,
  latestObsDt = "2026-06-19", numSpeciesAllTime = 200,
  zone = "Portland / Vancouver", stringsAsFactors = FALSE
)
notable_key <- make_observation_key("L1", "new1")
summary_one <- summarize_hotspot(mock[mock$locId == "L1", ], hotspot_one, life, notable_key)
stopifnot(summary_one$summary$potential_lifers == 1L)
stopifnot(summary_one$summary$recent_species == 2L)
stopifnot(summary_one$summary$notable_lifers == 1L)

rank_input <- rbind(
  summary_one$summary,
  transform(summary_one$summary, locId = "L2", hotspot = "Two", potential_lifers = 0L)
)
ranked <- rank_hotspots(rank_input)
stopifnot(ranked$locId[1] == "L1")
stopifnot(LOOKBACK_DAYS == 3L)

details <- list(L1 = summary_one)
species_locations <- build_species_locations(details, ranked[ranked$locId == "L1", , drop = FALSE])
species_summary <- summarize_species(species_locations)
stopifnot(nrow(species_locations) == 1L)
stopifnot(nrow(species_summary) == 1L)
stopifnot(species_summary$common_name[1] == "Imaginary Lifer")
stopifnot(species_summary$reporting_hotspots[1] == 1L)

no_lifers <- mark_potential_lifers(mock[1, , drop = FALSE], life)
stopifnot(!any(no_lifers$is_lifer))

area_species <- build_area_lifer_species(mock, life)
stopifnot(nrow(area_species) == 2L)
stopifnot(!seen_common %in% area_species$common_name)

hotspot_details <- build_hotspot_details(marked[marked$is_lifer, , drop = FALSE])
stopifnot(nrow(hotspot_details) == 2L)
stopifnot(all(c("One", "Two") %in% hotspot_details$hotspot))
stopifnot(all(is.finite(hotspot_details$approx_miles)))

other_mock <- mock
other_mock$category <- "species"
hybrid <- other_mock[2, , drop = FALSE]
hybrid$speciesCode <- "hybrid1"
hybrid$comName <- "Imaginary Hybrid"
hybrid$sciName <- "Avis imaginaria x secunda"
hybrid$category <- "hybrid"
other_mock <- rbind(other_mock, hybrid)
other_lifers <- build_other_lifers(other_mock, life, exclude_species_code = "new1")
stopifnot(nrow(other_lifers) == 1L)
stopifnot(other_lifers$speciesCode[1] == "new2")
stopifnot(count_potential_lifers(other_mock, life) == 2L)

cat("All logic tests passed.\n")
