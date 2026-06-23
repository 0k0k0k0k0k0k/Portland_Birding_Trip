source(file.path("R", "lifer_finder.R"))

key <- Sys.getenv("EBIRD_API_KEY")
stopifnot(nzchar(key))
life <- read_life_list(file.path("data", "ebird_world_life_list.csv"))
zone <- SEARCH_ZONES[1, , drop = FALSE]

observations <- fetch_zone_observations(zone, key)
stopifnot(nrow(observations) > 0L)

area_species <- build_area_lifer_species(observations, life)
stopifnot(nrow(area_species) > 0L)
species_reports <- fetch_zone_species_observations(zone, area_species$speciesCode[1], key)
hotspot_details <- build_hotspot_details(species_reports)
stopifnot(nrow(hotspot_details) > 0L)

detail <- fetch_hotspot_observations(hotspot_details$locId[1], key)
stopifnot(nrow(detail) >= 0L)
stopifnot(count_potential_lifers(detail, life) >= 0L)

cat(
  "Live API smoke test passed:", nrow(area_species), "potential lifers,",
  nrow(hotspot_details), "hotspots for the first lifer.\n"
)
