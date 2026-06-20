source(file.path("R", "lifer_finder.R"))

key <- Sys.getenv("EBIRD_API_KEY")
stopifnot(nzchar(key))
life <- read_life_list(file.path("data", "ebird_world_life_list.csv"))
zone <- SEARCH_ZONES[1, , drop = FALSE]

hotspots <- fetch_zone_hotspots(zone, key)
observations <- fetch_zone_observations(zone, key)
notable <- fetch_zone_observations(zone, key, notable = TRUE)
stopifnot(nrow(hotspots) > 0L)
stopifnot(nrow(observations) > 0L)

candidates <- choose_candidates(hotspots, observations, life, max_candidates = 2L)
stopifnot(nrow(candidates) > 0L)

detail <- fetch_hotspot_observations(candidates$locId[1], key)
notable_keys <- make_observation_key(notable$locId, notable$speciesCode)
summary <- summarize_hotspot(detail, candidates[1, , drop = FALSE], life, notable_keys)
stopifnot(summary$summary$recent_species >= 0L)

area_species <- build_area_lifer_species(observations, life)
stopifnot(nrow(area_species) > 0L)
species_reports <- fetch_zone_species_observations(zone, area_species$speciesCode[1], key)
hotspot_details <- build_hotspot_details(species_reports)
stopifnot(nrow(hotspot_details) > 0L)

cat(
  "Live API smoke test passed:", nrow(hotspots), "active hotspots,",
  nrow(observations), "broad observations.\n"
)
