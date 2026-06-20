# Portland Lifer Finder

A compact Shiny app for finding potential eBird lifers around Portland, Oregon. It compares recent public-hotspot reports with an exported eBird life list.

## Features

- Searches the previous 24 hours, two days, or three days.
- Lists potential lifers first, followed by their recent hotspots.
- Sorts hotspots by recency, distance, or total potential lifers.
- Builds a persistent hotspot shortlist.
- Exports phone-friendly HTML comparison reports with a species-by-hotspot matrix and best hotspot pairs.

## Run locally

1. Copy `.Renviron.example` to `.Renviron`.
2. Add your eBird API key as `EBIRD_API_KEY`.
3. Open `Portland_Birding_Trip.Rproj` and run `shiny::runApp()`.

The real `.Renviron` file is excluded from Git.

## Posit Connect Cloud

Deploy `app.R` from this repository and add `EBIRD_API_KEY` as a secret environment variable in Connect Cloud. The included eBird life-list export supplies the species already seen.
