library(shiny)
library(bslib)
library(dplyr)
library(httr2)

source(file.path("R", "lifer_finder.R"), local = TRUE)
source(file.path("R", "comparison_report.R"), local = TRUE)
life_list_path <- file.path("data", "ebird_world_life_list.csv")
APP_TIMEZONE <- "America/Los_Angeles"

format_app_time <- function(x) {
  format(x, "%b %d, %I:%M %p", tz = APP_TIMEZONE)
}

theme <- bs_theme(
  version = 5,
  bg = "#F4F5F2",
  fg = "#24352A",
  primary = "#2A5235"
)

ui <- page_fluid(
  theme = theme,
  tags$head(tags$style(HTML("\n    body { background: #F4F5F2; }\n    .app-shell { max-width: 1320px; margin: 0 auto; padding: 18px 14px 32px; }\n    .app-header { display: flex; justify-content: space-between; align-items: center; gap: 18px; margin-bottom: 16px; }\n    .app-title { font-size: 1.65rem; font-weight: 750; margin: 0; }\n    .app-subtitle { color: #68746C; margin: 2px 0 0; font-size: .93rem; }\n    .bslib-card { border: 1px solid #D9DED9; box-shadow: none; border-radius: 12px; overflow: hidden; }\n    .card-header { background: #FFFFFF; border-bottom: 1px solid #E2E6E2; font-weight: 700; }\n    .explore-box { height: 540px; overflow-y: auto; background: #FFFFFF; }\n    .species-search { padding: 12px 14px 5px; position: sticky; top: 0; background: #FFFFFF; z-index: 2; border-bottom: 1px solid #EEF0EE; }\n    .species-search .form-group { margin-bottom: 8px; }\n    .species-list { padding: 0; }\n    .species-item { display: block; padding: 11px 15px; border-bottom: 1px solid #EEF0EE; color: #24352A; text-decoration: none; }\n    .species-item:hover { background: #F1F5F1; color: #1F482C; }\n    .species-item.selected { background: #E4EEE6; box-shadow: inset 4px 0 #2A5235; }\n    .species-name { display: block; font-weight: 700; line-height: 1.2; }\n    .species-scientific { display: block; color: #758078; font-style: italic; font-size: .84rem; margin-top: 2px; }\n    .species-meta { display: block; color: #5B695F; font-size: .82rem; margin-top: 5px; }\n    .location-panel { padding: 4px 0; }\n    .location-item { padding: 13px 16px; border-bottom: 1px solid #EEF0EE; }\n    .location-name { font-weight: 700; line-height: 1.25; }\n    .location-meta { color: #657168; font-size: .87rem; margin-top: 4px; }\n    .empty-state { color: #6F7972; padding: 24px 18px; }\n    .selected-heading small { display: block; color: #758078; font-style: italic; font-weight: 400; margin-top: 2px; }\n    .map-card { margin-top: 16px; }\n    .leaflet-container { background: #E7ECE8; }\n    @media (max-width: 767px) {\n      .app-header { align-items: flex-start; flex-direction: column; }\n      .explore-box { height: 450px; }\n    }\n  "))),
  tags$head(tags$style(HTML("\n    body { font-size: 14px; }\n    .app-title { font-size: 1.4rem; }\n    .app-subtitle { font-size: .82rem; }\n    .header-controls { display: flex; align-items: center; gap: 14px; }\n    .lookback-control { margin: 0; }\n    .lookback-control .form-check { margin-right: 10px; font-size: .82rem; }\n    .lookback-control .shiny-options-group { margin: 0; }\n    .species-search { padding: 8px 11px 2px; }\n    .species-search .form-group { margin-bottom: 6px; }\n    .species-search .form-control { font-size: .84rem; padding: 5px 9px; min-height: 32px; }\n    .species-item { padding: 7px 12px; }\n    .species-name { font-size: .92rem; line-height: 1.15; }\n    .species-meta { font-size: .72rem; margin-top: 3px; }\n    .location-item { padding: 8px 12px; }\n    .location-name { font-size: .9rem; line-height: 1.2; }\n    .location-meta { font-size: .74rem; margin-top: 3px; }\n    @media (max-width: 767px) { .header-controls { align-items: flex-start; flex-direction: column; gap: 4px; } }\n  "))),
  div(
    id = "app_shell",
    class = "app-shell mobile-lifers",
    tags$script(HTML("\n      window.mobileShowHotspots = function() {\n        if (!window.matchMedia('(max-width: 767px)').matches) return;\n        var shell = document.getElementById('app_shell');\n        shell.classList.remove('mobile-lifers');\n        shell.classList.add('mobile-hotspots');\n        window.scrollTo(0, 0);\n      };\n      window.mobileShowLifers = function() {\n        var shell = document.getElementById('app_shell');\n        shell.classList.remove('mobile-hotspots');\n        shell.classList.add('mobile-lifers');\n        window.scrollTo(0, 0);\n      };\n    ")),
    tags$style(HTML("\n      .mobile-back { display: none; border: 0; background: transparent; color: #2A5235; font-size: 10px; font-weight: 700; padding: 1px 4px 1px 0; white-space: nowrap; }\n      @media (max-width: 767px) {\n        .app-shell { padding: 8px 7px 10px; min-height: 100dvh; }\n        .app-header { position: sticky; top: 0; z-index: 20; background: #F4F5F2; margin-bottom: 6px; padding-bottom: 4px; }\n        .app-subtitle { display: none; }\n        .header-controls { flex-direction: row !important; align-items: center !important; gap: 8px !important; }\n        .mobile-lifers .hotspot-pane { display: none !important; }\n        .mobile-hotspots .species-pane { display: none !important; }\n        .mobile-lifers .species-pane, .mobile-hotspots .hotspot-pane { display: flex !important; }\n        .mobile-back { display: inline-block; }\n        .explore-box { height: calc(100dvh - 132px) !important; min-height: 380px; }\n        .species-pane, .hotspot-pane { margin: 0 !important; }\n      }\n    ")),
    tags$style(HTML("\n      .location-header { flex-wrap: nowrap !important; }\n      .location-header > .shiny-html-output { flex: 1 1 auto; min-width: 0; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }\n      .location-header .form-group { flex: 0 0 92px; width: 92px !important; margin: 0 !important; }\n      .location-header select { white-space: nowrap; }\n    ")),
    tags$style(HTML("\n      body { font-size: 13px; }\n      .app-title { font-size: 1.28rem; }\n      .app-subtitle { font-size: .75rem; }\n      .card-header { font-size: .82rem; padding: 7px 10px; }\n      .species-search { padding: 6px 9px 1px; }\n      .species-search .form-control { font-size: .76rem; min-height: 28px; padding: 3px 7px; }\n      .species-item { padding: 5px 10px; }\n      .species-name { font-size: .82rem; }\n      .species-meta { font-size: .64rem; margin-top: 2px; }\n      .location-item { padding: 6px 10px; }\n      .location-name { font-size: .8rem; }\n      .location-meta { font-size: .66rem; margin-top: 2px; }\n      .location-header { display: flex; align-items: center; justify-content: space-between; gap: 10px; width: 100%; }\n      .location-header .form-group { margin: 0; }\n      .location-header .form-select { font-size: .72rem; min-height: 27px; padding: 2px 24px 2px 7px; }\n    ")),
    tags$style(HTML("\n      .hotspot-item { display: block; color: #24352A; text-decoration: none; }\n      .hotspot-item:hover { background: #F1F5F1; color: #24352A; }\n      .hotspot-item.selected { background: #E8F0E9; box-shadow: inset 3px 0 #2A5235; }\n      .other-lifers { margin-top: 6px; padding: 6px 8px; background: #FFFFFF; border: 1px solid #DDE5DE; border-radius: 6px; }\n      .other-lifers-title { font-size: .68rem; font-weight: 700; margin-bottom: 3px; color: #405046; }\n      .other-lifer-row { display: flex; justify-content: space-between; gap: 8px; padding: 2px 0; font-size: .67rem; }\n      .other-lifer-time { color: #6D786F; white-space: nowrap; }\n      .other-lifers-empty { color: #6D786F; font-size: .67rem; }\n    ")),
    tags$style(HTML("\n      .sort-select { flex: 0 0 auto; width: 78px; height: 20px; min-height: 20px; padding: 0 14px 0 4px; border: 1px solid #9AA49D; border-radius: 3px; background-color: #FFFFFF; color: #405046; font-size: 9px; line-height: 18px; }\n      .sort-select option { font-size: 9px; }\n    ")),
    tags$style(HTML("\n      .hotspot-main { display: flex; align-items: flex-start; justify-content: space-between; gap: 8px; }\n      .hotspot-copy { min-width: 0; flex: 1 1 auto; }\n      .shortlist-toggle { flex: 0 0 22px; width: 22px; height: 22px; border: 1px solid #91A097; border-radius: 50%; background: #FFFFFF; color: #2A5235; font-size: 12px; font-weight: 800; line-height: 18px; padding: 0; }\n      .shortlist-toggle.chosen { background: #2A5235; border-color: #2A5235; color: #FFFFFF; }\n      .shortlist-bar { display: flex; align-items: center; gap: 5px; min-height: 30px; padding: 4px 8px; border-bottom: 1px solid #E2E6E2; background: #F8FAF8; white-space: nowrap; overflow-x: auto; }\n      .shortlist-count { margin-right: auto; color: #4F5E54; font-size: 9px; font-weight: 700; }\n      .shortlist-hint { color: #6D786F; font-size: 9px; }\n      .shortlist-bar .btn, .shortlist-bar a { min-height: 20px; margin: 0; padding: 2px 6px; font-size: 9px; line-height: 14px; text-decoration: none; }\n      .shortlist-disabled { color: #9AA29C; border: 1px solid #D6DCD7; border-radius: 3px; padding: 2px 6px; font-size: 9px; }\n      @media (max-width: 767px) { .explore-box { height: calc(100dvh - 160px) !important; } }\n    ")),
    div(
      class = "app-header",
      div(
        h1(class = "app-title", "Portland Lifer Finder"),
        p(class = "app-subtitle", "Lifers reported in the search area during the selected time window")
      ),
      div(
        class = "header-controls",
        radioButtons(
          "lookback", NULL,
          choices = c("24 hours" = "1", "2 days" = "2", "3 days" = "3"),
          selected = "3", inline = TRUE
        ) |> tagAppendAttributes(class = "lookback-control"),
        actionButton("refresh", "Refresh sightings", class = "btn-primary btn-sm")
      )
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(
        class = "species-pane",
        card_header(uiOutput("lifer_heading")),
        div(
          class = "explore-box",
          div(class = "species-search", textInput("species_search", NULL, placeholder = "Search lifers")),
          uiOutput("species_list")
        )
      ),
      card(
        class = "hotspot-pane",
        card_header(
          div(
            class = "location-header",
            tags$button(type = "button", class = "mobile-back", onclick = "mobileShowLifers();", "‹ Lifers"),
            uiOutput("selected_heading"),
            tags$select(
              class = "sort-select",
              onchange = "Shiny.setInputValue('hotspot_sort', this.value, {priority: 'event'});",
              tags$option(value = "recent", selected = "selected", "Recent"),
              tags$option(value = "distance", "Nearest"),
              tags$option(value = "lifers", "Most lifers")
            )
          )
        ),
        uiOutput("shortlist_bar"),
        div(class = "explore-box location-panel", uiOutput("location_info"))
      )
    )
  )
)

server <- function(input, output, session) {
  life_list <- read_life_list(life_list_path)
  results <- reactiveVal(NULL)
  selected_species <- reactiveVal(NULL)
  hotspot_results <- reactiveVal(NULL)
  selected_hotspot <- reactiveVal(NULL)
  hotspot_observation_cache <- reactiveVal(list())
  hotspot_count_failures <- reactiveVal(character())
  hotspot_detail_loading <- reactiveVal(FALSE)
  hotspot_detail_error <- reactiveVal(NULL)
  shortlist <- reactiveVal(empty_shortlist())

  lookback_days <- reactive({
    days <- suppressWarnings(as.integer(input$lookback))
    if (is.na(days) || !days %in% 1:3) 3L else days
  })

  load_species_hotspots <- function(species_code) {
    api_key <- Sys.getenv("EBIRD_API_KEY")
    if (!nzchar(api_key) || is.null(species_code) || !nzchar(species_code)) return()
    hotspot_results(NULL)
    selected_hotspot(NULL)
    tryCatch({
      observations <- withProgress(message = "Finding recent hotspots", value = 0, {
        pieces <- vector("list", nrow(SEARCH_ZONES))
        for (i in seq_len(nrow(SEARCH_ZONES))) {
          incProgress(1 / nrow(SEARCH_ZONES), detail = SEARCH_ZONES$zone[i])
          pieces[[i]] <- fetch_zone_species_observations(
            SEARCH_ZONES[i, , drop = FALSE], species_code, api_key, lookback_days()
          )
        }
        bind_rows(pieces)
      })
      hotspot_results(build_hotspot_details(observations))
      load_all_hotspot_lifers()
    }, error = function(e) {
      hotspot_results(build_hotspot_details(empty_observations()))
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  }

  hotspot_cache_key <- function(loc_id) paste(loc_id, lookback_days(), sep = "|")

  load_hotspot_lifers <- function(loc_id) {
    api_key <- Sys.getenv("EBIRD_API_KEY")
    if (!nzchar(api_key) || is.null(loc_id) || !nzchar(loc_id)) return()
    selected_hotspot(loc_id)
    hotspot_detail_error(NULL)
    key <- hotspot_cache_key(loc_id)
    hotspot_count_failures(setdiff(hotspot_count_failures(), key))
    cached <- hotspot_observation_cache()
    if (!is.null(cached[[key]])) return()
    hotspot_detail_loading(TRUE)
    tryCatch({
      observations <- fetch_hotspot_observations(loc_id, api_key, lookback_days())
      cached <- hotspot_observation_cache()
      cached[[key]] <- observations
      hotspot_observation_cache(cached)
    }, error = function(e) {
      hotspot_detail_error(conditionMessage(e))
    }, finally = {
      hotspot_detail_loading(FALSE)
    })
  }

  ensure_hotspot_observations <- function(loc_ids, progress_message = "Loading hotspot lifers") {
    api_key <- Sys.getenv("EBIRD_API_KEY")
    loc_ids <- unique(as.character(loc_ids[!is.na(loc_ids) & nzchar(loc_ids)]))
    if (!nzchar(api_key) || !length(loc_ids)) return(invisible(NULL))
    keys <- vapply(loc_ids, hotspot_cache_key, character(1))
    cached <- hotspot_observation_cache()
    failed <- hotspot_count_failures()
    missing <- loc_ids[vapply(keys, function(key) is.null(cached[[key]]) && !key %in% failed, logical(1))]
    if (!length(missing)) return(invisible(NULL))

    new_failures <- character()
    withProgress(message = progress_message, value = 0, {
      for (i in seq_along(missing)) {
        loc_id <- missing[i]
        key <- hotspot_cache_key(loc_id)
        incProgress(1 / length(missing), detail = paste(i, "of", length(missing)))
        tryCatch({
          observations <- fetch_hotspot_observations(loc_id, api_key, lookback_days())
          current_cache <- hotspot_observation_cache()
          current_cache[[key]] <- observations
          hotspot_observation_cache(current_cache)
        }, error = function(e) {
          new_failures <<- c(new_failures, key)
        })
      }
    })
    if (length(new_failures)) {
      hotspot_count_failures(unique(c(hotspot_count_failures(), new_failures)))
      showNotification(
        paste(length(new_failures), "hotspot observation list(s) were unavailable."),
        type = "warning", duration = 8
      )
    }
    invisible(NULL)
  }

  load_all_hotspot_lifers <- function() {
    spots <- hotspot_results()
    if (is.null(spots) || !nrow(spots)) return()
    ensure_hotspot_observations(spots$locId, "Calculating lifers at each hotspot")
  }

  load_reports <- function() {
    api_key <- Sys.getenv("EBIRD_API_KEY")
    if (!nzchar(api_key)) {
      showNotification("EBIRD_API_KEY is missing from the project .Renviron file.", type = "error", duration = NULL)
      return()
    }
    selected_species(NULL)
    hotspot_results(NULL)
    selected_hotspot(NULL)
    tryCatch({
      observations <- withProgress(message = "Finding lifers in the search area", value = 0, {
        pieces <- vector("list", nrow(SEARCH_ZONES))
        for (i in seq_len(nrow(SEARCH_ZONES))) {
          incProgress(1 / nrow(SEARCH_ZONES), detail = SEARCH_ZONES$zone[i])
          pieces[[i]] <- fetch_zone_observations(
            SEARCH_ZONES[i, , drop = FALSE], api_key, back_days = lookback_days()
          )
        }
        bind_rows(pieces)
      })
      species <- build_area_lifer_species(observations, life_list)
      results(list(species = species, observations = observations))
      if (nrow(species)) {
        selected_species(species$speciesCode[1])
        load_species_hotspots(species$speciesCode[1])
      } else {
        showNotification("No lifers were reported in the search area during the selected time window.", type = "warning", duration = 8)
      }
    }, error = function(e) {
      results(NULL)
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  }

  observeEvent(input$refresh, load_reports())
  observeEvent(input$lookback, {
    hotspot_observation_cache(list())
    hotspot_count_failures(character())
    selected_hotspot(NULL)
    if (!is.null(results())) load_reports()
  }, ignoreInit = TRUE)

  filtered_species <- reactive({
    req(results())
    x <- results()$species
    query <- if (is.null(input$species_search)) "" else trimws(input$species_search)
    if (nzchar(query)) {
      keep <- grepl(query, x$common_name, ignore.case = TRUE) |
        grepl(query, x$scientific_name, ignore.case = TRUE)
      x <- x[keep, , drop = FALSE]
    }
    x
  })

  output$lifer_heading <- renderUI({
    if (is.null(results())) return("Lifers in the search area")
    paste(nrow(results()$species), "lifers in the search area")
  })

  output$species_list <- renderUI({
    if (is.null(results())) return(div(class = "empty-state", "Select Refresh sightings to search the area."))
    x <- filtered_species()
    if (!nrow(x)) return(div(class = "empty-state", "No lifers match your search."))
    div(
      class = "species-list",
      lapply(seq_len(nrow(x)), function(i) {
        selected <- identical(x$speciesCode[i], selected_species())
        tags$a(
          href = "#",
          class = paste("species-item", if (selected) "selected" else ""),
          onclick = sprintf("Shiny.setInputValue('species_click', '%s', {priority: 'event'}); mobileShowHotspots(); return false;", x$speciesCode[i]),
          span(class = "species-name", x$common_name[i]),
          span(class = "species-meta", paste("Latest report", format_app_time(x$latest_report[i])))
        )
      })
    )
  })

  observeEvent(input$species_click, {
    selected_species(input$species_click)
    selected_hotspot(NULL)
    load_species_hotspots(input$species_click)
  })

  observeEvent(input$hotspot_click, load_hotspot_lifers(input$hotspot_click))
  observeEvent(input$shortlist_toggle, {
    spots <- hotspot_results()
    if (is.null(spots) || !nrow(spots)) return()
    row <- spots[spots$locId == input$shortlist_toggle, , drop = FALSE]
    if (nrow(row)) shortlist(toggle_shortlist_hotspot(shortlist(), row[1, , drop = FALSE]))
  })
  observeEvent(input$clear_shortlist, shortlist(empty_shortlist()))
  observeEvent(input$hotspot_sort, {
    if (identical(input$hotspot_sort, "lifers")) load_all_hotspot_lifers()
  })

  selected_species_row <- reactive({
    req(results(), selected_species())
    results()$species[results()$species$speciesCode == selected_species(), , drop = FALSE]
  })

  current_report_hotspots <- reactive({
    chosen <- shortlist()
    spots <- hotspot_results()
    if (!nrow(chosen) || is.null(spots) || !nrow(spots)) return(empty_shortlist())
    ids <- chosen$locId[chosen$locId %in% spots$locId]
    if (!length(ids)) return(empty_shortlist())
    spots[match(ids, spots$locId), c("locId", "hotspot", "obs_time", "approx_miles"), drop = FALSE]
  })

  output$shortlist_bar <- renderUI({
    chosen_n <- nrow(shortlist())
    current_n <- nrow(current_report_hotspots())
    if (!chosen_n) {
      return(div(class = "shortlist-bar", span(class = "shortlist-hint", "Use + to add hotspots for comparison")))
    }
    div(
      class = "shortlist-bar",
      span(class = "shortlist-count", paste(chosen_n, if (chosen_n == 1L) "selected" else "selected")),
      if (current_n) downloadButton("download_current_report", "Current species", class = "btn btn-outline-secondary btn-sm")
      else span(class = "shortlist-disabled", "Current species"),
      downloadButton("download_trip_report", "Trip shortlist", class = "btn btn-outline-secondary btn-sm"),
      actionLink("clear_shortlist", "Clear")
    )
  })

  lookback_label <- function() {
    switch(as.character(lookback_days()),
      "1" = "Previous 24 hours", "2" = "Previous 2 days", "3" = "Previous 3 days"
    )
  }

  safe_filename <- function(x) {
    x <- tolower(gsub("[^A-Za-z0-9]+", "-", x))
    gsub("(^-+|-+$)", "", x)
  }

  prepare_comparison <- function(spots, progress_message) {
    if (is.null(spots) || !nrow(spots)) stop("Choose at least one hotspot.")
    ensure_hotspot_observations(spots$locId, progress_message)
    cache <- hotspot_observation_cache()
    observations <- setNames(lapply(spots$locId, function(loc_id) {
      cache[[hotspot_cache_key(loc_id)]]
    }), spots$locId)
    failed <- spots$locId[vapply(spots$locId, function(loc_id) {
      hotspot_cache_key(loc_id) %in% hotspot_count_failures()
    }, logical(1))]
    build_comparison_data(spots, observations, life_list, failed)
  }

  output$download_current_report <- downloadHandler(
    filename = function() {
      species_name <- if (is.null(selected_species())) "current-species" else selected_species_row()$common_name[1]
      paste0("portland-lifers-", safe_filename(species_name), "-", Sys.Date(), ".html")
    },
    content = function(file) {
      spots <- current_report_hotspots()
      req(nrow(spots) > 0L)
      species <- selected_species_row()
      comparison <- prepare_comparison(spots, "Preparing current-species report")
      write_comparison_report(
        comparison, file,
        title = paste(species$common_name[1], "hotspot comparison"),
        window_label = lookback_label(),
        highlighted_species_code = selected_species(),
        highlighted_species_name = species$common_name[1]
      )
    },
    contentType = "text/html"
  )

  output$download_trip_report <- downloadHandler(
    filename = function() paste0("portland-lifer-trip-shortlist-", Sys.Date(), ".html"),
    content = function(file) {
      spots <- shortlist()
      req(nrow(spots) > 0L)
      comparison <- prepare_comparison(spots, "Preparing trip-shortlist report")
      write_comparison_report(
        comparison, file,
        title = "Portland lifer trip shortlist",
        window_label = lookback_label()
      )
    },
    contentType = "text/html"
  )

  sorted_hotspots <- reactive({
    req(hotspot_results())
    x <- hotspot_results()
    x$lifer_count <- vapply(x$locId, function(loc_id) {
      observations <- hotspot_observation_cache()[[hotspot_cache_key(loc_id)]]
      if (is.null(observations)) return(NA_integer_)
      count_potential_lifers(observations, life_list)
    }, integer(1))
    if (identical(input$hotspot_sort, "lifers")) {
      x[order(is.na(x$lifer_count), -x$lifer_count, x$approx_miles, -as.numeric(x$obs_time)), , drop = FALSE]
    } else if (identical(input$hotspot_sort, "distance")) {
      x[order(x$approx_miles, -as.numeric(x$obs_time)), , drop = FALSE]
    } else {
      x[order(-as.numeric(x$obs_time), x$approx_miles), , drop = FALSE]
    }
  })

  output$selected_heading <- renderUI({
    if (is.null(results()) || is.null(selected_species())) return("Recent hotspots")
    selected_species_row()$common_name[1]
  })

  output$location_info <- renderUI({
    if (is.null(results()) || is.null(selected_species())) {
      return(div(class = "empty-state", "Select a lifer to see hotspot information."))
    }
    if (is.null(hotspot_results())) return(div(class = "empty-state", "Loading recent hotspots…"))
    x <- sorted_hotspots()
    if (!nrow(x)) return(div(class = "empty-state", "No recent public hotspot reports found."))
    tagList(lapply(seq_len(nrow(x)), function(i) {
      loc_id <- x$locId[i]
      expanded <- identical(loc_id, selected_hotspot())
      shortlisted <- loc_id %in% shortlist()$locId
      count_text <- if (is.na(x$how_many[i])) "Count not reported" else paste("Count", x$how_many[i])
      lifer_count_text <- NULL
      if (!is.na(x$lifer_count[i])) {
        lifer_count_text <- paste(
          x$lifer_count[i],
          if (x$lifer_count[i] == 1L) "lifer" else "lifers"
        )
      } else if (
        identical(input$hotspot_sort, "lifers") &&
          hotspot_cache_key(loc_id) %in% hotspot_count_failures()
      ) {
        lifer_count_text <- "lifers unavailable"
      }
      key <- hotspot_cache_key(loc_id)
      cached <- hotspot_observation_cache()[[key]]
      hotspot_lifers <- NULL
      if (isTRUE(hotspot_detail_loading()) && is.null(cached)) {
        hotspot_lifers <- div(class = "other-lifers", div(class = "other-lifers-empty", "Loading potential lifers…"))
      } else if (key %in% hotspot_count_failures()) {
        hotspot_lifers <- div(class = "other-lifers", div(class = "other-lifers-empty", "Could not load hotspot lifers."))
      } else if (!is.null(cached)) {
        lifers <- build_other_lifers(cached, life_list, exclude_species_code = NULL)
        if (!nrow(lifers)) {
          hotspot_lifers <- div(class = "other-lifers", div(class = "other-lifers-empty", "No potential lifers reported here."))
        } else {
          hotspot_lifers <- div(
            class = "other-lifers",
            div(class = "other-lifers-title", paste(nrow(lifers), "potential lifers here")),
            lapply(seq_len(nrow(lifers)), function(j) {
              div(
                class = "other-lifer-row",
                span(lifers$common_name[j]),
                span(class = "other-lifer-time", format_app_time(lifers$latest_report[j]))
              )
            })
          )
        }
      }
      div(
        class = paste("location-item hotspot-item", if (expanded) "selected" else ""),
        role = "button", tabindex = "0",
        onclick = sprintf("Shiny.setInputValue('hotspot_click', '%s', {priority: 'event'});", loc_id),
        div(
          class = "hotspot-main",
          div(
            class = "hotspot-copy",
            div(class = "location-name", x$hotspot[i]),
            div(
              class = "location-meta",
              paste(c(
                format_app_time(x$obs_time[i]),
                count_text,
                paste0("about ", round(x$approx_miles[i]), " mi away"),
                lifer_count_text
              ), collapse = " · ")
            )
          ),
          tags$button(
            type = "button",
            class = paste("shortlist-toggle", if (shortlisted) "chosen" else ""),
            title = if (shortlisted) "Remove from comparison" else "Add to comparison",
            `aria-label` = if (shortlisted) "Remove from comparison" else "Add to comparison",
            onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('shortlist_toggle', '%s', {priority: 'event'});", loc_id),
            if (shortlisted) "✓" else "+"
          )
        ),
        hotspot_lifers
      )
    }))
  })

}

shinyApp(ui, server)
