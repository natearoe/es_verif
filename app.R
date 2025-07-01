
# library -----------------------------------------------------------------
library(shiny)
library(xml2)
library(rvest)
library(dplyr)
library(mapview)
library(soilDB)
library(shinyWidgets)
library(leaflet)
library(sf)
library(shinyjs)
library(DT)


# ui ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel("ES Verification QC"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "esid", label = "Ecological Site ID:"),
      tags$div(style = "margin-top: 12px;"),
      actionButton("fetch_button", "Fetch Data"),
      tags$div(style = "margin-top: 12px;"),

      ## pickerInput -------------------------------------------------------------


      shinyWidgets::pickerInput(inputId = "site_choices",
                                label = "Site",
                                choices = c("User Site ID", "Veg Plot Size",
                                            "Elevation", "Hillslope Profile",
                                            "Slope Position", "Slope Gradient",
                                            "Aspect", "Slope Shape (Across & Down)",
                                            "Geomorph Component", "Drainage Class",
                                            "Flooding Frequency Class", "Ponding Frequency Class",
                                            "Site Geomorphic Desc. Feature Type",
                                            "Site Geomorphic Desc. Feature Name Plural",
                                            "Ecological Site Hist. Ecological Site ID",
                                            "Ecological Site Hist. Ecological Site Name"),
                                multiple = TRUE,
                                selected = c("User Site ID", "Veg Plot Size",
                                             "Elevation", "Hillslope Profile",
                                             "Slope Position", "Slope Gradient",
                                             "Aspect", "Slope Shape (Across/Up-Down)",
                                             "Geomorph Component", "Site Geomorphic Desc. Feature Type",
                                             "Site Geomorphic Desc. Feature Name Plural",
                                             "Ecological Site Hist. Ecological Site ID",
                                             "Ecological Site Hist. Ecological Site Name")),
      shinyWidgets::pickerInput(inputId = "siteobs_choices",
                                label = "Site Observation",
                                choices = c("Observation Date",
                                            "Observation Data Kind",
                                            "Earth Cover Kind One",
                                            "Earth Cover Kind Two",
                                            "Associate Proj ID",
                                            "Associate Proj Name",
                                            "Ecological State ID",
                                            "Ecological State Name",
                                            "Community Phase ID",
                                            "Community Phase Name",
                                            "Pedoderm Class",
                                            "Biol. Crust Type Dom.",
                                            "Biol. Crust Type Sec.",
                                            "Crust Dev Class",
                                            "Local Disturb. Dist.",
                                            "Local Disturb. Desc.",
                                            "Site Soil Moist. Top Depth",
                                            "Site Soil Moist. Bottom Depth",
                                            "Site Soil Moist. Obs. Moist Status"),
                                multiple = TRUE,
                                selected = c("Observation Date",
                                             "Observation Data Kind",
                                             "Earth Cover Kind One",
                                             "Earth Cover Kind Two",
                                             "Associate Proj ID",
                                             "Associate Proj Name",
                                             "Ecological State ID",
                                             "Ecological State Name",
                                             "Community Phase ID",
                                             "Community Phase Name")),
      shinyWidgets::pickerInput(inputId = "vegplot_choices",
                                label = "Vegetation Plot",
                                choices = c("Vegetation Plot ID",
                                            "Primary Data Collector",
                                            "Observation Intensity",
                                            "Data Origin",
                                            "Soil Profile?",
                                            "Associated User Pedon ID",
                                            "QC Review Person",
                                            "QC Review Date",
                                            "QA Review Person",
                                            "QA Review Date",
                                            "Vegetation Plot Text",
                                            "Vegetation Plot Text Date",
                                            "Plot Sampling Protocol Name",
                                            "Vegetation Transect Protocol"),
                                multiple = TRUE,
                                selected = c("Vegetation Plot ID",
                                             "Primary Data Collector",
                                             "Observation Intensity",
                                             "Data Origin",
                                             "Soil Profile?",
                                             "Associated User Pedon ID",
                                             "QC Review Person",
                                             "QC Review Date",
                                             "QA Review Person",
                                             "QA Review Date",
                                             "Vegetation Plot Text",
                                             "Vegetation Plot Text Date",
                                             "Plot Sampling Protocol Name",
                                             "Vegetation Transect Protocol")),
      shinyWidgets::pickerInput(inputId = "pedon_choices",
                                label = "Pedon",
                                choices = c("User Pedon ID",
                                            "Pedon Tax. Hist. Classif. Date",
                                            "Pedon Tax. Hist. Taxon Name",
                                            "Pedon Tax. Hist. Text Date",
                                            "Pedon Tax. Hist. Text Author",
                                            "Pedon Tax. Hist. Text Entry"),
                                multiple = TRUE,
                                selected = c("User Pedon ID",
                                             "Pedon Tax. Hist. Classif. Date",
                                             "Pedon Tax. Hist. Taxon Name",
                                             "Pedon Tax. Hist. Text Date",
                                             "Pedon Tax. Hist. Text Author",
                                             "Pedon Tax. Hist. Text Entry")),
      dateRangeInput(inputId = "daterange", label = "Site Observation from:"),
      actionButton("filter_button", "Filter and Map Data")
    ),

    # mainPanel ---------------------------------------------------------------


    mainPanel(
      verbatimTextOutput("fetch_msg"),
      verbatimTextOutput("missing_coords_msg"),
      tabsetPanel(
        tabPanel("Map",
                 verbatimTextOutput("map_msg"),
                 leafletOutput("map_data", height = 700)
        ),
        tabPanel("Tabular",
                 verbatimTextOutput("map_msg"),
                 tags$div(style = "margin-top: 12px;"),
                 DT::dataTableOutput("tabular_disp")
        )
      )
    )
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session) {


  # fetchReact --------------------------------------------------------------

  fetch_out <- eventReactive(input$fetch_button, {
    # require esid
    req(input$esid)

    ## Web report
    # access through web report url
    url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifSiteDataByEcosite&es1=", input$esid)

    # Try to read the page
    page <- tryCatch({
      read_html(url)
    }, error = function(e) {
      return(list(
        tabular_data = NULL,
        geom_data = NULL,
        fetch_msg = paste("NASIS web report service appears to be down:", e$message)
      ))
    })

    # return early on read_html failure
    if (inherits(page, "list")) return(page)

    # Try to parse the table
    web_data <- tryCatch({
      html_node(page, "table") |> html_table(header = TRUE)
    }, error = function(e) {
      return(list(
        tabular_table = NULL,
        geom_data = NULL,
        fetch_msg = "No sites associated with this Ecological Site ID."
      ))
    })

    # return early on html_node error
    if (inherits(web_data, "list")) return(web_data)

    # access mapunits
    mu_data <- soilDB::fetchSDA_spatial(x = input$esid,
                                        by.col = "ecoclassid",
                                        method = "bbox")

    # check for SDA error
    if(inherits(mu_data, "try-error"))
      return(
        list(
          tabular_data = web_data,
          geom_data = NULL,
          fetch_msg = "Unable to access mapunit geometry - Soil Data Access appears to be down."
        ))
    # check for no mapunit data
    if(inherits(mu_data, "NULL"))
      return(
        list(
          tabular_data = web_data,
          geom_data = NULL,
          fetch_msg = "No mapunit geometry returned. Is the ecological site in SSURGO?"
        ))
    # return tabular and spatial data
    if(inherits(mu_data, "data.frame")) {
      mu_data <- mu_data |>
        sf::st_as_sf(coords = c("longstddecimaldegrees",
                                "latstddecimaldegrees"),
                     crs = 4326)

      return(
        list(
          tabular_data = web_data,
          geom_data = mu_data,
          fetch_msg = paste("Data successfully retrieved for Ecological Site ID:", input$esid)
        ))
    }

  })


  # filterReact -------------------------------------------------------------

  filter_out <- eventReactive(input$filter_button, {

    tabular_data <- fetch_out()$tabular_data
    mu_data <- fetch_out()$geom_data

    if(inherits(tabular_data, "NULL")) {
      return(
        list(
          map_data = NULL,
          map_msg = "Map not generated when site data is missing.",
          missing_coords_msg = NULL
        )
      )

    }

    if(inherits(fetch_out()$geom_data, "NULL")) {
      return(
        list(
          map_data = NULL,
          map_msg = "Map not generated when mapunit data is missing.",
          missing_coords_msg = NULL
        )
      )
    }

    # check for missing coordinates
    missing_data <- tabular_data |> dplyr::filter(is.na(longstddecimaldegrees) | is.na(latstddecimaldegrees))

    missing_message <- if (nrow(missing_data) > 0) {
      paste("The following sites are missing WGS84 coordinates and were removed:",
            paste(missing_data$usiteid, collapse = ", "))
    } else {
      NULL
    }

    # check for missing values
    selected_fields <- c(input$site_choices,
                         input$siteobs_choices,
                         input$vegplot_choices,
                         input$pedon_choices)

    # user choice mapping to web_data
    field_mapping <- list("User Site ID" = "usiteid",
                          "Veg Plot Size" = "vegplotsize",
                          "Elevation" = "elev",
                          "Hillslope Profile" = "hillslopeprof",
                          "Slope Position" = "geomslopeseg",
                          "Slope Gradient" = "slope",
                          "Aspect" = "aspect",
                          "Slope Shape (Across & Down)" = c("shapeacross",
                                                            "shapedown"),
                          "Geomorph Component" = c("geomposhill",
                                                   "geomposmntn",
                                                   "geompostrce",
                                                   "geomposflats"),
                          "Drainage Class" = "drainagecl",
                          "Flooding Frequency Class" = "flodfreqcl",
                          "Ponding Frequency Class" = "pondfreqcl",
                          "Site Geomorphic Desc. Feature Type" = "geomfname",
                          "Site Geomorphic Desc. Feature Name Plural" = "geomfnamep",
                          "Ecological Site Hist. Ecological Site ID" = "ecositeid",
                          "Ecological Site Hist. Ecological Site Name" = "ecositenm",
                          "Observation Date" = "obsdate",
                          "Observation Data Kind" = "obsdatekind",
                          "Earth Cover Kind One" = "earthcovkind1",
                          "Earth Cover Kind Two" = "earthcovkind2",
                          "Associate Proj ID" = "p.uprojectid",
                          "Associate Proj Name" = "p.projectname",
                          "Ecological State ID" = "ecostateid",
                          "Ecological State Name" = "ecostatename",
                          "Community Phase ID" = "commphaseid",
                          "Community Phase Name" = "commphasename",
                          "Pedoderm Class" = "pedodermclass",
                          "Biol. Crust Type Dom." = "biolcrusttypedom",
                          "Biol. Crust Type Sec." = "biolcrusttypesecond",
                          "Crust Dev Class" = "crustdevcl",
                          "Local Disturb. Dist." = "localdisturbancedistance",
                          "Local Disturb. Desc." = "localdisturbancedescription",
                          "Site Soil Moist. Top Depth" = "soilmoistdept",
                          "Site Soil Moist. Bottom Depth" = "soilmoistdepb",
                          "Site Soil Moist. Obs. Moist Status" = "obssoilmoiststat",
                          "Vegetation Plot ID" = "vegplotid",
                          "Primary Data Collector" = "primarydatacollector",
                          "Observation Intensity" = "obsintensity",
                          "Data Origin" = "vegdataorigin",
                          "Soil Profile?" = "silprofileindicator",
                          "Associated User Pedon ID" = "assocuserpedonid",
                          "QC Review Person" = "qcreviewperson",
                          "QC Review Date" = "qcreviewdate",
                          "QA Review Person" = "qareviewperson",
                          "QA Review Date" = "qareviewdate",
                          "Vegetation Plot Text" = "vt.textentry",
                          "Vegetation Plot Text Date" = "vt.recdate",
                          "Plot Sampling Protocol Name" = "psp.plotsampprotocolname",
                          "Vegetation Transect Protocol" = "tsp.transsampprotocolname",
                          "User Pedon ID" = "upedonid",
                          "Pedon Tax. Hist. Classif. Date" = "ptaxhistclassdate",
                          "Pedon Tax. Hist. Taxon Name" = "ptaxhisttaxonname",
                          "Pedon Tax. Hist. Text Date" = "pedontextrecdate",
                          "Pedon Tax. Hist. Text Author" = "pedontextrecauthor",
                          "Pedon Tax. Hist. Text Entry" = "pedontextentry")

    # determine required columns
    required_columns <- unlist(field_mapping[selected_fields])

    # assess completeness
    completeness_matrix <- sapply(selected_fields, FUN = function(field){
      cols <- field_mapping[[field]]

      if(length(cols) == 1){
        # 1:1 mapping - single required column must have some data
        !is.na(tabular_data[[cols]])
      } else {
        # multi-column mapping - only one column needs to be populated
        rowSums(!is.na(tabular_data[, cols, drop = FALSE])) > 0
      }

    }) |> as.matrix()

    # store completeness in column
    tabular_data$is_complete <- apply(completeness_matrix, 1, all)

    ## assess data range

    # Extract the most recent date per string
    tabular_data$most_recent_date <- lapply(tabular_data$obsdate, function(x) {
      # Split on commas
      parts <- strsplit(x, ",\\s*")[[1]]

      # Convert to Date (extract just the date part)
      dates <- as.Date(sub(" .*", "", parts), format = "%m/%d/%Y")

      # Return the most recent
      max(dates, na.rm = TRUE)
    }) |> unlist()

    web_data <- tabular_data  |>
      dplyr::filter(most_recent_date >= input$daterange[1],
                    most_recent_date <= input$daterange[2])

    # Replace NA with empty strings just for visual
    tab_display <- tabular_data |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    tab_display[is.na(tab_display)] <- ""

    # Create DT table
    df_disp <- DT::datatable(tab_display, options = list(scrollX = TRUE)) |>
      # Highlight required fields if they were NA (now empty string)
      DT::formatStyle(
        columns = required_columns,
        valueColumns = required_columns,
        backgroundColor = styleEqual("", 'mistyrose')
      )

    # set color palette
    pal <- colorFactor(
      palette = c("#d95f02", "#1b9e77"),
      domain = c(FALSE, TRUE)
    )

    ## createMap ---------------------------------------------------------------

    my_map  <- web_data |> dplyr::filter(!is.na(longstddecimaldegrees) & !is.na(latstddecimaldegrees)) |>
      sf::st_as_sf(coords = c("longstddecimaldegrees",
                              "latstddecimaldegrees"),
                   crs = st_crs(4326)) |>
      leaflet::leaflet() |>
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap")  |>
      addProviderTiles("Esri.WorldImagery", group = "Esri Satellite")  |>
      addProviderTiles("Esri.WorldTopoMap", group = "Esri Topo")  |>

      addCircleMarkers(popup = ~usiteid,
                       radius = 5,
                       color = ~pal(is_complete),
                       group = "Points") |>
      addPolygons(data = mu_data,
                  fillColor = "#7570b3",
                  color = "#7570b3",
                  opacity = 0.4,
                  group = "Polygons") |>
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Esri Satellite", "Esri Topo"),
        overlayGroups = c("Polygons", "Points"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addLegend(position = "bottomright",
                pal = pal,
                values = ~is_complete,
                title = "Meets Verification")




    # my_map <- my_map@map

    list(
      map_data = my_map,
      map_msg = NULL,
      missing_coords_msg = missing_message,
      tabular_disp = df_disp
    )

  })


  # output ------------------------------------------------------------------


  output$fetch_msg <- renderText({
    req(fetch_out())
    fetch_out()$fetch_msg
  })

  output$map_msg <- renderText({
    req(filter_out())
    filter_out()$map_msg
  })

  output$tabular_disp <- DT::renderDataTable({
    req(fetch_out())
    filter_out()$tabular_disp
  })

  output$missing_coords_msg <- renderText({
    req(filter_out())
    filter_out()$missing_coords_msg
  })

  output$map_data <- renderLeaflet({
    req(filter_out())
    filter_out()$map_data
  })

}

shinyApp(ui, server)
