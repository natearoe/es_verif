# library -----------------------------------------------------------------
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
library(httr)
library(jsonlite)
library(stringr)
library(shiny)
library(ggplot2)
library(stats)
library(tidyr)

# ui ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel("ES Verification QC"),
  sidebarLayout(
    sidebarPanel(
      helpText("Instructions:"),
      helpText("1. Enter an Ecological Site ID (Section A)"),
      helpText("2. Click 'Fetch Data' and wait for load (Section A)"),
      helpText("3. Choose required fields (Section B)"),
      helpText("4. Choose filtering criteria, if desired (Section C)"),
      helpText("5. Click 'Filter and Display Data'"),

      hr(style = "border-top: 1px solid #2f2f2f; opacity: 1; margin: 8px 0;"),

      tags$h5("Section A:", style = "text-decoration: underline;"),


      fluidRow(
        column(6,
               pickerInput(inputId = "mlra", label = "MLRA:",
                           choices = NULL, options = list(
                             title = "Choose MLRA",
                             `live-search` = TRUE
                           ))),
        column(6,
               pickerInput(inputId = "esid", label = "Ecological Site ID:",
                           choices = character(0),
                           options = list(
                             title = "Choose Ecological Site ID...",
                             `live-search` = TRUE
                           )))
      ),

      actionButton("fetch_button", "Fetch Data by Selection"),

      tags$h5("OR",
              style = "text-decoration: underline; text-align: center;"
              ),


      textInput(inputId = "esid", label = "Ecological Site ID:",
                placeholder = "e.g., R018XI101"
                ),
      actionButton("fetch_button", "Fetch Data by Ecological Site ID"),

      hr(style = "border-top: 1px solid #2f2f2f; opacity: 1; margin: 8px 0;"),
      tags$h5("Section B:", style = "text-decoration: underline;"),


      ## pickerInput -------------------------------------------------------------


      shinyWidgets::pickerInput(inputId = "site_choices",
                                label = "Site",
                                choices = list("User Site ID", "Veg Plot Size",
                                            "Elevation", "Hillslope Profile",
                                            "Slope Position", "Slope Gradient",
                                            "Aspect", "Slope Shape Across","Slope Shape Down",
                                            "Geomorph Component" = c("Hills", "Mountains", "Terrace", "Flats"),
                                            "Drainage Class",
                                            "Flooding Frequency Class", "Ponding Frequency Class",
                                            "Site Geomorphic Desc. Feature Type",
                                            "Site Geomorphic Desc. Feature Name Plural",
                                            "Ecological Site Hist. Ecological Site ID",
                                            "Ecological Site Hist. Ecological Site Name",
                                            "Observation Date",
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
                                            "Site Soil Moist. Obs. Moist Status",
                                            "User Pedon ID",
                                            "Pedon Tax. Hist. Classif. Date",
                                            "Pedon Tax. Hist. Taxon Name",
                                            "Pedon Tax. Hist. Text Date",
                                            "Pedon Tax. Hist. Text Author",
                                            "Pedon Tax. Hist. Text Entry"),
                                multiple = TRUE,
                                selected = c("User Site ID", "Veg Plot Size",
                                             "Elevation", "Hillslope Profile",
                                             "Slope Position", "Slope Gradient",
                                             "Aspect", "Slope Shape Across","Slope Shape Down",
                                             "Hills", "Mountains", "Terrace", "Flats",
                                             "Site Geomorphic Desc. Feature Type",
                                             "Site Geomorphic Desc. Feature Name Plural",
                                             "Ecological Site Hist. Ecological Site ID",
                                             "Ecological Site Hist. Ecological Site Name",
                                             "Observation Date",
                                             "Observation Data Kind",
                                             "Earth Cover Kind One",
                                             "Earth Cover Kind Two",
                                             "Associate Proj ID",
                                             "Associate Proj Name",
                                             "Ecological State ID",
                                             "Ecological State Name",
                                             "Community Phase ID",
                                             "Community Phase Name",
                                             "User Pedon ID",
                                             "Pedon Tax. Hist. Classif. Date",
                                             "Pedon Tax. Hist. Taxon Name",
                                             "Pedon Tax. Hist. Text Date",
                                             "Pedon Tax. Hist. Text Author",
                                             "Pedon Tax. Hist. Text Entry"),
 #                               selected = NULL,
                                options = shinyWidgets::pickerOptions(
                                  title = "test"
                                )),
      shinyWidgets::pickerInput(inputId = "vegplot_choices",
                                label = "Veg Plot",
                                choices = list("Vegetation Plot ID",
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
                                            "Vegetation Transect Protocol",
                                            "Total Overstory Canopy" = c("Total Overstory Canopy Cover Percent",
                                                                         "Total Overstory Canopy Cover Class"),
                                            "Total Canopy Cover" = c("Total Canopy Cover Percent",
                                                                     "Total Canopy Cover Class"),
                                            "Basal Area Plot Total",
                                            "Basal Area Assessment Method",
                                            "Plot Sampling Protocol",
                                            "Transect Sampling Protocol"),
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
                                             "Vegetation Transect Protocol",
                                             "Plot Sampling Protocol",
                                             "Total Canopy Cover Percent",
                                             "Total Canopy Cover Class")),
      shinyWidgets::pickerInput(inputId = "ppi_choices",
                                label = "Plot Plant Inv.",
                                choices = list("Plant Symbol",
                                               "Abundance" = c("Species Canopy Cover Class",
                                                               "Species Canopy Cover Percent",
                                                               "Species Trace Amount Flag",
                                                               "AK Stratum Cover Class Percent"),
                                               "Strata" = c("Height Class Upper",
                                                            "Height Class Lower",
                                                            "Plant Type Group",
                                                            "Vegetation Strata Level",
                                                            "AK Stratum Cover Class"),
                                               "Estimated Annual Production"),
                                multiple = TRUE,
                                selected = c("Plant Symbol",
                                             "Species Canopy Cover Class",
                                             "Species Canopy Cover Percent",
                                             "Species Trace Amount Flag",
                                             "AK Stratum Cover Class Percent",
                                             "AK Stratum Cover Class",
                                             "Height Class Upper",
                                             "Height Class Lower",
                                             "Plant Type Group",
                                             "Vegetation Strata Level")),
      shinyWidgets::pickerInput(inputId = "vegtrans_choices",
                                label = "Veg Transect",
                                choices = c("Veg Transect ID",
                                            "Transect Azimuth",
                                            "Transect Length",
                                            "LPI Observation Interval",
                                            "Total Number of Points Sampled"),
                                multiple = TRUE,
                                selected = c("Veg Transect ID",
                                             "Transect Azimuth",
                                             "Transect Length",
                                             "LPI Observation Interval",
                                             "Total Number of Points Sampled")),
      shinyWidgets::pickerInput(inputId = "vegtranssum_choices",
                                label = "Veg Transect Summary",
                                choices = c("Plant Symbol",
                                            "Number of Species Foliar Cover Hit",
                                            "Foliar Cover Pct Line Int",
                                            "Number Species Basal Cover Hits",
                                            "Basal Cover Pct Line Int"),
                                multiple = TRUE,
                                selected = c("Plant Symbol",
                                             "Number of Species Foliar Cover Hit",
                                             "Foliar Cover Pct Line Int",
                                             "Number Species Basal Cover Hits",
                                             "Basal Cover Pct Line Int")),

      hr(style = "border-top: 1px solid #2f2f2f; opacity: 1; margin: 8px 0;"),

 tags$h5("Section C:", style = "text-decoration: underline;"),



      dateRangeInput(inputId = "daterange", label = "Site Observation from:",
                     start = as.Date("1912-04-15")),
      fluidRow(
        column(6,
               textInput(inputId = "stateid", label = "State ID:")),
        column(6,
               textInput(inputId = "phaseid", label = "Community Phase ID:"))
      ),
      textInput(inputId = "usiteid", label = "User Site ID:"),
      actionButton("filter_button", "Filter and Map Data")
    ),

    # mainPanel ---------------------------------------------------------------


    mainPanel(
      verbatimTextOutput("fetch_msg"),
      verbatimTextOutput("missing_coords_msg"),
      tabsetPanel(
        tabPanel("Ecosite",
                   verbatimTextOutput("ecosite_msg"),
                   verbatimTextOutput("ecosite_comp_msg"),
                   verbatimTextOutput("ecosite_statephase_msg"),
                   verbatimTextOutput("ecosite_data"),
                   verbatimTextOutput("stm_not_site"),
                   verbatimTextOutput("site_not_stm"),
                   textOutput("summary_title", container = function(...) tags$h5(style = "text-decoration: underline; margin: 8px 0;", ...)),
                   tableOutput("summary_table"),
                   textOutput("statephase_title", container = function(...) tags$h5(style = "text-decoration: underline; margin: 8px 0;", ...)),
                   DT::dataTableOutput("statephase_table"),
                   tags$br(),
                   fluidRow(
                     column(6, plotOutput("site_fig", height = 250)),
                     column(6, plotOutput("vegplot_fig", height = 250))
                   ),
                   tags$br(),
                   fluidRow(
                     column(6, plotOutput("ppi_fig", height = 250)),
                     column(6, plotOutput("vegtrans_fig", height = 250))
                   ),
                   tags$br(),
                   fluidRow(
                     column(6, plotOutput("vegtranssum_fig", height = 250))
                   )

        ),
        tabPanel("Map",
                 verbatimTextOutput("map_msg"),
                 downloadButton("download_map", "Download Map"),
                 leafletOutput("map_data", height = 700)
        ),
        tabPanel("Site",
                 verbatimTextOutput("site_msg"),
                 downloadButton("download_site_data", "Download Site Data (.csv)"),
                 # verbatimTextOutput("map_msg"),
                 DT::dataTableOutput("site_data")
        ),

        tabPanel("Veg Plot",
                 verbatimTextOutput("vp_msg"),
                 downloadButton("download_vegplot_data", "Download Veg Plot Data (.csv)"),
                 DT::dataTableOutput("vp_data")
                 # verbatimTextOutput("map_msg"),
                 # DT::dataTableOutput("tabular_disp")
        ),
        tabPanel("Plot Plant Inv.",
                 verbatimTextOutput("ppi_msg"),
                 downloadButton("download_ppi_data", "Download Plot Plant Inv. Data (.csv)"),
                 DT::dataTableOutput("ppi_data")
                 # verbatimTextOutput("map_msg"),
                 # DT::dataTableOutput("tabular_disp")
        ),
        tabPanel("Veg Transect",
                 verbatimTextOutput("vegtrans_msg"),
                 downloadButton("download_vegtrans_data", "Download Veg Transect Data (.csv)"),
                 DT::dataTableOutput("veg_trans_data")
                 # verbatimTextOutput("map_msg"),
                 # DT::dataTableOutput("tabular_disp")
        ),
        tabPanel("Veg Transect Summary",
                 verbatimTextOutput("vegtranssum_msg"),
                 downloadButton("download_vegtranssum_data", "Download Veg Transect Summary Data (.csv)"),
                 DT::dataTableOutput("veg_trans_sum_data")
                 # verbatimTextOutput("map_msg"),
                 # DT::dataTableOutput("tabular_disp")
        )
      )
    )
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session) {

  # pull MLRAs from EDIT
  mlra_choices <- read.table("https://edit.jornada.nmsu.edu/services/downloads/esd/geo-unit-list.txt",
                      sep="\t", quote = "\"", header=TRUE, skip=2) |>
    dplyr::mutate(mlra_name = paste(MLRA, MLRA.name, sep = " - ")) |> dplyr::pull(mlra_name)

  # update MLRA choice list with line above
  observe({
    updatePickerInput(
      session, "mlra",
      choices  = unique(mlra_choices),
      selected = NULL
    )
  })

  # update ESID choice list based on MLRA selection
  esid_df <- reactive({
    req(input$mlra)
    soilDB::get_EDIT_ecoclass_by_geoUnit(geoUnit = substr(input$mlra, 1, 4), catalog = "esd") |>
      dplyr::select(id, name)
  })

  observeEvent(input$mlra, {
    req(input$mlra)
    choices <- stats::setNames(esid_df()$id, paste(esid_df()$id, esid_df()$name))

    updatePickerInput(
      session, "esid",
      choices  = choices,
      selected = NULL
    )
  })



  # logic for clearing results after new fetch

  # initialize
  clear_after_fetch <- reactiveVal(TRUE)

  # flip switch on fetch - preventing output
  observeEvent(input$fetch_button, ignoreInit = TRUE, {
    # User fetched (again) -> blank filter-driven outputs
    clear_after_fetch(TRUE)
  })

  # flip switch on filter - allow output
  observeEvent(input$filter_button, ignoreInit = TRUE, {
    # User filtered -> show filter-driven outputs
    clear_after_fetch(FALSE)
  })


  # fetchReact --------------------------------------------------------------

  fetch_out <- eventReactive(input$fetch_button, {

    withProgress(message = "Fetching data...", value = 0, min = 0, max = 1, {


      # require esid
      req(input$esid)

      fetch_results <- list(ecosite_data = list(comp_data = NULL,
                                                statephase_data = NULL),
                            site_data = NULL,
                            vegplot_data = list(vplot_data = NULL,
                                                ppi_data = NULL,
                                                vegtrans_data = NULL,
                                                vegtranssum_data = NULL),
                            geom_data = NULL,
                            fetch_msg = list(ecosite = list(comp = NULL,
                                                            statephase = NULL),
                                             site = NULL,
                                             vegplot = list(vplot = NULL,
                                                            ppi = NULL,
                                                            vegtrans = NULL,
                                                            vegtranssum = NULL),
                                             geom = NULL),
                            fetch_msg_sum = NULL)

      # web reports






      # Ecosite data ------------------------------------------------------------

      incProgress(1/7, detail = "Fetching ecosite data...")

      # check if esid has a space - it should not
      if (grepl("\\s", input$esid)) {
        stop("Invalid input: 'Ecological Site ID' cannot contain spaces.")
      }

      # ecosite data includes the number of comps web report, accessing states/phases
      # from EDIT, and accessing states/phases from site data

      ## Web report: numb comps --------------------------------------------------

      url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifNumbComps&es1=", input$esid)
      # comp_numb <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifNumbComps&es1=", "R018XI101CA")

      # Try to read the page
      page <- tryCatch({
        rvest::read_html(url)
      }, error = function(e) {
        return(
          paste("NASIS web report service appears to be down:", e$message)
        )
      })

      # return early on read_html failure
      if (inherits(page, "character")){
        fetch_results[["fetch_msg"]][["ecosite"]][["comp"]] = page
      }

      # Try to parse the table
      if (inherits(page, "xml_document")){
        web_data <- tryCatch({
          rvest::html_node(page, "table") |> html_table(header = TRUE)
        }, error = function(e) {
          return(
            "No components associated with this Ecological Site ID."
          )
        })

        if (inherits(web_data, "character")){
          fetch_results[["fetch_msg"]][["ecosite"]][["comp"]] = page
        }
        else {fetch_results[["ecosite_data"]][["comp_data"]] = web_data}
      }


      ## EDIT API: access states/phases ------------------------------------------

      mlra <- stringr::str_extract(input$esid, "(?<=[:alpha:])[:digit:]{3}[:alpha:]{1}")
      edit_api <- paste0("https://edit.jornada.nmsu.edu/services/models/esd/", mlra, "/",
                         input$esid, "/states.json")

      # mlra <- stringr::str_extract("R018XI101CA", "(?<=[:alpha:])[:digit:]{3}[:alpha:]{1}")
      #
      # edit_api <- paste0("https://edit.jornada.nmsu.edu/services/models/esd/", mlra, "/",
      #                    "R018XI101CA", "/states.json")


      # Make GET request to the API
      response <- GET(edit_api)

      # Check if the request was successful (status code 200)
      if (status_code(response) == 200) {
        # Parse the JSON content
        content_list <- content(response, as = "text", encoding = "UTF-8")
        data <- fromJSON(content_list, flatten = TRUE)

        # store results
        fetch_results$ecosite_data$statephase_data  <- data$states |>
          dplyr::select(type, state, community, narratives.name)


      } else {
        fetch_results$fetch_msg$ecosite$statephase <- cat("Request failed with status code:", status_code(response), "\n")
      }

      # Site data ---------------------------------------------------------------

      incProgress(1/7, detail = "Fetching site data...")
      # site data all comes from one web report

      ## Web report: site data ---------------------------------------------------

      url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifSiteDataByEcosite&es1=", input$esid)
      # url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifSiteDataByEcosite&es1=", "R018XI101CA")

      # Try to read the page
      page <- tryCatch({
        rvest::read_html(url)
      }, error = function(e) {
        return(
          paste("NASIS web report service appears to be down:", e$message)
        )
      })

      # return early on read_html failure
      if (inherits(page, "character")){
        fetch_results[["fetch_msg"]][["site"]] = page
      }

      # Try to parse the table
      if (inherits(page, "xml_document")){
        web_data <- tryCatch({
          rvest::html_node(page, "table") |> html_table(header = TRUE)
        }, error = function(e) {
          return(
            "No sites associated with this Ecological Site ID."
          )
        })

        if (inherits(web_data, "character")){
          fetch_results[["fetch_msg"]][["site"]] = web_data
        }
        else {fetch_results[["site_data"]] = web_data |> dplyr::filter(ecositeid == input$esid)}
      }


      # Geom data ---------------------------------------------------------------

      incProgress(1/7, detail = "Fetching map unit geometry...")
      # geom data comes from one SDA query

      ## SDA query ---------------------------------------------------------------

      # access mapunits
      mu_data <- soilDB::fetchSDA_spatial(x = input$esid,
                                          by.col = "ecoclassid",
                                          method = "bbox")

      # check for SDA error
      if(inherits(mu_data, "try-error")){
        fetch_results[["fetch_msg"]][["geom"]] = "Unable to access mapunit geometry - Soil Data Access appears to be down."
      }
      # check for no mapunit data
      if(inherits(mu_data, "NULL")){
        fetch_results[["fetch_msg"]][["geom"]] = "No mapunit geometry returned. Is the ecological site in SSURGO?"

      }

      # return tabular and spatial data
      if(inherits(mu_data, "data.frame")) {
        mu_data <- mu_data |>
          sf::st_as_sf(coords = c("longstddecimaldegrees",
                                  "latstddecimaldegrees"),
                       crs = 4326)

        fetch_results[["geom_data"]] = mu_data

      }


      # Veg Plot ----------------------------------------------------------------

      incProgress(1/7, detail = "Fetching vegetation plot data")
      ## Web report: veg plot ----------------------------------------------------

      url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifVegPlot&es1=", input$esid)
      # url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifVegPlot&es1=", "R018XI101CA")

      # Try to read the page
      page <- tryCatch({
        rvest::read_html(url)
      }, error = function(e) {
        return(
          paste("NASIS web report service appears to be down:", e$message)
        )
      })

      # return early on read_html failure
      if (inherits(page, "character")){
        fetch_results[["fetch_msg"]][["vegplot"]][["vplot"]] = page
      }

      # Try to parse the table
      if (inherits(page, "xml_document")){
        web_data <- tryCatch({
          rvest::html_node(page, "table") |> html_table(header = TRUE)
        }, error = function(e) {
          return(
            "No data associated with this Ecological Site ID."
          )
        })

        if (inherits(web_data, "character")){
          fetch_results[["fetch_msg"]][["vegplot"]][["vplot"]] = web_data
        }
        else {fetch_results[["vegplot_data"]][["vplot_data"]] = web_data |> dplyr::filter(ecositeid == input$esid)}
      }

      # Plot Plant Inventory -----------------------------------------------------------

      incProgress(1/7, detail = "Fetching plot plant inventory data...")
      ## Web report: plot plant inventory ----------------------------------------


      url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifPPI&es1=", input$esid)
      # url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifPPI&es1=", "R018XI101CA")

      # Try to read the page
      page <- tryCatch({
        rvest::read_html(url)
      }, error = function(e) {
        return(
          paste("Plot Plant Inventory: NASIS web report service appears to be down:", e$message)
        )
      })

      # return early on read_html failure
      if (inherits(page, "character")){
        fetch_results[["fetch_msg"]][["vegplot"]][["ppi"]] = page
      }

      # Try to parse the table
      if (inherits(page, "xml_document")){
        web_data <- tryCatch({
          rvest::html_node(page, "table") |> html_table(header = TRUE)
        }, error = function(e) {
          return(
            "Plot Plant Inventory: No data associated with this Ecological Site ID."
          )
        })

        if (inherits(web_data, "character")){
          fetch_results[["fetch_msg"]][["vegplot"]][["ppi"]] = web_data
        }
        else {fetch_results[["vegplot_data"]][["ppi_data"]] = web_data |> dplyr::filter(ecositeid == input$esid)}
      }


      # Veg trans ---------------------------------------------------------------

      incProgress(1/7, detail = "Fetching vegetation transect data...")
      ## Web report: veg trans ---------------------------------------------------

      url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifVegTrans&es1=", input$esid)
      # vtrans <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifVegTrans&es1=", "R018XI101CA")

      # Try to read the page
      page <- tryCatch({
        rvest::read_html(url)
      }, error = function(e) {
        return(
          paste("NASIS web report service appears to be down:", e$message)
        )
      })

      # return early on read_html failure
      if (inherits(page, "character")){
        fetch_results[["fetch_msg"]][["vegplot"]][["vegtrans"]] = page
      }

      # Try to parse the table
      if (inherits(page, "xml_document")){
        web_data <- tryCatch({
          rvest::html_node(page, "table") |> html_table(header = TRUE)
        }, error = function(e) {
          return(
            "No data associated with this Ecological Site ID."
          )
        })

        if (inherits(web_data, "character")){
          fetch_results[["fetch_msg"]][["vegplot"]][["vegtrans"]] = web_data
        }
        else {fetch_results[["vegplot_data"]][["vegtrans_data"]] = web_data |> dplyr::filter(ecositeid == input$esid)}
      }


      # Veg Trans Summary -------------------------------------------------------

      incProgress(1/7, detail = "Fetching transect summary data...")
      # Web report: veg trans sum -----------------------------------------------

      url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifVegTransSummary&es1=", input$esid)
      # url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifVegTransSummary&es1=", "F018XI201CA")

      # Try to read the page
      page <- tryCatch({
        rvest::read_html(url)
      }, error = function(e) {
        return(
          paste("NASIS web report service appears to be down:", e$message)
        )
      })

      # return early on read_html failure
      if (inherits(page, "character")){
        fetch_results[["fetch_msg"]][["vegplot"]][["vegtranssum"]] = page
      }

      # Try to parse the table
      if (inherits(page, "xml_document")){
        web_data <- tryCatch({
          rvest::html_node(page, "table") |> html_table(header = TRUE)
        }, error = function(e) {
          return(
            "No data associated with this Ecological Site ID."
          )
        })

        if (inherits(web_data, "character")){
          fetch_results[["fetch_msg"]][["vegplot"]][["vegtranssum"]] = web_data
        }
        else {fetch_results[["vegplot_data"]][["vegtranssum_data"]] = web_data |> dplyr::filter(ecositeid == input$esid)}
      }
      # Return data -------------------------------------------------------------

      # Flatten all nested messages into a vector
      messages <- unlist(fetch_results$fetch_msg, recursive = TRUE, use.names = TRUE)

      # Safely coerce to character, avoiding pointer garbage
      messages <- sapply(messages, function(x) {
        if (is.null(x)) return(NA_character_)
        if (inherits(x, "xml_document") || inherits(x, "xml_node")) return("<HTML document>")
        as.character(x)
      }, USE.NAMES = TRUE)


      # Keep only non-NULL messages
      error_msgs <- messages[!sapply(messages, is.null)]

      pretty_names <- c(
        "ecosite.comp" = "Ecosite Components",
        "ecosite.statephase" = "Ecosite States/Phases",
        "site" = "Site Data",
        "vegplot.vplot" = "Veg Plot",
        "vegplot.ppi" = "Plot Plant Inventory",
        "vegplot.vegtrans" = "Veg Transect",
        "vegplot.vegtranssum" = "Veg Transect Summary",
        "geom" = "Geometry"
      )

      # Build fetch_msg_sum
      if (length(error_msgs) == 0) {
        fetch_results$fetch_msg_sum <- paste("✅ Data successfully retrieved for Ecological Site ID:", input$esid)
      } else {
        fetch_results$fetch_msg_sum <- paste(
          "⚠️ Some data failed to load:\n",
          paste(paste0("- ", pretty_names[names(error_msgs)], ": ", error_msgs), collapse = "\n")
        )
      }


      return(fetch_results)

    })

  })

  # filterReact -------------------------------------------------------------

  filter_out <- eventReactive(input$filter_button, {


  ## Filter Prep -------------------------------------------------------------



    ### Field mapping -----------------------------------------------------------

    # user choice mapping to web_data
    field_mapping <- list("User Site ID" = "usiteid",
                          "Veg Plot Size" = "vegplotsize",
                          "Elevation" = "elev",
                          "Hillslope Profile" = "hillslopeprof",
                          "Slope Position" = "geomslopeseg",
                          "Slope Gradient" = "slope",
                          "Aspect" = "aspect",
                          "Slope Shape Across" = "shapeacross",
                          "Slope Shape Down" =  "shapedown",
                          "Geom Comp" = c("Hills" = "geomposhill",
                                          "Mountains" = "geomposmntn",
                                          "Terrace" = "geompostrce",
                                          "Flats" = "geomposflats"),
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
                          "Total Overstory Canopy" = c("Total Overstory Canopy Cover Percent" = "overstorycancontotalpct",
                                                       "Total Overstory Canopy Cover Class" = "overstorycancovtotalclass"),
                          "Total Canopy Cover" = c("Total Canopy Cover Percent" = "cancovtotalpct",
                                                   "Total Canopy Cover Class" = "cancovtotalclass"),
                          "Basal Area Plot Total" = "basalareaplottotal",
                          "Basal Area Assessment Method" = "basalareaassessmethod",
                          "Plot Sampling Protocol" = "psp.plotsampprotocolname",
                          "Transect Sampling Protocol" = "tsp.transsampprotocolname",
                          "Vegetation Transect Protocol" = "tsp.transsampprotocolname",
                          "User Pedon ID" = "upedonid",
                          "Pedon Tax. Hist. Classif. Date" = "ptaxhistclassdate",
                          "Pedon Tax. Hist. Taxon Name" = "ptaxhisttaxonname",
                          "Pedon Tax. Hist. Text Date" = "pedontextrecdate",
                          "Pedon Tax. Hist. Text Author" = "pedontextrecauthor",
                          "Pedon Tax. Hist. Text Entry" = "pedontextentry",
                          "Plant Symbol" = "plantsym",
                          "Abundance" = c("Species Canopy Cover Class" = "speciescancovclass",
                                          "Species Canopy Cover Percent" = "speciescancovpct",
                                          "Species Trace Amount Flag" = "speciestraceamtflag",
                                          "AK Stratum Cover Class Percent" = "akstratumcoverclasspct"),
                          "Strata" = c("Height Class Upper" = "livecanopyhttop",
                                       "Height Class Lower" = "livecanopyhtbottom",
                                       "Plant Type Group" = "planttypegroup",
                                       "Vegetation Strata Level" = "vegetationstratalevel",
                                       "AK Stratum Cover Class" = "akstratumcoverclass"),
                          "Estimated Annual Production" = "estannualprod",
                          "Veg Transect ID" = "vegtransectid",
                          "Transect Azimuth" = "transectazimuth",
                          "Transect Length" = "transectlength",
                          "LPI Observation Interval" = "lpiobsinterval",
                          "Total Number of Points Sampled" = "totalpointssampledcount",
                          "Number of Species Foliar Cover Hit" = "speciesfoliarcovhitcount",
                          "Foliar Cover Pct Line Int" = "speciesfoliarcovpctlineint",
                          "Number Species Basal Cover Hits" = "speciesbasalcovhitcount",
                          "Basal Cover Pct Line Int" = "speciesbasalcovpctlineint")

    # flatten field mapping
    field_mapping_flat <- unlist(field_mapping)

    # remove the concatenated text (e..g, want Hills not Geom Comp.Hills)
    names(field_mapping_flat) <- sub("Geom Comp\\.", "", names(field_mapping_flat))
    names(field_mapping_flat) <- sub("Abundance\\.", "", names(field_mapping_flat))
    names(field_mapping_flat) <- sub("Strata\\.", "", names(field_mapping_flat))
    names(field_mapping_flat) <- sub("Total Overstory Canopy\\.", "", names(field_mapping_flat))
    names(field_mapping_flat) <- sub("Total Canopy Cover\\.", "", names(field_mapping_flat))

    # build a parent map
    parent_map <- setNames(
      rep(names(field_mapping), lengths(field_mapping)),
      unname(unlist(field_mapping, use.names = FALSE))
    )

    ### Return list build -------------------------------------------------------

    return_list <- list(ecosite_return = list(comp_req = NULL,
                                              stm = list(sites_not_stm = NULL,
                                                         stm_not_sites = NULL)),
                        ppi_data = NULL)


    ### Build Figs NULL as default ----------------------------------------------

    site_fig <- NULL
    vegplot_fig <- NULL
    ppi_fig <- NULL
    vegtrans_fig <- NULL
    vegtranssum_fig <- NULL


    ### Reactives to objects ----------------------------------------------------

    geom_data <- fetch_out()$geom_data
    ecosite_data <- fetch_out()$ecosite_data
    site_data <- fetch_out()$site_data
    ppi_data <- fetch_out()$vegplot_data$ppi_data
    vp_data <- fetch_out()$vegplot_data$vplot_data
    veg_trans_data <- fetch_out()$vegplot_data$vegtrans_data
    veg_trans_sum_data <- fetch_out()$vegplot_data$vegtranssum_data

    ## User filtering ----------------------------------------------------------

    ### Ecosite -----------------------------------------------------------------

    if (nzchar(input$usiteid)) {
      if (!is.null(site_data)          && NROW(site_data) > 0L)
        site_data <- dplyr::filter(site_data, usiteid == input$usiteid)

      if (!is.null(ppi_data)           && NROW(ppi_data) > 0L)
        ppi_data <- dplyr::filter(ppi_data, usiteid == input$usiteid)

      if (!is.null(vp_data)            && NROW(vp_data) > 0L)
        vp_data <- dplyr::filter(vp_data, usiteid == input$usiteid)

      if (!is.null(veg_trans_data)     && NROW(veg_trans_data) > 0L)
        veg_trans_data <- dplyr::filter(veg_trans_data, usiteid == input$usiteid)

      if (!is.null(veg_trans_sum_data) && NROW(veg_trans_sum_data) > 0L)
        veg_trans_sum_data <- dplyr::filter(veg_trans_sum_data, usiteid == input$usiteid)
    }


    ### State -----------------------------------------------------------

    if (nzchar(input$stateid)) {
      stateid_usiteid <-
        if (!is.null(site_data) && NROW(site_data) > 0L)
          dplyr::filter(site_data, ecostateid %in% input$stateid) |> dplyr::pull(usiteid)
      else character(0)

      if (!is.null(site_data)          && NROW(site_data) > 0L)
        site_data <- dplyr::filter(site_data, usiteid %in% stateid_usiteid)

      if (!is.null(ppi_data)           && NROW(ppi_data) > 0L)
        ppi_data <- dplyr::filter(ppi_data, usiteid %in% stateid_usiteid)

      if (!is.null(vp_data)            && NROW(vp_data) > 0L)
        vp_data <- dplyr::filter(vp_data, usiteid %in% stateid_usiteid)

      if (!is.null(veg_trans_data)     && NROW(veg_trans_data) > 0L)
        veg_trans_data <- dplyr::filter(veg_trans_data, usiteid %in% stateid_usiteid)

      if (!is.null(veg_trans_sum_data) && NROW(veg_trans_sum_data) > 0L)
        veg_trans_sum_data <- dplyr::filter(veg_trans_sum_data, usiteid %in% stateid_usiteid)
    }

    ### Phase -------------------------------------------------------------------

    if (nzchar(input$phaseid)) {
      phaseid_usiteid <-
        if (!is.null(site_data) && NROW(site_data) > 0L)
          dplyr::filter(site_data, commphaseid %in% input$phaseid) |> dplyr::pull(usiteid)
      else character(0)

      if (!is.null(site_data)          && NROW(site_data) > 0L)
        site_data <- dplyr::filter(site_data, usiteid %in% phaseid_usiteid)

      if (!is.null(ppi_data)           && NROW(ppi_data) > 0L)
        ppi_data <- dplyr::filter(ppi_data, usiteid %in% phaseid_usiteid)

      if (!is.null(vp_data)            && NROW(vp_data) > 0L)
        vp_data <- dplyr::filter(vp_data, usiteid %in% phaseid_usiteid)

      if (!is.null(veg_trans_data)     && NROW(veg_trans_data) > 0L)
        veg_trans_data <- dplyr::filter(veg_trans_data, usiteid %in% phaseid_usiteid)

      if (!is.null(veg_trans_sum_data) && NROW(veg_trans_sum_data) > 0L)
        veg_trans_sum_data <- dplyr::filter(veg_trans_sum_data, usiteid %in% phaseid_usiteid)
    }


    ### Date --------------------------------------------------------------------


    if(is.null(site_data) || NROW(site_data) == 0L){
      site_msg <- "No site data associated with the supplied criteria."} else {
      # assess data range
      ## Extract the most recent date per string
      site_data$most_recent_date <- lapply(site_data$obsdate, function(x) {

        ## Split on commas
        parts <- strsplit(x, ",\\s*")[[1]]

        ## Convert to Date (extract just the date part)
        dates <- as.Date(sub(" .*", "", parts), format = "%m/%d/%Y")

        ## Return the most recent
        max(dates, na.rm = TRUE)

      }) |> unlist()

      date_usiteid <- site_data |>
        dplyr::filter(most_recent_date >= input$daterange[1],
                      most_recent_date <= input$daterange[2]) |>
        dplyr::pull(usiteid)

      if (!is.null(site_data)          && NROW(site_data) > 0L)
        site_data <- dplyr::filter(site_data, usiteid %in% date_usiteid)

      if (!is.null(ppi_data)           && NROW(ppi_data) > 0L)
        ppi_data <- dplyr::filter(ppi_data, usiteid %in% date_usiteid)

      if (!is.null(vp_data)            && NROW(vp_data) > 0L)
        vp_data <- dplyr::filter(vp_data, usiteid %in% date_usiteid)

      if (!is.null(veg_trans_data)     && NROW(veg_trans_data) > 0L)
        veg_trans_data <- dplyr::filter(veg_trans_data, usiteid %in% date_usiteid)

      if (!is.null(veg_trans_sum_data) && NROW(veg_trans_sum_data) > 0L)
        veg_trans_sum_data <- dplyr::filter(veg_trans_sum_data, usiteid %in% date_usiteid)

    }

    ## Ecosite -------------------------------------------------------
    ecosite_msg <- NULL
    statephase_tbl <- NULL
    site_msg <- NULL

    # assign site_msg
    if(is.null(site_data) || NROW(site_data) == 0L){
      site_msg <- "No site data associated with the supplied criteria."
      ecosite_msg <- "No site data associated with the supplied criteria."
    }

    if(is.null(site_msg)){


      ### States/Phases Missing ---------------------------------------------------

      if(is.null(ecosite_data$statephase_data) || NROW(ecosite_data$statephase_data) == 0L){
        ecosite_msg <- "No ecosite data associated with the supplied criteria."
      } else {


        ### Numb. Sites/Comps -------------------------------------------------------

        # determine number of sites correlated to ecositeid
        n_sites <- length(unique(site_data$usiteid))
        # determine minimum number of sites based on number of comps
        comp_req <- ifelse(ecosite_data$comp < 4, ecosite_data$comp * 5, 20)

        # create message stating whether there are a sufficient number of sites based on comps
        if(n_sites >= comp_req){
          return_list[["ecosite_return"]][["comp_req"]] <- paste0("✅ " , n_sites, " sites with vegetation plots are correlated to ", input$esid, ".",
                                                                  " A minimum of ", comp_req, " are required.")
        } else {

          return_list[["ecosite_return"]][["comp_req"]] <- paste0("⚠️ ", comp_req, " sites with vegetation plots are required to be correlated to ", input$esid,
                                                                  " but only ", n_sites, " exist.")
        }


    ### States & Phases ---------------------------------------------------------

    # reduce site data down to just states/phases and change NAs to ""
    site_data_statephase <- site_data |> dplyr::select(usiteid, ecostateid, ecostatename, commphaseid, commphasename) |>
      mutate(across(everything(), as.character))
    site_data_statephase[is.na(site_data_statephase)] <- ""

    # change NAs to "" in EDIT data
    ecosite_data$statephase_data <- ecosite_data$statephase_data |> mutate(across(everything(), as.character))
    ecosite_data$statephase_data[is.na(ecosite_data$statephase_data)] <- ""

    ecosite_data$statephase_data <- ecosite_data$statephase_data %>%
      mutate(statephase = paste(state, community, sep = ".")) %>%
      filter(!grepl("\\.$", statephase))

    site_data_statephase <- site_data_statephase %>%
      mutate(statephase = paste(ecostateid, commphaseid, sep = ".")) %>%
      filter(!grepl("\\.$", statephase))

    # remove sites missing statephase
    site_data_statephase <- site_data_statephase |> dplyr::filter(statephase != "")

    # state/phase in site but not stm
    site_not_stm <- setdiff(site_data_statephase$statephase,
                            ecosite_data$statephase_data$statephase)
    site_not_stm_usiteid <- site_data_statephase |> dplyr::filter(statephase %in% site_not_stm) |>
      dplyr::pull(usiteid)

    if(length(site_not_stm) > 0){
      return_list[["ecosite_return"]][["site_not_stm"]] <-
        paste("⚠️ usiteids: ", paste(site_not_stm_usiteid, collapse = ", "), "have the following states/phases that do not occur in the EDIT STM:",
              paste(site_not_stm, collapse = ", "))
    }

    # state/phase in stm but not in site data
    stm_not_site <- setdiff(ecosite_data$statephase_data$statephase,
                            site_data_statephase$statephase)

    if(length(stm_not_site) > 0){
      return_list[["ecosite_return"]][["stm_not_site"]] <-
        paste("⚠️ The following states/phases from the EDIT STM are not represented in the site data:",
              paste(stm_not_site, collapse = ", "))
    }

    #### Plotting ----------------------------------------------------------------

    # EDIT documented states/phases
    edit_statephases <- ecosite_data$statephase_data %>%
      select(state, community)


    # All states/phases documented in Site data
    site_statephases <- site_data |> select(ecostateid, commphaseid)
    # Change no text to "None"
    site_statephases$ecostateid[is.na(site_statephases$ecostateid)] <- "None"
    site_statephases$commphaseid[is.na(site_statephases$commphaseid)] <- "None"

    # count number of observations of state/phase combinations
    counts <- site_statephases |> count(ecostateid, commphaseid, name = "n")


    # ---- Build full grid across all EDIT states & phases ----
    states <- sort(unique(c(edit_statephases$state, "None")))
    phases <- sort(unique(c(edit_statephases$community, "None")))

    grid <- tidyr::crossing(state = states, community = phases)

    # Mark allowed pairs from EDIT
    allowed_pairs <- edit_statephases %>%
      distinct(state, community) %>%
      mutate(allowed = TRUE)

    full <- grid %>%
      left_join(allowed_pairs, by = c("state","community")) %>%
      mutate(allowed = tidyr::replace_na(allowed, FALSE)) %>%
      left_join(counts, by = c("state" = "ecostateid", "community" = "commphaseid")) %>%
      mutate(
        n = tidyr::replace_na(n, 0L)    # NA for disallowed (to style grey)
      )

    # Pretty labels (keep "None" unprefixed)
    pretty_state  <- function(x) ifelse(x == "None", "None", paste("State", x))
    pretty_phase  <- function(x) ifelse(x == "None", "None", paste("Phase", x))

    # Wide counts table
    wide_counts <- full %>%
      transmute(State = pretty_state(state),
                Phase = pretty_phase(community),
                n) %>%
      tidyr::pivot_wider(names_from = Phase, values_from = n)

    # Parallel wide mask for styling (TRUE allowed / FALSE disallowed)
    wide_mask <- full %>%
      transmute(State = pretty_state(state),
                Phase = pretty_phase(community),
                allowed) %>%
      tidyr::pivot_wider(names_from = Phase, values_from = allowed, names_prefix = "mask__")

    # Merge counts + hidden mask columns for DT
    statephase_tbl <- dplyr::left_join(wide_counts, wide_mask, by = "State")

    phase_cols <- setdiff(names(statephase_tbl), c("State", grep("^mask__", names(statephase_tbl), value = TRUE)))
    mask_cols  <- paste0("mask__", phase_cols)

    # Hide mask columns (DataTables uses 0-based indices)
    mask_idx <- match(mask_cols, names(statephase_tbl)) - 1

    dt <- DT::datatable(
      statephase_tbl, rownames = FALSE,
      options = list(
        dom = "t", pageLength = 50, scrollX = TRUE, fixedHeader = TRUE,
        columnDefs = list(list(visible = FALSE, targets = mask_idx))
      )
    )

    # Grey out disallowed cells (mask == FALSE), leave allowed as default
    for (i in seq_along(phase_cols)) {
      dt <- DT::formatStyle(
        dt,
        columns      = phase_cols[i],
        valueColumns = mask_cols[i],
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(NA, "darkgrey")),
        color           = DT::styleEqual(c(TRUE, FALSE), c("black", "black"))
      )
    }

    # Optional: center numbers
    statephase_tbl <- DT::formatStyle(dt, columns = phase_cols)

    }
    }




    ## Site --------------------------------------------------------------------

    ### Missing fields ----------------------------------------------------------
    site_disp <- NULL

    if(is.null(site_msg)){

    # acquire required cols from user input
    req_site_cols <- input$site_choices

    required_columns <- field_mapping_flat[req_site_cols]

    # change all columns to character
    site_data_alpha <- site_data |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    # change NAs to ""
    site_data_alpha[is.na(site_data_alpha)] <- ""

    # write function to assess required cols
    make_bad_flags <- function(df, field_mapping, required_fields){
      # initiate list for output
      bad_flags <- list()

      for(col in names(df)) {

        # assign FALSE non required fields
        if(!col %in% field_mapping_flat[req_site_cols]){
          bad_flags[[paste0(col, "_bad")]] <- FALSE
        } else {

          matched_field <- parent_map[col]

          if(length(field_mapping[[matched_field]]) == 1){
            bad_flags[[paste0(col, "_bad")]] <- df[[col]] == ""
          } else {
            # pull the column names associated with user choice/iteration of loop
            df_fields <- field_mapping[[matched_field]][field_mapping[[matched_field]] %in% required_columns]
            # reduce df down to conditional columns
            df_sub <- df |> dplyr::select(all_of(df_fields))
            # determine output for all
            cond_output <- apply(df_sub, 1, function(x)all(x == ""))

            bad_flags[[paste0(col, "_bad")]] <- cond_output
          }

        }

      }
      cbind(df, bad_flags)
      }

    ls_df <- make_bad_flags(df = site_data_alpha, field_mapping = field_mapping,
                            required_fields = req_site_cols) |> as.data.frame()

    ls_df$geomcomp_bad <- rowSums(ls_df[, c("geomposhill_bad", "geomposmntn_bad",
                                             "geompostrce_bad", "geomposflats_bad")],
                                  na.rm = TRUE) > 0

    ls_df_nogeom <- ls_df |> dplyr::select(c(paste0(field_mapping_flat[req_site_cols], "_bad"), "geomcomp_bad"))

    if(length(field_mapping[["Geom Comp"]][req_site_cols[req_site_cols %in% names(field_mapping[["Geom Comp"]])]]) > 0){
      ls_df_nogeom <- ls_df_nogeom |>
        dplyr::select(-paste(field_mapping[["Geom Comp"]][req_site_cols[req_site_cols %in% names(field_mapping[["Geom Comp"]])]], "bad", sep = "_"))
    }



    ls_df$`% Complete` <- (ncol(ls_df_nogeom)-rowSums(ls_df_nogeom))/(ncol(ls_df_nogeom))*100

    ls_df$`# Cols. Req.` <- ncol(ls_df_nogeom)
    ls_df$`# Cols. Missing` <- rowSums(ls_df_nogeom)


    ls_df$Passing <- ifelse(ls_df$`% Complete` == 100, "\u2705", "\u274C")

    ls_df_site <- ls_df |> dplyr::select(Passing, `% Complete`, `# Cols. Req.`, `# Cols. Missing`, everything())


    ### Plotting ----------------------------------------------------------------

    # Create DT table
    site_disp <- DT::datatable(ls_df_site,
                             rownames = FALSE,
                             options = list(
                               pageLength = 10,
                               lengthMenu = list(c(1, 5, 10, 25, 50, 100),
                                                 c("1", "5", "10", "25", "50", "100")),
                               scrollX = TRUE,
                               fixedHeader = TRUE,
                               scrollY = "500px",
                               columnDefs = list(
                                 list(visible = FALSE,
                                      targets = 53:ncol(ls_df)-1
                                      )
                               )
                               )) |>
      DT::formatRound("% Complete", digits = 0) |>
      DT::formatStyle(
        columns = "usiteid",
        valueColumns = "usiteid_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "vegplotsize",
        valueColumns = "vegplotsize_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "elev",
        valueColumns = "elev_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "hillslopeprof",
        valueColumns = "hillslopeprof_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "geomslopeseg",
        valueColumns = "geomslopeseg_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "slope",
        valueColumns = "slope_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "aspect",
        valueColumns = "aspect_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "shapeacross",
        valueColumns = "shapeacross_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "shapedown",
        valueColumns = "shapedown_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "geomposhill",
        valueColumns = "geomposhill_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "geomposmntn",
        valueColumns = "geomposmntn_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "geompostrce",
        valueColumns = "geompostrce_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "geomposflats",
        valueColumns = "geomposflats_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "drainagecl",
        valueColumns = "drainagecl_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "flodfreqcl",
        valueColumns = "flodfreqcl_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "pondfreqcl",
        valueColumns = "pondfreqcl_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "geomfname",
        valueColumns = "geomfname_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "geomfnamep",
        valueColumns = "geomfnamep_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "ecositeid",
        valueColumns = "ecositeid_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "ecositenm",
        valueColumns = "ecositenm_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "obsdate",
        valueColumns = "obsdate_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "obsdatekind",
        valueColumns = "obsdatekind_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "earthcovkind1",
        valueColumns = "earthcovkind1_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "earthcovkind2",
        valueColumns = "earthcovkind2_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "p.uprojectid",
        valueColumns = "p.uprojectid_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "aspect",
        valueColumns = "aspect_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "p.projectname",
        valueColumns = "p.projectname_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "ecostateid",
        valueColumns = "ecostateid_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "ecostatename",
        valueColumns = "ecostatename_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "commphaseid",
        valueColumns = "commphaseid_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "aspect",
        valueColumns = "aspect_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "commphasename",
        valueColumns = "commphasename_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "pedodermclass",
        valueColumns = "pedodermclass_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "biolcrusttypedom",
        valueColumns = "biolcrusttypedom_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "biolcrusttypesecond",
        valueColumns = "biolcrusttypesecond_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "crustdevcl",
        valueColumns = "crustdevcl_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "localdisturbancedistance",
        valueColumns = "localdisturbancedistance_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "localdisturbancedescription",
        valueColumns = "localdisturbancedescription_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "soilmoistdept",
        valueColumns = "soilmoistdept_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "soilmoistdepb",
        valueColumns = "soilmoistdepb_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "obssoilmoiststat",
        valueColumns = "obssoilmoiststat_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "upedonid",
        valueColumns = "upedonid_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "ptaxhistclassdate",
        valueColumns = "ptaxhistclassdate_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "ptaxhisttaxonname",
        valueColumns = "ptaxhisttaxonname_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "pedontextrecdate",
        valueColumns = "pedontextrecdate_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "pedontextrecauthor",
        valueColumns = "pedontextrecauthor_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "pedontextentry",
        valueColumns = "pedontextentry_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      )

    site_fig <- ggplot(ls_df_site, aes(x = `% Complete`)) +
      geom_histogram(binwidth = 10, fill = "lavender", color = "black") +
      labs(
        title = "Histogram of Site Completeness",
        x = "% Complete",
        y = "Record Count"
      ) +
      theme_minimal() +
      coord_cartesian(xlim = c(0, 100)) +
      scale_x_continuous(breaks = seq(0, 100, 20))

    }



    # defaults
    missing_data <- NULL
    missing_message <- NULL

    # robust guard
    req_cols <- c("longstddecimaldegrees", "latstddecimaldegrees", "usiteid")
    if (!is.null(site_data) &&
        NROW(site_data) > 0L ) {

      missing_data <- site_data |>
        dplyr::filter(is.na(.data$longstddecimaldegrees) | is.na(.data$latstddecimaldegrees))

      if (NROW(missing_data) > 0L) {
        missing_message <- paste(
          "The following sites are missing WGS84 coordinates and were removed:",
          paste(missing_data$usiteid, collapse = ", ")
        )
      }
    }



    ### Map -----------------------------------------------------------------

    map_msg <- NULL

    if(is.null(geom_data) || NROW(geom_data) == 0L){
      my_map <- NULL
      map_msg <- "No mapunit geometry returned. Is the ecological site in SSURGO?"
    } else {


    pal <- colorFactor(
      palette = c("#d95f02", "#1b9e77"),
      domain = c(FALSE, TRUE)
    )

    my_map  <- leaflet::leaflet() |>
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap")  |>
      addProviderTiles("Esri.WorldImagery", group = "Esri Satellite")  |>
      addProviderTiles("Esri.WorldTopoMap", group = "Esri Topo") |>
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Esri Satellite", "Esri Topo"),
        overlayGroups = c("Polygons", "Points"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addPolygons(
      data = geom_data,
      fillColor = "#882E72",
      color = "#882E72",
      opacity = 0.4,
      group = "Polygons"
    ) |>
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(
          maxWidth = 100,
          metric = TRUE,
          imperial = TRUE,
          updateWhenIdle = FALSE
        )
      )

    if(is.null(site_msg)){

      pts <- site_data |>
        dplyr::filter(!is.na(longstddecimaldegrees), !is.na(latstddecimaldegrees)) |>
        sf::st_as_sf(
          coords = c("longstddecimaldegrees", "latstddecimaldegrees"),
          crs = 4326
        )

      my_map <- my_map |> addCircleMarkers(data = pts,
                                           radius = 5,
                                           color = "#E69F00",
                                           group = "Points",
                                           popup = ~sprintf("<b>Site:</b> %s<br><b>Ecosite:</b> %s
                                        <br><b>Ecostate:</b> %s
                                        <br><b>Commphase:</b> %s", usiteid, ecositeid, ecostateid, commphaseid),
                                           clusterOptions = markerClusterOptions(
                                             showCoverageOnHover = TRUE,
                                             zoomToBoundsOnClick = TRUE,
                                             spiderfyOnMaxZoom = TRUE,         # separate points at max zoom
                                             disableClusteringAtZoom = 15
                                           ))

    }
    }

    ### Veg plot ----------------------------------------------------------------

    #### Missing fields ----------------------------------------------------------
    vp_msg <- NULL

    if(is.null(vp_data) || NROW(vp_data) == 0L){
      vp_disp <- NULL
      vp_msg <- "No veg plot data associated with the supplied criteria."
    } else {

    # acquire required cols from user input
    req_site_cols <- input$vegplot_choices

    required_columns <- field_mapping_flat[req_site_cols]

    # change all columns to character
    site_data <- vp_data |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    # change NAs to ""
    site_data[is.na(site_data)] <- ""

    # write function to assess required cols
    make_bad_flags <- function(df, field_mapping, required_fields){
      # initiate list for output
      bad_flags <- list()

      for(col in names(df)) {

        # assign FALSE non required fields
        if(!col %in% field_mapping_flat[req_site_cols]){
          bad_flags[[paste0(col, "_bad")]] <- FALSE
        } else {

          matched_field <- parent_map[col]

          if(length(field_mapping[[matched_field]]) == 1){
            bad_flags[[paste0(col, "_bad")]] <- df[[col]] == ""
          } else {
            # pull the column names associated with user choice/iteration of loop
            df_fields <- field_mapping[[matched_field]][field_mapping[[matched_field]] %in% required_columns]
            # reduce df down to conditional columns
            df_sub <- df |> dplyr::select(all_of(df_fields))
            # determine output for all
            cond_output <- apply(df_sub, 1, function(x)all(x == ""))

            bad_flags[[paste0(col, "_bad")]] <- cond_output
          }

        }

      }
      cbind(df, bad_flags)
    }

    ls_df <- make_bad_flags(df = site_data, field_mapping = field_mapping,
                            required_fields = req_site_cols) |> as.data.frame()

    ls_df$totover_bad <- rowSums(ls_df[, c("overstorycancontotalpct_bad", "overstorycancovtotalclass_bad")],
                                  na.rm = TRUE) > 0

    ls_df$totcanopy_bad <- rowSums(ls_df[, c("cancovtotalpct_bad", "cancovtotalclass_bad")],
                                 na.rm = TRUE) > 0

    ls_df_nogeom <- ls_df |> dplyr::select(paste0(field_mapping_flat[req_site_cols], "_bad"), "totover_bad", "totcanopy_bad")

    if(length(field_mapping[["Total Overstory Canopy"]][req_site_cols[req_site_cols %in% names(field_mapping[["Total Overstory Canopy"]])]]) > 0){
      ls_df_nogeom <- ls_df_nogeom |>
        dplyr::select(-paste(field_mapping[["Total Overstory Canopy"]][req_site_cols[req_site_cols %in% names(field_mapping[["Total Overstory Canopy"]])]], "bad", sep = "_"))
    }

    if(length(field_mapping[["Total Canopy Cover"]][req_site_cols[req_site_cols %in% names(field_mapping[["Total Canopy Cover"]])]]) > 0){
      ls_df_nogeom <- ls_df_nogeom |>
        dplyr::select(-paste(field_mapping[["Total Canopy Cover"]][req_site_cols[req_site_cols %in% names(field_mapping[["Total Canopy Cover"]])]], "bad", sep = "_"))
    }

    ls_df$`% Complete` <- (ncol(ls_df_nogeom)-rowSums(ls_df_nogeom))/(ncol(ls_df_nogeom))*100

    ls_df$`# Cols. Req.` <- ncol(ls_df_nogeom)
    ls_df$`# Cols. Missing` <- rowSums(ls_df_nogeom)


    ls_df$Passing <- ifelse(ls_df$`% Complete` == 100, "\u2705", "\u274C")

    ls_df_vegplot <- ls_df |> dplyr::select(Passing, `% Complete`, `# Cols. Req.`, `# Cols. Missing`, everything())


    #### Plotting ----------------------------------------------------------------

    # Create DT table
    vp_disp <- DT::datatable(ls_df_vegplot,
                             rownames = FALSE,
                             options = list(
                               pageLength = 10,
                               lengthMenu = list(c(1, 5, 10, 25, 50, 100),
                                                 c("1", "5", "10", "25", "50", "100")),
                               scrollX = TRUE,
                               fixedHeader = TRUE,
                               scrollY = "500px",
                               columnDefs = list(
                                 list(visible = FALSE,
                                      targets = 26:ncol(ls_df)-1
                                 )
                               )
                             )) |>
      DT::formatRound("% Complete", digits = 0) |>
      DT::formatStyle(
        columns = "vegplotid",
        valueColumns = "vegplotid_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "obsintensity",
        valueColumns = "obsintensity_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "vegdataorigin",
        valueColumns = "vegdataorigin_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "silprofileindicator",
        valueColumns = "silprofileindicator_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "assocuserpedonid",
        valueColumns = "assocuserpedonid_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "qcreviewperson",
        valueColumns = "qcreviewperson_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "qcreviewdate",
        valueColumns = "qcreviewdate_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "qareviewperson",
        valueColumns = "qareviewperson_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "qareviewdate",
        valueColumns = "qareviewdate_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "overstorycancovtotalclass",
        valueColumns = "overstorycancovtotalclass_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "overstorycancontotalpct",
        valueColumns = "overstorycancontotalpct_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "cancovtotalclass",
        valueColumns = "cancovtotalclass_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "cancovtotalpct",
        valueColumns = "cancovtotalpct_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "basalareaplottotal",
        valueColumns = "basalareaplottotal_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "basalareaassessmethod",
        valueColumns = "basalareaassessmethod_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "vt.textentry",
        valueColumns = "vt.textentry_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "vt.recdate",
        valueColumns = "vt.recdate_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "psp.plotsampprotocolname",
        valueColumns = "psp.plotsampprotocolname_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "tsp.transsampprotocolname",
        valueColumns = "tsp.transsampprotocolname_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      )

    vegplot_fig <- ggplot(ls_df_vegplot, aes(x = `% Complete`)) +
      geom_histogram(binwidth = 10, fill = "lavender", color = "black") +
      labs(
        title = "Histogram of Veg Plot Completeness",
        x = "% Complete",
        y = "Record Count"
      ) +
      theme_minimal() +
      coord_cartesian(xlim = c(0, 100)) +
      scale_x_continuous(breaks = seq(0, 100, 20))

    }

    ### Plot Plant Inventory ----------------------------------------------------

    #### Missing fields ----------------------------------------------------------
    ppi_msg <- NULL

    if(is.null(ppi_data) || NROW(ppi_data) == 0L){
      ppi_disp <- NULL
      ppi_msg <- "No plot plant inventory data associated with the supplied criteria."
    } else {

    # acquire required cols from user input
    req_site_cols <- input$ppi_choices

    required_columns <- field_mapping_flat[req_site_cols]

    # change all columns to character
    site_data <- ppi_data |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    # change NAs to ""
    site_data[is.na(site_data)] <- ""

    # write function to assess required cols
    make_bad_flags <- function(df, field_mapping, required_fields){
      # initiate list for output
      bad_flags <- list()

      for(col in names(df)) {

        # assign FALSE non required fields
        if(!col %in% field_mapping_flat[req_site_cols]){
          bad_flags[[paste0(col, "_bad")]] <- FALSE
        } else {

          matched_field <- parent_map[col]

          if(length(field_mapping[[matched_field]]) == 1){
            bad_flags[[paste0(col, "_bad")]] <- df[[col]] == ""
          } else {
            # pull the column names associated with user choice/iteration of loop
            df_fields <- field_mapping[[matched_field]][field_mapping[[matched_field]] %in% required_columns]
            # reduce df down to conditional columns
            df_sub <- df |> dplyr::select(all_of(df_fields))
            # determine output for all
            cond_output <- apply(df_sub, 1, function(x)all(x == ""))

            bad_flags[[paste0(col, "_bad")]] <- cond_output
          }

        }

      }
      cbind(df, bad_flags)
    }

    ls_df <- make_bad_flags(df = site_data, field_mapping = field_mapping,
                            required_fields = req_site_cols) |> as.data.frame()

    ls_df$abund_bad <- rowSums(ls_df[, c("speciescancovclass_bad", "speciescancovpct_bad",
                                         "speciestraceamtflag_bad", "akstratumcoverclasspct_bad")],
                                  na.rm = TRUE) > 0

    ls_df$strata_bad <- rowSums(ls_df[, c("livecanopyhttop_bad", "livecanopyhtbottom_bad",
                                          "planttypegroup_bad", "vegetationstratalevel_bad",
                                          "akstratumcoverclass_bad")],
                               na.rm = TRUE) > 0

    ls_df_nogeom <- ls_df |> dplyr::select(c(paste0(field_mapping_flat[req_site_cols], "_bad"), "abund_bad", "strata_bad"))

    if(length(field_mapping[["Abundance"]][req_site_cols[req_site_cols %in% names(field_mapping[["Abundance"]])]]) > 0){
      ls_df_nogeom <- ls_df_nogeom |>
        dplyr::select(-paste(field_mapping[["Abundance"]][req_site_cols[req_site_cols %in% names(field_mapping[["Abundance"]])]], "bad", sep = "_"))
    }

    if(length(field_mapping[["Strata"]][req_site_cols[req_site_cols %in% names(field_mapping[["Strata"]])]]) > 0){
      ls_df_nogeom <- ls_df_nogeom |>
        dplyr::select(-paste(field_mapping[["Strata"]][req_site_cols[req_site_cols %in% names(field_mapping[["Strata"]])]], "bad", sep = "_"))
    }

    ls_df$`% Complete` <- (ncol(ls_df_nogeom)-rowSums(ls_df_nogeom))/(ncol(ls_df_nogeom))*100

    ls_df$`# Cols. Req.` <- ncol(ls_df_nogeom)
    ls_df$`# Cols. Missing` <- rowSums(ls_df_nogeom)


    ls_df$Passing <- ifelse(ls_df$`% Complete` == 100, "\u2705", "\u274C")

    ls_df_ppi <- ls_df |> dplyr::select(Passing, `% Complete`, `# Cols. Req.`, `# Cols. Missing`, everything())


    ### Plotting ----------------------------------------------------------------

    # Create DT table
    ppi_disp <- DT::datatable(ls_df_ppi,
                             rownames = FALSE,
                             options = list(
                               pageLength = 10,
                               lengthMenu = list(c(1, 5, 10, 25, 50, 100),
                                                 c("1", "5", "10", "25", "50", "100")),
                               scrollX = TRUE,
                               fixedHeader = TRUE,
                               scrollY = "500px",
                               columnDefs = list(
                                 list(visible = FALSE,
                                      targets = 19:ncol(ls_df)-1
                                 )
                               )
                             )) |>
      DT::formatRound("% Complete", digits = 0) |>
      DT::formatStyle(
        columns = "plantsym",
        valueColumns = "plantsym_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "speciescancovclass",
        valueColumns = "speciescancovclass_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "speciescancovpct",
        valueColumns = "speciescancovpct_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "speciestraceamtflag",
        valueColumns = "speciestraceamtflag_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "akstratumcoverclasspct",
        valueColumns = "akstratumcoverclasspct_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "livecanopyhttop",
        valueColumns = "livecanopyhttop_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "livecanopyhtbottom",
        valueColumns = "livecanopyhtbottom_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "planttypegroup",
        valueColumns = "planttypegroup_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "vegetationstratalevel",
        valueColumns = "vegetationstratalevel_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "akstratumcoverclass",
        valueColumns = "akstratumcoverclass_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      ) |>
      DT::formatStyle(
        columns = "estannualprod",
        valueColumns = "estannualprod_bad",
        backgroundColor = DT::styleEqual(TRUE, "blueviolet")
      )

    ppi_fig <- ggplot(ls_df_ppi, aes(x = `% Complete`)) +
      geom_histogram(binwidth = 10, fill = "lavender", color = "black") +
      labs(
        title = "Histogram of PPI Completeness",
        x = "% Complete",
        y = "Record Count"
      ) +
      theme_minimal() +
      coord_cartesian(xlim = c(0, 100)) +
      scale_x_continuous(breaks = seq(0, 100, 20))

    }


    ### Veg Trans ---------------------------------------------------------------

    #### Missing fields ----------------------------------------------------------
    vegtrans_msg <- NULL

     if(is.null(veg_trans_data) || NROW(veg_trans_data) == 0L){
      vegtrans_disp <- NULL
      vegtrans_msg <- "No veg transect data associated with the supplied criteria."
    } else {
      # acquire required cols from user input
      req_site_cols <- input$vegtrans_choices

      required_columns <- field_mapping_flat[req_site_cols]

      # change all columns to character
      site_data <- veg_trans_data |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

      # change NAs to ""
      site_data[is.na(site_data)] <- ""

      # write function to assess required cols
      make_bad_flags <- function(df, field_mapping, required_fields){
        # initiate list for output
        bad_flags <- list()

        for(col in names(df)) {

          # assign FALSE non required fields
          if(!col %in% field_mapping_flat[req_site_cols]){
            bad_flags[[paste0(col, "_bad")]] <- FALSE
          } else {

            matched_field <- parent_map[col]

            if(length(field_mapping[[matched_field]]) == 1){
              bad_flags[[paste0(col, "_bad")]] <- df[[col]] == ""
            } else {
              # pull the column names associated with user choice/iteration of loop
              df_fields <- field_mapping[[matched_field]][field_mapping[[matched_field]] %in% required_columns]
              # reduce df down to conditional columns
              df_sub <- df |> dplyr::select(all_of(df_fields))
              # determine output for all
              cond_output <- apply(df_sub, 1, function(x)all(x == ""))

              bad_flags[[paste0(col, "_bad")]] <- cond_output
            }

          }

        }
        cbind(df, bad_flags)
      }

      ls_df <- make_bad_flags(df = site_data, field_mapping = field_mapping,
                              required_fields = req_site_cols) |> as.data.frame()

      ls_df_nogeom <- ls_df |> dplyr::select(paste0(field_mapping_flat[req_site_cols], "_bad"))

      ls_df$`% Complete` <- (ncol(ls_df_nogeom)-rowSums(ls_df_nogeom))/(ncol(ls_df_nogeom))*100

      ls_df$`# Cols. Req.` <- ncol(ls_df_nogeom)
      ls_df$`# Cols. Missing` <- rowSums(ls_df_nogeom)


      ls_df$Passing <- ifelse(ls_df$`% Complete` == 100, "\u2705", "\u274C")

      ls_df_vegtrans <- ls_df |> dplyr::select(Passing, `% Complete`, `# Cols. Req.`, `# Cols. Missing`, dplyr::everything())


      ### Plotting ----------------------------------------------------------------

      # Create DT table
      vegtrans_disp <- DT::datatable(ls_df_vegtrans,
                                     rownames = FALSE,
                                     options = list(
                                       pageLength = 10,
                                       lengthMenu = list(c(1, 5, 10, 25, 50, 100),
                                                         c("1", "5", "10", "25", "50", "100")),
                                       scrollX = TRUE,
                                       fixedHeader = TRUE,
                                       scrollY = "500px",
                                       columnDefs = list(
                                         list(visible = FALSE,
                                              targets = 11:ncol(ls_df)-1
                                         )
                                       )
                                     )) |>

        DT::formatRound("% Complete", digits = 0) |>
        DT::formatStyle(
          columns = "vegtransectid",
          valueColumns = "vegtransectid_bad",
          backgroundColor = DT::styleEqual(TRUE, "blueviolet")
        ) |>
        DT::formatStyle(
          columns = "transectazimuth",
          valueColumns = "transectazimuth_bad",
          backgroundColor = DT::styleEqual(TRUE, "blueviolet")
        ) |>
        DT::formatStyle(
          columns = "transectlength",
          valueColumns = "transectlength_bad",
          backgroundColor = DT::styleEqual(TRUE, "blueviolet")
        ) |>
        DT::formatStyle(
          columns = "lpiobsinterval",
          valueColumns = "lpiobsinterval_bad",
          backgroundColor = DT::styleEqual(TRUE, "blueviolet")
        ) |>
        DT::formatStyle(
          columns = "totalpointssampledcount",
          valueColumns = "totalpointssampledcount_bad",
          backgroundColor = DT::styleEqual(TRUE, "blueviolet")
        )

      vegtrans_fig <- ggplot(ls_df_vegtrans, aes(x = `% Complete`)) +
        geom_histogram(binwidth = 10, fill = "lavender", color = "black") +
        labs(
          title = "Histogram of Veg Trans. Completeness",
          x = "% Complete",
          y = "Record Count"
        ) +
        theme_minimal() +
        coord_cartesian(xlim = c(0, 100)) +
        scale_x_continuous(breaks = seq(0, 100, 20))

    }



    ### Veg Trans Sum -----------------------------------------------------------

    #### Missing fields ----------------------------------------------------------
    vegtranssum_msg <- NULL

    if(is.null(veg_trans_sum_data) || NROW(veg_trans_sum_data) == 0L){
      vegtranssum_disp <- NULL
      vegtranssum_msg <- "No veg transect summary data associated with the supplied criteria."
    } else {
        # acquire required cols from user input
        req_site_cols <- input$vegtranssum_choices

        required_columns <- field_mapping_flat[req_site_cols]

        # change all columns to character
        site_data <- veg_trans_sum_data |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

        # change NAs to ""
        site_data[is.na(site_data)] <- ""

        # write function to assess required cols
        make_bad_flags <- function(df, field_mapping, required_fields){

          # initiate list for output
          bad_flags <- list()

          for(col in names(df)) {

            # assign FALSE non required fields
            if(!col %in% field_mapping_flat[req_site_cols]){
              bad_flags[[paste0(col, "_bad")]] <- FALSE
            } else {

              matched_field <- parent_map[col]

              if(length(field_mapping[[matched_field]]) == 1){
                bad_flags[[paste0(col, "_bad")]] <- df[[col]] == ""
              } else {
                # pull the column names associated with user choice/iteration of loop
                df_fields <- field_mapping[[matched_field]][field_mapping[[matched_field]] %in% required_columns]
                # reduce df down to conditional columns
                df_sub <- df |> dplyr::select(all_of(df_fields))
                # determine output for all
                cond_output <- apply(df_sub, 1, function(x)all(x == ""))

                bad_flags[[paste0(col, "_bad")]] <- cond_output
              }

            }

          }
          cbind(df, bad_flags)
        }

        ls_df <- make_bad_flags(df = site_data, field_mapping = field_mapping,
                                required_fields = req_site_cols) |> as.data.frame()


        ls_df_nogeom <- ls_df |> dplyr::select(paste0(field_mapping_flat[req_site_cols], "_bad"))

        ls_df$`% Complete` <- (ncol(ls_df_nogeom)-rowSums(ls_df_nogeom))/(ncol(ls_df_nogeom))*100

        ls_df$`# Cols. Req.` <- ncol(ls_df_nogeom)
        ls_df$`# Cols. Missing` <- rowSums(ls_df_nogeom)


        ls_df$Passing <- ifelse(ls_df$`% Complete` == 100, "\u2705", "\u274C")

        ls_df_vegtranssum <- ls_df |> dplyr::select(Passing, `% Complete`, `# Cols. Req.`, `# Cols. Missing`, everything())


        ### Plotting ----------------------------------------------------------------

        # Create DT table
        vegtranssum_disp <- DT::datatable(ls_df_vegtranssum,
                                          rownames = FALSE,
                                          options = list(
                                            pageLength = 10,
                                            lengthMenu = list(c(1, 5, 10, 25, 50, 100),
                                                              c("1", "5", "10", "25", "50", "100")),
                                            scrollX = TRUE,
                                            fixedHeader = TRUE,
                                            scrollY = "500px",
                                            columnDefs = list(
                                              list(visible = FALSE,
                                                   targets = 11:ncol(ls_df)-1
                                              )
                                            )
                                          )) |>
          DT::formatRound("% Complete", digits = 0) |>
          DT::formatStyle(
            columns = "speciesfoliarcovhitcount",
            valueColumns = "speciesfoliarcovhitcount_bad",
            backgroundColor = DT::styleEqual(TRUE, "blueviolet")
          ) |>
          DT::formatStyle(
            columns = "speciesfoliarcovpctlineint",
            valueColumns = "speciesfoliarcovpctlineint_bad",
            backgroundColor = DT::styleEqual(TRUE, "blueviolet")
          ) |>
          DT::formatStyle(
            columns = "speciesbasalcovhitcount",
            valueColumns = "speciesbasalcovhitcount_bad",
            backgroundColor = DT::styleEqual(TRUE, "blueviolet")
          ) |>
          DT::formatStyle(
            columns = "speciesbasalcovpctlineint",
            valueColumns = "speciesbasalcovpctlineint_bad",
            backgroundColor = DT::styleEqual(TRUE, "blueviolet")
          ) |>
          DT::formatStyle(
            columns = "plantsym",
            valueColumns = "plantsym_bad",
            backgroundColor = DT::styleEqual(TRUE, "blueviolet")
          )

        vegtranssum_fig <- ggplot(ls_df_vegtranssum, aes(x = `% Complete`)) +
          geom_histogram(binwidth = 10, fill = "lavender", color = "black") +
          labs(
            title = "Histogram of Veg Trans. Sum. Completeness",
            x = "% Complete",
            y = "Record Count"
          ) +
          theme_minimal() +
          theme(
            axis.line.x = element_line(color = "black"),  # full x-axis line
            axis.line.y = element_line(color = "black")   # add y-axis line too if you want
          ) +
          coord_cartesian(xlim = c(0, 100)) +
          scale_x_continuous(breaks = seq(0, 100, 20))


      }


    ### Pass/Not Passing Table ------------------------------------------------------------

    sum_table <- NULL

    if(is.null(site_data) || NROW(site_data) == 0L){
      site_disp <- NULL
      site_msg <- "No site data associated with the supplied criteria."
    } else {



    # helper: returns c(total_unique_usiteid, passing_usiteid_where_all_rows_pass)
    count_tot_and_allpass <- function(df) {
      total <- n_distinct(df$usiteid)

      passing <- df %>%
        group_by(usiteid) %>%
        summarise(
          all_pass = all(Passing == "\u2705") & !any(is.na(Passing)),
          .groups = "drop"
        ) %>%
        summarise(n = sum(all_pass)) %>%
        pull(n)

      c(total, passing)
    }

    # wrappers that return zeros when the section is "off"
    z_or_counts <- function(df, enabled) {
      if (is.null(enabled)) c(0L, 0L) else count_tot_and_allpass(df)
    }

    site_vals  <- z_or_counts(ls_df_site,        site_disp)
    vp_vals    <- z_or_counts(ls_df_vegplot,     vp_disp)
    ppi_vals   <- z_or_counts(ls_df_ppi,         ppi_disp)
    vt_vals    <- z_or_counts(ls_df_vegtrans,    vegtrans_disp)
    vts_vals   <- z_or_counts(ls_df_vegtranssum, vegtranssum_disp)




    sum_table <- data.frame(
      Metric = c("# of Sites", "# of Sites Passing"),
      Site = site_vals,
      `Veg Plot` = vp_vals,
      `Plot Plant Inv.` = ppi_vals,
      `Veg Trans` = vt_vals,
      `Veg Trans Sum.` = vts_vals,
      check.names = FALSE
    )

    }




    ### Return Output -----------------------------------------------------------

    return_list[["map_data"]] <- my_map

    return_list[["map_msg"]] <- map_msg

    return_list[["missing_coords_msg"]] <- missing_message

    return_list[["ppi_data"]] <- ppi_disp

    return_list[["ppi_msg"]] <- fetch_out()$fetch_msg$vegplot$ppi

    return_list[["vp_data"]] <- vp_disp

    return_list[["vp_msg"]] <- fetch_out()$fetch_msg$vegplot$vplot

    return_list[["veg_trans_data"]] <- vegtrans_disp

    return_list[["veg_trans_msg"]] <- fetch_out()$fetch_msg$vegplot$vegtrans

    return_list[["veg_trans_sum_data"]] <- vegtranssum_disp

    return_list[["veg_trans_sum_msg"]] <- fetch_out()$fetch_msg$vegplot$vegtranssum

    return_list[["site_data"]] <- site_disp

    return_list[["site_fig"]] <- site_fig

    return_list[["vegplot_fig"]] <- vegplot_fig

    return_list[["ppi_fig"]] <- ppi_fig

    return_list[["vegtrans_fig"]] <- vegtrans_fig

    return_list[["vegtranssum_fig"]] <- vegtranssum_fig

    return_list[["summary_table"]] <- sum_table

    return_list[["statephase_table"]] <- statephase_tbl

    return_list[["ecosite_msg"]] <- ecosite_msg

    return_list[["site_msg"]] <- site_msg

    return_list[["vp_msg"]] <- vp_msg

    return_list[["ppi_msg"]] <- ppi_msg

    return_list[["vegtrans_msg"]] <- vegtrans_msg

    return_list[["vegtranssum_msg"]] <- vegtranssum_msg

    return(return_list)

  })

  # output ------------------------------------------------------------------


  ## Data retrieval message --------------------------------------------------

  output$fetch_msg <- renderText({
    req(fetch_out())
    fetch_out()$fetch_msg_sum
    # fetch_out()$fetch_msg$ecosite$statephase
    # fetch_out()$fetch_msg$site
    # fetch_out()$fetch_msg$vegplot$vplot
    # fetch_out()$fetch_msg$vegplot$ppi
    # fetch_out()$fetch_msg$vegplot$vegtrans
    # fetch_out()$fetch_msg$vegplot$vegtranssum
  })

  ## Map ---------------------------------------------------------------------

  output$map_msg <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$map_msg
  })

  output$missing_coords_msg <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$missing_coords_msg
  })

  output$map_data <- renderLeaflet({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$map_data
  })

    output$download_map <- downloadHandler(
      filename = function() { paste0(input$esid, "_map.html") },
      content = function(file) {
        htmlwidgets::saveWidget(filter_out()$map_data, file, selfcontained = TRUE)
      }
    )




  ## Ecosite -----------------------------------------------------------------

  output$ecosite_comp_msg <- renderText({
    req(!clear_after_fetch())
    req(fetch_out())
    fetch_out()$fetch_msg$ecosite$comp
  })

  output$ecosite_statephase_msg <- renderText({
    req(!clear_after_fetch())
    req(fetch_out())
    fetch_out()$fetch_msg$ecosite$statephase
  })

  output$stm_not_site <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$ecosite_return$stm_not_site
  })

  output$site_not_stm <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$ecosite_return$site_not_stm
  })

  output$ecosite_data <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$ecosite_return$comp_req
  })

  output$ecosite_msg <- renderText({
    req(!clear_after_fetch())
    req(fetch_out())
    fetch_out()$ecosite_msg
  })

  output$summary_table <- renderTable({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$summary_table
  })

  output$statephase_table <- DT::renderDataTable({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$statephase_table
  })

  output$site_fig <- renderPlot({
    req(!clear_after_fetch())
    filter_out()$site_fig
  })

  output$vegplot_fig <- renderPlot({
    req(!clear_after_fetch())
    filter_out()$vegplot_fig
  })

  output$ppi_fig <- renderPlot({
    req(!clear_after_fetch())
    filter_out()$ppi_fig
  })

  output$vegtrans_fig <- renderPlot({
    req(!clear_after_fetch())
    filter_out()$vegtrans_fig
  })

  output$vegtranssum_fig <- renderPlot({
    req(!clear_after_fetch())
    filter_out()$vegtranssum_fig
  })

  output$summary_title <- renderText({
    req(!clear_after_fetch())
    "Summary of Sites Passing Criteria:"
  })

  output$statephase_title <- renderText({
    req(!clear_after_fetch())
    "State & Phase Table (grey cells do not exist in EDIT STM):"
  })



  ## Site --------------------------------------------------------------------

  output$site_msg <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$site_msg
  })

  output$site_data <- DT::renderDataTable({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$site_data
  })

  output$download_site_data <- downloadHandler(
    filename = function() {
      paste0(input$esid, "_", "SITE_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(fetch_out()$site_data, file, row.names = FALSE)
    }
  )


  ## Veg Plot ----------------------------------------------------------------

  output$vp_msg <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$vp_msg
  })

  output$vp_data <- DT::renderDataTable({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$vp_data
  })

  output$download_vegplot_data <- downloadHandler(
    filename = function() {
      paste0(input$esid, "_", "VEGPLOT_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(fetch_out()$vegplot_data$vplot_data, file, row.names = FALSE)
    }
  )

  ## PPI ---------------------------------------------------------------------

  output$ppi_msg <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$ppi_msg
  })

  output$ppi_data <- DT::renderDataTable({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$ppi_data
  })

  output$ppi_msg <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$ppi_msg
  })

  output$download_ppi_data <- downloadHandler(
    filename = function() {
      paste0(input$esid, "_", "PPI_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(fetch_out()$vegplot_data$ppi_data, file, row.names = FALSE)
    }
  )

  # Veg Trans ---------------------------------------------------------------
  output$vegtrans_msg <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$vegtrans_msg
  })

  output$veg_trans_data <- DT::renderDataTable({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$veg_trans_data
  })

  output$download_vegtrans_data <- downloadHandler(
    filename = function() {
      paste0(input$esid, "_", "VEGTRANS_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(fetch_out()$vegplot_data$vegtrans_data, file, row.names = FALSE)
    }
  )

# Veg Trans Sum -----------------------------------------------------------

  output$vegtranssum_msg <- renderText({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$vegtranssum_msg
  })

  # output$veg_trans_sum_msg <- renderText({
  #   req(filter_out())
  #   filter_out()$veg_trans_sum_msg
  # })

  output$veg_trans_sum_data <- DT::renderDataTable({
    req(!clear_after_fetch())
    req(filter_out())
    filter_out()$veg_trans_sum_data
  })

  output$download_vegtranssum_data <- downloadHandler(
    filename = function() {
      paste0(input$esid, "_", "VEGTRANSSUM_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(fetch_out()$vegplot_data$vegtranssum_data, file, row.names = FALSE)
    }
  )



}

shinyApp(ui, server)
