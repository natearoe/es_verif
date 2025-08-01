
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
library(httr)
library(jsonlite)
library(stringr)


# ui ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel("ES Verification QC"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "esid", label = "Ecological Site ID:"),
      # tags$div(style = "margin-top: 12px;"),
      actionButton("fetch_button", "Fetch Data"),
      # tags$div(style = "margin-top: 12px;"),

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
                                             "Aspect", "Slope Shape (Across/Up-Down)",
                                             "Geomorph Component", "Site Geomorphic Desc. Feature Type",
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
                                             "Pedon Tax. Hist. Text Entry")),
      shinyWidgets::pickerInput(inputId = "vegplot_choices",
                                label = "Veg Plot",
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
      shinyWidgets::pickerInput(inputId = "ppi_choices",
                                label = "Plot Plant Inv.",
                                choices = c("Plant Symbol",
                                            "Canopy Cover Pct",
                                            "Species Trace Amount Flag",
                                            "Species Canopy Cover Percent",
                                            "Height Class Upper & Lower",
                                            "Plant Type Group",
                                            "AK Stratum Cover Class",
                                            "Vegetation Strata Level",
                                            "Estimated Annual Production"),
                                multiple = TRUE,
                                selected = c("Plant Symbol",
                                             "Canopy Cover Pct",
                                             "Species Trace Amount Flag",
                                             "Height Class Upper & Lower",
                                             "Plant Type Group",
                                             "AK Stratum Cover Class",
                                             "Estimated Annual Production")),
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
      dateRangeInput(inputId = "daterange", label = "Site Observation from:"),
      textInput(inputId = "usiteid", label = "User Site ID:"),
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
        tabPanel("Ecosite",
                 verbatimTextOutput("ecosite_comp_msg"),
                 verbatimTextOutput("ecosite_statephase_msg"),
                 verbatimTextOutput("ecosite_data"),
                 verbatimTextOutput("stm_not_site"),
                 verbatimTextOutput("site_not_stm")
                 # # tags$div(style = "margin-top: 12px;"),
                 # DT::dataTableOutput("tabular_disp")
        ),
        tabPanel("Site",
                 # verbatimTextOutput("map_msg"),
                 # tags$div(style = "margin-top: 12px;"),
                 DT::dataTableOutput("site_data")
        ),

        tabPanel("Veg Plot",
                 DT::dataTableOutput("vp_data")
                 # verbatimTextOutput("map_msg"),
                 # # tags$div(style = "margin-top: 12px;"),
                 # DT::dataTableOutput("tabular_disp")
        ),
        tabPanel("Plot Plant Inv.",
                 verbatimTextOutput("ppi_msg"),
                 DT::dataTableOutput("ppi_data")
                 # verbatimTextOutput("map_msg"),
                 # # tags$div(style = "margin-top: 12px;"),
                 # DT::dataTableOutput("tabular_disp")
        ),
        tabPanel("Veg Transect",
                 DT::dataTableOutput("veg_trans_data")
                 # verbatimTextOutput("map_msg"),
                 # # tags$div(style = "margin-top: 12px;"),
                 # DT::dataTableOutput("tabular_disp")
        ),
        tabPanel("Veg Transect Summary",
                 DT::dataTableOutput("veg_trans_sum_data")
                 # verbatimTextOutput("map_msg"),
                 # # tags$div(style = "margin-top: 12px;"),
                 # DT::dataTableOutput("tabular_disp")
        ),
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

    # site data all comes from one web report

    ## Web report: site data ---------------------------------------------------

    url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifSiteDataByEcosite&es1=", input$esid)
    # site_data <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifSiteDataByEcosite&es1=", "R018XI101CA")

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

    # Web report: veg trans sum -----------------------------------------------

    url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifVegTransSummary&es1=", input$esid)
    # url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-ESverifegTransSummary&es1=", "F018XI201CA")

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

  # filterReact -------------------------------------------------------------

  filter_out <- eventReactive(input$filter_button, {



    ## Field mapping -----------------------------------------------------------

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

    ## Return list build -------------------------------------------------------

    return_list <- list(ecosite_return = list(comp_req = NULL,
                                              stm = list(sites_not_stm = NULL,
                                                         stm_not_sites = NULL)),
                        ppi_data = NULL)

    # tabular_data <- fetch_out()$tabular_data
    # mu_data <- fetch_out()$geom_data

    # es_comp_data <- fetch_out$ecosite_data$comp_data
    #
    # if (inherits(es_comp_data, "data.frame")){
    #   es_comp_data
    # }

    # es_statephase_data <- fetch_out$ecosite_data$statephase_data
    #
    # site_data <- fetch_out$site_data


    ## Reactives to objects ----------------------------------------------------

    geom_data <- fetch_out()$geom_data
    ecosite_data <- fetch_out()$ecosite_data
    site_data <- fetch_out()$site_data
    ppi_data <- fetch_out()$vegplot_data$ppi_data
    vp_data <- fetch_out()$vegplot_data$vplot_data
    veg_trans_data <- fetch_out()$vegplot_data$vegtrans_data
    veg_trans_sum_data <- fetch_out()$vegplot_data$vegtranssum_data


    ## User filtering ----------------------------------------------------------


    ### Ecosite -----------------------------------------------------------------


    if(nzchar(input$usiteid)){
      site_data <- site_data |> dplyr::filter(usiteid == input$usiteid)
      ppi_data <- ppi_data |> dplyr::filter(usiteid == input$usiteid)
      vp_data <- vp_data |> dplyr::filter(usiteid == input$usiteid)
      veg_trans_data <- veg_trans_data |> dplyr::filter(usiteid == input$usiteid)
      veg_trans_sum_data <- veg_trans_sum_data |> dplyr::filter(usiteid == input$usiteid)
    }

    ### Date

    ## Ecosite filtering -------------------------------------------------------

    ### Components --------------------------------------------------------------

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

    # create new state.phase columns in both dfs
    ecosite_data$statephase_data$statephase <- paste(ecosite_data$statephase_data$state, ecosite_data$statephase_data$community, sep = ".")
    ecosite_data$statephase_data$statephase <- sub("\\.$", "", ecosite_data$statephase_data$statephase)

    site_data_statephase$statephase <- paste(site_data_statephase$ecostateid, site_data_statephase$commphaseid, sep = ".")
    site_data_statephase$statephase <- sub("\\.$", "", site_data_statephase$statephase)

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


    ## Site --------------------------------------------------------------------


    ### Missing fields ----------------------------------------------------------


    # user inputs
    site_fields <- input$site_choices

    # determine required columns
    required_columns <- unlist(field_mapping[site_fields])

    # assess completeness
    completeness_matrix <- sapply(site_fields, FUN = function(field){
      cols <- field_mapping[[field]]

      if(length(cols) == 1){
        # 1:1 mapping - single required column must have some data
        !is.na(site_data[[cols]])
      } else {
        # multi-column mapping - only one column needs to be populated
        rowSums(!is.na(site_data[, cols, drop = FALSE])) > 0
      }

    }) |> as.matrix()

    if(nchar(input$usiteid) >= 1){
      completeness_matrix <- completeness_matrix |> t()
    }

    # # store completeness in column
    site_data$is_complete <- apply(completeness_matrix, 1, all)

    # ## assess data range
    #
    # # Extract the most recent date per string
    # site_data$most_recent_date <- lapply(site_data$obsdate, function(x) {
    #   # Split on commas
    #   parts <- strsplit(x, ",\\s*")[[1]]
    #
    #   # Convert to Date (extract just the date part)
    #   dates <- as.Date(sub(" .*", "", parts), format = "%m/%d/%Y")
    #
    #   # Return the most recent
    #   max(dates, na.rm = TRUE)
    # }) |> unlist()
    #
    # site_data <- site_data  |>
    #   dplyr::filter(most_recent_date >= input$daterange[1],
    #                 most_recent_date <= input$daterange[2])

    # Replace NA with empty strings just for visual
    tab_display <- site_data |>
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


    # vegplot_data <- fetch_out$vegplot_data$vplot_data
    # vegplot_ppi_data <- fetch_out$vegplot_data$ppi_data
    # vegplot_vt_data <- fetch_out$vegplot_data$vegtrans_data
    # vegplot_vts_data <- fetch_out$vegplot_data$vegtranssum_data



    # if(inherits(tabular_data, "NULL")) {
    #   return(
    #     list(
    #       map_data = NULL,
    #       map_msg = "Map not generated when site data is missing.",
    #       missing_coords_msg = NULL
    #     )
    #   )
    #
    # }
    #
    # if(inherits(fetch_out()$geom_data, "NULL")) {
    #   return(
    #     list(
    #       map_data = NULL,
    #       map_msg = "Map not generated when mapunit data is missing.",
    #       missing_coords_msg = NULL
    #     )
    #   )
    # }

    # check for missing coordinates
    missing_data <- site_data |> dplyr::filter(is.na(longstddecimaldegrees) | is.na(latstddecimaldegrees))

    missing_message <- if (nrow(missing_data) > 0) {
      paste("The following sites are missing WGS84 coordinates and were removed:",
            paste(missing_data$usiteid, collapse = ", "))
    } else {
      NULL
    }


    ## Veg plot ----------------------------------------------------------------



    # user inputs
    selected_fields <- c(input$site_choices,
                         input$siteobs_choices,
                         input$vegplot_choices,
                         input$pedon_choices)



    # # determine required columns
    # required_columns <- unlist(field_mapping[selected_fields])
    #
    # # assess completeness
    # completeness_matrix <- sapply(selected_fields, FUN = function(field){
    #   cols <- field_mapping[[field]]
    #
    #   if(length(cols) == 1){
    #     # 1:1 mapping - single required column must have some data
    #     !is.na(tabular_data[[cols]])
    #   } else {
    #     # multi-column mapping - only one column needs to be populated
    #     rowSums(!is.na(tabular_data[, cols, drop = FALSE])) > 0
    #   }
    #
    # }) |> as.matrix()
    #
    # # # store completeness in column
    # tabular_data$is_complete <- apply(completeness_matrix, 1, all)
    #
    # ## assess data range
    #
    # # Extract the most recent date per string
    # site_data$most_recent_date <- lapply(site_data$obsdate, function(x) {
    #   # Split on commas
    #   parts <- strsplit(x, ",\\s*")[[1]]
    #
    #   # Convert to Date (extract just the date part)
    #   dates <- as.Date(sub(" .*", "", parts), format = "%m/%d/%Y")
    #
    #   # Return the most recent
    #   max(dates, na.rm = TRUE)
    # }) |> unlist()
    #
    # site_data <- site_data  |>
    #   dplyr::filter(most_recent_date >= input$daterange[1],
    #                 most_recent_date <= input$daterange[2])
    #
    # # Replace NA with empty strings just for visual
    # tab_display <- tabular_data |>
    #   dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    #
    # tab_display[is.na(tab_display)] <- ""
    #
    # # Create DT table
    # df_disp <- DT::datatable(tab_display, options = list(scrollX = TRUE)) |>
    #   # Highlight required fields if they were NA (now empty string)
    #   DT::formatStyle(
    #     columns = required_columns,
    #     valueColumns = required_columns,
    #     backgroundColor = styleEqual("", 'mistyrose')
    #   )


    ## Create map --------------------------------------------------------------

    # set color palette
    pal <- colorFactor(
      palette = c("#d95f02", "#1b9e77"),
      domain = c(FALSE, TRUE)
    )

    ## createMap ---------------------------------------------------------------

    my_map  <- site_data |> dplyr::filter(!is.na(longstddecimaldegrees) & !is.na(latstddecimaldegrees)) |>
      sf::st_as_sf(coords = c("longstddecimaldegrees",
                              "latstddecimaldegrees"),
                   crs = st_crs(4326)) |>
      leaflet::leaflet() |>
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap")  |>
      addProviderTiles("Esri.WorldImagery", group = "Esri Satellite")  |>
      addProviderTiles("Esri.WorldTopoMap", group = "Esri Topo")  |>

      addCircleMarkers(popup = ~usiteid,
                       radius = 5,
                       #color = ~pal(is_complete),
                       group = "Points") |>
      addPolygons(data = geom_data,
                  fillColor = "#7570b3",
                  color = "#7570b3",
                  opacity = 0.4,
                  group = "Polygons") |>
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Esri Satellite", "Esri Topo"),
        overlayGroups = c("Polygons", "Points"),
        options = layersControlOptions(collapsed = FALSE)
      )
    # addLegend(position = "bottomright",
    #           pal = pal,
    #           values = ~is_complete,
    #           title = "Meets Verification")



    return_list[["map_data"]] <- my_map
    return_list[["missing_coords_msg"]] <- missing_message

    return_list[["ppi_data"]] <- ppi_data

    return_list[["ppi_msg"]] <- fetch_out()$fetch_msg$vegplot$ppi

    return_list[["vp_data"]] <- vp_data

    return_list[["veg_trans_data"]] <- veg_trans_data

    return_list[["veg_trans_sum_data"]] <- veg_trans_sum_data

    return_list[["site_data"]] <- df_disp

    return(return_list)


    # # my_map <- my_map@map
    #
    # list(
    #   map_data = my_map,
    #   map_msg = NULL,
    #   missing_coords_msg = missing_message,
    #   tabular_disp = df_disp
    # )

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

  # output$map_msg <- renderText({
  #   req(filter_out())
  #   filter_out()$map_msg
  # })


  ## Ecosite -----------------------------------------------------------------

  output$ecosite_comp_msg <- renderText({
    req(fetch_out())
    fetch_out()$fetch_msg$ecosite$comp
  })

  output$ecosite_statephase_msg <- renderText({
    req(fetch_out())
    fetch_out()$fetch_msg$ecosite$statephase
  })

  output$stm_not_site <- renderText({
    req(filter_out())
    filter_out()$ecosite_return$stm_not_site
  })

  output$site_not_stm <- renderText({
    req(filter_out())
    filter_out()$ecosite_return$site_not_stm
  })

  output$ecosite_data <- renderText({
    req(filter_out())
    filter_out()$ecosite_return$comp_req
  })

  ## Site --------------------------------------------------------------------





  output$site_data <- DT::renderDataTable({
    req(fetch_out())
    filter_out()$site_data
  })

  output$ppi_data <- DT::renderDataTable({
    req(filter_out())
    filter_out()$ppi_data
  })

  output$ppi_msg <- renderText({
    req(filter_out())
    filter_out()$ppi_msg
  })

  output$veg_trans_data <- DT::renderDataTable({
    req(filter_out())
    filter_out()$veg_trans_data
  })

  output$veg_trans_sum_data <- DT::renderDataTable({
    req(filter_out())
    filter_out()$veg_trans_sum_data
  })



  output$vp_data <- DT::renderDataTable({
    req(filter_out())
    filter_out()$vp_data
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
