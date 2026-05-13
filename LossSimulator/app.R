###################################
### Insurance Development Forum ###
### Disaster Risk Pooling Tool  ###
### App.R                       ###
###################################

#TODO: refactor and split this very long file of code into:
#file: define UI and tabs
#file: define navigation button options
#file: one file to define Tab 1-4
#variable: repeated plot features, e.g. axis / title font


# Added this to get wbtool1 functions
file_sources <-
  list.files(
    "./R/",
    pattern = "*.R",
    full.names = TRUE
  )

sapply(file_sources, source)

source('global.R')


### ----- Plotting Variables -----
fontsize_title <- 12
fontsize_axis <- 10
font_colour <- "#000000"
font_family <- "Raleway, sans-serif"

# Helper to apply included-perils filtering and factor level ordering
apply_included_perils_filter <- function(df, included_perils) {
  if (length(included_perils) > 0) {
    df %>%
      dplyr::filter(as.character(.data$Disaster) %in% included_perils) %>%
      dplyr::mutate(Disaster = factor(.data$Disaster, levels = included_perils))
  } else {
    df
  }
}
font_title <- list(family = font_family, size = fontsize_title, color = font_colour)
font_axis <- list(family = font_family, size = fontsize_axis, color = font_colour)

button_style <- "color: white; background-color: #ff0000; font-weight: bold;
position: relative; text-align:center; border-radius: 6px; border-width: 2px"

str_perils <- c("Drought", "Earthquake", "Flood", "Cyclone")

colourList <- c("#ff0000", "#ff00e9", "#00053A", "#ff0074")
## "#ff0074" = IDF brand Red (not possible to use in some text boxes, so is replaced with "red")
## "#ff00e9" = IDF brand Pink
## "#00053A" = IDF brand Navy Blue,
## "#ff0000" = Black

str_tabs <- c('Data Selection', 'Data Manipulation', 'Simulations', 'Outputs')


### ----- Tab names and numbers -----
tab_dict <-
  tibble::tibble(
    number = 1:4,
    name = str_tabs
  )

n_tabs <- nrow(tab_dict)

### ----- Define UI header -----
header <- dashboardHeader(
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 100px}"),
          tags$style(".main-header .logo {height: 100px}"),
          tags$link(href = "https://fonts.googleapis.com/css2?family=Raleway:wght@400;700&display=swap", rel = "stylesheet")
          #,
          #tags$span(tags$img(id = 'top-logo',
          #                   src = 'Horizontal logos.png',
           #                  alt = 'Contributors',
            #                 style = "height: 100px;"),
             #       style = "padding: 5px;")
  ),
  title = span(h2(id = "tool_title", "Loss Simulator")),
  titleWidth = 260
)

### ----- Define UI sidebar -----
sidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
  width = 260,
  sidebarMenu(
    id = 'side_tab',
    menuItem(
      text= "Use the Loss Simulator",
      tabName="main"
      ),
    menuItem(
      text = 'About',
      tabName = 'about'
    ),
    menuItem(
      text = 'User Guides',
      tabName = 'guides'
      ),
    p("The Loss Simulator is one component of the Disaster Risk Pooling Tool.
      "),
    div(
      id = 'logo-div',
      tags$img(id = 'allLogo', src = 'Vertical_logos_newIDFinclWB.png',
               alt = 'Contributors', width = '250px'
               )
      )
    )
  )

### ----- Define UI Body -----
body <- dashboardBody(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Raleway:wght@400;700&display=swap", rel = "stylesheet")
  ),
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(tags$style(shiny::HTML('.nav-tabs a {cursor: default}'))),
  tags$head(tags$style(shiny::HTML("#upload-error-modal .modal-content {background-color: #00053A; color: #ff0000}"))),
  tags$head(tags$style(shiny::HTML("#upload-error-modal .modal-title {background-color: #00053A; color: #ff0000}"))),
  tags$head(tags$style(shiny::HTML(".shiny-notification {height: 100px; width: 800px; position: fixed; top: calc(50% - 50px); left: calc(50% - 400px); font-size: 150%; text-align: center;}"))),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  useShinyjs(),
  hidden(
    lapply(seq(n_tabs), function(i) {
      div(class = "page",
          id = paste0("step", i),
          "Step", i
      )
    })
  ),
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(theme = 'custom.css',
        tabsetPanel(id = 'tabs',

          #######
          #Tab 1
          #######
          tabPanel(title = uiOutput('tool_settings_ui'), value = str_tabs[1],
            fluidPage(
              bsPopover(id = "tabs",
                        title = '',
                        content = 'Use the buttons at the bottom of the page to navigate.',
                        placement = "bottom", trigger = "hover", options = list(container ='body')),
              tab1_heading,
              # User input for advanced / basic mode
              uiOutput("advanced_ui"),
              br(),
              # User input for input data type
              uiOutput('data_type_ui'),
              br(),

              # User input for uploading data and accompanying text
              conditionalPanel("input.data_type == 'Manual Input'", uiOutput('upload_ui')),
              conditionalPanel("input.data_type == 'Manual Input'", uiOutput('manual_data_chosen_ui')),
              conditionalPanel("input.data_type == 'Manual Input'", uiOutput('uploaded_perils_ui')),

              # User input for picking country / archetype
              uiOutput('country_ui'),
              br(),

              # User input for choosing database and accompanying text
              conditionalPanel("input.data_type == 'Country' & input.advanced == 'Advanced'", uiOutput('manual_database_ui')),
              conditionalPanel("input.data_type == 'Country'", uiOutput('data_source_ui')),

              # User input for type of peril data
              uiOutput('damage_type_ui'),
              br(),

              # User input for cost per person assumption
              conditionalPanel("input.damage_type == 'People Affected Response Cost'", uiOutput('cost_per_person_ui')),

              # Create tabs to house data table and plot
              uiOutput('freq_loss_switch_ui'),
              uiOutput("display_data_tab1_ui"),
              br(),
              uiOutput('edited_data_ui')
            )
          ),

          #######
          #Tab 2
          #######
          tabPanel(title = uiOutput('data_ui'), value = str_tabs[2],
            fluidPage(
              # Scaling section
              scale_heading,
              uiOutput('select_scale_ui'),
              uiOutput('upload_scale_ui'),
              uiOutput('scale_table_ui'),
              conditionalPanel("input.select_scale == 'Manual Input'", uiOutput('manual_scaling_data_chosen_ui')),
              # Final data section
              uiOutput('edited_scale_data_ui'),
              uiOutput('model_message_ui'),
              br(),
              uiOutput('scaling_error_ui'),
              # De-trending section only appears when in advanced mode
              conditionalPanel("input.advanced == 'Advanced'", uiOutput("detrend_heading_ui")),
              conditionalPanel("input.advanced == 'Advanced'", uiOutput('trend_test_ui')),
              # Final data section
              final_data_heading,
              # Create tabs to house data table and plot
              uiOutput("display_data_tab2_ui")
            )
          ),

          #######
          #Tab 3
          #######
          tabPanel(title = uiOutput('input_ui'), value = str_tabs[3],
            fluidPage(
              sim_heading,
              uiOutput('peril_ui'),
              uiOutput('simulations_ui'),
            )
          ),

          #######
          #Tab 4
          #######
          tabPanel(title = uiOutput('output_ui'), value = str_tabs[4],
            fluidPage(
              # Header section
              outputs_heading,
              # Peril selection box
              uiOutput('select_peril_ui'),
              br(),
              # Confidence interval toggle
              uiOutput('confidence_interval_ui'),
              br(),
              # Budget input
              uiOutput('budget_ui'),
              br(),
              uiOutput('tab4_plot_tabs')
            )
          )
        ),
        # Navigation buttons (Prev / Next / etc.)
        br(),
        br(),
        uiOutput("progress_btn_ui")
      )
    ),
    # Admin Sidebar items
    tabItem(
      tabName = 'about',
      about_page
    ),
    tabItem(
      tabName = 'guides',
      user_guide
    )
  )
)

#####
# UI
#####
ui <- dashboardPage(
  header, sidebar, body, skin="blue",
  title="Disaster Risk Pooling Tool: Loss Simulator"
  )

#########
# Server
#########
server <- function(input, output, session) {
  # Disable Top Nav Tabs
  shinyjs::disable(selector = '.nav-tabs a')

  # Reactive tab data
  tab_data <- reactiveValues(data = tab_dict)

  # Build a country -> ISO3A lookup from EM-DAT-style event IDs.
  country_iso_lookup <-
    country_data %>%
    dplyr::filter(!is.na(.data$id)) %>%
    dplyr::transmute(
      country = as.character(.data$country),
      iso3a = toupper(sub(".*-([A-Za-z]{3})$", "\\1", as.character(.data$id)))
    ) %>%
    dplyr::filter(grepl("^[A-Z]{3}$", .data$iso3a)) %>%
    dplyr::distinct(.data$country, .data$iso3a)

  resolve_download_iso3a <- reactive({
    country_name <-
      if (!is.null(input$data_type) && identical(input$data_type, "Manual Input")) {
        uploaded_country <- tryCatch(upload_name(), error = function(e) NA_character_)
        if (length(uploaded_country) > 0) as.character(uploaded_country[[1]]) else NA_character_
      } else if (!is.null(input$country)) {
        as.character(input$country)
      } else {
        NA_character_
      }

    iso3a <-
      country_iso_lookup %>%
      dplyr::filter(.data$country == country_name) %>%
      dplyr::pull(.data$iso3a) %>%
      .[1]

    if (length(iso3a) == 0 || is.na(iso3a) || !grepl("^[A-Z]{3}$", iso3a)) {
      "UNK"
    } else {
      iso3a
    }
  })

  build_out_filename_occ <- function() {
    paste0("LossSim_", resolve_download_iso3a(), "_", format(Sys.Date(), "%Y%m%d"), "_occ.csv")
  }

  build_out_filename <- function() {
    paste0("LossSim_", resolve_download_iso3a(), "_", format(Sys.Date(), "%Y%m%d"), "_agg.csv")
  }

  # Download handler for peril data upload template
  output$peril_template_download <-
    downloadHandler(
      filename = "LossSimulator_uploadTemplate.xlsx",
      content = function(file) {file.copy("data/templates/LossSimulator_uploadTemplate.xlsx", file)}
      )

  # Download handler for peril data upload template
  output$scaling_template_download <-
    downloadHandler(
      filename = "manual_scaling_template.csv",
      content = function(file) {file.copy("data/templates/manual_scaling_template.csv", file)},
      contentType = "text/csv"
      )

  # Download all underlying data for tool handler
  output$allDataBtn <-
    downloadHandler(
      filename = function() {
        build_out_filename_occ()
      },
      content = function(file) {
        write.csv(get_sims_export(), file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

  output$allDataAggBtn <-
    downloadHandler(
      filename = function() {
        build_out_filename_agg()
      },
      content = function(file) {
        write.csv(get_sims_export_agg(), file, row.names = FALSE)
      },
      contentType = "text/csv"
    )


  ###############
  # Landing Page
  ###############
  # Beginning observeEvent functions for Landing page creates a pop up page that the user must accept to access the app
  histdata <- rnorm(1)

  # Close welcome modal on clicking accept button
  observeEvent(input$accept,{
    removeModal()
  })

  # About page link to go to guides
  observeEvent(input$guide_link_2, {
    updateTabItems(session, "side_tab", "guides")
  })

  # About page button to start using the tool
  observeEvent(input$start_btn, {
    updateTabItems(session, "side_tab", "main")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })


  ###############
  # Tab Controls
  ###############

  # Define navigation buttons (previous/next/run)
  output$progress_btn_ui <- renderUI({
    # Tab 2: "Run Tool" otherwise "Next"
    btn_label <-
      if (input$tabs == tab_label(2, tab_dict)) {"Run Tool"}
    else {"Next"}

    fluidRow(column(12, align = 'center',
      splitLayout(cellsWidths = 300,
        if (rv$page > 1)
          actionButton("prevBtn",
                       "Previous",
                       icon = icon("arrow-left"),
                       style = button_style,
                       width = '110px'
          ),
        if (rv$page == n_tabs)
          popify(
            downloadButton("allDataBtn",
                           "Download Simulations",
                           style = button_style,
                           icon = icon("download"),
                           width = '200px'
            ),
            title = '',
            content = 'Click to download loss simulation data, for input to Risk Pool Structuring Tool',
            placement = 'auto top',
            trigger = 'hover',
            options = NULL
          ),
        if (rv$page == n_tabs)
          popify(
            downloadButton("allDataAggBtn",
                           "Download Aggregate",
                           style = button_style,
                           icon = icon("download"),
                           width = '200px'
            ),
            title = '',
            content = 'Click to download aggregated loss simulation data by year, country, and peril',
            placement = 'auto top',
            trigger = 'hover',
            options = NULL
          ),
          popify(
            actionButton("rtnBtn",
                         "Restart",
                         style = button_style,
                         icon = icon("undo"),
                         width = '110px'),
            title = '',
            content = paste('Click to return to the ', str_tabs[1], ' page'),
            placement = 'auto top',
            trigger = 'hover',
            options = NULL
            ),
        if (rv$page < n_tabs)
          actionButton("nextBtn",
                       btn_label,
                       style = button_style,
                       icon = icon("arrow-right"),
                       width = '110px'
                       )
        )
      ))
    })

  # Observe any click on the restart button
  observeEvent(input$rtnBtn, {
    current_upload <<- FALSE
    current_upload_scaling <<- FALSE
    rv$page <- 0
    updateTabsetPanel(session, inputId = "tabs", selected = str_tabs[1])
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  # Define a reactive value which is the currently selected tab number
  rv <- reactiveValues(page = 1)

  # Disable the forward, back buttons depending on position
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < n_tabs)
    hide(selector = ".page")
  })

  # Define function for changing the tab number in one direction or the other as a function of forward / back clicks
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }

  # Observe the forward/back clicks, and update rv$page accordingly
  observeEvent(input$prevBtn, {
    navPage(-1)
    # Add so that next page loads at the top of the page
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  observeEvent(input$nextBtn, {
    navPage(1)
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  # Observe any changes to rv$page, and update the selected tab accordingly
  shiny::observeEvent(
    rv$page,
    {
      tab_number <- rv$page
      td <- tab_data$data
      tab_name <- td %>% dplyr::filter(number == tab_number) %>% .$name
      updateTabsetPanel(session, inputId = "tabs", selected = tab_name)
    }
  )

  # Observe any click on the tab menu, and update accordingly the rv$page object
  shiny::observeEvent(
    input$tabs,
    {
      tab_name <- input$tabs
      td <- tab_data$data
      tab_number <- td %>% dplyr::filter(name == tab_name) %>% .$number
      rv$page <- tab_number
    }
  )

  ## Look up function for tab label
  tab_label <- function(n, tab_dict) {
    tab_dict$name[match(n, tab_dict$number)]
  }

  # UIs for panel headers
  output$tool_settings_ui <- renderUI({
    tab_maker(n = 1, label = str_tabs[1],
              input = input,
              tab_data = tab_data)
  })
  output$data_ui <- renderUI({
    tab_maker(n = 2, label = str_tabs[2],
              input = input,
              tab_data = tab_data)
  })
  output$input_ui <- renderUI({
    tab_maker(n = 3, label = str_tabs[3],
              input = input,
              tab_data = tab_data)
  })
  output$output_ui <- renderUI({
    tab_maker(n = 4, label = str_tabs[4],
              input = input,
              tab_data = tab_data)
  })

  ########
  # Tab 1
  ########
  # Input for user mode
  output$advanced_ui <- renderUI({
    fluidRow(column(11, offset = 1,
      popify(
        radioButtons("advanced",
                     "Select User Mode",
                     choices = c('Basic', 'Advanced'),
                     selected = 'Basic',
                     inline = TRUE),
        title = "",
        content = 'We recommend starting with "Basic" mode to run the tool with a default statistical distribution. For more statistical options choose "Advanced" mode.',
        placement = "auto left",
        trigger = "hover",
        option = NULL)
    ))
  })

  # An input that depends on which type of mode (advanced or basic) is selected
  output$data_type_ui <- renderUI({
    req(input$advanced)
    use_core_data_edited(FALSE)
    if(input$advanced == 'Advanced'){
      radio_choices <- c('Country', 'Manual Input')
      radio_selected <- input$data_type
    }
    else {
      radio_choices = c('Country')
      if(is.null(input$data_type) || input$data_type == 'Manual Input'){
        radio_selected <- 'Country'
      }
      else {
        radio_selected <- input$data_type
      }
    }
    fluidRow(column(11, offset = 1,
      popify(
        radioButtons("data_type",
                     "Select Input Data",
                     choices = radio_choices,
                     selected = radio_selected,
                     inline = TRUE),
                title = "",
        content = 'Choose to upload preloaded historical loss data from EM-DAT for your
        chosen country, or switch to advanced mode to upload your own data.',
        placement = "auto left",
        trigger = "hover",
        option = NULL)
    ))
  })

  # Reset edited data to raw data
  observeEvent(input$data_type, {
    current_upload <<- FALSE
  })

  # Observes country data (list) based on input and displays available data sources
  available_data_source <- "EM_DAT"

  # Shows the upload and template download buttons if manual input is selected
  output$upload_ui <- renderUI({
    req(input$data_type)
    fluidPage(
      fluidRow(column(11, offset = 1,
        p("Use this option to upload historical event loss data from another source. Include one row per event and data for one country only."),
        p("Monetary loss in USD is required: any cost-per-person calculations must be done before data is uploaded."),
        p("Perils are restricted to Earthquake, Drought, Flood, and Cyclone.")
      )),
      fluidRow(column(11, offset = 1,
        downloadButton("peril_template_download", "Data Template")
      )),
      br(),
      fluidRow(column(11, offset = 1,
        fileInput(inputId = 'ownFile',
                  label = "Choose .xlsx file",
                  multiple = FALSE,
                  accept = c(".xlsx"),
                  buttonLabel = "Browse"),
      ))
    )
  })

  # Shared reactive: read and cache uploaded manual input data once
  uploaded_manual_data <- reactive({
    req(input$data_type, input$ownFile)

    if (input$data_type != 'Manual Input') {
      return(NULL)
    }

    inFile <- input$ownFile

    tryCatch({
      readxl::read_xlsx(inFile$datapath, sheet = "Historical_Loss_Data")
    }, error = function(e) {
      NULL
    })
  })

  uploaded_perils <- reactive({
    req(input$data_type, input$ownFile)

    if (input$data_type != 'Manual Input') {
      return(character(0))
    }

    uploaded_data <- uploaded_manual_data()

    if (is.null(uploaded_data)) {
      return(character(0))
    }

    if (!("Peril" %in% names(uploaded_data))) {
      return(character(0))
    }

    raw_perils <-
      uploaded_data$Peril %>%
      as.character() %>%
      unique() %>%
      .[!is.na(.) & . != ""]

    str_perils[str_perils %in% raw_perils]
  })

  # Show unique perils found in uploaded manual input file
  output$uploaded_perils_ui <- renderUI({
    req(input$data_type)

    if (input$data_type != 'Manual Input' || is.null(input$ownFile)) {
      return(NULL)
    }

    uploaded_data <- uploaded_manual_data()

    peril_text <- if (is.null(uploaded_data)) {
      "Unable to read perils from uploaded file."
    } else if (!("Peril" %in% names(uploaded_data))) {
      "Peril column not found in uploaded file."
    } else {
      perils <- uploaded_perils()

      if (length(perils) == 0) {
        "No perils found in uploaded file."
      } else {
        paste(perils, collapse = ", ")
      }
    }

    peril_text <- as.character(peril_text)
    formatted_peril_text <- if (grepl("[.!?]$", peril_text)) peril_text else paste0(peril_text, ".")

    fluidRow(
      column(
        11,
        offset = 1,
        strong(paste0("Perils in uploaded file: ", formatted_peril_text, " Distributions will not be fitted for missing perils."), style = "color: red")
      )
    )
  })

  # Create dynamic text to describe whether user data has been imported or not -
  # commented out until we can be sure the error message doesn't contradict the 'data uploaded' message - i.e. is acting reactively.
  #output$manual_data_chosen_ui <-
  #  shiny::renderUI(
  #    {
  #      if (is.null(input$ownFile) || !is.null(validate_upload())) {
  #        fluidRow(
  #          column(
  #            11,
  #            offset = 1,
  #            strong(
  #              "You have not imported any data or have imported invalid data.",
  #              style = "color: red"
  #            )
  #          ),
  #          br()
  #        )
  #      } else {
  #        fluidRow(
  #          column(
  #            11,
  #            offset = 1,
  #              tagList(
  #                "You have uploaded historical loss data for ",
  #                strong(upload_name())
  #              ),
  #              style = "color: red"
  #            ),
  #            br()
  #          )
  #      }
  #  }
  #)

  # Output that renders UI based on an input. If data type is country, then countries are shown, else, archetypes.
  # Manual input will show the upload dialogue.
  output$country_ui <-
    shiny::renderUI({

      shiny::req(input$data_type)
      use_core_data_edited(FALSE)

      if(input$data_type == 'Country'){
        btn_id <- "country"
        btn_text <- emdat_country_btn_text
        btn_choice <- countries
        btn_select <- 'Bangladesh'
        btn_width <- '250px'
        pop_content <- 'Select a country for which to examine historical disaster data
        (limited to those in EM-DAT with losses from earthquake, drought, flood or cyclone.'
      } else if(input$data_type == 'Archetype') { # ARCHETYPE HAS BEEN REMOVED FROM UI - NOT AN OPTION.
        btn_id <- "archetype"
        btn_text <- "Select Archetype"
        btn_choice <- archetypes
        btn_select <- 1
        btn_width <- '400px'
        pop_content <- 'Select an archetype that matches the characteristics of the country or countries of interest to you.'
      } else {
        return(NULL)
      }

      fluidRow(
        column(
          width = 11,
          offset = 1,
          popify(
            selectInput(btn_id,
                        btn_text,
                        choices = btn_choice,
                        selected = btn_select,
                        width = btn_width),
            title = '',
            content = pop_content,
            placement = "auto left",
            trigger = "hover",
            options = NULL
          )
        )
      )
  })

  # An output that depends on which type of damage is selected.
  output$damage_type_ui <- renderUI({
    req(input$data_type)
    if(input$data_type == 'Archetype'){# ARCHETYPE HAS BEEN REMOVED FROM UI - NOT AN OPTION.
      btn_choice <- c('Total Economic Damage', 'People Affected Response Cost')
      btn_selected <- 'People Affected Response Cost'
    }
    else if(input$data_type == 'Manual Input'){
      return(NULL)
    }
    else {
      btn_choice <- c('Total Economic Damage', 'People Affected Response Cost')
      btn_selected <- 'People Affected Response Cost'
    }
    fluidRow(column(11, offset = 1,
      popify(
        radioButtons('damage_type',
                     'Select impact metric',
                     choices = btn_choice,
                     selected = btn_selected,
                     inline = TRUE),
        title = 'Select impact metric',
        #content = "Select whether you would like to view the loss as People Affected (converted to a response cost, using the specified value) or as Total Economic Damage.", # ' If Archetype is chosen, you can only do the former' - text removed; ARCHETYPE HAS BEEN REMOVED FROM UI - NOT AN OPTION.
        content = "Select the impact metric to use in the analysis.",
        #content = People Affected (plot/table will show the number of people affected multiplied by the response cost per person) or as Total Economic Damage (as reported by EM-DAT). Only records with a data entry FOR THE SELECTED PARAMETER IN EM-DAT will show in the plot/table - typically there are fewer events with Total Economic Damage reported in the loss catalogue"
        placement = 'auto left',
        trigger = "hover",
        options = NULL
      )
    ))
  })

  # Create a UI output based on if People Affected was chosen - if so create input for Cost Per Person
  output$cost_per_person_ui <- renderUI({
    if(input$data_type == 'Manual Input'){
      return(NULL)
    }
    else {
      fluidRow(column(11, offset = 1,
        popify(
          numericInput('cost_per_person',
                       paste0('Input Cost Per Person (', ccy_code, ')'),
                       min = NA, max = NA,
                       step = 10, value = 50,
                       width = '250px'),
          title = '',
          content = "This is the cost of disaster response per person in US Dollars, to be used in estimating the overall response cost.",
          placement = "auto left",
          trigger = "hover",
          options = NULL),
        br()
      ))
    }
  })

  # Create Frequency / Loss toggle buttons
  output$freq_loss_switch_ui <- renderUI({
    fluidRow(column(10, offset = 1,
      popify(
        radioButtons('view_data',
                     NULL,
                     choices = c("Historical Losses", 'Frequency'),
                     inline = TRUE),
        title = '',
        content = 'Choose to view the data in terms of historical losses or the frequency of the loss-related events.',
        placement = 'auto left',
        trigger = "hover",
        options = NULL
      )
    ))
  })

  # Create tabs to house data table and plot and accompanying text and buttons
  output$display_data_tab1_ui <- renderUI({
    fluidRow(column(width = 10, offset = 1,
      tabBox(title = NULL, id = "tabset_data", width = 12,
        tabPanel("Plot",
          br(),
          withSpinner(plotlyOutput("peril_data_bars_plot"), type = 7)
        ),
        tabPanel("Table",
          withSpinner(DT::dataTableOutput('raw_data_table'), type = 7),
          style = "height: 350px; overflow-y: scroll")
        )
    ))
  })

  # Create data table to view that data selected by the user
  output$raw_data_table <- DT::renderDataTable({
    # Determine if the data was edited by the user

    shiny::req(input$view_data)

    cored <-
      if(use_core_data_edited())
      {
        core_data_edited$data
      }
    else
      {
        core_data()
      }

    shiny::req(cored)

    out <-
      if(input$view_data != 'Frequency'){
        cored[[1]] %>%
          dplyr::group_by(.data$country, .data$peril, .data$year) %>%
          dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
          tidyr::complete(
            country, peril,
            year = cored[[3]]$`Min Year`:cored[[3]]$`Max Year`,
            fill = list(value = 0)
          ) %>%
          tidyr::pivot_wider(
            names_from = peril,
            values_from = value,
            names_glue = "{peril}, USD"
          )
      } else {
        cored[[2]] %>%
          dplyr::group_by(.data$country, .data$peril, .data$year) %>%
          dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
          tidyr::complete(
            country, peril,
            year = cored[[3]]$`Min Year`:cored[[3]]$`Max Year`,
            fill = list(value = 0)
          ) %>%
          tidyr::pivot_wider(
            names_from = "peril", values_from = "value"
          )
      }

    country_name <- out$country[1]

    out <-
      out %>%
      dplyr::select( -"country") %>%
      dplyr::rename("Year" = "year")

      DT::datatable(out,
                    rownames = FALSE,
                    extensions = 'Buttons',
                    options =
                      list(
                        paging = FALSE,
                        lengthChange = FALSE,
                        buttons = c('copy', 'csv'),
                        columnDefs =
                          list(list(className = 'dt-center', targets = '_all')),
                        dom ='Brt'))  %>%
        DT::formatRound(setdiff(names(out), "Year"), digits = 0, interval = 3, mark = ",")

  })

  # Create bar plot to view data selected by the user - tab 1
  output$peril_data_bars_plot <- renderPlotly({
    # determine if the data was edited by the user

    shiny::req(input$view_data)

    cored <-
      if(use_core_data_edited())
      {
        core_data_edited$data
      }
    else
    {
        core_data()
    }


    shiny::req(cored)

    out <-
      if(input$view_data != 'Frequency'){
          cored[[1]] %>%
            dplyr::group_by(.data$country, .data$peril, .data$year) %>%
            dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
            tidyr::complete(
              .data$country,
              .data$peril,
              year = cored[[3]]$`Min Year`:cored[[3]]$`Max Year`, fill = list(value = 0)
            )
      } else {
          cored[[2]]
      }

    country_name <- out$country[1]

    out <-
      out %>%
      dplyr::rename(
        "Year" = "year" ,
        "Cost" = "value" ,
        "Disaster" = "peril"
      )

    if (input$data_type == 'Manual Input') {
      included_perils <-
        if (is.null(input$ownFile)) {
          character(0)
        } else {
          uploaded_perils()
        }

      if (length(included_perils) > 0) {
        out <-
          out %>%
          dplyr::filter(as.character(.data$Disaster) %in% included_perils) %>%
          dplyr::mutate(Disaster = factor(.data$Disaster, levels = included_perils))
      }
    }

    if (input$view_data == 'Frequency')
      {
      perils_out <- peril_names_global
      y_title <- "Frequency"
      }
    else
      {
      if (input$damage_type == 'People Affected Response Cost')
        {
        perils_out <- paste(peril_names_global, ccy_code, sep=", ")
        y_title <- paste0('Annual Response Cost (', ccy_code, ')')
        }
      else
        {
        perils_out <- paste(peril_names_global, ccy_code, sep=", ")
        y_title <- paste0('Annual Economic Loss (', ccy_code, ')')
        }
      }


    too_much_data <-
      if(length(cored) >= 3)
        {
          if(cored[[3]]$`Number of Years` > 100)
            {
            TRUE
            }
        else
            {
              FALSE
            }
        }
    else
      {
        FALSE
      }

    if(!too_much_data) {

    # Set the title based on the input
      plot_title <- if (input$damage_type == 'People Affected Response Cost')
        {
        paste0("<b>Average Annual Response Cost: ", country_name)
        }
      else
        {
          paste0("<b>Average Annual Loss: ", country_name)
          }


      out %>%
        plotly::plot_ly(x = ~ Year,
                        y = ~ Cost,
                        type = "bar",
                        color = ~ Disaster,
                        colors = colourList,
                        marker = list(
                          line = list(
                            color = "gray40",
                            width = 0.5
                          )
                        )
                      )%>%
        plotly::layout(title = plot_title,
                       font = font_title,
                       yaxis = list(title = y_title, font = font_axis),
                       xaxis = list(title = "", autotick = F, dtick = 1, font = font_axis),
                       barmode = "stack",
                       legend = list(orientation = "h", xanchor = "center", x = 0.5, y=-0.3))

    } else {

      out %>%
        dplyr::group_by(.data$Disaster) %>%
        dplyr::summarise(Cost = sum(.data$Cost) / cored[[3]]$`Number of Years`) %>%
        plotly::plot_ly(
          labels = ~Disaster,
          values = ~Cost,
          type = "pie",
          marker = list(colors = colourList)
        ) %>%
        plotly::layout(
          title = paste0("<b>plot_title: ", country_name),
          font = font_title,
          legend = list(orientation = "h", xanchor = "center", x = 0.5)
        )
    }

  })

  # Get a reactive object that selects country data (list) based on input, if country perils are selected/
  selected_country <-
    shiny::reactive(
      {
        shiny::req(input$country)
        dplyr::filter(country_data, .data$country == input$country)
      }
    )

  # Get a reactive object that picks up the selected source when advanced is chosen
  selected_source <-
    reactive(
      {
        shiny::req(input$data_type == 'Country' && input$advanced == 'Advanced')
        input$manual_data_type
      }
    )

  # Create reactive object for getting frequency data from selected country, if country perils are selected.
  country_frequency <-
    shiny::reactive(
      {
        shiny::req(input$country)
        dplyr::filter(frequency_data, .data$country == input$country)
      }
    )

  # Create a reactive object to get archetype data if selected.
  selected_archetype <-
    shiny::reactive(
      {
        shiny::req(input$archetype)
        dplyr::filter(archetype_cost_data, .data$archetype == archetype)
      }
    )

  # Create reactive object to get archetype frequency if selected.
  archetype_frequency <-
    shiny::reactive(
      {
        shiny::req(input$archetype)
        dplyr::filter(archetype_freq_data, .data$archetype == archetype)
      }
    )

  # Create a reactive object to retrieve the best source
  best_data_source <- "EM_DAT"

  # Prepare loss (Total Economic Damage) data if Total Economic Damage was selected. Subset by best data source
  prepare_loss_data <-
    shiny::reactive(
      {

        shiny::req(
          country_data,
          country_frequency(),
          best_data_source,
          input$damage_type
        )

        data_temp <- list()

        chosen_dmg_type <-
          if(input$damage_type == 'Total Economic Damage'){'damage'} else {'affected'}

        data_temp[[3]] <-
          tibble::tibble(
            `Min Year` = start_year,
            `Max Year` = end_year,
            `Number of Years` = end_year - start_year,
            `Data Type` = "Historic"
          )

        data_temp[[1]] <-
          selected_country() %>%
            dplyr::filter(
              .data$origin %in% best_data_source,
              .data$damage_type == chosen_dmg_type
            ) %>%
            dplyr::select(-"origin", -"damage_type") %>%
            dplyr::mutate(
                  peril =
                    factor(
                      .data$peril,
                      levels = str_perils
                    )
                )

        data_temp[[2]] <-
          country_frequency() %>%
          dplyr::filter(.data$damage_type == chosen_dmg_type) %>%
          dplyr::select(-"damage_type") %>%
          dplyr::group_by(.data$country, .data$peril, .data$year) %>%
          dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
          tidyr::complete(
            .data$peril, .data$country,
            year = data_temp[[3]]$`Min Year`[1]:data_temp[[3]]$`Max Year`[1],
            fill = list(value = 0)
          ) %>%
          dplyr::mutate(
              peril =
                factor(
                  .data$peril,
                  levels = str_perils
                )
            )

        data_temp
      }
    )

  # Reset edited data to raw data
  observeEvent(input$reset_edit, {
    use_core_data_edited(FALSE)
  })

  proxy <- dataTableProxy('raw_data_table')

  # Capture the edits to the raw_data_table
  observeEvent(input$raw_data_table_cell_edit, {
    # Indicate that we should instead use edited core data, instead of the default
    # Capture edits
    info = input$raw_data_table_cell_edit
    i = info$row
    j = info$col
    v = info$value
    # Update core data accordingly
    if (use_core_data_edited()){
      cdx <- core_data_edited$data
      head(cdx)
    }
    else {
      cdx <- core_data()
    }
    cdx1 <- cdx[[1]]
    cdx2 <- cdx[[2]]
    cdx1 <- widen_core_data(cdx1)
    # This needs to be +2 as widen_core_data changes the order of perils to drought, earthquake, flood, storm
    cdx1[i,j+2] <- as.numeric(v)
    cdx1 <- elongate_core_data(cdx1)
    cdx2 <- values_to_frequency(cdx1)
    use_core_data_edited(TRUE)
    edited <- list(cdx1, cdx2)
    core_data_edited$data <- edited
  })

  # Find the name of the country in the uploaded data
  upload_name <-
    shiny::reactive(
      {
        shiny::req(input$ownFile, input$data_type)
        inFile <- input$ownFile
        cdx3 <- readxl::read_xlsx(inFile$datapath, sheet = "Historical_Loss_Data")
        upload_country <- unique(cdx3$Country)
        return(upload_country)
    }
  )

  # Performs basic data validation on uploaded peril data
  validate_upload <-
    shiny::reactive({
      shiny::req(input$ownFile, input$data_type)

      err <- NULL
      inFile <- input$ownFile

      if(
        is.null(inFile) ||
        input$data_type != 'Manual Input' ||
        current_upload == FALSE)
      {
        err <- 'No Upload'
        return(err)
      }

      sheet_check <-
        any(grepl("Historical_Loss_Data", readxl::excel_sheets(inFile$datapath)))

      if(!sheet_check){
        err <- 'Workbook not uploaded, data input sheet is missing'
        return(err)
      }

      cdx1 <-
        readxl::read_xlsx(inFile$datapath, sheet = "Historical_Loss_Data") %>%
          dplyr::select(- dplyr::contains("..."))

      cdx_name <- names(cdx1)
      exp_name <-
        c(
          "Year",
          "Event Name or ID",
          "Country",
          "Peril",
          "Loss (USD)",
          "Loss Type",
          "Min Year",
          "Number of Years",
          "Countries included",
          "Perils included",
          "Loss Summary (all perils all countries)",
          "Value"
        )

##      print(cdx_name)
      print(exp_name)

      if (!identical(cdx_name, exp_name)) {
        err <-
          'File is missing one or more columns from
          ["Year", "Event Name or ID",	"Country",	"Peril",	"Loss (USD)",
          "Min Year", "Number of Years", "Data Type"]. Data not
          uploaded.'
      } else if (input$data_type == 'Country' || input$data_type == 'Manual Input') {
        upload_country <- unique(cdx1$Country)
        if (length(upload_country) > 1) {
            err <-
              'More than one country found in uploaded file. Data not uploaded,
              reverting to standard data.'
        }
        cdx1 <-
          cdx1 %>%
          dplyr::mutate(
            Year = as.integer(.data$Year),
            `Event Name or ID` = as.character(.data$`Event Name or ID`),
            `Country` = as.character(.data$Country),
            `Loss (USD)` = as.numeric(.data$`Loss (USD)`),
            `Min Year` = as.integer(.data$`Min Year`),
            `Number of Years` = as.integer(.data$`Number of Years`),
            `Data Type` = "Historic"
          )

        check_col_data <- purrr:::map(cdx1, function(x) all(is.na(x))) %>% unlist()

        if(any(check_col_data == TRUE)) {
          err <-
            'One or more of your columns contain either the wrong data type or
             are entirely blank'
          }
      }

      return(err)
  })

  # Handle file upload of data, place into edited data store
  observeEvent(input$ownFile, {

    req(input$ownFile)
    inFile <- input$ownFile
    current_upload <<- TRUE
    if (is.null(inFile)){
      return(NULL)
    }

    val <- validate_upload()

    if (is.null(val) || val == 'No Upload') {

      cdx1 <-
        inFile$datapath %>%
        readxl::read_xlsx(sheet = "Historical_Loss_Data") %>%
        dplyr::select(- dplyr::contains("..."))

      data_info <-
        cdx1 %>%
          dplyr::select("Min Year", "Number of Years") %>%
          dplyr::mutate(
            `Max Year` = .data$`Min Year` + .data$`Number of Years` - 1
          ) %>%
          dplyr::slice(1) %>%
          dplyr::mutate(`Data Type` = "Historical")

      # Cache and normalize uploaded peril levels to avoid repeated calls and
      # mismatches due to surrounding whitespace.
      peril_levels <- uploaded_perils()
      if (length(peril_levels) > 0L) {
        peril_levels <- trimws(peril_levels)
      }

      cdx1 <-
        cdx1  %>%
          dplyr::filter(
            .data$`Loss (USD)` > 0 & !is.na(.data$`Loss (USD)`),
            dplyr::between(
              .data$Year, data_info$`Min Year`[1], data_info$`Max Year`[1]
            )
          ) %>%
          dplyr::select(-"Number of Years") %>%
          dplyr::rename(
            "id" = "Event Name or ID",
            "country" = "Country",
            "year" = "Year",
            "peril" = "Peril",
            "value" = "Loss (USD)",
            "type" = "Loss Type"
          ) %>%
          # Normalize peril values (e.g., trim whitespace) before factoring
          dplyr::mutate(peril = trimws(.data$peril)) %>%
          dplyr::mutate(
            peril = {
              if (length(peril_levels) > 0L) {
                factor(.data$peril, levels = peril_levels)
              } else {
                # Fall back to using the raw peril values as levels to avoid
                # coercing everything to NA when no uploaded perils are found.
                factor(.data$peril)
              }
            }
          )

      cdx2 <-
        cdx1 %>%
          dplyr::group_by(.data$country, .data$peril, .data$year) %>%
          dplyr::summarise(value = dplyr::n(), .groups = "drop") %>%
          tidyr::complete(
            country,
            peril,
            year = data_info$`Min Year`[1]:data_info$`Max Year`[1],
            fill = list(value = 0)
          )  %>%
          dplyr::mutate(
            peril = {
              if (length(peril_levels) > 0L) {
                factor(.data$peril, levels = peril_levels)
              } else {
                factor(.data$peril)
              }
            }
          )

      use_core_data_edited(TRUE)
      core_data_edited$data <- list(cdx1, cdx2, data_info)
    } else {
      use_core_data_edited(FALSE)
      showModal(tags$div(id = 'upload-error-modal',
                         modalDialog(
                           title = 'File Upload Error',
                           val,
                           easyClose = TRUE,
                           footer = NULL)
      ))
    }
  })

  # Prepare cost per person data if damage type chosen is 'People Affected'
  prepare_cost_data <-
    shiny::reactive(
      {

        shiny::req(
          country_data,
          country_frequency(),
          best_data_source,
          input$damage_type,
          input$cost_per_person
        )

        data <- list()

        chosen_dmg_type <-
          if(input$damage_type == 'Total Economic Damage'){'damage'} else {'affected'}

        data[[3]] <-
          dplyr::tibble(
            `Min Year` = start_year,
            `Max Year` = end_year,
            `Number of Years` = end_year - start_year,
            `Data Type` = "Historic"
          )

        data[[1]] <-
          selected_country() %>%
          dplyr::filter(
            .data$origin %in% best_data_source,
            .data$damage_type == chosen_dmg_type
          ) %>%
          dplyr::mutate(value = input$cost_per_person * .data$value) %>%
          dplyr::select(-"origin", -"damage_type") %>%
          dplyr::mutate(
            peril =
              factor(
                .data$peril,
                levels = str_perils
              )
            )

        data[[2]] <-
          country_frequency() %>%
          dplyr::filter(.data$damage_type == chosen_dmg_type) %>%
          dplyr::select( -"damage_type") %>%
          dplyr::group_by(.data$country, .data$peril, .data$year) %>%
          dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
          tidyr::complete(
            country,
            peril,
            year = data[[3]]$`Min Year`[1]:data[[3]]$`Max Year`[1],
            fill = list(value = 0)
          ) %>%
          dplyr::mutate(
            peril =
              factor(
                .data$peril,
                levels = str_perils
              )
          )

        data
      }
    )

  # prepare archetype data if archetypes are chosen. Archetypes are by default cost per person method
  prepare_archetype_data <-
    shiny::reactive(
      {

        shiny::req(
          input$damage_type,
          selected_archetype(),
          archetype_frequency(),
          input$cost_per_person
        )

        data <- list()

        data[[1]] <-
          selected_archetype() %>%
            dplyr::select(-"data_type", -"damage_type", -"best_data") %>%
            dplyr::mutate(value = .data$value * input$cost_per_person)

        data[[2]] <-
          archetype_frequency %>%
            dplyr::select(-"data_type", -"damage_type")

        data
      }
    )

  # Generates description text depending on whether the user edits the data or not
  output$edited_data_ui <- renderUI({
    if (use_core_data_edited()) {
      fluidRow(
        column(6, offset = 1, strong("You are using edited data.", style = "color: red")),
        column(1, offset = 2, actionButton(inputId = 'reset_edit', label = 'Reset Data', icon('edit')))
      )
    } else {
      fluidRow(
        column(11, offset = 1, "You are viewing and using historical loss data.", style = "color: red"),
        column(11, offset = 1, "Events are shown only if the impact metric is non-zero in the historical
               loss catalogue. It is common for events to have a zero loss for Economic Damage and non-zero
               loss for people affected - therefore the number of events displayed will differ by impact
               metric selected.", style = "color: red"),
        column(11, offset = 1, "The data selection (Total economic damage or response cost) shown on this
               chart will be used in the next steps of simulation.", style = "color: red")
      )
    }
  })

  ########
  # Tab 2
  ########
  # Retrieve the scaling data for the country selected
  prepare_scale_data <- reactive({
    req(input$data_type)
    chosen_scale <- input$select_scale
    country_name <- input$country
    if(is.null(country_name) || chosen_scale == 'No Scaling'){
      return(NULL)
    }
    else {
      if (use_scale_data_edited()) {
        scaling_data <- scale_data_edited$data
      }
      else {
        scaling_data <- scale_data
      }
      if (chosen_scale == "Manual Input") {
        chosen_scale <- "Measure"
      }
      if (input$data_type == "Manual Input") {
        country_name <- upload_name()
      }
      # Filter data by country name
      scaling_data <- scaling_data[scaling_data$country == country_name,]
      # Remove unneeded columns
      scaling_data <- scaling_data %>% dplyr::select('year', tolower(chosen_scale))
      # Produce scaling factors
      scaling_data <- calc_scale_factors(scaling_data, chosen_scale)
      names(scaling_data) <- c('year', chosen_scale, paste(chosen_scale, 'Factor'))
      return(scaling_data)
    }
  })

  #
  output$upload_scale_ui <- renderUI({
    req(input$data_type, input$select_scale)
    if (input$data_type == 'Archetype' || input$select_scale != 'Manual Input') {
      return(NULL)
    }
    else {
      fluidPage(
        fluidRow(column(11, offset = 1, downloadButton("scaling_template_download", "Data Template"))),
        br(),
        fluidRow(column(11, offset = 1,
          popify(
            fileInput(inputId = 'ownFileScaling',
                      label = "Choose CSV file",
                      multiple = FALSE,
                      accept = c("text/comma-separated-values,text/plain",".csv"),
                      buttonLabel = "Browse"),
            title = '',
            content = "Select your own data file (.csv). If your uploaded file contains the currently selected country, it will be used to scale the data.",
            placement = "auto left",
            trigger = "hover",
            options = NULL)
        ))
      )
    }
  })

  #
  output$manual_scaling_data_chosen_ui <- renderUI({
    if (input$data_type == 'Archetype') {
      return(NULL)
    }
    else {
      if (!is.null(validate_upload_scaling()) || is.null(input$ownFileScaling)) {
        fluidPage(
          br(),
          fluidRow(column(11, offset = 1, strong(paste0(validate_upload_scaling()), style = "color: red"))),
          br()
        )
      }
      else {
        fluidPage(
          br(),
          fluidRow(column(11, offset = 1, strong(paste0("Scaling data for ", upload_name(), " uploaded correctly."), style = "color: red"))),
          br()
        )
      }
    }
  })

  #
  observeEvent(input$ownFileScaling, {
    req(input$ownFileScaling)
    inFile <- input$ownFileScaling
    current_upload_scaling <<- TRUE
    if (is.null(inFile)){
      return(NULL)
    }
    cdx1 <- read.csv(inFile$datapath)
    names(cdx1) <- tolower(names(cdx1))
    val <- validate_upload_scaling()
    if (is.null(val) || val == 'No Upload') {
      scale_data_edited$data <<- cdx1
      use_scale_data_edited(TRUE)
    }
    else {
      use_scale_data_edited(FALSE)
      scale_data_edited$data <<- scale_data
      showModal(tags$div(id = 'upload-error-modal',
                         modalDialog(
                           title = 'File Upload Error',
                           val,
                           easyClose = TRUE,
                           footer = NULL)
      ))
    }
  })

  # Performs basic data validation on uploaded scaling data
  validate_upload_scaling <- reactive({
    req(input$ownFileScaling, input$data_type)
    err <- NULL
    inFile <- input$ownFileScaling
    if (is.null(inFile) || current_upload_scaling == FALSE){
      err <- 'No Upload'
      return(err)
    }
    cdx1 <- read.csv(inFile$datapath)
    cdx_name <- names(cdx1)
    exp_name <- c("Year", "Country", "Measure")
    compared_country <- input$country
    available_countries <- as.character(upload_name_scaling())
    if (input$data_type == "Manual Input") {
      compared_country <- upload_name()
    }
    if (!identical(cdx_name, exp_name)) {
      err <- 'File is missing one or more columns from ["Year","Country","Measure"]. Scaling data not uploaded, reverting to standard data.'
    }
    else if (is.na(match(compared_country, available_countries)) || current_upload_scaling == FALSE) {
       err <- paste0(compared_country, ' is missing from preloaded scale data. Scaling data not uploaded, reverting to standard scaling data (where available).')
    }
    else {
      err <- NULL
    }
    scaling_upload_error(!is.null(err))
    return(err)
  })

  #
  upload_name_scaling <- reactive({
    req(input$ownFileScaling)
    inFile <- input$ownFileScaling
    cdx3 <- read.csv(inFile$datapath)
    upload_country <- unique(cdx3$Country)
    return(upload_country)
  })

  # Generates description text depending on whether the user edits the scaling data or not
  output$edited_scale_data_ui <- renderUI({
    req(input$data_type, input$select_scale)
    if (input$data_type != 'Archetype') {
      if (use_scale_data_edited()) {
        fluidRow(
          column(6, offset = 1, strong("You are using edited scaling data.", style = "color: red")),
          column(1, offset = 1, align = 'right', actionButton(inputId = 'reset_edit_scaling', label = 'Reset Data', icon('edit')))
        )
      } else {
        fluidRow(
          column(11, offset = 1, strong("You are using raw scaling data.", style = "color: red"))
        )
      }
    }
  })

  output$model_message_ui <-
    shiny::renderUI({
      req(input$data_type, input$select_scale)

      if (core_data()[[3]]$`Data Type`[[1]] == "Model") {
        fluidRow(
          column(
            11,
            offset = 1,
            strong("No trending available with model data", style = "color: red"))
        )
      }
    })

  # Output error if scaling fails
  output$scaling_error_ui <- renderUI({
    if (scaling_error() && input$data_type != 'Archetype') {
      fluidRow(column(11, offset = 1,
        strong("Scaling has not been applied: Scaling data does not identify data by year or is missing for selected years. ", style = "color: red")
      ))
    }
  })

  #
  observeEvent(input$select_scale, {
    use_scale_data_edited(FALSE)
    scaling_upload_error(FALSE)
    current_upload_scaling <<- FALSE
    if (input$select_scale != 'Manual Input') {
      scale_data_edited$data <- scale_data
    }
  })

  # Reset edited data to raw data
  observeEvent(input$reset_edit_scaling, {
    use_scale_data_edited(FALSE)
  })

  observeEvent(input$raw_scaled_data_cell_edit, {
    req(input$data_type)
    # Indicate that we should instead use edited core data, instead of the default & capture edits
    info = input$raw_scaled_data_cell_edit
    edited_country <- input$country
    if (input$data_type == "Manual Input") {
      edited_country <- upload_name()
    }
    used_scale <- tolower(input$select_scale)
    if (input$select_scale == "Manual Input") {
      used_scale <- "Measure"
    }
    i = info$row
    j = info$col
    v = info$value
    cdx <- prepare_scale_data()
    edited_year <- as.numeric(cdx[i,"year"])
    use_scale_data_edited(TRUE)
    scale_data_edited$data[scale_data_edited$data$year == edited_year & scale_data_edited$data$country == edited_country,used_scale] <<- as.numeric(v)
  })

  # Create a UI output that gives the user a choice on what data to scale by based on user mode
  output$select_scale_ui <- renderUI({

    req(input$data_type, input$advanced)

    cored <-
      if(use_core_data_edited()) {core_data_edited$data} else {core_data()}

    if(input$advanced == 'Basic' && input$data_type == 'Country'){
      scaled_choices <- 'Population'
    } else if(
      (input$advanced == 'Basic' && input$data_type == 'Archetype')
    ) {
      scaled_choices <- 'No Scaling'
    } else {
      country_name <- input$country
      scaling_data <- scale_data[scale_data$country == country_name,]
      scaling_data$inflation_ok <- length(which(is.na(scaling_data$inflation))) <= 1 # but inflation is commented out in global.app
      scaling_data$gdp_ok <- length(which(is.na(scaling_data$gdp))) <= 1
      use_inflation <- isTRUE(unique(scaling_data$inflation_ok))
      use_gdp <- isTRUE(unique(scaling_data$gdp_ok))
      if(use_inflation & use_gdp){
        scaled_choices <- c('Population', 'GDP')
      } else if(!use_inflation & use_gdp){
        scaled_choices <- c('Population', 'GDP')
      } else if(use_inflation & !use_gdp){
        scaled_choices <- c('Population', 'Inflation')
      } else if(!use_inflation & !use_gdp){
        scaled_choices <- c('Population')
      }
      scaled_choices <- c(scaled_choices, 'No Scaling')
      if (input$data_type == 'Manual Input') {
        if (is.na(match(upload_name(),scale_data$country))) {
          scaled_choices <- c('No Scaling')
        }
      }
      if (input$data_type == 'Archetype') {# Disabled option of archetype data
        scaled_choices <- c('No Scaling')
      }
      if (input$advanced == "Advanced" && input$data_type != 'Archetype') {# Disabled option of archetype data
        scaled_choices <- scaled_choices
      }
    }
    fluidRow(column(11, offset = 1,
      popify(
        selectInput('select_scale',
                    'Scale Data By',
                    choices = scaled_choices,
                    selected = scaled_choices[1]),
        title = '',
        content = 'Choose how you would like to see your data scaled (the default is by population).',
        placement = "auto left",
        trigger = "hover",
        options = NULL
      ),
      br()
    ))
  })

  #
  output$scale_table_ui <- renderUI({
    if(req(input$select_scale) == 'No Scaling' || req(input$data_type) == 'Archetype' ||# Disabled option of archetype data
      (scaling_upload_error() & input$select_scale == "Manual Input") | (is.null(input$ownFileScaling) & input$select_scale == "Manual Input")) {
      return(NULL)
    }
    else {
      fluidPage(
        fluidRow(column(10, offset = 1,
          DT::dataTableOutput('raw_scaled_data'), style = "height: 350px; overflow-y: scroll"
        )),
        br()
      )
    }
  })

  # Create a reactive object that gathers all the information from inputs so far and creates a list of two data-frames:
  # with homogenized format so that they can be passed to analysis functions.
  core_data_edited <- reactiveValues(data = data.frame())
  use_core_data_edited <- reactiveVal(value = FALSE)

  scale_data_edited <- reactiveValues(data = scale_data)
  use_scale_data_edited <- reactiveVal(value = FALSE)

  scaling_error <- reactiveVal(value=FALSE)
  scaling_upload_error <- reactiveVal(value = FALSE)

  core_data <-
    shiny::reactive(
      {
        shiny::req(
          input$data_type,
          input$damage_type
        )

        out <-
          if(use_core_data_edited()) {
            core_data_edited$data
          } else if ((input$data_type == "Country" |
                      input$data_type == "Manual Input")  &
                      input$damage_type == "People Affected Response Cost") {

            prepare_cost_data()

          } else if ((input$data_type == "Country" |
                      input$data_type == "Manual Input")  &
                     input$damage_type == "Total Economic Damage") {

            prepare_loss_data()

          } else {
            NULL
          }

        shiny::req(out)
        out
    }
  )

  # Create tabs to house data table and plot and accompanying text and buttons
  output$display_data_tab2_ui <- renderUI({
    fluidRow(column(width = 10, offset = 1,
      tabBox(title = NULL, id = "tabset_data", width = 12,
        tabPanel("Plot",
          br(),
          withSpinner(plotlyOutput("final_data_bars_plot"), type = 7)
        ),
        tabPanel("Table",
          withSpinner(DT::dataTableOutput('final_data_table'), type = 7), style = "height: 350px; overflow-y: scroll"
        )
      )
    ))
  })

  # Create data table to view that data selected by the user
  output$final_data_table <-
    DT::renderDataTable(
      {

        shiny::req(get_right_data())

        out <-
          get_right_data()[[1]] %>%
            dplyr::group_by(.data$country, .data$peril, .data$year) %>%
            dplyr::summarise(
              value = sum(.data$value), .groups = "drop"
            ) %>%
            tidyr::complete(
              country, peril,
              year = get_right_data()[[3]]$`Min Year`:get_right_data()[[3]]$`Max Year`,
              fill = list(value = 0)
            ) %>%
            tidyr::pivot_wider(
              names_from = peril,
              values_from = value,
              names_glue = "{peril}, USD"
            ) %>%
            dplyr::select(-"country") %>%
            dplyr::rename("Year" = "year")

        out %>%
          DT::datatable(
            rownames = FALSE,
            extensions = 'Buttons',
            options = list(
              paging = FALSE,
              lengthChange = FALSE,
              buttons = c('copy', 'csv'),
              columnDefs = list(list(
                className = 'dt-center', targets = '_all'
              )),
              dom = 'Brt'
            )
          ) %>%
          DT::formatRound(
            setdiff(names(out), "Year"),
            digits = 0,
            interval = 3,
            mark = ","
          )
    }
  )

  # Create bar plot to view data selected by the user - tab 2
  output$final_data_bars_plot <-
    plotly::renderPlotly({

    out <- get_right_data()

    too_much_data <-
      if(length(out) >= 3) {
        if(out[[3]]$`Number of Years` > 100) {
          TRUE
        } else {
          FALSE
        }
      } else {
        FALSE
      }

    out <- out[[1]]

    country_name <- out$country[1]

    out <-  dplyr::select(out, -"country")
    perils <- peril_names_global
    perils_out <- paste(perils, ccy_code, sep=", ")

    out <-
      out %>%
        dplyr::group_by(.data$peril, .data$year) %>%
        dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
        tidyr::complete(
          .data$peril,
          year = get_right_data()[[3]]$`Min Year`:get_right_data()[[3]]$`Max Year`,
          fill = list(value = 0)
        ) %>%
        dplyr::rename("Disaster" = "peril", "Cost"= "value", "Year" = "year")

    if (input$data_type == 'Manual Input' && !is.null(input$ownFile)) {
      included_perils <- uploaded_perils()
      out <- apply_included_perils_filter(out, included_perils)
    }

    if(!too_much_data) {
      if (input$damage_type == 'People Affected Response Cost')
        {
          y_title <- paste0('Annual Response Cost (', ccy_code, ')')
        }
        else
        {
          y_title <- paste0('Annual Economic Loss (', ccy_code, ')')
        }

    #y_title <-  paste0('Annual Loss (', ccy_code, ')')
    plotly::plot_ly(out,
                    x = ~Year,
                    y = ~Cost,
                    type = "bar",
                    color = ~Disaster,
                    colors = colourList,
                    marker = list(
                      line = list(
                        color = "#000000",
                        width = 0.5
                      )
                    )
                  ) %>%
      plotly::layout(title = paste0("<b>Scaled ", y_title, ": ", country_name),
                     font = font_title,
                     yaxis = list(title = y_title, font = font_axis),
                     xaxis = list(title = "", autotick = F, dtick = 1, font = font_axis),
                     barmode = "stack",
                     legend = list(orientation = "h", xanchor = "center", x = 0.5))
    } else {

      out %>%
        dplyr::group_by(.data$Disaster) %>%
        dplyr::summarise(Cost = sum(.data$Cost) / get_right_data()[[3]]$`Number of Years`) %>%
        plotly::plot_ly(
          labels = ~ Disaster,
          values = ~ Cost,
          type = "pie",
          marker = list(colors = colourList)
        ) %>%
        plotly::layout(
          title = paste0("<b>Processed Losses in ", country_name),
          font = list(family = "Raleway, sans-serif", size = 12, color ="#000000"),
          legend = list(orientation = "h", xanchor = "center", x = 0.5)
        )
    }
  })

  # Create table to visualize the country's scaling data
  output$raw_scaled_data <- DT::renderDataTable({
    out <- prepare_scale_data()
    chosen_scale <- input$select_scale
    if(is.null(out) || chosen_scale == "No Scaling" || input$data_type == 'Archetype'){
      NULL
    }
    else {
      chosen_factor <- paste(chosen_scale, 'Scaling Factor')
      if(chosen_scale == 'GDP'){
        chosen_scale <- paste(chosen_scale, ccy_code)
      }
      names(out) <- c('Year', chosen_scale, chosen_factor)
      DT::datatable(out,
                    rownames = FALSE,
                    editable = list(target = 'cell', disable = list(columns = c(0,2))),
                    extensions = 'Buttons',
                    options = list(paging = FALSE,
                                   lengthChange = FALSE,
                                   buttons = c('copy', 'csv'),
                                   columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                   dom = 'Brt')) %>%
        formatRound(c(2), digits = 0, interval = 3, mark = ",") %>%
        formatRound(c(3), digits = 2, interval = 3, mark = ",")
    }
  })

  # Reactive object to scale data
  scale_data_reactive <- reactive({

    final_list <- list()
    usde <- use_core_data_edited()
    if(usde){
      cored <- core_data_edited$data
    }
    else {
      cored <- core_data()
    }
    if(is.list(cored)){
      second_part <- cored[[2]]
      cored <- cored[[1]]
    }
    else {
      return(NULL)
    }
    req(input$data_type, input$select_scale)
    is_archetype <- input$data_type == 'Archetype'
    chosen_scale <- input$select_scale
    out <- NULL
    if(is.null(cored)){
      return(NULL)
    }
    if(is_archetype || chosen_scale == 'No Scaling'){
      out <- cored
      out <- out %>% dplyr::select(year:value)
      final_list[[1]] <- out
      final_list[[2]] <- second_part
      return(final_list)
    }
    else { # Scaling index chosen
      sd <- prepare_scale_data()
      if(is.null(sd)){
        return(NULL)
      }
      combined_data <- left_join(cored, sd, by = 'year')
      chosen_factor <- paste(chosen_scale, 'Factor')
      if (chosen_scale == "Manual Input") {
        chosen_factor <- "Measure Factor"
      }

      if (input$data_type == "Manual Input") {
        chosen_country <- upload_name()
      } else {
        chosen_country <- input$country
      }
      combined_data$value_scaled <-
        combined_data[[chosen_factor]] * combined_data$value
      if (any(is.na(combined_data[combined_data$country == chosen_country,"value_scaled"]))) {
        scaling_error(TRUE)
      }
      else {
        combined_data <- dplyr::mutate(combined_data, value = .data$value_scaled)
        scaling_error(FALSE)
      }
      out <-
        combined_data %>%
          dplyr::select("country", "peril", "year", "value")
      final_list[[1]] <- out
      final_list[[2]] <- second_part
      return(final_list)
    }
  })

  significant_trends <- reactiveVal(FALSE)

  # Generate detrending heading
  # output$detrend_heading_ui <- renderUI({
  #   detrend_heading
  # })

  # Tests if trend exists, if it does, a ui input is created to give user the option to correct for trend.
  # If a trend does not exists, text alerts the user that no trend is present.
  # output$trend_test_ui <- renderUI({
  #   sdr <- scale_data_reactive()
  #   sdr <- sdr[[1]]
  #   if(!is.null(sdr) && nrow(sdr) > 0){
  #     test_data <- sdr
  #     all_perils <- test_linear_trend(test_data)
  #     peril_names <- all_perils$peril[all_perils$p_value == TRUE & !is.na(all_perils$p_value)]
  #     if(identical(peril_names, character(0))){
  #       significant_trends(FALSE)
  #       fluidPage(
  #         fluidRow(column(11, offset = 1,
  #           strong('The application automatically checked each peril in the data for linear trends but found no trends.'),
  #         )),
  #         br()
  #       )
  #     }
  #     else {
  #       significant_trends(TRUE)
  #       peril_names <- paste(peril_names, collapse = ',')
  #       fluidPage(
  #         fluidRow(column(11, offset = 1,
  #           strong(paste0('The application automatically checked each peril in the data for linear trends and found trends in ', paste0(peril_names, collapse = ', '), '.')),
  #         )),
  #         br(),
  #         fluidRow(column(11, offset = 1,
  #           selectInput('trend_test',
  #                       'Correct Linear Trends',
  #                       choices = c('Yes', 'No'),
  #                       selected = 'No'),
  #         )),
  #         br()
  #       )
  #     }
  #   }
  #   else {
  #     return(NULL)
  #   }
  # })

  # this is a reactive object that tests (again) if trend is present and returns p value
  execute_trend_test <- reactive({
    sdr <- scale_data_reactive()
    sdr <- sdr[[1]]
    if(!is.null(sdr) && nrow(sdr) > 0){
      test_data <- sdr
      all_perils <- test_linear_trend(test_data)
      if(input$advanced == 'Basic' || is.null(all_perils)){
        return(NULL)
      }
      else {
        return(all_perils)
      }
    }
  })

  # This is a reactive object that corrects the trend for each peril if a pvalue is present that is
  # less than 0.05 and if the user selects to correct the trend.
  correct_trend <- reactive({
    if(is.null(input$trend_test) || is.null(execute_trend_test())){
      return(NULL)
    }
    else {
      if(input$trend_test  == 'No' || significant_trends() == FALSE){
        return(NULL)
      }
      trend_perils <- execute_trend_test()
      trend_data <- scale_data_reactive()
      # get all perils that have a significant trend
      peril_type <- trend_perils$peril[trend_perils$p_value == TRUE & !is.na(trend_perils$p_value)]
      if (identical(peril_type, character(0))) {
        return(NULL)
      }
      # split data into 3 groups - loss trend perils, loss other perils, and other data
      second_part <- trend_data[[2]]
      trend_data <- trend_data[[1]]
      trend_data_peril <-  trend_data[trend_data$peril %in% peril_type,]
      # Apply trend to trend data for both trends. When there is a p-value large than 0.05 the Tool replaces a past year value with
      # (linear prediction at starting year + (original value - linear prediction on the given year)).
      data_list <- list()
      for(i in 1:length(peril_type)){
        peril <- peril_type[i]
        sub_data <- trend_data[trend_data$peril == peril,]
        sub_data <- detrend_linear_data(sub_data)
        data_list[[i]] <- sub_data
      }
      # collapse list
      trended_data <- do.call('rbind', data_list)
      trend_data$value[trend_data$peril %in% peril_type] <- trended_data$trend_value
      final_data <- list()
      final_data[[1]] <- trend_data
      final_data[[2]] <- values_to_frequency(trend_data)
      return(final_data)
    }
  })

  ########
  # Tab 3
  ########
  # Reactive object to create right data based on all user inputs from tool settings and risk profile fitting tabs
  get_right_data <- reactive({
    req(input$data_type)

    usde <- use_core_data_edited()
    if(usde){
      cored <- core_data_edited$data
    }
    else {
      cored <- core_data()
    }
    scale_dat <- scale_data_reactive()
    trend_dat <- correct_trend()
    is_trend <- input$trend_test
    out <- NULL
    if(input$data_type == 'Archetype'){
      out <- cored
    } else {
      # Capture whether there are significant trends
      if(is.null(is_trend)){
        out <- scale_dat
      }
      else {
        if(is_trend == 'Yes' && !is.null(trend_dat)){
          out <- trend_dat
        }
        else {
          out <- scale_dat
        }
      }
    }

    if(length(cored) >= 3) {
      out <- append(scale_dat, list(cored[[3]]))
    }

    return(out)
  })

  # Reactive object that receives curated data and simulates Bernoulli
  simulate_bernoulli <- reactive({
    rd <- get_right_data()
    rd <- rd[[2]]
    temp<- sim_bern(rd)
    return(temp)
  })

  # Reactive object that receives curated data and fits a distribution
  fitted_distribution <- reactive({

    progress <- Progress$new()
    progress$set(message = "Fitting Distributions",value = 0)
    on.exit(progress$close())
    updateProgress <- function(value = NULL, detail = NULL,scale = 1) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / scale
      }
      progress$set(value = value, detail = detail)
    }
    rd <- get_right_data()
    data_type <- rd[[3]]$`Data Type`
    loss_dat <- rd[[1]]

    temp <-
      fit_distribution(
        loss_dat,
        updateProgress,
        data_type,
        years = c(rd[[3]]$`Min Year`, rd[[3]]$`Max Year`)
      )

    return(temp)
  })

  # reactive object that gets the AIC scores and filters based on the lowest one for each peril.
  filtered_distribution <- reactive({

    fd <- fitted_distribution()[[1]]
    number_of_dists(nrow(filter_distribution(fd)))
    filter_distribution(fd)

  })

  number_of_dists <- reactiveVal(value = NULL)

  output$simulations_ui <- renderUI({

    shiny::req(number_of_dists())

    if (!is.na(number_of_dists()) & number_of_dists() > 0 ) {
      fluidRow(
        uiOutput("qof_head_ui"),
        uiOutput('MLE_table_ui'),
        uiOutput('simulated_data_ui')
      )
    }
  })

  # Based on the perils that were fit, a series of possible UI inputs are created on this risk profiling tab.
  output$peril_ui <- renderUI({

    req(input$advanced)

    is_advanced <- input$advanced == 'Advanced'
    fd <- filtered_distribution()
    fdx <- fitted_distribution()[[1]]
    # Filter to keep only those non na values
    fdx_ok <- FALSE
    if(!is.null(fdx) & nrow(fdx) > 0){
      fdx_ok <- TRUE
      fdx <- fdx %>% dplyr::filter(!is.na(aic))
    }
    if(is.null(fd)){
      return(NULL)
    } else {

      chosen_flood <-
        fd  %>%
        dplyr::filter(.data$peril == 'Flood' & .data$distribution != "Freq") %>%
        dplyr::select("distribution") %>%
        unlist() %>%
        unname()

      chosen_earthquake <-
        fd  %>%
        dplyr::filter(.data$peril == 'Earthquake' & .data$distribution != "Freq") %>%
        dplyr::select("distribution")%>%
        unlist() %>%
        unname()

      chosen_drought <-
        fd  %>%
        dplyr::filter(.data$peril == 'Drought' & .data$distribution != "Freq") %>%
        dplyr::select("distribution")%>%
        unlist() %>%
        unname()

      chosen_storm <-
        fd  %>%
        dplyr::filter(.data$peril == 'Cyclone' & .data$distribution != "Freq") %>%
        dplyr::select("distribution")%>%
        unlist() %>%
        unname()

      chosen_freq <-
        fd %>%
          dplyr::filter(.data$distribution == "Freq") %>%
            dplyr::mutate(
              distribution =
                dplyr::case_when(
                  is.na(.data$mle2) ~ "Poisson", .default = "Negative Binomial"
                )
            )

      freq_flood_choices <- chosen_freq %>% dplyr::filter(peril == 'Flood') %>% .$distribution
      freq_earthquake_choices <- chosen_freq %>% dplyr::filter(peril == 'Earthquake') %>% .$distribution
      freq_drought_choices <- chosen_freq %>% dplyr::filter(peril == 'Drought') %>% .$distribution
      freq_storm_choices <- chosen_freq %>% dplyr::filter(peril == 'Cyclone') %>% .$distribution

      if(!is_advanced){
        flood_choices <- chosen_flood
        earthquake_choices <- chosen_earthquake
        drought_choices <- chosen_drought
        storm_choices <- chosen_storm
      }
      else {
        if(fdx_ok){

          flood_choices <- fdx %>% dplyr::filter(peril == 'Flood') %>% .$distribution
          earthquake_choices <- fdx %>% dplyr::filter(peril == 'Earthquake') %>% .$distribution
          drought_choices <- fdx %>% dplyr::filter(peril == 'Drought') %>% .$distribution
          storm_choices <- fdx %>% dplyr::filter(peril == 'Cyclone') %>% .$distribution

        } else {
          flood_choices <- earthquake_choices <- drought_choices <- storm_choices <- advanced_parametric
        }
      }
      no_go <- c()
      # Fitted Distributions for Flood
      if(length(chosen_flood) == 0){
        no_go <- c(no_go, 'Flood')
        flood_go <- NULL
      }
      else {
        if (!is_advanced) {
          title_flood <- paste('Best Severity Fit Distribution for Flood')
        }
        else {
          title_flood <- 'Choose Severity Distribution for Flood'
        }
        flood_go <-
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                'dist_flood_input',
                title_flood,
                choices = flood_choices,
                selected = chosen_flood
              ),
              shinyBS::bsPopover(
                id = "dist_flood_input",
                title = '',
                content =
                  'The selected distribution is the "best"
                   distribution for the observed flood damage.
                   In advanced mode you can select other
                   distributions as well.',
                placement = "auto left",
                trigger = "hover",
                options = NULL
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectInput(
                'freq_dist_flood_input',
                label = "Frequency Distribution for Flood",
                choices = freq_flood_choices
              )
            ),
          shiny::br()
        )
      }
      # Fitted Distributions for Earthquake
      if(length(chosen_earthquake) == 0){
        no_go <- c(no_go, 'Earthquake')
        earthquake_go <- NULL
      }
      else {
        if(!is_advanced){
          title_earthquake <- paste('Best Severity Fit Distribution for Earthquake')
        }
        else {
          title_earthquake <- 'Choose Distribution for Earthquake'
        }
        earthquake_go <-
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                'dist_earthquake_input',
                title_earthquake,
                choices = earthquake_choices,
                selected = chosen_earthquake
              ),
              shinyBS::bsPopover(
                id = "dist_earthquake_input",
                title = '',
                content =
                  'The selected distribution is the "best"
                   distribution for the observed earthquake damage.
                   In advanced mode you can select other
                   distributions as well.',
                placement = "auto left",
                trigger = "hover",
                options = NULL
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectInput(
                'freq_dist_earthquake_input',
                label = "Frequency Distribution for Earthquake",
                choices = freq_earthquake_choices
              )
            ),
            shiny::br()
          )
      }
      # Fitted Distributions for Drought
      if(length(chosen_drought) == 0){
        no_go <- c(no_go, 'Drought')
        drought_go <- NULL
      }
      else {
        if(!is_advanced){
          title_drought <- paste('Best Severity Fit Distribution for Drought')
        }
        else {
          title_drought <- 'Choose Distribution for Drought'
        }
        drought_go <-
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                'dist_drought_input',
                title_drought,
                choices = drought_choices,
                selected = chosen_drought
              ),
              shinyBS::bsPopover(
                id = "dist_drought_input",
                title = '',
                content =
                  'The selected distribution is the "best"
                   distribution for the observed drought damage.
                   In advanced mode you can select other
                   distributions as well.',
                placement = "auto left",
                trigger = "hover",
                options = NULL
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectInput(
                'freq_dist_drought_input',
                label = "Frequency Distribution for Drought",
                choices = freq_drought_choices
              )
            ),
            shiny::br()
          )
      }
      # Fitted Distributions for Storm
      if(length(chosen_storm) == 0){
        no_go <- c(no_go, 'Cyclone')
        storm_go <- NULL
      }
      else {
        if(!is_advanced){
          title_storm <- paste('Best Severity Fit Distribution for Cyclone')
        }
        else {
          title_storm <- 'Choose Distribution for Cyclone'
        }
        storm_go <-
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                'dist_storm_input',
                title_storm,
                choices = storm_choices,
                selected = chosen_storm
              ),
              shinyBS::bsPopover(
                id = "dist_storm_input",
                title = '',
                content =
                  'The selected distribution is the "best"
                   distribution for the observed cyclone damage.
                   In advanced mode you can select other
                   distributions as well.',
                placement = "auto left",
                trigger = "hover",
                options = NULL
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectInput(
                'freq_dist_storm_input',
                label = "Frequency Distribution for Cyclone",
                choices = freq_storm_choices
              )
            ),
            shiny::br()
          )
      }
      # Text for failed distribution fits
      no_go <- paste0(no_go, collapse = ', ')
      no_distribution_perils(no_go)
      ht <- fluidRow(
        p(paste0('No distributions could be fit to the data for the following perils: ', no_go)),
        br())
      # The final UI to be rendered
      fluidPage(
        conditionalPanel("length(chosen_flood) !== 0", fluidRow(column(11, offset = 1, flood_go))),
        conditionalPanel("length(chosen_drought) !== 0", fluidRow(column(11, offset = 1, drought_go))),
        conditionalPanel("length(chosen_storm) !== 0", fluidRow(column(11, offset = 1, storm_go))),
        conditionalPanel("length(chosen_earthquake) !== 0", fluidRow(column(11, offset = 1, earthquake_go))),
        conditionalPanel("length(no_go) > 0)", fluidRow(column(11, offset = 1, ht)))
      )
    }
  })


  no_distribution_perils <- reactiveVal(value = NULL)

  # Gathers the information on which distribution fits each peril and prepares the data for simulations
  prepared_simulations <- reactive({

    fd <- fitted_distribution()[[1]]
    x <- prepare_simulations(fd, dist_flood = input$dist_flood_input,
                             dist_drought = input$dist_drought_input,
                             dist_storm = input$dist_storm_input,
                             dist_earthquake = input$dist_earthquake_input)
    x
  })

  # This dataset takes the simulated Bernoulli data (15k) and the MLEs for the best distributions and runs the simulations
  ran_simulations <-
    shiny::reactive(
      {
        ps <- prepared_simulations()
        rep_sims <- repeatable(run_simulations, seed = 203) # setting seed to ensure repeatable outcomes
        rep_sims(ps)
      }
    )

  # Create table of simulations for exporting into Risk Pooling spreadsheet
  get_sims_export <- reactive({
    country_select <-
      if (input$data_type == 'Manual Input') {upload_name()} else {input$country}

      ran_simulations()$Event %>%
        dplyr::rename(
          "Event Year" = "year",
          "Peril" = "key",
          "Loss (USD)" = "value"
        ) %>%
        dplyr::mutate(
          `Event ID` = dplyr::row_number(),
          Country = country_select,
          `Loss Type` = "Occurrence"
        ) %>%
        dplyr::select(
          "Event Year",
          "Event ID",
          "Country",
          "Peril",
          "Loss (USD)",
          "Loss Type"
        )

  })


  # Create aggregated export with each peril in separate column, one row per event year
  get_sims_export_agg <- reactive({
    country_select <-
      if (input$data_type == 'Manual Input') {upload_name()} else {input$country}

    # Step 1: Rename columns to match export format and add Country
    out <- ran_simulations()$Event %>%
      dplyr::rename(
        "Event Year" = "year",
        "Peril"      = "key",
        "Loss (USD)" = "value"
      ) %>%
      dplyr::mutate(Country = country_select)

    # Step 2: Sum Loss (USD) where Event Year, Country, and Peril match
    out <- out %>%
      dplyr::group_by(`Event Year`, Country, Peril) %>%
      dplyr::summarise(`Loss (USD)` = sum(`Loss (USD)`), .groups = "drop")

    # Step 3: Pivot wider so each peril becomes its own column
    out <- out %>%
      tidyr::pivot_wider(
        names_from = Peril,
        values_from = `Loss (USD)`,
        values_fill = 0,
        names_expand = TRUE,
        names_glue = "{Peril} Loss (USD)"
      )

    # Step 4: Add Loss Type label
    out <- out %>%
      dplyr::mutate(`Loss Type` = "Aggregate")

    # Step 5: Ensure all peril columns exist; fill missing ones with zeros
    required_peril_cols <- c(
      "Drought Loss (USD)",
      "Earthquake Loss (USD)",
      "Flood Loss (USD)",
      "Cyclone Loss (USD)"
    )

    missing_peril_cols <- setdiff(required_peril_cols, names(out))
    if (length(missing_peril_cols) > 0) {
      for (col_name in missing_peril_cols) {
        out[[col_name]] <- 0
      }
    }

    # Step 6: Ensure Event Year is complete from 1 to 15000
    out <- out %>%
      tidyr::complete(`Event Year` = 1:15000) %>%
      dplyr::mutate(
        Country = dplyr::coalesce(Country, country_select),
        `Loss Type` = dplyr::coalesce(`Loss Type`, "Aggregate")
      )

    for (col_name in required_peril_cols) {
      out[[col_name]] <- dplyr::coalesce(out[[col_name]], 0)
    }

    # Step 7: Order columns and sort by year
    out <- out %>%
      dplyr::select(
        `Event Year`,
        Country,
        `Drought Loss (USD)`,
        `Earthquake Loss (USD)`,
        `Flood Loss (USD)`,
        `Cyclone Loss (USD)`,
        `Loss Type`
      ) %>%
      dplyr::arrange(`Event Year`)

    out
  })

  # Run function to generate simulation exhibit
  output$simulation_plot <- renderPlotly({
    simulations_plot()
  })

  # Plots two overlapping densities of the simulated data and the original loss data, comparing each by peril
  simulations_plot <- reactive({

    rs <- ran_simulations()
    rd <- get_right_data()
    ips <- input$peril_simulation
    ioc <- input$overlap_choices
    fill_colours <- c("#ff0000", "#00053A")
    if (ioc == 'Observed and Simulated data') {
      ioc <- c('Observed data', 'Simulated data')
    }
    else if (ioc == 'Observed data') {
      fill_colours <- fill_colours[1]
    }
    else if (ioc == 'Simulated data') {
      fill_colours <- fill_colours[2]
    }
    observed_data <-
      rd[[1]] %>%
        dplyr::filter(peril == ips) %>%
        dplyr::select(value) %>%
        dplyr::mutate(data_type = 'Observed data')
    # Limiting x axis to 125% of largest observed value
    plot_title <- paste0('<b>Comparison of Historical to Simulated Data for the Selected Fit for ', ips)
    y_title <- 'Density'
    x_title <- paste0('Value (', ccy_code, ')')
    g <- plot_simulations(rs = rs,
                          right_data = rd,
                          chosen_peril = ips,
                          overlap = ioc) +
      coord_cartesian(xlim = c(0, max(observed_data$value * 1.25))) +
      scale_fill_manual(values = fill_colours) +
      theme_classic()
    fig <- plotly::ggplotly(g, tooltip = "all")
    fig <- fig %>% layout(
      title = plot_title, font = font_title,
      yaxis = list(title = y_title, font = font_axis),
      xaxis = list(title = x_title, autotick = F, dtick = 1, font = font_axis),
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.3))
    return(fig)
  })

  # This table shows the AIC scores and MLEs for the all the distributions that converged for
  # each peril. In advanced mode, you can chose any of these.
  output$simulation_table <- DT::renderDataTable({

    fd <- fitted_distribution()[[1]]
    ok <- FALSE
    if(!is.null(fd)){
      if(nrow(fd) > 0){
        ok <- TRUE
      }
    }
    if(ok){

      selected_peril <- input$peril_simulation
      fd <- fd %>% dplyr::filter(peril == selected_peril) %>% dplyr::select(-peril)
      fd <- fd[, c('distribution', 'aic', 'mle1', 'mle2')]
      fd <- fd[!(fd$aic == "" | is.na(fd$aic)),]
      fd$mle1 <- round(fd$mle1, 3)
      fd$mle2 <- round(fd$mle2, 3)
      fd$aic <- akaike.weights(fd$aic)$weights
      names(fd) <- c('Distribution',
                     'AIC Weights',
                     'MLE 1',
                     'MLE 2')
      return(DT::datatable(fd,
                           rownames = FALSE,
                           extensions = 'Buttons',
                           options = list(paging = FALSE,
                                          lengthChange = FALSE,
                                          buttons = c('copy', 'csv'),
                                          columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                          dom = 'Brt')) %>%
               formatRound(-c(1), digits = 3, interval = 3, mark = ","))
    }
    else {
      return(NULL)
    }
  })

  # List which perils have distributions fitted to them
  get_peril_choices <-
    shiny::reactive({
      dat_sim <- ran_simulations()
      if(is.null(dat_sim)){
        return(NULL)
      }

      dat_sim$Yearly %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::select("key") %>%
        unique() %>%
        unlist() %>%
        unname()
    }
  )

  # Creates an input to select which perils you want to view on the output page.
  output$select_peril_ui <- renderUI({
    if (!is.na(number_of_dists()) & number_of_dists() > 0 ) {
      peril_choices <- get_peril_choices()
       fluidRow(column(11, offset = 1,
        checkboxGroupInput('select_peril',
                           label = 'Select Perils to View',
                           choices = peril_choices,
                           selected = peril_choices,
                           inline = TRUE),
        bsPopover(id = "select_peril",
                  title = '',
                  content = 'Select all perils which you would like to view.',
                  placement = "auto left",
                  trigger = "hover",
                  options = NULL
        )
      ))
    } else {
      fluidRow(
        column(11, offset = 1,
          tags$p("No perils available.")
        )
      )
    }

  })

  # Qual of Fit section heading
  output$qof_head_ui <- renderUI({
    req(input$advanced)
    if (input$advanced == "Advanced") {
      qof_heading
    }
    else {
      NULL
    }
  })

  # Display MLE table conditional on advanced mode being selected
  output$MLE_table_ui <- renderUI({
    req(input$advanced)
    if (input$advanced == "Advanced") {
      fluidPage(
        fluidRow(column(10, offset = 1,
          radioButtons('peril_simulation',
                       NULL,
                       choices = get_peril_choices(),
                       inline = TRUE),
          bsPopover(id = "peril_simulation",
                    title = '',
                    content = 'Select one specific peril to examine details on the fit with the distributions below.',
                    placement = "auto left",
                    trigger = "hover",
                    options = NULL
          )
        )),
        fluidRow(column(10, offset = 1, DT::dataTableOutput('simulation_table'))),
        br(),
        br()
      )
    }
    else {
      NULL
    }
  })

  output$simulated_data_ui <- renderUI({
    req(input$advanced)
    if (input$advanced == "Advanced") {
      fluidPage(
        fluidRow(column(10, offset = 1,
          radioButtons('overlap_choices',
                       '',
                       choiceNames = c('Observed and Simulated Data',
                                      'Observed Data',
                                      'Simulated Data'),
                       choiceValues = c('Observed and Simulated data',
                                       'Observed data',
                                       'Simulated data'),
                       selected = 'Observed and Simulated data',
                       inline = TRUE),
          bsPopover(id = "overlap_choices",
                    title = '',
                    content = 'The chart below will show one or both of the simulated and observed data.',
                    placement = "auto left",
                    trigger = "hover",
                    options = NULL
          )
        )),
        fluidRow(column(10, offset = 1, plotlyOutput('simulation_plot')))
      )
    }
    else {
      NULL
    }
  })

  ########
  # TAB 4
  ########
  # Layout for tabbed charts
  output$tab4_plot_tabs <- renderUI({
    fluidRow(column(width = 10, offset = 1,
      tabBox(id = "tabset_exhibits", width = 12,
        # Exhibit 1
        tabPanel("Exhibit 1",
          uiOutput('ex1_switch_ui'),
          br(),
          uiOutput('ex1_ui'),
          uiOutput('ex1_description')
        ),
        # Exhibit 2
        tabPanel("Exhibit 2",
          br(),
          uiOutput('ex2_ui'),
          uiOutput('ex2_description')
        ),
        #Exhibit 3
        tabPanel("Exhibit 3",
          uiOutput('ex3_switch_ui'),
          br(),
          uiOutput('ex3_ui'),
          uiOutput('ex3_description')
        ),
        # Exhibit 4
        tabPanel("Exhibit 4",
          br(),
          uiOutput('ex4_ui'),
          uiOutput('ex4_description')
        )
      )
    ))
  })

  # User toggle to turn confidence intervals on / off
  output$confidence_interval_ui <- renderUI({
    fluidRow(column(11, offset = 1,
      radioButtons('ci',
                   "Toggle 95% Confidence Intervals",
                   choices = c('On', 'Off'),
                   selected = 'Off',
                   inline = TRUE),
      bsPopover(id = "ci",
                title = '',
                content = "Enable confidence intervals to view the range in which 95% of losses occur.",
                placement = "auto left",
                trigger = "hover",
                option = NULL
      )
    ))
  })

  # This reactive data frame combines perils based on which are chosen to view on the risk profile tab.
  gather_perils <-
    shiny::reactive(
      {

        selected_perils <- input$select_peril
        if(is.null(selected_perils)){
          NULL
        } else {
          ran_simulations()$Yearly %>%
            dplyr::filter(.data$key %in% selected_perils) %>%
            dplyr::group_by(.data$year) %>%
            dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
            dplyr::mutate(key = "all_perils", .after = "year")
        }
    }
  )

  # This reactive data-frame combines the original loss data based on the perils selected to view from the risk profile page.
  gather_data <-
    shiny::reactive(
      {

        req(input$select_peril)
        selected_perils <- input$select_peril
        dat <- get_right_data()
        dat <- dat[[1]]
        # filter selected_perils
        dat %>%
          dplyr::filter(peril %in% selected_perils) %>%
          dplyr::group_by(.data$year) %>%
          dplyr::summarise(value = sum(.data$value))
      }
    )

  # User Input for budget input
  output$budget_ui <- renderUI({
    fluidRow(column(11, offset = 1,
      numericInput('budget',
                   paste0('Budget (', ccy_code, ' millions)'),
                   min = 0,
                   step = 5,
                   value = 0),
      bsPopover(id = "budget",
                title = '',
                content = 'The budget will be used for calculating the likelihood of exceeding funding.',
                placement = 'auto left',
                trigger = "hover",
                options = NULL
      )
    ))
  })

  # Set empty budget to 0
  ex_budget <- reactive({
    if (is.na(input$budget)) {
      budget <- 0}
    else {budget <- input$budget}
    return(budget)
    })

  # ----- Generate description text based on user inputs for the exhibits -----
  get_dynamic_text <- reactive({
    dmg_type <- input$damage_type
    cost_pp <- input$cost_per_person
    scale_type <- input$select_scale
    detrending <- input$trend_test
    scale_fail <- is.null(prepare_scale_data)
    detrend_fail <- is.null(correct_trend)

    if (dmg_type == 'People Affected Response Cost') {
      cpp_text <- paste0('This data was generated with a cost per person assumption of ', cost_pp, ' ', ccy_code, '.')
    }
    else {
      cpp_text <- NULL
    }
    #if (scale_fail && detrend_fail) {
    if (scale_fail) {
      trend_text <- paste0('The historical values have not been scaled.')
    }
    #else if (!scale_fail && detrend_fail) {trend_text <- paste0('The historical values have been scaled by ', scale_type, '.')}
    #else if (scale_fail && !detrend_fail) {trend_text <- paste0('The historical values have not been scaled.')}
    else {
      trend_text <- paste0('The historical values have been scaled by ', scale_type, '.')
    }
    out <- paste(cpp_text, trend_text)
    return(out)
  })

  # Exhibit 1
  ############
  # User toggle to switch between charts and tables
  output$ex1_switch_ui <- renderUI({
    fluidRow(column(11, offset = 1,
                    radioButtons('ex1_switch',
                                 '',
                                 choices = c('Plot', 'Table'),
                                 selected = 'Plot',
                                 inline = TRUE)
    ))
  })

  freq_sev_ci <-
    shiny::reactive({

      shiny::req(input$select_peril)

      if(filtered_distribution()$data_type[[1]] == "Model" |
        nrow(gather_data()) > 100)
      {
        NULL
      } else {

        return_list <- NULL
        input_data <- get_right_data()

        peril_dists <-
          lapply(list(input$dist_drought_input,
                      input$dist_earthquake_input,
                      input$dist_flood_input,
                      input$dist_storm_input), FUN = match_dist_names) %>%
          unlist() %>%
          dplyr::bind_cols(str_perils) #this changed the order of perils - check impact (c("Drought", "Earthquake", "Flood", "Cyclone"))

        names(peril_dists) <- c("dist", "peril")

        peril_dists <-
          dplyr::filter(peril_dists, .data$peril %in% input$select_peril)

        selected_dists <-
          fitted_distribution()[[1]] %>%
          dplyr::inner_join(peril_dists, by = c("distribution" = "dist", "peril"))

        valid_perils <- unique(selected_dists$peril)

        selected_dists <-
          fitted_distribution()[[1]] %>%
          dplyr::filter(
            .data$peril %in% valid_perils & .data$distribution == "Freq"
          ) %>%
          dplyr::bind_rows(selected_dists)

        selected_bootstraps <- list()

        for(i in 1:length(valid_perils)) {

          peril_name <- valid_perils[i]

          dist_name <-
            selected_dists %>%
              dplyr::filter(
                .data$distribution != "Freq" & .data$peril == peril_name
              ) %>%
              dplyr::select("distribution") %>%
              unlist() %>%
              switch(
                "Log normal" = "lnorm",
                "Weibull" = "weibull",
                # "Pareto" = "pareto",
                "Gamma" = "gamma"
              )

          selected_bootstraps[[i]] <-
            fitted_distribution()[[2]][[peril_name]][[dist_name]]
        }

        get_freq_sev_ci(
          selected_bootstraps
        )

      }
    }
  )

  # ----- Reactive dataframe: prepare data for plotting -----
  annual_loss_data <- reactive({
    budget <- ex_budget()
    gp <- gather_perils()
    gd <- gather_data()
    perils_ci <- freq_sev_ci()
    selected_perils <- input$select_peril

    if(is.na(budget) | is.null(gp) | is.null(gd) | is.null(selected_perils)){
      return(NULL)
      }
    else {
      dat_sim <- dplyr::filter(gp, !is.na(.data$value))
      dat <-
        gd %>%
          dplyr::filter(.data$value > 0) %>%
          dplyr::arrange(.data$year)

      sub_dat <-
        quant_that(dat_sim = dat_sim, dat = dat, combined_ci = perils_ci)

      sub_dat$variable <-
        factor(sub_dat$variable,
               levels = c('1 in 5 Years', '1 in 10 Years', '1 in 25 Years',
                          '1 in 50 Years', '1 in 100 Years', 'Long-term average',
                          'Highest historical annual loss', 'Most recent annual loss'
                          )
               )
      sub_dat$value <- round(sub_dat$value, 2)
      return(sub_dat)
    }
  })

  # ----- Exhibit 1 Table -----
  output$annual_loss_tably <-
    DT::renderDataTable({
      x <- annual_loss_data()
      if(!is.null(x)){
        x$value <- round(x$value)
        x$value_lower <- round(x$value_lower)
        x$value_upper <- round(x$value_upper)
        names_curr <- c(paste('Loss,',ccy_code), paste('Lower bound,', ccy_code),
                      paste('Upper bound,', ccy_code))
        names(x) <- c('Variable', names_curr)
        if (input$ci == 'Off') {
          x <- x[,1:2]
          names_curr <- names_curr[0:1]
          }
        DT::datatable(x,
                      rownames = FALSE, editable = FALSE,
                      extensions = 'Buttons',
                      options = list(lengthChange = FALSE,
                                     buttons = c('copy', 'csv'),
                                     dom = 'Brt')) %>%
          formatRound(names_curr, digits = 0, interval = 3, mark = ",")
        }
      }
      )

  # ----- Exhibit 1 Plot -----
  output$annual_loss_plotly <- renderPlotly({output_1_plot()})

  output_1_plot <- reactive({
    budget <- ex_budget()
    plot_dat <- annual_loss_data()
    plot_dat$legend <- c(rep("Simulated Losses", 6), rep("Historical Losses", 2))
    sp <-  paste0(input$select_peril, collapse = ', ')
    if(is.null(plot_dat) | is.null(sp)){
      return(NULL)
    }
    if (input$data_type == 'Manual Input') {
      country_archetype <- upload_name()
    }
    else {
      country_archetype <- tolower(input$data_type)
      country_archetype <- input[[country_archetype]]
    }
    plot_title <- paste0('<b>Estimate of Annual Loss by ', sp, ' for ', country_archetype)
    if (input$ci == 'On'){
      g_ci <- geom_errorbar(aes(x = variable,
                                ymin = value_lower/scale_size,
                                ymax = value_upper/scale_size))
    }
    else {
      g_ci <- NULL
    }
    if(budget != 0){
      g_budget1 <- geom_hline(aes(yintercept = budget, linetype = "Budget"), colour='#00053A')
      g_budget2 <- scale_linetype_manual(name = "", values = 1, guide = guide_legend(override.aes = list(color = '#ff00e9')))
    }
    else {
      g_budget1 <- NULL
      g_budget2 <- NULL
    }
    # Plot
    y_title <- paste0('Annual Loss (Million ', ccy_code, ')')
    g <- ggplot(plot_dat, aes(x = variable,
                              y = value/scale_size,
                              fill = legend,
                              text = paste0("Loss: ", paste(format(round(value / scale_size, 1), trim = TRUE), "m")))) +
      geom_bar(stat = 'identity', color ="#000000") +
      scale_fill_manual(values = c("#ff0000", "#ff00e9")) +
      labs(fill = "") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      theme_classic()
    g <- g + g_ci + g_budget1 + g_budget2
    # Convert to plotly
    fig <- plotly::ggplotly(g, tooltip = "text")
    fig <- fig %>% layout(
      title = plot_title, font = font_title,
      yaxis = list(title = y_title, font = font_axis),
      xaxis = list(title = "Return Period", autotick = F, dtick = 1, font = font_axis),
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.3))
    # Fix to edit legend. For some reason ggplotly adds a 1 after legend name - so foo becomes foo, 1. This removes the , 1
    for (i in 1:length(fig$x$data)){
      if (!is.null(fig$x$data[[i]]$name)){
        fig$x$data[[i]]$name =  gsub("\\(","",str_split(fig$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(fig)
  })


  # ----- Exhibit 1 Tab Contents -----
  output$ex1_ui <- renderUI({
    req(input$ex1_switch)
    if(input$ex1_switch == 'Plot'){
      withSpinner(plotlyOutput('annual_loss_plotly'), type = 7)
    }
    else {
      withSpinner(DT::dataTableOutput('annual_loss_tably'), type = 7)
    }
  })

  output$ex1_description <- renderUI({
    ex1_desc_text <- fluidPage(
      p('This exhibit shows the estimated annual loss across all filtered perils individually
        and also the sum of the perils (overall figure). As such the sum of the estimated
        losses of the individual perils at different return periods may differ to the associated overall figure.'),
      p('A return period of 1 in 5 years is the estimated annual loss expected to happen
        once every 5 years, i.e. a 20% probability. Similarly, a return period of 1 in 10
        years is the estimated annual loss expected to happen once every 10 years, i.e. a 10% probability.'),
      p('When confidence intervals are turned on, the error bars show the 95% confidence interval for each return period.'),
      get_dynamic_text()
    )
  })


  # Exhibit 2
  ############

  # Reactive object: store probability of exceeding budget
  probability_of_exceeding <- reactive({
    budget <- ex_budget()
    if(is.null(budget) | is.null(gather_perils())){
      NULL
    } else {
      dat_sim <- gather_perils()
      dat_sim <- dat_sim %>% filter(!is.na(value))
      # get budget
      output <- as.data.frame(quantile(dat_sim$value,seq(0.5, 0.99, by = 0.002), na.rm = TRUE))
      output$x <- rownames(output)
      rownames(output) <- NULL
      names(output)[1] <- 'y'
      # remove percent and turn numeric
      output$x <- gsub('%', '', output$x)
      output$x <- as.numeric(output$x)
      output$x <- output$x/100
      names(output)[1] <- 'Total Loss'
      names(output)[2] <- 'Probability'
      output$Probability <- 1 - output$Probability
      # find where budget equals curve
      prob_exceed <- output$Probability[which.min(abs(output$`Total Loss` - budget))]
      return(prob_exceed)
    }
  })

  # ----- Exhibit 2 Plot -----
  output$loss_exceedance_plotly <- renderPlotly({output_2_plot()})

  output_2_plot <- reactive({
    budget <- ex_budget()
    sp <- paste0(input$select_peril, collapse = ', ')
    perils_ci <- freq_sev_ci()
    if(is.na(budget) || is.null(gather_data()) || is.null(sp)){
      NULL
    }
    else {
      # get loss data and perils
      dat <- gather_data()
      dat_sim <- gather_perils()
      dat <- dat[order(dat$year, decreasing = FALSE),]
      largest_loss_num <- round(max(dat$value), 2)
      largest_loss_year <- dat$year[dat$value == max(dat$value)]
      largest_loss_num <- largest_loss_num/scale_size
      # get country input for plot title
      if (input$data_type == 'Manual Input') {
        country_archetype <- upload_name()
      }
      else {
        country_archetype <- tolower(input$data_type)
        country_archetype <- input[[country_archetype]]
      }
      plot_title <- paste0('<b>Loss Exceedance Curve by ', sp, ' for ', country_archetype)
      output <- as.data.frame(quantile(dat_sim$value,seq(0.5,0.99, by = 0.01), na.rm = TRUE))
      output$x <- rownames(output)
      rownames(output) <- NULL
      names(output)[1] <- 'y'
      # remove percent and turn numeric
      output$x <- gsub('%', '', output$x)
      output$x <- as.numeric(output$x)
      output$x <- output$x/100
      names(output)[1] <- 'Total Loss'
      names(output)[2] <- 'Probability'
      output$Probability <- 1 - output$Probability
      prob_exceed <- output$Probability[which.min(abs(scale_size*budget-output$`Total Loss`))]
      # caption
      exceed_budget <- paste0('Probability of exceeding budget = ', prob_exceed)
      if (!is.null(perils_ci) & !any(is.na(perils_ci))) {
        number_of_quantiles <- length(perils_ci$average)
        output$`Total Loss lower` <-  perils_ci$percentiles[1,paste0(seq(0.5, 0.99, by = 0.01) * 100, "%")]
        output$`Total Loss upper` <-  perils_ci$percentiles[number_of_quantiles,paste0(seq(0.5, 0.99, by = 0.01) * 100, "%")]
      } else {
        output$`Total Loss lower` <-  NA
        output$`Total Loss upper` <-  NA
      }
      plot_dat <- output
      clr_breaks <- c('Probability',
                      'Highest Historical Annual Loss',
                      'Prob. of Exceeding Budget')
      clr_values <- c('Probability' = "red",
                      'Highest Historical Annual Loss' = "#ff00e9",
                      'Prob. of Exceeding Budget' = "red")
      if (input$ci == 'On'){
        g_ci_low <- geom_line(aes(Probability, `Total Loss lower`/scale_size, color = 'CI Lower'), linetype = 'dotted')
        g_ci_upp <- geom_line(aes(Probability, `Total Loss upper`/scale_size, color = 'CI Upper'), linetype = 'dotted')
        clr_breaks <- c(clr_breaks, 'CI Lower', 'CI Upper')
        clr_values <- c(clr_values, 'CI Lower' = '#000000', 'CI Upper' = '#000000')
      }
      else {
        g_ci_low <- NULL
        g_ci_upp <- NULL
      }
      if(budget != 0){
        g_budget1 <- geom_hline(aes(yintercept = budget, color = "Budget"))
        clr_breaks <- c(clr_breaks, 'Budget')
        clr_values <- c(clr_values, 'Budget' = '#00053A')
      }
      else {
        g_budget1 <- NULL
      }
      # Plot
      y_title <- paste0('Annual Loss (Million ', ccy_code, ')')
      g <- ggplot(plot_dat, aes(x = Probability,
                                y = `Total Loss`/scale_size,
                                text = c(paste0("Probability: ", round(Probability * 100, 0),
                                                "%\n", paste0("Loss: ", paste(round(`Total Loss`/scale_size, 1), "mn")))),
                                group = 1)) +
        geom_line(aes(color = "Probability")) +
        scale_x_reverse(labels = scales::percent_format()) +
        geom_hline(aes(yintercept = largest_loss_num, color = "Highest Historical Annual Loss")) +
        geom_vline(aes(xintercept = prob_exceed, color = "Prob. of Exceeding Budget"), linetype = "dashed", alpha = 0.7) +
        scale_color_manual(name = "", breaks = clr_breaks, values = clr_values) +
        theme_classic()
      g <- g + g_ci_low + g_ci_upp + g_budget1
      # Convert to Plotly
      fig <- plotly::ggplotly(g, tooltip = "text")
      fig <- fig %>% layout(
        title = plot_title, font = font_title,
        yaxis = list(title = y_title, font = font_axis),
        xaxis = list(title = "", autotick = F, dtick = 1, font = font_axis),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1))
    }
    return(fig)
  })


  # ----- Exhibit 2 Value Box -----
  output$ex2_exceeding_prob <- renderValueBox({
    budget <- ex_budget()
    sp <- paste0(input$select_peril, collapse = ', ')
    perils_ci <- freq_sev_ci()
    if(is.na(budget) || is.null(gather_data()) || is.null(sp)){
      prob_exceed = NULL
    }
    else {
      # get loss data and perils
      dat_sim <- gather_perils()
      output <- as.data.frame(quantile(dat_sim$value,seq(0.5, 0.99, by = 0.01), na.rm = TRUE))
      output$x <- rownames(output)
      rownames(output) <- NULL
      names(output)[1] <- 'y'
      # remove percent and turn numeric
      output$x <- gsub('%', '', output$x)
      output$x <- as.numeric(output$x)
      output$x <- output$x/100
      names(output)[1] <- 'Total Loss'
      names(output)[2] <- 'Probability'
      output$Probability <- 1 - output$Probability
      prob_exceed <- output$Probability[which.min(abs(scale_size*budget-output$`Total Loss`))]
    }
    valueBox(prob_exceed,
             "Probability of Exceeding Budget",
             icon = NULL,
             width = NULL,
             color = "red"
    )
  })

  # ----- Exhibit 1 Tab Contents -----
  output$ex2_ui <- renderUI({
    fluidPage(
      withSpinner(plotlyOutput('loss_exceedance_plotly'), type = 7),
      valueBoxOutput("ex2_exceeding_prob", width = NULL)
    )
  })

  output$ex2_description <- renderUI({
    ex2_desc_text <- fluidPage(
      p('This exhibit shows the probability of a year taking place that exceeds the aggregate
        annual loss amount on the y-axis. The probability of exceeding the available budget
        is represented by the probability where the available budget line and the loss exceedance curve cross.'),
      p('When confidence intervals are turned on, the two dotted lines either side of the
        loss exceedance curve show the upper and lower bound of the 95% confidence interval.'),
      get_dynamic_text()
    )
  })

  # Exhibit 3
  ############

  # ----- Exhibit 3 User toggle -----
  output$ex3_switch_ui <- renderUI({
    fluidRow(column(11, offset = 1,
                    radioButtons('ex3_switch',
                                 '',
                                 choices = c('Plot', 'Table'),
                                 selected = 'Plot',
                                 inline = TRUE)
    ))
  })

  # Creates data for Exhibit 3, based on the inputs for the definitions of 'extreme' and 'severe'
  annual_loss_gap_data <- reactive({
    budget <- ex_budget()
    perils_ci <- freq_sev_ci()
    if(is.na(budget) || is.null(gather_perils()) || is.null(gather_data())){
      NULL
    }
    else {
      dat <- gather_data()
      dat_sim <- gather_perils()
      dat_sim <- dat_sim %>% filter(!is.na(value))
      # remove observations with 0, if any
      dat <- dat[dat$value > 0,]
      dat <- dat[order(dat$year, decreasing = FALSE),]
      #is_archetype <- input$data_type == 'Archetype' #not used in this function / plot
      severe <- input$severe
      severe <- 1 - (severe / 100)
      extreme <- input$extreme
      extreme <- 1 - (extreme / 100)

      # get quantiles for severe and extreme probability
      output <- quantile(dat_sim$value,c(severe, extreme))
      annual_avg <- mean(dat_sim$value)
      if (!is.null(perils_ci) & !any(is.na(perils_ci))) {
        number_of_quantiles <- length(perils_ci$average)
        annual_avg_lower <- round(perils_ci$average[[1]])
        output_lower <- perils_ci$percentiles[1, paste0(round(c(severe,extreme), digits = 2) * 100, "%")]
        annual_avg_upper <- round(perils_ci$average[[number_of_quantiles]])
        output_upper <- perils_ci$percentiles[number_of_quantiles, paste0(round(c(severe,extreme), digits = 2) * 100, "%")]
      }
      else {
        annual_avg_lower <- NA
        output_lower <- NA
        annual_avg_upper <- NA
        output_upper <- NA
      }
      # create data frame to store output with chart labels
      sub_plot_dat <- tibble("Average" = annual_avg,
                             "Severe" = output[1],
                             "Extreme" = output[2])

      sub_plot_dat <- pivot_longer(sub_plot_dat, cols = c("Average", "Severe", "Extreme"),
                                   names_to = "variable", values_to = "value")

      sub_plot_dat$value_lower <- c(annual_avg_lower, output_lower[1], output_lower[2])
      sub_plot_dat$value_upper <- c(annual_avg_upper, output_upper[1], output_upper[2])

      return(sub_plot_dat)
    }
  })
  #write.csv(sub_plot_dat, file = "/Users/stufraser/subplotdat.csv", row.names = FALSE)


  # ----- Exhibit 3 Plot -----
  output$annual_loss_gap_plotly <- renderPlotly({
    output_3_plot()
  })

  output_3_plot <- reactive({
    # get data from reactive object, where severe, extreme probabilities are taken into account.
    plot_dat <- annual_loss_gap_data()
    sp <- paste0(input$select_peril, collapse = ', ')
    if(is.null(plot_dat) || is.null(sp)){
      return(NULL)
    }
    # Get user input for plot title
    if (input$data_type == 'Manual Input') {
      country_archetype <- upload_name()
    }
    else {
      country_archetype <- tolower(input$data_type)
      country_archetype <- input[[country_archetype]]
    }
    plot_title <- paste0('<b>Estimate of Annual Loss by likelihood by ', sp, ' for ', country_archetype)
    if (input$ci == 'On'){
      g_ci <- geom_errorbar(aes(x = variable, ymin = value_lower/scale_size, ymax = value_upper/scale_size))
    }
    else {
      g_ci <- NULL
    }
    # Plot
    y_title <- paste0('Annual Loss (Million ', ccy_code, ')')
    g <- ggplot(plot_dat, aes(x = variable,
                              y = value/scale_size,
                              text = paste0("Loss: ", paste(format(round(value / 1e6, 1), trim = TRUE), "m")))) +
      geom_bar(stat = 'identity',
               fill = c('#00053A', 'red', '#ff00e9'),
               col = c('#000000', '#000000',"#000000"),
               alpha = 1) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      theme_classic()
    # Convert to Plotly
    g <- g + g_ci
    fig <- plotly::ggplotly(g, tooltip = "text")
    fig <- fig %>% layout(
      title = plot_title, font = font_title,
      yaxis = list(title = y_title, font = font_axis),
      xaxis = list(title = "", autotick = F, dtick = 1, font = font_axis)
      )
    return(fig)
  })


  # ----- Exhibit 3 Table -----
  output$annual_loss_gap_tably <- DT::renderDataTable({
    x <- annual_loss_gap_data()
    if(!is.null(x)){
      x$value <- round(x$value)
      x$value_lower <- round(x$value_lower)
      x$value_upper <- round(x$value_upper)
      names_curr <- c(paste('Loss,',ccy_code), paste('Lower bound,', ccy_code),
                      paste('Upper bound,', ccy_code))
      names(x) <- c('Variable', names_curr)
      if (input$ci == 'Off') {
        x <- x[,1:2]
        names_curr <- names_curr[0:1]
      }
      DT::datatable(x,
                    rownames = FALSE,
                    editable = FALSE,
                    extensions = 'Buttons',
                    options = list(lengthChange = FALSE,
                                   buttons = c('copy', 'csv'),
                                   dom = 'Brt')) %>%
        formatRound(names_curr, digits = 0, interval = 3, mark = ",")
    }
  })

  # # Output box for Severe probability
  # output$ex2_severe_prob <- renderValueBox({
  #   plot_dat <- annual_loss_gap_data()
  #
  #   severe_prob <-
  #     plot_dat %>%
  #       dplyr::filter(.data$variable == "Severe") %>%
  #       dplyr::mutate(value = .data$value / scale_size) %>%
  #       dplyr::select("value") %>%
  #       dplyr::slice(1) %>%
  #       unname() %>%
  #       round(digits = 2)
  #
  #   valueBox(severe_prob,
  #            "Associated Loss (Million USD)",
  #            icon = NULL,
  #            width = NULL,
  #            color = "red"
  #   )
  # })
  #
  # # Output box for Extreme probability
  # output$ex2_extreme_prob <- renderValueBox({
  #   plot_dat <- annual_loss_gap_data()
  #
  #   extreme_prob <-
  #     plot_dat %>%
  #       dplyr::filter(.data$variable == "Extreme") %>%
  #       dplyr::mutate(value = .data$value / scale_size) %>%
  #       dplyr::select("value") %>%
  #       dplyr::slice(1) %>%
  #       unname() %>%
  #       round(digits = 2)
  #
  #   valueBox(extreme_prob,
  #            "Associated Loss (Million USD)",
  #            icon = NULL,
  #            width = NULL,
  #            color = "red"
  #   )
  # }). REFACTORED BELOW::


  # ----- Exhibit 3 Value Boxes -----

  # Helper function to process and extract the probability value
  get_prob_value <- function(variable_name, scale_size, data) {

    data |>
      dplyr::filter(.data$variable == variable_name) |>
      dplyr::mutate(value = .data$value / scale_size) |>
      dplyr::select("value") |>
      unlist() |>
      unname() |>
      round(digits = 2)
    }

  # General function to create a value box
  create_value_box <- function(variable_name, scale_size, data, color) {

    prob_value <- get_prob_value(variable_name, scale_size, data)
    valueBox(prob_value,
             "Associated Loss (Million USD)",
             icon = NULL,
             width = NULL,
             color = color
    )
    }

  # Output boxes
  output$ex2_severe_prob <- renderValueBox({
    plot_dat <- annual_loss_gap_data()
    create_value_box("Severe", scale_size, plot_dat, "red")
    })

  output$ex2_extreme_prob <- renderValueBox({
    plot_dat <- annual_loss_gap_data()
    create_value_box("Extreme", scale_size, plot_dat, "red")
    })

  # ----- Exhibit 3 UI -----
  output$ex3_ui <- renderUI({
    req(input$ex3_switch)
    fluidPage(
      fluidRow(
        if(input$ex3_switch == 'Plot'){
          withSpinner(plotlyOutput('annual_loss_gap_plotly'), type = 7)
        }
        else {
          withSpinner(DT::dataTableOutput('annual_loss_gap_tably'), type = 7)
        }
      ),
      br(),
      fluidRow(
        column(width = 6,
          sliderInput("severe",
                      "Probability of severe loss occurring (%)",
                      value = 25,
                      min = 1,
                      max = 50,
                      step = 1,
                      ticks = FALSE)
        ),
        column(width = 6, align = "center",
          valueBoxOutput("ex2_severe_prob", width = NULL)
        )
      ),
      fluidRow(
        column(width = 6,
          sliderInput("extreme",
                      "Probability of extreme loss occurring (%)",
                      value = 10,
                      min = 1,
                      max = 50,
                      step = 1,
                      ticks = FALSE)
        ),
        column(width = 6, align = "center",
          valueBoxOutput("ex2_extreme_prob", width = NULL)
        )
      )
    )
  })

  output$ex3_description <- renderUI({
    ex3_desc_text <- fluidPage(
      p('This exhibit shows the estimated annual loss across all filtered perils
        individually and also the sum of the perils (overall figure). As such the sum
        of the estimated losses of the individual perils at different event severities
        may differ to the associated overall figure.'),
      p('When confidence intervals are turned on, the error bars show the 95% confidence
        interval for each severity.'),
      p('The figures represented by the bars are generated from simulated data.'),
      get_dynamic_text()
    )
  })

  #############
  # Exhibit 4 #
  #############

  # ----- Exhibit 4 Plot -----
  output$loss_exceedance_gap_plotly <- renderPlotly({
    output_4_plot()
  })

  output_4_plot <- reactive({
    # Get the probability of exceeding the budget and the budget
    budget <- ex_budget()
    data_type <- input$data_type
    sp <- paste0(input$select_peril, collapse = ', ')
    perils_ci <- freq_sev_ci()
    if(is.na(budget) || is.null(data_type) || is.null(gather_data())){
      NULL
    }
    else {
      # get the historical loss data and simulations data
      dat <- gather_data()
      dat_sim <- gather_perils()
      # remove nas and store largest loss number and year
      dat_sim <- dat_sim %>% filter(!is.na(value))
      dat <- dat[order(dat$year, decreasing = FALSE),]
      largest_loss_num <- max(dat$value)
      largest_loss_year <- dat$year[dat$value == max(dat$value)]
      # scale up budget and exceed budget to shift the curve
      budget <- budget*scale_size
      # get country input for plot title
      if (input$data_type == 'Manual Input') {
        country_archetype <- upload_name()
      }
      else {
        country_archetype <- tolower(input$data_type)
        country_archetype <- input[[country_archetype]]
      }
      plot_title <- paste0('<b>Estimate of Annual Funding Gap by ', sp, ' for ', country_archetype)
      # Create funding gap curve
      curve <- get_gap_curve(dat_sim$value, budget = budget)
      if (!is.null(perils_ci) & !any(is.na(perils_ci))) {
        number_of_quantiles <- length(perils_ci$average)
        curve_lower <- get_gap_curve(perils_ci$percentiles[1,paste0(seq(0.5, 0.99, by = 0.01) * 100, "%")],
                                     budget = budget,
                                     is_ci = TRUE)
        curve_upper <- get_gap_curve(perils_ci$percentiles[number_of_quantiles,paste0(seq(0.5, 0.99, by = 0.01) * 100, "%")],
                                     budget = budget,
                                     is_ci = TRUE)
        curve$value_lower <- curve_lower$`Funding gap`
        curve$value_upper <- curve_upper$`Funding gap`
      }
      else {
        curve$value_lower <- NA
        curve$value_upper <- NA
      }
      clr_breaks <- c('Probability')
      clr_values <- c('Probability' = "#ff0000")
      if (input$ci == 'On'){
        g_ci_low <- geom_line(aes(`Probability of exceeding loss`, value_lower/scale_size, color = 'CI Lower'),
                              linetype = 'dotted')
        g_ci_upp <- geom_line(aes(`Probability of exceeding loss`, value_upper/scale_size, color = 'CI Upper'),
                              linetype = 'dotted')
        clr_breaks <- c(clr_breaks, 'CI Lower', 'CI Upper')
        clr_values <- c(clr_values, 'CI Lower' = '#000000', 'CI Upper' = '#000000')
      }
      else {
        g_ci_low <- NULL
        g_ci_upp <- NULL
      }
      # Plot
      y_title <- paste0('Annual Funding Gap (Million ', ccy_code, ')')
      g <-  ggplot(curve,
                   aes(x = `Probability of exceeding loss`,
                       y = `Funding gap`/scale_size,
                       text = c(paste0("Probability: ", round(`Probability of exceeding loss` * 100, 0),
                                       "%\n", paste0("Funding Gap: ", paste(round(`Funding gap` / scale_size, 1), "m")))),
                       group = 1)) +
        geom_line(aes(color = "Probability")) +
        scale_color_manual(name = "",
                           breaks = clr_breaks,
                           values = clr_values) +
        scale_x_reverse(labels = scales::percent_format(), position = 'top') +
        theme_classic()

      g <- g + g_ci_low + g_ci_upp

      fig <- plotly::ggplotly(g, tooltip = "text")

      fig <- fig %>% layout(
        title = plot_title, font = font_title,
        yaxis = list(title = y_title, font = font_axis),
        xaxis = list(title = "", autotick = F, dtick = 1, font = font_axis),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1))
      return(fig)
    }
  })

  # ----- Exhibit 4 UI -----
  output$ex4_ui <- renderUI({
    fluidPage(
      withSpinner(plotlyOutput('loss_exceedance_gap_plotly'), type = 7)
    )
  })

  output$ex4_description <- renderUI({
    ex4_desc_text <- fluidPage(
      p('This exhibit shows the probability of experiencing different sized funding gaps (the
        difference between the estimated aggregate annual cost and the available budget). When the
        line is above 0 it indicates a funding surplus. When the line is below zero it indicates a
        funding deficit. The point at which the curve crosses zero is the probability that the available
        funds will be fully used.'),
      br(),
      p('Hover over the line to find the probability of a particular funding gap occurring.'),
      br(),
      p('When confidence intervals are turned on, the two dotted lines either side of the loss
        exceedance curve show the upper and lower bound of the 95% confidence interval.'),
      get_dynamic_text()
    )
  })
}


###############
### RUN APP ###
###############
shinyApp(ui, server)
