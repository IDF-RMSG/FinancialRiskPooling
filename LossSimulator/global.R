# Load additional libraries
library(actuar)
library(boot)
library(bsplus)
library(data.table)
library(dplyr)
library(DT)
library(fitdistrplus)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(htmltools)
library(MASS)
library(plotly)
library(qpcR)
library(readxl)
library(scales)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjqui)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tictoc)
library(tidyr)
library(tidyverse)
#library(reshape2) - deprecated
#library(optimx)
#library(WBTool1)


# Remove scientific notation in plots
options(scipen = 999)

#################################################################################
# Create objects to store floating constants in for indexing and custom equations
#################################################################################
# For scaling outputs
scale_size = 1000000 # scale by millions
start_year <- 2002
end_year <- 2024

emdat_last_update_file <- "data/Countries/emdat_country_losses_last_update.txt"
emdat_last_update <- if (file.exists(emdat_last_update_file)) {
  trimws(readLines(emdat_last_update_file, warn = FALSE, n = 1))
} else {
  "unknown"
}

if (is.na(emdat_last_update) || emdat_last_update == "") {
  emdat_last_update <- "unknown"
}

emdat_country_btn_text <- paste0(
  "Select Country historical loss data from EM-DAT (last update: ",
  emdat_last_update,
  ")"
)


#######################
# Read in country data
#######################
# EM-DAT
emdat_data_losses <-
  read.csv('data/Countries/emdat_country_losses.csv', stringsAsFactors = FALSE) |>
  dplyr::filter(
    dplyr::between(
      .data$Year, start_year, end_year
      )
    ) |>
  dplyr::mutate(Sum.of.Total.Damage = .data$`Sum.of.Total.Damage` * 1000)

emdat_data_yearly <-
  emdat_data_losses %>%
    dplyr::group_by(
      .data$`Country`,
      .data$`Year`,
      .data$Type.of.event
    ) %>%
    dplyr::summarise(
      `Total.affected` = sum(.data$`Sum.of.Total.affected`),
      `Total.Damage` = sum(.data$`Sum.of.Total.Damage`),
      `Event Count` = sum(.data$`Sum.of.Total.Damage` > 0),
      .groups = "drop"
    )

emdat_data_losses$origin <- 'EM_DAT'

# Combine data
country_data <- emdat_data_losses
# Rename columns
names(country_data) <-
  c('id', 'country', 'year', 'peril', 'affected', 'damage', 'origin')

# Melt data to collapse damage type
country_data <-
  country_data %>%
  tidyr::pivot_longer(
    c("damage", "affected"),
    names_to = "damage_type"
  )

#################################################################
# Create vectors to store choices for inputs in the app.r script
#################################################################
# Create a countries vector
countries <- unique(country_data$country)
countries <- countries[order(countries)]
peril_names_global <- unique(country_data$peril)
peril_names_global <- peril_names_global[order(peril_names_global)]

# Set currency code
ccy_code <- "USD"

# Create a place-holder for distribution types
basic_parametric <- c('Gamma', 'Log normal', 'Weibull', 'Pareto')

# Get frequency data
cost_data <- dplyr::filter(country_data, .data$damage_type == 'affected')
damage_data <-  dplyr::filter(country_data, .data$damage_type == 'damage')

cost_freq <-
  cost_data %>%
  dplyr::group_by(
    .data$country,
    .data$year,
    .data$peril,
    .data$damage_type
  ) %>%
  dplyr::summarise(
     value = sum(.data$value > 0),
    .groups = "drop"
  ) %>%
  tidyr::complete(.data$country, .data$peril, .data$year, fill = list(value = 0))

damage_freq <-
  damage_data %>%
  dplyr::group_by(
    .data$country,
    .data$year,
    .data$peril,
    .data$damage_type
  ) %>%
  dplyr::summarise(
    value = sum(.data$value > 0),
    .groups = "drop"
  ) %>%
  tidyr::complete(.data$country, .data$peril, .data$year, fill = list(value = 0))

# Combine the cost per person and loss data
frequency_data <- rbind(cost_freq, damage_freq)

country_data <-
  rbind(damage_data, cost_data)

rm(cost_freq, damage_freq, cost_data, damage_data)

#######################
# Read in scaling data
#######################
# Population data

population_data <- read.csv('data/Scale/population_data.csv')
gdp_data <- read.csv('data/Scale/gdp.csv')
# inflation_data <- read.csv('data/Scale/inflation.csv')

# Join data
scale_data <- full_join(population_data, gdp_data, by = c('Country', 'Year'))
# scale_data <- full_join(scale_data, inflation_data, by = c('Country', 'Year'))
rm(population_data, gdp_data)
names(scale_data) <- c('country', 'year', 'population', 'gdp')

#########################
# Read in archetype data
#########################
# Get all archetype cost data into one data frame
archetype_cost_data <- read_csv("data/Archetypes/archetype_cost_data.csv")
archetype_cost_data <- cbind(archetype_cost_data, data_type = "archetype_cost")
names(archetype_cost_data) <-
  c('archetype', 'year', 'peril', 'value', 'data_type')

# Get frequency data
archetype_freq_data <- expand_data(archetype_cost_data, "archetype")

# Fill NA with zeroes
#archetype_cost_freq <- fill_na(archetype_cost_freq)
archetype_freq_data$value[is.na(archetype_freq_data$value)] <- 0
archetype_freq_data$value <- ifelse(archetype_freq_data$value > 0, 1, 0)

# Get all archetype frequency data into data frame
archetype_freq_data <-
  archetype_freq_data[c('year', 'peril', 'value','archetype', 'data_type')]
archetypes <- unique(archetype_cost_data$archetype)
archetypes <- archetypes[order(archetypes)]

# Number of years in each time horizon
years_archetype <- unique(archetype_cost_data$year)
years_country <- unique(country_data$year)
min_year <- min(min(years_archetype),min(years_country))
max_year <- max(max(years_archetype),max(years_country))
num_years <-
  pmax(
    max(years_archetype) - min(years_archetype) + 1,
    max(years_country) - min(years_country) + 1
  )

## Define text for the about page
about_page <-
  fluidPage(
    fluidRow(column(10, offset = 1,
      h1('About'),
      #h2("Overview"),
      p("
         The Loss Simulator enables loss exceedance curves to be created from
         historical event loss catalogues. It is one component of the Disaster Risk Pooling Tool.
        "),
        p("
         The Disaster Risk Pooling Tool has been developed to support users to better understand the
         risk of climate and disaster losses, and use of risk pools. It responds
         to requests in the development and humanitarian sectors for better understanding of
         disaster risk analytics and the implementation of risk layering and risk pooling.
        "),
       p("
         Using the accompanying Risk Pool Structuring tool, users can take the
         output of the Loss Simulator, place those loss curves into a risk pool
         and explore how a pool of risks can be used to increase financial resilience.
         The Risk Pool Structuring tool is an Excel workbook.
         "),
      p("
         The Loss Simulator takes historical disaster event data from the EM-DAT
         catalogue (www.emdat.be), or via manual input (input template provided
         on GitHub), applies scaling factors for population growth,
         and fits parametric distributions to the data to simulate a loss curve.
         This is one approach to assess disaster loss profiles with incomplete
         historical loss data, which may exclude key historical events so the
         quality of the loss curves generated depends on the quality of the input data.
         Therefore, the output from this tool providesonly an indication of the
         losses associated with a disaster and actual losses may differ
         significantly from the tool output.
         "),
      p("
         The Tool is designed to work on a laptop or desktop PC with the web
         browser maximised, you may not be able to use the tool on smaller
         screens. Please read the user guides before use.
        "),
      fluidRow(column(width = 12, align = "center",
                      br(),
                      actionButton("start_btn", "Use the Loss Simulator"),
                      actionButton("Structuring_btn",
                                   label = "Go to Risk Pool Structuring",
                                   onclick = "window.open('https://github.com/IDF-RMSG/DisasterRiskpooling/tree/develop/RiskPoolStructuring', '_blank')")
      )),
      h2("Authorship"),
      p("
          The Loss Simulator was developed by Maximum Information on behalf of
          the Insurance Development Forum Risk Modelling Steering Group
          (RMSG) and the World Bank Disaster Risk Finance team under the Financial Services global department. It is based on the existing 'Financial Risk Assessment Tool'
          developed by the World Bank for capacity building of their clients.
          Insurance Development Forum RMSG are responsible for the update of the Loss Simulator.
        "),
      h2("Disclaimer"),
      p("
          The Loss Simulator has been developed to support users better understand
          disaster loss profiles, where historical event data catalogues are the
          main or only source of loss information (e.g., in the absence of catastrophe models).
          Information in the Tool is provided for educational purposes only and
          does not constitute legal or scientific advice or service.
          Neither the Insurance Development Forum, RMSG, Maximum Information  or the World Bank makes warranties or
          representations, express or implied as to the accuracy or reliability of
          the Tool or the data contained therein.
          "),
      p("
          Users of the Loss Simulator should seek
          qualified expert advice for specific diagnostic and analysis of a
          particular project. Any use thereof or reliance thereon is at the sole
          and independent discretion and responsibility of the user. No conclusions
          or inferences drawn from the Loss Simulator should be attributed to the
          developers of the tool.
          In no event will the Insurance Development Forum, RMSG, Maximum Information or the World Bank
          be liable for any form of damage arising from the application or
          misapplication of the tool, or any other associated materials.
        "
      )
    ))
  )

## Define text for user guide page
user_guide <-
  fluidPage(
    fluidRow(column(10, offset = 1,
      h1('User Guide'),
      p('As well as tool-tips in the tool user interface, please refer to the Disaster Risk Pooling user guide.
        This includes general information, key decision-making questions and fundamental
        principles of risk pooling, as well as the specific workflow of the Loss Simulator
        (used in Phase 1) and the Risk Pool Structuring Tool (Used in Phase 2 onwards).'
        ),
      fluidRow(column(width = 12, align = "center",
                      br(),
                      actionButton("UG_btn", label ="User Guide", onclick = "window.open('https://idf-rmsg.github.io/DisasterRiskPooling', '_blank')")
      ))
    ))
  )

# Define text for tab 1 heading
tab1_heading <-
  fluidRow(column(10, offset = 1,
    h2('Data Selection'),
    p("Please make selections to specify the data you wish to analyse. The data selected can be
      viewed using the graphic at the bottom of the page."),
    p("For extra flexibility in specifying the data source and/or statistics produced, please
    select Advanced mode, which enables upload of a new dataset, using templates provided on GitHub"),
    br()
  ))

# Define text for scaling heading
scale_heading <-
  fluidRow(column(10, offset = 1,
    h2('Scaling'),
    p("Scaling can remove trends caused by known indexes such as population, to help make
    losses more comparable between years. Basic mode always scales by population but advanced
      mode allows for more options. Where a manual input file does not attribute loss to a year
      (year might be labelled 1, 2, 3, etc., scaling can not been applied."),
    p("For each given year, a scaling factor is calculated by dividing the scaling data for the
    most recent year by the given year. Each peril year is then multiplied by the scaling factor
    for that year to give a corrected loss in terms of the most recent scaling year."),
    p("Edit the scaling data by double clicking and entering new population data in the relevant
    cell or by adjusting the data and using the advanced manual input approach."),
    p("Manual input data which starts before 2002 will not be scaled using the pre-loaded scaling factors (2002 onwards). Scale your data before loading, and select no scaling."),
    br()
  ))

# Define text for detrending heading (REMOVED FUNCTIONALITY SO REMOVING SECTION)
#detrend_heading <-
#  fluidRow(column(10, offset = 1,
#    h2('Detrending'),
#    p("Detrending also aims to remove trends from the data, but does so automatically, without extra data, by performing a linear regression."),
#    p("For each peril, the Tool looks for any linear trends and, if successful, the user can choose to modify the loss data in order to remove the trend."),
#    br()
#  ))

# Define text for final data heading
final_data_heading <-
  fluidRow(column(10, offset = 1,
    h2('Final Data '),
    p("The peril data displayed below has been multiplied by the chosen scaling factors and
      detrended (if detrending selected in advanced mode)."),
    p("This data is what the tool will use to fit the parametric distributions to each peril
      and produce the outputs."),
    br(),
  ))

# Define text for simulations heading
sim_heading <-
  fluidRow(column(10, offset = 1,
    h2('Simulations'),
    p("The tool runs 15,000 simulations each parametric distribution that has been successfully fitted
    to a given peril. In both Basic and Advanced modes Lognormal, Gamma and Weibull distributions are tested.
      If multiple severity distributions are fit for a peril, the one with the highest AIC (Akaike Information
      Criterion) weight is selected."),
    p("In advanced mode the user can change the selected severity distribution for a given peril."),
    p("For frequency distribution, if the sample variance equals the mean, Poisson distribution is
      used, otherwise Negative Binomial distribution is used."),
    br()
  ))

# Define text for quality of fit heading
qof_heading <-
  fluidRow(column(10, offset = 1,
    h2('Quality of Severity Fit and Maximum Likelihood Estimates'),
    p("The table below displays the results from each successful distribution fitting for the selected peril."),
    p("The Tool tries to find the best (most likely) parameters for each fitted distribution to estimate the
      distribution of the observed data. The maximum likelihood estimates (MLEs) are the parameters of the
      estimated distributions."),
    p("The AIC weight measures the quality of fit of the estimated distributions - the higher the AIC
      weight, the better the fit."),
    br()
  ))

# Define text for outputs heading
outputs_heading <-
  fluidRow(column(10, offset = 1,
    h2('Outputs'),
    p("In this tab, the user can view the simulated losses across the selected perils calculated
    from the distributions selected on the previous page.Selecting combinations of perils will combine
    each peril's simulations to produce a new set of 15,000 simulations. Therefore the risk profile of
    two perils is not the sum of the losses at each return period. When perils are combined the tools
      assumes no correlation between each peril."),
    p("95% confidence intervals can be toggled. These show the range of possible values for each
      return period that 95% of losses will fall within."),
    br()
  ))

