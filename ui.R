library(shiny)
library(plotly)
library(rworldmap)

description <- c("International Standards Organization",
                 "Country Name",
                 "EPI Peer Groupings by Geographic Region",
                 "GEO_subregion	Regional groups used for some imputation",
                 "Population, wdi2005",
                 "GDP per capita, WDI and CIA",
                 "Landlocked Country Dummy Variable",
                 "Land Area",
                 "High Density Countries",
                 "Environmental Performance Index (EPI)",
                 "Environmental Health",
                 "Ecosystem Vitality",
                 "Environmental Health (2)",
                 "Air Pollution",
                 "Water (effects on nature)",
                 "Biodiversity & Habitat",
                 "Productive Natural Resources",
                 "Climate Change",
                 "Environmental burden of disease",
                 "Water (effects on humans)",
                 "Air Pollution (effects on humans)",
                 "Air Pollution (effects on nature)",
                 "Water (effects on nature) (2)",
                 "Biodiversity & Habitat (2)",
                 "Forestry",
                 "Fisheries",
                 "Agriculture",
                 "Climate Change (2)",
                 "Adequate sanitation",
                 "Drinking water",
                 "Environmental burden of disease (2)",
                 "Indoor air pollution",
                 "Urban particulates",
                 "Local ozone",
                 "Sulfur dioxide emissions",
                 "Regional ozone",
                 "Water quality",
                 "Water quality Index - GEMS",
                 "Water stress",
                 "Conservation risk index",
                 "Effective conservation",
                 "Critical habitat protection",
                 "Marine Protected Areas",
                 "Growing stock change",
                 "Marine Trophic Index",
                 "Trawling intensity",
                 "Irrigation Stress",
                 "Intensive cropland",
                 "Agricultural Subsidies",
                 "Burnt Land Area",
                 "Pesticide Regulation",
                 "Emissions per capita",
                 "Industrial carbon intensity",
                 "Emissions per electricity generation",
                 "Adequate sanitation (2)",
                 "Drinking water (2)",
                 "Environmental burden of disease (3)",
                 "Indoor air pollution (2)",
                 "Urban particulates (2)",
                 "Local ozone (2)",
                 "Sulfur dioxide emissions (2)",
                 "Regional ozone (2)",
                 "Water quality (2)",
                 "Water stress (2)",
                 "Water quality Index - GEMS (2)",
                 "Conservation risk index (2)",
                 "Effective conservation (2)",
                 "Critical habitat protection (2)",
                 "Marine Protected Areas (2)",
                 "Growing stock change (2)",
                 "Marine Trophic Index (2)",
                 "Trawling intensity (2)",
                 "Irrigation Stress (2)",
                 "Agricultural Subsidies (2)",
                 "Intensive cropland (2)",
                 "Burnt Land Area (2)",
                 "Pesticide Regulation (2)",
                 "Emissions per capita (2)",
                 "Industrial carbon intensity (2)",
                 "Emissions per electricity generation (2)")

choices_index <- c(1:55)
hidden_index <- c(1:4, 7, 13, 23, 24, 28, 31, 55)
choices_index <- setdiff(choices_index, hidden_index)

choices <- colnames(countryExData)[choices_index]
choices_verbose <- description[choices_index]

region_types <- c("GEO3", "GEO3major", "IMAGE24", "GLOCAF", "Stern", "SRES", "SRESmajor", "GBD", "AVOIDname")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("World Environmental Performance Indexes"),
  helpText("Select the environmental performance index,",
           "the country regions classification",
           "and the performers list of interest"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
      column(4, selectInput("kpi", "Performance Index", choices_verbose, selected = choices_verbose[1])),
      column(4, selectInput("region_type", "Region Type", region_types, selected = region_types[1])),
      column(4, radioButtons("top_bottom", "Best/Worst Performers",
                             c("top-10" = "top10", "bottom-10" = "bottom10"))),
      column(12, plotlyOutput("plot")),
      column(6, tableOutput("table")),
      column(6, tableOutput("performers"))
    )
  )
)
