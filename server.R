library(shiny)
library(plotly)
library(rworldmap)

# color scales used in the interactive map for representing EPI scale values
# depending on the fact that the EPI represent a positive or negative aspect,
# the red/green colors appear at the top or at the bottom; the blue color
# is used when "water" is involved
colors <- c("gyr", "ryg", "ryg", "gyr", "ryg", "ryg", "ryg", "gyr", "ryb", "ryg", 
  "ryg", "gyr", "gyr", "ryb", "gyr", "gyr", "ryg", "ryg", "ryg", "ryg",
  "ryb", "gyr", "gyr", "gyr", "gyr", "gyr", "ryb", "ryb", "gyr", "gyr",
  "ryg", "ryg", "ryb", "ryg", "ryb", "gyr", "byr", "ryg", "ryg", "gyr",
  "ryg", "gyr", "gyr", "gyr")

# description of the EPI appearing in the top-left select input widget
# redundant fields are taken out by appropriate index selection as done ahead
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

# not considering the raw data field
choices_index <- c(1:55)

# index of the fields we do not intend to show as selectable
hidden_index <- c(1:4, 7, 13, 23, 24, 28, 31, 55)

# resulting indexes
choices_index <- setdiff(choices_index, hidden_index)

# dataset columns to be considered for selection
choices <- colnames(countryExData)[choices_index]

# description of the selectable fields
choices_verbose <- description[choices_index]

# function to compute the gradient color scale used in the interactive map
get_color_scale <- function(field_idx) {
  scale <- colors[field_idx]
  scale_out <- c("green", "yellow", "red")
  if (scale == "ryg") {
    scale_out <- c("red", "yellow", "green")
  } else if (scale == "ryb") {
    scale_out <- c("red", "yellow", "blue")
  } else if (scale == "byr") {
    scale_out <- c("blue", "yellow", "red")
  } 
  scale_out
}

# plotly based function to plot the interactive map
myplotly <- function(field_idx, color_idx, kpi_field, field_desc) {
  l <- list(color = toRGB("red"), width = 0.5)
  f <- list(family = "Arial New", size = 12, color = "#7f7f7f")
  g <- list(showframe = FALSE, showcoastlines = FALSE, projection = list(type = 'Mercator'))
  z_formula <- as.formula(paste("~", kpi_field))
  colors_scale <- get_color_scale(color_idx)
  
  p <- plot_geo(countryExData) %>%
    add_trace(z = z_formula, color = z_formula, colors = colors_scale, text = ~Country,
      locations = ~ISO3V10, marker = list(line = l)) %>%
    colorbar(title = field_desc, tickprefix = '', yanchor = "middle", titleside ="top") %>%
    layout(title = paste(' <br>Source:<a href=" http://epi.yale.edu/Downloads">Environmental Performance Index Project</a><br><br>'),
      titlefont = f, geo = g
    )

  p
}

# function to compute the dataframe shown as region based EPI table
mydf <- function(field_descr, field, rt) {
  sternEnvHealth <- country2Region(inFile = countryExData, nameDataColumn = field, 
                                   joinCode = "ISO3", nameJoinColumn = "ISO3V10",
                                   regionType = rt, FUN = "mean")
  df <- data.frame(region = rownames(sternEnvHealth), region_index = sternEnvHealth)
  col2name <- paste("Average", field_descr, sep = " ")
  colnames(df) <- c("Region", col2name)
  df
}

# function to cmpute the dataframe showing the top-10/bottom-10 countries 
# and their selected EPI value
performerTable <- function(field, kpi_field, top_bottom) {
  dt <- countryExData[, c("Country", field), drop = FALSE]
  decreasing <- ifelse(top_bottom == "top10", TRUE, FALSE)
  ord <- order(dt[,field], decreasing = decreasing)
  dt <- dt[ord[1:10],]
  colnames(dt) <- c("Country", kpi_field)
  dt
}

shinyServer(function(input, output) {
  
  kpi <- reactive({input$kpi})
  
  region_type <- reactive({input$region_type})
  
  top_bottom <- reactive({input$top_bottom})
  
  output$plot <- renderPlotly({
    idx <- which(description == kpi())
    color_idx <- which(choices_verbose == kpi())
    field <- colnames(countryExData)[idx]
    myplotly(idx, color_idx, field, kpi())
  })

  output$table <- renderTable({
    idx <- which(description == kpi())
    field <- colnames(countryExData)[idx]
    mydf(description[idx], field, region_type())
  })  

  output$performers <- renderTable({
    idx <- which(description == kpi())
    field <- colnames(countryExData)[idx]
    performerTable(field, kpi(), top_bottom())
  })
  
})
