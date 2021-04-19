# Example 03: Shiny dashboards

library(tidyverse)
library(shiny)
library(shinydashboard)
library(palmerpenguins)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "We love penguins!"),
  dashboardSidebar(
    menuItem("Bill length & depth", tabName = "lendep", icon = icon("drafting-compass")),
    menuItem("Body mass ", tabName = "bodymass", icon = icon("crow"))
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(".main-sidebar { font-size: 16px; }")) # change the font size to 16
    ),
    fluidRow(
      infoBox(
        "Male penguins", length(which(penguins$sex == "male")),
        icon = icon("air-freshener"), fill = TRUE,color = "maroon"
      ),
      infoBox(
        "Female penguins", length(which(penguins$sex == "female")),
        icon = icon("music"), fill = TRUE, color = "light-blue"
      )
    ), # /fluidRow
    tabItems(
      tabItem(
        "lendep",
        fluidPage(
          h1("Bil length and depth"),
          box(selectInput("species", "Species:", unique(penguins$species)), width = 8),
          box(plotOutput("correlation_plot"), width = 8)
        )
      ),
      tabItem(
        "bodymass",
        fluidPage(
          h1("Body mass"),
          box(selectInput("sex", "Sex:", c("female", "male")), width = 8),
          box(plotOutput("bodymass_plot"), width = 8)
        ) # /fluidPage
      ) # /tabItem
    ) # /tabItems
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$correlation_plot <- renderPlot({
    penguin_species <- filter(
      penguins,
      species == input$species & !is.na(sex)
    )

    ggplot(penguin_species, aes(x = bill_length_mm, y = bill_depth_mm, colour = sex)) +
      geom_point(size = 3) +
      scale_color_brewer() +
      theme_classic(base_size = 14)
  })

  output$bodymass_plot <- renderPlot({
    penguin_sex <- filter(
      penguins,
      sex == input$sex & !is.na(species)
    )

    ggplot(penguin_sex, aes(x = body_mass_g, colour = species, fill = species)) +
      geom_density(size = 3) +
      scale_color_brewer(palette = 2) +
      scale_fill_brewer(palette = 2) +
      theme_classic(base_size = 14)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
