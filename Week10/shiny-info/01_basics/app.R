# Shiny overview
# Example 1
# Basic functionality

# **** optional exercise! ***** #
# 1) Change the dog breed selectInput() to radio buttons. Since there are a lot of breeds, just make the options Mischling klein, Chihuahua, Labrador Retriever, and Jack Russel Terrier (the four most popular breeds)
# 2) See how the dogs dataframe is filtered the same way in each output object? Turn the filtered dogs dataframe into a reactive object instead, using rdogs <- reactive(). Remember that when you call a reactive object, you have to put open and closed parens after it, e.g., my_object()

library(tidyverse)
library(kableExtra)
library(shiny)

load(here::here("Week10","shiny-info","01_basics", "ZurichDogs.RData"))

# Define UI
ui <- fluidPage(

  # Application title
  titlePanel("Dogs of Zurich"),

  # Sidebar with a dropdown menu for breed
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "breed", # the id tag that relates to a server object
        label = "Breed:",
        choices = c("Mischling klein", "Chihuahua", "Labrador Retriever", "Jack Russel Terrier"), # gives what the user sees
        selected = "Chihuahua" # default selection
      ), 
    ),

    # Show a plot of the city-wide distribution
    mainPanel(
      column(
        6, # column() modifies the layout (# is the column width)
        h4("District-level abundance"),
        plotOutput("distPlot")
      ),
      p(), # a line break
      p(),
      column(
        6,
        plotOutput("birthdayPlot")
      )
    ) # /mainPanel
  )
) # /fluidPage


# Server logic
server <- function(input, output) {
  rdogs <- reactive({
    dogs %>%
      filter(BREED == input$breed & !is.na(DISTRICT)) # this step is repeated in both scripts below, so we can have it here and then remove it from the following scripts
  })
  
  output$distPlot <- renderPlot({
    # count of chosen breed x by district
    rdogs() %>%
      #filter(BREED == input$breed & !is.na(DISTRICT)) %>% # using the tag breed to connect to ui
      ggplot(aes(x = factor(DISTRICT))) +
      xlab("District") +
      ylab("Number of dogs") +
      ggtitle(paste("Count of", input$breed, "\n in each district", sep = " ")) + # using the tag breed to connect to ui
      geom_bar(fill = "#74CEB7") +
      theme_classic(base_size = 16)
  })

  # use inputs here to subset the data to the user's district of choice:
  # breed = "Shih Tzu"

  output$birthdayPlot <- renderPlot({
    rdogs() %>%
      filter(BREED == input$breed & !is.na(DISTRICT)) %>%
      filter(DOG_BIRTHDAY < 2020) %>%
      ggplot(aes(
        x = factor(DISTRICT),
        y = DOG_BIRTHDAY
      )) +
      geom_boxplot() +
      xlab("District") +
      ylab("Dog's birth year") +
      theme_classic(base_size = 16)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
