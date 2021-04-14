#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(here)
library(png)
library(grid)
library(ggimage)


babydata <- read_csv(here::here("HatchBabyExport.csv"))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Theme
    theme = shinytheme("darkly"),

    # Application title
    titlePanel("Monitoring the first two months of the Lil Silbigers"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId = "which.baby", # the id tag that relates to a server object
                label = "Lil Beeb:",
                choices = c("Blakely", "Micah"), # gives what choices the user sees
                selected = "Blakely" # default selection
            ),
            uiOutput("image", align = "center") # image of baby
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("weightPlot"), # weight plot
            plotOutput("diaperPlot") # diaper plot
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
## Manipulate dataframe before plotting  
    
    babyweight <- babydata %>% 
        rename(Name = 'Baby Name') %>% 
        filter(Activity == "Weight") %>% 
        select(-'End Time') %>% 
        separate(col = 'Start Time', into = c('Date','Time'), sep = " ", remove = T) %>% 
        mutate(Date = lubridate::mdy(Date)) %>% 
        mutate(Amount = as.numeric(Amount))
    
    # create personal palette
    palette<-c("mediumaquamarine", "steelblue", "orchid4")
    
    # create list of emoji image paths
    images <- c(here("www","Poop.png"),
                here("www","Drip.png"),
                here("www","Skull.png"))
    
    diaper <- babydata %>% 
      rename(Name = 'Baby Name',
             Date = 'Start Time') %>% 
      filter(Activity == "Diaper") %>% 
      mutate(Date = lubridate::mdy_hm(Date)) %>% 
      separate(col = 'Date', into = c('Date','Time'), sep = " ") %>% 
      select(c(Name,Date,Amount,Notes)) %>% 
      group_by(Name) %>% 
      count(Amount) %>% 
      mutate(images = images) %>% 
      ungroup()

## Conditional filtering based on sidebar selection
    # scatter plot
    weight.filter <- reactive({
        babyweight %>%
            filter(Name == input$which.baby & !is.na(Amount)) # this step is repeated in both scripts below, so we can have it here and then remove it from the following scripts
    })
    
    # bar graph
    diaper.filter <- reactive({
      diaper %>%
        filter(Name == input$which.baby & !is.na(Amount)) # this step is repeated in both scripts below, so we can have it here and then remove it from the following scripts
    })

    output$weightPlot <- renderPlot({
        
## Generate scatter and line plot of baby weight over time with trendline based on input$which.baby from ui.R
        
        weight.filter() %>% 
            ggplot(aes(x = Date, y = Amount)) +
            xlab("Date") +
            ylab("Baby Weight (lb)") +
            ggtitle(paste(" Weight of Baby" , input$which.baby ,"\n February and March", sep = " ")) + 
            geom_point(color = "orchid4") +
            geom_smooth(method = "lm", se=F, color = "orchid4") +
            theme_classic(base_size = 16)
        
    })
    
## Generate bar plot of total diapers used by type of diaper accident
    
    output$diaperPlot <- renderPlot({
        
      diaper.filter() %>% 
        ggplot(aes(x = Amount, y = n, fill = Amount)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = palette) +
        theme_classic(base_size = 16) +
        geom_image(aes(image = images), size = 0.1) +
        labs(x = "Diaper Condition",
             y = "Total Diaper Count",
             title = paste0(" Number of ",input$which.baby,"'s diapers dealt with \n (lovingly) in February and March, 2021"))
    
    })    

## Embed a photo of Blakely or Micah depending on side bar selection
    
    photo.path <- reactive({
        paste0(input$which.baby, ".png") # photo name in file path must match a which.baby character string 
    })
    
    output$image <- renderUI({
        img(src = photo.path(), width = 150, height = 300) # searches for file in www folder
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#rsconnect::deployApp(here::here("Week10","Scripts","Baby_Monitoring"))

