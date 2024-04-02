library(shiny)
library(ggplot2)
library(tidyverse)
library(billboarder)
library(RColorBrewer)
gifts_gender <- read.csv("gifts_gender.csv")
historical_spending <- read.csv("historical_spending_copy1 copy.csv")
gifts_age <- read.csv("gifts_age.csv")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Overview on Valentines Day Spending"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "QuantitativeSentiments",
                  label = "Dataset",
                  choices = c("Planned Gifts Per Gender"= "gifts_gender",
                              "Percentage of People Celebrating" = "historical_spending",
                              "Percentage of people spending Across Ages" = "gifts_age"),
                  selected = "historical_spending"),
      
      h6("Data is from National Retail Foundation, Extracted by Suraj Das on Kaggle")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("visualgraphs")
      
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  output$visualgraphs <- renderPlot({
    selected_dataset <- switch(input$QuantitativeSentiments,
                               "gifts_gender" = gifts_gender,
                               "historical_spending" = historical_spending,
                               "gifts_age" = gifts_age)
    
    
    if (input$QuantitativeSentiments == "gifts_gender" ) {
      # Plot for gifts_gender dataset
      new_gifts_gender <- gifts_gender %>% 
        select(Gender,Candy,Flowers,Jewelry,GreetingCards,EveningOut,Clothing,GiftCards)
      billboarder() %>% bb_barchart(data = new_gifts_gender) %>% 
        bb_color(palette = brewer.pal(n = 5, name = "RdPu")) %>% 
        bb_legend(position = 'right') %>% bb_title("Planned Gifts By Gender") %>%
  bb_labs(x ="Gender", y ="Percentage(%)")
    } else if (input$QuantitativeSentiments == "historical_spending") {
      # Plot for historical_spending dataset
      new_hs <- historical_spending %>%
        select(PercentCelebrating)
      billboarder() %>% bb_linechart(new_hs) %>% 
        bb_color(palette = brewer.pal(n = 5, name = "Spectral"))  %>% 
      bb_legend(position = 'right')
    } else if(input$QuantitativeSentiments == "gifts_age") {
      # Plot for gifts_age dataset
      billboarder() %>% bb_barchart(data = gifts_age ) %>% bb_color("RdPu")
    }
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)