library(shiny)
library(ggplot2)

library(tidyverse)
historical_spending <- read.csv("historical_spending.csv")
total_spending <-read.csv("total_spending_billions.csv")

# Define UI for dataset viewer app ----
ui <- fluidPage(

  
  # App title ----
  titlePanel("Overview on Valentines Day Spending"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      img(src ="nrf-logo-working.png", height = 140, width = 200),
      hr(),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "SpendingTrends",
                  label = "Dataset",
                  choices = c("Total Spenditure"= "total_spending",
                              "Spenditure Per Person" = "historical_spending"),
                  selected = "total_spending"),
      
      h6("Data is from National Retail Foundation, Extracted by Suraj Das on Kaggle")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("lineplot")
      
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  output$lineplot <- renderPlot({
    selected_dataset <- switch(input$SpendingTrends,
                               "total_spending" = total_spending,
                               "historical_spending" = historical_spending)
    
    if (input$SpendingTrends == "total_spending" ) {
      # Plot for total_spending dataset
      ggplot(total_spending,aes(x=Year,y=Total.Expected.Valentine.s.Day.Spending..in.billions.))+
        geom_line(colour="red") +
        geom_text(aes(label=Total.Expected.Valentine.s.Day.Spending..in.billions.), vjust=2.0, color="black", size=3.5) +
        geom_point(colour="red")+ 
        labs(x="Year",
             y="Total Spent in Billions($B)",
             title="Total Spenditure Across the Years ")
    } else if (input$SpendingTrends == "historical_spending") {
      # Plot for historical_spending dataset
      ggplot(historical_spending, aes(x = Year,y=PerPerson)) + geom_area(fill="pink")+
        geom_line(colour="red") + geom_point(shape = 18 ,colour="red",size=3) + 
        geom_text(aes(label=PerPerson), vjust=2.0, color="black", size=3.5) +
        labs(x = "Year",
             y = "Spending Per Person on Valentines Day ($) ",
             title = "Valentines spending across the Years")
    } 
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)