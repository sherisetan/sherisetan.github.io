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
                  choices = c("Total Expenditure"= "total_spending",
                              "Spending Per Person" = "historical_spending"),
                  selected = "total_spending"),
      
      h6("Data is from National Retail Foundation, Extracted by Suraj Das on Kaggle"),
      
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
        geom_point(colour="red", shape = 25,fill="red")+ 
        labs(x="Year",
             y="Total Spent in Billions($B)",
             title="Total Expenditure Across the Years ",
             subtitle = 
            "The total expenditure on Valentines Day has been steadily increasing from $14.7 Billion in 2010 to $23.9 Billion in 2022. 
Remarkably, there is an obvious peak in Valentines Day expenditure in 2020 at $27.4 Billion. Rather than being 
attributed to a specific reason, Valentines Day spending trajectory was anctipated to increase. 
However, due to onset of the pandemic in 2020,it severly disrupted global economic activity, causing job losses 
(SPOR, n.d.).Hence, this resulted in a significant drop in Valentines Day spenditure from 2020 to 2021.We can see 
this increasing expenditure trend pick up from 2021 to 2022, albeit much slower due to the recovering economy  ")
    } else if (input$SpendingTrends == "historical_spending") {
      # Plot for historical_spending dataset
      ggplot(historical_spending, aes(x = Year,y= PerPerson)) + geom_area(fill="pink")+
        geom_line(colour="red") + geom_point(shape = 18 ,colour="red",size=3) + 
        geom_text(aes(label= PerPerson), vjust=2.0, color="black", size=3.5) +
        labs(x = "Year",
             y = "Spending Per Person on Valentines Day ($) ",
             title = "Valentines Day spending across the Years",
             subtitle = 
             "Expenditure per person also saw a steady increase from $103 in 2010 to $175.40 in 2022. 
Similar to the spending trends for total expenditure, there was a notable spike in 2020 which could be attributed to
the onset of pandemic in 2020, causing reduced consumption towards Valentines day from 2021 to 2022 as 
(SPOR, n.d.).It can be reasoned that this is because Valentines Day is not seen as a necessity in a 
climate of financial instability and uncertainty.")
    } 
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)