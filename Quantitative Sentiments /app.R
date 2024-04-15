library(shiny)
library(tidyverse)
library(ggplot2)
library(billboarder)
library(RColorBrewer)

historical_spending <- read.csv("historical_spending_copy1 copy.csv")
gifts_age <- read.csv("gifts_age.csv")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Quantitative Sentiments"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h4("Data Analysis"),
      h6("Percentage of People Celebrating Valentines Day Over the Years"),
      p("Unlike the spending trends, the percentage of people celebrating Valentines Day has been on a general decline from 60% in 2010 to 53% in 2022.
        This may be aligned with recent sentiments that Valentines Day is becoming increasingly commercialised by big corporations, causing the hikes in prices for Valentines Day related goods and 
        marketing the need to buy these gifts for the holiday, which may discourage people from celebrating it (Ferguson, 2022). "),
      hr(),
      h6("Percentage of People Celebrating Valentines Day by Age in 2022"),
      p("From the barchart, it is evident that the Valentines Day is the most popular with people between the ages of 18-24 at 51%, before it starts on a steady decline from ages 25-34 all the way to 65+ years old.
        It is evident that the Celebration of Valentines Day is the most popular amongst Gen Zs, who are between 18-25 currently.
        However, instead of an increased emphasis on romantic relationships, this popularity of celebrating Valentines Day amongst Gen Zs could be due to the increased emphasis on celebrating platonic friendships or celebrating self love(Singh, 2024)"),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(column(12, billboarderOutput("Percent_Celebrating")),
               column(12, billboarderOutput("Spending_by_Age")))
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  
    
      
    output$Percent_Celebrating<- renderBillboarder({
      # Plot for historical_spending dataset
      new_hs <- historical_spending %>%
        select(Year,PercentCelebrating)
      billboarder() %>% bb_linechart(data=new_hs, show_point = TRUE) %>% 
        bb_color(palette = brewer.pal(n = 5, name = "Spectral"))  %>% 
        bb_legend(position = 'right') %>% bb_title("Percentage of People Celebrating Valentines Day Over the Years") %>%
        bb_labs(x ="Years", y ="Percentage(%)") %>% bb_point(shape=25,r=3) })
      
    output$Spending_by_Age<- renderBillboarder({
      # Plot for gifts_age dataset
      new_ga <- gifts_age %>%
        select(Age,SpendingCelebrating)
      billboarder() %>% bb_barchart(data = new_ga ) %>% bb_title("Percentage of People Celebrating Valentines Day by Age in 2022") %>%
        bb_labs(x ="Years", y ="Percentage(%)") %>%
        bb_color(palette = brewer.pal(n = 5, name = "Spectral"))
    })
  }



# Create Shiny app ----
shinyApp(ui = ui, server = server)