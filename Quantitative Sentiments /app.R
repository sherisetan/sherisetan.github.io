library(shiny)
library(tidyverse)
library(ggplot2)
library(billboarder)
library(RColorBrewer)

gifts_gender <- read.csv("gifts_gender.csv")
historical_spending <- read.csv("historical_spending_copy1 copy.csv")
gifts_age <- read.csv("gifts_age.csv")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Quantitative Sentiments"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(column(12, billboarderOutput("Percent_Celebrating")),
               column(12, billboarderOutput("Spending_by_Age")),
               column(12, billboarderOutput("Planned_Gifts_By_Gender_chart")))
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  
    output$Planned_Gifts_By_Gender_chart <- renderBillboarder({
      # Plot for gifts_gender dataset
      new_gifts_gender <- gifts_gender %>% 
        select(Gender,Candy,Flowers,Jewelry,GreetingCards,EveningOut,Clothing,GiftCards)
      billboarder() %>% bb_barchart(data = new_gifts_gender) %>% 
        bb_color(palette = brewer.pal(n = 5, name = "RdPu")) %>% 
        bb_legend(position = 'right') %>% bb_title("Planned Gifts By Gender") %>%
  bb_labs(x ="Gender", y ="Percentage(%)") %>% bb_point(show(TRUE)) })
      
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