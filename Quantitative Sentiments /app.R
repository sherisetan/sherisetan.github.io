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
    sidebarPanel(
      h4("Data Analysis"),
      h6("Percentage of People Celebrating Valentines Day Over the Years"),
      p("However, unlike the spending trends, the percentage of people celebrating Valentines Day has been on a general decline from 60% in 2010 to 53% in 2022.
        This may be aligned with recent sentiments that Valentines Day is becoming increasingly commercialised and couples feel less inclined to follow marketing gimmicks of various corporations that dictate how one should celebrate Valentines Day. "),
      hr(),
      h6("Percentage of People Celebrating Valentines Day by Age in 2022"),
      p("From the barchart, it is evident that the Valentines Day is the most popular with people between the ages of 18-24 at 51%, before it starts on a steady decline from ages 25-34 all the way to 65+ years old.
        It is evident that the Celebration of Valentines Day is the most popular amongst Gen Zs, who are between 18-25 currently.
        However, instead of an increased emphasis on romantic relationships, this popularity of celebrating Valentines Day could be due to the increased emphasis on celebrating platonic friendships or celebrating self love(Singh, 2024)"),
      hr(),
      h6("Planned Gifts By Gender"),
      p("The most popular gift choice across the genders is candy at 52% for men and 59% for women.Besides this,men are more likely to give their partners flowers(56%),greeting cards(37%),evening outs(33%), 
        whereas women are more likely to give their partners greetingcards (43%),evening outs (29%) or giftcards & clothing at 24%. 
        Most notable is the disparity between the choices of gifts chosen amongst the genders. Men are more likely to choose a wider selection of gifts, whereas women tend to gravitate towards a few types of gifts.
        Hence, it seems to suggest that men take on the heavier burden of procuring the gitfs for Valentines  ")
    ),
    
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