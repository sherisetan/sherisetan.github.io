library(shiny)
library(billboarder)
library(tidyverse)
library(RColorBrewer)

# Read data
gifts_gender <- read.csv("gifts_gender.csv")
POValentines <- read.csv("point_of_valentines.csv")

# UI
ui <- fluidPage(
  titlePanel("Sentiments towards Valentines Day"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("show_gifts_gender", "Show Planned Gifts by Gender", value = FALSE)
    ),
    mainPanel(
      fluidRow(
        column(6, billboarderOutput("male_pie_chart")),
        column(6, billboarderOutput("female_pie_chart")),
        column(12, billboarderOutput("gifts_gender_chart"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Render male pie chart
  output$male_pie_chart <- renderBillboarder({
    male_data <- POValentines %>% 
      select(Point, Men)
    billboarder() %>%
      bb_piechart(data = male_data, label = "Point", values = "Men") %>%
      bb_color(palette = brewer.pal(n = 5, name = "Blues")) %>%
      bb_legend(position = 'right') %>%
      bb_title("Men")
  })
  
  # Render female pie chart
  output$female_pie_chart <- renderBillboarder({
    female_data <- POValentines %>% 
      select(Point, Women)
    billboarder() %>%
      bb_piechart(data = female_data, label = "Point", values = "Women") %>%
      bb_color(palette = brewer.pal(n = 5, name = "Oranges")) %>%
      bb_legend(position = 'right') %>%
      bb_title("Women")
  })
  
  # Render gifts by gender chart if checkbox is checked
  output$gifts_gender_chart <- renderBillboarder({
    if (input$show_gifts_gender) {
      new_gifts_gender <- gifts_gender %>% 
        select(Gender, Candy, Flowers, Jewelry, GreetingCards, EveningOut, Clothing, GiftCards)
      billboarder() %>% 
        bb_barchart(data = new_gifts_gender) %>% 
        bb_color(palette = brewer.pal(n = 5, name = "RdPu")) %>% 
        bb_legend(position = 'right') %>% 
        bb_title("Planned Gifts By Gender") %>% 
        bb_labs(x ="Gender", y ="Percentage(%)")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

