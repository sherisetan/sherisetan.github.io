library(shiny)
library(billboarder)
library(tidyverse)
library(RColorBrewer)

# Read data
noValentines<- read.csv("Xcelebratingbutstillmarking(G).csv")
POValentines <- read.csv("point_of_valentines.csv")

# UI
ui <- fluidPage(
  titlePanel("Sentiments towards Valentines Day"),
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      fluidRow(
        column(6, billboarderOutput("male_pie_chart")),
        column(6, billboarderOutput("female_pie_chart")),
        column(12, billboarderOutput("noValentines_chart"))
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
      bb_title("Men")
  })
  
  # Render female pie chart
  output$female_pie_chart <- renderBillboarder({
    female_data <- POValentines %>% 
      select(Point, Women)
    billboarder() %>%
      bb_piechart(data = female_data, label = "Point", values = "Women") %>%
      bb_color(palette = brewer.pal(n = 5, name = "Oranges")) %>%
      bb_title("Women")
  })
  
  # Render gifts by gender chart if checkbox is checked
  output$noValentines_chart <- renderBillboarder({
      new_no_valentines <- noValentines %>% 
        slice(1:3) %>%
        rename(Reasons= Percent.of.those.not.celebrating.Valentine.s.Day.still.marking.the.occasion) 
      billboarder() %>% bb_barchart(data = new_no_valentines, stacked = T,position = "fill") %>% 
        bb_color(palette = brewer.pal(n = 5, name = "RdPu")) %>% 
        bb_legend(position = 'right') %>% 
        bb_title("Reasons for marking occasion despite not celebrating") %>% 
        bb_labs(x = "Gender", y = "Percentage(%)")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

