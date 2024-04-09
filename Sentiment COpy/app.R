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
    h4("Data Insights"),
    hr(),
    h4("Sentiments towards the point of Valentines Day across Genders"),
    p("The top three reasons women feel that Valentines Day is important are to 'Show How Much You Care' (51%),'To Treat Yourself' (14%),'To buy someone gifts' (12%).
    On the contrary, the top three reasons men feel that Valentines Day is important is to 'Show How much you care' (29%), 'Sex' (25%), 'To Go Out for Dinner'(18%).
    This disaprity shows that men and women have different view towards the significance of Valentines Day. Hence, generisations on how one should celebrate Valentines Day cannot be made across men and women.
      "),
    h4("Reasons for marking Valentines Day and not celebrating it across Genders"),
    p("Out of population of men and women that said they will not be celebrating Valentines Day, majority of them stated using Valentines Day as a reason to treat oneself (Men (11%), Women (16%)) and plan a get-together (Men (10%), Women (8%)).
      This shows that not everyone is treating Valentines Day strictly as a means of celebrating romantic relationships.")
      
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
      bb_color(palette = brewer.pal(n = 5, name = "Reds")) %>%
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
        bb_title("Plans for those marking occasion despite not celebrating") %>% 
        bb_labs(x = "Gender", y = "Percentage(%)")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

