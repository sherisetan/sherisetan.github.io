library(tm)
library(RColorBrewer)
library(wordcloud2)

palette <- brewer.pal(8, "Spectral")
# Define UI
ui <- fluidPage(
  titlePanel("Valentine's Day Word Clouds"),
  
  # Content submission form
  sidebarPanel(
    textInput("story", "Share your Valentine's Day story"),
    actionButton("submit", "Submit")
  ),
  
  # Display word clouds
  mainPanel(
    h3("Word Cloud from User Submissions"),
    wordcloud2Output("wordcloudPlot")
  )
)

# Define server logic
# Define server logic
server <- function(input, output) {
  # Initialize reactive value for storing submissions
  userSubmissions <- reactiveValues(submissions = character(0))
  
  # Handle content submissions
  observeEvent(input$submit, {
    if (!is.null(input$story) && input$story != "") {
      userSubmissions$submissions <- c(userSubmissions$submissions, input$story)
    }
  }) 
  
  # Generate word cloud from user submissions
  output$wordcloudPlot <- renderWordcloud2({
    # Print contents of userSubmissions$submissions for debugging
    print(userSubmissions$submissions)
    
    # Preprocess user submissions
    corpus <- Corpus(VectorSource(userSubmissions$submissions))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    # Check if corpus is empty
    if (length(corpus) == 0) {
      return(NULL)
    }
    
    # Create document term matrix
    dtm <- DocumentTermMatrix(corpus)
    
    # Check if document term matrix is empty
    if (length(dtm) == 0) {
      return(NULL)
    }
    
    freq <- colSums(as.matrix(dtm))
    
    # Generate word cloud
    word_freq_df <- data.frame(word = names(freq), freq = freq)
    wordcloud2(word_freq_df,color = palette)
  })
}

shinyApp(ui = ui, server = server)
