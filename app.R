#install.packages(c("shiny", "tidyverse", "wordcloud", "ggplot2", "shinythemes", "RColorBrewer"))
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(shiny)
library(ggplot2)
library(shinythemes)
library(tidytext)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")
# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE){
  if(!(book %in% books))
    stop("Unknown book")
  
  text <- tibble(text=readLines(sprintf("./data/%s.txt", book), 
                                encoding = "UTF-8"))
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}

# task6: add in shinythemes function
theme = shinytheme("cerulean")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    
    # task2: add in the inputs in the sidebarPanel
    sidebarPanel(
      selectInput("selection", label = "Choose a book:", choices = books),
      checkboxInput("stopwords", "Stop Words:", value = TRUE),
      actionButton("go", "Update"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput("numwords", "Max number of words:", min = 10, max = 200, value = 100, step = 1),
      sliderInput("maxwords", "Max. size of words:",  min = 1, max = 8, value = 4),
      sliderInput("minwords", "Min. size of words:",  min = 0.1, max = 4, value = 0.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput("mincount", "Minimum words for Counts chart:",  min = 10, max = 100, value = 25),
      sliderInput("wordsize", "Word size for Counts chart:",  min = 8, max = 30, value = 14),
    ),
    
    # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("cloud", height = "600px")),
        tabPanel("Word Counts", plotOutput("freq", height = "600px"))
      )
    ))
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$go, {
    
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$selection, input$stopwords)
    })
})
  
  output$cloud <- renderPlot({
    v<- freq()
    pal<- brewer.pal(8,"Dark2")
    v%>%
      with(
        wordcloud(
          word,
          n,
          scale = c(input$maxwords, input$minwords),
          random.order = FALSE,
          max.words = input$numwords,
          colors = pal))
  })
  
  output$freq <- renderPlot({
    v <- freq()
    v%>%
      filter(n>input$mincount) %>%
      ggplot(aes(x=reorder(word, n), y=n)) +
      geom_col() +
      coord_flip() +
      theme(text = element_text(size=input$wordsize)) +
      labs(x="", y="", title="")
  })
}
shinyApp(ui = ui, server = server)
