library(shiny)
library(tidyverse)
library(wordcloud)
library(shinythemes)
library(RColorBrewer)
library(shinyWidgets)
library(tidytext)

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")


# task4: add in getFreq function for pre-processing


getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words) 
  }
  
  return(text)
}
# stopwords removed data set
merchant_book <- getFreq("merchant") %>% 
  mutate(books = "merchant")
romeo_book <- getFreq("romeo") %>% 
  mutate(books = "romeo")
summer_book <- getFreq("summer") %>% 
  mutate(books = "summer")
book_data <- bind_rows(merchant_book,romeo_book,summer_book)

# stopwords not removed data set
merchant_book_stopword <- getFreq("merchant", stopwords = FALSE) %>% 
  mutate(books = "merchant")
romeo_book_stopword <- getFreq("romeo", stopwords = FALSE) %>% 
  mutate(books = "romeo")
summer_book_stopword <- getFreq("summer", stopwords = FALSE) %>% 
  mutate(books = "summer")
book_data_stopword <- bind_rows(merchant_book_stopword,romeo_book_stopword,summer_book_stopword) 







# task6: add in shinythemes function

ui <- fluidPage(
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
   
    
  
  # task2: add in the inputs in the sidebarPanel
    sidebarPanel = sidebarPanel(
      
      # Drop down for book selection
      pickerInput(
        inputId = "books",
        label   = "Choose a Book:", 
        choices = c("A Mid Summer Night's Dream" = "summer",
                    "The Merchant of Venice"     = "merchant",
                    "Romeo and Juliet"           = "romeo")),
      
      br(),
      # Action button for stop words removed or not
      checkboxInput(
        inputId = "stopwords",
        label = "Stop Words:",
        value   = TRUE),
      #verbatimTextOutput(outputId = "stops"),
      
      
      # play buttom
      actionButton(
        inputId = "apply", 
        label = "Apply", 
        icon = icon("play")),
      
      
      #verbatimTextOutput(outputId = "book_selection"),
      
  
      
      hr(), 
      
      h3("Word Cloud Settings"),
      
      # Slide bar naximum number of words 
      sliderInput(
        inputId = "maxwords",
        label   = "Max # of Words:",
        min     = 10,
        max     = 200,
        value   = 100,
        step    = 10),
      
      sliderInput(
        inputId = "largewords",
        label   = "Size of Largest Words:",
        min     = 0.1,
        max     = 8,
        value   = 4),
      
      sliderInput(
        inputId = "smallwords",
        label   = "Size of Smallest Words:",
        min     = 0.1,
        max     = 4,
        value   = 0.5),
      
      hr(),
      
      h3("Word Count Settings"),
      
      
      
      
      sliderInput(
        inputId = "minwordcount",
        label   = "Minimum Words For Counts Chart:",
        min     = 10,
        max     = 100,
        value   = 25),
     
      
      sliderInput(
        inputId = "fontizecount",
        label   = "Font Size For Counts Chart:",
        min     = 8,
        max     = 30,
        value   = 14),
      
      ),
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    mainPanel = mainPanel(
      
      tabsetPanel(type = "tab",
                  
        tabPanel("Word Cloud",plotOutput("cloud",
                                         height = "750px",
                                         width = "100%"
                                         )),
        
        
        
        tabPanel("Word Count",plotOutput("freq",
                                         height = "800px"))
        ),
      
        
        #verbatimTextOutput(outputId = "count_data"),
        #verbatimTextOutput(outputId = "cloud_data"),


     
  
  # task6: and modify your figure heights
    )
  )
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  
  # Getting Book Authors ----
  book_names <- eventReactive(input$apply,{
      input$books
    },
    ignoreNULL = FALSE
 )
  #output$book_selection <- renderPrint(book_names())
  
  
  
  # Get Word Count Data ----
  word_count_data <- reactive({
    if(input$stopwords){
      book_data %>% 
        filter(books == book_names()) %>% 
        arrange(desc(n)) %>%
        mutate(word = word %>% as_factor() %>% fct_reorder(n)) %>% 
        slice(1:input$minwordcount)
    }else{
      
      book_data_stopword %>% 
        filter(books == book_names()) %>% 
        arrange(desc(n)) %>%
        mutate(word = word %>% as_factor() %>% fct_reorder(n)) %>% 
        slice(1:input$minwordcount)
    }
     
  })
  #output$count_data <- renderPrint(word_count_data ())

  
  
  
  
  # Plots Word Count-----
  
  output$freq <- renderPlot({
    
      word_count_data() %>%
        ggplot(aes(n,word)) +
        geom_col(color = "white", fill = "cornflowerblue") +
        labs(
          title = "Word Count Bar Chart",
          x     = "Total Number of Words",
          y     = "Words"
        ) +
        theme(
          text = element_text(size = input$fontizecount)
        )
  })
  
  
  
  
  
  stopwords <- reactive({
  
    input$stopwords 
  })
  #output$stops <- renderPrint(stopwords())
  
  # Get Word Cloud Data ----
  word_cloud_data <- reactive({
    if (input$stopwords) {
      pal <- brewer.pal(8,"Dark2")
      
      book_data %>% 
        filter(books == book_names()) 
  
    }else{
      pal <- brewer.pal(8,"Dark2")
      book_data_stopword %>% 
        filter(books == book_names()) 
    }
    
  })
  
  
  output$cloud <- renderPlot({
    pal <- brewer.pal(8, "Dark2")
    word_cloud_data() %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$largewords, input$smallwords),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
   
  })
  
  
  #output$cloud_data <- renderPrint(word_cloud_data())
  
  
  
  
}



shinyApp(ui = ui, server = server)


