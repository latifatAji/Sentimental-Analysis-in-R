## Sentiment Analysis
rm(list = ls())
require(tidyverse)
require(tidytext)
require(SnowballC)
require(reshape2)
require(gridExtra)
require(wordcloud)
setwd("/Users/lateefah/Downloads/")

hotel_data <- read_csv('tourist_accommodation_reviews.csv')
hotel_data %>% str()
## Rename Hotel name for ease
df <- 
  hotel_data %>% rename(name = `Hotel/Restaurant name`)
df %>%  head()
df %>% distinct(name)
df %>% distinct(name) %>% n_distinct()
selected_hotels <-
  df %>% 
  distinct(name) %>% 
  slice(10:39)
hotel_df <-
  df %>% filter(name %in% pull(selected_hotels))
hotel_df %>% head()
hotel_df %>% distinct(name) %>% n_distinct()  ## Affirm hotel numbers
### Reviews per hotel

hotel_df <-
  hotel_df %>%  select(-`Review Date`, -Location)
hotel_df %>%
  group_by(name) %>%
  summarize(messages = n_distinct(ID)) %>%
  ggplot(aes(x = reorder(name, messages), y = messages, fill = name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  xlab("Hote/Restaurant Name") +
  ylab("Frequency of reviews") + theme_classic()
### Text Processing
hotel_df$Review <- iconv(hotel_df$Review, "UTF-8", "UTF-8", "byte")
hotel_df$Review <- tolower(hotel_df$Review) ## Convert to Lower case
hotel_df$Review <- gsub("[[:punct:]]", "", hotel_df$Review) ## Removing Punctuation
hotel_df$Review <- gsub("[[:digit:]]", "", hotel_df$Review)  ## Remove digits
hotel_df$Review <- gsub('http[[:alnum:]]*', " ", hotel_df$Review) ## Remove URL
hotel_df$Review <- gsub("\\s+", " ", str_trim(hotel_df$Review)) ## Remove excess white spaces
hotel_df$Review[1]
## Removing stowprds
## Stop-words removal and Stemming (Not necessary in all cases)
clean_review <-
  hotel_df %>% select(-ID) %>% 
  unnest_tokens(word, Review) %>% 
  anti_join(stop_words) %>% 
  mutate(stemmed_word = wordStem(word))
clean_review %>% head(10)
## Exporting data for sas-em analysis (sample) we did for 1, 2, ..., 30
hot1 <- hotel_df %>% filter(name == pull(selected_hotels)[1]); write.csv(hot1, "hotel1.csv", row.names = F)
## Common words per hotel
information <- function(hotel_name, no){
  print(paste("Hotel name:", pull(hotel_name)[no]))
  clean_review %>% filter(name == pull(hotel_name)[no]) %>% 
    count(name, word, sort = TRUE) %>% 
    ungroup() %>% select(-name)
}

information(selected_hotels, 1)
information(selected_hotels, 2)
information(selected_hotels, 3)
information(selected_hotels, 4)
information(selected_hotels, 5)
## We notice that (food, restaurant) were so much in use. We will then drop them from our analysis
clean_review <-
  clean_review %>% filter(!word %in% c('food', 'restaurant', 'hotel'))
information(selected_hotels, 1) ## Looking better
## Sentiment Scores

clean_review <-
  clean_review %>% 
  count(name, word, sort = TRUE) %>% 
  ungroup()

hotels_sentiment <- clean_review %>% 
  inner_join(get_sentiments("afinn"), by = 'word') %>% 
  group_by(name) %>% 
  summarise(value = sum(value * n) / sum(n))

hotels_sentiment %>% 
  mutate(name = reorder(name, value)) %>% 
  ggplot(aes(name, value, fill = name, value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  xlab("Hotel/Restaurant Name") +
  ylab("Average Sentiment Score") + theme_bw()


contributions <- clean_review %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(value))


information <- function(hotel_name, no){
  print(paste("Hotel name:", pull(hotel_name)[no]))
  clean_review %>% filter(name == pull(hotel_name)[no]) %>% 
    count(name, word, sort = TRUE) %>% 
    ungroup() %>% select(-name)
}
contributions %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + theme_bw()

chatcount <- clean_review %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word~sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("#D55E00", "#009E73"),
                   max.words = 100)

sentiment_by_hotel <- function(hotel_name, no){
  par(mfrow = c(1,2))
  set.seed(1234)
  print(paste("Hotel name:", pull(hotel_name)[no]))
  
  hotel <- 
    clean_review %>% filter(name == pull(hotel_name)[no])
  wordcloud(words = hotel$word, freq = hotel$n, random.order=FALSE, 
            rot.per=0.35, colors=brewer.pal(8, "Dark2"), min.freq = 5, max.words = 100)
  
  df <-
    hotel %>%
    inner_join(get_sentiments("bing"), by = "word") %>% select(sentiment) %>% 
    table() %>% data.frame() %>% rename(Count = Freq,
                                        Sentiment = ".") %>% mutate(Proportion = Count/sum(Count)) %>% 
    mutate_at(vars(Proportion), funs(round(.,3)))
  barplot(df$Proportion, names.arg = df$Sentiment, 
          main = "Sentiment Distribution", xlab = "Sentiment type", 
          ylab = 'Proportion', col = 'cyan', lwd = 7)
  
  return(df) 
}
sentiment_by_hotel(selected_hotels, 1)
sentiment_by_hotel(selected_hotels, 2)
sentiment_by_hotel(selected_hotels, 3)
sentiment_by_hotel(selected_hotels, 4)
sentiment_by_hotel(selected_hotels, 5)
sentiment_by_hotel(selected_hotels, 6)
sentiment_by_hotel(selected_hotels, 7)
sentiment_by_hotel(selected_hotels, 8)
sentiment_by_hotel(selected_hotels, 9)
sentiment_by_hotel(selected_hotels, 10)
sentiment_by_hotel(selected_hotels, 11)
sentiment_by_hotel(selected_hotels, 12)
sentiment_by_hotel(selected_hotels, 13)
sentiment_by_hotel(selected_hotels, 14)
sentiment_by_hotel(selected_hotels, 15)
sentiment_by_hotel(selected_hotels, 16)
sentiment_by_hotel(selected_hotels, 17)
sentiment_by_hotel(selected_hotels, 18)
sentiment_by_hotel(selected_hotels, 19)
sentiment_by_hotel(selected_hotels, 20)
sentiment_by_hotel(selected_hotels, 21)
sentiment_by_hotel(selected_hotels, 22)
sentiment_by_hotel(selected_hotels, 23)
sentiment_by_hotel(selected_hotels, 24)
sentiment_by_hotel(selected_hotels, 25)
sentiment_by_hotel(selected_hotels, 26)
sentiment_by_hotel(selected_hotels, 27)
sentiment_by_hotel(selected_hotels, 28)
sentiment_by_hotel(selected_hotels, 29)
sentiment_by_hotel(selected_hotels, 30)

### Bi-gram Analysis
bigram_ <-
  hotel_df %>% select(-ID) %>% 
  unnest_tokens(bigram, Review, token = "ngrams", n = 2) 

bigram_count <- 
  bigram_ %>%
  count(name, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")

negation_words <- c("no", "not", "cant", 'dont')
bigram_count %>% 
  filter(word1 %in% negation_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ungroup() %>%
  mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by a negation") +
  ylab("Sentiment score * # of occurrences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() + theme_bw()
## Shiny Dashboard
require(tidyverse)
require(tidytext)
require(SnowballC)
require(reshape2)
require(gridExtra)
require(wordcloud)
require(wordcloud2)

hotel_data <- read_csv(('tourist_accommodation_reviews.csv'))

hotel_data %>% head()

hotel_data %>% str()

## Rename Hotel name for ease

df <- 
  hotel_data %>% rename(name = `Hotel/Restaurant name`)

df %>%  head()

df %>% distinct(name)

df %>% distinct(name) %>% n_distinct()


selected_hotels <-
  df %>% 
  distinct(name) %>% 
  slice(10:39)

hotel_df <-
  df %>% filter(name %in% pull(selected_hotels))

hotel_df <-
  hotel_df %>%  select(-`Review Date`, -Location)

### Text Processing

hotel_df$Review <- iconv(hotel_df$Review, "UTF-8", "UTF-8", "byte")
hotel_df$Review <- tolower(hotel_df$Review) ## Convert to Lower case
hotel_df$Review <- gsub("[[:punct:]]", "", hotel_df$Review) ## Removing Punctuation
hotel_df$Review <- gsub("[[:digit:]]", "", hotel_df$Review)  ## Remove digits
hotel_df$Review <- gsub('http[[:alnum:]]*', " ", hotel_df$Review) ## Remove URL
hotel_df$Review <- gsub("\\s+", " ", str_trim(hotel_df$Review)) ## Remove excess white spaces


## Stop-words removal and Stemming (Not necessary in all cases)

clean_review <-
  hotel_df %>% select(-ID) %>% 
  unnest_tokens(word, Review) %>% 
  anti_join(stop_words) %>% 
  mutate(Stemmed = wordStem(word))

clean_review <- 
  clean_review %>% rename(Unstemmed = word)

clean_review <-
  clean_review %>% filter(!Unstemmed %in% c('food', 'restaurant', 'hotel'))

bigram_ <-
  hotel_df %>% select(-ID) %>% 
  unnest_tokens(bigram, Review, token = "ngrams", n = 2)

bigram_count <- 
  bigram_ %>%
  count(name, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")


negation_words <- c("no", "not", "cant", 'dont')


ui <- dashboardPage(
  dashboardHeader(title = "Sentiment Analysis", dropdownMenu(type = "messages",
                                                             messageItem(
                                                               from = "Latifat Ajibola",
                                                               message = "Only 30 Hotels are present here",
                                                               time = "today"
                                                             )), dropdownMenu(type = "notifications",
                                                                              notificationItem(
                                                                                text = "Hotels were randomly selected",
                                                                                icon("hotel")
                                                                              ),
                                                                              notificationItem(
                                                                                text = "Second tab contains overall analysis",
                                                                                icon("hotel"),
                                                                                status = "success"
                                                                              ))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Hotel Sentiment", tabName = 'perhotel', icon = icon('hotel')),
      menuItem('All Hotels', icon = icon('th'), tabName = 'overall', badgeLabel = "optional", badgeColor = "green")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "perhotel",
              fluidRow(
                box(
                  title = "Select Hotel", status = "primary",
                  solidHeader = T, collapsible = TRUE,
                  selectInput('hotelnames', 'Choose hotel to analyze', pull(selected_hotels)), 
                  width = 2
                ),
                box(
                  title = "Analysis Type", status = "primary",
                  solidHeader = T, collapsible = TRUE,
                  selectInput('stemming', 'Choose analysis type', c("Unstemmed", "Stemmed")), 
                  width = 2
                ),
                box(
                  title = "Max words on cloud (100 is default)", status = "primary",
                  solidHeader = T, collapsible = TRUE,
                  sliderInput("slider", "max.words:", 50, 500, 100),
                  width = 3
                ),
                infoBoxOutput("progressbox0", width = 2),
                infoBoxOutput("progressbox1", width = 2),
                infoBoxOutput("progressbox2", width = 2),
                infoBoxOutput("progressbox3", width = 2)
              ),
              fluidRow(
                box(
                  title = "Wordcloud", status = "primary",
                  solidHeader = T, collapsible = TRUE, 
                  plotOutput("wdcl", height = 650), width = 6
                ),
                box(
                  title = "Sentiment", status = "primary",
                  solidHeader = T, collapsible = TRUE, 
                  plotOutput("stp", height = 650)
                )
              )
              
      ),
      
      ## Second Tab
      tabItem(tabName = 'overall',
              fluidRow(
                box(
                  title = "Overall Wordcloud (Unstemmed Words Only)", status = "primary",
                  solidHeader = T, collapsible = TRUE,
                  plotOutput("ovwc", height = 850), width = 6
                ),
                box(
                  title = "Bigram Analysis (Unstemmed Words Only)", status = "primary",
                  solidHeader = T, collapsible = TRUE,
                  plotOutput("biga", height = 850), width = 6
                )
              )
      )
    )
  )
)

server <- function(input, output) { 
  
  output$progressbox0 <- renderInfoBox({
    
    site <- 
      hotel_data %>% filter(`Hotel/Restaurant name` == !!input$hotelnames) 
    st <- site$Location[1]
    
    infoBox(
      "Hotel Location", st, icon = icon("map"),
      color = 'purple', fill = T
    )
  })
  
  output$progressbox1 <- renderInfoBox({
    
    review_size <- 
      hotel_df %>% filter(name == !!input$hotelnames) %>% nrow()
    
    infoBox(
      "Number of Reviews", review_size, icon = icon("list"),
      color = 'purple', fill = T
    )
  })
  
  output$wdcl <- renderPlot({
    
    clean_review <-
      clean_review %>% 
      count(name, get(input$stemming), sort = TRUE) %>% 
      ungroup()
    
    hotel <- 
      clean_review %>% filter(name == !!input$hotelnames)
    
    colnames(hotel)[2] <- 'word'
    
    wordcloud(words = hotel$word, freq = hotel$n, random.order=FALSE, scale = c(7, 0.3),
              rot.per=0.5, colors=brewer.pal(8, "Dark2"), max.words = input$slider)
  })
  
  output$stp <- renderPlot({
    
    clean_review <-
      clean_review %>% 
      count(name, get(input$stemming), sort = TRUE) %>% 
      ungroup()
    
    hotel <- 
      clean_review %>% filter(name == !!input$hotelnames)
    
    colnames(hotel)[2] <- 'word'
    
    df <-
      hotel %>%
      inner_join(get_sentiments("bing"), by = "word") %>% select(sentiment) %>% 
      table() %>% data.frame() %>% rename(Count = Freq,
                                          Sentiment = ".") %>% mutate(Proportion = Count/sum(Count)) %>% 
      mutate_at(vars(Proportion), funs(round(.,3)))
    
    df %>%
      ggplot(aes(x = Sentiment, 
                 y = Proportion, fill = Sentiment)) +
      geom_bar(stat = "identity") +
      ylab("Proportion") + xlab("Sentiment Type") + theme_classic()
    
  })
  
  output$progressbox2 <- renderInfoBox({
    
    clean_review <-
      clean_review %>% 
      count(name, get(input$stemming), sort = TRUE) %>% 
      ungroup()
    
    hotel <- 
      clean_review %>% filter(name == !!input$hotelnames)
    
    colnames(hotel)[2] <- 'word'
    
    df <-
      hotel %>%
      inner_join(get_sentiments("bing"), by = "word") %>% select(sentiment) %>% 
      table() %>% data.frame() %>% rename(Count = Freq,
                                          Sentiment = ".") %>% mutate(Proportion = Count/sum(Count)) %>% 
      mutate_at(vars(Proportion), funs(round(.,3)))
    
    pos <- df$Proportion[2]
    
    infoBox(
      "Positive Percentage", paste0(pos, "%"), icon = icon("list"),
      color = 'purple', fill = T
    )
  })
  
  output$progressbox3 <- renderInfoBox({
    
    clean_review <-
      clean_review %>% 
      count(name, get(input$stemming), sort = TRUE) %>% 
      ungroup()
    
    hotel <- 
      clean_review %>% filter(name == !!input$hotelnames)
    
    colnames(hotel)[2] <- 'word'
    
    df <-
      hotel %>%
      inner_join(get_sentiments("bing"), by = "word") %>% select(sentiment) %>% 
      table() %>% data.frame() %>% rename(Count = Freq,
                                          Sentiment = ".") %>% mutate(Proportion = Count/sum(Count)) %>% 
      mutate_at(vars(Proportion), funs(round(.,3)))
    
    neg <- df$Proportion[1]
    
    infoBox(
      "Negative Percentage", paste0(neg, "%"), icon = icon("list"),
      color = 'purple', fill = T
    )
  })
  
  output$ovwc <- renderPlot({
    
    clean_review <-
      clean_review %>% 
      count(name, Unstemmed, sort = TRUE) %>% 
      ungroup()
    
    colnames(clean_review)[2] <- 'word'
    
    chatcount <- clean_review %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word~sentiment, value.var = "n", fill = 0) %>% 
      comparison.cloud(colors = c("#D55E00", "#009E73"),
                       max.words = input$slider)
  })
  
  output$biga <- renderPlot({
    bigram_count %>% 
      filter(word1 %in% negation_words) %>%
      count(word1, word2, wt = n, sort = TRUE) %>%
      inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
      mutate(contribution = value * n) %>%
      group_by(word1) %>%
      top_n(10, abs(contribution)) %>%
      ungroup() %>%
      mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
      ggplot(aes(word2, contribution, fill = contribution > 0)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ word1, scales = "free", nrow = 3) +
      scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
      xlab("Words preceded by a negation") +
      ylab("Sentiment score * # of occurrences") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      coord_flip() + theme_bw()
  })
  
}
shinyApp(ui, server)
