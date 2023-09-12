## load libraries
library(tidyverse)
library(stringr)
library(tidytext)

##load in dataset and filter for inaugural speeches
speeches <- read.csv("presidential_speeches.csv")
first <- speeches %>% filter(str_detect(Speech.Title, "Inaugural"))

# script to get inaugral speeches not in dataset like biden
source("addspeeches.R")

## add inaugural speeches titled something other than inaugural from speeches dataset
tyler <-
  speeches %>% filter(
    str_detect(
      Speech.Title,
      "Address Upon Assuming the Office of President of the United States"
    )
  )
fillmore <-
  speeches %>% filter(
    str_detect(Speech.Title, "First Annual Message") &
      str_detect(President, "Millard Fillmore")
  )
arthur <-
  speeches %>% filter(
    str_detect(Speech.Title, "Address Upon Assuming the Office of the President") &
      str_detect(President, "Chester")
  )
truman <-
  speeches %>% filter(str_detect(Speech.Title, "First Speech") &
                        str_detect(President, "Harry"))
ford <-
  speeches %>% filter(str_detect(Speech.Title, "Oath") &
                        str_detect(President, "Ford"))
roosevelt <-
  speeches %>% filter(str_detect(Speech.Title, "Death") &
                        str_detect(President, "Theodore Roosevelt"))

## add all inaugural speeches to the dataset
first <-
  rbind(first, tyler, fillmore, arthur, truman, ford, roosevelt)
first <-
  first %>% select(Date, President, Party, Speech.Title, Transcript)
first <- rbind(first, biden, eisenhower)

first <- first %>% arrange(Date)
first <- first[order(first$President), ]


## standardize speech names 
rows <- nrow(first)
count <- 1
for (i in 1:rows) {
  name <- first$President[i]
  if (i > 1) {
    old_name <- first$President[i - 1]
    if (name == old_name)
    {
      count <- count + 1
    }
    else
    {
      count <- 1
    }
    if (count == 1) {
      first$Speech.Title[i] <- "1st Inaugural Speech"
    }
    
    if (count == 2) {
      first$Speech.Title[i] <- "2nd Inaugural Speech"
    }
    
    if (count == 3) {
      first$Speech.Title[i] <- "3rd Inaugural Speech"
    }
    
    if (count == 4) {
      first$Speech.Title[i] <- "4th Inaugural Speech"
    }
    
  }
  
}
first <- first %>% arrange(Date)
##silly lincoln
first$Speech.Title[21] <- "1st Inaugural Speech"



## tokenize speeches
f <- first %>%
  unnest_tokens(word, Transcript)



## classify each word as positive or negative
library(syuzhet)
bing <- get_sentiments("bing")

tokenized_first <- f %>% left_join(bing)

token <- na.omit(tokenized_first)



## color palettes for graph
r_palette <- c("#E24613", "#a70000")
d_palette <- c("#011f4b", "#83A1CD", "#00308F", "#F0F8FF")
dr_palette <- c("#310047", "#AA336A")
w_palette <- c("#EEA429", "#562b00")



## dashboard
library(shiny)
library(shinydashboard)

presidents_list <- unique(token$President)

ui <- fluidPage(
  titlePanel("Sentiments of Presidents' Inaugural Speeches"),
  helpText( HTML(
    '<a id = "yes" href="https://github.com/skanji1004/USInauguralSentiments" target="_blank"> Code </a>'),
    align = "right"),
  tags$head(tags$style(
    HTML(
      "
        
        body {
          background-color: #E8DCCA;
        }

        #sidebar{
          background-color: #E8DCCA;
          border: none;
        }

        #main{

          border: none;
        }


        "
    )
  )),
  
 ## sidebar with inputs of president name and number of words per group 
sidebarLayout(
  sidebarPanel(
    id = "sidebar",
    selectizeInput(
      inputId = "president_input",
      label = "President",
      choices = presidents_list,
      selected = presidents_list[7],
      multiple = FALSE,
      options = list(placeholder = "Type here")
    ),
    
    sliderInput(
      inputId = "group_slider",
      label = "Number of Words per Group",
      min = 1,
      max = 100,
      value = 25,
    ),
    
    
    helpText(
      "In each speech, words are grouped together (The first 20 words form the first group and so on).The number of positive words and negative words in each group is calculated. The difference in positive and negative words for one group of words is plotted as one point. Adjust the slider to specify the number of words in a group"
    ),
    
    
  ),
  
  ##main panel has graph displayed
  mainPanel(
    id = "main",
    plotOutput("president_plot"),
    helpText(
      "Words were classified as positive or negative using the bing lexicon. It should be acknowledged that many words in these inaugural speeches are not part of this lexicon and therefore could not be categorized. Nevertheless, this visualization is an interesting perspective on the ",
      HTML(
        '<a href="https://www.kaggle.com/datasets/littleotter/united-states-presidential-speeches" target="_blank"> data </a>'
      ),
      align = "center"
    )
    
  )
))




#Server
server <- function(input, output, session) {
  
    output$president_plot <- renderPlot({
    num_words <- input$group_slider
    president <- input$president_input
    
    ## when a president is selected, form groups of words based on user input. 
    ## Find number of positive and negative words in group, and find the difference
    ## create dataset with president name, group number, and net sentiment (the differnce of postive and negative)
    
    sentiment_summary <- token %>%
      group_by(Date,
               Party,
               President,
               Speech.Title,
               group = (row_number() - 1) %/% num_words) %>%
      summarise(
        positive_sum = sum(sentiment == "positive"),
        negative_sum = sum(sentiment == "negative"),
        net_sentiment = positive_sum - negative_sum
      ) %>%
      filter(President == president) %>%
      arrange(Date) %>%
      ungroup()
    ## reset group number count if a new speech begins (i.e second inaugural speech etc)
    rows <- nrow(sentiment_summary)
    count <- 1
    for (i in 1:rows)
    {
      title <- sentiment_summary$Speech.Title[i]
      
      if (i > 1)
      {
        old_title <- sentiment_summary$Speech.Title[i - 1]
        if (title == old_title)
        {
          sentiment_summary$group[i] <- count
          count <- count + 1
          
        }
        
        else
        {
          count <- 1
          sentiment_summary$group[i] <- count
          count <- count + 1
        }
        
      }
      
      else
      {
        sentiment_summary$group[i] <- count
        count <- count + 1
      }
    }
    
    ## color palette of graph based on party of president 
    party <- sentiment_summary$Party[1]
    palette
    if (party == "Democratic")
    {
      palette <- d_palette
    }
    
    else if (party == "Republican")
    {
      palette <- r_palette
    }
    
    else if (party == "Democratic-Republican")
    {
      palette <- dr_palette
    }
    
    else
    {
      palette <- w_palette
    }
    
    ## plot net sentiment
    ggplot(sentiment_summary,
           aes(x = group, y = net_sentiment, color = Speech.Title)) +
      geom_line(linewidth = 1.5) +
      scale_color_manual(values = palette, name = "Speech Title") +
      geom_hline(
        yintercept = 0,
        color = "black",
        linewidth = 0.5
      ) +
      labs(x = paste("Groups (", num_words, "words per group)"),
           y = "Net Sentiment") + theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#E8DCCA"),
        panel.grid = element_line(color = "beige"),
        legend.background = element_rect(fill = "#E8DCCA"),
        legend.title = element_text(color = "black", size = 12)
      )
    
    
  })
  
  
  
}



# # # Run the app
shinyApp(ui, server)
