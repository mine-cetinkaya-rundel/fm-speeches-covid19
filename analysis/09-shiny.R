# load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(tidytext)
library(glue)

# read data --------------------------------------------------------------------

covid_speeches_words <- read_rds(here::here("processed-data", "covid-speeches-words.rds"))

# ui ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Scotland and UK COVID-19 Speeches"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "origin",
        label   = "Select origin",
        choices = c("Scotland", "UK")
      ),
      dateRangeInput(
        inputId = "date_range",
        label   = "Select date range",
        start   = min(covid_speeches_words$date),
        end     = max(covid_speeches_words$date)
      )
    ),
    mainPanel(
      plotOutput(outputId = "sentiment_plot")
    )
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output) {
  output$sentiment_plot <- renderPlot({
    covid_speeches_words %>%
      filter(origin == input$origin) %>%
      filter(between(date, input$date_range[1], input$date_range[2])) %>%
      anti_join(stop_words) %>%
      filter(word != "positive") %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(date, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(x = date, y = sentiment)) +
      geom_line(color = "gray") +
      geom_point(aes(color = sentiment > 0), size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
      guides(color = FALSE) +
      labs(
        title = glue("Daily sentiment score of words in {input$origin} COVID-19 briefings"),
        subtitle = "Bing lexicon",
        x = "Date", y = "Sentiment score (positive - negative)"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

# create shiny app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
