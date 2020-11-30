# load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)

# load data --------------------------------------------------------------------



# ui ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Scotland and UK COVID-19 Speeches")
)

# server -----------------------------------------------------------------------

server <- function(input, output){}

# create shiny app -------------------------------------------------------------

shinyApp(ui = ui, server = server)