
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
data(MisLinks)
data(MisNodes)

shinyUI(fluidPage(

  # Application title
  titlePanel("School Networks"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("school",
                  label="Select a character",
                  choices = as.list(sort(MisNodes$name)))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      
      forceNetworkOutput("school_network")
    )
  )
))
