
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)

data(MisLinks)
data(MisNodes)

input <- c()
input$school <- "Valjean"
shinyServer(function(input, output) {

  output$school_network <- renderForceNetwork({
    MisNodes <-
      MisNodes %>%
      mutate(id=row_number()-1)
    
    selected_school <- input$school
    school_id <- MisNodes[which(MisNodes$name==selected_school), ]$id
    school_group_num <- MisNodes[which(MisNodes$name==selected_school), ]$group
    
    # school_group_nodes <-
    #   MisNodes %>%
    #   filter(group==school_group_num)
    
    school_group_links <-
      MisLinks %>%
      filter(source==school_id | target==school_id)
    
    forceNetwork(Links = school_group_links, Nodes = MisNodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.4, zoom = TRUE)

  })

})
