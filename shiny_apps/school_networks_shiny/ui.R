
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(networkD3)
library(htmlwidgets)
library(readr)
library(Matrix)

load('cooccur_matrix.Rdata')
load('schoolcodemapping_fuller.Rdata')
codemap <- percode_schoolmapping_fuller
school_codemap <- data.frame(codemap$pgcode,codemap$school_name)
colnames(school_codemap) <- c("pgcode","schoolname")

choices <- paste(as.character(codemap$pgname), " (", as.character(codemap$pgcode), ")", sep="")

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #A9A9A9;}"))
  ),

  # Application title
  titlePanel("School Networks: Which Schools Are Related To Each Other?"),
  
  helpText(HTML("<p>Select a school program to discover similar programs.</p>
                <p>Double-click on a node to see the network for that program.</p>
                <p>The node size is relative to the number of applicants that apply to that school.</p>")),
  
  sidebarLayout(
                sidebarPanel(
                  
                  selectizeInput("school",
                                 label="Select or type a program name or program code.\n",
                                 choices = sort(choices)),
                  
                  conditionalPanel(
                    condition='input.school!=""',
                    
                    hr(),
                    
                    h4("Program Information"),
                    htmlOutput("school_info"),
                    
                    br(),
                    
                    h4("Related Programs"),
                    htmlOutput("related_programs")
                  )
                ),
                
          mainPanel(
            forceNetworkOutput("school_network")
            
          )
      )
    )
  )
  
