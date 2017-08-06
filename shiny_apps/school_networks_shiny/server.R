
# This is the server logic for a Shiny web application.
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
library(stringr)

load('cooccur_matrix.Rdata')
load('schoolcodemapping_fuller.Rdata')
codemap <- percode_schoolmapping_fuller
school_codemap <- data.frame(codemap$pgcode,codemap$school_name)
colnames(school_codemap) <- c("pgcode","schoolname")

clickJS <- '
d3.selectAll(".node").on("click",
    function(d){
      Shiny.unbindAll();
      var $select = $(\'#school\').selectize();
      $select[0].selectize.setValue(d.name);
      Shiny.bindAll()
    })
'
get_related_nodes <- function(code) {
  if(code!="") {
    
    code <-
      codemap %>%
      mutate(display_name=paste(as.character(pgname), " (", as.character(pgcode), ")", sep="")) %>%
      filter(display_name==code)
    
    code <- as.character(code[1, "pgcode"])
    
    v <- C[code, which(C[code, ] > 2)]
    v <-  tail(sort(v),11)
    
    co_occur<- C[names(v),names(v)]
    
    ### now plot things!
    # convert the matrix back to a long dataframe
    # first, grab indicies
    network <- as.data.frame(which(co_occur!=0,arr.ind = T))
    df <- as.data.frame(network)
    
    # next, get edge weights
    rownames(df) <- NULL
    df$weight <- co_occur[which(co_occur!=0)]
    
    # make things zero indexed so there's no complaining
    df<- df %>%
      filter(row < col) %>%
      mutate(row = row - 1,
             col = col - 1)
    
    # and provide nice names
    names(df) <- c("source", "target", "value")
    
    #mapping school names to the program codes 
    df_mat <- data.frame(names(v),v[names(v)])
    
    colnames(df_mat)<-c("pgcode","value")
    rownames(df_mat)<- NULL
    final <- merge(df_mat,percode_schoolmapping_fuller, by ="pgcode") %>%
      mutate(display_name=paste(as.character(pgname), " (", as.character(pgcode), ")", sep=""))
    
    # add node labels and sizes
    nodes<- data.frame(name = final$display_name,
                       group = 1,
                       size = diag(co_occur),
                       dbn = final$dbn,
                       school_name = final$school_name)
    
    
    # same thing for program codes only
    # nodes<- data.frame(name = final$pgcode,
    #                    group = 1,
    #                    size = diag(co_occur))
    # 
    nodes <- nodes %>% 
      mutate(group = ifelse(name==code,1,2))
    
    return(nodes)
  }
}

display_school_neighborhood <- function(code){
  if(code!="") {
    
    code <-
      codemap %>%
      mutate(display_name=paste(as.character(pgname), " (", as.character(pgcode), ")", sep="")) %>%
      filter(display_name==code)
    
    code <- as.character(code[1, "pgcode"])
    
    v <- C[code, which(C[code, ] > 2)]
    v <-  tail(sort(v),10)
  
    co_occur<- C[names(v),names(v)]
  
    ### now plot things!
    # convert the matrix back to a long dataframe
    # first, grab indicies
    network <- as.data.frame(which(co_occur!=0,arr.ind = T))
    df <- as.data.frame(network)
    
    # next, get edge weights
    rownames(df) <- NULL
    df$weight <- co_occur[which(co_occur!=0)]
    
    # make things zero indexed so there's no complaining
    df<- df %>%
      filter(row < col) %>%
      mutate(row = row - 1,
             col = col - 1)
    
    # and provide nice names
    names(df) <- c("source", "target", "value")
    
    #mapping school names to the program codes 
    df_mat <- data.frame(names(v),v[names(v)])
    
    colnames(df_mat)<-c("pgcode","value")
    rownames(df_mat)<- NULL
    final <- merge(df_mat,percode_schoolmapping_fuller, by ="pgcode") %>%
       mutate(display_name=paste(as.character(pgname), " (", as.character(pgcode), ")", sep=""))
    
    # add node labels and sizes
    nodes<- data.frame(name = final$display_name,
                       group = 1,
                       size = diag(co_occur))

    code <- as.character((final %>% filter(pgcode == code))$display_name[1])

    # same thing for program codes only
    # nodes<- data.frame(name = final$pgcode,
    #                    group = 1,
    #                    size = diag(co_occur))
    # 
    nodes <- nodes %>% mutate(group = ifelse(name==code,1,2))
    
    # do a nicer plot that sizes nodes and weights edges
    df <-
      df %>%
      filter(value>1)
    
    ColourScale <- 'd3.scaleOrdinal().domain(["1","2"]).range(["#FF6900","#694489"]);'
    
    fn <-forceNetwork(Links = df, Nodes = nodes, Source = "source",
                      Target = "target", Value = "value", NodeID = "name", Nodesize = "size",
                      Group = "group", opacity = 0.4, zoom = TRUE,fontSize = 32, linkDistance = 150, colourScale=JS(ColourScale),
                      clickAction = clickJS)
    return(fn)
  }
}

shinyServer(function(input, output) {

  output$school_network <- renderForceNetwork({
    
    display_school_neighborhood(input$school)

  })
  
  output$school_info <- renderUI({
    code <-
      codemap %>%
      mutate(display_name=paste(as.character(pgname), " (", as.character(pgcode), ")", sep="")) %>%
      filter(display_name==input$school)
    
    code <- as.character(code[1, "pgcode"])
    
    pgname <- as.character((percode_schoolmapping_fuller %>% filter(pgcode == code))$pgname[1])
    school_name <- as.character((percode_schoolmapping_fuller %>% filter(pgcode == code))$school_name[1])
    interest <- as.character((percode_schoolmapping_fuller %>% filter(pgcode == code))$interest[1])
    method <- as.character((percode_schoolmapping_fuller %>% filter(pgcode == code))$method[1])
    #size <- as.character((percode_schoolmapping_fuller %>% filter(pgcode == code))$size[1])
    
    nodes <- get_related_nodes(input$school)
    input_name <- unlist(strsplit(input$school, split=' (', fixed=TRUE))[1]
    size <- as.character((nodes %>% filter(grepl(input_name, name)))$size[1])

    str1 <- paste("<b>Program Name:</b> ", pgname)
    str2 <- paste("<b>Program Code:</b> ", code)
    str3 <- paste("<b>School:</b> ", school_name)
    str4 <- paste("<b>Interest Area:</b> ", interest)
    str5 <- paste("<b>Admissions Method:</b> ", method)
    str6 <- paste("<b>Number of Applicants in 2015:</b>", size)
    
    HTML(paste(str1, str2, str3, str4, str5, str6, sep='<br/>'))
  })
  
  output$related_programs <- renderUI({
    nodes <- get_related_nodes(input$school)
    
    final_str <- "<ol type='1'>"
    
    for (i in seq(nrow(nodes))) {
      if(nodes[i, 'name']!=input$school) {
        final_str <- paste(final_str,
                           "<li>", 
                           "<a href='http://schoolfinder.nyc.gov/#/schools/",
                           as.character(nodes[i,'dbn']),
                           "' target='_blank'>",
                           as.character(nodes[i, 'name']),
                           "</a></li>",
                           sep="")
      }
    }
    
    HTML(paste(final_str, "</ol/>"))
  })

})
