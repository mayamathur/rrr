
library(shiny)

# try to fix deployment problem
library(purrr)
library(plogr)

library(shinythemes)
library(bsplus)
library(ggplot2)
library(data.table)
library(dplyr)

# keeps original error messages
options(shiny.sanitize.errors = FALSE)

agg1 = read.csv("agg1_porig.csv")
agg2 = read.csv("agg2_phat.csv")

# functions
subset_agg1 = function(input) {
  data = agg1
  
  # successively subset on each variable
  if ( !is.null(input$k) ) {
    data <- data[data$k %in% input$k,]
  }

  if ( !is.null(input$Norig) ) {
    data <- data[data$N.orig %in% input$Norig,]
  }
  if ( !is.null(input$minN) ) {
    data <- data[data$minN %in% input$minN,]
  }
  if ( !is.null(input$V) ) {
    data <- data[data$V %in% input$V,]
  }
  if ( !is.null(input$dist) ) {
    data <- data[data$dist.pretty %in% input$dist,]
  }
  if ( !is.null(input$POrigMethod) ) {
    data <- data[data$POrig.Method %in% input$POrigMethod,]
  }
  if ( !is.null(input$delta) ) {
    data <- data[data$delta %in% input$delta,]
  }
  data
}



subset_agg2 = function(input) {
  data = agg2
  
  # successively subset on each variable
  if ( !is.null(input$k2) ) {
    data <- data[data$k %in% input$k2,]
  }
  if ( !is.null(input$minN2) ) {
    data <- data[data$minN %in% input$minN2,]
  }
  if ( !is.null(input$V2) ) {
    data <- data[data$V %in% input$V2,]
  }
  if ( !is.null(input$dist2) ) {
    data <- data[data$dist.pretty %in% input$dist2,]
  }
  if ( !is.null(input$TheoryP2) ) {
    data <- data[data$TheoryP %in% input$TheoryP2,]
  }
  data
}



