
library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(ggplot2)
# library(gbm)
library(dplyr)
library(magrittr)
library(knitr)
library(RCurl)

shinyServer(function(input, output, session) {

    # create the drop down box one-way EDA variable selection
    selectData <- reactive({
        newData <- data %>% select(!!input$variable)
        if(!!input$variable == "Fatal" | 
           !!input$variable == "Total.Passengers" |
           !!input$variable == "Total.Injuries"){
            table(summary(newData), dnn=names(newData))
        } else {
            table(newData, dnn=names(newData))
        }
        })
    
    output$table1 <- renderTable({
        selectData()
    })
        
    # create the datatable for tab 5
    getData <- reactive({
        data
    })
    
    output$datatable <- renderDataTable({
        getData()
    })
    
})


