
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

    ### Tab 2: EDA ###
    # create and render the drop down box for the one-way EDA variable selection
    selectData <- reactive({
        newData <- data %>% select(!!input$var1)
        if(!!input$var1 == "Fatal" | 
           !!input$var1 == "Total.Passengers" |
           !!input$var1 == "Total.Injuries"){
            summary(newData)#, dnn=names(newData))
        } else {
            table(newData, dnn=names(newData))
        }
        })
    
    # render the frequency table for categorical variables
    output$table1 <- renderTable({
        selectData()
    })
    
    # create the one-way dotplot
    plotDot <- reactive({
        ggplot(data, aes(x=!!input$var1)) + geom_dotplot(dotsize=.3, colour="orange")
    })
    
    # create the one-way barplot w/ a coordindate flip    
    plotBar <- reactive({
        ggplot(data, aes(x=!!input$var1)) + geom_bar(colour="orange") + coord_flip()
    })
    
    # create the bivariate plot with the target variable based on user selection
    plotBivarTarget <- reactive({
        ggplot(data, aes(x=!!input$var1, y=Fatal)) + 
            geom_bar(stat="identity", colour="orange") + 
            coord_flip() +
            labs(y="Count of Fatal Incidents")
    })
    
    # create the bivariate plot between two numeric response variables
    plotBivarResponseNumeric <- reactive({
        ggplot(data, aes(x=Total.Passengers, y=Total.Injuries)) + geom_point( colour="orange")
    })
    
    # select the appropriate one-way plot based on user selection
    output$plot1 <- renderPlot({
        if(!!input$var1 == "Total.Passengers" |
           !!input$var1 == "Total.Injuries"){
            plotDot() + labs(y="Count")
        } else {
            plotBar()
        }
    })
    
    output$plot2 <- renderPlot({
            plotBivarTarget()
    })
    
    # select the appropriate bivariate plot based on user selection
    output$plot2 <- renderPlot({
        plotBivarTarget()
    })
    
    # select the appropriate bivariate plot based on user selection
    output$bivarsnum <- renderPlot({
        plotBivarResponseNumeric()
    })

        ### Tab 5: Data Table of all Data ###
    # create the datatable for tab 5
    getData <- reactive({
        data
    })
    
    output$datatable <- renderDataTable({
        getData()
    })
    
})


