
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
    
    # NUMERIC VARIABLE EXPLORATION
    output$fatal <- renderTable({
        table(data$Fatal, dnn="Fatal")
    })

    output$pass <- renderPlot({
        ggplot(data, aes(Total.Passengers)) + geom_dotplot(dotsize=.3, colour="orange")
    })

    output$injur <- renderPlot({
        ggplot(data, aes(Total.Passengers)) + geom_dotplot(dotsize=.3, colour="orange")
    })

    calcSummmaryStats1 <- function(){
        subset <- data %>%
            select(Fatal) %>%
            summary()
        Min <- as.numeric(gsub('.*:', '', subset[1,]))
        Q1 <- as.numeric(gsub('.*:', '', subset[2,]))
        Median <- as.numeric(gsub('.*:', '', subset[3,]))
        Mean <- as.numeric(gsub('.*:', '', subset[4,]))
        Q3 <- as.numeric(gsub('.*:', '', subset[5,]))
        Max <- as.numeric(gsub('.*:', '', subset[6,]))
        tbl <- round(cbind(Min,Q1,Mean,Median,Q3,Max),3)
    }

    output$sumfatal <- renderTable({
        calcSummmaryStats1()
    })

    calcSummmaryStats2 <- function(){
        subset <- data %>%
            select(Total.Passengers) %>%
            summary()
        Min <- as.numeric(gsub('.*:', '', subset[1,]))
        Q1 <- as.numeric(gsub('.*:', '', subset[2,]))
        Median <- as.numeric(gsub('.*:', '', subset[3,]))
        Mean <- as.numeric(gsub('.*:', '', subset[4,]))
        Q3 <- as.numeric(gsub('.*:', '', subset[5,]))
        Max <- as.numeric(gsub('.*:', '', subset[6,]))
        tbl <- round(cbind(Min,Q1,Mean,Median,Q3,Max),3)
    }

    output$sumpass <- renderTable({
        calcSummmaryStats2()
    })

    calcSummmaryStats3 <- function(){
        subset <- data %>%
            select(Total.Injuries) %>%
            summary()
        Min <- as.numeric(gsub('.*:', '', subset[1,]))
        Q1 <- as.numeric(gsub('.*:', '', subset[2,]))
        Median <- as.numeric(gsub('.*:', '', subset[3,]))
        Mean <- as.numeric(gsub('.*:', '', subset[4,]))
        Q3 <- as.numeric(gsub('.*:', '', subset[5,]))
        Max <- as.numeric(gsub('.*:', '', subset[6,]))
        tbl <- round(cbind(Min,Q1,Mean,Median,Q3,Max),3)
    }

    output$suminjur <- renderTable({
        calcSummmaryStats3()
    })
    # create the bivariate plot between two numeric response variables
    plotBivarResponseNumeric <- reactive({
        ggplot(data, aes(x=Total.Passengers, y=Total.Injuries)) + geom_point(colour="orange")
    })
    # select the appropriate bivariate plot based on user selection
    output$bivarsnum <- renderPlot({
        plotBivarResponseNumeric()
    })

    # CLUSTERING TAB        
    # Combine user-selected variables into a new data frame
    selectClusterData <- reactive({
        numSubset[, c(input$xlcus, input$yclus)]
    })
    
    clusters <- reactive({
        set.seed(10)
        kmeans(selectClusterData(), 
               input$clusters, 
               iter.max = input$iteration, 
               algorithm = "MacQueen")
    })
    
    output$plotclus <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectClusterData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    output$plotclus <- renderPlot({
        hierClust <- hclust(dist(data.frame(data$Total.Passengers, data$Total.Injuries)))
        plot(hierClust, xlab = "")
    })
    
    knitr::include_graphics("hierClust.PNG")
    
    ### Tab 5: Data Table of all Data ###    !!!!! FIND TEH WAY TO SET UP USER DOWNLOADS FROM THE FOR THIS DATA TABLE
    # create the datatable for tab 5
    getData <- reactive({
        data
    })
    
    output$datatable <- renderDataTable({
        getData()
    })
    
})


