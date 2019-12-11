
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
    
    # create the categorical one-way plots (based on user variable selection)
    output$plot1 <- renderPlot({
            plotBar()
    })
    
    # select the appropriate bivariate plot based on user selection
    output$plot2 <- renderPlot({
        plotBivarTarget()
    })

    # create download for categorical plot1
    output$downloadPlot1 <- downloadHandler(
        filename = function() { paste('PlotFreq', input$var1, '.png', sep='') },
        content = function(file) {
            device <- function(..., width, height) 
                grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = plotBar(), device = device)
        }
    )
    
    # create download for categorical plot2
    output$downloadPlot2 <- downloadHandler(
        filename = function() { paste('PlotFreq', input$var1, 'byFatal', '.png', sep='') },
        content = function(file) {
            device <- function(..., width, height) 
                grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = plotBivarTarget(), device = device)
        }
    )
    
    
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
        data[, c("Total.Passengers","Total.Injuries")]
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
    
    observe({
        updateNumericInput(session, "clusters")
    })
    
    observe({
        updateNumericInput(session, "iteration")
    })
    
    # Button to download current version fo interactive cluster plot
    # output$downloadPlot <- downloadHandler(
    #     filename = function() {
    #         paste('ClusterPlot-', Sys.Date(), '.png', sep='')
    #     },
    #     content = function(con) {
    #         ggsave("cluster plot", input$plotclus)
    #     }
    # )
    
    output$dendro <- renderPlot({ ### HOW DO I INCLUDE INPUTS FOR THIS THING ????
        hcl <- hclust(dist(data.frame(data$Total.Passengers, data$Total.Injuries)))
        plot(hcl, xlab = "")
    })
    
    ### Tab 5: Data Table of all Data ###    !!!!! FIND TEH WAY TO SET UP USER DOWNLOADS FROM THE FOR THIS DATA TABLE
    # create the datatable for tab 5
    getData <- reactive({
        data
    })
    
    output$datatable <- renderDataTable({
        getData()
    })
    
    # Button to download entire modelign data file from the data table page
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('AviationData-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(data, con)
        }
    )
    
})


