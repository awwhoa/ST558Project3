
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
library(plotly)
library(LiblineaR)
library(randomForest)


shinyServer(function(input, output, session) {

    ### Tab 2: EDA ###
    # create and render the drop down box for the one-way EDA variable selection
    selectCatData <- reactive({
        newData <- data %>% select(!!input$var1)
            table(newData, dnn=names(newData))
        })
    
    # render the frequency table for categorical variables
    output$table1 <- renderTable({
        selectCatData()
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
    
    ##### NUMERIC VARIABLE EXPLORATION
    ### Numeric Response Variable
    # distribution table for Fatal
    output$fatal <- renderTable({
        table(data$Fatal, dnn="Fatal")
    })
    # numeric summary for Fatal MAY LEAVE THIS OUT SINCE IT'S A TRULY BINARY VARIABLE
    calcSummmaryStats <- function(x){
        subset <- data %>%
            select(x) %>%
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
        calcSummmaryStats("Fatal")
    })
    
    ### Numeric Predictors
    # create histogram of total injuries
    plotHist <- reactive({
        ggplot(data, aes(x=!!input$numvar)) + geom_histogram(colour="orange")
    })

    output$pass <- renderPlot({
        # ggplot(data, aes(Total.Passengers)) + geom_histogram(colour="orange")
        plotHist()
    })

    # create histogram of total injuries
    output$injur <- renderPlot({
        ggplot(data, aes(Total.Injuries)) + geom_histogram(colour="orange")
    })

    # create summary stats table for total passengerss
    output$sum <- renderTable({
        if(!!input$numvar == "Total.Passengers"){
            calcSummmaryStats("Total.Passengers")
        } else {
            calcSummmaryStats("Total.Injuries")
        }
    })

    # better plot for total passengers
    numPlot <- reactive({
        if(!!input$numvar == "Total.Passengers"){
            p <- data %>% group_by(Total.Passengers) %>% summarize(ProportionFatal = mean(Fatal), n=n())
            ggplot(p, aes(x = Total.Passengers, y = ProportionFatal)) + 
                geom_point(stat = "identity", aes(size = n), colour="orange")
        } else {
            p <- data %>% group_by(Total.Injuries) %>% summarize(ProportionFatal = mean(Fatal), n=n())
            ggplot(p, aes(x = Total.Injuries, y = ProportionFatal)) + 
                geom_point(stat = "identity", aes(size = n), colour="orange")
        }
    })
    
    output$propfatal <- renderPlot({
        numPlot()
    })
    
    # create the bivariate plot between two numeric response variables
    plotBivarResponseNumeric <- reactive({
        ggplot(data, aes(x=Total.Passengers, y=Total.Injuries)) + geom_point(colour="orange")
    })
    
    plotBivarResponse <- reactive({
        g <- ggplot(data, aes(x=Total.Passengers, y=Total.Injuries, col=Fatal)) + geom_point()
        print(g + scale_colour_gradient(low = "black", high = "orange"))
    })
    
    # create the bivariate plot of the two numeric predictors
    # #### INCLUDE AN ADDITIONAL OPTION/PLOT/conditional panel TO HIGHLIGHT THOSE THAT WERE FATAL
    output$bivarsnum <- renderPlot({
        plotBivarResponseNumeric()
    })
    
    # make the numeric predictor bivariate plot clickable
    # output$info <- renderText({
    #     xy_str <- function(e) {
    #         if(is.null(e)) return("")
    #         paste0(round(e$x, 0), " total passengers and ", round(e$y, 0), " total injuries")
    #     }
    #     xy_str(input$plot_hover)
    # })
    
    ### Tab 3:  Clustering ###
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
    
    # ADD INTERACTIVITY TO THIS CLUSTER PLOT IF POSSIBLE
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
    
    output$dendro <- renderPlot({ ### HOW DO I INCLUDE INPUTS FOR THIS THING ????
        hcl <- hclust(dist(data.frame(data$Total.Passengers, data$Total.Injuries)))
        plot(hcl, xlab = "")
    })
    
    ##### Tab 4:  Regression Model #####
    # build the glm equation 
    buildGLM <- eventReactive(input$go, {
        var1 <- !!!input$predlog1
        var2 <- !!!input$predlog2
        var3 <- !!!input$predlog3
        fit <- glm(Fatal ~ var1 + var2 + var3,
                   family="binomial",
                   data=train)
        summary(fit)
    })
    
    output$regmodel <- renderPrint({
        buildGLM()
    })
    
    
    # try a linear regression model
    buildLM <- eventReactive(input$go, {
        var1 <- !!input$predlog1
        # var2 <- !!input$predlog2
        # var3 <- !!input$predlog3
        f <- as.formula(Fatal ~ var1, data=train)
        # fit <- lm(Total.Injuries ~ var1,# + var2 + var3,
        #           data=train)
        summary(fit)
    })
    
    output$regmodel <- renderPrint({
        buildLM()
        # "success"
    })
    
    # try the thing found at stackoverflow
    mlt <- reactive({
        reformulate(input$dep, Fatal)
        
    })
    
    
    
    ### Tab 5: Data Table of all Data ###    
    # create the datatable for tab 5
    getData <- reactive({
        data
    })
    
    output$datatable <- renderDataTable({
        getData()
    })
    
    # Button to download entire analysis data file from the data table page
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('AviationData-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(data, con)
        }
    )
    
    ##### Modeling:  Logistic Regression #####
    
    

})


