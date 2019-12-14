
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
library(tree)


shinyServer(function(input, output, session) {
    
    # Render Data Dictionary Table 
    output$dict <- renderTable({
        dictionary
    })
    
    ##### Tab 2: EDA #####
    ### Response Variable - Fatal ###
    # Distribution table - render
    output$fatal <- renderTable({
        table(data$Fatal, dnn="Fatal")
    })
    
    # Distribution plot - create
    plotBarFatal <- reactive({
        ggplot(data, aes(x=Fatal)) + geom_bar(colour="orange") + 
            geom_text(stat='count', aes(label=..count..), size=6) 
    })
    
    # Distribution plot - render
    output$plot13 <- renderPlot({
        plotBarFatal()
    })

    ### Categorical Predictors ###
    # Create the reactive freq table
    selectCatData <- reactive({
        newData <- data %>% select(!!input$var1)
        table(newData, dnn=names(newData))
    })
    
    # render the reactive frequency table 
    output$table1 <- renderTable({
        selectCatData()
    })
    
    # create the reactive one-way barplot 
    plotBar <- reactive({
        ggplot(data, aes(x=!!input$var1)) + geom_bar(colour="orange") + 
            geom_text(stat='count', aes(label=..count..), colour="black", size=5) +
            labs(y="Count of All Incidents") + 
            coord_flip()
    })
    
    # render the one-way plots 
    output$plot1 <- renderPlot({
        plotBar()
    })
    
    # create download for univariate categorical plot1
    output$downloadPlot1 <- downloadHandler(
        filename = function() { paste('PlotFreq', input$var1, '.png', sep='') },
        content = function(file) {
            device <- function(..., width, height) 
                grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = plotBar(), device = device)
        }
    )
    
    # create download for univariate categorical plot2
    output$downloadPlot2 <- downloadHandler(
        filename = function() { paste('PlotFreq', input$var1, 'byFatal', '.png', sep='') },
        content = function(file) {
            device <- function(..., width, height) 
                grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = plotBivarTarget(), device = device)
        }
    )

    # create the bivariate plot with the target variable based on user selection
    plotBivarTarget <- reactive({
        ggplot(data, aes(x=!!input$var1, y=Fatal)) +
            geom_bar(stat="identity", colour="orange") +
            labs(y="Count of Fatal Incidents") +
            coord_flip()
    })
    
    # render the bivariate plot based on user selection
    output$plot2 <- renderPlot({
        plotBivarTarget()
    })

    # create bivariate plot for all categorical variables
    plotBivarsCat <- reactive({
        ggplot(data, aes(x=!!input$var4)) +
            geom_bar(aes(fill = !!input$var5), position = "dodge") #+
        # labs(x=, fill="Category")
    })
    # render the biviariate plot for categorical variables
    output$plot45 <- renderPlot({
        plotBivarsCat()
    })
    
    ## create bivariate plots for all the world to see
    # bivariatePlots <- reactive({
        # ggplot(data, aes(x=Aircraft.Damage)) +
        #     geom_bar(aes(fill = Aircraft.Category), position = "dodge")
    # if((input$bivar1 == "Total.Passengers" | 
    #     input$bivar1 == "Total.Injuries" |
    #     input$bivar1 == "Fatal") & 
    #    (input$bivar2 == "Total.Passengers" | 
    #     input$bivar2 == "Total.Injuries" |
    #     input$bivar2 == "Fatal")) {
    #     ggplot(data, aes(x=input$bivar1, y=input$bivar2)) + geom_point()
    # } else if(is.character(input$bivar1) & is.character(input$bivar2)) {
    #     ggplot(data, aes(x=input$bivar1)) + 
    #         geom_bar(aes(fill = input$bivar2), position = "dodge") 
    # } else if((input$bivar1 == "Total.Passengers" | 
    #            input$bivar1 == "Total.Injuries" |
    #            input$bivar1 == "Fatal") & 
    #           is.character(input$bivar2)){
    #     ggplot(data, aes(x=input$bivar2, y=input$bivar1)) + 
    #         geom_bar(stat="identity", colour="orange") #+ coord_flip()
    # } else if((input$bivar2 == "Total.Passengers" | 
    #            input$bivar2 == "Total.Injuries" |
    #            input$bivar2 == "Fatal") & 
    #           is.character(input$bivar1)){ 
    #     ggplot(data, aes(x=input$bivar1, y=input$bivar2)) + 
    #         geom_bar(stat="identity") #+ coord_flip()
    # }
    # })
    

    # # Some bivariates - hope they work     
    # output$bivarsall <- renderPlot({
    #     bivariatePlots()
    # })
    # 
    # observe({
    #     updateSelectInput(session, "bivar1")
    # })
    # 
    # observe({
    #     updateSelectInput(session, "bivar2")
    # })
    
    
    ##### NUMERIC VARIABLE EXPLORATION
    ### Numeric Response Variable
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
    
    # create the scatterplot plot for the numeric variables
    plotBivarResponseNumeric <- reactive({
        ggplot(data, aes(x=Total.Passengers, y=Total.Injuries)) + 
            geom_point(colour="orange")
    })
    # create the ninteractive umeric scatterplot by the response variable
    plotBivarResponse <- reactive({
        g <- ggplot(data, aes(x=Total.Passengers, y=Total.Injuries, col=Fatal)) + geom_point()
        print(g + scale_colour_gradient(low = "black", high = "orange"))
    })
    
    # render the bivariate plot of the two numeric predictors
    output$bivarsnum <- renderPlot({
        plotBivarResponseNumeric()
    })
    
    # render the scatterplot by the response variabe
    output$info <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return(NULL)
            paste0(round(e$x, 0), " total passengers and ", round(e$y, 0), " total injuries")
        }
        xy_str(input$plot_hover)
    })
    
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
    
    # generate interactive cluster plot
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
    ##### Tab 4:  Regression Model #####
    # Build and fit the logistic regression model
    logFitGLM <- eventReactive(input$go, {
        fit <- glm(as.formula(paste("Fatal ~ ",
                                    paste0(input$indvar, collapse="+"))),
                   data=train,
                   family=binomial)
        summary(fit)
    })
    # run anova stats
    logFitANOVA <- reactive({
        fit <- glm(as.formula(paste("Fatal ~ ",
                                    paste0(input$indvar, collapse="+"))),
                   data=train,
                   family=binomial)
        anova(fit)
    })
    # calculate misclassification rate of predictionso from the mode onto the holdout data set
    logFitMisclass <- reactive({
        fit <- glm(as.formula(paste("Fatal ~ ",
                                    paste0(input$indvar, collapse="+"))),
                   data=train,
                   family=binomial)
        predTbl <- table(fit, test[,"Fatal"])
        classRate <- sum(diag(predTbl)/sum(predTbl))
        as.vector(classRate)
    })
    
    wtf <- reactive({
        paste("does this work?", logFitMisclass())
    })
    
    # output$misclassrate <- renderPrint({
    #     if(input$misclass == 1) {
    #         paste("This model correctly classified accidents as Fatal ",
    #               wtf(),
    #               "% of the time.")
    #     }
    # })
        
    # generate model output w/ option for user selection
    output$regoutput <- renderPrint({
        logFitGLM()
       if(input$selectout == "ANOVA summary") {
           # then switch to user-selected output
           observe({updateSelectInput(session,"regoutput")})
           logFitANOVA()
       } else {
           observe({updateSelectInput(session,"regoutput")})
           logFitGLM()
       }
    })

    # run the user's selected attributes in the model they created
    logPredictions <- eventReactive(input$run, {
        fit <- glm(as.formula(paste("Fatal ~ ",
                                    paste0(input$indvar, collapse="+"))),
                   data=train,
                   family=binomial)

        p <- predict(fit, 
                newdata = data.frame(Aircraft.Damage = input$predictdmg, 
                                     Aircraft.Category = input$predictcateg, 
                                     Purpose.of.Flight = input$predictpurp,
                                     Broad.Phase.of.Flight = input$predictphase,
                                     Weather.Condition = input$predictweath,
                                     Engine.Count = input$predicteng,
                                     Amateur.Built = input$predictblt,
                                     Total.Injuries = input$predictinj,
                                     Total.Passengers = input$predictpass
                ),
                type = "response", se.fit = TRUE)
        p[[1]]
        paste("On average, the estimated probability of a flight having at least one fatality for the predictor attributes entered is ",
              p[[1]])
    })

    # output the prediction box conditionally
    output$regpredout <- renderPrint({
        if(input$run == 1) {
            logPredictions()
        } else if(input$go == 0){
            "Fit a model in order to run prediction "
        }
    })

    ##### Create classification tree
    treeplot <- reactive({
        t <- tree::tree(as.formula("Fatal ~ ."),
                        data=train,
                        split=input$split)
        
        plot(t)
        text(t)
        })

    treefit <- reactive({
        t <- tree::tree(as.formula("Fatal ~ ."),
                        data=train,
                        split=input$split)
        t        
    })
    
    treermse <- reactive({
        t <- tree::tree(as.formula("Fatal ~ ."),
                        data=train,
                        split=input$split)
        t        
        pred <- predict(t, newdata = dplyr::select(test, -Fatal))
        rmse <- sqrt(mean((pred-test$Fatal)^2))
        rmse
    })
    
    output$rmse <- renderPrint({
        treermse()
    })
    
    output$treestats <- renderPrint({
        treefit()
    })
    
    output$treeplot <- renderPlot({
        treeplot()
    })
    
    # run the user's selected attributes for this tree model
    treePredictions <- eventReactive(input$runtreepred, {
        t <- tree::tree(as.formula("Fatal ~ ."),
                        data=train,
                        split=input$split)
        # prediction for entered attributes
        p <- predict(t, 
                     newdata = data.frame(Aircraft.Damage = input$predictdmg2, 
                                          Aircraft.Category = input$predictcateg2,
                                          Purpose.of.Flight = input$predictpurp2,
                                          Broad.Phase.of.Flight = input$predictphase2,
                                          Weather.Condition = input$predictweath2,
                                          Engine.Count = input$predicteng2,
                                          Amateur.Built = input$predictblt2,
                                          Total.Injuries = input$predictinj2,
                                          Total.Passengers = input$predictpass2
                                          )
                     )
        # p[[1]]
        paste("On average, the estimated probability of a flight having at least one fatality for the predictor attributes entered is ", p,".")
    })
    
    output$treepred <- renderPrint({
        treePredictions()
    })
    
    observe({
        updateSelectInput(session, "predictdmg2")
    })
    observe({
        updateSelectInput(session, "predcateg2")
    })
    observe({
        updateSelectInput(session, "predpurp2")
    })
    observe({
        updateSelectInput(session, "predphase2")
    })
    observe({
        updateSelectInput(session, "predeng2")
    })
    observe({
        updateSelectInput(session, "predweath2")
    })
    observe({
        updateSelectInput(session, "predblt2")
    })
    observe({
        updateNumericInput(session, "predpass2")
    })
    observe({
        updateNumericInput(session, "predinj2")
    })

    
    
    # mathjax stuff
    output$math <- renderUI({
        withMathJax('$$\\mu=\\frac{\\exp(\\mathbf{X}\\boldsymbol{\\beta})}{1 + \\exp(\\mathbf{X}\\boldsymbol{\\beta})} = \\frac 1 {1 + \\exp(-\\mathbf{X} \\boldsymbol{\\beta})}$$')
    })
    
    
    
})

