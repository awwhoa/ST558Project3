
library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(ggplot2)
library(gbm)
library(dplyr)
library(magrittr)
library(knitr)

# runGitHub("ST558Project3", "awwhoa")

# output$downloadData <- downloadHandler(
#     filename = function() { paste(input$dataset, '.csv', sep='') },
#     content = function(file) {
#         write.csv(datasetInput(), file)
#     }
# )

shinyServer(function(input, output, session) {
    data <- as.data.frame(read_csv("https://raw.githubusercontent.com/awwhoa/ST558Project3/master/AnalysisDataset.csv"))
    
    output$myList <- renderUI(HTML("<ul><li>...text...</li><li>...more text...</li></ul>"))
    
    # datafile = "https://raw.githubusercontent.com/awwhoa/ST558Project3/master/AnalysisDataset.csv"
    # data = tbl_df(read.table(datafile,header=T,as.is=F,sep=",")) 
        
    # getData <- reactive({
    #     newData <- data %>% select(!!!input$variable)
    #     table(newData, dnn=!!!input$variable)
    # })

    #create output of observations    
    output$table <- renderTable(data)
    output$table

})

    # tryThis <- reactive({
    #     # newData <- data %>% dplyr::select(Aircraft.Damage)
    #     # table(newData, dnn="Aircraft.Damage")
    #     # summary(newData)
    #     
    #     # if(is.numeric(x)){
    #     #     summary(x)
    #     # } else if(is.factor(x)){
    #     #     table(x) %>% knitr::kable(col.names=c("Variable","Frequency"))
    #     # } else {
    #     #     break()
    # })
    # 
    # output$table <- renderTable({
    #     # table(getData(), dnn="Aircraft.Damage")
    #     # table(getData(), dnn=!input$variable)
    #     getData()
    # })

