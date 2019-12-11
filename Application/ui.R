
library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(ggplot2)
library(gbm)
library(dplyr)
library(magrittr)
library(knitr)
library(RCurl)


shinyUI(
    dashboardPage(skin="yellow",
        dashboardHeader(title = "Modeling with NTSB Aviation Accident Data",
                        titleWidth = 450),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Start Here", tabName = "start"),
                # downloadButton('downloadData', 'Download'),
                menuItem("EDA", tabName = "eda"),
                         # menuSubItem("Bivariates - Target", tabName="bivarstgt"),
                         # numeric EDA, including tables/summaries of Fatal target variable
                         menuSubItem("Numeric Variables", tabName="numvars"),
                         # categorical EDA 
                         menuSubItem("Categorical Variables", tabName="catvars"),
                         menuSubItem("Clustering", tabName="unsuper"),
                menuItem("Models", tabName = "model"),
                         menuSubItem("Logistic Regression", tabName="log"),
                         menuSubItem("Classification Tree", tabName="tree"),
                menuItem("Give Me All the Data", tabName = "all")
            )
        ),
        dashboardBody(
            tabItems(
                # First tab content
                tabItem(tabName = "start",
                        fluidRow(
                            column(12,
                                   h2("About the data"),
                                   "This dataset is a subset of the National Transportation Safety Board's Aviation Accident Database. The original database contains information about civil aviation accidents and incidents that occurred within the United States from 1962 to present day. Due to large number of records and variables in the original dataset, a subset of approximately 3,000 records and 10 variables are included with this application. This subset has been cleaned and pre-processed to facilitate data exploration and modeling. It focuses primarily on accidents or incidents by airplanes or helicopters. The target variable is Fatal, which indicates if at least one person perished due to an aviation accident or incident.",
                                   br(),br(),
                                   "Below are descriptions of the variables included in this application:",
                                   br(),br()),
                            column(12,
                                   "More information about the original database can be found at",
                                   HTML("<a href=https://catalog.data.gov/dataset/aviation-data-and-documentation-from-the-ntsb-accident-database-system>here,</a>"),
                                   "and the full data dictionary can be found",
                                   HTML("<a href=https://www.ntsb.gov/_layouts/ntsb.aviation/AviationDownloadDataDictionary.aspx>here</a>."),
                                   br(),br(),
                                   h2("What this application does"),
                                   uiOutput("myList"),
                                   HTML("<ul>
                                   <li>Perform exploratory data analysis</li>
                                   <li>Create an unsupervised model</li>
                                   <li>Create some supervised models</li>
                                   <li>View, subset, and explore the data</li>
                                   </ul>")))
                        ),
                # Second tab content - first submenu
                tabItem(tabName = "catvars",
                        fluidRow(
                            column(6,varSelectInput("var1", "Select a variable to explore:", catSubset)),
                            column(6,tableOutput("table1")),
                        ),
                        fluidRow(
                            box(title="One-Way Distribution",
                                plotOutput("plot1"),
                                br(),
                            downloadButton('downloadPlot1','Download this plot')),
                            box(title="Selected Predictor vs. Fatal Response",
                                plotOutput("plot2"),
                                br(),
                            downloadButton('downloadPlot2','Download this plot')),
                        ),
                ),
                # Second tab content - third submenu
                tabItem(tabName = "numvars",
                        titlePanel(h3("The Response Variable")),
                        fluidRow(
                          box(title="Distribution of Fatal",
                              tableOutput("fatal")),
                          box(title="Numeric Summary of Fatal",
                              tableOutput("sumfatal"))
                        ),
                        titlePanel(h3("The Predictor Variables")),
                        fluidRow(
                            box(title="Distribution of Total Passengers Per Incident",
                                plotOutput("pass")),
                            box(title="Numeric Summary of Total Passengers",
                                tableOutput("sumpass")),
                        ),
                        fluidRow(
                            box(title="Distribution of Total Injuries Per Incident",
                                plotOutput("injur")),
                            box(title="Numeric Summary of Total Injuries",
                                tableOutput("suminjur")),
                        ),
                        titlePanel(h3("Scatterplot of the Two Numeric Predictors")),
                        fluidRow(
                            box(plotOutput("bivarsnum"))
                        )
                ),
                # Third tab content
                tabItem(tabName = "unsuper",
                        titlePanel("Interactive Clustering Plot for Total Passengers and Total Injuries"),
                        numericInput('clusters', 'Select number of clusters', 
                                     value = 3, min = 1, max = 10),
                        numericInput('iteration', 'Select number of algorithm iterations', 
                                     value = 3, min = 1, max = 10),
                        plotOutput("plotclus"),
                        # downloadButton('downloadPlot','Download cluster plot to .png file'),
                        titlePanel("Dendrogram for Total Passengers and Total Injuries"),
                        plotOutput("dendro")
                ),
                # Fourth tab content
                tabItem(tabName = "super",
                        fluidRow(
                            box(textOutput("morestuff2"))
                        )
                ),
                # Fifth tab content
                tabItem(tabName = "all",
                        fluidRow(
                            column(12,
                                   DT::dataTableOutput("datatable"))
                        ),
                        br(),
                        downloadButton('downloadData', 'Download entire modeling dataset to a .csv file')
                )
            )
        )
    )
)