
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
                         menuSubItem("One-Ways & Bivariates", tabName="onebivars"),
                         # menuSubItem("Bivariates - Target", tabName="bivarstgt"),
                         menuSubItem("Bivariates - Numerics", tabName="bivarsnum"),
                         menuSubItem("Cluster or PCA", tabName="unsuper"),
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
                                   "This dataset is a subset of the National Transportation Safety Board's Aviation Accident Database. The original database contains information about civil aviation accidents and incidents that occurred within the United States from 1962 to present day. Due to large number of records and variables in the original dataset, a subset of approximately 3,000 records and 10 variables are included with this application. This subset focuses on accidents or incidents by airplanes or helicopters that... The target variable is Fatal, which indicates if at least one person perished due to an accident or incident.",
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
                tabItem(tabName = "onebivars",
                        titlePanel(h2("Exploratory Data Analysis")),
                        fluidRow(
                            column(6,varSelectInput("var1", "Select a variable to explore:", data)),
                            column(6,tableOutput("table1"))
                        ),
                        fluidRow(
                            box(title="One-Way Distribution",
                                plotOutput("plot1")),
                            box(title="Selected Response Variable vs. Fatal Target Variable",
                                plotOutput("plot2"))
                        ),
                ),
                # SEcond tab content - third submenu
                tabItem(tabName = "bivarsnum",
                        fluidRow(
                            box(title=h3("Bivariate Plot of the Two Numeric Response Variables"),
                                plotOutput("bivarsnum"))
                        )
                ),
                # Third tab content
                tabItem(tabName = "unsup",
                        fluidRow(
                            box(textOutput("morestuff1"))
                        )
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
                        )
                )
            )
        )
    )
)