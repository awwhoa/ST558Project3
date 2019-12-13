
library(shiny)
library(shinydashboard)

shinyUI(
    dashboardPage(skin="yellow",
        dashboardHeader(title = "Modeling with NTSB Aviation Accident Data",
                        titleWidth = 450),
        dashboardSidebar(
            sidebarMenu(
                menuItem("About", tabName = "tabstart"),
                menuItem("EDA", tabName = "tabeda",
                         menuItem("Numeric Variables", tabName="tabnumvars"), # numeric one-ways
                         # categorical one-ways #include another 2nd level tab for bivariates acorss the dataset if time
                         menuItem("Categorical Variables", tabName="tabcatvars")), 
                menuItem("Unsupervised Learning", tabName="tabunsup", 
                         menuItem("Clustering", tabName="tabcluster"),
                         menuItem("Dendrogram", tabName="tabdendro")),
                menuItem("Supervised Learning Models", tabName = "tabsuper",
                         menuItem("Logistic Regression", tabName="tabreg"),
                                  # menuSubItem("Make predictions", tabName="tabpredict")),
                         menuItem("Classification Tree", tabName="tabtree")),
                menuItem("Give Me All the Data", tabName = "taball")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "tabstart",
                        fluidRow(
                            column(12,
                                   h2("About the data"),
                                   "This dataset is a subset of the National Transportation Safety Board's Aviation
                                   Accident Database. The original database contains information about civil aviation
                                   accidents and incidents that occurred within the United States from 1962 to present
                                   day. Due to large number of records and variables in the original dataset, a subset
                                   of approximately 3,000 records and 10 variables are included with this application.
                                   This subset has been cleaned and pre-processed to facilitate data exploration and
                                   modeling. It focuses primarily on accidents or incidents by airplanes or helicopters.
                                   The target variable is Fatal, which indicates if at least one person perished due to
                                   an aviation accident or incident.",
                                   br(),br(),
                                   "Below are descriptions of the variables included in this application:",
                                   br(),br()),
                            column(12,
                                   "More information about the original database can be found at",
                                   HTML("<a href=https://catalog.data.gov/dataset/aviation-data-and-documentation-from
                                        -the-ntsb-accident-database-system>here,</a>"),
                                   "and the full data dictionary can be found",
                                   HTML("<a href=https://www.ntsb.gov/_layouts/ntsb.aviation/AviationDownloadDataDictio
                                        nary.aspx>here</a>."),
                                   br(),br(),
                                   h2("What you can do with this application"),
                                   uiOutput("myList"),
                                   HTML("<ul>
                                   <li>Perform exploratory data analysis on the dataset variables</li>
                                   <ul>
                                   <li>8 categorical variables</il>
                                   <li>2 numeric variables</il>
                                   </ul>
                                   <li>Perform supervised learning activities</li>
                                   <li>Fit two different supervised learning models and make predictions</li>
                                   <li>View, subset, and download the analysis data</li>
                                   </ul>")))
                        ),
                # EDA:  Numeric Variables
                tabItem(tabName = "tabcatvars",
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
                # EDA: Categorical Variables
                tabItem(tabName = "tabnumvars",
                        titlePanel(h3("The Response Variable")),
                        fluidRow(
                          box(title="Distribution of Fatal",
                              tableOutput("fatal")),
                          box(title="Numeric Summary of Fatal",
                              tableOutput("sumfatal"))
                        ),
                        titlePanel(h3("The Numeric Predictors")),
                        fluidRow(
                            column(12,varSelectInput("numvar", "Select a variable to explore:", numPredSubset)),
                            box(title="Numeric Summary",
                                tableOutput("sum"))
                            ),
                        fluidRow(
                            box(title="Distribution of the Variable",
                                plotOutput("pass")),
                            box(title="Proportion of Fatal Incidents ",
                                plotOutput("propfatal"))
                            ),
                        titlePanel(h3("Scatterplot of Total Passengers and Total Injuries by Fatal")),
                        # titlePanel(h4("Hover over a point in the plot to view the total number of
                        #               passengers and number of injured passengers in a given accident")),
                        fluidRow(
                            box(title="Hover over a point in the plot to view the total number of passengers and
                                injuries in a given accident",
                                verbatimTextOutput("info"),
                                plotOutput("bivarsnum",hover = "hover"))
                        )
                ),
                # Clustering content
                tabItem(tabName = "tabcluster",
                        titlePanel("Interactive Clustering Plot for Total Passengers and Total Injuries"),
                        column(12,
                        numericInput('clusters', 'Select number of clusters',
                                     value = 3, min = 1, max = 10),
                        numericInput('iteration', 'Select number of algorithm iterations',
                                     value = 3, min = 3, max = 10)),
                        column(8,
                        plotOutput("plotclus")),  
                ),
                # Dendrogram content
                tabItem(tabName = "tabdendro",
                        titlePanel("Dendrogram for Total Passengers and Total Injuries"),
                        plotOutput("dendro")
                ),
                # Logistic Regression model fit and prediction content
                tabItem(tabName = "tabreg",
                        titlePanel("Build Your Own Logistic Regression Model to Predict Whether a Fatality Occurred
                                   Due to an Aviation Accident"),
                        fluidRow(
                            column(12,(h4("The logistic regression technique used to build this model uses a form of
                            the general binomial equation: ")))
                        ),
                        br(),
                        fluidRow(
                            column(4,
                            selectInput('indvar', 'Select independent variable(s):', 
                                        predSubset, multiple=TRUE),
                            actionButton("go","Fit model"), br(),br(),br(),
                            selectInput('selectout', 'Select desired model output:', 
                                        choices=c("Model fit summary",
                                                  "ANOVA summary")),
                            # checkboxInput('misclass', 'View misclassification rate for this model')
                            ),
                            # textOutput("misclassrate"),
                            # Only show this panel if the misclass box is checked
                            # conditionalPanel(condition = "input.misclass == 1",
                            #                  verbatimTextOutput("misclassrate")),
                            # ),
                            column(8,
                                   h4(tags$b("Selected model output:")),
                                   verbatimTextOutput("regoutput"))
                        ),
                # ),
                # # Regression Model prediction content
                # tabItem(tabName = "tabpredict",
                        fluidRow(
                            column(12,
                            # Only show this panel if the "go" run model box is checked
                            conditionalPanel(condition = "input.go == 1",
                                             h3("Make Customized Predictions Based on the Logistic 
                                   Regression Model You Just Created"),
                                             br(),
                                             fluidRow(
                                                 column(4,
                                                        selectInput('predictdmg', 'Select Aircraft Damage:', dmg, 
                                                                    selected=dmg[1]),
                                                        selectInput('predictcateg', 'Select Aircraft Category', cat, 
                                                                    selected=cat[1]),
                                                        selectInput('predictpurp', 'Select Purpose of Flight', purp, 
                                                                    selected=purp[1])),
                                                 column(4,
                                                        selectInput('predictphase', 'Select Broad Phase of Flight', phase, 
                                                                    selected=phase[1]),
                                                        selectInput('predictweath', 'Select Weather Condition', wthr, 
                                                                    selected=wthr[1]),
                                                        selectInput('predicteng', 'Select Engine Count', eng, 
                                                                    selected=eng[1])),
                                                 column(4,
                                                        selectInput('predictblt', 'Select Amateur Built Indicator', blt, 
                                                                    selected=blt[1]),
                                                        numericInput('predictinj', 'Select number of Total Injuries', 
                                                                     value=0, min=0, max=50),
                                                        numericInput('predictpass', 'Select number of Total Passengers', 
                                                                     value=5, min=1, max=50))
                                             ),
                                             actionButton("run","Run Predictions"), br(),
                                             verbatimTextOutput("regpredout")))
                            )
                            # box(actionButton("run","Run predictions"), br(),
                            #     textOutput("regpredout"))
                        # )
                ),
                # Models:  Classification Tree Content
                tabItem(tabName = "tabtree",
                        h3("Build Your Own Classification Tree Model"),
                        fluidRow(
                            column(4,
                                   numericInput('ntrees', 'Enter number of trees:', value=100))
                        )
                ),
                # Fifth tab content
                tabItem(tabName = "taball",
                        fluidRow(
                            column(12, DT::dataTableOutput("datatable"))
                        ),
                        br(),
                        downloadButton('downloadData', 'Download entire modeling dataset to a .csv file')
                )
            )
        )
    )
)
