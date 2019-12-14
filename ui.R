
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
                         menuItem("Response Variable", tabName="tabresp"),
                         menuItem("Numeric Predictors", tabName="tabnumvars"), 
                         # #include another 2nd level tab for bivariates acorss the dataset if time
                         menuItem("Categorical Predictors", tabName="tabcatvars")), 
                menuItem("Unsupervised Learning", tabName="tabunsup", 
                         menuItem("Clustering", tabName="tabcluster"),
                         menuItem("Dendrogram", tabName="tabdendro")),
                menuItem("Supervised Learning", tabName = "tabsuper",
                         menuItem("Logistic Regression Model", tabName="tabreg"),
                                  menuSubItem("Equation", tabName="math"),
                         menuItem("Classification Tree Model", tabName="tabtree")),
                menuItem("Give Me All the Data", tabName = "taball")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "tabstart",
                        fluidRow(
                            column(6,
                                   h2("About the data"),
                                   h4("This dataset is a subset of the National Transportation Safety Board's Aviation
                                   Accident Database. The original database contains information about civil aviation
                                   accidents and incidents that occurred within the United States from 1962 to present
                                   day. Due to the large number of records and variables in the original dataset, a subset
                                   of approximately 3,000 records and 10 variables are included with this application.
                                   This subset has been cleaned and pre-processed to facilitate data exploration and
                                   modeling. It focuses primarily on events involving airplanes or helicopters
                                   belonging to the United States. The target variable is Fatal, which indicates if at
                                   least one person perished from an event."),
                                   h4("More information about the original database can be found ",
                                   tags$a(href="https://catalog.data.gov/dataset/aviation-data-and-documentation-from-the-ntsb-accident-database-system", "here"),", and the full data dictionary can be found",tags$a(href="https://www.ntsb.gov/_layouts/ntsb.aviation/AviationDownloadDataDictionary.aspx", "here")),
                            ),
                            column(6,
                                   h2("What you can do with this application"),
                                   uiOutput("myList"),
                                   h4(HTML("<ul>
                                   <li>Perform univarite and bivariate exploratory analysis on the dataset variables</li>
                                   <li>Interact with and download plots</li>
                                   <li>Engage in an interactive supervised learning activity</li>
                                   <li>Fit two different supervised learning models and make predictions</li>
                                   <li>View and download the analysis dataset</li>
                                   </ul>"))
                            ),
                            column(12,
                                   h2("Abridged Data Dictionary"),
                                   h4("Below are descriptions of the variables included in this application. 
                                      Some variables are original to the larger database, and some have been 
                                      derived specifically for this application."),
                                   br(),
                                   tableOutput("dict")
                                   )
                        )
                ),
                # EDA:  Response Variable
                tabItem(tabName="tabresp",
                        titlePanel(h3("The Response Variable")),
                        fluidRow(
                            column(12,
                            titlePanel(h4("The response variable in this dataset is Fatal, which indicates 
                                          whether at least one passenger on the aircraft died as a result
                                          of the given event. It is a binary variable, which 
                                          equals 1 if the event resulted in at least one death and 0 if the
                                          event did not result in any death.")),
                            box(title="Distribution of Fatal",
                                tableOutput("fatal"),
                                plotOutput("plot13")),
                            )
                        )
                ),
                # EDA:  Categorical Predictors
                tabItem(tabName = "tabcatvars",
                        fluidRow(
                            column(6,
                                   varSelectInput("var1", "Select a variable to explore:", catSubset)),
                            column(6,
                                   tableOutput("table1"))
                        ),
                        fluidRow(
                            box(title="One-Way Distribution",
                                plotOutput("plot1"),
                                br(),
                            downloadButton('downloadPlot1','Download plot')),
                            box(title="Selected Predictor vs. Fatal Response",
                                plotOutput("plot2"),
                                br(),
                            downloadButton('downloadPlot2','Download plot'))
                        ),
                        # fluidRow(
                        #     column(12,
                        #            selectInput("bivar1", "Select a variable:", subset),
                        #            selectInput("bivar2", "Select another variable:", subset, selected=subset[2])),
                        #     box(title="All Bivariates",
                        #         plotOutput("bivarsall"))
                        # )
                ),
                # EDA: Numeric Predictors
                tabItem(tabName = "tabnumvars",
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
                        fluidRow(
                            box(title="Hover over a point in the plot to view the total number of passengers and
                                injuries in a given event",
                                verbatimTextOutput("info"),
                                plotOutput("bivarsnum",
                                           hover = "plot_hover"))
                        )
                ),
                # Clustering content
                tabItem(tabName = "tabcluster",
                        titlePanel("Interactive Clustering Plot for Total Passengers and Total Injuries"),
                        column(12,
                        numericInput('clusters', 'Select number of clusters',
                                     value=3, min = 1, max = 10),
                        numericInput('iteration', 'Select number of algorithm iterations',
                                     value=4, min = 4, max = 12)),
                        column(8,
                        plotOutput("plotclus")) 
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
                            column(12,(h5('Directions:  Select one or more indpendent variables to include in your model and
                                          click the "Fit model" button. Output will display to the right, and different output
                                          options are available. Options for running a prediction based on your model will also 
                                          display below after you run your model. Select your desired independent variable 
                                          attributes and click the "Run Predictions" button. To run a new model, 
                                          restart the entire application.')))
                        ),
                        br(),
                        fluidRow(
                            column(4,
                            selectInput('indvar', 'Select independent variable(s):', 
                                        predSubset, multiple=TRUE),
                            actionButton("go","Fit model"), br(),br(),br(),
                            selectInput('selectout', 'Select desired model output:', 
                                        choices=c("Model fit summary",
                                                  "ANOVA summary"))
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
                                             verbatimTextOutput("regpredout"))
                            )
                            ),
                ),
                tabItem(tabName="math",
                        h4("The mean function commonly used for binomial distributions like the response 
                           variable in the dataset used in this applicaiton."),
                        fluidRow(
                            box(withMathJax(),
                                uiOutput('math'))
                        )
                ),
                # Models:  Classification Tree Content
                tabItem(tabName = "tabtree",
                        h3("Fit a classification tree model based on your desired split type."),
                        fluidRow(
                            column(8,
                                   selectInput("split", "Select desired split type:",
                                               choices=c("deviance","gini"))
                            ),
                            column(6,
                                   titlePanel(h4("Root Mean Squared Error of Current Tree")),
                                   verbatimTextOutput("rmse")
                                   ),
                            column(12,
                                   titlePanel(h4("Tree plot")),
                                   plotOutput("treeplot")
                                   ),
                        ),
                            fluidRow(column(12,
                                            actionButton("predtree","Click here to predict with this model"))
                            ),
                            # Only show this panel if the "go" run model box is checked
                            fluidRow(
                                column(12,
                            conditionalPanel(
                                condition = "input.predtree == 1",
                                             h4("Make Customized Predictions Based on this Classification Tree"),
                                             br(),
                                             fluidRow(
                                                 column(4,
                                                        selectInput('predictdmg2', 'Select Aircraft Damage:', dmg, 
                                                                    selected=dmg[1]),
                                                        selectInput('predictcateg2', 'Select Aircraft Category', cat, 
                                                                    selected=cat[1]),
                                                        selectInput('predictpurp2', 'Select Purpose of Flight', purp, 
                                                                    selected=purp[1])),
                                                 column(4,
                                                        selectInput('predictphase2', 'Select Broad Phase of Flight', phase, 
                                                                    selected=phase[1]),
                                                        selectInput('predictweath2', 'Select Weather Condition', wthr, 
                                                                    selected=wthr[1]),
                                                        selectInput('predicteng2', 'Select Engine Count', eng, 
                                                                    selected=eng[1])),
                                                 column(4,
                                                        selectInput('predictblt2', 'Select Amateur Built Indicator', blt, 
                                                                    selected=blt[1]),
                                                        numericInput('predictinj2', 'Select number of Total Injuries', 
                                                                     value=0, min=0, max=50),
                                                        numericInput('predictpass2', 'Select number of Total Passengers', 
                                                                     value=5, min=1, max=50))
                                             ),
                                             actionButton("runtreepred","Run Predictions"), br(),
                                             verbatimTextOutput("treepred")
                                )
                                )
                            )
                ),
                # Fifth tab content
                tabItem(tabName = "taball",
                        fluidRow(
                            column(12, DT::dataTableOutput("datatable"))
                        ),
                        br(),
                        downloadButton('downloadData', 'Download dataset to a .csv file')
                )
            )
        )
    )
)

