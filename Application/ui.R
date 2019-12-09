
library(shiny)
library(shinydashboard)

dashboardPage(
    dashboardHeader(title = "NTSB Aviation Accidents"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Start Here", tabName = "start"),
            # downloadButton('downloadData', 'Download'),
            menuItem("EDA", tabName = "eda"),
            menuItem("Unsupervised Learning", tabName = "unsup"),
            menuItem("Supervised Learning", tabName = "super"),
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
                               "This dataset is a subset of the National Transportation Safety Board's Aviation Accident Database. The original database contains information about civil aviation accidents and incidents that occurred within the United States from 1962 to present day. Due to large number of records and variables in the original dataset, a subset of approximately 3,000 records and 10 variables are included with this application. This subset focuses on accidents or incidents by airplanes or helicopters that... More infromation about the original database can be found at",
                               HTML("<a href=https://catalog.data.gov/dataset/aviation-data-and-documentation-from-the-ntsb-accident-database-system>here</a>"),
                               ", and the full data dictionary can be found",
                               HTML("<a href=https://www.ntsb.gov/_layouts/ntsb.aviation/AviationDownloadDataDictionary.aspx>here</a>."),
                               br(),
                               "Below is a table of the structure/attributes of the data you will see here:"),
                        column(12,
                               h2("What this application does"),
                               uiOutput("myList"),
                               HTML("<ul>
                               <li>Perform exploratory data analysis</li>
                               <li>Create an unsupervised model</li>
                               <li>Create some supervised models</li>
                               <li>View, subset, and explore the data</li>
                               </ul>")))
                    ),
            # Second tab content
            tabItem(tabName = "eda",
                    fluidRow(
                        box(varSelectInput("variable", "Select a variable:", data)),
                        box(tableOutput("table"))
                        )
                    ),
            # Third tab content
            tabItem(tabName = "unsup",
                    fluidRow(
                        box(textOutput("more stuff"))
                    )
            ),
            # Fourth tab content
            tabItem(tabName = "super",
                    fluidRow(
                        box(textOutput("more stuff"))
                    )
            ),
            # Fifth tab content
            tabItem(tabName = "all",
                    fluidRow(
                        box(tableOutput("table"))
                    )
            )
        )
    )
)


# dashboardPage(
#     dashboardHeader(title = "Model NTSB Aviation Accident Data"),
#     dashboardSidebar(),
#     dashboardBody(
#         # Boxes need to be put in a row (or column)
#         fluidRow(
#             box(plotOutput("plot1", height = 250)),
#             
#             box(
#                 title = "Controls",
#                 sliderInput("slider", "Number of observations:", 1, 100, 50)
#             )
#         )
#     )
# )


