# Instantiate the UI function
ui = navbarPage("DataViz",theme = shinytheme("cerulean"),
                tabPanel("Landing Page",
                         sidebarPanel(
                             actionButton("uploadFile", "Upload Data"),
                             actionButton("processButton", "Process Data"),
                             p(""),
                             p("Upload the data as a prepared CSV or Zip file, then press the 'Process Data' button to begin.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("A data table will generated; this is the processed data created from the cleaned data. For a reference to the cleaning and processing steps, look in the 'Further Information' tab.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("Note: searching within the table is computationally intensive. If you enter a search term, press enter once, and wait for the search to complete.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("Filtering the data (from the relevant tab) is much faster.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("The Data Integrity check gives various details on the completeness and cleanliness of the inputted data.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("Final note: depending on the power of your computer, various features may take a few seconds to load or render. In general, press a button and wait a few seconds before proceeding.",
                               style = "font-family: 'arial'; font-si30pt"),
                         ),
                         mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Table",dataTableOutput("processedTableToView")),
                                         tabPanel("Data Integrity",verbatimTextOutput("integrityTest_TextResults"),
                                                  p("Below is a table showing the Image and Object ID's of records that have NA in the Channel",
                                                   style = "font-family: 'arial'; font-si30pt"),
                                                  dataTableOutput("integrityTest_NAChannelTable"))
                                        )
                         )
                         ),
                tabPanel("Data Filtering",
                         sidebarPanel(
                             style = "position: fixed; height: 90vh; overflow-y: auto;",
                             actionButton("filterButton", "Filter Data"),
                             p(""),
                             p("Select the options for filtering then press \"Filter Data\".",
                              style = "font-family: 'arial'; font-si30pt"),
                             checkboxGroupInput(inputId = "cOI",h3("Categories")),
                             checkboxGroupInput(inputId = "chOI",h3("Channel")),
                             checkboxGroupInput(inputId = "sOOI",h3("Surpass Objects")),
                             checkboxGroupInput(inputId = "lDOI",h3("Treatment")),
                             checkboxGroupInput(inputId = "eOI",h3("Genotypes")),
                             checkboxGroupInput(inputId = "nROI",h3("Image File")),
                             actionLink("selectallimages","Select/Clear All Images"),
                         ),
                         mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Table",dataTableOutput("filteredTableToView"))
                                        )
                         )
                        ),
                tabPanel("1-D Plots",
                         sidebarPanel(
                             actionButton("plotRefined", "Generate Plots"),
                             actionButton("downloadRefinedPlots", "Download Plots"),
                             p(""),
                             actionButton("generatePlotParams", "Reset Parameters"),
                             selectInput(inputId = "catVariableForFill","Color by",choices = list("Filter Data First"="Image File"),selected = "Image File"),
                             selectInput(inputId = "catVariableForSplitting","Split by",choices = list("Filter Data First"="Genotype"),selected = "Genotype"),
                             selectInput(inputId = "singleConVariable","Continuous Variable",choices = list("Filter Data First"="NormSum"),selected = "NormSum"),
                             p(""),
                             h4("Parameters and Formatting"),
                             tabsetPanel(type = "tabs",
                                         tabPanel("Histo",
                                                  fluidRow(
                                                      column(
                                                          width = 6,
                                                          div(style = "white-space: nowrap;", 
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("xLLHistogram",label="X Min",value=0)
                                                                 ),
                                                              h3("-",style="display:inline-block"),
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("xULHistogram",label="X Max",value=1)
                                                                 ),
                                                             ))),
                                                  numericInput("numOfBinsRefined","# of Bins",value="30")
                                                 ),
                                         tabPanel("Density",
                                                  fluidRow(
                                                      column(
                                                          width = 6,
                                                          div(style = "white-space: nowrap;", 
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("xLLKDE",label="X Min",value=0)
                                                                 ),
                                                              h3("-",style="display:inline-block"),
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("xULKDE",label="X Max",value=1)
                                                                 ),
                                                             ))),
                                                  fluidRow(
                                                      column(
                                                          width = 6,
                                                          div(style = "white-space: nowrap;", 
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("yLLKDE",label="Y Min",value=0)
                                                                 ),
                                                              h3("-",style="display:inline-block"),
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("yULKDE",label="Y Max",value=1)
                                                                 ),
                                                             ))),
                                                 ),
                                         tabPanel("Boxplots",
                                                  fluidRow(
                                                      column(
                                                          width = 6,
                                                          div(style = "white-space: nowrap;", 
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("yLLBoxplot",label="Y Min",value=0)
                                                                 ),
                                                              h3("-",style="display:inline-block"),
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("yULBoxplot",label="Y Max",value=1)
                                                                 ),
                                                             ))),
                                         ),
                                         tabPanel("Layout",
                                                  numericInput("numColumns","# of Plot Columns in Layout",value=2,min=1,max=100,step=1),
                                                  sliderInput("plotHeight",label="Plot height",ticks=FALSE,min=200,max=2000,value=600,step=5)
                                                 ),
                                         tabPanel("Themes",
                                                  selectInput("theme",label=h4("Select theme for plots"),choices=names(themesForPlotting),selected=names(themesForPlotting)[4])
                                         ),
                                         tabPanel("Colors",
                                                  selectInput("chosenPalette",label=h4("Select color palette"),choices=row.names(colorDF),selected="Custom"),
                                                  textInput("hexStrings",label="Optionally edit the colors",value=toString(colorDF["Custom","hexcodes"][[1]]))
                                                 )
                                        )
                         ),
                         mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Directions",
                                                  p(""),
                                                  p("1. Select variables."),
                                                  p("2. Press 'Reset Parameters'"),
                                                  p("3. Press 'Generate Plots' and customize parameters."),
                                                  p("4. Edit the colors (or choose a new palette via the 'Plot Themes and Colors' tab."),
                                                  p("5. Investigate Outliers."),
                                                  p("6. If you refilter the data, follow the same steps 1-5.")
                                                 ),
                                         tabPanel("Histograms",
                                                  plotOutput("histoRefined")),
                                         tabPanel("Density Plots",
                                                  plotOutput("kdeRefined"),
                                                  plotOutput("kdeRefinedSplit")),
                                         tabPanel("Boxplots",
                                                  plotOutput("boxplotRefined")),
                                         tabPanel("Outliers",
                                                  h3("Outliers for Selected Level"),
                                                  selectInput(inputId="outlierLevel",h4("Category Level"),choices = list("Generate Plots First"="Genotype"),selected = "Genotype"),
                                                  actionButton("generateOutliers","Generate Outliers for Selected Variable"),
                                                  verbatimTextOutput("outlierText"),
                                                  dataTableOutput("outlierTable")
                                                 )
                                        )
                         )
                        ),
                tabPanel("2-D Plots",
                         sidebarPanel(
                             actionButton("plotScatter", "Generate Scatterplot"),
                             actionButton("downloadScatter", "Download Scatterplot"),
                             p(""),
                             actionButton("scatterParams", "Reset Parameters"),
                             # Input the X and Y variables of interest plus the categorical variables of interest
                             selectInput(inputId = "scatterX","X Variable",choices = list("Filter Data First"="NormMean"),selected = "NormMean"),
                             selectInput(inputId = "scatterY","Y Variable",choices = list("Filter Data First"="NormStDev"),selected = "NormStDev"),
                             selectInput(inputId = "scatterCatColor","Categorical Variable for Color",choices = list("Filter Data First"="Treatment"),selected = "Treatment"),
                             selectInput(inputId = "scatterCatFacet","Categorical Variable for Splitting",choices = list("Filter Data First"="Treatment"),selected = "Image File"),
                             tabsetPanel(type = "tabs",
                                         tabPanel("Parameters",
                                             fluidRow(
                                                 column(
                                                     width = 6,
                                                     div(style = "white-space: nowrap;", 
                                                         div(style="display: inline-block; width: 100%;",
                                                             numericInput("xLLScatter",label="X Min",value=0)
                                                            ),
                                                         h3("-",style="display:inline-block"),
                                                         div(style="display: inline-block; width: 100%;",
                                                             numericInput("xULScatter",label="X Max",value=1)
                                                            ),
                                                        ))),
                                             fluidRow(
                                                 column(
                                                     width = 6,
                                                     div(style = "white-space: nowrap;", 
                                                         div(style="display: inline-block; width: 100%;",
                                                             numericInput("yLLScatter",label="Y Min",value=0)
                                                            ),
                                                         h3("-",style="display:inline-block"),
                                                         div(style="display: inline-block; width: 100%;",
                                                             numericInput("yULScatter",label="Y Max",value=1)
                                                            ),
                                                        )))
                                         ),
                                         tabPanel("Layout",
                                                  numericInput("scatterNumColumns","# of Plot Columns in Layout",value=2,min=1,max=100,step=1),
                                                  sliderInput("scatterplotHeight",label="Plot height",ticks=FALSE,min=200,max=2000,value=600,step=5)
                                         ),
                                         tabPanel("Themes",
                                                  selectInput("scatterplotTheme",label=h4("Select theme for plots"),choices=names(themesForPlotting),selected=names(themesForPlotting)[4])
                                         ),
                                         tabPanel("Colors",
                                                  selectInput("scatterplotChosenPalette",label=h4("Select color palette"),choices=row.names(colorDF),selected="Custom"),
                                                  textInput("scatterplotHexStrings",label="Optionally edit the colors",value=toString(colorDF["Custom","hexcodes"][[1]]))
                                                 )
                                         
                             )
                         ),
                         mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Directions",
                                                  p("1. Choose x and y variables."),
                                                  p("2. Choose categorical variables for color and splitting"),
                                                  p("'Reset Parameters' will update plotting parameters."),
                                         ),
                                         tabPanel("Scatter Plot",
                                                  plotOutput("scatter")
                                                 ),
                                         tabPanel("Scatter Plot Data Table",
                                                  dataTableOutput("scatterplotTable")
                                                 )
                                        )
                         )
                        ),
                tabPanel("Data Export",
                         sidebarPanel(
                             actionButton("selectColumnsButton", "Select"),
                             p("Choose the variables (columns) to export then press 'Select'."),
                             p("The filtered dataset created in the ‘Filtering’ tab will be exported with the selected variables."),
                             p("Browse through the different tabs to review the available formats for download and the summaries of the data."),
                             actionLink("selectallcolumns","Select/Clear All Columns"),
                             checkboxGroupInput(inputId = "exportColumns",h3("Columns to Export"),selected=NULL),
                         ),
                         mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Data Table",dataTableOutput("dataToExportTableToView"),downloadButton("downloadData","Download")),
                                         tabPanel("Data Integrity",h1("Numeric Data"),downloadButton("downloadSummaryNumericRData","Download as RData"),
                                                  downloadButton("downloadSummaryNumericCSV","Download as CSV"),
                                                  dataTableOutput("skimrOutputNumeric"),h1("Categorical Data Summary"),downloadButton("downloadSummaryCategoricalRData","Download as RData"),
                                                  downloadButton("downloadSummaryCategoricalCSV","Download as CSV"),
                                                  dataTableOutput("skimrOutputCategorical")),
                                         tabPanel("HTML Strings",
                                                  h4("Directions"),
                                                  p("Copy and paste the HTML strings below in a blank text document, then save it with the '.html' extension to recreate the integrity tables locally."),
                                                  h4("Numeric Data HTML"),
                                                  verbatimTextOutput("skimrOutputNumericText"),
                                                  h4("Categorical Data HTML"),
                                                  verbatimTextOutput("skimrOutputCategoricalText"))
                                        )
                         )
                        ),
                tabPanel("FAQs",
                         mainPanel(
                             h4("What is involved in the 'Data Cleaning' step?"),
                             p("The data cleaning step refers to taking the raw data from Imaris, which is formatted within a zipped directory structure, and harmonizing it into a single outputted CSV file/table. The cleaningFunction.R script shows all cleaning steps in code with paired annotations. Of note: the directory structure of the raw data, in addition to the directory names, must be specified according to the example .zip file for the cleaning process to work properly. This step will only occur when the data is loaded as a prepared .zip file; if a CSV is uploaded, the data is sent directly to the Processing function."),
                             h4("What is involved in the 'Processing' step?"),
                             p("The data processing step refers to taking the inputted CSV/table (either the result of the cleaning function or a CSV table formatted to the same schema) and adding new variables of interest to the dataset; e.g., the normalized variables. The processingFunction.R script shows all processing steps in code with paired annotations. The processing step is cued by the 'Process Data' button on the Landing Page."),
                             h4("What is the 'Data Integrity' check on the Landing Page?"),
                                 p("The 'Data Integrity' check on the landing page refers to a specific set of tests performed on the processed data. It is currently coded within the 'server.R' script. Currenty, the test includes checking for images that have miscoded Surpass Object values (e.g., mispelle strings), checking if images are missing nucleus center of mass and nucleus objects, and identifying columns with values that are totally missing. The accepted spelling of Surpass Object types are hard-coded within the server.R script."),
                             h4("Acknowledgements"),
                             p("This tool was made possible by funding and support from Dr. Célia Baroux. Conceptual development involved Dr. Célia Baroux, Devin Routh, and Dr. Philip Shemella. Principal Software Development involved Devin Routh and Dr. Philip Shemella."),
                             h4("Affiliations"),
                             HTML("<p> Dr. Célia Baroux - <a href='https://www.botinst.uzh.ch/en/research/development/celiabaroux.html' target='_blank'> Department of Plant and Microbial Biology at the University of Zürich</a><br>Devin Routh and Philip Shemella - <a href='https://www.zi.uzh.ch/en/teaching-and-research/science-it.html' target='_blank'> S<sup>3</sup>IT at the University of Zürich </a></p>"),
                             h4("Color Brewer Available Palettes"),
                             plotOutput("rColorBrewer",height="600px")
                         )
                        )
               )