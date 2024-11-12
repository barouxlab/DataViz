# Instantiate the UI function
ui = navbarPage("DataViz",theme = shinytheme("cerulean"),
                header = tagList(add_busy_spinner(spin = "fading-circle", color = "#FFF", margins = c(0, 10))),
                tabPanel("Getting Started",
                         mainPanel(
                             h2("Getting Started"),
                             p(""),
                             p("1. Begin at the ‘Import’ Tab, select the file to upload and press the 'Import Data' button, then wait until the table is displayed.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("2. There is the possibility to combine and normalize variables, see the ‘Process’ Tab.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("3. Before plotting select variables and categories of interest in the ‘Select’ Tab.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("4. Generate plots and explore your selected data with the tabs ‘1-D Plots’ and ‘2-D Plots’. Read the instructions in the ‘Directions’ sub-tab.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("5. Plots can be downloaded in their respective tab, Data can be exported in diverse formats under the ‘Data Export’ tab.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("6. Detailed information or common questions are listed in the FAQ.",
                               style = "font-family: 'arial'; font-si30pt"),
                             p("For requests and suggestions please write to : cbaroux@botinst.uzh.ch.",
                               style = "font-family: 'arial'; font-si30pt")
                        )
                         ),
                tabPanel("Import",
                         sidebarPanel(
                             fileInput("inputFile", "Upload a Zip or CSV file", accept = c(".zip",".csv")),
                             actionButton("confirmUpload", "Import Data"),
                             downloadButton("quickDownload", "Download Data"),
                             p(""),
                             p("Press the \"Import Data\" button after selecting/uploading your file of interest."),
                             p(""),
                             p("Either a zipped file of original Imaris tables can be selected or a CSV of pre-processed data."),
                             p(""),
                             p("Note: because Zip files are cleaned upon import they will take longer to appear. CSV file imports will be much faster."),
                             p(""),
                             p("Recommended workflow: upload your Zip file and allow it to be cleaned during the import process, then immediately download the data using the \"Download Data\" button. You can then re-upload the CSV very quickly (bypassing the cleaning pipeline).")
                         ),
                         mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Table",dataTableOutput("cleanedTableToView")),
                                         tabPanel("Data Integrity",
                                                  verbatimTextOutput("integrityTest_TextResults"),
                                                  p("Below is a table showing the Image and Object ID's of records that have NA in the Channel",
                                                   style = "font-family: 'arial'; font-si30pt"),
                                                  dataTableOutput("integrityTest_NAChannelTable"),
                                                 p("These files showed no data (or little data)"),
                                                 verbatimTextOutput("problematicFilesList"))
                                        )
                         )
                        ),
                tabPanel("Process",
                         sidebarPanel(
                             actionButton("processButton", "Process Data"),
                             downloadButton("quickProcDownload", "Download Data"),
                             p(""),
                             p("Data processing is an optional step that adds normalized variables such as: Intensity sum, mean, and stdev normalised per nucleus; Normalized shortest distance to nucleus; Normalised intensity ratios (chX:chY); Group intensity sum, mean, and stdev; Signal density (intensity sum normalised per nucleus; volume)."),
                             p("After pressing the \"Process Data\" button, the data will appear in a table when the processing pipeline has finished. You will then be able to select the processed version of the data within the \"Filtering\" tab."),
           p("See the FAQ section for more details."),
                             checkboxGroupInput("varsToCreate",h3("Optional Variables"),choices=processVarsOptions),
                             actionLink("selectallvars","Select/Clear All Variables"),
                             checkboxGroupInput("ratioSumsToCreate",h4("Ratio Variables - Normalized Sum")),
                             actionLink("selectRatioSums","Select/Clear All Sum Ratios"),
                             checkboxGroupInput("ratioSumsPerGroupToCreate",h4("Ratio Variables - Normalized Sum per Group")),
                             actionLink("selectRatioSumsPerGroup","Select/Clear All Sum per Group Ratios")
                             #checkboxGroupInput("ratioMeansToCreate",h4("Ratio Variables - Normalized Mean")),
                             #actionLink("selectRatioMeans","Select/Clear All Mean Ratios")
                         ),
                         mainPanel(
                             tabPanel("Table",dataTableOutput("processedTableToView"))
                         )
                        ),
                tabPanel("Select",
                         sidebarPanel(
                             actionButton("filterButton", "Select Data"),
                             downloadButton("selectedDQD","Download"),
                             p(""),
                             p("Select the options for filtering then press \"Select Data\"."),
                             p("Note: there is the possibility to plot data per treatment separately."),
                             p(""),
                             radioButtons(inputId = "dataToSelect",
                                          h3("Dataset for Selection"),
                                          c("Original Data"="rawData"),
                                          selected = "rawData"
                                         ),
                             checkboxGroupInput(inputId = "chOI",h3("Channel")),
                             checkboxGroupInput(inputId = "sOOI",h3("Object (Type)")),
                             checkboxGroupInput(inputId = "lDOI",h3("Treatment")),
                             checkboxGroupInput(inputId = "eOI",h3("Genotypes")),
                             checkboxGroupInput(inputId = "nROI",h3("Image File")),
                             actionLink("selectallimages","Select/Clear All Images"),
                             checkboxGroupInput(inputId = "exportVariables",h3("Variables"),selected=NULL),
                             actionLink("selectallvariables","Select/Clear All Variables")
                         ),
                         mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Table",
                                                  dataTableOutput("filteredTableToView")),
                                         tabPanel("Outliers",
                                                  h3("Outliers for Selected Level"),
                                                  selectInput(inputId="outlierVariable",h4("Category Level"),choices = list("Filter Data First"="Genotype"),selected = "Genotype"),
                                                  radioButtons(inputId = "removeOutliersRadio",
                                                               h3("Remove outliers on next filter?"),
                                                               c("Yes"="yes","No"="no"),
                                                               selected = "no"
                                                              ),
                                                  actionButton("generateOutliers","Generate Outliers for Selected Variable"),
                                                  p(""),
                                                  verbatimTextOutput("outlierText"),
                                                  p(""),
                                                  dataTableOutput("outlierTable"),
                                                  HTML("<h4 style=\"margin-top: 25px\">Outliers</h4>"),
                                                  HTML("<p>Method <code>boxplot.stats()</code> is used to compute the outliers.</p>"),
                                                  HTML("<p>Outliers are the values of any data points which lie beyond the extremes of the whiskers."),
                                                  HTML("<p>Lower whisker = smallest observation greater than or equal to lower hinge - 1.5 * IQR, where lower hinge is 25% quantile.</p>"),
                                                  HTML("<p>Upper whisker = largest observation less than or equal to upper hinger + 1.5 * IQR, where upper hinge is 75% quantile.</p>"),
                                                  HTML("<p>Outlier is a data point less than a lower whisker or a data point greater than an upper whisker.</p>")
                                                 )
                                        )
                         )
                        ),
                                                         tabPanel("Filtering / Binning",
                                                                  sidebarPanel(
                                                                      h2("Filtering"),
                                                  selectInput(inputId = "additionalVarForFiltering","Filter by",choices = list("Select Variable"="Genotype"),selected = "Genotype"),
                                                  fluidRow(
                                                      column(
                                                          width = 6,
                                                          div(style = "white-space: nowrap;", 
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("additionalFilterLower",label="Lower Limit (Inclusive)",value=0)
                                                                 ),
                                                              h3("-",style="display:inline-block"),
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("additionalFilterUpper",label="Upper Limit (Inclusive)",value=1)
                                                                 ),
                                                             ))),
                                                  actionButton("additionalFilter", "Apply Filter"),
                                                  actionButton("cancelFilter", "Cancel Filter"),
                                                                      h2("Binning"),
                                                                      tabPanel("Subset",
                                                  p("Subset the data into groups defined by thresholds"),
                                                  selectInput(inputId = "binningVariable","Variable for Group Creation",choices = list("Select Data First"="NormSum"),selected = "NormSum"),
                                                  radioButtons(inputId = "rangeOrGroups",
                                                               p("Create groups by thresholds or subset by a single range"),
                                                               c("Thresholds (e.g., '0,1,5,10')"="threshold","Range (e.g., '1,5')"="range"),
                                                               selected = "threshold"
                                                              ),
                                                  textInput("binCuts","",value="0,1"),
                                                  actionButton("addBins", "Create Bins/Groups"),
                                                  actionButton("removeBins","Clear Bins/Groups")
                                         )
                                                                      ),
                                                                  mainPanel(
                                                                      h4("Filtered / Binned Data will be displayed below:"),
                                                  dataTableOutput("filteredDatasetForFilterTab")
                                                                  )
                                                 ),
                tabPanel("1-D Plots",
                         sidebarPanel(
                             actionButton("generatePlotParams", "Update Parameters"),
                             actionButton("plotRefined", "Generate Plots"),
                             actionButton("downloadRefinedPlots", "Download Plots"),
                             downloadButton("downloadAdditionalFilteredData", "Download Data"),
                             p(""),
                             selectInput(inputId = "singleConVariable","Select Variable",choices = list("Select Data First"="NormSum"),selected = "NormSum"),
                             selectInput(inputId = "catVariableForFill","Color by",choices = list("Select Data First"="Image File"),selected = "Image File"),
                             selectInput(inputId = "catVariableForSplitting","Split by",choices = list("Select Data First"="Genotype"),selected = "Genotype"),
                             p(""),
                             checkboxInput("kruskallwallisCheckbox",label="Kruskall-Wallis test",value = FALSE),
                             checkboxInput("anovaCheckbox",label="ANOVA test",value = FALSE),
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
                                                  fluidRow(
                                                      column(
                                                          width = 6,
                                                          div(style = "white-space: nowrap;", 
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("kdeAdjust",label="KDE Smoothing Adjust",value=1,min=0,step=0.02)
                                                                 )
                                                             ))),
                                                  fluidRow(
                                                      column(
                                                          width = 6,
                                                          div(style = "white-space: nowrap;", 
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("kdeNumOfBinsRefined",label="# of Bins for % KDE",value=30)
                                                                 )
                                                             )))
                                                 ),
                                         tabPanel("Box/Vio",
                                                  shinyjs::useShinyjs(), 
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
                                                  radioButtons(inputId="boxplotYScale",label="Y scale",choices=c("Linear"="linearY","Log"="logY"),selected="linearY",inline=TRUE),
                                                  fluidRow(
                                                      column(
                                                          width = 6,
                                                          div(style = "white-space: nowrap;", 
                                                              div(style="display: inline-block; width: 100%;",
                                                                  numericInput("boxplotBoxWidth",label="Box Width",value=0.5,step=0.02)
                                                                 )
                                                             ))),
                                                  selectInput(inputId = "boxplotDistribution","Data points",
                                                              choices = list("none"="list","default"="geom_point","jitter"="geom_jitter"),
                                                              selected = "none"),
                                                  fluidRow(
                                                    column(
                                                      width = 6,
                                                      div(style = "white-space: nowrap;", 
                                                          div(style="display: inline-block; width: 100%;",
                                                              numericInput("boxplotPointSize",label="Size",value=0.1,step=0.1)
                                                          ),
                                                          div(style="display: inline-block; width: 100%;",
                                                              numericInput("boxplotPointTransparency",label="Transparency",value=0.5,step=0.1,min=0,max=1)
                                                          ),
                                                      )))
                                         ),
                                         tabPanel("Layout",
                                                  numericInput("numColumns","# of Plot Columns in Layout",value=2,min=1,max=100,step=1),
                                                  sliderInput("plotHeight",label="Plot height",ticks=FALSE,min=50,max=1500,value=400,step=5)
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
                         mainPanel(style = "overflow-y: auto;",
                             tabsetPanel(type = "tabs",
                                         id = "tabs1Dplots",
                                         tabPanel("Directions",
                                                  p(""),
                                                  p("1. Requirements: data to plot have been selected using the Select tab
"),
                                                  p("2. Choose the variables to plot, to color and split by"),
                                                  p("3. Press 'Update Parameters'"),
                                                  p("4. Press 'Generate Plots'"),
                                                  p("5. Customize the axes limits in the sub-tabs for each plot type"),
                                                  p("6. Customize the colors and background via the 'Layout', 'Themes' and 'Colors' tabs"),
                                                  p("7. Download the data."),
                                                  p("Note: If you filter or bin the data, the download button will provide you with this data.")
                                                 ),
                                         tabPanel("Histograms",
                                                  tabsetPanel(type = "tabs",
                                                              tabPanel("Histogram by Counts",
                                                                       plotOutput("histoRefined",height="auto")),
                                                              tabPanel("Histogram by Percent",
                                                                       plotOutput("histoRefinedPercentage",height="auto"))
                                                              )
                                                  ),
                                         tabPanel("Density Plots",
                                                  tabsetPanel(type = "tabs",
                                                              tabPanel("KDE by Density",
                                                                       plotOutput("kdeRefined",height="auto")),
                                                              tabPanel("KDE by Percent",
                                                                       plotOutput("kdeRefinedPercentage",height="auto"))
                                                              )
                                                  ),
                                         tabPanel("Boxplots",
                                                  plotOutput("boxplotRefined",height="auto")),
                                         tabPanel("Boxplot Stats",
                                                  dataTableOutput("summaryTableFromBoxplot"),
                                                  h4("Boxplot statistics"),
                                                  p("The lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles). This differs slightly from the method used by the boxplot() function, and may be apparent with small samples. See boxplot.stats() for for more information on how hinge positions are calculated for boxplot()."),
                                                  p('The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called "outlying" points and are plotted individually.'),
                                                  p("In a notched box plot, the notches extend 1.58 * IQR / sqrt(n). This gives a roughly 95% confidence interval for comparing medians. See McGill et al. (1978) for more details."),
                                                  h4("Computed variables"),
                                                  HTML("<p><b>width</b>: width of boxplot</p>"),
                                                  HTML("<p><b>ymin</b>: lower whisker = smallest observation greater than or equal to lower hinge - 1.5 * IQR</p>"),
                                                  HTML("<p><b>lower</b>: lower hinge, 25% quantile</p>"),
                                                  HTML("<p><b>notchlower</b>: lower edge of notch = median - 1.58 * IQR / sqrt(n)</p>"),
                                                  HTML("<p><b>middle</b>: median, 50% quantile</p>"),
                                                  HTML("<p><b>mean</b>: mean, average of all data points</p>"),
                                                  HTML("<p><b>notchupper</b>: upper edge of notch = median + 1.58 * IQR / sqrt(n)</p>"),
                                                  HTML("<p><b>upper</b>: upper hinge, 75% quantile</p>"),
                                                  HTML("<p><b>ymax</b>: upper whisker = largest observation less than or equal to upper hinge + 1.5 * IQR</p>"),
                                                  HTML("<p>(All information above is copied directly from <code>?stat_boxplot</code>)</p>"),
                                                 ),
                                         tabPanel("Violin Plots",
                                                  plotOutput("violinplotRefined",height="auto")
                                                  ),
                                         tabPanel("Kruskall-Wallis Stats Report",
                                                  dataTableOutput("kwTable1D")
                                         ),
                                         tabPanel("ANOVA Stats Report",
                                                  dataTableOutput("anovaTable1D")
                                         )
                                        )
                         )
                        ),
                tabPanel("2-D Plots",
                         sidebarPanel(
                             actionButton("scatterParams", "Update Parameters"),
                             actionButton("plotScatter", "Generate Scatterplot"),
                             actionButton("downloadScatter", "Download Scatterplot"),
                             downloadButton("downloadScatterplotData", "Download Data"),
                             p(""),
                             # Input the X and Y variables of interest plus the categorical variables of interest
                             selectInput(inputId = "scatterX","X Variable",choices = list("Filter Data First"="NormMean"),selected = "NormMean"),
                             selectInput(inputId = "scatterY","Y Variable",choices = list("Filter Data First"="NormStDev"),selected = "NormStDev"),
                             selectInput(inputId = "scatterCatFacet","Categorical Variable for Splitting",choices = list("Filter Data First"="Treatment"),selected = "Image File"),
                             checkboxInput("contourCheckbox",label="Add contours?",value = TRUE),
                             uiOutput("scatterColorSelect"),
                             tabsetPanel(type = "tabs",
                                         tabPanel("X-Y Parameters",
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
                                                        ))),
                                             radioButtons(inputId="scatterplotXScale",label="X scale",choices=c("Linear"="linearX","Log"="logX"),selected="linearX",inline=TRUE),
                                             radioButtons(inputId="scatterplotYScale",label="Y scale",choices=c("Linear"="linearY","Log"="logY"),selected="linearY",inline=TRUE),
                                             checkboxInput("scatterplotPearsonCheckbox",label="Pearson correlation",value = FALSE)
                                         ),
                                         tabPanel("Layout",
                                                  numericInput("scatterNumColumns","# of Plot Columns in Layout",value=2,min=1,max=100,step=1),
                                                  sliderInput("scatterplotHeight",label="Plot height",ticks=FALSE,min=200,max=2000,value=600,step=5)
                                         ),
                                         tabPanel("Themes",
                                                  selectInput("scatterplotTheme",label=h4("Select theme for plots"),choices=names(themesForPlotting),selected=names(themesForPlotting)[4])
                                         ),
                                         tabPanel("Colors",
                                                  selectInput("contourColor",label="Contour color",choices=c("viridis","gradient"),selected="viridis"),
                                                  selectInput("scatterplotChosenPalette",label="Colors for non-contour scatter",choices=row.names(colorDF),selected="Custom"),
                                                  textInput("scatterplotHexStrings",label="Optionally edit the non-contour colors",value=toString(colorDF["Custom","hexcodes"][[1]]))
                                                 ),
                                         tabPanel("Transparency",
                                                  numericInput("scatterplotTransparency",label="Alpha",value=0.9,min=0,max=1,step=0.05)
                                         )
                             )
                         ),
                         mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Directions",
                                                  p("1. Choose x and y variables."),
                                                  p("2. Choose categorical variables for color and splitting."),
                                                  p("3. Press 'Update Parameters' then 'Generate Scatterplot'."),
                                                  p("4. Customize the x and y axis limits in the 'Parameters' tab."),
                                                  p("5. Customize the number, layout, and size of plots via the 'Layout' tab."),
                                                  p("6. Customize the color palette and background color via the 'Colors' and 'Themes' tabs."),
                                                  p("7.  Explore your data by choosing different x and y variables, select 'Update Parameters' and 'Generate plots' each time a new variable is chosen, this will update the plotting parameters.
"),
                                                  p("Note: the 'Update Parameters' button will provide you with plotting parameters that are updated for your current data selection. Use this to guide your initial parameter set (e.g., x and y axes limits.)"),
                                         ),
                                         tabPanel("Scatter Plot",
                                                  plotOutput("scatter")
                                                 ),
                                         tabPanel("Scatter Plot Data Table",
                                                  dataTableOutput("scatterplotTable")
                                                 ),
                                         tabPanel("Statistic Report",
                                                  dataTableOutput("summaryTableFromScatterplot")
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
                             p("The data cleaning step refers to taking the original data from Imaris, which is formatted within a zipped directory structure, and harmonizing it into a single outputted CSV file/table. The cleaningFunction.R script shows all cleaning steps in code with paired annotations. Of note: the directory structure of the original data, in addition to the directory names, must be specified according to the example .zip file for the cleaning process to work properly. This step will only occur when the data is loaded as a prepared .zip file; if a CSV is uploaded, the data is sent directly to the Processing function."),
                             h4("What is the 'Data Integrity' check on the Landing Page?"),
                             p("The 'Data Integrity' check on the landing page refers to a specific set of tests performed on the processed data. It is currently coded within the 'server.R' script. Currenty, the test includes checking for images that have miscoded Object values (e.g., mispelle strings), checking if images are missing nucleus center of mass and nucleus objects, and identifying columns with values that are totally missing. The accepted spelling of Object types are hard-coded within the server.R script."),
                             h4("What is involved in the 'Processing' step?"),
                             HTML("<p>The data processing step refers to taking the inputted CSV/table (either the result of the cleaning function or a CSV table formatted to the same schema) and adding new variables of interest to the dataset; e.g., the normalized variables. The processingFunction.R script shows all processing steps in code with paired annotations. The processing step is cued by the 'Process Data' button on the Landing Page. Optional Variables are explained <a href='https://github.com/barouxlab/DataViz/blob/main/Optional%20Variables' target='_blank'>here</a>. Examples of applications are also provided.</p>"),
                             h4("What are the 'Filtering/Binning' options?"),
                             HTML("<p><b>Filtering:</b> Select a subset of data based on a range of values for one variable. This selection can be used to create specific plots and can be reverted to the original DataFrame by clicking the 'Cancel Filter' button.</p>"),
                             HTML("<p><b>Binning:</b> Divide the data into groups defined by ranges of values for one variable. The groups can be defined using thresholds or by setting upper and lower limits. This selection can be used to create specific plots, utilizing the 'Groups' category for plot splitting or coloring, and can be reverted to the original DataFrame by clicking the 'Clear Bins/Groups' button.</p>"),
                             h4("Subsetting a Dataframe with Data Export"),
                             p("The raw or processed DataFrame can be downloaded in full or as a subset of data—simply select the columns to be exported. The subsetted dataset can be re-imported later into DataViz or used for further analysis in third-party software."),
                             h4("Where can I get help"),
                             HTML("<p>A guideline with examples and a file explaining the Optional Variables and their use are available at <a href='https://github.com/barouxlab/DataViz/tree/main' target='_blank'>https://github.com/barouxlab/DataViz/tree/main</a>. For questions and advice, do not hesitate to contact Célia Baroux at <a href = 'mailto: cbaroux@botinst.uzh.ch'>cbaroux@botinst.uzh.ch</a>. We are committed to helping users, promote broad use and improvement of DataViz.</p>"),
#                              h4("Acknowledgements"),
#                              p("This tool was made possible by funding and support from Dr. Célia Baroux. Conceptual development involved Dr. Célia Baroux, Devin Routh, and Dr. Philip Shemella. Principal Software Development involved Devin Routh and Dr. Philip Shemella."),
#                              h4("Affiliations"),
#                              HTML("<p> Dr. Célia Baroux - <a href='https://www.botinst.uzh.ch/en/research/development/celiabaroux.html' target='_blank'> Department of Plant and Microbial Biology at the University of Zürich</a><br>Devin Routh and Philip Shemella - <a href='https://www.zi.uzh.ch/en/teaching-and-research/science-it.html' target='_blank'> S<sup>3</sup>IT at the University of Zürich </a></p>"),
                             h4("Color Brewer Available Palettes"),
                             plotOutput("rColorBrewer",height="600px")
                         )
                        ),
                tabPanel("About Us",
                         mainPanel(
                             h4("Authors"),
                             HTML("<p> Dr. Célia Baroux - <a href='https://www.botinst.uzh.ch/en/research/development/celiabaroux.html' target='_blank'>IPMB, University of Zürich</a>: conception, financial support<br>Devin Routh and Philip Shemella - <a href='https://www.zi.uzh.ch/en/teaching-and-research/science-it.html' target='_blank'> S<sup>3</sup>IT at the University of Zürich </a>: conception and tool development</p>"),
                             h4("Funding"),
                             p("University of Zürich, Swiss National Science Foundation (SNSF), Ricola Foundation"),
                             h4("Contact"),
                             HTML("<p>Email your comments or suggestion to <a href = 'mailto: cbaroux@botinst.uzh.ch'>cbaroux@botinst.uzh.ch</a></p>")
                         )
                        )
               )