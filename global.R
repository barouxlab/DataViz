# Load the libraries that are used
if(!'pacman' %in% rownames(installed.packages())) install.packages('pacman')
pacman::p_load(shiny,tictoc,DT,data.table,tidyverse,shinythemes,skimr,htmlTable,RColorBrewer,tools,ggpubr,shinybusy,shinyjs)

# Adjust other options and load processing data functions
options(shiny.maxRequestSize=500*1024^2)
source("cleaningFunction.R")
source("processingFunction.R")

# Instantiate a plot theme variable
themesForPlotting = list("Light" = theme_light(),
                         "Minimal" = theme_minimal(),
                         "Classic" = theme_classic(),
                         "Gray" = theme(
                             panel.border = element_blank(), 
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), 
                             axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
                         ))

# Instantiate a color brewer list of palettes
colorBrewerPalettes = rownames(brewer.pal.info)
colorDF = brewer.pal.info
colorDF$hexcodes = lapply(rownames(brewer.pal.info),function(name){brewer.pal(brewer.pal.info[name,"maxcolors"], name)})
customColors = list("#0C2B7B","#F09636","#A92357","#3B8AF0","#2E6B1C","#53B7AB","#C169DF","#A34C1C","#F09689","#3E3C93","#58C3F3","#858428","#4AA370","#000673","#75677D")
customLength = length(customColors)
colorDF = colorDF %>% add_row(maxcolors=customLength,category="custom",colorblind=FALSE,hexcodes=list(customColors))
row.names(colorDF)[36] = "Custom"

# Instantiate a list of acceptable surpass object types/strings
acceptableSOLevels = c('Nucleus','Nucleus Center of Mass','Center of Mass','S2P','Nucleus Center Of Mass','Nucleolus','CC','NanoCC')

# Instantiate a list of files to skip when cleaning a ZIP of the data
fileNamestoSkip = c("Diameter.csv",
                   "Diameter_X.csv",
                   "Diameter_Y.csv",
                   "Diameter_Z.csv",
                   "Distance_from_Origin.csv",
                   "Distance_to_Image_Border_XY_Img=1.csv",
                   "Generation.csv",
                   "Intensity_Center_Ch=1_Img=1.csv",
                   "Intensity_Center_Ch=2_Img=1.csv",
                   "Intensity_Max_Ch=1_Img=1.csv",
                   "Intensity_Max_Ch=2_Img=1.csv",
                   "Intensity_Median_Ch=1_Img=1.csv",
                   "Intensity_Median_Ch=2_Img=1.csv",
                   "Intensity_Min_Ch=1_Img=1.csv",
                   "Intensity_Min_Ch=2_Img=1.csv",
                   "Overlapped_Volume_Ratio_to_Surfaces_Surfaces=CC.csv",
                   "Overlapped_Volume_Ratio_to_Surfaces_Surfaces=NanoCC.csv",
                   "Overlapped_Volume_Ratio_to_Surfaces_Surfaces=Nucleolus.csv",
                   "Overlapped_Volume_Ratio_to_Surfaces_Surfaces=Nucleus.csv",
                   "Overlapped_Volume_to_Surfaces_Surfaces=CC.csv",
                   "Overlapped_Volume_ to_Surfaces_Surfaces=NanoCC.csv",
                   "Overlapped_Volume_ to_Surfaces_Surfaces=Nucleolus.csv",
                   "Overlapped_Volume_ to_Surfaces_Surfaces=Nucleus.csv",
                   "Position_X.csv",
                   "Position_Y.csv",
                   "Position_Z.csv",
                   "Position.csv",
                   "Time.csv",
                   "Time_Index.csv")

# Instantiate the names of the processed variables that can be created
processVarsOptions = c("Normalized Intensity Sum",
                       "Normalized Intensity Mean",
                       "Normalized Intensity StdDev",
                       "Normalized Distance to Nucleus",
                       "Group Intensity Sum",
                       "Group Intensity Mean",
                       "Normalized Intensity Sum Ratio Ch2:Ch1",
                       "Normalized Intensity Mean Ratio Ch2:Ch1",
                       "Signal Density",
                       "Relative Intensity Sum",
                       "Relative Intensity Mean",
                       "Object Count")

# Instantiate the starting nulls for the plot selection options
catVariableForFill_Reference = NULL
singleConVariable_Reference = NULL
binningVariable_Reference = NULL
catVariableForSplitting_Reference = NULL
additionalVarForFiltering_Reference = NULL
scatterX_Reference = NULL
scatterY_Reference = NULL
scatterCatColor_Reference = NULL
scatterCatFacet_Reference = NULL

# Instantiate a list of variables that you know should not be filled (see the cleaningFunction.R)
fillingVarsToRemove = c('Genotype','Treatment','Image File','Object ID','Category','Channel','Object','Image Subset','Time')