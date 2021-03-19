# Load the libraries that are used
if(!'pacman' %in% rownames(installed.packages())) install.packages('pacman')
pacman::p_load(shiny,tictoc,DT,data.table,tidyverse,shinythemes,skimr,htmlTable,RColorBrewer,tools,ggpubr,shinybusy)

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
                             axis.line = element_line(colour = "black")
                         ))
# Instantiate a color brewer list of palettes
colorBrewerPalettes = rownames(brewer.pal.info)
colorDF = brewer.pal.info
colorDF$hexcodes = lapply(rownames(brewer.pal.info),function(name){brewer.pal(brewer.pal.info[name,"maxcolors"], name)})
customColors = list("#0C2B7B","#F09636","#A92357","#3B8AF0","#2E6B1C","#53B7AB","#C169DF","#A34C1C","#F09689","#3E3C93","#58C3F3","#858428","#4AA370","#000673","#75677D")
customLength = length(customColors)
colorDF = colorDF %>% add_row(maxcolors=customLength,category="custom",colorblind=FALSE,hexcodes=list(customColors))
row.names(colorDF)[36] = "Custom"