# Instantiate the server side function
server = function(input, output, session) {
    
    # Clean the data after uploading
    inputtedData = reactive({
        if(is.null(input$inputFile)){
            inputtedData = NULL
        } else{
            inputtedData = input$inputFile
        }
    })
    
    # Import the data
    importedData = eventReactive(input$confirmUpload,{
        dataToImport = inputtedData()
        if(file_ext(dataToImport$name)=="zip"){
            # Unzip the file once it's selected to a sub-directory (to be created) inside the working directory
            tmpDirName = paste("TMP_",toString(abs(rnorm(1))*1e15),sep="")
            unzip(dataToImport$datapath, overwrite = TRUE, exdir = paste(getwd(),tmpDirName,sep="/"))
            inputtedDataToReturn = cleaningFunction(tmpDirName)
            unlink(tmpDirName, recursive = TRUE)
            return(inputtedDataToReturn)
        } else if(file_ext(dataToImport$name)=="csv"){
            importedData = read.csv(dataToImport$datapath,check.names = FALSE)
            importedData$`Object ID` = as.character(importedData$`Object ID`)
            importedData$`Channel` = as.character(importedData$`Channel`)
            importedData$`Time` = as.numeric(importedData$`Time`)
            importedData[["Channel"]][is.na(importedData[["Channel"]])] = "NA"
            inputtedDataToReturn = as_tibble(importedData)
            return(inputtedDataToReturn)
        }
            dataToCheck = inputtedDataToReturn
        return(dataToCheck)
    })
    
    # Output the data table as a function of the inputs given in the user interface above
    output$cleanedTableToView = renderDataTable({
        DT::datatable(importedData(), extensions = "FixedColumns",plugins = "natural",options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, fixedColumns = list(leftColumns = 4)))
    })
    
    output$integrityTest_TextResults = renderPrint({
        dataToIntegrityCheck = importedData()
        
        # Determine which images have Surpass object values that are unexpected
        # Acceptable Surpass Object Levels are hard-coded here
        acceptableSOLevels = c('Nucleus','Nucleus center of mass','S2P')
        extantSOLevels = unique(dataToIntegrityCheck$'Surpass Object')
        differenceSO = setdiff(extantSOLevels,acceptableSOLevels)
        differenceDF_SO = as.data.frame(unique(dataToIntegrityCheck[dataToIntegrityCheck$`Surpass Object` %in% differenceSO,"Image File"]))[,1]
        cat("Images with unexpected Surpass Objects: ",paste(differenceDF_SO,collapse=", "),"\n")
        cat("\n")
        cat("Unexpected Surpass Object string: ",paste(differenceSO,collapse=", "),"\n")
        cat("\n")
        
        # Find images lacking a nucleus center of mass object
        findUniqueImagesWoNCoM = dataToIntegrityCheck %>% group_by(`Image File`) %>% filter(!any(`Surpass Object`=="Nucleus center of mass")) %>% ungroup()
        imagesWithOutNCoM = unique(findUniqueImagesWoNCoM$`Image File`)
        cat("Images without a Nucleus center of mass: ",paste(imagesWithOutNCoM,collapse=", "),"\n")
        cat("\n")
        
        # Find images lacking a nucleus object
        findUniqueImagesWoNucleus = dataToIntegrityCheck %>% group_by(`Image File`) %>% filter(!any(`Surpass Object`=="Nucleus")) %>% ungroup()
        imagesWithOutNucleus = unique(findUniqueImagesWoNucleus$`Image File`)
        cat("Images without a Nucleus: ",paste(imagesWithOutNucleus,collapse=", "),"\n")
        cat("\n")
        
        # Find images with columns that are completely NA
        groupsToUniqueToFindNACols = dataToIntegrityCheck %>% group_by(`Image File`) %>% filter(max(colSums(is.na(cur_data()))) == n()) %>% ungroup()
        imagesWithNACols = unique(groupsToUniqueToFindNACols$`Image File`)
        cat("Images with nothing but NA's in a column: ",paste(imagesWithNACols,collapse=", "),"\n")
        cat("\n")
        
    })
    
    output$integrityTest_NAChannelTable = renderDataTable({
        dataToIntegrityCheck = importedData()
        # Find the Image and Object ID's with NA's in the channel
        findRecordsWithNAInChannel = dataToIntegrityCheck %>% filter(is.na(Channel))
        recordsWithNAChannel = findRecordsWithNAInChannel[,c("Image File","Object ID")]
        DT::datatable(recordsWithNAChannel, options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE))
    })
    
    # Create an observe() call to complete the checkbox items once the data is uploaded, unzipped, cleaned, and processed
    observe({
        subsettableData = importedData()
        updateCheckboxGroupInput(session, "cOI", choices = levels(as.factor(subsettableData[["Category"]])), selected = levels(as.factor(subsettableData[["Category"]])))
        updateCheckboxGroupInput(session, "sOOI", choices = levels(as.factor(subsettableData[["Surpass Object"]])), selected = levels(as.factor(subsettableData[["Surpass Object"]])))
        imageLevelsForFiltering <<- levels(as.factor(subsettableData[["Image File"]]))
        updateCheckboxGroupInput(session, "nROI", choices = levels(as.factor(subsettableData[["Image File"]])), selected = levels(as.factor(subsettableData[["Image File"]])))
        updateCheckboxGroupInput(session, "lDOI", choices = levels(as.factor(subsettableData[["Treatment"]])), selected = levels(as.factor(subsettableData[["Treatment"]])))
        updateCheckboxGroupInput(session, "eOI", choices = levels(as.factor(subsettableData[["Genotype"]])), selected = levels(as.factor(subsettableData[["Genotype"]])))
        updateCheckboxGroupInput(session, "chOI", choices = levels(as.factor(subsettableData[["Channel"]])), selected = levels(as.factor(subsettableData[["Channel"]])))
    })
    
    # Create an observe() call for the select/clear all option in the image filtering area
    observe({
        if(input$selectallimages == 0) return(NULL) 
        else if (input$selectallimages%%2 != 0)
        {updateCheckboxGroupInput(session,"nROI",choices=imageLevelsForFiltering)}
        else
        {updateCheckboxGroupInput(session,"nROI",choices=imageLevelsForFiltering,selected=imageLevelsForFiltering)}
    })
    
    # Process the data
    processedData = eventReactive(input$processButton,{
        req(importedData())
        dataToProcess = importedData()
        processedDataToReturn = processingFunction(dataToProcess)
        updateRadioButtons(session,"dataToSelect",choices=c("Raw Data"="rawData","Processed Data"="processedData"))
        return(processedDataToReturn)
    })
    
    # Output the processed data table as a function of the inputs given in the user interface above
    output$processedTableToView = renderDataTable({
        DT::datatable(processedData(), extensions = "FixedColumns",plugins = "natural",options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, fixedColumns = list(leftColumns = 4)))
    })
    
    # Instantiate the reactive variable for filtering
    filteredDataset = eventReactive(input$filterButton,{
        # Turn the imported/processed data into a format that can be filtered and subsetted
        if (input$dataToSelect == "rawData"){
            subsettableData = importedData()
        }
        else if (input$dataToSelect == "processedData"){
            subsettableData = processedData()
        }
         # Apply the filters to the data (from the checkbox)
        filteredData = subsettableData %>% dplyr::filter(`Category` %in% input$cOI) %>% dplyr::filter(`Surpass Object` %in% input$sOOI) %>% dplyr::filter(`Image File` %in% input$nROI) %>% dplyr::filter(`Treatment` %in% input$lDOI) %>% dplyr::filter(`Genotype` %in% input$eOI) %>% dplyr::filter(`Channel` %in% input$chOI)
        
        return(filteredData)
    },ignoreNULL=TRUE)
    
    # Create an observe() call to complete the select input items for plotting
    observe({
        subsettableData = filteredDataset()
        l = sapply(subsettableData, class)
        categoricalVars = names(l[str_which(l,pattern="character")])
        categoricalVars = categoricalVars[-which(categoricalVars=="Object ID")]
        continuousVars = names(l[str_which(l,pattern="numeric")])
        updateSelectInput(session, "catVariableForFill", choices = categoricalVars, selected = NULL)
        updateSelectInput(session, "singleConVariable", choices = continuousVars, selected = NULL)
        updateSelectInput(session, "catVariableForSplitting", choices = categoricalVars, selected = NULL)
        updateSelectInput(session, "scatterX", choices = continuousVars, selected = NULL)
        updateSelectInput(session, "scatterY", choices = continuousVars, selected = NULL)
        updateSelectInput(session, "scatterCatColor", choices = categoricalVars, selected = NULL)
        updateSelectInput(session, "scatterCatFacet", choices = categoricalVars, selected = NULL)
    })
    
    # Output the filtered data table as a function of the inputs given in the user interface above
    filteredTableToRender = eventReactive(input$filterButton,{
        table = filteredDataset()
        return(table)
    })
    output$filteredTableToView = renderDataTable({
        DT::datatable(filteredTableToRender(), extensions = "FixedColumns",plugins = "natural",options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, fixedColumns = list(leftColumns = 4)))
    })
    
    # Once the dataset is filtered, then update the options for column selection / exporting
    observe({
        subsettableData = filteredDataset()
        updateCheckboxGroupInput(session,"exportColumns",choices=colnames(subsettableData),selected=NULL)
        columnExportOptions <<- colnames(subsettableData)
    })
    
    # Create an observe() call for the select/clear all option in the export area
    observe({
        if(input$selectallcolumns == 0) return(NULL) 
        else if (input$selectallcolumns%%2 != 1)
        {updateCheckboxGroupInput(session,"exportColumns",choices=columnExportOptions)}
        else
        {updateCheckboxGroupInput(session,"exportColumns",choices=columnExportOptions,selected=columnExportOptions)}
    })
    
    # Use the inputted column selection for data exporting/download
    dataToExport = eventReactive(input$selectColumnsButton,{
        subsettableData = filteredDataset()
        dataToExport = subsettableData %>% select(input$exportColumns)
        return(dataToExport)
    },ignoreNULL=TRUE)
    
    output$dataToExportTableToView = renderDataTable({
        DT::datatable(dataToExport(), options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE))
    })
    
    # Finalize the exporting / downloading functionality of the tabular data
    output$downloadData = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "ExportedData_Date_%Y_%m_%d_Time_%H%M%S"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(dataToExport(), file, row.names = FALSE)
        }
    )
    
    # Provide a skimr summary in a nicely formatted HTML view for the shiny app
    mySkim = skim_with(
        numeric=sfl(mean=~mean(.,na.rm=TRUE),sd=~sd(.,na.rm=TRUE)),
        character=sfl(n_unique=n_unique),
        append=FALSE
    )
    
    output$skimrOutputNumeric = renderDataTable({
        tableToDisplayNumeric = partition(mySkim(dataToExport()))$numeric %>% mutate(percentmissing = (1-complete_rate)) %>% rename("Variable"="skim_variable", "% Missing"="percentmissing", "SD"="sd","Mean"="mean") %>% select(Variable,`% Missing`,Mean,SD)
        DT::datatable(tableToDisplayNumeric, options = list(dom = 't', scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE)) %>% formatPercentage("% Missing", 2) %>% formatSignif(columns = c("Mean","SD"),digits=4)
    })
    
    output$skimrOutputCategorical = renderDataTable({
        tableToDisplayCategorical = partition(mySkim(dataToExport()))$character %>% mutate(percentmissing = (1-complete_rate)*100) %>% rename("Variable"="skim_variable", "# Levels"="n_unique", "% Missing"="percentmissing") %>% select(Variable,`# Levels`,`% Missing`)
        DT::datatable(tableToDisplayCategorical, options = list(dom = 't', scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE)) %>% formatPercentage("% Missing", 2)
    })
    
    # Provide the formatted HTML for download
    skimrOutputNumeric = eventReactive(input$selectColumnsButton,{
        tableToDisplayNumeric = partition(mySkim(dataToExport()))$numeric %>% rename("Variable"="skim_variable", "# Missing"="n_missing", "% Complete"="complete_rate")
        htmlToSaveNumeric = htmlTable(addHtmlTableStyle(tableToDisplayNumeric,col.rgroup = c("none", "#F7F7F7")))
        return(htmlToSaveNumeric)
    })
    
    skimrOutputCategorical = eventReactive(input$selectColumnsButton,{
        tableToDisplayCategorical = partition(mySkim(dataToExport()))$character %>% rename("Variable"="skim_variable", "# Missing"="n_missing", "% Complete"="complete_rate")
        htmlToSaveCategorical = htmlTable(addHtmlTableStyle(tableToDisplayCategorical,col.rgroup = c("none", "#F7F7F7")))
        return(htmlToSaveCategorical)
    })
    
    output$skimrOutputCategoricalText = renderPrint({
        cat(skimrOutputCategorical())
    })
    
    output$skimrOutputNumericText = renderPrint({
        cat(skimrOutputNumeric())
    })
    
    # Provide functionality to download the summary tables as RData files
    output$downloadSummaryNumericRData = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "SummaryTable_Numeric_Date_%Y_%m_%d_Time_%H%M%S"), ".RData", sep = "")
        },
        content = function(file) {
            numericSummaryTable = skimrOutputNumeric()
            save(numericSummaryTable, file = file)
        }
    )
    output$downloadSummaryCategoricalRData = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "SummaryTable_Categorical_Date_%Y_%m_%d_Time_%H%M%S"), ".RData", sep = "")
        },
        content = function(file) {
            categoricalSummaryTable = skimrOutputCategorical()
            save(categoricalSummaryTable, file = file)
        }
    )
    
    # Provide functionality to download the summary tables as CSV's
    output$downloadSummaryNumericCSV = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "SummaryTable_Numeric_Date_%Y_%m_%d_Time_%H%M%S"), ".csv", sep = "")
        },
        content = function(file) {
            numericSummaryTable = skimrOutputNumeric()
            write.csv(numericSummaryTable, file = file)
        }
    )
    output$downloadSummaryCategoricalCSV = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "SummaryTable_Categorical_Date_%Y_%m_%d_Time_%H%M%S"), ".csv", sep = "")
        },
        content = function(file) {
            categoricalSummaryTable = skimrOutputCategorical()
            write.csv(categoricalSummaryTable, file = file)
        }
    )
    
    
    # Subset the filtered data for plotting histograms, boxplots, and KDE's
    densityDataToHistoBox = eventReactive(input$generatePlotParams,{
        return(filteredDataset())
    },ignoreNULL=TRUE)
    
    # Instantiate a themes reactive variable
    plotTheme = reactive({themesForPlotting[[input$theme]]})
    
    # Generate recommended parameters for the plots
    observe({
        dataForPlotParams = densityDataToHistoBox()
        
        # Isolate the inputs so that only pressing the Reset Parameters button will trigger the changes
        singleConVariable = isolate(input$singleConVariable)
        catVariableForFill = isolate(input$catVariableForFill)
        numOfBinsRefined = isolate(input$numOfBinsRefined)
        
        kde = ggplot(dataForPlotParams,aes(x=!!sym(singleConVariable),color=!!sym(catVariableForFill))) + geom_density(adjust=0.9) + ylab("Density")
        xRangeKDE <<- ggplot_build(kde)$layout$panel_params[[1]]$x.range
        yRangeKDE <<- ggplot_build(kde)$layout$panel_params[[1]]$y.range
        updateNumericInput(session,"xLLKDE",value=xRangeKDE[1])
        updateNumericInput(session,"xULKDE",value=xRangeKDE[2])
        updateNumericInput(session,"yLLKDE",value=yRangeKDE[1])
        updateNumericInput(session,"yULKDE",value=yRangeKDE[2])
        
        histogram = ggplot(dataForPlotParams,aes(x=!!sym(singleConVariable),color=!!sym(catVariableForFill))) + 
        geom_histogram(bins=as.numeric(numOfBinsRefined)) + facet_wrap(as.formula(paste("~", paste("`",catVariableForFill,"`",sep=""))))
        xRangeHistogram <<- ggplot_build(histogram)$layout$panel_params[[1]]$x.range
        updateNumericInput(session,"xLLHistogram",value=xRangeHistogram[1])
        updateNumericInput(session,"xULHistogram",value=xRangeHistogram[2])
        
        dataForPlotParams = densityDataToHistoBox()
        boxplot = ggplot(dataForPlotParams,aes(y=!!sym(singleConVariable),x=!!sym(catVariableForFill),color=!!sym(catVariableForFill))) + geom_boxplot(varwidth = FALSE)
        yRangeBoxplot <<- ggplot_build(boxplot)$layout$panel_params[[1]]$y.range
        updateNumericInput(session,"yLLBoxplot",value=yRangeBoxplot[1])
        updateNumericInput(session,"yULBoxplot",value=yRangeBoxplot[2])
        })
    
    # Generate the data for the histograms and boxplots
    densityDataToHistoBoxRefined = eventReactive(input$plotRefined,{
        req(input$singleConVariable)
        req(input$catVariableForFill)
        req(input$catVariableForSplitting)
        subsettableDataForHistoBoxKDE = filteredDataset()
        filteredDataForHistoBoxKDE = subsettableDataForHistoBoxKDE %>% select(`Image File`,`Object ID`,!!sym(input$catVariableForFill),!!sym(input$singleConVariable),!!sym(input$catVariableForSplitting))
        return(filteredDataForHistoBoxKDE)
    },ignoreNULL=TRUE)
    
    # Input the plot height variable
    plotHeight = reactive(as.numeric(input$plotHeight))
    
    # Output histograms, kde's, and boxplots based on categorical and numeric variable selection for refined plotting
    histoRefined = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        data = densityDataToHistoBoxRefined()
        histogramCount = ggplot(densityDataToHistoBoxRefined(),aes(x=!!sym(input$singleConVariable),fill=!!sym(input$catVariableForFill))) +
        geom_histogram(bins=as.numeric(input$numOfBinsRefined)) + ylab("Count") +
        xlim(input$xLLHistogram,input$xULHistogram) + plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE)
        histogramCount
    })
    
    output$histoRefined = renderPlot({
        req(histoRefined())
        histoRefined()
        },height=plotHeight)

    histoRefinedPercentage = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        data = densityDataToHistoBoxRefined()
        histogramPercentage = ggplot(densityDataToHistoBoxRefined(),aes(x=!!sym(input$singleConVariable),y=stat(count)/sum(stat(count)),fill=!!sym(input$catVariableForFill))) +
        geom_histogram(bins=as.numeric(input$numOfBinsRefined)) + ylab("Percent") + scale_y_continuous(labels=scales::percent) +
        xlim(input$xLLHistogram,input$xULHistogram) + plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE)
        histogramPercentage
    })
    
    output$histoRefinedPercentage = renderPlot({
        req(histoRefinedPercentage())
        histoRefinedPercentage()
        },height=plotHeight)
    
    kdeRefined = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        data = densityDataToHistoBoxRefined()
        kdeSplit = ggplot(densityDataToHistoBoxRefined(),aes(x=!!sym(input$singleConVariable),fill=!!sym(input$catVariableForFill))) + geom_density() + ylab("Density") +
        xlim(input$xLLKDE,input$xULKDE) + ylim(input$yLLKDE,input$yULKDE) + plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE)
        kdeSplit
    })
    
    output$kdeRefined = renderPlot({
        req(kdeRefined())
        kdeRefined()
        },height=plotHeight)

        kdeRefinedPercentage = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        data = densityDataToHistoBoxRefined()
        kdeSplitPercentage = ggplot(densityDataToHistoBoxRefined(),aes(x=!!sym(input$singleConVariable),y=stat(count)/sum(stat(count)),fill=!!sym(input$catVariableForFill))) + geom_density(stat='bin',bins=as.numeric(input$numOfBinsRefined)) + ylab("Percent") + scale_y_continuous(labels=scales::percent) +
        xlim(input$xLLKDE,input$xULKDE) + plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE)
        kdeSplitPercentage
    })
    
    output$kdeRefinedPercentage = renderPlot({
        req(kdeRefinedPercentage())
        kdeRefinedPercentage()
        },height=plotHeight)
    
    boxplotRefined = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        data = densityDataToHistoBoxRefined()
        boxplot = ggplot(densityDataToHistoBoxRefined(),aes(y=!!sym(input$singleConVariable),x=!!sym(input$catVariableForFill),fill=!!sym(input$catVariableForFill))) + geom_boxplot(varwidth = FALSE) +
        ylim(input$yLLBoxplot,input$yULBoxplot) + plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE)
        boxplot
    })
    
    output$boxplotRefined = renderPlot({
        req(boxplotRefined())
        boxplotRefined()
        },height=plotHeight)
    
    # Create a pop up that allows for the download of the refined plots
    observeEvent(input$downloadRefinedPlots,{
        showModal(
            modalDialog(
                numericInput("plotWidthForDownload","Plot Width (in cm)",value=10),
                numericInput("plotHeightForDownload","Plot Height (in cm)",value=10),
                numericInput("plotResolution","Plot Resolution (in DPI)",value=72),
                downloadButton("downloadHistograms","Download Histograms"),
                downloadButton("downloadKDEs","Download KDE's"),
                downloadButton("downloadBoxplots","Download Boxplots"),
                easyClose = TRUE,
            )
        )
    })
    
    output$downloadHistograms = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "Histogram_Date_%Y_%m_%d_Time_%H%M%S"), ".png", sep = "")
        },
        content = function(file) {
            req(histoRefined())
            ggsave(file,plot=histoRefined(),device='png',width=input$plotWidthForDownload,height=input$plotHeightForDownload,units="cm",dpi=input$plotResolution)
        }
    )
    
    output$downloadKDEs = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "KDE_Date_%Y_%m_%d_Time_%H%M%S"), ".png", sep = "")
        },
        content = function(file) {
            req(kdeRefined())
            ggsave(file,plot=kdeRefined(),device='png',width=input$plotWidthForDownload,height=input$plotHeightForDownload,units="cm",dpi=input$plotResolution)
        }
    )
    
    output$downloadBoxplots = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "Boxplot_Date_%Y_%m_%d_Time_%H%M%S"), ".png", sep = "")
        },
        content = function(file) {
            req(boxplotRefined())
            ggsave(file,plot=boxplotRefined(),device='png',width=input$plotWidthForDownload,height=input$plotHeightForDownload,units="cm",dpi=input$plotResolution)
        }
    )
    
    
    
    # Once the category is selected for histograms, boxplots, and KDE's, update the selection of levels based on that variable
    observe({
        subsettableData = filteredDataset()
        updateSelectInput(session, "outlierLevel",choices = levels(as.factor(subsettableData[[input$catVariableForFill]])), selected = NULL)
    })
    
    generatedOutliers = eventReactive(input$generateOutliers,{
        req(input$outlierLevel)
        req(input$catVariableForFill)
        req(input$singleConVariable)
        subsettableData = filteredDataset()
        filteredData = subsettableData %>% select(`Image File`,`Object ID`,!!sym(input$singleConVariable),!!sym(input$catVariableForFill)) %>% dplyr::filter(!!sym(input$catVariableForFill) == input$outlierLevel) %>% na.omit()
        indexDF = filteredData %>% select(!!sym(input$singleConVariable))
        outliers = boxplot.stats(indexDF[[input$singleConVariable]])$out
        outlierTable = filteredData[indexDF[[input$singleConVariable]] %in% outliers,]
        return(outlierTable)
    },ignoreNULL=TRUE)
    
    output$outlierTable = renderDataTable({
            DT::datatable(generatedOutliers(), options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE))
        })
    
    
    # Output scatterplot parameters
    densityDataToScatterParams = eventReactive(input$scatterParams,{
        return(filteredDataset())
    },ignoreNULL=TRUE)
    
    # Generate data for the scatterplot
    observe({
        dataForPlotParams = densityDataToScatterParams()
        scatterForParams = ggplot(dataForPlotParams,aes(y=!!sym(input$scatterY),x=!!sym(input$scatterX),color=!!sym(input$scatterCatColor))) + geom_point() + facet_wrap(paste("~", paste("`",input$scatterCatFacet,"`",sep="")),ncol=input$numColumns,drop=FALSE)
        xRangeScatter <<- ggplot_build(scatterForParams)$layout$panel_params[[1]]$x.range
        yRangeScatter <<- ggplot_build(scatterForParams)$layout$panel_params[[1]]$y.range
        updateNumericInput(session,"xLLScatter",value=xRangeScatter[1])
        updateNumericInput(session,"xULScatter",value=xRangeScatter[2])
        updateNumericInput(session,"yLLScatter",value=yRangeScatter[1])
        updateNumericInput(session,"yULScatter",value=yRangeScatter[2])
    })
    
    # Output scatterplots
    scatterplotHeight = reactive(as.numeric(input$scatterplotHeight))
    
    # Instantiate a scatterplot themes reactive variable
    scatterplotTheme = reactive({themesForPlotting[[input$scatterplotTheme]]})
    
    densityDataToScatter = eventReactive(input$plotScatter,{
        req(input$scatterY)
        req(input$scatterX)
        req(input$scatterCatColor)
        req(input$scatterCatFacet)
        subsettableDataForScatter = filteredDataset()
        filteredDataForScatter = subsettableDataForScatter %>% select(`Image File`,`Object ID`,!!sym(input$scatterY),!!sym(input$scatterX),!!sym(input$scatterCatColor),!!sym(input$scatterCatFacet))
        return(filteredDataForScatter)
    },ignoreNULL=TRUE)
    
    scatterPlot = reactive({
        listOfColors = as.list(strsplit(input$scatterplotHexStrings, ",")[[1]])
        ggplot(densityDataToScatter(),aes(y=!!sym(input$scatterY),x=!!sym(input$scatterX),color=!!sym(input$scatterCatColor))) + geom_point() + facet_wrap(paste("~", paste("`",input$scatterCatFacet,"`",sep="")),ncol=input$scatterNumColumns,drop=FALSE) +
        xlim(input$xLLScatter,input$xULScatter) + ylim(input$yLLScatter,input$yULScatter) + scatterplotTheme() + scale_color_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")}))
    })
    
    output$scatter = renderPlot({
        req(scatterPlot())
        scatterPlot()
        },height=scatterplotHeight)
    
    # Create a table of the scatterplot data for viewing
    output$scatterplotTable = renderDataTable({
        DT::datatable(densityDataToScatter(), options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE))
        })
    
    # Create a pop up that allows for the download of the scatterplots
    observeEvent(input$downloadScatter,{
        showModal(
            modalDialog(
                numericInput("scatterWidth","Plot Width (in cm)",value=10),
                numericInput("scatterHeight","Plot Height (in cm)",value=10),
                numericInput("scatterResolution","Plot Resolution (in DPI)",value=72),
                downloadButton("downloadScatter","Download Scatterplot"),
                easyClose = TRUE,
            )
        )
    })
    
    output$downloadScatter = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "Scatterplot_Date_%Y_%m_%d_Time_%H%M%S"), ".png", sep = "")
        },
        content = function(file) {
            req(scatterPlot())
            ggsave(file,plot=scatterPlot(),device='png',width=input$scatterWidth,height=input$scatterHeight,units="cm",dpi=input$scatterResolution)
        }
    )
    
    output$rColorBrewer = renderPlot({
        display.brewer.all()
        })
    
    # Create an event reactive function that changes hex code strings according to selected color palette
    observeEvent(input$chosenPalette,{
        updateTextInput(session,"hexStrings",value=toString(colorDF[input$chosenPalette,"hexcodes"][[1]]))
    })
    
    observeEvent(input$scatterplotChosenPalette,{
        updateTextInput(session,"scatterplotHexStrings",value=toString(colorDF[input$scatterplotChosenPalette,"hexcodes"][[1]]))
    })
    
    
}

