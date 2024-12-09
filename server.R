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
        req(inputtedData())
        dataToImport = inputtedData()
        if(file_ext(dataToImport$name)=="zip"){
            # Unzip the file once it's selected to a sub-directory (to be created) inside the working directory
            tmpDirName = paste("TMP_",toString(abs(rnorm(1))*1e15),sep="")
            unzip(dataToImport$datapath, overwrite = TRUE, exdir = paste(getwd(),tmpDirName,sep="/"))
           # Mark and delete the problematic empty files (i.e., specifically: files <100 bytes)
            listOfFilesToCheck = list.files(path=tmpDirName,full.names=TRUE,recursive=TRUE)
            listOfProblematicFiles = listOfFilesToCheck[sapply(listOfFilesToCheck, file.size) < 100]
            sapply(listOfProblematicFiles,unlink)
            # Mark and delete files that are flagged for deletion in the global.r script
            listOfFilesToCheck = list.files(path=tmpDirName,full.names=TRUE,recursive=TRUE)
            fileKeepList = listOfFilesToCheck
            for(fileName in fileNamestoSkip){
                listOfFilesToCheck = listOfFilesToCheck[!str_detect(listOfFilesToCheck,pattern=fileName)]
            }
            filesToDelete = setdiff(fileKeepList,listOfFilesToCheck)
            sapply(filesToDelete,unlink)
            # Clean the remaining files
            # Apply the cleaning function and change the types of certain variables
            inputtedDataToReturn = cleaningFunction(tmpDirName)
            inputtedDataToReturn$`Object ID` = as.character(inputtedDataToReturn$`Object ID`)
            inputtedDataToReturn$`Channel` = as.character(inputtedDataToReturn$`Channel`)
            if ("Time" %in% colnames(inputtedDataToReturn)) {
              inputtedDataToReturn$`Time` = as.character(inputtedDataToReturn$`Time`)
            }
            inputtedDataToReturn[["Channel"]][is.na(inputtedDataToReturn[["Channel"]])] = "NA"
            unlink(tmpDirName, recursive = TRUE)
            return(inputtedDataToReturn)
        } else if(file_ext(dataToImport$name)=="csv"){
            importedData = read.csv(dataToImport$datapath,check.names = FALSE)
            importedData$`Object ID` = as.character(importedData$`Object ID`)
            importedData$`Channel` = as.character(importedData$`Channel`)
            if ("Time" %in% colnames(importedData)) {
              importedData$`Time` = as.character(importedData$`Time`)
            }
            importedData[["Channel"]][is.na(importedData[["Channel"]])] = "NA"
            inputtedDataToReturn = as_tibble(importedData) %>% relocate(c("Genotype",
                                                                          "Treatment",
                                                                          "Image File"))
            return(inputtedDataToReturn)
        }
            dataToCheck = inputtedDataToReturn
        return(dataToCheck)
    })
    
    # Create an option for a quick download of the raw data from a Zip upload
    output$quickDownload = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "RawData_Date_%Y_%m_%d_Time_%H%M%S"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(importedData(), file, row.names = FALSE)
        }
    )
    
    # Output the data table as a function of the inputs given in the user interface above
    output$cleanedTableToView = renderDataTable({
        DT::datatable(importedData(), extensions = "FixedColumns",plugins = "natural",options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, fixedColumns = list(leftColumns = 4)))
    })
    
    output$integrityTest_TextResults = renderPrint({
        dataToIntegrityCheck = importedData()
        
        # Determine which images have Object values that are unexpected
        # Acceptable Object Levels are hard-coded in the global.R file
        extantSOLevels = unique(dataToIntegrityCheck$'Object')
        differenceSO = setdiff(extantSOLevels,acceptableSOLevels)
        differenceDF_SO = as.data.frame(unique(dataToIntegrityCheck[dataToIntegrityCheck$`Object` %in% differenceSO,"Image File"]))[,1]
        cat("Images with unexpected Objects: ",paste(differenceDF_SO,collapse=", "),"\n")
        cat("\n")
        cat("Unexpected Object string: ",paste(differenceSO,collapse=", "),"\n")
        cat("\n")
        
        # Find images lacking a nucleus center of mass object
        findUniqueImagesWoNCoM = dataToIntegrityCheck %>% group_by(`Image File`) %>% filter(!any(`Object`=="Nucleus Center of Mass")) %>% ungroup()
        imagesWithOutNCoM = unique(findUniqueImagesWoNCoM$`Image File`)
        cat("Images without a Nucleus Center of Mass: ",paste(imagesWithOutNCoM,collapse=", "),"\n")
        cat("\n")
        
        # Find images lacking a nucleus object
        findUniqueImagesWoNucleus = dataToIntegrityCheck %>% group_by(`Image File`) %>% filter(!any(`Object`=="Nucleus")) %>% ungroup()
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
        updateCheckboxGroupInput(session, "sOOI", choices = levels(as.factor(subsettableData[["Object"]])), selected = levels(as.factor(subsettableData[["Object"]])))
        imageLevelsForFiltering <<- levels(as.factor(subsettableData[["Image File"]]))
        updateCheckboxGroupInput(session, "nROI", choices = levels(as.factor(subsettableData[["Image File"]])), selected = levels(as.factor(subsettableData[["Image File"]])))
        updateCheckboxGroupInput(session, "lDOI", choices = levels(as.factor(subsettableData[["Treatment"]])), selected = levels(as.factor(subsettableData[["Treatment"]])))
        updateCheckboxGroupInput(session, "eOI", choices = levels(as.factor(subsettableData[["Genotype"]])), selected = levels(as.factor(subsettableData[["Genotype"]])))
        updateCheckboxGroupInput(session, "chOI", choices = levels(as.factor(subsettableData[["Channel"]])), selected = levels(as.factor(subsettableData[["Channel"]])))
    })
    
    # Create an observe() call for the select/clear all option in the image filtering area
    observe({
        if(input$selectallimages == 0) return(NULL) 
        else if (input$selectallimages%%2 == 1)
        {updateCheckboxGroupInput(session,"nROI",choices=imageLevelsForFiltering)}
        else
        {updateCheckboxGroupInput(session,"nROI",choices=imageLevelsForFiltering,selected=imageLevelsForFiltering)}
    })
    
    # Create an observe() call for numeric variables in Select (after Process)
    observe({
      
      if (input$dataToSelect == "rawData"){
        subsettableData = importedData()
      }
      else if (input$dataToSelect == "processedData"){
        subsettableData = processedData()
      }
      
      l = sapply(subsettableData, class)
      numericVariables <<- names(grep(paste(c("numeric","integer"),collapse="|"), l, value=TRUE)) 
      updateCheckboxGroupInput(session,"exportVariables",choices=numericVariables, selected = numericVariables)
    })
    
    # Create an observe() call for the select/clear all option in the Select numeric variables
    observe({
      if(input$selectallvariables == 0) return(NULL)
      else if (input$selectallvariables%%2 == 1)
      {updateCheckboxGroupInput(session,"exportVariables",choices=numericVariables)}
      else
      {updateCheckboxGroupInput(session,"exportVariables",choices=numericVariables,selected=numericVariables)}
    })

        
    # Create an observe call the updates the ratio options for the processing step after the data is imported
    observe({
        subsettableData = importedData()
        
        # create normalization variable choice
        # check Object uniqueness, except Object=Nucleus as it is default
        normVars = subsettableData %>% filter(Category == "Surface" & Object != "Nucleus") %>% 
          group_by(`Image File`,Channel,Object,Category) %>% 
          summarise(nc_obj = n(), .groups = "drop") %>% filter(nc_obj == 1) %>% 
          mutate(nc_img_ch = n_distinct(paste(`Image File`,Channel, sep="_"))) %>% 
          group_by(Object, nc_img_ch) %>% summarise(nc_img_ch_obj = n(), .groups = "drop") %>% 
          filter(nc_img_ch == nc_img_ch_obj) %>% pull(Object)
        
        # add Nucleus back
        if ("Nucleus" %in% subsettableData$Object) {
          normVars = c('Nucleus',normVars)
        }
        updateSelectInput(session, "normalizationVar", choices = normVars, selected = "")

        # Compute the potential channel pairings
        potentialChannelPairsDF = do.call(expand.grid, rep(list(unique(subsettableData[["Channel"]])), 2)) %>% filter(Var1 != Var2)
        channelPairsList <<- unname(as.list(as.data.frame(t(potentialChannelPairsDF))))
        channelPairsStrings = lapply(channelPairsList,toString)
        formatChannelStrings = function(s){return(paste("Ch",toString(strsplit(s[[1]],", ")[[1]][1]),":Ch",toString(strsplit(s[[1]],", ")[[1]][2]),sep=""))}
        channelPairsStringsFormatted <<- lapply(channelPairsStrings,formatChannelStrings)
        names(channelPairsStrings) = channelPairsStringsFormatted
        channelPairsStringsForDisplay <<- channelPairsStrings
        # Update the relevant areas in the Processing Section
        updateCheckboxGroupInput(session,"ratioSumsToCreate",choices=channelPairsStringsForDisplay)
        updateCheckboxGroupInput(session,"ratioSumsPerGroupToCreate",choices=channelPairsStringsForDisplay)
    })
    
    output$ratioSumsToCreateTitle <- renderText({
      paste0("Ratio of Intensity Sum Normalised per ",input$normalizationVar)
    })
    
    # update Process Tab checkbox Optional Variables based on Normalization selection
    observeEvent(input$normalizationVar, {

      searchVar = "Intensity variables"
      ivVarOpts <<- getValuesFromOptVarsNormalize(searchVar,input$normalizationVar)
      updateCheckboxGroupInput(session,"ivVarsToCreate",choices=ivVarOpts)

      searchVar = "Distance variables"
      dvVarOpts <<- getValuesFromOptVarsNormalize(searchVar,input$normalizationVar)
      updateCheckboxGroupInput(session,"dvVarsToCreate",choices=dvVarOpts)

      searchVar = "Shape variables"
      svVarOpts <<- getValuesFromOptVarsNormalize(searchVar,input$normalizationVar)
      updateCheckboxGroupInput(session,"svVarsToCreate",choices=svVarOpts)

      searchVar = "Object count"
      ocVarOpts <<- getValuesFromOptVarsNormalize(searchVar,input$normalizationVar)
      updateCheckboxGroupInput(session,"ocVarsToCreate",choices=ocVarOpts)

      searchVar = "Group variables"
      gvVarOpts <<- getValuesFromOptVarsNormalize(searchVar,input$normalizationVar)
      updateCheckboxGroupInput(session,"gvVarsToCreate",choices=gvVarOpts)

      searchVar = "Other variables"
      ovVarOpts <<- getValuesFromOptVarsNormalize(searchVar,input$normalizationVar)
      updateCheckboxGroupInput(session,"ovVarsToCreate",choices=ovVarOpts)

    }, ignoreInit = TRUE)
    
    getValuesFromOptVarsNormalize <- function(searchVar, normVar) {
      res = rjson::fromJSON(file = "optionalvariables_w_normalization.json")
      
      for(elem in res$optionalvariables){
        if (elem$name == searchVar) {
          return(getVariableNames_w_Normalization(elem,normVar))
        }
      }
    }
    
    # select all handlers
    observe({
      if(input$selectallIVvars == 0) return(NULL) 
      else if (input$selectallIVvars%%2 == 0)
      {updateCheckboxGroupInput(session,"ivVarsToCreate",choices=ivVarOpts)}
      else
      {updateCheckboxGroupInput(session,"ivVarsToCreate",choices=ivVarOpts,selected=ivVarOpts)}
    })    
    
    observe({
      if(input$selectallDVvars == 0) return(NULL) 
      else if (input$selectallDVvars%%2 == 0)
      {updateCheckboxGroupInput(session,"dvVarsToCreate",choices=dvVarOpts)}
      else
      {updateCheckboxGroupInput(session,"dvVarsToCreate",choices=dvVarOpts,selected=dvVarOpts)}
    })      
    
    observe({
      if(input$selectallSVvars == 0) return(NULL) 
      else if (input$selectallSVvars%%2 == 0)
      {updateCheckboxGroupInput(session,"svVarsToCreate",choices=svVarOpts)}
      else
      {updateCheckboxGroupInput(session,"svVarsToCreate",choices=svVarOpts,selected=svVarOpts)}
    })
    
    observe({
      if(input$selectallOCvars == 0) return(NULL) 
      else if (input$selectallOCvars%%2 == 0)
      {updateCheckboxGroupInput(session,"ocVarsToCreate",choices=ocVarOpts)}
      else
      {updateCheckboxGroupInput(session,"ocVarsToCreate",choices=ocVarOpts,selected=ocVarOpts)}
    })     
    
    observe({
      if(input$selectallGVvars == 0) return(NULL) 
      else if (input$selectallGVvars%%2 == 0)
      {updateCheckboxGroupInput(session,"gvVarsToCreate",choices=gvVarOpts)}
      else
      {updateCheckboxGroupInput(session,"gvVarsToCreate",choices=gvVarOpts,selected=gvVarOpts)}
    })     
    
    observe({
      if(input$selectallOVvars == 0) return(NULL) 
      else if (input$selectallOVvars%%2 == 0)
      {updateCheckboxGroupInput(session,"ovVarsToCreate",choices=ovVarOpts)}
      else
      {updateCheckboxGroupInput(session,"ovVarsToCreate",choices=ovVarOpts,selected=ovVarOpts)}
    })     
    
    combinedVarsToCreate <- reactive({
      c(input$ivVarsToCreate, 
        input$dvVarsToCreate,
        input$svVarsToCreate,
        input$ocVarsToCreate,
        input$gvVarsToCreate,
        input$ovVarsToCreate)
    })
    
    # Assign id attribute to individual checkbox elements
    shinyjs::runjs(HTML('
    
      function setIDname(checkboxes) {
        for(var i = 0; i < checkboxes.length; i++) {
            let idName = checkboxes[i].getAttribute("value").replace(/\\s+/g, "-");
            checkboxes[i].setAttribute("id", idName);
          }
      }
      
      var interval = setInterval(function() {
        elemId = "ivVarsToCreate"
        if (document.getElementById(elemId)) {
          clearInterval(interval);
          setIDname($("#" + elemId).find(".checkbox input"))
        }
      },100);

      var interval = setInterval(function() {
        elemId = "dvVarsToCreate"
        if (document.getElementById(elemId)) {
          clearInterval(interval);
          setIDname($("#" + elemId).find(".checkbox input"))
        }
      },100);

      var interval = setInterval(function() {
        elemId = "svVarsToCreate"
        if (document.getElementById(elemId)) {
          clearInterval(interval);
          setIDname($("#" + elemId).find(".checkbox input"))
        }
      },100);
      
      var interval = setInterval(function() {
        elemId = "ocVarsToCreate"
        if (document.getElementById(elemId)) {
          clearInterval(interval);
          setIDname($("#" + elemId).find(".checkbox input"))
        }
      },100);
      
      var interval = setInterval(function() {
        elemId = "gvVarsToCreate"
        if (document.getElementById(elemId)) {
          clearInterval(interval);
          setIDname($("#" + elemId).find(".checkbox input"))
        }
      },100);      

      var interval = setInterval(function() {
        elemId = "ovVarsToCreate"
        if (document.getElementById(elemId)) {
          clearInterval(interval);
          setIDname($("#" + elemId).find(".checkbox input"))
        }
      },100);      
      
      var interval = setInterval(function() {
        elemId = "ratioSumsToCreate"
        if (document.getElementById(elemId)) {
          clearInterval(interval);
          var h5elem = $("#" + elemId).find("h5");
          for(var i = 0; i < h5elem.length; i++) {
            let idName = h5elem[i].outerText.replace(/\\s+/g, "-");
            h5elem[i].setAttribute("id", idName);
          }
        }
      },100);      

    '))
    
    # to do update tooltips to get desc from correct ID
    output$checkboxTooltips <- renderUI({
      res = rjson::fromJSON(file = "optionalvariables_w_normalization.json")
      
      tooltips = c()
      for(elem in res$optionalvariables){
        for (a in elem$variables){
          
          # check normalization / change to generate correct tooltip id
          if (a$is_norm == "true" & input$normalizationVar != "Nucleus") {
            a$name = gsub("Nucleus",input$normalizationVar,a$name)
            a$desc = gsub("Nucleus",input$normalizationVar,a$desc)
          }
          
          a$id = gsub("\\s+", "-", a$name)
          tooltips = c(tooltips, list(a))
        }
      }
      
      bsTooltipList = lapply(tooltips, function(tooltip) {
        bsTooltip(tooltip$id, tooltip$desc, placement = "top", trigger = "hover")
      })
      
      tagList(
        bsTooltipList
      )
    })    
    
    # select all handlers for Ratio variables
    observe({
        if(input$selectRatioSums == 0) return(NULL) 
        else if (input$selectRatioSums%%2 == 0)
        {updateCheckboxGroupInput(session,"ratioSumsToCreate",choices=channelPairsStringsForDisplay)}
        else
        {updateCheckboxGroupInput(session,"ratioSumsToCreate",choices=channelPairsStringsForDisplay,selected=channelPairsStringsForDisplay)}
    })
    
    observe({
        if(input$selectRatioSumsPerGroup == 0) return(NULL) 
        else if (input$selectRatioSumsPerGroup%%2 == 0)
        {updateCheckboxGroupInput(session,"ratioSumsPerGroupToCreate",choices=channelPairsStringsForDisplay)}
        else
        {updateCheckboxGroupInput(session,"ratioSumsPerGroupToCreate",choices=channelPairsStringsForDisplay,selected=channelPairsStringsForDisplay)}
    })
    
    # Dependency checkbox
    observeEvent(input$ratioSumsToCreate, {
      if(length(input$ratioSumsToCreate) > 0){
        
        updateCheckboxGroupInput(session,"ivVarsToCreate",
                                 selected=append(input$ivVarsToCreate, paste0("Intensity Sum Normalised per ",input$normalizationVar)))
      }
    })
    
    observeEvent(input$ovVarsToCreate, {
      if("Signal Density" %in% input$ovVarsToCreate){
        updateCheckboxGroupInput(session,"ivVarsToCreate",
                                 selected=append(input$ivVarsToCreate, paste0("Intensity Sum Normalised per ",input$normalizationVar)))
      }
    })
    
    observeEvent(input$ivVarsToCreate, {
      if("Intensity Sum Normalised by Group" %in% input$ivVarsToCreate){
        updateCheckboxGroupInput(session,"ivVarsToCreate",
                                 selected=append(input$ivVarsToCreate, paste0("Intensity Sum Normalised per ",input$normalizationVar)))
      }
    })    
    
    observeEvent(input$ivVarsToCreate, {
      if("Intensity Mean Normalised by Group" %in% input$ivVarsToCreate){
        updateCheckboxGroupInput(session,"ivVarsToCreate",
                                 selected=append(input$ivVarsToCreate, paste0("Intensity Mean Normalised per ",input$normalizationVar)))
      }
    })
    
    observeEvent(input$gvVarsToCreate, {
      if(paste0("Group Intensity Sum Relative to ",input$normalizationVar) %in% input$gvVarsToCreate) {
        updateCheckboxGroupInput(session,"gvVarsToCreate",
                                 selected=append(input$gvVarsToCreate,"Group Intensity Sum"))
      }
    })
    
    observeEvent(input$gvVarsToCreate, {
      if(paste0("Group Intensity Mean Relative to ",input$normalizationVar) %in% input$gvVarsToCreate) {
        updateCheckboxGroupInput(session,"gvVarsToCreate",
                                 selected=append(input$gvVarsToCreate,"Group Intensity Mean"))
      }
    })
    
    observeEvent(input$ratioSumsPerGroupToCreate, {
      if(length(input$ratioSumsPerGroupToCreate) > 0){
        updateCheckboxGroupInput(session,"ivVarsToCreate",
                                 selected=append(input$ivVarsToCreate,"Intensity Sum Normalised by Group"))
      }
    }) 

    observeEvent(input$svVarsToCreate, {
      if("Volume Relative to Group" %in% input$svVarsToCreate) {
        updateCheckboxGroupInput(session,"gvVarsToCreate",
                                 selected=append(input$gvVarsToCreate,"Group Volume"))
      }
    })
        
    observeEvent(input$gvVarsToCreate, {
      if(paste0("Group Volume Relative to ",input$normalizationVar) %in% input$gvVarsToCreate) {
        updateCheckboxGroupInput(session,"gvVarsToCreate",
                                 selected=append(input$gvVarsToCreate,"Group Volume"))
      }
    })
    
    observeEvent(input$gvVarsToCreate, {
      if("Group Surface Area-to-Volume Ratio" %in% input$gvVarsToCreate) {
        updateCheckboxGroupInput(session,"gvVarsToCreate",
                                 selected=append(input$gvVarsToCreate,c("Group Volume","Group Surface Area")))
      }
    })
    
    # Process the data
    processedData = eventReactive(input$processButton,{
        req(importedData())
        dataToProcess = importedData()
        combinedVars <- unlist(as.list(combinedVarsToCreate()), recursive = TRUE)
        processedData = suppressWarnings(processingFunction(dataToProcess,combinedVars,input$ratioSumsToCreate,input$ratioSumsPerGroupToCreate,input$normalizationVar))
        processedDataToWrite = processedData
        write.csv(processedDataToWrite, "TMP__ProcessedData.csv", row.names = FALSE)
        processedDataToReturn = read.csv("TMP__ProcessedData.csv",check.names = FALSE)
        updateRadioButtons(session,"dataToSelect",choices=c("Original Data"="rawData","Processed Data"="processedData"))
        processedDataToReturn$`Object ID` = as.character(processedDataToReturn$`Object ID`)
        processedDataToReturn$`Channel` = as.character(processedDataToReturn$`Channel`)
        if ("Time" %in% colnames(processedDataToReturn)) {
          processedDataToReturn$`Time` = as.numeric(processedDataToReturn$`Time`) 
        }
        processedDataToReturn[["Channel"]][is.na(processedDataToReturn[["Channel"]])] = "NA"
        finalProcessedDataToReturn = as_tibble(processedDataToReturn)
        unlink("TMP__ProcessedData.csv")
        return(finalProcessedDataToReturn)
    })
    
    # Create observe calls to update the outlier selection dropdown after the intial data is uploaded or processed
    observe({
      if (input$dataToSelect == "rawData"){
        subsettableData = importedData()
      }
      else if (input$dataToSelect == "processedData"){
        subsettableData = processedData()
      }
      l = sapply(subsettableData, class)
      continuousVars = names(grep(paste(c("numeric","integer"),collapse="|"), l, value=TRUE)) 
      updateSelectInput(session, "outlierVariable", choices = continuousVars, selected = NULL)
    })
    
    observe({
      subsettableData = reactiveDF$filteredDataset
      l = sapply(subsettableData, class)
      continuousVars = names(grep(paste(c("numeric","integer"),collapse="|"), l, value=TRUE)) 
      updateSelectInput(session, "outlierVariable", choices = continuousVars, selected = NULL)
    })
    
    # Create an option for a quick download of the processed data
    output$quickProcDownload = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "ProcessedData_Date_%Y_%m_%d_Time_%H%M%S"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(processedData(), file, row.names = FALSE)
        }
    )
    
    # Output the processed data table as a function of the inputs given in the user interface above
    output$processedTableToView = renderDataTable({
        DT::datatable(processedData(), extensions = "FixedColumns",plugins = "natural",options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, fixedColumns = list(leftColumns = 4)))
    })
    
    # Create options for generating outliers based on variable input
    generatedOutliers = eventReactive(input$generateOutliers,{
        subsettableData = reactiveDF$filteredDataset
        req(input$outlierVariable)
        outlierVariableString = input$outlierVariable
        dataToBoxplot = subsettableData %>%
                        select(!!sym(outlierVariableString)) %>%
                        pull(!!sym(outlierVariableString))
        outliersToRemove = boxplot.stats(dataToBoxplot)$out
        outlierIndicesToRemove = which(dataToBoxplot %in% c(outliersToRemove))
        outlierTibble = subsettableData[outlierIndicesToRemove,]
        return(outlierTibble)
    },ignoreNULL=TRUE)
    
    output$outlierTable = renderDataTable({
            DT::datatable(generatedOutliers(), options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE))
        })
    
    # Instantiate the reactive variable for filtering
    reactiveDF = reactiveValues() 
    
    observeEvent(input$filterButton,{
        # Turn the imported/processed data into a format that can be filtered and subsetted
        if (input$dataToSelect == "rawData"){
            sData = importedData()
        }
        else if (input$dataToSelect == "processedData"){
            sData = processedData()
        }
         # Apply the filters to the data (from the checkbox)
        filteredDataToOutlier = sData %>% dplyr::filter(`Object` %in% input$sOOI) %>% dplyr::filter(`Image File` %in% input$nROI) %>% dplyr::filter(`Treatment` %in% input$lDOI) %>% dplyr::filter(`Genotype` %in% input$eOI) %>% dplyr::filter(`Channel` %in% input$chOI)
        
        # Apply selection filter of variables (numeric)
        # num. variables to remove = all num. variabes - selected num. variable
        l = sapply(filteredDataToOutlier, class)
        numericVariables = names(grep(paste(c("numeric","integer"),collapse="|"), l, value=TRUE))
        numericVariablesToRemove = setdiff(numericVariables,input$exportVariables)
        filteredDataToOutlier = filteredDataToOutlier %>% select(-numericVariablesToRemove)

        # Optionally: remove outliers
        if (input$removeOutliersRadio == "no"){
            filteredData = filteredDataToOutlier
        }
        else if (input$removeOutliersRadio == "yes"){
            if (input$outlierVariable %in% colnames(filteredDataToOutlier)){
                sData = filteredDataToOutlier
                req(input$outlierVariable)
                outlierVariableString = input$outlierVariable
                dataToBoxplot = sData %>%
                                select(!!sym(outlierVariableString)) %>%
                                pull(!!sym(outlierVariableString))
                outliersToRemove = boxplot.stats(dataToBoxplot)$out
                outlierIndicesToRemove = which(dataToBoxplot %in% c(outliersToRemove))
                outlierTibble = sData[outlierIndicesToRemove,]
                filteredData = anti_join(filteredDataToOutlier,outlierTibble)
            } else {
                filteredData = filteredDataToOutlier
            }
        }
        
        reactiveDF$filteredDataset = filteredData
        reactiveDF$filteredDatasetRef = filteredData
        
        # Update the variables for plotting so that unavailable variables are not offered
        fData = filteredData
        l = sapply(fData, class)
        categoricalVars = sort(names(l[str_which(l,pattern="character")]))
        categoricalVars = sort(categoricalVars[-which(categoricalVars=="Object ID")])
        continuousVars = sort(names(l[str_which(l,pattern="character",negate=TRUE)]))
        
        updateCategoriesContinuousVars <- updateCategoriesContinuousVars1()
        updateSelectInput(session, "catVariableForFill", choices = categoricalVars, selected = catVariableForFill_Reference)
        updateSelectInput(session, "singleConVariable", choices = updateCategoriesContinuousVars(continuousVars), selected = singleConVariable_Reference)
        updateSelectInput(session, "binningVariable", choices = updateCategoriesContinuousVars(continuousVars), selected = binningVariable_Reference)
        updateSelectInput(session, "catVariableForSplitting", choices = categoricalVars, selected = catVariableForSplitting_Reference)
        updateSelectInput(session, "additionalVarForFiltering", choices = updateCategoriesContinuousVars(continuousVars), selected = additionalVarForFiltering_Reference)
        updateSelectInput(session, "scatterX", choices = updateCategoriesContinuousVars(continuousVars), selected = scatterX_Reference)
        updateSelectInput(session, "scatterY", choices = updateCategoriesContinuousVars(continuousVars), selected = scatterY_Reference)
        updateSelectInput(session, "scatterCatColor", choices = categoricalVars, selected = scatterCatColor_Reference)
        updateSelectInput(session, "scatterCatFacet", choices = categoricalVars, selected = scatterCatFacet_Reference)
    },ignoreNULL=TRUE)
    
    
    getVariableNames_w_Normalization <- function(elem,normVar) {
      allOptVars = c()
      for(a in elem$variables){
        if (a$is_norm == "true" & normVar != "Nucleus") {
          a$name = gsub("Nucleus",normVar,a$name)
        }
        allOptVars = c(allOptVars, a$name)
      }
      return(allOptVars)
    }
    
    # assign categories to continuous variables as defined in JSON file
    updateCategoriesContinuousVars1 <- reactive({
      function(continuousVars) {
        
        normVar = input$normalizationVar
        
        allOptVarsJSON = rjson::fromJSON(file = "optionalvariables_w_normalization.json")
        allOptVars = c()
        for(elem in allOptVarsJSON$optionalvariables) {
          allOptVars = c(allOptVars, getVariableNames_w_Normalization(elem,normVar))
        }
        
        dfVars = continuousVars[!continuousVars %in% allOptVars]
        selectedOptVars = continuousVars[continuousVars %in% allOptVars]
        ratioVars = dfVars[grepl("Ratio Ch", dfVars)]
        dfVars = dfVars[!grepl("Ratio Ch", dfVars)]
        
        ll = list()
        ll[[" "]] = dfVars
        
        # assign selected opt vars to categories from JSON
        for(elem in allOptVarsJSON$optionalvariables) {
          if (length(selectedOptVars) > 0) {
            categoryVar = elem$name
            varList = getVariableNames_w_Normalization(elem,normVar)
            
            selCatVars = selectedOptVars[selectedOptVars %in% varList]
            if (length(selCatVars) > 0) {
              ll[[categoryVar]] = c(selCatVars,"")
              selectedOptVars = selectedOptVars[!selectedOptVars %in% varList]
            }
          }
        }
        
        ratioVars_SumNormNucl = ratioVars[grepl(paste0("Intensity Sum Normalised per ",normVar), ratioVars)]
        if (length(ratioVars_SumNormNucl) > 0) {
          ll[[paste0('Ratio of Intensity Sum Normalised per ',normVar)]] = c(ratioVars_SumNormNucl,"")
        }
        
        ratioVars_SumNormGr = ratioVars[grepl("Intensity Sum Normalised per Group", ratioVars)]
        if (length(ratioVars_SumNormGr) > 0) {
          ll[['Ratio of Intensity Sum Normalised per Group']] = c(ratioVars_SumNormGr,"")
        }
        
        return(ll)
        
      }
    })

    # Finalize the exporting / downloading functionality of the filtered/selected data
    output$selectedDQD = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "FilteredData_Date_%Y_%m_%d_Time_%H%M%S"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(reactiveDF$filteredDataset, file, row.names = FALSE)
        }
    )
    
    # Output the filtered data table as a function of the inputs given in the user interface above
    filteredTableToRender = eventReactive(input$filterButton,{
        table = reactiveDF$filteredDataset
        return(table)
    })
    output$filteredTableToView = renderDataTable({
        DT::datatable(filteredTableToRender(), extensions = "FixedColumns",plugins = "natural",options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, fixedColumns = list(leftColumns = 4)))
    })
    
    # Once the dataset is filtered, then update the options for column selection / exporting
    observe({
        subsettableData = reactiveDF$filteredDataset
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
        subsettableData = reactiveDF$filteredDataset
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
        return(reactiveDF$filteredDataset)
    },ignoreNULL=TRUE)
    
    # Instantiate a themes reactive variable
    plotTheme = reactive({themesForPlotting[[input$theme]]})
    
    
    # gets min and max of a column from dataframe
    getMinMax <- function(df,col) {
      df = df %>% filter(!is.na(!!sym(col)))
      rangeCol = df %>%
        summarize(min_value = min(!!sym(col), na.rm = TRUE), 
                  max_value = max(!!sym(col), na.rm = TRUE)) %>% unlist() %>% as.array()
      names(rangeCol) <- NULL
      return(rangeCol)
    }
    
    # Generate recommended parameters for the plots
    observe({
        dataForPlotParams = densityDataToHistoBox()
        
        # Isolate the inputs so that only pressing the Reset Parameters button will trigger the changes
        singleConVariable = isolate(input$singleConVariable)
        catVariableForFill = isolate(input$catVariableForFill)
        numOfBinsRefined = isolate(input$numOfBinsRefined)
        
        # estimate kde X from data
        xRangeKDE <<- getMinMax(dataForPlotParams,singleConVariable)
        updateNumericInput(session,"xLLKDE",value=xRangeKDE[1])
        updateNumericInput(session,"xULKDE",value=xRangeKDE[2])
        
        # estimate kde Y via pre-plotting
        kde = ggplot(dataForPlotParams,aes(x=!!sym(singleConVariable),color=!!sym(catVariableForFill))) + geom_density() + ylab("Density")
        y_ranges = lapply(ggplot_build(kde)$layout$panel_params, function(panel) panel$y.range)
        yRangeKDE <<- c(min(sapply(y_ranges, function(x) x[1])),max(sapply(y_ranges, function(x) x[2])))
        updateNumericInput(session,"yLLKDE",value=yRangeKDE[1])
        updateNumericInput(session,"yULKDE",value=yRangeKDE[2])

        # estimate histogram X from data
        xRangeHistogram <<- getMinMax(dataForPlotParams,singleConVariable)
        updateNumericInput(session,"xLLHistogram",value=xRangeHistogram[1])
        updateNumericInput(session,"xULHistogram",value=xRangeHistogram[2])
        
        # estimate boxplot/violin X from data
        yRangeBoxplot <<- getMinMax(dataForPlotParams,singleConVariable)
        updateNumericInput(session,"yLLBoxplot",value=yRangeBoxplot[1])
        updateNumericInput(session,"yULBoxplot",value=yRangeBoxplot[2])
        })
    
    
    # updates Filtering/Binning Tab Channel selection
    observe({
      if ("Channel" %in% names(reactiveDF$filteredDataset)) {
        updateSelectInput(session, "filterChOI", choices = c("All", sort(unique(reactiveDF$filteredDataset %>% pull(`Channel`)))),selected=input$filterChOI)
      }
    })
    
    # Create the option for an additional filter based on a user defined unidemionsal set of lower and upper bounds
    # and render the data to a table in the 1-D plot area
    observeEvent(input$additionalFilter,{
        req(reactiveDF$filteredDataset)
        # Make a copy so that ONLY LAST applied filter can be canceled (if several filters applied sequentially)
        reactiveDF$filteredDatasetPreFilterReference = reactiveDF$filteredDataset
        
        dataToFilter = reactiveDF$filteredDataset
        
        # filter per All channels
        if (input$filterChOI == "All") {
          additionalFilteredDataset = dataToFilter %>% filter(!!sym(input$additionalVarForFiltering) >= input$additionalFilterLower,
                                                              !!sym(input$additionalVarForFiltering) <= input$additionalFilterUpper)
        } else {
          additionalFilteredDataset = dataToFilter %>% 
            filter(!(`Channel` %in% input$filterChOI) | 
                     (`Channel` %in% input$filterChOI & 
                        !!sym(input$additionalVarForFiltering) >= input$additionalFilterLower & 
                        !!sym(input$additionalVarForFiltering) <= input$additionalFilterUpper))
        }
        reactiveDF$filteredDataset = additionalFilteredDataset
        
        groupVarName <<- "__reset"
    },ignoreNULL=TRUE)
    
    # Create the option to reset the data / cancel the filter and return to the originally selected data
    observeEvent(input$cancelFilter,{
        req(reactiveDF$filteredDataset)
        req(reactiveDF$filteredDatasetRef)
        reactiveDF$filteredDataset = reactiveDF$filteredDatasetPreFilterReference
    },ignoreNULL=TRUE)
    
    observeEvent(input$cancelAllFilter,{
      req(reactiveDF$filteredDataset)
      req(reactiveDF$filteredDatasetRef)
      reactiveDF$filteredDataset = reactiveDF$filteredDatasetRef
    },ignoreNULL=TRUE)    
    
    output$filteredDatasetForFilterTab = renderDataTable({
      req(reactiveDF$filteredDataset)
      DT::datatable(reactiveDF$filteredDataset, extensions = "FixedColumns",plugins = "natural",
                    options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, fixedColumns = list(leftColumns = 4)))
    })
    
    # Create the option to download the filtered data
    output$downloadAdditionalFilteredData = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "FilteredData_Date_%Y_%m_%d_Time_%H%M%S"), ".csv", sep = "")
        },
        content = function(file) {
          
            downloadDataStub = reactiveDF$filteredDataset
            if(input$tabs1Dplots == "Histograms") {
              downloadDataStub = downloadDataStub %>% 
                filter(!!sym(input$singleConVariable) >= input$xLLHistogram) %>% 
                filter(!!sym(input$singleConVariable) <= input$xULHistogram)
            } else if(input$tabs1Dplots == "Density Plots") {
              downloadDataStub = downloadDataStub %>% 
                filter(!!sym(input$singleConVariable) >= input$xLLKDE) %>% 
                filter(!!sym(input$singleConVariable) <= input$xULKDE)
            } else if(input$tabs1Dplots %in% c("Boxplots", "Boxplot Stats", "Violin Plots")) {
              downloadDataStub = downloadDataStub %>% 
                filter(!!sym(input$singleConVariable) >= input$yLLBoxplot) %>% 
                filter(!!sym(input$singleConVariable) <= input$yULBoxplot)
            } 
            
            downloadDataStub = downloadDataStub %>% 
              select("Image File","Treatment", "Genotype",!!sym(input$singleConVariable), !!sym(input$catVariableForFill), !!sym(input$catVariableForSplitting))
            write.csv(downloadDataStub, file, row.names = FALSE)
        }
    )
    
    # Generate the data for the histograms and boxplots
    densityDataToHistoBoxRefined = eventReactive(input$plotRefined,{
        req(input$singleConVariable)
        req(input$catVariableForFill)
        req(input$catVariableForSplitting)
        subsettableDataForHistoBoxKDE = reactiveDF$filteredDataset
#         Filter the data first?
#         filteredDataForHistoBoxKDE = subsettableDataForHistoBoxKDE %>% select(`Image File`,`Object ID`,!!sym(input$catVariableForFill),!!sym(input$singleConVariable),!!sym(input$catVariableForSplitting))
        
        # Store the selected inputs for reference in the future
        catVariableForFill_Reference <<- input$catVariableForFill
        singleConVariable_Reference <<- input$singleConVariable
        binningVariable_Reference <<- input$binningVariable
        catVariableForSplitting_Reference <<- input$catVariableForSplitting
        additionalVarForFiltering_Reference <<- input$additionalVarForFiltering
        scatterX_Reference <<- input$scatterX
        scatterY_Reference <<- input$scatterY
        scatterCatColor_Reference <<- input$scatterCatColor
        scatterCatFacet_Reference <<- input$scatterCatFacet
        return(subsettableDataForHistoBoxKDE)
        
    },ignoreNULL=TRUE)
    
    # Input the plot height variable
    plotHeight = reactive(as.numeric(input$plotHeight))
    
    # applies min and max on a column
    applyMinMax <- function(df,col,ll,ul) {
      df = df %>% filter(!is.na(!!sym(col)))
      df = df %>% filter(!!sym(col) >= ll & !!sym(col) <= ul)
      return(df)
    }
    
    # calculateKWTextPosition <- function(plotStub,split_col,kruskal_results){
    #   # show modal KW not possible
    #   if (sum(is.na(kruskal_results$statistic)) == nrow(kruskal_results)) {
    #     showModal(modalDialog(
    #       title = "Number of groups in the panel(s) is not equal to 2, cannot perform Kruskall-Wallis test.",
    #       easyClose = TRUE
    #     ))
    #     updateCheckboxInput(session, "kruskallwallisCheckbox", value = FALSE)
    #     return(plotStub)
    #   }
    #   
    #   g = ggplot_build(plotStub)
    #   layerData = layer_data(plotStub)
    #   facet_strip = as_tibble(g$layout$layout)
    #   layerData = layerData %>% left_join(facet_strip, by = join_by(PANEL == PANEL)) 
    #   layerData = layerData %>% group_by(!!sym(split_col)) %>% 
    #     summarise(minx = if ("xmin" %in% colnames(.)) min(xmin) else min(x), 
    #               maxx = if ("xmax" %in% colnames(.)) max(xmax) else max(x),
    #               miny = if ("ymin_final" %in% colnames(.)) min(ymin_final) else { if ("ymin" %in% colnames(.)) min(ymin) else min(y) },
    #               maxy = if ("ymax_final" %in% colnames(.)) max(ymax_final) else { if ("ymax" %in% colnames(.)) max(ymax) else max(y) }) 
    #   
    #   kwToPlot = layerData %>% 
    #     left_join(kruskal_results, by = join_by(!!sym(split_col) == !!sym(split_col))) %>% 
    #     select(!!sym(split_col), minx, maxy, `p-value`) %>% rename("x" = minx, "y" = maxy) %>% 
    #     mutate(`p-value` = paste0("p =", `p-value`))
    #   
    #   return(kwToPlot)  
    # }

    # add p-value of KW test to plot
    # addKWPValueToPlot <- function(plotStub,value_col,group_col,split_col,df){
    #   kruskal_results = calculateKWTest(df, value_col, group_col, split_col)
    #   kwToPlot = calculateKWTextPosition(plotStub,split_col,kruskal_results)
    #   plotStub = plotStub + geom_text(data = kwToPlot, 
    #                                   aes(x = x, y = y, label = `p-value`), 
    #                                   color = "black", size = 3.5, 
    #                                   vjust=-1, hjust=0, 
    #                                   inherit.aes = FALSE)
    # }
    
    addKWToFacetTitle <- function(kruskal_results, plotStub, catVariableForSplitting) {
      g = ggplotGrob(plotStub)
      strip_indices <- which(grepl("strip-t", g$layout$name))
      for (i in seq_along(strip_indices)) {
        strip <- g$grobs[[strip_indices[i]]]
        text_grob = strip$grobs[[1]]$children[[2]]$children[[1]]
        old_label = text_grob$label
        new_label = kruskal_results %>% filter(!!sym(catVariableForSplitting) == old_label) %>% 
          mutate(`p-value` = paste0("p = ", `p-value`)) %>% pull(`p-value`)
        new_label = paste(old_label," ",new_label)
        text_grob$label = new_label
        g$grobs[[strip_indices[i]]]$grobs[[1]]$children[[2]]$children[[1]] <- text_grob
      }
      return(ggplotify::as.ggplot(g))
    }    
        
    # Output histograms, kde's, boxplots, and violin plots based on categorical and numeric variable selection for refined plotting
    histoRefined = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        densityDataToHistoBoxRefined = applyMinMax(densityDataToHistoBoxRefined(),input$singleConVariable,input$xLLHistogram,input$xULHistogram)
        histogramCount = ggplot(densityDataToHistoBoxRefined,aes(x=!!sym(input$singleConVariable),fill=!!sym(input$catVariableForFill))) +
        geom_histogram(bins=as.numeric(input$numOfBinsRefined)) + ylab("Count") +
        plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE,scales="free") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
        
        if (input$kruskallwallisCheckbox) {
          kruskal_results = calculateKWTest(densityDataToHistoBoxRefined, input$singleConVariable, input$catVariableForFill, input$catVariableForSplitting)
          histogramCount = addKWToFacetTitle(kruskal_results, histogramCount, input$catVariableForSplitting)
        }
        
        histogramCount
    })
    
    output$histoRefined = renderPlot({
        req(histoRefined())
        histoRefined()
        },height=plotHeight)

    histoRefinedPercentage = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        densityDataToHistoBoxRefined = applyMinMax(densityDataToHistoBoxRefined(),input$singleConVariable,input$xLLHistogram,input$xULHistogram)
        histogramPercentage = ggplot(densityDataToHistoBoxRefined,aes(x=!!sym(input$singleConVariable),y=after_stat(count / ave(count, PANEL, FUN = sum)),fill=!!sym(input$catVariableForFill))) +
        geom_histogram(bins=as.numeric(input$numOfBinsRefined)) + ylab("Percent") + scale_y_continuous(labels=scales::percent) +
        plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE,scales="free") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
        
        if (input$kruskallwallisCheckbox) {
          kruskal_results = calculateKWTest(densityDataToHistoBoxRefined, input$singleConVariable, input$catVariableForFill, input$catVariableForSplitting)
          histogramPercentage = addKWToFacetTitle(kruskal_results, histogramPercentage, input$catVariableForSplitting)
        }

        histogramPercentage
    })
    
    output$histoRefinedPercentage = renderPlot({
        req(histoRefinedPercentage())
        histoRefinedPercentage()
        },height=plotHeight)
    
    kdeRefined = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        densityDataToHistoBoxRefined = applyMinMax(densityDataToHistoBoxRefined(),input$singleConVariable,input$xLLKDE,input$xULKDE)
        kdeSplit = ggplot(densityDataToHistoBoxRefined,aes(x=!!sym(input$singleConVariable),fill=!!sym(input$catVariableForFill))) + geom_density(adjust=input$kdeAdjust, alpha=input$kdeTransparency) + ylab("Density") +
        ylim(input$yLLKDE,input$yULKDE) + plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE,scales="free") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
        
        if (input$kruskallwallisCheckbox) {
          kruskal_results = calculateKWTest(densityDataToHistoBoxRefined, input$singleConVariable, input$catVariableForFill, input$catVariableForSplitting)
          kdeSplit = addKWToFacetTitle(kruskal_results, kdeSplit, input$catVariableForSplitting)
        }
        
        kdeSplit
    })
    
    output$kdeRefined = renderPlot({
        req(kdeRefined())
        kdeRefined()
        },height=plotHeight)

    kdeRefinedPercentage = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        densityDataToHistoBoxRefined = applyMinMax(densityDataToHistoBoxRefined(),input$singleConVariable,input$xLLKDE,input$xULKDE)
        kdeSplitPercentage = ggplot(densityDataToHistoBoxRefined,aes(x=!!sym(input$singleConVariable),y=stat(count)/sum(stat(count)),fill=!!sym(input$catVariableForFill))) + geom_density(stat='bin',bins=as.numeric(input$kdeNumOfBinsRefined), alpha=input$kdeTransparency) + ylab("Percent") + scale_y_continuous(labels=scales::percent) +
        plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE,scales="free") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
        
        if (input$kruskallwallisCheckbox) {
          kruskal_results = calculateKWTest(densityDataToHistoBoxRefined, input$singleConVariable, input$catVariableForFill, input$catVariableForSplitting)
          kdeSplitPercentage = addKWToFacetTitle(kruskal_results, kdeSplitPercentage, input$catVariableForSplitting)
        }
        
        kdeSplitPercentage
    })
    
    output$kdeRefinedPercentage = renderPlot({
        req(kdeRefinedPercentage())
        kdeRefinedPercentage()
        },height=plotHeight)
    
    boxplotRefined = reactive({
      
        # for LogY axis prevents log(0) 
        yLLBoxplot = input$yLLBoxplot
        if (yLLBoxplot == 0) {yLLBoxplot = yLLBoxplot + 0.00001}
        yULBoxplot = input$yULBoxplot 
        if (yULBoxplot == 0) {yULBoxplot = yULBoxplot + 0.00001}
        
        boxplotData = applyMinMax(densityDataToHistoBoxRefined(),input$singleConVariable,input$yLLBoxplot,input$yULBoxplot)
        req(nrow(boxplotData)>0)

        normVar = input$normalizationVar
        
        # deduplicate (take 1st row) for group variables
        if (input$singleConVariable %in% c("Group Intensity Sum", 
                                           "Group Intensity Mean", 
                                           paste0("Group Intensity Sum Relative to ",normVar), 
                                           paste0("Group Intensity Mean Relative to ",normVar),
                                           "Group Volume",
                                           paste0("Group Volume Relative to ",normVar),
                                           "Group Surface Area",
                                           "Group Surface Area-to-Volume Ratio")) {
          l = sapply(boxplotData, class)
          categoricalVars = sort(names(l[str_which(l,pattern="character")]))
          categoricalVars = sort(categoricalVars[-which(categoricalVars=="Object ID")])
          boxplotData = boxplotData %>% distinct(!!!syms(categoricalVars), .keep_all = TRUE) 
        }
        
        # apply next min value after 0 for data equal to 0 to avoid NAs at log and beeswarm
        nmin = boxplotData %>% filter(!!sym(input$singleConVariable) > 0) %>% summarise(next_min = min(!!sym(input$singleConVariable))) %>% pull(next_min)
        boxplotData = boxplotData %>% mutate(!!sym(input$singleConVariable) := 
                                               if_else(!!sym(input$singleConVariable) == 0, !!sym(input$singleConVariable) + nmin, !!sym(input$singleConVariable)))
        
        
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        boxplot = ggplot(boxplotData,aes(y=!!sym(input$singleConVariable),x=!!sym(input$catVariableForFill),fill=!!sym(input$catVariableForFill))) + geom_boxplot(varwidth = FALSE, width = input$boxplotBoxWidth, outlier.alpha = 0.3, outlier.size = 0.6) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=0, color="NA") +
        plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE,scales="free") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
        
        if (input$boxplotYScale == "logY") {
          boxplot = boxplot + scale_y_continuous(trans = scales::log_trans(),labels = scales::label_math(e^.x, format = function(x){scales::number(log(x), accuracy = 0.1)}),limits=c(yLLBoxplot,yULBoxplot))
        }
        
        if (input$boxplotDistribution != "list") {
          # downsampling to 10k, otherwise cannot plot esp. beeswarm and also not visible
          if (nrow(boxplotData) > 10000 & input$boxplotDistribution == "geom_beeswarm") {
            sampleBoxplotData = sample_n(boxplotData, 10000)
          } else {
            sampleBoxplotData = boxplotData
          }
          boxplot = boxplot + match.fun(input$boxplotDistribution)(data=sampleBoxplotData,
                                               alpha=input$boxplotPointTransparency,
                                               size=input$boxplotPointSize,
                                               width=0.1,height=0)
        }
        
        if (input$kruskallwallisCheckbox) {
          kruskal_results = calculateKWTest(boxplotData, input$singleConVariable, input$catVariableForFill, input$catVariableForSplitting)
          boxplot = addKWToFacetTitle(kruskal_results, boxplot, input$catVariableForSplitting)
        }
        
        boxplot
    })
    
    output$boxplotRefined = renderPlot({
        req(boxplotRefined())
        boxplotRefined()
        },height=plotHeight)
    
    violinplotRefined = reactive({
        listOfColors = as.list(strsplit(input$hexStrings, ",")[[1]])
        densityDataToHistoBoxRefined = applyMinMax(densityDataToHistoBoxRefined(),input$singleConVariable,input$yLLBoxplot,input$yULBoxplot)
        req(nrow(densityDataToHistoBoxRefined)>0)
        violinplot = ggplot(densityDataToHistoBoxRefined,aes(y=!!sym(input$singleConVariable),x=!!sym(input$catVariableForFill),fill=!!sym(input$catVariableForFill))) + geom_violin(draw_quantiles = c(0.5), width = input$boxplotBoxWidth) +
        plotTheme() + scale_fill_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) + facet_wrap(paste("~", paste("`",input$catVariableForSplitting,"`",sep="")),ncol=input$numColumns,drop=FALSE,scales="free") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
        
        if (input$kruskallwallisCheckbox) {
          kruskal_results = calculateKWTest(densityDataToHistoBoxRefined, input$singleConVariable, input$catVariableForFill, input$catVariableForSplitting)
          violinplot = addKWToFacetTitle(kruskal_results, violinplot, input$catVariableForSplitting)
        }
        
        violinplot
    })
    
    output$violinplotRefined = renderPlot({
        req(violinplotRefined())
        violinplotRefined()
        },height=plotHeight)
    
    # Create a summary table of values from the boxplot
    output$summaryTableFromBoxplot = renderDataTable({

        singleConVariable = input$singleConVariable
        catVariableForFill = input$catVariableForFill
        catVariableForSplitting = input$catVariableForSplitting
        yLLBoxplot = input$yLLBoxplot
        yULBoxplot = input$yULBoxplot
      
        g = ggplot_build(boxplotRefined())
        
        # get stats + mean
        layerData = layer_data(boxplotRefined()) %>% select(lower,middle,upper,PANEL,group,fill)
        layerData = layerData %>% mutate(fill = sapply(fill, as.character))
        boxplot_means_df = as_tibble(g$data[[2]]) %>% select(group,PANEL,"mean"=y,fill)
        boxplot_means_df = boxplot_means_df %>% mutate(fill = sapply(fill, as.character))
        layerData = left_join(layerData, boxplot_means_df,by = c("PANEL", "group", "fill")) %>% mutate_if(is.numeric, round, 3)

        # Panel Assignments
        facet_strip = as_tibble(g$layout$layout)
        layerData = layerData %>% left_join(facet_strip, by = join_by(PANEL == PANEL)) 
        
        # X-Label / Group Assignments
        color_scale = g$plot$scales$get_scales("fill")
        legend_labels = color_scale$get_labels()
        legend_colors = unlist(color_scale$palette(length(legend_labels))[1:length(legend_labels)])
        legend_info = data.frame(legend_labels,legend_colors,check.names = FALSE)
        names(legend_info) <- c(catVariableForFill, "fill")
        layerData = layerData %>% left_join(legend_info, by = join_by(fill == fill))
        
        # bring count data n()
        bx_data = applyMinMax(densityDataToHistoBoxRefined(),singleConVariable,yLLBoxplot,yULBoxplot)
        countData = bx_data %>% group_by(!!sym(catVariableForSplitting),!!sym(catVariableForFill)) %>% summarise(n = n(), .groups="drop")
        layerData = layerData %>% 
          left_join(countData, by = c(setNames(catVariableForSplitting,catVariableForSplitting),
                                      setNames(catVariableForFill,catVariableForFill)))
        
        # Select then rename the columns
        boxDataToDisplay = layerData %>% 
          select(!!sym(catVariableForSplitting),!!sym(catVariableForFill),lower,middle,mean,upper,n) %>% 
          rename(Lower = lower, Middle = middle, Mean = mean, Upper = upper)

        DT::datatable(boxDataToDisplay, extensions = "FixedColumns",plugins = "natural",options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, fixedColumns = list(leftColumns = 2)))
    })
    
    # calculate Kruskall-Wallis test
    calculateKWTest <- function(df, value_col, group_col, split_col) {
      
      # remove NAs in value_col
      df = df %>% filter(!is.na(!!sym(value_col)))
      
      # keep unique categories
      unique_categories = df %>% pull(!!sym(split_col)) %>% unique()
      
      kruskal_results = df %>%
        group_by(!!sym(split_col)) %>%
        filter(n_distinct(!!sym(group_col)) == 2) %>%
        group_split() %>%
        map_dfr(~{
          if (nrow(.x) > 0) {
            kruskal_test = kruskal.test(.x[[value_col]] ~ .x[[group_col]])
            tibble(
              !!sym(split_col) := unique(.x[[split_col]]),
              p_value = kruskal_test$p.value,
              statistic = kruskal_test$statistic,
              n = nrow(.x)
            )
          }
        })
      
      # add Bonferroni correction (check)
      if (nrow(kruskal_results) > 0) {
        kruskal_results = kruskal_results %>%
          mutate(
            corrected_p_value = pmin(p_value * n(), 1)
          )
        
        # create DF with empty values for remaining facets
        # indicates that in those facets num.groups=1 or >2, cannot do KW test
        # or that all values are identical
        rest_categories = 
          as_tibble(unique_categories) %>% 
          filter(!(value %in% kruskal_results[[split_col]])) %>%
          pull(value) %>% unique()
        
        rest_results = tibble(
          !!sym(split_col) := rest_categories,
          p_value = NA,
          statistic = NA,
          n = NA,
          corrected_p_value = NA
        )
        
        kruskal_results = kruskal_results %>% union(rest_results)
        
      } else if (nrow(kruskal_results) == 0){
        # create DF with empty values for all facets
        kruskal_results = tibble(
          !!sym(split_col) := unique_categories,
          p_value = NA,
          statistic = NA,
          n = NA,
          corrected_p_value = NA
        )
      }
      
      # display <0.0001 for small values
      kruskal_results = kruskal_results %>% 
        mutate(`p-value` = ifelse(`p_value` < 0.0001, "<0.0001", sprintf("%.4f", `p_value`))) %>% 
        mutate(`corrected p-value` = ifelse(`corrected_p_value` < 0.0001, "<0.0001", sprintf("%.4f", `corrected_p_value`))) %>%
        select(-`p_value`) %>% select(-`corrected_p_value`)
      
      # show modal KW not possible
      if (sum(is.na(kruskal_results$statistic)) == nrow(kruskal_results)) {
        showModal(modalDialog(
          title = "Number of groups in the panel(s) is not equal to 2, cannot perform Kruskall-Wallis test.",
          easyClose = TRUE
        ))
      }
      
      return(kruskal_results)
    }

    # Create a summary table of values from 1D plot incl. Kruskall-Wallis test
    output$kwTable1D = renderDataTable({
      
      # display stats only if Kruskall-Wallis checkbox was enabled
      if (input$kruskallwallisCheckbox) {
        
        value_col = input$singleConVariable
        group_col = input$catVariableForFill
        split_col = input$catVariableForSplitting
        
        df = densityDataToHistoBoxRefined()
        
        kruskal_results = calculateKWTest(df, value_col, group_col, split_col)

        rowCallback <- c(
          "function(row, data){",
          "  for(var i=0; i<data.length; i++){",
          "    if(data[i] === null){",
          "      $('td:eq('+i+')', row).html('NA')",
          "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
          "    }",
          "  }",
          "}"  
        )
        
        DT::datatable(kruskal_results, extensions = "FixedColumns", plugins = "natural", 
                      options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, 
                                     fixedColumns = list(leftColumns = 2),
                                     rowCallback = JS(rowCallback))) %>% 
          formatRound(columns = "statistic", digits = 3)
      }
    })
    
    # Create summary table statistics for ANOVA test
    output$anovaTable1D = renderDataTable({
      
      if (input$anovaCheckbox) {
        
        value_col = input$singleConVariable
        group_col = input$catVariableForFill
        
        # keep unique categories
        unique_categories = densityDataToHistoBoxRefined() %>% pull(!!sym(input$catVariableForSplitting)) %>% unique()
        
        # remove NAs in value_col
        densityDataToHistoBoxRefined = densityDataToHistoBoxRefined() %>% filter(!is.na(!!sym(value_col)))
        
        anova_results = densityDataToHistoBoxRefined %>%
          group_by(!!sym(input$catVariableForSplitting)) %>%
          filter(n_distinct(!!sym(group_col)) > 1) %>%
          group_split() %>%
          map_dfr(~{
            if (nrow(.x) > 0) {
              anova_test = aov(.x[[value_col]] ~ .x[[group_col]])
              anova_summary = summary(anova_test)
              tibble(
                !!sym(input$catVariableForSplitting) := unique(.x[[input$catVariableForSplitting]]),
                p_value = anova_summary[[1]]$`Pr(>F)`[1],
                statistic = anova_summary[[1]]$`F value`[1],
                n = nrow(.x)
              )
            }
          })
        
        if (nrow(anova_results) > 0) {
          
          rest_categories = 
            as_tibble(unique_categories) %>% 
            filter(!(value %in% anova_results[[input$catVariableForSplitting]])) %>%
            pull(value) %>% unique()
          
          rest_results = tibble(
            !!sym(input$catVariableForSplitting) := rest_categories,
            p_value = NA,
            statistic = NA,
            n = NA
          )
          
          anova_results = anova_results %>% union(rest_results)
          
        } else if (nrow(anova_results) == 0){
          anova_results = tibble(
            !!sym(input$catVariableForSplitting) := unique_categories,
            p_value = NA,
            statistic = NA,
            n = NA
          )
        }
        
        # display <0.0001 for small values
        anova_results = anova_results %>% 
          mutate(`p-value` = ifelse(`p_value` < 0.0001, "<0.0001", sprintf("%.4f", `p_value`))) %>% 
          select(-`p_value`)
        
        
        rowCallback <- c(
          "function(row, data){",
          "  for(var i=0; i<data.length; i++){",
          "    if(data[i] === null){",
          "      $('td:eq('+i+')', row).html('NA')",
          "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
          "    }",
          "  }",
          "}"  
        )
        
        DT::datatable(anova_results, extensions = "FixedColumns", plugins = "natural", 
                      options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, 
                                     fixedColumns = list(leftColumns = 2),
                                     rowCallback = JS(rowCallback))) %>% 
          formatRound(columns = "statistic", digits = 3)
        
      }  
    })
    
    # Boxplot/Violing tab show/hide UI elements based on selection
    observe({ 
      if(input$tabs1Dplots == "Boxplots") {
        shinyjs::show(id = "boxplotDistribution")
        shinyjs::show(id = "boxplotYScale")
        shinyjs::show(id = "boxplotPointSize")
        shinyjs::show(id = "boxplotPointTransparency")
      } else if(input$tabs1Dplots == "Violin Plots"){
        shinyjs::hide(id = "boxplotDistribution")
        shinyjs::hide(id = "boxplotYScale")
        shinyjs::hide(id = "boxplotPointSize")
        shinyjs::hide(id = "boxplotPointTransparency")
      }
    })
    
    # Create the option to apply bins to the data then remove them if desired
    groupVarName <<- "__reset"
    observeEvent(input$addBins,{
        if(groupVarName=="__reset"){
            req(reactiveDF$filteredDataset)
        
            # Retrieve the breaks
            breaksForBinning = as.list(strsplit(input$binCuts, ",")[[1]])
            # Make a new group variable name
            groupVarName <<- paste("Bin - ",toString(input$binningVariable))

            # Apply the breaks
            # TODO: fix the lower threshold (incorrect bin assignment)
            dataToBin = reactiveDF$filteredDataset
            binnedDataset = dataToBin %>% mutate(Group=cut(!!sym(input$binningVariable),breaks=breaksForBinning),
                                                 Group=forcats::fct_explicit_na(Group,paste("> ",toString(tail(breaksForBinning, n=1)))))
            
            # keep threshold only
            reactiveDF$filteredDataset = binnedDataset %>% rename(!!groupVarName:=Group)
            
            # Turn the imported/processed data into a format that can be filtered and subsetted
            # Allow the option to have only data within the ranges or as a full series of thresholds / cut points
            # if (input$rangeOrGroups == "threshold"){
            #     reactiveDF$filteredDataset = binnedDataset %>% rename(!!groupVarName:=Group)
            # }
            # else if (input$rangeOrGroups == "range"){
            #     reactiveDF$filteredDataset = binnedDataset %>% filter(Group != paste("> ",toString(tail(breaksForBinning, n=1)))) %>% rename(!!groupVarName:=Group)
            # }
            
            
            # output$filteredDatasetForFilterTab = renderDataTable({
            #     DT::datatable(reactiveDF$filteredDataset,
            #                   extensions="FixedColumns",
            #                   plugins="natural",
            #                   options=list(scrollX=TRUE,scroll ="500px",
            #                                  scrollCollapse=TRUE,fixedColumns=list(leftColumns = 4)))
            # })
            # Update the categorical variable selection to include groups (previously called bins)
            subsettableData = reactiveDF$filteredDataset
            l = sapply(subsettableData, class)
            categoricalVars = names(l[str_which(l,pattern="character")])
            categoricalVars = categoricalVars[-which(categoricalVars=="Object ID")]
            updateSelectInput(session,"catVariableForFill",choices=c(categoricalVars,groupVarName), selected=categoricalVars[1])
            updateSelectInput(session,"catVariableForSplitting",choices=c(categoricalVars,groupVarName), selected=categoricalVars[2])
            updateSelectInput(session,"scatterCatColor",choices=c(categoricalVars,groupVarName), selected=categoricalVars[1])
            updateSelectInput(session,"scatterCatFacet",choices=c(categoricalVars,groupVarName), selected=categoricalVars[2])
        }
        else if (groupVarName == paste("Group - ",toString(input$binningVariable))){
            # req(reactiveDF$filteredDataset)
            # output$filteredDatasetForFilterTab = renderDataTable({
            #     DT::datatable(reactiveDF$filteredDataset,
            #                   extensions="FixedColumns",
            #                   plugins="natural",
            #                   options=list(scrollX=TRUE,scroll ="500px",
            #                                  scrollCollapse=TRUE,fixedColumns=list(leftColumns = 4)))
            # })
        }
    },ignoreNULL=TRUE)
    
    observeEvent(input$removeBins,{
        groupVarName <<- "__reset"
        reactiveDF$filteredDataset = reactiveDF$filteredDatasetRef
        subsettableData = reactiveDF$filteredDataset
        output$oneDTableView = renderDataTable({NULL})
        l = sapply(subsettableData, class)
        categoricalVars = names(l[str_which(l,pattern="character")])
        categoricalVars = categoricalVars[-which(categoricalVars=="Object ID")]
        updateSelectInput(session,"catVariableForFill",choices=categoricalVars,selected=catVariableForFill_Reference)
        updateSelectInput(session,"catVariableForSplitting",choices=categoricalVars,selected=catVariableForSplitting_Reference)
        updateSelectInput(session,"scatterCatColor",choices=categoricalVars,selected=scatterCatColor_Reference)
        updateSelectInput(session,"scatterCatFacet",choices=categoricalVars,selected=scatterCatFacet_Reference)
    },ignoreNULL=TRUE)
    
    
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
                downloadButton("downloadViolinPlots","Download Violin Plots"),
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
    
    output$downloadViolinPlots = downloadHandler(
        filename = function() {
            paste(format(Sys.time(), "ViolinPlot_Date_%Y_%m_%d_Time_%H%M%S"), ".png", sep = "")
        },
        content = function(file) {
            req(violinplotRefined())
            ggsave(file,plot=violinplotRefined(),device='png',width=input$plotWidthForDownload,height=input$plotHeightForDownload,units="cm",dpi=input$plotResolution)
        }
    )
    
    # Output scatterplot parameters
    densityDataToScatterParams = eventReactive(input$scatterParams,{
        return(reactiveDF$filteredDataset)
    },ignoreNULL=TRUE)
    
    # Generate data for the scatterplot
    observe({
        dataForPlotParams = densityDataToScatterParams()

        # ensure NA from X and Y are removed
        dataForPlotParams = dataForPlotParams %>% filter(!is.na(!!sym(input$scatterX)) & !is.na(!!sym(input$scatterY)))
        if (nrow(dataForPlotParams) == 0) {
          showModal(modalDialog(
            title = "Data is NA in both X and Y variables. Use other variables.",
            easyClose = TRUE
          ))
        }
        req(nrow(dataForPlotParams) > 0, message = "No available data.")
        
        xRangeScatter = dataForPlotParams %>%
          summarize(min_value = min(!!sym(input$scatterX), na.rm = TRUE), 
                    max_value = max(!!sym(input$scatterX), na.rm = TRUE)) %>% unlist() %>% as.array()
        
        yRangeScatter = dataForPlotParams %>%
          summarize(min_value = min(!!sym(input$scatterY), na.rm = TRUE), 
                    max_value = max(!!sym(input$scatterY), na.rm = TRUE)) %>% unlist() %>% as.array()
        
        names(xRangeScatter) <- NULL
        names(yRangeScatter) <- NULL
        
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
        req(input$scatterCatFacet)
        subsettableDataForScatter = reactiveDF$filteredDataset
        # !! Do we need to filter for only the columns of interest first before creating the plots?
        filteredDataForScatter = subsettableDataForScatter ## %>% select(`Image File`,`Object ID`,!!sym(input$scatterY),!!sym(input$scatterX),!!sym(input$scatterCatColor),!!sym(input$scatterCatFacet))
        
        # Save the plotting parameter values that were inputted so they can be referenced in the future
        scatterX_Reference <<- input$scatterX
        scatterY_Reference <<- input$scatterY
        scatterCatColor_Reference <<- input$scatterCatColor
        scatterCatFacet_Reference <<- input$scatterCatFacet
        
        return(filteredDataForScatter)
    },ignoreNULL=TRUE)
    
    # Make an option to choose whether the scatterplot is contoured or not
    output$scatterColorSelect = renderUI({
        req(input$contourCheckbox==FALSE)
        req(reactiveDF$filteredDataset)
        subsettableData = reactiveDF$filteredDataset
        l = sapply(subsettableData, class)
        categoricalVars = names(l[str_which(l,pattern="character")])
        categoricalVars = categoricalVars[-which(categoricalVars=="Object ID")]
        continuousVars = names(l[str_which(l,pattern="numeric")])
        selectInput(inputId = "scatterCatColor","Categorical Variable for Color",choices = categoricalVars)
    })
    
    # Create the scatterplot so that it is either contoured or standard
    scatterPlot = reactive({
        req(densityDataToScatter())
        listOfColors = as.list(strsplit(input$scatterplotHexStrings, ",")[[1]])
        
        # ensure NA from X and Y are removed
        densityDataToScatter = densityDataToScatter() %>% filter(!is.na(!!sym(input$scatterX)) & !is.na(!!sym(input$scatterY)))
        if (nrow(densityDataToScatter) == 0) {
          showModal(modalDialog(
            title = "Data is NA in both X and Y variables. Use other variables.",
            easyClose = TRUE
          ))
        }
        req(nrow(densityDataToScatter) > 0, message = "No available data.")

        # apply X/Y limits separately
        densityDataToScatter = densityDataToScatter %>% filter(!!sym(input$scatterY) >= input$yLLScatter & 
                                                               !!sym(input$scatterY) <= input$yULScatter & 
                                                               !!sym(input$scatterX) >= input$xLLScatter &
                                                               !!sym(input$scatterX) <= input$xULScatter)
        
        if(input$contourCheckbox==FALSE){
            scatterPlotStub = 
            ggplot(densityDataToScatter,
                   aes(y=!!sym(input$scatterY),
                       x=!!sym(input$scatterX),
                       color=!!sym(input$scatterCatColor))) +
            geom_point(alpha=input$scatterplotTransparency) +
            facet_wrap(paste("~", paste("`",input$scatterCatFacet,"`",sep="")),ncol=input$scatterNumColumns,drop=FALSE, scales = "free") +
            scatterplotTheme() +
            scale_color_manual(values=lapply(listOfColors,function(x){str_replace_all(x," ", "")})) +
            theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
            theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
        } else {
            scatterPlotStub = 
            ggplot(densityDataToScatter,
                   aes(y=!!sym(input$scatterY),
                       x=!!sym(input$scatterX))) +
            stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white",contour_var = "ndensity") +
            facet_wrap(paste("~", paste("`",input$scatterCatFacet,"`",sep="")),ncol=input$scatterNumColumns,drop=FALSE, scales = "free") +
            scatterplotTheme() +
            scale_fill_continuous(type = input$contourColor) +
            theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
            theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
        }
        
        # for LogY axis prevents log(0)
        yLLScatter = input$yLLScatter 
        if (yLLScatter == 0) {yLLScatter = yLLScatter + 0.00001}
        yULScatter = input$yULScatter
        if (yULScatter == 0) {yULScatter = yULScatter + 0.00001}

        # for LogX axis prevents log(0)
        xLLScatter = input$xLLScatter 
        if (xLLScatter == 0) {xLLScatter = xLLScatter + 0.00001}
        xULScatter = input$xULScatter
        if (xULScatter == 0) {xULScatter = xULScatter + 0.00001}
        
        # add log X / log Y
        scatterPlotStub = scatterPlotStub +
          {
            if (input$scatterplotXScale == "logX") scale_x_continuous(trans = scales::log_trans(),labels = scales::label_math(e^.x, format = function(x){scales::number(log(x), accuracy = 0.1)}),limits=c(xLLScatter,xULScatter))
          } +
          {
            if (input$scatterplotYScale == "logY") scale_y_continuous(trans = scales::log_trans(),labels = scales::label_math(e^.x, format = function(x){scales::number(log(x), accuracy = 0.1)}),limits=c(yLLScatter,yULScatter))
          }
        
        # add Pearson correlation
        scatterPlotStub = scatterPlotStub +
          {
            if (input$scatterplotPearsonCheckbox) stat_cor(method = "pearson", size = 3.5, show.legend = FALSE)
          }
        
        scatterPlotStub
    })
    
    output$scatter = renderPlot({
        req(scatterPlot())
        scatterPlot()
        },height=scatterplotHeight)
    
    # Create a table of the scatterplot data for viewing
    scatterplotDataTable = eventReactive(input$plotScatter,{
        req(densityDataToScatter())
        scatterDataTableDataPreFilter = densityDataToScatter()
        scatterDataTableDataPostFilter = scatterDataTableDataPreFilter %>%
                                            filter(!!sym(input$scatterY) >= input$yLLScatter) %>%
                                            filter(!!sym(input$scatterY) <= input$yULScatter) %>%
                                            filter(!!sym(input$scatterX) >= input$xLLScatter) %>%
                                            filter(!!sym(input$scatterX) <= input$xULScatter)
        return(as.data.frame(scatterDataTableDataPostFilter))
    },ignoreNULL=TRUE)
    output$scatterplotTable = renderDataTable({
        DT::datatable(scatterplotDataTable(), options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE))
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
    
    # Create the option to download the scatterplot data
    output$downloadScatterplotData = downloadHandler(
      filename = function() {
        paste(format(Sys.time(), "ScatterplotData_Date_%Y_%m_%d_Time_%H%M%S"), ".csv", sep = "")
      },
      content = function(file) {
        req(densityDataToScatter())
        log_scatterX = paste("Log","(",input$scatterX,")",sep="")
        log_scatterY = paste("Log","(",input$scatterY,")",sep="")
        scatterPlotDataStub = densityDataToScatter()
        scatterPlotDataStub = scatterPlotDataStub %>%
          filter(!!sym(input$scatterY) >= input$yLLScatter) %>%
          filter(!!sym(input$scatterY) <= input$yULScatter) %>%
          filter(!!sym(input$scatterX) >= input$xLLScatter) %>%
          filter(!!sym(input$scatterX) <= input$xULScatter) %>%
          mutate(!!sym(log_scatterX) := log(!!sym(input$scatterX)), !!sym(log_scatterY) := log(!!sym(input$scatterY)))
        
        if (!input$contourCheckbox & !is.null(input$scatterCatColor)) { 
          scatterPlotDataStub = scatterPlotDataStub %>% select("Image File","Treatment", "Genotype",!!sym(input$scatterX),!!sym(input$scatterY),!!sym(input$scatterCatFacet),!!sym(input$scatterCatColor),!!sym(log_scatterX),!!sym(log_scatterY)) 
        } else {
          scatterPlotDataStub = scatterPlotDataStub %>% select("Image File","Treatment", "Genotype",!!sym(input$scatterX),!!sym(input$scatterY),!!sym(input$scatterCatFacet),!!sym(log_scatterX),!!sym(log_scatterY)) 
        }
        write.csv(scatterPlotDataStub, file, row.names = FALSE)
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
    
    # Create a summary table of values from the scatterplot incl. Pearson correlation
    output$summaryTableFromScatterplot = renderDataTable({
      
      # display stats only if Pearson checkbox was enabled
      if (input$scatterplotPearsonCheckbox) {
        req(densityDataToScatter())
          
        g = ggplot_build(scatterPlot())
        pearsonData = as_tibble(g$data[[2]])
        pearsonData = pearsonData %>% mutate(colour = sapply(colour, as.character))
        
        facet_strip = as_tibble(g$layout$layout)
        
        scatterPlotPearsonData = pearsonData %>% 
          left_join(facet_strip, by = join_by(PANEL == PANEL)) 
        
        scatterPlotDataStub = densityDataToScatter()
        sc_data = scatterPlotDataStub %>%
          filter(!!sym(input$scatterY) >= input$yLLScatter) %>%
          filter(!!sym(input$scatterY) <= input$yULScatter) %>%
          filter(!!sym(input$scatterX) >= input$xLLScatter) %>%
          filter(!!sym(input$scatterX) <= input$xULScatter)
        
        if (!input$contourCheckbox & !is.null(input$scatterCatColor)) { 
          color_scale = g$plot$scales$get_scales("colour")
          legend_labels = color_scale$get_labels()
          legend_colors = unlist(color_scale$palette(length(legend_labels))[1:length(legend_labels)])
          legend_info = data.frame(legend_labels,legend_colors,check.names = FALSE)
          names(legend_info) <- c(input$scatterCatColor, "colour")
          
          countData = sc_data %>% group_by(!!sym(input$scatterCatFacet),!!sym(input$scatterCatColor)) %>% 
            summarise(n = n(), .groups="drop")
          
          scatterPlotPearsonData = scatterPlotPearsonData %>% 
            left_join(legend_info, by = join_by(colour == colour))
          
          scatterPlotPearsonData = left_join(scatterPlotPearsonData, countData, 
                                             by = c(setNames(input$scatterCatColor,input$scatterCatColor),
                                                    setNames(input$scatterCatFacet,input$scatterCatFacet)))
          
          scatterPlotPearsonData = scatterPlotPearsonData %>%
            select(!!sym(input$scatterCatFacet),!!sym(input$scatterCatColor),estimate,p.value,n)
        } else {
          countData = sc_data %>% group_by(!!sym(input$scatterCatFacet)) %>% summarise(n = n())
          scatterPlotPearsonData = left_join(scatterPlotPearsonData, countData, by = setNames(input$scatterCatFacet,input$scatterCatFacet))
          scatterPlotPearsonData = scatterPlotPearsonData %>% select(!!sym(input$scatterCatFacet),estimate,p.value,n)
        }
        
        # display <0.0001 for small values
        scatterPlotPearsonData = scatterPlotPearsonData %>% 
          mutate(`p-value` = ifelse(`p.value` < 0.0001, "<0.0001", sprintf("%.4f", `p.value`))) %>% 
          select(-`p.value`) %>%
          rename("Pearson's R" = "estimate")
        
        DT::datatable(scatterPlotPearsonData, extensions = "FixedColumns", plugins = "natural", 
                      options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse=TRUE, 
                                     fixedColumns = list(leftColumns = 2))) %>% 
          formatRound(columns = "Pearson's R", digits = 3)
      }
    })
    
}

