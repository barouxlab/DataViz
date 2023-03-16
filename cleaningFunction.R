# This cleaning function accepts a top level directory string designation as an input,
# then cleans the data according to the needs described by the Baroux Lab.
# It accepts a top level directory string as it is intended to operate on unzipped files that contain
# raw CSV data from the Imaris program. The organization of these files is specific, and should match the
# example zipped data provided with this repository.
# The basic functionality is to go to each CSV within the directory structure, extract the pertinent data,
# then join all CSV's into a single, final CSV.

cleaningFunction = function(inputTopLevelDirectory){
    # Instantiate a function that analyzes / cleans all of the files within the lowest file directory.
    analyzeLowestDir = function(directoryPath){
    
        # Extract the metadata of interest from the directory name.
        # !! In other words: the names of the directories will determine variable names in the cleaned data.
        # !! As such, make sure to copy the format of the directory names from the supplied Zip file.
        listOfSplitStrings = strsplit(directoryPath,.Platform$file.sep)[[1]]
        numOfSplitStrings = length(listOfSplitStrings)
        
        # Prepare the string information to add to the final data frame.
        # The editedFinalString variable is the Image File name.
        editedFinalString = listOfSplitStrings[numOfSplitStrings] %>% str_replace("_Statistics","")
        lightOrDarkString = listOfSplitStrings[numOfSplitStrings-1]
        experimentNumberString = listOfSplitStrings[numOfSplitStrings-2]
    
        # List all of the files in the lowest level directory to pull all information together.
        listOfFiles = list.files(path=directoryPath,full.names=TRUE)
    
        # Input the default statistic file names to search for.
        # !! These statistics determine the statistics contained in the output data; i.e., the zipped file
        # of data should contain these strings in order for the function to work properly.
        statFileNames = c("Intensity_Mean","Intensity_StdDev","Intensity_Sum")
    
        # Return a list of lists, matching each file name, and detailing each group
        # of DF's that need to be joined initially.
        # In other words, all data is prepared as a list of data frames that will be joined.
        listOfLists = lapply(statFileNames,function(s)(str_subset(listOfFiles,pattern=s)))
        # If one of the principal 3 statistics is missing from the dataset, remove its placeholder
        # so as to avoid bugs
        listOfLists = Filter(length, listOfLists)
        
        # If any of these files is empty, remove it
        listOfLists = lapply(listOfLists,function(l)(discard(l,function(t)(suppressWarnings(suppressMessages(nrow(read_csv(t,skip=3))==0))))))
    
        # Indicate the string indicators for the columns to keep.
        # These strings determine which columns of the raw data will be kept. If a column from the 
        # original / unzipped data contains one of these strings, it will be included as a column
        # in the cleaned data.
        stringIndicators = "Intensity|Category|Channel|Image|Surpass|Time|ID|Overall|Position|Shortest|Surfaces|Sphericity|Volume|Area|Average|Diameter|Distance|Ellipticity|Spots"
    
        # Join each of the data frames together.
        # !! Optionally: don't suppress the messages for the read.csv function.
        listOfJoinedDFs = lapply(listOfLists,
        function(l)(l %>% map(~suppressWarnings(suppressMessages(read_csv(.,skip=3))))
                      %>% map(~select(.,matches(stringIndicators))) 
                      %>% reduce(full_join)
                   )
            )

        fullPreJoin = listOfJoinedDFs %>% reduce(full_join)
                             
        # List the rest of the CSV's that need to be imported, then import them and merge them.
        remainingFileNames = listOfFiles %>% str_subset(pattern=paste(statFileNames,collapse="|"),negate=TRUE)
        listOfRemainingFiles = lapply(remainingFileNames,function(s)(suppressWarnings(suppressMessages(read_csv(s,skip=3))))) %>% map(~select(.,matches(stringIndicators))) 
                                  
        # Drop files that are empty.
        filesToMerge = discard(listOfRemainingFiles,function(t)(nrow(t)==0))
    
        # Merge all remaining files.
        remainingMerge = filesToMerge %>% reduce(full_join)
        finalJoin = full_join(fullPreJoin,remainingMerge)
    
        # Add all of the string metadata on the particular experiment (extracted from the lowest level directory string).
        dataToReturn = finalJoin
        dataToReturn["ImageID"] = editedFinalString
        dataToReturn["Treatment"] = lightOrDarkString
        dataToReturn["Genotype"] = experimentNumberString
        return(relocate(dataToReturn,"ID","Category","Channel","Surpass Object","Time","Image"))
    }

    # List the names of the subdirectories so you can then apply the function to each one of them.
    subDirectoriesOfInterest = unique(dirname(list.files(inputTopLevelDirectory,recursive=TRUE,full.names=TRUE)))

    # Apply the function to all of the files in each of the specified directories.
    dataToCat = suppressMessages(lapply(subDirectoriesOfInterest,analyzeLowestDir))

    # Join all of the separate files and make name edits as necessary
    dataToCoalesce = suppressMessages(dataToCat %>% reduce(full_join)) %>% relocate(c("ImageID","Treatment","Genotype")) %>%
                               rename(`Image File`=ImageID, `Object ID`=ID, `Image Subset`=Image)
    
    # Augment the Shortest Distance to Surfaces Columns via a pivot then a coalesce to merge jagged rows
    
    # Make a coalesce by column function for the coalesce step
    coalesce_by_column = function(df) {
        return(dplyr::coalesce(!!! as.list(df)))
    }

    dataToPivot = dataToCoalesce %>%
                               group_by(`Image File`,`Treatment`,`Object ID`,`Category`,`Channel`,`Surpass Object`,`Surfaces`) %>% 
                               summarise_all(coalesce_by_column) %>% ungroup()
    
    pivotColumnList = c("Shortest Distance to Surfaces","Overlapped Volume to Surfaces","Overlapped Volume Ratio to Surfaces")
    
    finalData = dataToPivot %>% distinct() %>%
                               pivot_wider(names_from="Surfaces",names_glue = "{.value} - {Surfaces}", values_from=pivotColumnList) %>%
                               dplyr::select(-c(`Shortest Distance to Surfaces - NA`)) %>% dplyr::select(-c(`Overlapped Volume to Surfaces - NA`)) %>%
                               dplyr::select(-c(`Overlapped Volume Ratio to Surfaces - NA`))
    
    # Augment the Shortest Distance to Spots Columns in the same way
    if("Shortest Distance to Spots" %in% colnames(finalData)){
        finalData = finalData %>% 
                    pivot_wider(names_from="Spots",values_from="Shortest Distance to Spots",names_prefix="Distance to Spot ") %>% dplyr::select(-c(`Distance to Spot NA`))
    }
    
    # Perform final data type changes, column movements, and column name changes
    finalData = finalData %>% relocate(c("Image Subset","Time"),.after=last_col())
    finalData$Channel = as.character(finalData$Channel)
    finalData$`Object ID` = as.character(finalData$`Object ID`)
    finalData$`Time` = as.character(finalData$`Time`)
    finalData = finalData %>% rename(`Object`= "Surpass Object")
    
    if("Average Distance To 3 Nearest Neighbours" %in% colnames(finalData)){
        finalData = finalData %>% rename(`Distance To 3 Nearest Neighbours`= "Average Distance To 3 Nearest Neighbours")
        finalData$`Distance To 3 Nearest Neighbours` = as.numeric(finalData$`Distance To 3 Nearest Neighbours`)
    }
                               
    if("Average Distance To 5 Nearest Neighbours" %in% colnames(finalData)){
        finalData = finalData %>% rename(`Distance To 5 Nearest Neighbours`= "Average Distance To 5 Nearest Neighbours")
        finalData$`Distance To 5 Nearest Neighbours` = as.numeric(finalData$`Distance To 5 Nearest Neighbours`)
    }
    
    if("Average Distance To 9 Nearest Neighbours" %in% colnames(finalData)){
        finalData = finalData %>% rename(`Distance To 9 Nearest Neighbours`= "Average Distance To 9 Nearest Neighbours")
        finalData$`Distance To 9 Nearest Neighbours` = as.numeric(finalData$`Distance To 9 Nearest Neighbours`)
    }
    
    # Perform column specific edits on strings for data unification
    finalData$`Object`[finalData$`Object`=="Nucleus_centre_of_mass"] = "Nucleus Center of Mass"
    finalData$`Object`[finalData$`Object`=="Nucleus center of mass"] = "Nucleus Center of Mass"
    finalData$`Object`[finalData$`Object`=="Nucleus Center Of Mass"] = "Nucleus Center of Mass"
                                   
    # Fill NA's across multiple channel observations
    finalDataToFill = finalData %>% relocate(c("Genotype","Treatment","Image File"))
    namesForFilling = colnames(finalDataToFill)
    colsToFill = namesForFilling [! namesForFilling %in% fillingVarsToRemove]
    filledDataToNARemove = finalDataToFill %>% group_by(`Image File`,`Treatment`,`Object ID`,`Category`,`Object`) %>% 
                               fill(all_of(colsToFill),.direction = c("downup")) %>% ungroup()
    
    # Remove observations with NA in the Channel column
    finalDataToReturn = filledDataToNARemove %>% filter(!is.na(Channel))
    
    # Return the final dataset
    return(finalDataToReturn)
}

