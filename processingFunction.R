# This function accepts the cleaned data table/frame as an input, then outputs the processed data.
# The "processed data" is defined as the original cleaned data with added custom variables that are
# made (i.e., processed) from the original variables.

processingFunction = function(importedData,varsToInclude){
    # Before any processing, drop columns with no names
    if("" %in% colnames(importedData)){
        importedData = importedData %>% dplyr::select(-c(""))
    }
    
    # First: fill the NA's for the Nucleus Center of Mass objects to a number for normalization.
    # 1 was chosen arbitrarily, and can be changed in future iterations of the code as necessary.
    # These values are filled because the data itself should not be missing for subsequent operations.
    # Moreover, the names of variables (e.g., "Nucleus center of mass") are unified.
    if("Nucleus Center of Mass" %in% unique(importedData$`Object`)){
        importedData$`Distance to Nucleus`[is.na(importedData$`Object`=="Nucleus Center of Mass" & importedData$`Distance to Nucleus`=="NA")] = 1
}
    
    # Filter out any group that doesn't have a nucleus center of mass object or where Channel is NA (provided
    # that a nucleus center of mass is included in the dataset)
    if("Nucleus Center of Mass" %in% unique(importedData$`Object`)){
        dataToProcess = importedData %>% group_by(`Image File`) %>% filter(any(`Object`=="Nucleus Center of Mass")) %>% 
                        filter(`Channel`!="NA") %>% ungroup()
    } else {
        dataToProcess = importedData
    }
    
    # Perform the variable normalizations in a "cascading fashion".
    # I.e., perform one normalization, add the column to the data frame, then perform the next, add it to
    # the data frame as well, then so on and so forth.
    
    # The "Normalized Distance to Surfaces" variable is also added here, using the (potentially filled) data from each "Nucleus Center of Mass observation"
    # !! This is an area of development as there are, potentially, multiple types of surfaces that 
    # !! can be handled by this computation
    
    # !! Wrap each variable in if/then logic to maintain the ability to select it
    
    # Normalized Intensity Sum
    if ("Normalized Intensity Sum" %in% varsToInclude & ("Intensity Sum" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
                        mutate("Normalized Intensity Sum" = `Intensity Sum`/`Intensity Sum`[which(`Object`=="Nucleus")]) %>% 
                        ungroup()
        }
    
    # Normalized Intensity Mean
    if ("Normalized Intensity Mean" %in% varsToInclude & ("Intensity Mean" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
                        mutate("Normalized Intensity Mean" = `Intensity Mean`/`Intensity Mean`[which(`Object`=="Nucleus")]) %>% 
                        ungroup()
        }
    
    # Normalized Intensity StdDev
    if ("Normalized Intensity StdDev" %in% varsToInclude & ("Intensity StdDev" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
                        mutate("Normalized Intensity StdDev" = `Intensity StdDev`/`Intensity StdDev`[which(`Object`=="Nucleus")]) %>% 
                        ungroup()
        }
    
    # Normalized Distance to Nucleus
    if (("Normalized Distance to Nucleus" %in% varsToInclude) & ("Nucleus Center of Mass" %in% unique(dataToProcess$`Object`)) & ("Distance to Nucleus" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
                        mutate("Normalized Distance to Nucleus" = `Distance to Nucleus`/`Distance to Nucleus`[which(`Object`=="Nucleus Center of Mass")]) %>% 
                        ungroup()
        }
    
    
    # Add the group intensity variables
    
    # Group Intensity Sum
    if ("Group Intensity Sum" %in% varsToInclude & ("Intensity Sum" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,Channel,Category,`Object`) %>% 
                        mutate("Group Intensity Sum" = sum(`Intensity Sum`)) %>% 
                        ungroup()
        }
    
    # Group Intensity Mean
    if ("Group Intensity Mean" %in% varsToInclude & ("Intensity Mean" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,Channel,Category,`Object`) %>% 
                        mutate("Group Intensity Mean" = sum(`Intensity Mean`)) %>% 
                        ungroup()
        }
    
    
    # The "Normalized Intensity Sum Ratio Ch2:Ch1" and "Normalized Intensity Mean Ratio Ch2:Ch1" variables are
    # added here.
    # !! For now, the channel ratios are set to be 2:1; future iterations of the code could automate these variables
    # to use any number of channels.
    
    # Normalized Intensity Sum Ratio Ch2:Ch1
    if ("Normalized Intensity Sum Ratio Ch2:Ch1" %in% varsToInclude & ("Normalized Intensity Sum" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`) %>% 
                        mutate("Normalized Intensity Sum Ratio Ch2:Ch1" = (`Normalized Intensity Sum`[which(`Channel`==2)])/(`Normalized Intensity Sum`[which(`Channel`==1)])) %>% 
                        ungroup()
        }
    
    # Normalized Intensity Mean Ratio Ch2:Ch1
    if ("Normalized Intensity Mean Ratio Ch2:Ch1" %in% varsToInclude & ("Normalized Intensity Mean" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`) %>% 
                        mutate("Normalized Intensity Mean Ratio Ch2:Ch1" = (`Normalized Intensity Mean`[which(`Channel`==2)])/(`Normalized Intensity Mean`[which(`Channel`==1)])) %>% 
                        ungroup()
        }
    
    
    # Add a "Signal Density" variable
    
    # Signal Density
    if ("Signal Density" %in% varsToInclude & ("Normalized Intensity Sum" %in% names(dataToProcess)) & ("Volume" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Channel`) %>% 
                        mutate("Signal Density" = `Normalized Intensity Sum`/`Volume`) %>% 
                        ungroup()
        }
    
    
    # Add "Relative" variables
    
    # Relative Intensity Sum
    if ("Relative Intensity Sum" %in% varsToInclude & ("Normalized Intensity Sum" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,Channel) %>% 
                        mutate("Relative Intensity Sum" = `Normalized Intensity Sum`/sum(`Normalized Intensity Sum`)) %>% 
                        ungroup()
        }
    
    # Relative Intensity Mean
    if ("Relative Intensity Mean" %in% varsToInclude & ("Normalized Intensity Mean" %in% names(dataToProcess))){
        dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,Channel) %>% 
                        mutate("Relative Intensity Mean" = `Normalized Intensity Mean`/sum(`Normalized Intensity Mean`)) %>% 
                        ungroup()
        }
    
    
    # Add counts on groups split by image file, and object type
    
    # Object Count
    if ("Object Count" %in% varsToInclude){
        dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,`Channel`) %>% 
                        mutate("Object Count" = n()) %>% 
                        ungroup()
        }
    
    
    # Relocate the Image Subset and Time variables and make final data type casts
    finalDataToReturn = dataToProcess %>% relocate(c("Image Subset","Time"),.after=last_col())
    finalDataToReturn$Channel = as.character(finalDataToReturn$Channel)
    finalDataToReturn$`Object ID` = as.character(finalDataToReturn$`Object ID`)
    finalDataToReturn$`Time` = as.character(finalDataToReturn$`Time`)
    
    return(finalDataToReturn)
}

