# This function accepts the cleaned data table/frame as an input, then outputs the processed data.
# The "processed data" is defined as the original cleaned data with added custom variables that are
# made (i.e., processed) from the original variables.

processingFunction = function(importedData){
    # Before any processing, drop columns with no names
    if("" %in% colnames(importedData)){
        importedData = importedData %>% dplyr::select(-c(""))
    }
    
    # First: fill the NA's for the Nucleus Center of Mass objects to a number for normalization.
    # 1 was chosen arbitrarily, and can be changed in future iterations of the code as necessary.
    # These values are filled because the data itself should not be missing for subsequent operations.
    # Moreover, the names of variables (e.g., "Nucleus center of mass") are unified.
    importedData$`Object`[importedData$`Object`=="Nucleus_centre_of_mass"] = "Nucleus Center of Mass"
    importedData$`Object`[importedData$`Object`=="Nucleus center of mass"] = "Nucleus Center of Mass"
    importedData$`Distance to Nucleus`[is.na(importedData$`Object`=="Nucleus Center of Mass" & importedData$`Distance to Nucleus`=="NA")] = 1
    
    # Filter out any group that doesn't have a nucleus center of mass object or where Channel is NA
    dataToProcess = importedData %>% group_by(`Image File`) %>%
                    filter(any(`Object`=="Nucleus Center of Mass")) %>%
                    filter(`Channel`!="NA") %>% ungroup()
    
    # Perform the variable normalizations in a "cascading fashion".
    # I.e., perform one normalization, add the column to the data frame, then perform the next, add it to
    # the data frame as well, then so on and so forth.
    # "Normalized Intensity Sum", "Normalized Intensity Mean", and "Normalized Intensity StdDev" are added as variables.
    dataWithSumAndMeanNormed = dataToProcess %>% group_by(`Image File`,Channel) %>% 
        mutate("Normalized Intensity Sum" = `Intensity Sum`/`Intensity Sum`[which(`Object`=="Nucleus")]) %>% 
        mutate("Normalized Intensity Mean" = `Intensity Mean`/`Intensity Mean`[which(`Object`=="Nucleus")]) %>%
        mutate("Normalized Intensity StdDev" = `Intensity StdDev`/`Intensity StdDev`[which(`Object`=="Nucleus")]) %>%
        ungroup()
    
    # Add the group intensity variables
    dataWithNormedAndGroup = dataWithSumAndMeanNormed %>% group_by(`Image File`,Channel,Category,`Object`) %>%
                                mutate("Group Intensity Sum" = sum(`Intensity Sum`)) %>%
                                mutate("Group Intensity Mean" = sum(`Intensity Mean`)) %>%
                                ungroup()

    # The "Normalized Distance to Surfaces" variable is added here, using the data from each "Nucleus Center of Mass observation"
    # !! This is an area of development as there are, potentially, multiple types of surfaces that 
    # !! can be handled by this computation
    dataWithNormedSumMeanStdDevSDtS = dataWithNormedSumMeanStdDev %>% group_by(`Image File`,Channel) %>%
        mutate("Normalized Distance to Nucleus" = `Distance to Nucleus`/`Distance to Nucleus`[which(`Object`=="Nucleus Center of Mass")])  %>% ungroup()
    
    # The "Normalized Intensity Sum Ratio Ch2:Ch1" and "Normalized Intensity Mean Ratio Ch2:Ch1" variables are
    # added here.
    # !! For now, the channel ratios are set to be 2:1; future iterations of the code could automate these variables
    # to use any number of channels.
    dataWithRatios = dataWithNormedSumMeanStdDevSDtS  %>% group_by(`Image File`,
                                                                      `Object ID`,
                                                                      `Object`) %>%
                        mutate("Normalized Intensity Sum Ratio Ch2:Ch1" = (`Normalized Intensity Sum`[which(`Channel`==2)])/(`Normalized Intensity Sum`[which(`Channel`==1)])) %>%
                        mutate("Normalized Intensity Mean Ratio Ch2:Ch1" = (`Normalized Intensity Mean`[which(`Channel`==2)])/(`Normalized Intensity Mean`[which(`Channel`==1)])) %>% ungroup()
    
    # Add a "Signal Density" variable
    postProcessedData = dataWithRatios %>% group_by(`Image File`,
                                                    `Object ID`,
                                                    `Object`,
                                                    `Channel`) %>%
                                            mutate("Signal Density" = `Normalized Intensity Sum`/`Volume`)
    
    # Relocate the Image Subset and Time variables and make final data type casts
    finalDataToReturn = postProcessedData %>% relocate(c("Image Subset","Time"),.after=last_col())
    finalDataToReturn$Channel = as.character(finalDataToReturn$Channel)
    finalDataToReturn$`Object ID` = as.character(finalDataToReturn$`Object ID`)
    finalDataToReturn$`Time` = as.character(finalDataToReturn$`Time`)
    
    return(finalDataToReturn)
}

