# This function accepts the cleaned data table/frame as an input, then outputs the processed data.
# The "processed data" is defined as the original cleaned data with added custom variables that are
# made (i.e., processed) from the original variables.

processingFunction = function(importedData,varsToInclude,ratioSumsToCreate,ratioSumsPerGroupToCreate){
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
  
  # Create Intensity Sum Normalised per Nucleus (former, Normalized Intensity Sum)
  if ("Intensity Sum Normalised per Nucleus" %in% varsToInclude & ("Intensity Sum" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
      mutate("Intensity Sum Normalised per Nucleus" = if ("Nucleus" %in% Object) {`Intensity Sum`/`Intensity Sum`[Object=="Nucleus"]} else {NA_real_}) %>% 
      ungroup()
  }
  
  # Create Intensity Mean Normalised per Nucleus (former, Normalized Intensity Mean)
  if ("Intensity Mean Normalised per Nucleus" %in% varsToInclude & ("Intensity Mean" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
      mutate("Intensity Mean Normalised per Nucleus" = if ("Nucleus" %in% Object) {`Intensity Mean`/`Intensity Mean`[`Object`=="Nucleus"]} else {NA_real_}) %>% 
      ungroup()
  }
  
  # Normalized Intensity StdDev
  if ("Normalized Intensity StdDev" %in% varsToInclude & ("Intensity StdDev" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
      mutate("Normalized Intensity StdDev" = if ("Nucleus" %in% Object) {`Intensity StdDev`/`Intensity StdDev`[`Object`=="Nucleus"]} else {NA_real_}) %>% 
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
  
  
  # Create Ratio Ch[a]:Ch[b] of Intensity Sum Normalised per Nucleus (former, "Normalized Intensity Sum Ratio Ch2:Ch1")
  # Variable "Normalized Intensity Mean Ratio Ch2:Ch1" is NOT created.
  # !! To make the code work with multiple channels beyond 1 and 2, the case_when function was used to handle
  # !! multiple criteria; moreover, due to some object ID's being both Surfaces and Spots, the `Category`
  # !! variables was added in the group_by function.
  # !! Confirm that this variable can be created when Normalized Sum/Mean lacks a Nucleus object
  
  # Ratio of Intensity Sum Normalized per Nucleus
  if (length(ratioSumsToCreate) != 0 & ("Intensity Sum Normalised per Nucleus" %in% names(dataToProcess))){
    for (c in ratioSumsToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Sum Normalised per Nucleus",sep = "")
      dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Category`) %>% 
        mutate(!!stringTitle := case_when(
          `Intensity Sum Normalised per Nucleus`[which(`Channel`==ch_a)] == NA | `Intensity Sum Normalised per Nucleus`[which(`Channel`==ch_b)] == NA ~ NA_real_,
          Channel == ch_a | Channel == ch_b ~ ((`Intensity Sum Normalised per Nucleus`[which(`Channel`==ch_a)])/(`Intensity Sum Normalised per Nucleus`[which(`Channel`==ch_b)])),
          Channel != ch_a & Channel != ch_b ~ NA_real_
        )
        ) %>% 
        ungroup()
    }
  }
  
  # Remove
  # Normalized Intensity Mean Ratio
  # if (length(ratioMeansToCreate) != 0 & ("Normalized Intensity Mean" %in% names(dataToProcess))){
  #   for (c in ratioMeansToCreate){
  #     splitStrings = lapply(strsplit(c,", "),as.numeric)
  #     ch_a = splitStrings[[1]][1]
  #     ch_b = splitStrings[[1]][2]
  #     stringTitle = paste("Normalized Intensity Mean Ratio Ch",ch_a,":Ch",ch_b,sep = "")
  #     dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Category`) %>% 
  #       mutate(!!stringTitle := case_when(
  #         `Normalized Intensity Mean`[which(`Channel`==ch_a)] == NA | `Normalized Intensity Mean`[which(`Channel`==ch_b)] == NA ~ NA_real_,
  #         Channel == ch_a | Channel == ch_b ~ ((`Normalized Intensity Mean`[which(`Channel`==ch_a)])/(`Normalized Intensity Mean`[which(`Channel`==ch_b)])),
  #         Channel != ch_a & Channel != ch_b ~ NA_real_
  #       )
  #       ) %>% 
  #       ungroup()
  #   }
  # }
  
  
  # Add a "Signal Density" variable
  
  # Signal Density
  if ("Signal Density" %in% varsToInclude & ("Intensity Sum Normalised per Nucleus" %in% names(dataToProcess)) & ("Volume" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Channel`) %>% 
      mutate("Signal Density" = `Intensity Sum Normalised per Nucleus`/`Volume`) %>% 
      ungroup()
  }
  
  
  # Add "Relative" variables
  
  # Create Intensity Sum Normalised by Group (former, Relative Intensity Sum)
  if ("Intensity Sum Normalised by Group" %in% varsToInclude & ("Intensity Sum Normalised per Nucleus" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,Channel) %>% 
      mutate("Intensity Sum Normalised by Group" = `Intensity Sum Normalised per Nucleus`/sum(`Intensity Sum Normalised per Nucleus`)) %>% 
      ungroup()
  }
  
  # Create Intensity Mean Normalised by Group (former, Relative Intensity Mean)
  if ("Intensity Mean Normalised by Group" %in% varsToInclude & ("Intensity Mean Normalised per Nucleus" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,Channel) %>% 
      mutate("Intensity Mean Normalised by Group" = `Intensity Mean Normalised per Nucleus`/sum(`Intensity Mean Normalised per Nucleus`)) %>% 
      ungroup()
  }
  
  
  # Add counts on groups split by image file, and object type
  
  # Object Count
  if ("Object Count" %in% varsToInclude){
    dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,`Channel`) %>% 
      mutate("Object Count" = n()) %>% 
      ungroup()
  }
  
  
  # Add group variables relative to Nucleus
  
  # Group Intensity Sum Relative to Nucleus
  if ("Group Intensity Sum Relative to Nucleus" %in% varsToInclude & ("Intensity Sum" %in% names(dataToProcess)) & ("Group Intensity Sum" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% 
      group_by(`Image File`,Channel) %>% 
      mutate("Group Intensity Sum Relative to Nucleus" = `Group Intensity Sum`/`Intensity Sum`[`Object`=="Nucleus" & `Category`=="Surface"]) %>%
      ungroup()
  }
  
  # Group Intensity Mean Relative to Nucleus
  if ("Group Intensity Mean Relative to Nucleus" %in% varsToInclude & ("Intensity Mean" %in% names(dataToProcess)) & ("Group Intensity Mean" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% 
      group_by(`Image File`,Channel) %>% 
      mutate("Group Intensity Mean Relative to Nucleus" = `Group Intensity Mean`/`Intensity Mean`[`Object`=="Nucleus" & `Category`=="Surface"]) %>%
      ungroup()
  }
  
  # Create Ratio Ch[a]:Ch[b] of Intensity Sum Normalized per Group
  if (length(ratioSumsPerGroupToCreate) != 0 & ("Intensity Sum Normalised by Group" %in% names(dataToProcess))){
    for (c in ratioSumsPerGroupToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Sum Normalised per Group",sep = "")
      dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Category`) %>% 
        mutate(!!stringTitle := case_when(
          `Intensity Sum Normalised by Group`[which(`Channel`==ch_a)] == NA | `Intensity Sum Normalised by Group`[which(`Channel`==ch_b)] == NA ~ NA_real_,
          Channel == ch_a | Channel == ch_b ~ ((`Intensity Sum Normalised by Group`[which(`Channel`==ch_a)])/(`Intensity Sum Normalised by Group`[which(`Channel`==ch_b)])),
          Channel != ch_a & Channel != ch_b ~ NA_real_
        )
        ) %>% 
        ungroup()
    }
  }
  
  # Relocate the Image Subset and Time variables and make final data type casts
  finalDataToReturn = dataToProcess %>% relocate(c("Image Subset","Time"),.after=last_col())
  finalDataToReturn$Channel = as.character(finalDataToReturn$Channel)
  finalDataToReturn$`Object ID` = as.character(finalDataToReturn$`Object ID`)
  finalDataToReturn$`Time` = as.character(finalDataToReturn$`Time`)
  
  return(finalDataToReturn)
}

