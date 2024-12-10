# This function accepts the cleaned data table/frame as an input, then outputs the processed data.
# The "processed data" is defined as the original cleaned data with added custom variables that are
# made (i.e., processed) from the original variables.

processingFunction = function(importedData,varsToInclude,ratioSumsUnnormToCreate,ratioMeansUnnormToCreate,ratioSumsToCreate,ratioSumsPerGroupToCreate,normVar){
  # Before any processing, drop columns with no names
  if("" %in% colnames(importedData)){
    importedData = importedData %>% dplyr::select(-c(""))
  }
  
  # First: fill the NA's for the Nucleus Center of Mass objects to a number for normalization.
  # 1 was chosen arbitrarily, and can be changed in future iterations of the code as necessary.
  # These values are filled because the data itself should not be missing for subsequent operations.
  # Moreover, the names of variables (e.g., "Nucleus center of mass") are unified.
  condVar = paste0("Distance to ",normVar)
  if("Nucleus Center of Mass" %in% unique(importedData$`Object`) & (condVar %in% names(importedData))){
    importedData = importedData %>% mutate(!!condVar := ifelse(`Object`=="Nucleus Center of Mass" & is.na(!!sym(condVar)), 1, !!sym(condVar)))
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
  
  # Create Intensity Sum Normalised per NORMALIZATION_VARIABLE (former, Normalized Intensity Sum)
  newVar = paste0("Intensity Sum Normalised per ",normVar)
  condVar = "Intensity Sum"
  if (newVar %in% varsToInclude & (condVar %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
      mutate(!!newVar := if (normVar %in% `Object`) {
        .data[[condVar]]/.data[[condVar]][.data$Object == normVar]
        } else {NA_real_}) %>% 
      ungroup()
    
    # handles caese where normVar can be 0
    dataToProcess = dataToProcess %>% mutate(across(matches(newVar), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }
  
  # Create Intensity Mean Normalised per NORMALIZATION_VARIABLE (former, Normalized Intensity Mean)
  newVar = paste0("Intensity Mean Normalised per ",normVar)
  condVar = "Intensity Mean"
  if (newVar %in% varsToInclude & (condVar %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
      mutate(!!newVar := if (normVar %in% `Object`) {
        .data[[condVar]]/.data[[condVar]][.data$Object == normVar]
      } else {NA_real_}) %>% 
      ungroup()
    
    # handles caese where normVar can be 0
    dataToProcess = dataToProcess %>% mutate(across(matches(newVar), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }
  
  # Create Intensity StdDev Normalised per NORMALIZATION_VARIABLE (former, Normalized Intensity StdDev)
  newVar = paste0("Intensity StdDev Normalised per ",normVar)
  condvar = "Intensity StdDev"
  if (newVar %in% varsToInclude & (condVar %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
      mutate(!!newVar := if (normVar %in% `Object`) {
        .data[[condVar]]/.data[[condVar]][.data$Object == normVar]
      } else {NA_real_}) %>% 
      ungroup()
    
    # handles caese where normVar can be 0
    dataToProcess = dataToProcess %>% mutate(across(matches(newVar), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }
  
  # Normalized Distance to NORMALIZATION_VARIABLE
  newVar = paste0("Normalized Distance to ",normVar)
  condVar = paste0("Distance to ",normVar)
  if ((newVar %in% varsToInclude) & ("Nucleus Center of Mass" %in% unique(dataToProcess$`Object`)) & (condVar %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
      mutate(!!newVar := .data[[condVar]]/.data[[condVar]][.data$Object == "Nucleus Center of Mass"]) %>% 
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
      mutate("Group Intensity Mean" = mean(`Intensity Mean`)) %>% 
      ungroup()
  }
  
  # Create Ratio Ch[a]:Ch[b] Unnormalised
  condVar = "Intensity Sum"
  if (length(ratioSumsUnnormToCreate) != 0 & (condVar %in% names(dataToProcess))){
    
    # calculate ratios
    for (c in ratioSumsUnnormToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Sum",sep = "")
      dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Category`) %>%
        mutate(!!stringTitle := .data[[condVar]][.data$Channel == ch_a] / .data[[condVar]][.data$Channel == ch_b]) %>%
        ungroup()
    }
    
    # assign NAs to non-relevant channels
    for (c in ratioSumsUnnormToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Sum",sep = "")
      dataToProcess = dataToProcess %>% 
        mutate(!!sym(stringTitle) := replace(!!sym(stringTitle),!(`Channel` %in% c(ch_a,ch_b)),NA_real_))
    }
    
    matchCol = "Ratio Ch.*Intensity Sum"
    dataToProcess = dataToProcess %>% mutate(across(matches(matchCol), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }

  condVar = "Intensity Mean"
  if (length(ratioMeansUnnormToCreate) != 0 & (condVar %in% names(dataToProcess))){
    
    # calculate ratios
    for (c in ratioMeansUnnormToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Mean",sep = "")
      dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Category`) %>%
        mutate(!!stringTitle := .data[[condVar]][.data$Channel == ch_a] / .data[[condVar]][.data$Channel == ch_b]) %>%
        ungroup()
    }
    
    # assign NAs to non-relevant channels
    for (c in ratioMeansUnnormToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Mean",sep = "")
      dataToProcess = dataToProcess %>% 
        mutate(!!sym(stringTitle) := replace(!!sym(stringTitle),!(`Channel` %in% c(ch_a,ch_b)),NA_real_))
    }
    
    matchCol = "Ratio Ch.*Intensity Mean"
    dataToProcess = dataToProcess %>% mutate(across(matches(matchCol), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }
  
  # Create Ratio Ch[a]:Ch[b] of Intensity Sum Normalised per NORMALIZATION_VARIABLE (former, "Normalized Intensity Sum Ratio Ch2:Ch1")
  # Variable "Normalized Intensity Mean Ratio Ch2:Ch1" is NOT created.
  # !! To make the code work with multiple channels beyond 1 and 2, the case_when function was used to handle
  # !! multiple criteria; moreover, due to some object ID's being both Surfaces and Spots, the `Category`
  # !! variables was added in the group_by function.
  # !! Confirm that this variable can be created when Normalized Sum/Mean lacks a Nucleus object
  
  # Ratio of Intensity Sum Normalized per NORMALIZATION_VARIABLE
  condVar = paste0("Intensity Sum Normalised per ",normVar)
  if (length(ratioSumsToCreate) != 0 & (condVar %in% names(dataToProcess))){

    # calculate ratios
    for (c in ratioSumsToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Sum Normalised per ",normVar,sep = "")
      dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Category`) %>%
        mutate(!!stringTitle := .data[[condVar]][.data$Channel == ch_a] / .data[[condVar]][.data$Channel == ch_b]) %>%
        ungroup()
    }

    # assign NAs to non-relevant channels
    for (c in ratioSumsToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Sum Normalised per ",normVar,sep = "")
      dataToProcess = dataToProcess %>% 
        mutate(!!sym(stringTitle) := replace(!!sym(stringTitle),!(`Channel` %in% c(ch_a,ch_b)),NA_real_))
    }
    
    matchCol = paste0("Ratio Ch.*Intensity Sum Normalised per ",normVar)
    dataToProcess = dataToProcess %>% mutate(across(matches(matchCol), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }

  # Add a "Signal Density" variable
  
  # Signal Density
  newVar = "Signal Density"
  condVar = paste0("Intensity Sum Normalised per ",normVar)
  if (newVar %in% varsToInclude & (condVar %in% names(dataToProcess)) & ("Volume" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Channel`) %>% 
      mutate(!!newVar := !!sym(condVar)/`Volume`) %>% 
      ungroup()
  }
  
  
  # Add "Relative" variables
  
  # Create Intensity Sum Normalised by Group (former, Relative Intensity Sum)
  newVar = "Intensity Sum Normalised by Group"
  condVar = paste0("Intensity Sum Normalised per ",normVar)
  if (newVar %in% varsToInclude & (condVar %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,Channel) %>% 
      mutate(!!newVar := !!sym(condVar)/sum(!!sym(condVar))) %>% 
      ungroup()
  }
  
  # Create Intensity Mean Normalised by Group (former, Relative Intensity Mean)
  newVar = "Intensity Mean Normalised by Group"
  condVar = paste0("Intensity Mean Normalised per ",normVar)
  if (newVar %in% varsToInclude & (condVar %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,Channel) %>% 
      mutate(!!newVar := !!sym(condVar)/sum(!!sym(condVar))) %>% 
      ungroup()
  }

  # Create Intensity StdDev Normalised by Group
  newVar = "Intensity StdDev Normalised by Group"
  condVar = paste0("Intensity StdDev Normalised per ",normVar)
  if (newVar %in% varsToInclude & (condVar %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,Channel) %>% 
      mutate(!!newVar := !!sym(condVar)/sum(!!sym(condVar))) %>% 
      ungroup()
  }
  
  # Add counts on groups split by image file, and object type
  
  # Object Count
  if ("Object Count" %in% varsToInclude){
    dataToProcess = dataToProcess %>% group_by(`Image File`,`Object`,`Channel`) %>% 
      mutate("Object Count" = n()) %>% 
      ungroup()
  }
  
  
  # Add group variables relative to NORMALIZATION_VARIABLE
  
  # Group Intensity Sum Relative to NORMALIZATION_VARIABLE
  newVar = paste0("Group Intensity Sum Relative to ",normVar)
  if (newVar %in% varsToInclude & ("Intensity Sum" %in% names(dataToProcess)) & ("Group Intensity Sum" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% 
      group_by(`Image File`,Channel) %>% 
      mutate(!!newVar := 
               if (normVar %in% Object & "Surface" %in% Category) {`Group Intensity Sum`/`Intensity Sum`[`Object`==normVar & `Category`=="Surface"]} else {NA_real_}) %>%
      ungroup()
    
    # handles caese where normVar can be 0
    dataToProcess = dataToProcess %>% mutate(across(matches(newVar), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }
  
  # Group Intensity Mean Relative to NORMALIZATION_VARIABLE
  newVar = paste0("Group Intensity Mean Relative to ",normVar)
  if (newVar %in% varsToInclude & ("Intensity Mean" %in% names(dataToProcess)) & ("Group Intensity Mean" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% 
      group_by(`Image File`,Channel) %>% 
      mutate(!!newVar := 
               if (normVar %in% Object & "Surface" %in% Category) {`Group Intensity Mean`/`Intensity Mean`[`Object`==normVar & `Category`=="Surface"]} else {NA_real_}) %>%
      ungroup()
    
    # handles caese where normVar can be 0
    dataToProcess = dataToProcess %>% mutate(across(matches(newVar), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }
  
  # Create Ratio Ch[a]:Ch[b] of Intensity Sum Normalized per Group
  if (length(ratioSumsPerGroupToCreate) != 0 & ("Intensity Sum Normalised by Group" %in% names(dataToProcess))){

    # calculate ratios
    for (c in ratioSumsPerGroupToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Sum Normalised per Group",sep = "")
      dataToProcess = dataToProcess %>% group_by(`Image File`,`Object ID`,`Object`,`Category`) %>%
        mutate(!!stringTitle := `Intensity Sum Normalised by Group`[`Channel`== ch_a]/ `Intensity Sum Normalised by Group`[`Channel`== ch_b]) %>%
        ungroup()
    }

    # for every Ratio column, get channel and set other channels to NA
    for (c in ratioSumsPerGroupToCreate){
      splitStrings = lapply(strsplit(c,", "),as.numeric)
      ch_a = splitStrings[[1]][1]
      ch_b = splitStrings[[1]][2]
      stringTitle = paste("Ratio Ch",ch_a,":Ch",ch_b," of Intensity Sum Normalised per Group",sep = "")
      dataToProcess = dataToProcess %>% mutate(!!sym(stringTitle) := replace(!!sym(stringTitle),!(`Channel` %in% c(ch_a,ch_b)),NA_real_))
    }
    dataToProcess = dataToProcess %>% mutate(across(
      matches("Ratio Ch.*Intensity Sum Normalised per Group"),
      ~ replace(., . %in% c(Inf, -Inf), NA_real_)))

  }
  
  # Add variable Volume Relative to NORMALIZATION_VARIABLE
  newVar = paste0("Volume Relative to ",normVar)
  if (newVar %in% varsToInclude & ("Volume" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`, Channel) %>% 
      mutate(!!newVar := 
               if (normVar %in% Object & "Surface" %in% Category) {`Volume`/`Volume`[`Object`==normVar & `Category`=="Surface"]} else {NA_real_}) %>%
      ungroup()
    
    # handles caese where normVar can be 0
    dataToProcess = dataToProcess %>% mutate(across(matches(newVar), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }
  
  # Add variable Group Volume
  if ("Group Volume" %in% varsToInclude & ("Volume" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Object,Channel,Category) %>% 
      mutate("Group Volume" = sum(`Volume`)) %>%
      ungroup()
  }
  
  # Add Volume Relative to Group
  if ("Volume Relative to Group" %in% varsToInclude & ("Volume" %in% names(dataToProcess)) & ("Group Volume" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% mutate("Volume Relative to Group" = `Volume`/`Group Volume`) 
  }
  
  # Add Group Volume Relative to NORMALIZATION_VARIABLE
  newVar = paste0("Group Volume Relative to ",normVar)
  if (newVar %in% varsToInclude & ("Volume" %in% names(dataToProcess)) & ("Group Volume" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Channel) %>% 
      mutate(!!newVar := 
               if (normVar %in% Object & "Surface" %in% Category) {`Group Volume`/`Volume`[`Object`==normVar & `Category`=="Surface"]} else {NA_real_}) %>%
      ungroup()
    
    # handles caese where normVar can be 0
    dataToProcess = dataToProcess %>% mutate(across(matches(newVar), ~ replace(., . %in% c(Inf, -Inf, NaN), NA_real_)))
  }
  
  # Add Surface Area-to-Volume Ratio
  if ("Surface Area-to-Volume Ratio" %in% varsToInclude & ("Volume" %in% names(dataToProcess)) & ("Area" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% mutate("Surface Area-to-Volume Ratio" = `Area` / `Volume`)
  }
  
  # Add Group Surface Area
  if ("Group Surface Area" %in% varsToInclude & ("Area" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% group_by(`Image File`,Object,Channel,Category) %>% 
      mutate("Group Surface Area" = sum(`Area`)) %>%
      ungroup()
  }
  
  # Add Group Surface Area-to-Volume Ratio
  if ("Group Surface Area-to-Volume Ratio" %in% varsToInclude & ("Group Volume" %in% names(dataToProcess)) & ("Group Surface Area" %in% names(dataToProcess))){
    dataToProcess = dataToProcess %>% mutate("Group Surface Area-to-Volume Ratio" = `Group Surface Area` / `Group Volume`)
  }
  
  # Relocate the Image Subset and Time variables and make final data type casts
  finalDataToReturn = dataToProcess
  if ("Image Subset" %in% colnames(dataToProcess)) {
    finalDataToReturn = dataToProcess %>% relocate(c("Image Subset"),.after=last_col())  
  }
  if ("Time" %in% colnames(finalDataToReturn)) {
    finalDataToReturn = finalDataToReturn %>% relocate(c("Time"),.after=last_col())  
    finalDataToReturn$`Time` = as.character(finalDataToReturn$`Time`)
  }
  finalDataToReturn$Channel = as.character(finalDataToReturn$Channel)
  finalDataToReturn$`Object ID` = as.character(finalDataToReturn$`Object ID`)
  
  return(finalDataToReturn)
}

