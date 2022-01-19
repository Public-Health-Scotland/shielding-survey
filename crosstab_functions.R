# -----------------------------
# Functions to make crosstabs
# -----------------------------

make_crosstab <- function(primary_variable, secondary_variables, selections=NA){
  
  ## Select the relevant variables
  reduced_survey <- cs %>% select(c(all_of(primary_variable), all_of(secondary_variables), "RescaledWeight"))
  
  
  ## Filter for specific answers to some questions
  
  if(!is.na(selections)){
    for(var_to_reduce in names(selections)){
      options_to_keep <- unlist(selections[[var_to_reduce]])
      reduced_survey <- reduced_survey[reduced_survey[[var_to_reduce]] %in% options_to_keep, ]
    }
  }
  
  
  for (secondary_variable in secondary_variables){
    
    # Make separate tables for unweighted, weighted and weighted percentage for each 2ndary var
    unwtd <- as.data.frame.matrix(table_by(reduced_survey, 
                                           secondary_variable, 
                                           primary_variable, 
                                           useNA="no")) %>% 
      remove_dots()
    
    wtd <- as.data.frame.matrix(table_by(reduced_survey, 
                                         secondary_variable, 
                                         primary_variable, 
                                         weighted=TRUE,
                                         useNA="no")) %>% 
      remove_dots()
    
    wtdpct <- table_by(reduced_survey, secondary_variable, primary_variable, 
                       weighted=TRUE, useNA="no")
    
    # Remove any NA answers for weighted %
    naans = c("Not applicable", "NA", "N/A", 
              "I am not sure / Not applicable",
              "I am not sure", NA)   ## Possible not applicable answers
    wtdpct <- wtdpct[!(row.names(wtdpct) %in% naans),]
    wtdpct <- prop.table(wtdpct, 2) %>% as.data.frame.matrix() %>% remove_dots()
    
    
    for(i in seq(length(row.names(wtd)))){
      if(row.names(wtd)[i] %in% naans){
        # Add new NA row
        wtdpct[nrow(wtdpct)+1,] <- NA
        row.names(wtdpct)[[i]] <- row.names(wtd)[i] 
      }
    }
    
    
    colnames(unwtd) <- paste(colnames(unwtd), " (unweighted)")
    colnames(wtd) <- paste(colnames(wtd), " (weighted)")
    colnames(wtdpct) <- paste(colnames(wtdpct), " (weighted %)")
    
    combined <- cbind(unwtd, wtd, wtdpct) %>% remove_dots()
    rownames(combined) <- rownames(unwtd)
    combined$SecondaryAnswer <- rownames(combined)
    combined %<>% relocate(SecondaryAnswer)
    combined$SecondaryVariable <- secondary_variable
    combined %<>% relocate(SecondaryVariable)
    
    
    write.csv(combined, glue("Crosstab contributions/{primary_variable}-{secondary_variable}.csv"), row.names=FALSE)
    
    if (secondary_variable == secondary_variables[[1]]){
      stacked <- combined
    } else {
      stacked <- rbind(stacked, combined)
    }
    
    
  }

  write.csv(stacked, glue("Crosstabs/{primary_variable}.csv"), row.names=FALSE)
  
}

# Makes individual crosstabs
table_by <- function(DF, row_var, col_var = NULL, weighted=FALSE, useNA="ifany") {
  # the repeated t() below ensures you have a 4 x 1 matrix
  if (weighted){
    tbl <- if (is.null(col_var)) t(t(questionr::wtd.table(DF[[row_var]], 
                                                          weights=DF[["RescaledWeight"]],
                                                          useNA=useNA))
    ) else questionr::wtd.table(DF[[row_var]], 
                                DF[[col_var]], 
                                weights=DF[["RescaledWeight"]],
                                useNA=useNA)
  } else {
    tbl <- if (is.null(col_var)) t(t(table(DF[[row_var]], useNA=useNA))) else table(DF[[row_var]], 
                                                                                    DF[[col_var]], useNA=useNA)
  }
  
  return(tbl)
}

# Removes dots from row names and column names
remove_dots <- function(tab){
  rownames(tab) <- str_replace_all(rownames(tab), "\\.", " ")
  rownames(tab) <- str_trim(rownames(tab))
  colnames(tab) <- str_replace_all(colnames(tab), "\\.", " ")
  colnames(tab) <- str_trim(colnames(tab))
  return(tab)
}





