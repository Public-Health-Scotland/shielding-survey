# Shielding functions (weighted)


# Function to get counts from data
gen.data <- function(x, wts = NA){
  
  if(!is.na(wts)){
    out <- questionr::wtd.table(x, weights = wts, useNA = "always")
  } else {
    out <- table(x, useNA = "always")
  }
  
  return(out)
}

# Function to pull necessary data
pull_question_data <- function(keyword, qstoignore = c()) {
 
  subq <- cs %>% select(contains(keyword), RescaledWeight) %>%  
    select(-qstoignore)

  wts <- subq$RescaledWeight
  subq %<>% select(-RescaledWeight)
  
  ## Do both weighted and non-weighted
  subq_count_w <- data.frame(apply(subq, 2, gen.data, wts=wts))
  subq_count <- data.frame(apply(subq, 2, gen.data))
  
  answers <- row.names(subq_count)
  subq_count_w %<>% dplyr::mutate(Answer = answers) %>% 
    reshape2::melt() %>% 
    dplyr::rename(Question = variable,
                  `Weighted` = value)
  subq_count %<>% dplyr::mutate(Answer = answers) %>% 
    reshape2::melt() %>% 
    dplyr::rename(Question = variable,
                  `Non-weighted` = value)
  
  output <- left_join(subq_count, subq_count_w)
  
  ## Before returning output, check the total participants
  check_total_participants(output, multivar = TRUE)
  
  
  return(output)
  
}


# Function to add percentages
add_percentages <- function(df, multivar=TRUE){
  
  naans = c("Not applicable", "NA", "N/A", 
            "I am not sure / Not applicable",
            "I am not sure", NA)   ## Possible not applicable answers
  
  # Define not in operator
  `%!in%` <- Negate(`%in%`)
  
  # Convert all dots in dataframe to space and trim spaces
  df$Answer <- str_replace_all(df$Answer, "\\.", " ")
  df$Answer <- str_trim(df$Answer)

  newcol <- vector() # empty vector to fill
  # Reorder not applicable answers to end
  num_naan <- 0
  for(naan in naans){
    if(naan %in% unique(df$Answer)){
      num_naan <- num_naan + 1
    }
  }
  
  df$Answer <- factor(df$Answer, levels=c(unique(df$Answer)[unique(df$Answer) %!in% naans], naans))
  
  
  if(multivar==TRUE){
    
    df <- df[order(df$Question, df$Answer),]
    
    for(q in unique(df$Question)){
      num_naan_q <- num_naan
      
      nums <- df$`Weighted`[df$Question == q & df$Answer %!in% naans]
      denom <- sum(nums)
      percs <- 100*nums/denom
      
      # Add NA in % column for as many nan answers as there are
      while(num_naan_q > 0){
        num_naan_q <- num_naan_q -1
        percs <- c(percs, NA)
      }
      
      newcol <- c(newcol, percs) # Last NA for the non applicable column
    }
  }else{

    df <- df[order(df$Answer),]
    nums <- df$`Weighted`[df$Answer %!in% naans]
    denom <- sum(nums)
    percs <- 100*nums/denom
    
    while(num_naan >0){
      num_naan <- num_naan -1
      percs <- c(percs, NA)
    }
    newcol <- c(newcol, percs)
  }
  
  df$`Weighted %` <- newcol
  
  return(df)
}


# Function to do basic 1 variable table

one_var_table <- function(varname){
  
  tab <- questionr::wtd.table(cs[,varname], 
                              weights = cs$RescaledWeight, 
                              useNA = "ifany") %>% 
    as.data.frame() %>% 
    dplyr::rename(`Weighted` = Freq)
  
  ## Add weighted percentage
  #tab %<>% mutate(`Weighted %` = 100*(Freq/sum(tab$Freq))) %>% 
 # tab %<>%   dplyr::rename(`Weighted` = Freq) 

  tab_notw <- table(cs[,varname], useNA = "ifany") %>% 
    as.data.frame() %>% 
    dplyr::rename(`Non-weighted` = Freq)

  output <- left_join(tab_notw, tab) 

  output %<>% dplyr::rename(Answer = Var1) 
  
  output %<>% add_percentages(multivar=FALSE) 

  ## Before returning output, check the total participants
  check_total_participants(output)
  

  write.csv(output, glue("Frequency tables/{varname}.csv"), row.names=FALSE)
  
}


# Function to check total participants is expected

check_total_participants <- function(check_table, multivar=FALSE){
  
  expected_total <- nrow(cs)
  
  if(multivar == FALSE){
    
    sum_nw <- sum(check_table$`Non-weighted`)
    sum_w <- sum(check_table$Weighted)
    if(sum_nw != expected_total){
      
      warning(glue("Expected total {expected_total} but got non-weighted total: {sum_nw}"))
    } 
    if(sum_w != expected_total){
      warning(glue("Expected total {expected_total} but got weighted total: {sum_w}"))
    }
    
    
  }else{
    # Multi variable table needs treated column by column
    questions <- unique(check_table$Question)
    
    for(question in questions){
      sum_nw <- sum(is_count[is_count$Question == question,]$`Non-weighted`)
      sum_w <- sum(is_count[is_count$Question == question,]$`Weighted`)
      
      if(sum_nw != expected_total){
        warning(glue("Expected total {expected_total} but for Question: {question} got non-weighted total: {sum_nw}"))
      } 
      if(sum_w != expected_total){
        warning(glue("Expected total {expected_total} but for Question: {question} got weighted total: {sum_w}"))
      }
      
    }
    
    
  }
  
  
}








