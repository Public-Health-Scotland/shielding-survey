# Shielding functions (weighted)


# Function to get counts from data
gen.data <- function(x, wts = NA){
  
  if(!is.na(wts)){
    out <- questionr::wtd.table(x, weights = wts)
  } else {
    out <- table(x)
  }
  
  return(out)
}

# Function to pull necessary data
pull_question_data <- function(keyword, qstoignore = c(), omitnans = FALSE, weighted = TRUE) {
  subq <- cs %>% select(contains(keyword), RescaledWeight) %>%  
    select(-qstoignore)
  if(omitnans){
    subq %<>% na.omit() 
  }
  wts <- subq$RescaledWeight
  subq %<>% select(-RescaledWeight)
  if(weighted == TRUE){
    subq_count <- data.frame(apply(subq, 2, gen.data, wts=wts))
  } else {
    subq_count <- data.frame(apply(subq, 2, gen.data))
  }
  
  answers <- row.names(subq_count)
  subq_count %<>% dplyr::mutate(Answer = answers) %>% 
    reshape2::melt() %>% 
    dplyr::rename(Question = variable,
                  `Number of Individuals` = value)
  
  return(subq_count)
  
}


# Function to add percentages
add_percentages <- function(df, addna = TRUE, naans = "Not applicable"){
  newcol <- vector() # empty vector to fill
  if(addna){
    # Reorder not applicable to end
    df$Answer <- factor(df$Answer, levels=c(unique(df$Answer)[unique(df$Answer) != naans], naans))
    df <- df[order(df$Question, df$Answer),]
  }
  for(q in unique(df$Question)){
    nums <- df$`Number of Individuals`[df$Question == q & df$Answer != naans]
    denom <- sum(nums)
    percs = 100*nums/denom
    
    if(addna){
      percs <- c(percs, NA)
    }
    newcol <- c(newcol, percs) # Last NA for the non applicable column
  }
  
  df$Percentages <- newcol
  
  return(df)
}
