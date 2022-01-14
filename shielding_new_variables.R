# Making new variables for survey


library(openxlsx)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(janitor)

setwd("~/weighting")

# Confidential answers in separate file
# Contains the vectors of answers
# - no_children_answers
# - children_answers
# - household_size_one
# - household_size_two
# - household_size_threeplus

source("new_var_answers.R")

# Getting cleaned data
cs <- read.xlsx("~/weighting/cleaned_survey.xlsx")

          

cs %<>% 
  ### Age groups
  mutate(AgeGroup2 = case_when(is.na(AgeGroup) ~ "unknown",
                                     AgeGroup == "Under 16" ~ "0-15",
                                     AgeGroup %in% c("16-24", "25-44", "45-64") ~ "16-64",
                                     AgeGroup %in% c("65-69", "70-74", "75-79", "80+") ~ "65+"
                               )
         ) %>% 
  ### Impairment
  mutate(Impairment = case_when(VisualImpairment == "Yes" ~ "any impairment",
                                HearingImpairment == "Yes" ~ "any impairment",
                                OtherSensoryImpairment == "Yes" ~ "any impairment",
                                MobilityImpairment == "Yes" ~ "any impairment",
                                OtherPhysicalImpairment == "Yes" ~ "any impairment",
                                LearningDifficulty == "Yes" ~ "any impairment",
                                NoListedImpairment == "Yes" ~ "no impairment",
                                TRUE ~ "unknown"
                                )
         
         ) %>% 
  ### Children in household
  mutate(ChildrenInHousehold = case_when(HouseholdChildren %in% no_children_answers ~ "no children",
                                         HouseholdChildren %in% children_answers ~ "children",
                                         TRUE ~ "unknown"
    
    
                                        )
         
         
         ) %>% 
  ### Number in household
  mutate(NumberInHousehold = case_when(HouseholdSize %in% household_size_one ~ "1", 
                                       
                                       HouseholdSize %in% household_size_two ~ "2",
                                       
                                       HouseholdSize %in% household_size_threeplus ~ "3+",
                                       TRUE ~ "unknown"
    
    
    
  )) %>% 
  ### Severely immunosuppressed
  mutate(SeverelyImmunosuppressed = case_when(
    AdvisedGPImmunosuppressed == "Yes - I am severely immunosuppressed or severely immunocompromised" ~ "Yes",
    grepl("No", AdvisedGPImmunosuppressed) ~ "No",
    TRUE ~ "unknown"
  )) %>% 
  ### Worried by no longer at highest risk
  
  mutate(WorriedButNoLongerHighestRisk = case_when(
    (grepl("I still", CurrentApproachToManagingRisk) & (SeverelyImmunosuppressed != "Yes")) ~ "Yes",
    TRUE ~ "No"
  ))



## Write new survey
openxlsx::write.xlsx(cs, "survey_with_new_vars.xlsx")


