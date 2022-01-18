# Weighted shielding analysis


library(openxlsx)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(janitor)
library(glue)
library(tidyverse)

# Getting functions for making summary tables
setwd("~/shielding-survey")
source("shielding_functions.R")

# Getting cleaned data
cs <- openxlsx::read.xlsx("~/weighting/survey_with_new_vars.xlsx")


##### Initial shielding

is_count <- pull_question_data("InitialShielding")

is_count$Question %<>% plyr::revalue(c("InitialShieldingQualityOfLife" = "Quality of Life",
                                       "InitialShieldingMentalHealth" = "Mental Health",
                                       "InitialShieldingPhysicalHealth" = "Physical Health",
                                       "InitialShieldingPhysicalActivity" = "Physical Activity",
                                       "InitialShieldingConfidenceLeavingHome" = "Confidence Leaving Home",
                                       "InitialShieldingLonely" = "Loneliness",
                                       "InitialShieldingRelationshipPartner" = "Relationship with Partner",
                                       "InitialShieldingRelationshipChildren" = "Relationship with Child(ren)",
                                       "InitialShieldingRelationshipFamilyFriends" = "Relationship with Family/Friends",
                                       "InitialShieldingQualityOfCare" = "Quality of Care",
                                       "InitialShieldingEmployment" = "Employment",
                                       "InitialShieldingEducation" = "Education",
                                       "InitialShieldingFinance" = "Financial Situation")) 

is_count %<>% add_percentages()


write.csv(is_count, "Frequency tables/InitialShielding.csv", row.names=FALSE)

##### High risk negatives

hr_count <- pull_question_data("HRN") %>% add_percentages()

write.csv(hr_count, "Frequency tables/HighRiskNegatives.csv", row.names=FALSE)


##### High risk positives

hrp_count <- pull_question_data("HRP", qstoignore = c("HRPOther")) %>%  add_percentages()

write.csv(hrp_count, "Frequency tables/HighRiskPositives.csv", row.names=FALSE)

##### Useful

useful <- pull_question_data("Useful", qstoignore = c("UsefulOther")) %>% add_percentages()

write.csv(useful, "Frequency tables/Useful.csv", row.names=FALSE)

##### Approach

approach <- pull_question_data("Approach", qstoignore = c("CurrentApproachToManagingRisk")) %>% add_percentages()

write.csv(approach, "Frequency tables/Approach.csv", row.names=FALSE)

##### SG

sg <- pull_question_data("SG", qstoignore = c("ApproachNoDependenceSGovAdvice")) %>% add_percentages()

write.csv(sg, "Frequency tables/ScottishGovernment.csv", row.names=FALSE)


#### Diff
revals <- c("I have looked at this and it has influenced some of my actions" = "Influenced",
            "I have looked at this, but I have not done anything differently as a result" = "LookedOnly",
            "I was aware that this existed, but I have not really looked at it" = "AwareNotLooked",
            "I was not aware that this existed" = "NotAware")

revals2 <- c("I used this option and it changed how I felt or behaved" = "UsedandChanged",
             "I was not aware of this option" = "NotAware",
             "I used this option, but it didn't really change how I felt or behaved" = "UsedDidNotChange",
             "I was aware of this option, but did not use it" = "AwareDidNotUse")

cs$DiffChiefMedicalOfficerLetter %<>% plyr::revalue(revals)
cs$DiffBookletBalancingRiskDailyActivities %<>% plyr::revalue(revals)
cs$DiffClearYourHead %<>% plyr::revalue(revals)
cs$DiffGoingShopping %<>% plyr::revalue(revals)
cs$DiffWorkplaceSafety %<>% plyr::revalue(revals)
cs$DiffInfoLocalCases %<>% plyr::revalue(revals)
cs$DiffHRVaccineEffectiveness %<>% plyr::revalue(revals)

cs$DiffPriorityVaccine %<>% plyr::revalue(revals2)
cs$DiffPriorityVaccineHousehold %<>% plyr::revalue(revals2)
cs$DiffLFTAccess %<>% plyr::revalue(revals2)
cs$DiffVitDAccess %<>% plyr::revalue(revals2)

diff1 <- pull_question_data("Diff", qstoignore = c("DifficultyGettingSocialCareSupport",
                                                   "LearningDifficulty",
                                                   "UnexpectedExpenseDifficulty",
                                                   "DiffPriorityVaccine",
                                                   "DiffPriorityVaccineHousehold",
                                                   "DiffLFTAccess",
                                                   "DiffVitDAccess")) %>% 
  add_percentages()


diff2 <- pull_question_data("Diff", qstoignore = c("DifficultyGettingSocialCareSupport",
                                                   "LearningDifficulty",
                                                   "UnexpectedExpenseDifficulty",
                                                   "DiffChiefMedicalOfficerLetter",
                                                   "DiffBookletBalancingRiskDailyActivities",
                                                   "DiffClearYourHead",
                                                   "DiffGoingShopping",
                                                   "DiffWorkplaceSafety",
                                                   "DiffInfoLocalCases",
                                                   "DiffHRVaccineEffectiveness")) %>% 
  add_percentages()


diff <- rbind(diff1, diff2)

write.csv(diff, "Frequency tables/Diff.csv", row.names=FALSE)


##### Changes

changes <- pull_question_data("Changes") %>% add_percentages()

write.csv(changes, "Frequency tables/Changes.csv", row.names=FALSE)

##### Future

future <- pull_question_data("Future", qstoignore = "HowSeeFuture") %>% 
  add_percentages()

write.csv(future, "Frequency tables/Future.csv", row.names=FALSE)

##### Outdoor

outdoor <- pull_question_data("Outdoor") %>% add_percentages()

write.csv(outdoor, "Frequency tables/Outdoor.csv", row.names=FALSE)

##### Employment

employment <- pull_question_data("Employment", qstoignore = c("InitialShieldingEmployment",
                                                              "HRNEmployment",
                                                             "EmploymentOther")) %>% add_percentages()

write.csv(employment, "Frequency tables/Employment.csv", row.names=FALSE)

##### Agree or diagree

ad_count <- pull_question_data("AD", qstoignore = c("ADMissingSupport", 
                                                    "AdvisedShieldGP", 
                                                    "AdvisedGPImmunosuppressed", 
                                                    "WhenAdvisedHighRisk", 
                                                    "ApproachInfluenceFromShieldingAdvice", 
                                                    "ApproachNoDependenceSGovAdvice", "DiffClearYourHead")) %>%  
  add_percentages()

write.csv(ad_count, "Frequency tables/AgreeDisagree.csv", row.names=FALSE)


##### Why in highest risk group

whyhighrisk <- pull_question_data(c("Cancer", 
                                    "OrganTransplant", 
                                    "RareDisease", 
                                    "PregnantAnd",
                                    "KidneyLiverSpleen",
                                    "AdvisedShieldGP",
                                    "NotSureWhyHighRisk",
                                    "SevereRespiratory",
                                    "ImmunosuppressionTherapy")) %>% 
  add_percentages()

write.csv(whyhighrisk, "Frequency tables/WhyHighRisk.csv", row.names=FALSE)

# Other reason
whyhighriskother <- cs %>% select(HighRiskOtherReason) %>%  na.omit() 

write.csv(whyhighriskother, "Frequency tables/ListOfOtherHighRisk.csv", row.names=FALSE)

##### Tables for single variable breakdowns

one_var_names <- c("AdvisedGPImmunosuppressed", "WhenAdvisedHighRisk", "DifficultyGettingSocialCareSupport",
                   "CurrentApproachToManagingRisk", "HowSeeFuture", "AgeGroup", "Gender", "Ethnicity",
                   "Carer", "Vaccinated", "ScotlandRegion", "UnexpectedExpenseDifficulty", "InternetAccess",
                   ## New variables
                   "AgeGroup2", "Impairment", "ChildrenInHousehold", "NumberInHousehold", "SeverelyImmunosuppressed",
                   "WorriedButNoLongerHighestRisk", "SurveySubject")

for(varname in one_var_names){
  one_var_table(varname)
}

##### More specific list for impairment
imp2 <- pull_question_data(c("Impairment", "LearningDifficulty"), qstoignore = c("Impairment"), checktotals=FALSE) %>% 
  add_percentages()

write.csv(imp2, "Frequency tables/ImpairmentBreakdown.csv", row.names=FALSE)








