# Weighted shielding analysis


library(openxlsx)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(janitor)


# Getting functions for making summary tables
setwd("~/weighting")
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


write.csv(is_count, "Quantitative tables weighted/InitialShielding.csv", row.names=FALSE)

##### High risk negatives

hr_count <- pull_question_data("HRN") %>% add_percentages()

write.csv(hr_count, "Quantitative tables weighted/HighRiskNegatives.csv", row.names=FALSE)


##### High risk positives

hrp_count <- pull_question_data("HRP", qstoignore = c("HRPOther")) %>%  add_percentages(addna=FALSE)

write.csv(hrp_count, "Quantitative tables weighted/HighRiskPositives.csv", row.names=FALSE)

##### Useful

useful <- pull_question_data("Useful", qstoignore = c("UsefulOther")) %>% add_percentages(naans = "N/A")

write.csv(useful, "Quantitative tables weighted/Useful.csv", row.names=FALSE)

##### Approach

approach <- pull_question_data("Approach", qstoignore = c("CurrentApproachToManagingRisk")) %>% add_percentages(naans = "I am not sure / Not applicable")

write.csv(approach, "Quantitative tables weighted/Approach.csv", row.names=FALSE)

##### SG

sg <- pull_question_data("SG", qstoignore = c("ApproachNoDependenceSGovAdvice")) %>% add_percentages(naans = "I am not sure / Not applicable")

write.csv(sg, "Quantitative tables weighted/ScottishGovernment.csv", row.names=FALSE)


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
  add_percentages(addna = FALSE)


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
  add_percentages(addna = FALSE)


diff <- rbind(diff1, diff2)

write.csv(diff, "Quantitative tables weighted/Diff.csv", row.names=FALSE)


##### Changes

changes <- pull_question_data("Changes") %>% add_percentages(naans = "I am not sure / Not applicable")

write.csv(changes, "Quantitative tables weighted/Changes.csv", row.names=FALSE)

##### Future

future <- pull_question_data("Future", qstoignore = "HowSeeFuture") %>% 
  add_percentages(naans = "I am not sure / Not applicable")

write.csv(future, "Quantitative tables weighted/Future.csv", row.names=FALSE)

##### Outdoor

outdoor <- pull_question_data("Outdoor") %>% add_percentages(addna=FALSE)

write.csv(outdoor, "Quantitative tables weighted/Outdoor.csv", row.names=FALSE)

##### Employment

employment <- pull_question_data("Employment", qstoignore = c("InitialShieldingEmployment",
                                                              "HRNEmployment",
                                                             "EmploymentOther"),
                                 omitnans = FALSE) %>% add_percentages(addna=FALSE)

write.csv(employment, "Quantitative tables weighted/Employment.csv", row.names=FALSE)

##### Agree or diagree

ad_count <- pull_question_data("AD", qstoignore = c("ADMissingSupport", 
                                                    "AdvisedShieldGP", 
                                                    "AdvisedGPImmunosuppressed", 
                                                    "WhenAdvisedHighRisk", 
                                                    "ApproachInfluenceFromShieldingAdvice", 
                                                    "ApproachNoDependenceSGovAdvice", "DiffClearYourHead")) %>%  
  add_percentages(naans="I am not sure")

write.csv(ad_count, "Quantitative tables weighted/AgreeDisagree.csv", row.names=FALSE)


##### Why in highest risk group

whyhighrisk <- pull_question_data(c("Cancer", 
                                    "OrganTransplant", 
                                    "RareDisease", 
                                    "PregnantAnd",
                                    "KidneyLiverSpleen",
                                    "AdvisedShieldGP",
                                    "NotSureWhyHighRisk")) %>% 
  add_percentages(addna=FALSE)

write.csv(whyhighrisk, "Quantitative tables weighted/WhyHighRisk.csv", row.names=FALSE)

# Other reason
whyhighriskother <- cs %>% select(HighRiskOtherReason) %>%  na.omit() 

write.csv(whyhighriskother, "Quantitative tables weighted/ListOfOtherHighRisk.csv", row.names=FALSE)

##### Advised GP Immunosuppressed

gpimm <- questionr::wtd.table(cs$AdvisedGPImmunosuppressed, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(gpimm$Freq))) %>% 
  dplyr::rename(`AdvisedGPImmunosuppressed` = Var1, `Weighted count` = Freq) 

write.csv(gpimm, "Quantitative tables weighted/AdvisedGPImmunosuppressed.csv", row.names=FALSE)

##### When Advised High risk

when <- questionr::wtd.table(cs$WhenAdvisedHighRisk, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(when$Freq))) %>% 
  dplyr::rename(`WhenAdvisedHighRisk` = Var1, `Weighted count` = Freq)

write.csv(when, "Quantitative tables weighted/WhenAdvisedHighRisk.csv", row.names=FALSE)

##### Difficulty getting social care support

diffic <- questionr::wtd.table(cs$DifficultyGettingSocialCareSupport, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(diffic$Freq))) %>% 
  dplyr::rename(`DifficultyGettingSocialCareSupport` = Var1, `Weighted count` = Freq)

write.csv(diffic, "Quantitative tables weighted/DifficultyGettingSocialCareSupport.csv", row.names=FALSE)

##### Current approach to managing risk

currappr <- questionr::wtd.table(cs$CurrentApproachToManagingRisk, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(currappr$Freq))) %>% 
  dplyr::rename(`CurrentApproachToManagingRisk` = Var1, `Weighted count` = Freq)

write.csv(currappr, "Quantitative tables weighted/CurrentApproachToManagingRisk.csv", row.names=FALSE)

##### How see the future

futuresee <- questionr::wtd.table(cs$HowSeeFuture, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(futuresee$Freq))) %>% 
  dplyr::rename(`HowSeeFuture` = Var1, `Weighted count` = Freq)

write.csv(futuresee, "Quantitative tables weighted/HowSeeFuture.csv", row.names=FALSE)

##### AgeGroup

agegrp1 <- questionr::wtd.table(cs$AgeGroup, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(agegrp1$Freq))) %>% 
  dplyr::rename(`Age Group` = Var1, `Weighted count` = Freq)

write.csv(agegrp1, "Quantitative tables weighted/AgeGroup.csv", row.names=FALSE)


##### Gender

gender <- questionr::wtd.table(cs$Gender, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(gender$Freq))) %>% 
  dplyr::rename(`Gender` = Var1, `Weighted count` = Freq)

write.csv(gender, "Quantitative tables weighted/Gender.csv", row.names=FALSE)

##### Ethnicity

ethnicity <- questionr::wtd.table(cs$Ethnicity, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(ethnicity$Freq))) %>% 
  dplyr::rename(`Ethnicity` = Var1, `Weighted count` = Freq)

write.csv(ethnicity, "Quantitative tables weighted/Ethnicity.csv", row.names=FALSE)


##### Carer

carer <- questionr::wtd.table(cs$Carer, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(carer$Freq))) %>% 
  dplyr::rename(`Carer` = Var1, `Weighted count` = Freq)

write.csv(carer, "Quantitative tables weighted/Carer.csv", row.names=FALSE)

##### Vaccinated

vacc <- questionr::wtd.table(cs$Vaccinated, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(vacc$Freq))) %>% 
  dplyr::rename(`Vaccinated` = Var1, `Weighted count` = Freq)

write.csv(vacc, "Quantitative tables weighted/Vaccinated.csv", row.names=FALSE)

##### Scotland region

region <- questionr::wtd.table(cs$ScotlandRegion, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(region$Freq))) %>% 
  dplyr::rename(`ScotlandRegion` = Var1, `Weighted count` = Freq)

write.csv(region, "Quantitative tables weighted/ScotlandRegion.csv", row.names=FALSE)

##### Unexpected expense difficulty

unex <- questionr::wtd.table(cs$UnexpectedExpenseDifficulty, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(unex$Freq))) %>% 
  dplyr::rename(`UnexpectedExpenseDifficulty` = Var1, `Weighted count` = Freq)

write.csv(unex, "Quantitative tables weighted/UnexpectedExpenseDifficulty.csv", row.names=FALSE)

##### Internet

internet <- questionr::wtd.table(cs$InternetAccess, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(internet$Freq))) %>% 
  dplyr::rename(`InternetAccess` = Var1, `Weighted count` = Freq)

write.csv(internet, "Quantitative tables weighted/InternetAccess.csv", row.names=FALSE)


########################################
############ NEW VARIABLES #############
########################################


##### New age group

agegrp <- questionr::wtd.table(cs$AgeGroup2, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(agegrp$Freq))) %>% 
  dplyr::rename(`Age Group` = Var1, `Weighted count` = Freq)

write.csv(agegrp, "Quantitative tables weighted/NewAgeGroup.csv", row.names=FALSE)

##### Impairment

imp <- questionr::wtd.table(cs$Impairment, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(imp$Freq))) %>% 
  dplyr::rename(`Impairment` = Var1, `Weighted count` = Freq)

write.csv(imp, "Quantitative tables weighted/Impairment.csv", row.names=FALSE)

## More specific

imp2 <- pull_question_data("Impairment", qstoignore = c("Impairment")) %>% 
  add_percentages(addna=FALSE)

write.csv(imp2, "Quantitative tables weighted/ImpairmentBreakdown.csv", row.names=FALSE)



##### Children in household

cih <- questionr::wtd.table(cs$ChildrenInHousehold, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(cih$Freq))) %>% 
  dplyr::rename(`Children in household` = Var1, `Weighted count` = Freq)

write.csv(cih, "Quantitative tables weighted/ChildrenInHousehold.csv", row.names=FALSE)

##### Number in household

nih <- questionr::wtd.table(cs$NumberInHousehold, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(nih$Freq))) %>% 
  dplyr::rename(`Number in household` = Var1, `Weighted count` = Freq)

write.csv(nih, "Quantitative tables weighted/NumberInHousehold.csv", row.names=FALSE)

##### Severely immunosuppressed

severeimmuno <- questionr::wtd.table(cs$SeverelyImmunosuppressed, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(severeimmuno$Freq))) %>% 
  dplyr::rename(`Severely Immunosuppressed` = Var1, `Weighted count` = Freq)

write.csv(severeimmuno, "Quantitative tables weighted/SeverelyImmunosuppressed.csv", row.names=FALSE)


##### Worried but no longer highest risk

worried <- questionr::wtd.table(cs$WorriedButNoLongerHighestRisk, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(worried$Freq))) %>% 
  dplyr::rename(`Worried but no longer highest risk` = Var1, `Weighted count` = Freq)

write.csv(worried, "Quantitative tables weighted/WorriedButNoLongerHighestRisk.csv", row.names=FALSE)


##### Survey subject

subject <- questionr::wtd.table(cs$SurveySubject, weights = cs$RescaledWeight) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(subject$Freq))) %>% 
  dplyr::rename(`SurveySubject` = Var1, `Weighted count` = Freq)

write.csv(subject, "Quantitative tables weighted/SurveySubject.csv", row.names=FALSE)







