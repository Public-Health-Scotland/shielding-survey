#### Shielding analysis

library(openxlsx)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(janitor)

# Getting functions for making summary tables
setwd("~/weighting")
source("shielding_functions.R")


cs <- read.xlsx("~/weighting/cleaned_survey.xlsx")

##### Initial shielding

is_count <- pull_question_data("InitialShielding", weighted=FALSE)

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


write.csv(is_count, "Quantitative tables/InitialShielding.csv", row.names=FALSE)

##### High risk negatives

hr_count <- pull_question_data("HRN", weighted=FALSE) %>% add_percentages()

write.csv(hr_count, "Quantitative tables/HighRiskNegatives.csv", row.names=FALSE)


##### High risk positives

hrp_count <- pull_question_data("HRP", qstoignore = c("HRPOther"), weighted=FALSE) %>%  add_percentages(addna=FALSE)

write.csv(hrp_count, "Quantitative tables/HighRiskPositives.csv", row.names=FALSE)

##### Useful

useful <- pull_question_data("Useful", qstoignore = c("UsefulOther"), weighted=FALSE) %>% add_percentages(naans="N/A")

write.csv(useful, "Quantitative tables/Useful.csv", row.names=FALSE)

##### Approach

approach <- pull_question_data("Approach", qstoignore = c("CurrentApproachToManagingRisk"), weighted=FALSE) %>% add_percentages(naans = "I am not sure / Not applicable")

write.csv(approach, "Quantitative tables/Approach.csv", row.names=FALSE)

##### SG

sg <- pull_question_data("SG", qstoignore = c("ApproachNoDependenceSGovAdvice"), weighted=FALSE) %>% add_percentages(naans = "I am not sure / Not applicable")
  
write.csv(sg, "Quantitative tables/ScottishGovernment.csv", row.names=FALSE)


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
                                                  "DiffVitDAccess"), weighted=FALSE) %>% 
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
                                                   "DiffHRVaccineEffectiveness"), weighted=FALSE) %>% 
  add_percentages(addna = FALSE)

  
diff <- rbind(diff1, diff2)

write.csv(diff, "Quantitative tables/Diff.csv", row.names=FALSE)


##### Changes

changes <- pull_question_data("Changes", weighted=FALSE) %>% add_percentages(naans = "I am not sure / Not applicable")

write.csv(changes, "Quantitative tables/Changes.csv", row.names=FALSE)

##### Future

future <- pull_question_data("Future", qstoignore = "HowSeeFuture", weighted=FALSE) %>% 
  add_percentages(naans = "I am not sure / Not applicable")

write.csv(future, "Quantitative tables/Future.csv", row.names=FALSE)

##### Outdoor

outdoor <- pull_question_data("Outdoor", weighted=FALSE) %>% add_percentages(addna=FALSE)

write.csv(outdoor, "Quantitative tables/Outdoor.csv", row.names=FALSE)

##### Employment

#employment <- pull_question_data("Employment", qstoignore = c("InitialShieldingEmployment",
#                                                              "HRNEmployment",
 #                                                             "EmploymentOther"), weighted=FALSE) %>% add_percentages(addna=FALSE)

#write.csv(employment, "Quantitative tables/Employment.csv", row.names=FALSE)

##### Agree or diagree

#ad_count <- pull_question_data("AD", qstoignore = c("ADMissingSupport", 
#                                                    "AdvisedShieldGP", 
#                                                    "AdvisedGPImmunosuppressed", 
#                                                    "WhenAdvisedHighRisk", 
#                                                    "ApproachInfluenceFromShieldingAdvice", 
#                                                    "ApproachNoDependenceSGovAdvice", "DiffClearYourHead"), weighted=FALSE) %>%  
#  add_percentages(naans="I am not sure")
  
#write.csv(ad_count, "Quantitative tables/AgreeDisagree.csv", row.names=FALSE)

##### Why in highest risk group

whyhighrisk <- pull_question_data(c("Cancer", 
                                    "OrganTransplant", 
                                    "RareDisease", 
                                    "PregnantAnd",
                                    "KidneyLiverSpleen",
                                    "AdvisedShieldGP",
                                    "NotSureWhyHighRisk"), weighted=FALSE) %>% 
  add_percentages(addna=FALSE)

write.csv(whyhighrisk, "Quantitative tables/WhyHighRisk.csv", row.names=FALSE)

# Other reason
whyhighriskother <- cs %>% select(HighRiskOtherReason) %>%  na.omit()

write.csv(whyhighriskother, "Quantitative tables/ListOfOtherHighRisk.csv", row.names=FALSE)

#### Advised GP immunosuppressed

gpimm <- table(cs$AdvisedGPImmunosuppressed) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(gpimm$Freq))) %>% 
  dplyr::rename(`AdvisedGPImmunosuppressed` = Var1, `count` = Freq)

write.csv(gpimm, "Quantitative tables/AdvisedGPImmunosuppressed.csv", row.names=FALSE)


##### When Advised High risk

when <- table(cs$WhenAdvisedHighRisk) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(when$Freq))) %>% 
  dplyr::rename(`WhenAdvisedHighRisk` = Var1, `count` = Freq)

write.csv(when, "Quantitative tables/WhenAdvisedHighRisk.csv", row.names=FALSE)

##### Difficulty getting social care support

diffic <- table(cs$DifficultyGettingSocialCareSupport) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(diffic$Freq))) %>% 
  dplyr::rename(`DifficultyGettingSocialCareSupport` = Var1, `count` = Freq)

write.csv(diffic, "Quantitative tables/DifficultyGettingSocialCareSupport.csv", row.names=FALSE)

##### Current approach to managing risk

currappr <- table(cs$CurrentApproachToManagingRisk) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(currappr$Freq))) %>% 
  dplyr::rename(`CurrentApproachToManagingRisk` = Var1, `count` = Freq)

write.csv(currappr, "Quantitative tables/CurrentApproachToManagingRisk.csv", row.names=FALSE)

##### How see the future

futuresee <- table(cs$HowSeeFuture) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(futuresee$Freq))) %>% 
  dplyr::rename(`HowSeeFuture` = Var1, `Weighted` = Freq)

write.csv(futuresee, "Quantitative tables/HowSeeFuture.csv", row.names=FALSE)

##### AgeGroup

agegrp1 <- table(cs$AgeGroup) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(agegrp1$Freq))) %>% 
  dplyr::rename(`Age Group` = Var1, `count` = Freq)

write.csv(agegrp1, "Quantitative tables/AgeGroup.csv", row.names=FALSE)


##### Gender

gender <- table(cs$Gender) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(gender$Freq))) %>% 
  dplyr::rename(`Gender` = Var1, `count` = Freq)

write.csv(gender, "Quantitative tables/Gender.csv", row.names=FALSE)

##### Ethnicity

ethnicity <- table(cs$Ethnicity) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(ethnicity$Freq))) %>% 
  dplyr::rename(`Ethnicity` = Var1, `count` = Freq)

write.csv(ethnicity, "Quantitative tables/Ethnicity.csv", row.names=FALSE)


##### Carer

carer <- table(cs$Carer) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(carer$Freq))) %>% 
  dplyr::rename(`Carer` = Var1, `count` = Freq)

write.csv(carer, "Quantitative tables/Carer.csv", row.names=FALSE)

##### Vaccinated

vacc <- table(cs$Vaccinated) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(vacc$Freq))) %>% 
  dplyr::rename(`Vaccinated` = Var1, `count` = Freq)

write.csv(vacc, "Quantitative tables/Vaccinated.csv", row.names=FALSE)

##### Scotland region

region <- table(cs$ScotlandRegion) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(region$Freq))) %>% 
  dplyr::rename(`ScotlandRegion` = Var1, `count` = Freq)

write.csv(region, "Quantitative tables/ScotlandRegion.csv", row.names=FALSE)

##### Unexpected expense difficulty

unex <- table(cs$UnexpectedExpenseDifficulty) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(unex$Freq))) %>% 
  dplyr::rename(`UnexpectedExpenseDifficulty` = Var1, `count` = Freq)

write.csv(unex, "Quantitative tables/UnexpectedExpenseDifficulty.csv", row.names=FALSE)

##### Internet

internet <- table(cs$InternetAccess) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(internet$Freq))) %>% 
  dplyr::rename(`InternetAccess` = Var1, `count` = Freq)

write.csv(internet, "Quantitative tables/InternetAccess.csv", row.names=FALSE)



########################################
############ NEW VARIABLES #############
########################################


##### New age group

agegrp <- table(cs$AgeGroup2) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(agegrp$Freq))) %>% 
  dplyr::rename(`Age Group` = Var1, `count` = Freq)

write.csv(agegrp, "Quantitative tables/NewAgeGroup.csv", row.names=FALSE)

##### Impairment

imp <- table(cs$Impairment) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(imp$Freq))) %>% 
  dplyr::rename(`Impairment` = Var1, `count` = Freq)

write.csv(imp, "Quantitative tables/Impairment.csv", row.names=FALSE)

## More specific

imp2 <- pull_question_data("Impairment", qstoignore = c("Impairment"), weighted = FALSE) %>% 
  add_percentages(addna=FALSE)

write.csv(imp2, "Quantitative tables/ImpairmentBreakdown.csv", row.names=FALSE)

##### Children in household

cih <- table(cs$ChildrenInHousehold) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(cih$Freq))) %>% 
  dplyr::rename(`Children in household` = Var1, `count` = Freq)

write.csv(cih, "Quantitative tables/ChildrenInHousehold.csv", row.names=FALSE)

##### Number in household

nih <- table(cs$NumberInHousehold) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(nih$Freq))) %>% 
  dplyr::rename(`Number in household` = Var1, `count` = Freq)

write.csv(nih, "Quantitative tables/NumberInHousehold.csv", row.names=FALSE)

##### Severely immunosuppressed

severeimmuno <- table(cs$SeverelyImmunosuppressed) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(severeimmuno$Freq))) %>% 
  dplyr::rename(`Severely Immunosuppressed` = Var1, `count` = Freq)

write.csv(severeimmuno, "Quantitative tables/SeverelyImmunosuppressed.csv", row.names=FALSE)


##### Worried but no longer highest risk

worried <- table(cs$WorriedButNoLongerHighestRisk) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(worried$Freq))) %>% 
  dplyr::rename(`Worried but no longer highest risk` = Var1, `count` = Freq)

write.csv(worried, "Quantitative tables/WorriedButNoLongerHighestRisk.csv", row.names=FALSE)


##### Survey subject

subject <- table(cs$SurveySubject) %>% 
  as.data.frame() %>% 
  mutate(Percentage = 100*(Freq/sum(subject$Freq))) %>% 
  dplyr::rename(`SurveySubject` = Var1, `count` = Freq)

write.csv(subject, "Quantitative tables/SurveySubject.csv", row.names=FALSE)




