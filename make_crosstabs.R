# ----------------
# Making crosstabs
# ----------------
library(magrittr)
library(tidyr)
library(dplyr)
library(janitor)
library(tidyverse)
library(glue)

# Get necessary functions
source("crosstab_functions.R")

# Getting cleaned data
cs <- openxlsx::read.xlsx("survey_with_new_vars.xlsx")


# ---------------------------------------------------------------------------------------------
# Carer
# ---------------------------------------------------------------------------------------------
primary_variable <- "Carer"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare", "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport")
selections <- list("ADSupportSinceShielding" = list("Disagree", "Strongly disagree"),
                   "DifficultyGettingSocialCareSupport" = list("Quite difficult", "Very difficult"))

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Shielding or caring
# ---------------------------------------------------------------------------------------------
primary_variable <- "SurveySubject"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport")
selections <- list("ADSupportSinceShielding" = list("Disagree", "Strongly disagree"),
                   "DifficultyGettingSocialCareSupport" = list("Quite difficult", "Very difficult"))

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Clinical category
# ---------------------------------------------------------------------------------------------
primary_variable <- "WhenAdvisedHighRisk"
secondary_variables <- c("WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "UsefulVaccineEffectivenessInfo",
                         "CurrentApproachToManagingRisk", "FutureHRGroup")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Gender
# ---------------------------------------------------------------------------------------------
primary_variable <- "Gender"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Age Groups
# ---------------------------------------------------------------------------------------------
primary_variable <- "AgeGroup2"
secondary_variables <- c("InitialShieldingEmployment", "UsefulReturnWorkNewJobSupport",
                         "SGSafeToWork", "HRPEmployerFlexibility")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)

primary_variable <- "AgeGroup3"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed","ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport")

selections <- list("ADSupportSinceShielding" = list("Disagree", "Strongly disagree"),
                   "DifficultyGettingSocialCareSupport" = list("Quite difficult", "Very difficult"))

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Impairment
# ---------------------------------------------------------------------------------------------
primary_variable <- "Impairment"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare", "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Number in household
# ---------------------------------------------------------------------------------------------
primary_variable <- "NumberInHousehold"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Children
# ---------------------------------------------------------------------------------------------
primary_variable <- "ChildrenInHousehold"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare", "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport", "SGSafeYoungSchoolUniChildcare")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Deprivation
# ---------------------------------------------------------------------------------------------
primary_variable <- "UnexpectedExpenseDifficulty"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingFinance", "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport", "SGSafePublicTransport",
                         "ChangesEndFreeFoodBoxes")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Employment
# ---------------------------------------------------------------------------------------------
primary_variable <- "Employment"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare",
                         "InitialShieldingEmployment", "InitialShieldingEducation",
                          "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport","UsefulReturnWorkNewJobSupport",
                         "UsefulReturnEducationSupport", "SGSafeToWork", "SGSafeYoungSchoolUniChildcare",
                         "HRPEmployerFlexibility", "HRPEducationFlexibility")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)

primary_variable <- "EmploymentEmployed"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare",
                         "InitialShieldingEmployment", "InitialShieldingEducation",
                         "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport","UsefulReturnWorkNewJobSupport",
                         "UsefulReturnEducationSupport", "SGSafeToWork", "SGSafeYoungSchoolUniChildcare",
                         "HRPEmployerFlexibility", "HRPEducationFlexibility")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)

primary_variable <- "EmploymentRetired"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare",
                         "InitialShieldingEmployment", "InitialShieldingEducation",
                         "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport","UsefulReturnWorkNewJobSupport",
                         "UsefulReturnEducationSupport", "SGSafeToWork", "SGSafeYoungSchoolUniChildcare",
                         "HRPEmployerFlexibility", "HRPEducationFlexibility")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)

primary_variable <- "EmploymentUnemployed"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare",
                         "InitialShieldingEmployment", "InitialShieldingEducation",
                         "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport","UsefulReturnWorkNewJobSupport",
                         "UsefulReturnEducationSupport", "SGSafeToWork", "SGSafeYoungSchoolUniChildcare",
                         "HRPEmployerFlexibility", "HRPEducationFlexibility")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)

primary_variable <- "EmploymentLookingAfterHomeOrFamily"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare",
                         "InitialShieldingEmployment", "InitialShieldingEducation",
                         "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport","UsefulReturnWorkNewJobSupport",
                         "UsefulReturnEducationSupport", "SGSafeToWork", "SGSafeYoungSchoolUniChildcare",
                         "HRPEmployerFlexibility", "HRPEducationFlexibility")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)

primary_variable <- "EmploymentNotWorkingDisabilityOrCondition"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare",
                         "InitialShieldingEmployment", "InitialShieldingEducation",
                         "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport","UsefulReturnWorkNewJobSupport",
                         "UsefulReturnEducationSupport", "SGSafeToWork", "SGSafeYoungSchoolUniChildcare",
                         "HRPEmployerFlexibility", "HRPEducationFlexibility")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)

primary_variable <- "EmploymentEducation"
secondary_variables <- c("InitialShieldingQualityOfLife", "InitialShieldingMentalHealth",
                         "InitialShieldingQualityOfCare",
                         "InitialShieldingEmployment", "InitialShieldingEducation",
                         "WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ADSupportSinceShielding", 
                         "DifficultyGettingSocialCareSupport","UsefulReturnWorkNewJobSupport",
                         "UsefulReturnEducationSupport", "SGSafeToWork", "SGSafeYoungSchoolUniChildcare",
                         "HRPEmployerFlexibility", "HRPEducationFlexibility")
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)

# ---------------------------------------------------------------------------------------------
# Worried but no longer at highest risk
# ---------------------------------------------------------------------------------------------
primary_variable <- "WorriedButNoLongerHighestRisk"
secondary_variables <- c("UsefulMentalHealthSupport","UsefulPhysicalHealthSupport",  "UsefulReturnWorkNewJobSupport",
                         "UsefulReturnEducationSupport", "UsefulVaccineEffectivenessInfo",
                         "UsefulMorePeopleFollowedGuidelines", "UsefulNoMoreHighRiskVulnerableTerms", 
                         "UsefulNoneAboveJustTime", "UsefulNoneAboveNotWantGoBack",
                         "FutureHRWebpage", "FutureChiefMedicalOfficerLetter", "FutureSMSShieldingUpdates",
                         "FutureHRGroup" )
selections <- NA
gitcreds::gitcreds_set()
make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Severely immunosuppressed
# ---------------------------------------------------------------------------------------------
primary_variable <- "SeverelyImmunosuppressed"
secondary_variables <- c("UsefulMentalHealthSupport","UsefulPhysicalHealthSupport",  "UsefulReturnWorkNewJobSupport",
                         "UsefulReturnEducationSupport", "ChangesEndFreeFoodBoxes",
                         "UsefulMorePeopleFollowedGuidelines", "UsefulNoMoreHighRiskVulnerableTerms", 
                         "UsefulNoneAboveJustTime", "UsefulNoneAboveNotWantGoBack", "CurrentApproachToManagingRisk",
                         "ApproachMainlyDrivenFearInfection", "ApproachInfluenceFromShieldingAdvice",
                         "ApproachNoDependenceSGovAdvice", "ApproachLessAfraidSinceVaccine", 
                         "ApproachHardToTrustGuidanceWhichSaysSafe", "SGSafeToWork" , "SGSafePublicTransport",
                         "SGSafeYoungSchoolUniChildcare", "SGSafeSameRestPopulation", "HowSeeFuture",
                         "FutureHRWebpage", "FutureChiefMedicalOfficerLetter", "FutureSMSShieldingUpdates", 
                         "FutureHRGroup"
 )
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)
# ---------------------------------------------------------------------------------------------
# Vaccination status
# ---------------------------------------------------------------------------------------------
primary_variable <- "Vaccinated"
secondary_variables <- c("WorriedButNoLongerHighestRisk",
                         "SeverelyImmunosuppressed", "ApproachMainlyDrivenFearInfection"
)
selections <- NA

make_crosstab(primary_variable, secondary_variables, selections)

