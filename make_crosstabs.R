# ----------------
# Making crosstabs
# ----------------

# Get necessary functions
source("crosstab_functions.R")

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