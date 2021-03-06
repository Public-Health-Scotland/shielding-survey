# Analysis of high risk group shielding survey for Public Health Scotland
[Find the publication here](https://www.publichealthscotland.scot/publications/covid-19-shielding-programme-scotland-impact-and-experience-survey-part-two/covid-19-shielding-programme-scotland-impact-and-experience-survey-part-two-30-march-2022/)
## Workflow

1. The uncleaned survey data are confidential so unprovided, but are stored locally in a password protected spreadsheet
2. The script cleaning_and_weighting.R cleans the raw data and calculates the rescaled weight by age group and sex by comparing to population fractions for the high risk group. 
    - We applied the following restrictions to remove entries from the survey
        - Require "Do you agree to take part in this survey" = "Yes"
        - Require "Yes" response to either they are in the highest risk group, they are caring for someone in the highest risk group(adult) or they are caring for someone in the highest risk group (child)
        - The last page completed is page 7 i.e. they completed the survey
        
      This leaves us with 11450 entries
    - We further cleaned the data by renaming columns to be more concise and easy to read
    - We removed columns which contained information for participants only
    - We made a new variable, SurveySubject, which is the result of the disambiguation questions about who the survey is reflecting.
    - For weighting, we created new columns GenderWeighting and AgeGroupWeighting, which are to be used for weighting only. We amalgamated 70-74 and 75-79 age groups for this purpose (for comparison with population). In the new GenderWeighting column, any gender that is not M/F is put as "Not specified" but this is just for weighting purposes. We retained the original Gender and Age columns for use in analysis.
    - To weight the data we used the following protocol:
        - For responses which had both a specified age and a M/F specified sex, we calculated
          grossing weight = population for group/number of survey responses for group
          rescaled weight = number of specified responses x grossing weight/total population
        - For responses which lacked one of both of specified age and M/F specified sex, we calculated the weight as
          weight (unspecified) = $\Sigma$ weights for specified entries/number of specified entries, which gives a weight of 1 as expected because we have no population data on these entries
        - The output is saved as the RescaledWeight column
        - Note that the sum of all the rescaled weights is equal to the number in the survey, as expected
        - N.B. the Weight and Weight_cutoff columns are not used as weights, and are there for comparison
3. We created new variables from the existing ones using the shielding_new_variables.R script. Specifically these are
    - AgeGroup2: different age grouping 0-15, 16-64 and 65+
    - Impairment: one or more of listed impairments
    - ChildrenInHousehold: children or no children
    - NumberInHousehold: 1, 2 or 3+
    - SeverelyImmunosuppressed: based off SeverelyImmunosuppressed and AdvisedGPImmunosuppressed
    - WorriedButNoLongerHighestRisk: based off CurrentApproachToManagingRisk and SeverelyImmunosuppressed
5. We then made frequency tables of the different variables and groups of variables, both weighted and unweighted. This is done using the script shielding_analysis_weighted.R
6. We then made crosstabs comparing each variable with several other variables. This is done using the script make_crosstabs.R


