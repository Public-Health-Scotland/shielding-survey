library(dplyr)
library(magrittr)
options(java.parameters = "-Xmx4g")
library(XLConnect)
library(purrr)
library(janitor)
library(keyring)

xlcFreeMemory()

setwd("~/weighting")


keyring::keyring_unlock(keyring = "weighting",
                        password = source("~/weighting/survey_keyring.R")[["value"]])

wb <- loadWorkbook("results-survey525584.xlsx", 
                   password = keyring::key_get("weighting_survey", Sys.info()[["user"]], keyring = "weighting"))

survey <- data.frame(readWorksheet(wb, sheet=1, check.names=FALSE))

xlcFreeMemory()

weight_dat <- read.csv("weight_dat.csv")


################### Cleaning ################### 

# Check they agree to take part and completed the survey
survey %<>% 
  janitor::clean_names() %>% 
  filter(do_you_agree_to_take_part_in_this_survey =="Yes") %>% 
  filter(are_you_in_the_highest_risk_group_or_are_you_caring_for_someone_else_who_is_i_am_in_the_highest_risk_group == "Yes" | 
         are_you_in_the_highest_risk_group_or_are_you_caring_for_someone_else_who_is_i_am_caring_for_a_child_under_16_who_is_in_the_highest_risk_group == "Yes" |
         are_you_in_the_highest_risk_group_or_are_you_caring_for_someone_else_who_is_i_am_caring_for_an_adult_16_or_older_who_is_in_the_highest_risk_group == "Yes",
                   last_page ==7
         ) %>% 
# Remove information columns  
  select(-c(col7, 
            contains("information_about_this_survey"),
            contains("in_march_2020"), 
            contains("unfortunately_this_survey"),
            contains("thank_you_for_your_time_and_consideration"),
            contains("agree_to_take_part"),
            last_page
            )
         ) %>% 
   dplyr::rename(
          ResponseID = response_id,
          Seed = seed,
          StartLanguage = start_language,
          DateSubmitted = date_submitted,
          # Risk group
          ShieldingLetter = have_you_or_the_person_you_care_for_ever_received_a_letter_from_scotland_s_chief_medical_officer_advising_you_that_you_were_in_the_shielding_or_highest_risk_group,
          IAmInHighRisk = are_you_in_the_highest_risk_group_or_are_you_caring_for_someone_else_who_is_i_am_in_the_highest_risk_group,
          CareChildHighRisk = are_you_in_the_highest_risk_group_or_are_you_caring_for_someone_else_who_is_i_am_caring_for_a_child_under_16_who_is_in_the_highest_risk_group,
          CareAdultHighRisk = are_you_in_the_highest_risk_group_or_are_you_caring_for_someone_else_who_is_i_am_caring_for_an_adult_16_or_older_who_is_in_the_highest_risk_group,
          DisambigOwnOrNot = you_ticked_more_than_one_option_in_the_previous_question_this_means_we_need_a_bit_more_information_to_help_us_interpret_your_responses_to_the_rest_of_the_survey_where_there_is_a_choice_will_your_response_relate_to_your_own_situation_or_the_situation_of_the_person_you_are_caring_for,
          DisambigChildOrAdult = you_ticked_more_than_one_option_in_the_previous_question_this_means_we_need_a_bit_more_information_to_help_us_interpret_your_responses_to_the_rest_of_the_survey_where_there_is_a_choice_will_your_response_relate_to_the_child_you_are_caring_for_or_the_adult_you_are_caring_for,
          DisambigOwnChildOrAdult = you_ticked_more_than_one_option_in_the_previous_question_this_means_we_need_a_bit_more_information_to_help_us_interpret_your_responses_to_the_rest_of_the_survey_where_there_is_a_choice_will_your_response_relate_to_your_own_situation_the_situation_of_the_child_you_are_caring_for_or_the_situation_of_the_adult_you_are_caring_for,
          SevereRespiratory = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_i_have_a_severe_respiratory_condition,
          ImmunosuppressionTherapy = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_i_am_on_immunosuppression_therapy,
          Cancer = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_i_am_being_treated_for_cancer,
          OrganTransplant = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_i_have_received_an_organ_transplant,
          RareDisease = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_i_have_a_rare_disease,
          PregnantAndHeartDisease = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_i_am_pregnant_and_have_significant_heart_disease,
          KidneyLiverSpleen = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_i_have_severe_kidney_disease_or_liver_cirrhosis_or_have_had_my_spleen_removed,
          AdvisedShieldGP = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_none_of_the_above_i_was_advised_to_shield_by_my_gp_or_consultant,
          NotSureWhyHighRisk = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_i_am_not_sure,
          HighRiskOtherReason = why_are_you_or_the_person_you_care_for_in_the_highest_risk_group_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_other,
          AdvisedGPImmunosuppressed = have_you_been_advised_by_a_gp_or_consultant_that_you_or_the_person_you_care_for_are_severely_immunosuppressed_or_immunocompromised,
          WhenAdvisedHighRisk = when_were_you_or_the_person_you_care_for_first_advised_that_you_were_in_the_highest_risk_group,
          # Shielding impact
          InitialShieldingQualityOfLife = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_quality_of_life,
          InitialShieldingMentalHealth = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_mental_health,
          InitialShieldingPhysicalHealth = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_physical_health,
          InitialShieldingPhysicalActivity = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_how_much_physical_activity_you_do,
          InitialShieldingConfidenceLeavingHome = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_confidence_when_leaving_your_home,
          InitialShieldingLonely = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_how_lonely_you_feel,
          InitialShieldingRelationshipPartner = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_relationship_with_your_partner,
          InitialShieldingRelationshipChildren = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_relationship_with_your_child_ren,
          InitialShieldingRelationshipFamilyFriends = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_relationship_with_other_family_and_friends,
          InitialShieldingQualityOfCare = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_the_quality_of_care_you_receive,
          InitialShieldingEmployment = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_employment,
          InitialShieldingEducation = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_education,
          InitialShieldingFinance = the_initial_advice_to_shield_to_minimise_all_physical_contact_with_others_was_in_place_from_march_until_july_2020_do_you_feel_that_you_or_the_person_you_care_for_now_still_experience_negative_impacts_from_this_initial_shielding_period_please_tell_us_how_much_of_a_negative_impact_you_still_experience_for_each_of_the_different_aspects_mentioned_below_your_financial_situation,
          # High Risk Group negative impacts
          HRNQualityOfLife = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_quality_of_life,
          HRNMentalHealth = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_mental_health,
          HRNPhysicalHealth = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_physical_health,
          HRNPhysicalActivity = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_how_much_physical_activity_you_do,
          HRNConfidenceLeavingHome = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_confidence_when_leaving_your_home,
          HRNLonely = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_how_lonely_you_feel,
          HRNRelationshipPartner = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_relationship_with_your_partner,
          HRNRelationshipChildren = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_relationship_with_your_child_ren,
          HRNRelationshipFamilyFriends = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_relationship_with_other_family_and_friends,
          HRNQualityCare = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_the_quality_of_care_you_receive,
          HRNEmployment = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_employment,
          HRNEducation = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_education,
          HRNFinancE = has_being_included_in_the_highest_risk_group_had_any_negative_impacts_please_tell_us_how_much_of_a_negative_impact_you_have_experienced_for_each_of_the_different_aspects_mentioned_below_your_financial_situation,
          # HR Risk Group positive impacts
          HRPExtraHealthcareSupport = has_being_included_in_the_highest_risk_group_had_any_positive_impacts_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_extra_support_when_accessing_healthcare,
          HRPExtraSupportFamilyFriends = has_being_included_in_the_highest_risk_group_had_any_positive_impacts_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_extra_support_from_family_or_friends,
          HRPImprovedMentalHealth = has_being_included_in_the_highest_risk_group_had_any_positive_impacts_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_improved_mental_health,
          HRPLiveMoreHealthily = has_being_included_in_the_highest_risk_group_had_any_positive_impacts_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_i_have_tried_to_live_even_more_healthily,
          HRPEmployerFlexibility = has_being_included_in_the_highest_risk_group_had_any_positive_impacts_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_flexibility_from_my_employer,
          HRPEducationFlexibility = has_being_included_in_the_highest_risk_group_had_any_positive_impacts_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_flexibility_from_my_school_college_or_university,
          HRPRelationshipFamilyFriends = has_being_included_in_the_highest_risk_group_had_any_positive_impacts_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_better_relationships_with_family_or_friends,
          HRPNoneAbove = has_being_included_in_the_highest_risk_group_had_any_positive_impacts_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_none_of_the_above,
          HRPOther = has_being_included_in_the_highest_risk_group_had_any_positive_impacts_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_other,
          # Agree or Disagree statements
          ADVulnerable = do_you_or_the_person_you_care_for_agree_with_the_following_statements_please_tell_us_to_what_extent_you_agree_or_disagree_being_included_on_the_highest_risk_list_has_made_me_feel_vulnerable,
          ADSupported = do_you_or_the_person_you_care_for_agree_with_the_following_statements_please_tell_us_to_what_extent_you_agree_or_disagree_being_included_on_the_highest_risk_list_has_made_me_feel_supported,
          ADSupportSinceShielding = do_you_or_the_person_you_care_for_agree_with_the_following_statements_please_tell_us_to_what_extent_you_agree_or_disagree_since_shielding_was_paused_31_july_2020_i_have_received_the_advice_and_support_i_need,
          ADMissingSupport = you_disagreed_with_the_statement_that_you_have_received_the_advice_and_support_you_need_what_advice_or_support_has_been_missing_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_you_can_choose_to_leave_this_text_box_empty,
          # 
          DifficultyGettingSocialCareSupport = how_difficult_has_it_been_to_get_the_social_care_support_which_you_or_the_person_you_care_for_need_social_care_support_includes_for_example_home_care_or_respite_care,
          CurrentApproachToManagingRisk = which_of_the_following_statements_best_describes_your_current_approach_to_managing_risk_or_the_approach_of_the_person_you_care_for_please_only_consider_covid_19_related_risk,
          # Useful to return to pre-COVID activities
          UsefulPhysicalHealthSupport = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_support_for_my_physical_health_and_wellbeing,
          UsefulMentalHealthSupport = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_support_for_my_mental_health_and_wellbeing_for_example_to_help_with_anxiety_or_loneliness,
          UsefulReturnWorkNewJobSupport = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_support_to_help_me_return_to_work_or_find_a_new_job,
          UsefulReturnEducationSupport = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_support_to_help_me_return_to_education,
          UsefulVaccineEffectivenessInfo = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_more_information_about_the_effectiveness_of_vaccination_for_people_with_my_clinical_condition_s,
          UsefulMorePeopleFollowedGuidelines = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_if_more_people_followed_the_covid_19_guidelines,
          UsefulNoMoreHighRiskVulnerableTerms = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_if_terms_such_as_highest_risk_or_extremely_vulnerable_were_no_longer_used,
          UsefulNoneAboveJustTime = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_none_of_the_above_i_just_need_time_and_will_go_back_to_doing_things_at_my_own_pace,
          UsefulNoneAboveNotWantGoBack = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_none_of_the_above_i_do_not_wish_to_go_back_to_doing_most_of_the_things_i_did_before_covid_19_hit,
          UsefulOther = what_would_be_useful_to_help_you_feel_comfortable_doing_most_or_all_of_the_things_you_did_before_covid_19_hit_if_you_select_something_else_please_remember_not_to_include_anything_in_your_response_which_might_reveal_your_identity_other,
          # Current approach
          ApproachLessAfraidSinceVaccine = do_you_or_the_person_you_care_for_agree_with_the_following_statements_about_your_current_approach_to_managing_risk_please_only_consider_covid_19_related_risk_please_tell_us_to_what_extent_you_agree_or_disagree_i_am_less_afraid_of_covid_19_infection_since_i_have_been_fully_vaccinated,
          ApproachInfluenceFromShieldingAdvice = do_you_or_the_person_you_care_for_agree_with_the_following_statements_about_your_current_approach_to_managing_risk_please_only_consider_covid_19_related_risk_please_tell_us_to_what_extent_you_agree_or_disagree_the_initial_march_2020_shielding_advice_continues_to_influence_my_approach_to_risk,
          ApproachNoDependenceSGovAdvice = do_you_or_the_person_you_care_for_agree_with_the_following_statements_about_your_current_approach_to_managing_risk_please_only_consider_covid_19_related_risk_please_tell_us_to_what_extent_you_agree_or_disagree_i_no_longer_depend_on_scottish_government_advice_to_manage_my_risk_i_can_make_my_own_decisions,
          ApproachHardToTrustGuidanceWhichSaysSafe = do_you_or_the_person_you_care_for_agree_with_the_following_statements_about_your_current_approach_to_managing_risk_please_only_consider_covid_19_related_risk_please_tell_us_to_what_extent_you_agree_or_disagree_i_find_it_hard_to_trust_guidance_which_tells_me_that_it_is_now_safe_to_do_things,
          ApproachMainlyDrivenFearInfection = do_you_or_the_person_you_care_for_agree_with_the_following_statements_about_your_current_approach_to_managing_risk_please_only_consider_covid_19_related_risk_please_tell_us_to_what_extent_you_agree_or_disagree_i_still_make_decisions_that_are_mainly_driven_by_fear_of_covid_19_infection,
          # Scottish Gov Advice
          SGSafeToWork = do_you_or_the_person_you_care_for_agree_with_the_current_scottish_government_advice_for_the_highest_risk_group_please_tell_us_to_what_extent_you_agree_or_disagree_with_each_of_the_following_elements_of_the_advice_it_is_safe_for_people_in_the_highest_risk_group_to_go_into_work_if_it_is_not_possible_to_work_from_home,
          SGSafePublicTransport = do_you_or_the_person_you_care_for_agree_with_the_current_scottish_government_advice_for_the_highest_risk_group_please_tell_us_to_what_extent_you_agree_or_disagree_with_each_of_the_following_elements_of_the_advice_it_is_safe_for_people_in_the_highest_risk_group_to_use_public_transport,
          SGSafeYoungSchoolUniChildcare = do_you_or_the_person_you_care_for_agree_with_the_current_scottish_government_advice_for_the_highest_risk_group_please_tell_us_to_what_extent_you_agree_or_disagree_with_each_of_the_following_elements_of_the_advice_it_is_safe_for_children_and_young_people_in_the_highest_risk_group_to_go_to_school_university_and_college_and_childcare,
          SGSafeSameRestPopulation = do_you_or_the_person_you_care_for_agree_with_the_current_scottish_government_advice_for_the_highest_risk_group_please_tell_us_to_what_extent_you_agree_or_disagree_with_each_of_the_following_elements_of_the_advice_it_is_safe_for_people_in_the_highest_risk_group_to_follow_the_same_advice_as_for_the_rest_of_the_population,
          #
          HowSeeFuture = which_of_the_following_statements_best_describes_how_you_or_the_person_you_care_for_see_the_future,
          # Which sources of advice made a difference
          DiffChiefMedicalOfficerLetter = for_each_of_the_following_sources_of_advice_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_letters_from_the_chief_medical_officer_to_the_highest_risk_group,
          DiffBookletBalancingRiskDailyActivities = for_each_of_the_following_sources_of_advice_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_booklet_balancing_the_risk_of_daily_activities_during_coronavirus,
          DiffClearYourHead = for_each_of_the_following_sources_of_advice_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_clear_your_head_leaflet_to_support_your_mental_health,
          DiffGoingShopping = for_each_of_the_following_sources_of_advice_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_advice_on_going_shopping_for_the_highest_risk_group,
          DiffWorkplaceSafety = for_each_of_the_following_sources_of_advice_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_advice_on_workplace_safety_for_the_highest_risk_group,
          DiffInfoLocalCases = for_each_of_the_following_sources_of_advice_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_information_about_the_number_of_covid_19_cases_in_your_local_neighbourhood,
          DiffHRVaccineEffectiveness = for_each_of_the_following_sources_of_advice_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_information_about_the_effectiveness_of_vaccination_in_the_highest_risk_group,
          DiffPriorityVaccine = for_each_of_the_following_support_offers_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_priority_access_to_covid_19_vaccination_for_you,
          DiffPriorityVaccineHousehold = for_each_of_the_following_support_offers_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_priority_access_to_covid_19_vaccination_for_others_in_your_household,
          DiffLFTAccess = for_each_of_the_following_support_offers_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_access_to_lateral_flow_tests,
          DiffVitDAccess = for_each_of_the_following_support_offers_please_tell_us_whether_it_has_made_a_difference_to_you_or_the_person_you_care_for_free_access_to_vitamin_d,
          # Changes
          ChangesShieldingToHighestRisk = over_time_there_have_been_a_number_of_changes_in_the_support_available_to_individuals_in_the_highest_risk_group_for_each_of_the_changes_mentioned_below_please_tell_us_whether_this_change_has_been_problematic_for_you_or_the_person_you_care_for_the_term_shielding_has_been_replaced_by_highest_risk,
          ChangesEndFreeFoodBoxes = over_time_there_have_been_a_number_of_changes_in_the_support_available_to_individuals_in_the_highest_risk_group_for_each_of_the_changes_mentioned_below_please_tell_us_whether_this_change_has_been_problematic_for_you_or_the_person_you_care_for_the_national_programme_of_free_food_boxes_has_been_discontinued,
          ChangesEndSupermarketPriority = over_time_there_have_been_a_number_of_changes_in_the_support_available_to_individuals_in_the_highest_risk_group_for_each_of_the_changes_mentioned_below_please_tell_us_whether_this_change_has_been_problematic_for_you_or_the_person_you_care_for_individuals_in_the_highest_risk_group_can_no_longer_register_for_priority_access_to_supermarket_online_delivery_slots,
          ChangesEndAvoidWorkplace = over_time_there_have_been_a_number_of_changes_in_the_support_available_to_individuals_in_the_highest_risk_group_for_each_of_the_changes_mentioned_below_please_tell_us_whether_this_change_has_been_problematic_for_you_or_the_person_you_care_for_individuals_in_the_highest_risk_group_are_no_longer_advised_to_stay_away_from_their_workplace,
          # Future changes,
          FutureFMBriefings = for_each_of_the_following_examples_of_advice_and_support_please_tell_us_how_important_it_is_for_you_or_the_person_you_care_for_that_it_continues_televised_covid_19_briefings_by_the_first_minister_available_to_everyone,
          FutureLocalInfo = for_each_of_the_following_examples_of_advice_and_support_please_tell_us_how_important_it_is_for_you_or_the_person_you_care_for_that_it_continues_information_about_covid_19_in_your_local_neighbourhood_available_to_everyone,
          FutureHRWebpage = for_each_of_the_following_examples_of_advice_and_support_please_tell_us_how_important_it_is_for_you_or_the_person_you_care_for_that_it_continues_a_webpage_especially_for_the_highest_risk_group_on_the_scottish_government_website,
          FutureChiefMedicalOfficerLetter = for_each_of_the_following_examples_of_advice_and_support_please_tell_us_how_important_it_is_for_you_or_the_person_you_care_for_that_it_continues_letters_from_the_chief_medical_officer_especially_for_the_highest_risk_group,
          FutureSMSShieldingUpdates = for_each_of_the_following_examples_of_advice_and_support_please_tell_us_how_important_it_is_for_you_or_the_person_you_care_for_that_it_continues_updates_to_your_phone_from_the_shielding_text_messaging_service,
          FutureCouncilSupport = for_each_of_the_following_examples_of_advice_and_support_please_tell_us_how_important_it_is_for_you_or_the_person_you_care_for_that_it_continues_support_from_your_local_council,
          FutureHRGroup = for_each_of_the_following_examples_of_advice_and_support_please_tell_us_how_important_it_is_for_you_or_the_person_you_care_for_that_it_continues_having_a_separate_highest_risk_group,
          # Age, gender and ethnicity
          AgeGroup = what_is_your_age_or_the_age_of_the_person_you_care_for,
          Gender = what_is_your_gender_or_the_gender_of_the_person_you_care_for,
          GenderOther = what_is_your_gender_or_the_gender_of_the_person_you_care_for_other,
          Ethnicity = what_is_your_ethnicity_or_the_ethnicity_of_the_person_you_care_for,
          EthnicityOther = what_is_your_ethnicity_or_the_ethnicity_of_the_person_you_care_for_other,
          # Disability
          VisualImpairment = do_you_or_the_person_you_care_for_have_any_of_the_following_visual_impairment,
          HearingImpairment = do_you_or_the_person_you_care_for_have_any_of_the_following_hearing_impairment,
          OtherSensoryImpairment = do_you_or_the_person_you_care_for_have_any_of_the_following_other_sensory_impairment,
          MobilityImpairment = do_you_or_the_person_you_care_for_have_any_of_the_following_mobility_impairment,
          OtherPhysicalImpairment = do_you_or_the_person_you_care_for_have_any_of_the_following_other_physical_impairment,
          LearningDifficulty = do_you_or_the_person_you_care_for_have_any_of_the_following_learning_difficulties,
          NoListedImpairment = do_you_or_the_person_you_care_for_have_any_of_the_following_none_of_the_above,
          #
          Carer = do_you_provide_unpaid_care_to_a_friend_neighbour_or_family_member_who_would_struggle_on_their_own,
          Vaccinated = have_you_received_your_covid_19_vaccination,
          HouseholdSize = including_yourself_how_many_people_live_in_your_household_please_write_your_answer_here,
          HouseholdChildren = how_many_people_in_your_household_are_children_under_16_please_write_your_answer_here,
          ScotlandRegion = where_in_scotland_do_you_live,
          # Outdoor space
          OutdoorPrivateGarden = what_outdoor_space_can_you_or_the_person_you_care_for_access_a_private_garden,
          OutdoorPrivateBalcony = what_outdoor_space_can_you_or_the_person_you_care_for_access_a_private_balcony,
          OutdoorPrivateOther = what_outdoor_space_can_you_or_the_person_you_care_for_access_another_private_outdoor_space,
          OutdoorSharedGarden = what_outdoor_space_can_you_or_the_person_you_care_for_access_a_shared_garden,
          OutdoorNone = what_outdoor_space_can_you_or_the_person_you_care_for_access_none,
          # Employment
          EmploymentEmployed = what_is_your_current_employment_situation_or_the_employment_situation_of_the_person_you_care_for_employed,
          EmploymentSelfEmployed = what_is_your_current_employment_situation_or_the_employment_situation_of_the_person_you_care_for_self_employed,
          EmploymentUnemployed = what_is_your_current_employment_situation_or_the_employment_situation_of_the_person_you_care_for_unemployed,
          EmploymentLookingAfterHomeOrFamily = what_is_your_current_employment_situation_or_the_employment_situation_of_the_person_you_care_for_looking_after_the_home_or_family,
          EmploymentNotWorkingDisabilityOrCondition = what_is_your_current_employment_situation_or_the_employment_situation_of_the_person_you_care_for_not_working_because_of_a_long_term_condition_or_disability,
          EmploymentEducation = what_is_your_current_employment_situation_or_the_employment_situation_of_the_person_you_care_for_in_education,
          EmploymentRetired = what_is_your_current_employment_situation_or_the_employment_situation_of_the_person_you_care_for_retired,
          EmploymentOther = what_is_your_current_employment_situation_or_the_employment_situation_of_the_person_you_care_for_other,
          #
          UnexpectedExpenseDifficulty = if_you_suddenly_had_to_find_100_to_meet_an_unexpected_expense_would_this_be,
          InternetAccess = do_you_or_the_person_you_care_for_have_access_to_the_internet_at_home
          ) 

## Aggregating columns

# Disambiguation of who the survey is for
survey %<>% mutate(SurveySubject = case_when(DisambigOwnOrNot == "My own situation" ~ "self",
                                             DisambigOwnOrNot == "The situation of the person I am caring for" ~ case_when(CareChildHighRisk == "Yes" ~ "child",
                                                                                                                           CareAdultHighRisk == "Yes" ~ "adult",
                                                                                                                           TRUE ~ "undetermined"),
                                             DisambigOwnChildOrAdult == "My own situation" ~ "self",
                                             DisambigOwnChildOrAdult == "The situation of the adult I am caring for" ~ "adult",
                                             DisambigOwnChildOrAdult == "The situation of the child I am caring for" ~ "child",
                                             DisambigChildOrAdult == "The adult I am caring for" ~ "adult",
                                             DisambigChildOrAdult == "The child I am caring for" ~ "child",
                                             IAmInHighRisk == "Yes" ~ "self",
                                             CareChildHighRisk == "Yes" ~ "child",
                                             CareAdultHighRisk == "Yes" ~ "adult"
                                            )
                  ) %>% 
  select(-c(DisambigOwnOrNot, DisambigChildOrAdult, DisambigOwnChildOrAdult, IAmInHighRisk, CareChildHighRisk, CareAdultHighRisk))


################### Weighting ################### 

# New columns for weighting
survey$GenderWeighting <- survey$Gender
survey$AgeGroupWeighting <- survey$AgeGroup

# Rename age groups
survey$AgeGroupWeighting[survey$AgeGroupWeighting %in% c("70-74", "75-79")] <- "70-79"

# Set NA gender and age to Not specified, for weighting purposes - GenderOther will retain info
survey$GenderWeighting[is.na(survey$GenderWeighting)] <- "Not specified"
survey$GenderWeighting[survey$GenderWeighting == "Other"] <- "Not specified"
survey$GenderWeighting[survey$GenderWeighting == "I prefer not to say"] <- "Not specified"
survey$AgeGroupWeighting[is.na(survey$AgeGroupWeighting)] <- "Not specified"

total_pop <- weight_dat[weight_dat$AgeGroup=="Not specified" & weight_dat$Gender =="Not specified",]$NumOfIndividuals
total_samp <- nrow(survey)

survey_pop_table <- table(survey %>% select(GenderWeighting, AgeGroupWeighting))
survey_pop_table_df <- as.data.frame(survey_pop_table)
num_specified <- sum(survey_pop_table_df[survey_pop_table_df$GenderWeighting != "Not specified" & survey_pop_table_df$AgeGroupWeighting != "Not specified",]$Freq)

get_weight <- function(agegroup, gender){
  
  num_pop <- weight_dat[weight_dat$AgeGroup == agegroup & weight_dat$Gender == gender,]$NumOfIndividuals
  
#  if(gender == "Not specified"){
#    if (agegroup == "Not specified"){
#    num_samp <- total_samp
#    } else {
#    num_samp <- nrow(survey[survey$AgeGroup == agegroup,])
#    }
#  } else if (agegroup == "Not specified"){
#    num_samp <- nrow(survey[survey$Gender == gender,])
#  } else {
#    num_samp <- nrow(survey[survey$AgeGroup == agegroup & survey$Gender == gender,])
#  }
  
  if(gender == "Not specified" | agegroup == "Not specified"){
    weight <- 1
  } else{
    num_samp <- survey_pop_table[gender, agegroup]
    frac_samp <- num_samp/total_samp
    frac_pop <- num_pop/total_pop
    weight <- frac_pop/frac_samp
  }
  
  #frac_pop <- num_pop/total_pop
  #frac_samp <- num_samp/total_samp
  #weight <- frac_pop/frac_samp
    
  return(weight)
}

get_rescaled_weight <- function(agegroup, gender){
  
  if(gender == "Not specified" | agegroup == "Not specified"){
    rescaled_weight <- NA
  } else{
  
  num_pop <- weight_dat[weight_dat$AgeGroup == agegroup & weight_dat$Gender == gender,]$NumOfIndividuals
  num_samp <- survey_pop_table[gender, agegroup]
  
  grossing_weight <- num_pop/num_samp
  
  # Only counting respondents who specified both age group and gender here
  total_responded <- num_specified #total_samp
  
  rescaled_weight <- (total_responded/total_pop) * grossing_weight
  }
  
  return(rescaled_weight)
}

survey %<>% mutate(Weight = map2(AgeGroupWeighting, GenderWeighting, get_weight)) %>% 
  mutate(Weight_cutoff = case_when(Weight < 0.3 ~ 0.3,
                                   Weight > 3 ~ 3)) %>% 
  mutate(RescaledWeight = map2(AgeGroupWeighting, GenderWeighting, get_rescaled_weight)) 

survey$RescaledWeight[is.na(survey$RescaledWeight)] <- mean(unlist(survey$RescaledWeight), na.rm = TRUE)


#survey %<>% transform(Weight_cutoff_2 = case_when(is.na(Weight_cutoff) ~ Weight)) 

# Print all weights

weight_printout <- survey %>% 
  select(GenderWeighting, AgeGroupWeighting, Weight, Weight_cutoff, RescaledWeight) %>% 
  distinct() %>% 
  arrange(factor(AgeGroupWeighting, 
                 levels = c("Under 16", "16-24", "25-44", "45-64", "65-69", "70-79", "80+", "Not specified"))) %>% 
  arrange(factor(GenderWeighting, 
                 levels = c("Male", "Female", "Not specified")))


wb2 <- loadWorkbook("calc_weights.xlsx", create = TRUE)

#creating sheets within an Excel workbook
createSheet(wb2, name = "Weights")

#writing into sheets within an Excel workbook : 
#writing ChickWeight data frame into chickSheet
writeWorksheet(wb2, weight_printout, sheet = "Weights", startRow = 1, startCol = 1)

#saving a workbook to an Excel file :
#saves a workbook to the corresponding Excel file and writes the file to disk.
saveWorkbook(wb2)



# Save cleaned workbook

xlcFreeMemory()

xlsx::write.xlsx(x = survey, file = "cleaned_survey.xlsx", 
              password = keyring::key_get("weighting_survey", Sys.info()[["user"]], keyring = "weighting"))

#wb_cleaned <- loadWorkbook("CleanedSurvey.xlsx", create = TRUE, 
#    password = keyring::key_get("weighting_survey", Sys.info()[["user"]], keyring = "weighting"))
#createSheet(wb_cleaned, name = "Survey")

#writeWorksheet(wb_cleaned, survey, sheet = "Survey", startRow = 1, startCol = 1)
#saveWorkbook(wb_cleaned)
xlcFreeMemory()

keyring::keyring_lock(keyring = "weighting")