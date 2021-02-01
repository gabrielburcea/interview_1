gender_levels <- c(
  "1" = "male", 
  "2" = "fem"
)  

fast_bld_sugar_levels <- c(
  "1" = "TRUE", 
  "0" = "fal"
)

exercise_ind_ang_levels <- c(
  "1" = "TRUE", 
  "0" = "fal"
)


healthy_sick_levels <- c(
  "healthy" = "buff"
)

chest_pain_type_levels <- c(
  "1" = "angina",
  "2" = "asympt",
  "3" = "notang",
  "4" = "abnang"
)

fasting_blood_sugar_levels <- c(
  "1" = "TRUE", 
  "0" = "fal"
)

electrocardiographic_results_levels <- c(
  "0" = "norm", 
  "1" = "abn", 
  "2" = "hyp"
)

slope_levels <- c(
  "1" = "up",
  "2" = "flat",
  "3" = "down" 
)

thals_levels <- c(
  "0" = "norm", 
  "1" = "fix", 
  "2" = "rev"
)

diagnosis_levels <- c(
  "0" = "H",        
  "1" = "S2",       
  "2" =  "S1",       
  "3" =  "S3",       
  "4" =  "S4" 
)

# number veseels colored has ? = NA
na_strings_numb_vessels_colored <- c(
  "?")

numerical_dt <- rename_vars_dt %>%
  dplyr::mutate(gender = forcats::fct_recode(sex, !!!gender_levels), 
                fasting_blood_sugar = forcats::fct_recode(fasting_bld_sugar, !!!fast_bld_sugar_levels), 
                chest_pain_type = forcats::fct_recode(chest_pain_type, !!!chest_pain_type_levels),
                exercise_induced_angina = forcats::fct_recode(exercise_ind_angina, !!!exercise_ind_ang_levels), 
                electrocardiographic_results = forcats::fct_recode(electrocardiographic_results, !!!electrocardiographic_results_levels), 
                slope = forcats::fct_recode(slope_peak_st_segment, !!!slope_levels), 
                thal = forcats::fct_recode(thal, !!!thals_levels), 
                healthy_sick = forcats::fct_recode(healthy_sick, !!!healthy_sick_levels), 
                diagnosis = forcats::fct_recode(diagnosis, !!!diagnosis_levels)
  )%>%
  dplyr::select(age, gender, chest_pain_type, resting_blood_pressure, cholesterol, fasting_blood_sugar, electrocardiographic_results, 
                maximum_heart_rate_achieved, exercise_induced_angina, st_depression_induced_exercise_rest, slope, thal, diagnosis, number_of_vessels_colored_by_flourosopy,
                healthy_sick)

numerical_dt$gender <- as.numeric(numerical_dt$gender)
numerical_dt$fasting_blood_sugar <- as.numeric(numerical_dt$fasting_blood_sugar)
numerical_dt$chest_pain_type <- as.numeric(numerical_dt$chest_pain_type)
numerical_dt$exercise_induced_angina <- as.numeric(numerical_dt$exercise_induced_angina)
numerical_dt$electrocardiographic_results <- as.numeric(numerical_dt$electrocardiographic_results)
numerical_dt$slope <- as.numeric(numerical_dt$slope)
numerical_dt$thal <- as.numeric(numerical_dt$thal)
numerical_dt$diagnosis <- as.numeric(numerical_dt$diagnosis)
numerical_dt$number_of_vessels_colored_by_flourosopy <- as.numeric(numerical_dt$number_of_vessels_colored_by_flourosopy)


#assigning ? with NA
numerical_dt <- numerical_dt %>%
  dplyr::mutate(across(starts_with('number_of_vessels'),
                       ~ replace(., . %in% na_strings_numb_vessels_colored, NA)))  

numerical_dt <- numerical_dt[complete.cases(numerical_dt),]

correlated_vars_plot <- correlation_num_dt %>%
  corrr::rplot(rdf = ., shape = 19, 
               colors = c("yellow", 
                          "purple")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  rplot_labs_v1

correlated_vars_plot

rplot_labs_v1 <- ggplot2::labs(
  title = "Correlations - Heart disease predictors")

rplot_labs_v1

removed_correlated_vars <- correlation_num_dt %>% corrr::shave(x =.)

# maximum heart rate - redundant variable
removed_correlated_vars %>%
  corrr::rplot(rdf = ., shape = 19, 
               colors = c("yellow", 
                          "purple")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  rplot_labs_v1

removed_correlated_vars