library(conflicted)
library(tidymodels)
library(ggrepel)
library(corrplot)
library(tidymodels)
library(dplyr)
library(corrr) 
library(themis)
library(rsample)
library(caret)

###Obs: clev.mod has the cleveland data - I have copied and paste it into excel. Then delimited the flat file into matrix
#read  data into csv format
clev_proc_data <- read_csv("cleveland_data_mine.csv")


# Renaming variables based on the Attribute information in the link: https://archive.ics.uci.edu/ml/datasets/Heart+Disease
# I feel more comfortable having clear variable names 

rename_vars_dt <- clev_proc_data %>% 
  rename(chest_pain_type = cp, 
         resting_blood_pressure = trestbps, 
         cholesterol = chol, 
         fasting_bld_sugar = fbs, 
         electrocardiographic_results = restecg,
         maximum_heart_rate_achieved = thalac, 
         exercise_ind_angina = exang, 
         st_depression_induced_exercise_rest = oldpeak, 
         slope_peak_st_segment = slope, 
         number_of_vessels_colored_by_flourosopy = ca, 
         healthy_sick = num)

# recoding age into age bands as the older the sicker; 
# want to keep age as age band within the model because if I will leave it on its own I am sure it does higly correlate with 
# heart disease. Yet, I am hoping to satisfy my curiosity as to whether this dataset shows something interesting with regards to age
age_recod_dt <- rename_vars_dt %>%
  dplyr::mutate(age_band = dplyr::case_when(
    age == 29 | age <= 29 ~ '0-19',
    age == 30 | age <= 39 ~ '20-39',
    age == 40 | age <= 49 ~ '40-49',
    age == 50 | age <= 59 ~ "50-59",
    age >= 60 ~ "60+"))

# as I had some issues with reading the file, I got some odd readings on several variables, thus wanting categories to appear as clear as possible
# because as I am running a variable importance within my algorithm and the data vizualisation should appear as intuitive as possible
gender_levels <- c(
  "female" = "fem"
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


# mapping the new categories into the vars 

final_dt <- age_recod_dt %>%
  dplyr::mutate(gender = forcats::fct_recode(sex, !!!gender_levels), 
                fasting_blood_sugar = forcats::fct_recode(fasting_bld_sugar, !!!fast_bld_sugar_levels), 
                exercise_induced_angina = forcats::fct_recode(exercise_ind_angina, !!!exercise_ind_ang_levels), 
                healthy_sick = forcats::fct_recode(healthy_sick, !!!healthy_sick_levels)) %>%
  dplyr::select(-maximum_heart_rate_achieved) # take out this variable as it is redundant - see plot number two as I applied a correlation 


# as I am trying to apply a supervise learning ml to get my associated variables with heart diesease, it is worth looking into the structure of each variable

final_dt %>% 
  dplyr::group_by(healthy_sick) %>%
  dplyr::tally() %>%
  dplyr::mutate(percent = n/sum(n)) # a difference of almost 10 % between sick and health - no need for balacing classes however I will do this 

# individually investigating several categorical variables 
# I may end up with skewed variables -> therefore need to apply k-fold cross validation => variance-biass tradeoff 
#age_band
final_dt %>%
  dplyr::group_by(age_band) %>%
  dplyr::tally() %>%
  dplyr::mutate(percent = n/sum(n)) # skew towards 50-59 group

#gender
final_dt %>%
  dplyr::group_by(gender) %>%
  dplyr::tally() %>%
  dplyr::mutate(percent = n/sum(n)) #more males than femals in cleveland data, difference of 31 % 


final_dt %>%
  dplyr::group_by(fasting_blood_sugar) %>%
  dplyr::tally() %>%
  dplyr::mutate(percent = n/sum(n)) # skewed towards false 


final_dt %>%
  dplyr::group_by(chest_pain_type) %>%
  dplyr::tally() %>%
  dplyr::mutate(percent = n/sum(n))

final_dt %>% 
  dplyr::group_by(slope_peak_st_segment) %>%
  dplyr::tally() %>%
  dplyr::mutate(percent = n/sum(n))


# get random forest classifier with the target variable healthy_sick 

final_dt %>% 
  dplyr::group_by(number_of_vessels_colored_by_flourosopy) %>%
  dplyr::tally() # I see a ? - may be a NA ? Probably - will assign it with NA

na_strings_numb_vessels_colored <- c(
  "?")

#assigning ? with NA
final_dt <- final_dt %>%
  dplyr::mutate(across(starts_with('number_of_vessels'),
                       ~ replace(., . %in% na_strings_numb_vessels_colored, NA)))  

# got the NA instead of ? 
final_dt %>%
  dplyr::group_by(number_of_vessels_colored_by_flourosopy) %>%
  dplyr::tally() 

# Check percentage of missing variables 
perc_miss_case <- sapply(final_dt, function(col) sum(is.na(col))/nrow(final_dt))
perc_miss_case # I have only 0.016 NA - for 1 var 
# I get rid of the rows with NA 


final_dt <- final_dt[complete.cases(final_dt),] # remained with 298 observations


# some variables appear as characters - need to transform them into factor 

final_dt$age_band  <- as.factor(final_dt$age_band)
final_dt$chest_pain_type  <- as.factor(final_dt$chest_pain_type)
final_dt$electrocardiographic_results  <- as.factor(final_dt$electrocardiographic_results)
final_dt$slope_peak_st_segment  <- as.factor(final_dt$slope_peak_st_segment)
final_dt$number_of_vessels_colored_by_flourosopy  <- as.factor(final_dt$number_of_vessels_colored_by_flourosopy)
final_dt$thal  <- as.factor(final_dt$thal)
final_dt$diagnosis  <- as.factor(final_dt$diagnosis)
#### Apply the ML 

# Obtain different perfomances measures, two wrapper functions
# For Accuracy, Kappa, the area under the ROC curve, 
# sensitivity and specificity
library(caret)
library(pROC)

newRF <- caretFuncs

fiveStats <- function (...)c(twoClassSummary(...),
                             defaultSummary(...))

newRF$summary <- fiveStats

# Everything but the area under the ROC curve - 
# it is not needed to look so much into predictions but want to satisfy my curiosity with regards to the output I may get 
fourStats <- function(data, lev=levels(data$obs), model =NULL){
  
  accKapp <- postResample(data[, "pred"], data[, "obs"])
  out<- c(accKapp,
          sensitivity(data[,"pred"], data[,"obs"], lev[1]),
          specificity(data[,"pred"], data[,"obs"], lev[2]))
  names(out)[3:4] <- c("Sens", "Spec")
  out
}

#create a table
table_perf = data.frame(model=character(0),
                        auc=numeric(0),
                        accuracy=numeric(0),
                        sensitivity=numeric(0),
                        specificity=numeric(0),
                        kappa=numeric(0),
                        stringsAsFactors = FALSE)

# get smote to balance the classes - but this will be applied within the k-fold cross validation to avoid bias
smote <- list(name = "SMOTE with more neighbors!",
                func = function (x, y) {
                  115
                  library(DMwR)
                  dat <- if (is.data.frame(x)) x else as.data.frame(x)
                  dat$.y <- y
                  dat <- SMOTE(.y ~ ., data = dat, k = 3, perc.over = 100, perc.under =
                                 200)
                  list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)],
                       y = dat$.y) },
                first = TRUE)

# get the 10 fold-cross validation with smote  passedinto sampling 
ctrlInside <- trainControl(method = "repeatedcv", 
                           number = 10,
                           repeats = 5,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           savePredictions = TRUE, 
                           search = "grid",
                           sampling = smote)

ctrl <- rfeControl(method = "repeatedcv", 
                   repeats = 5,
                   verbose = TRUE,
                   functions = rfFuncs) 

#create training dataset and test dataset 

train <- createDataPartition(final_dt$healthy_sick, p = 0.67, list = FALSE)
data_train <- final_dt[train,]
data_test <- final_dt[-train,]
# do the knn Imputation for missing variables only on training dataset 
library(mlbench)

random_forest <- caret::train(
  healthy_sick ~.,
  data = data_train, 
  method = "rf",
  trControl = ctrlInside,
  preProc = c("center", "scale", "nzv"),
  metric = "ROC")
  
print(random_forest)
predictors(random_forest)
varImp(random_forest)
plot(random_forest, type=c("g", "o"))  

rfe_random_forest <- caret::rfe(
  x = data_train[,1:14],
  y = data_train[,15], 
  method = "rf",
  trControl = ctrlInside,
  rfeControl = ctrl,
  metric = "ROC")

print(random_forest)
predictors(random_forest)
varImp(random_forest)
plot(random_forest, type=c("g", "o"))  

  