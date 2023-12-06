##### INSTALLATION #####
# install packages using pacman
if(!require(pacman)) install.packages('pacman', repos = 'http://cran.us.r-project.org')
pacman::p_load(pacman, # Package Management Tool
               tidyverse, # Easily Install and Load the 'Tidyverse'
               magrittr, # A Forward-Pipe Operator for R
               batman, # Convert Categorical Representations of Logicals to Actual Logicals
               stringi, # Character String Processing Facilities; addition to stringr
               rio, # A Swiss-Army Knife for Data I/O; imports readxl & openxlsx
               tidylog, # Logging for 'dplyr' and 'tidyr' Functions; See what is happening 
               janitor, # Simple Tools for Examining and Cleaning Dirty Data; clean column names
               lubridate, # Make Dealing with Dates a Little Easier
               caret, # Classification and Regression train
               here, # A Simpler Way to Find Your Files
               pdftools, # Text Extraction, Rendering and Converting of PDF Documents
               matrixStats, # Functions that Apply to Rows and Columns of Matrices (and to Vectors)
               Rborist, # Extensible, Parallelizable Implementation of the Random Forest Algorithm
               randomForest, # Classification and regression based on a forest of trees
               dslabs, # Data Science Labs
               tinytex, # Install, Maintain and Compile LaTeX Documents
               jsonlite, # only necessary if data is imported from datahub.io
               corrplot, # Visualization of a Correlation Matrix
               Hmisc, # Harrell Miscellaneous
               plotROC, # Generate Useful ROC Curve Charts for Print and Interactive Use
               caTools, # Moving Window Statistics, GIF, Base64, ROC AUC, etc
               pROC, # Display and Analyze ROC Curves
               mice, # Multivariate Imputation by Chained Equations
               xgboost, # Extreme Gradient Boosting
               klaR, # Classification and Visualization
               rpart.plot # Plot 'rpart' Models: An Enhanced Version of 'plot.rpart'
)

# define harmonic average, because we have a classification problem; F_meas function of caret package
# get params from confusion matrix
confusionMatrix_params <- function(data, reference) {# confusionMatrix(data = predicted_outcomes, reference = diabetes_validation$outcome)
  f_meas <- F_meas(data = data, reference = reference)
  confusion_matrix <- confusionMatrix(data = data, reference = reference)
  accuracy <- confusion_matrix$overall['Accuracy']
  kappa <- confusion_matrix$overall['Kappa']
  other_params <- confusion_matrix$byClass[c("Sensitivity","Specificity", "Pos Pred Value")]
  params <- tibble(f_value = f_meas,
                   accuracy = accuracy,
                   kappa = kappa,
                   sensitivity = other_params[1],
                   specificity = other_params[2],
                   precision = other_params[3]
  )
  return(params)
}

##### SETUP #####
# change general options
options(scipen = 10000, digits = 5, timeout = 120)

# save / load data from last session
# here::here()
# save.image('cyo_capstone_project.RData')
# load('cyo_capstone_project.RData')


##### IMPORT AND TRANSFORM #####
# import csv file
diabetes_df_raw <- rio::import(here('diabetes.csv'))

### Alternative: create data sets
# # download data from datahub using jsonlite package
# json_file <- 'https://datahub.io/machine-learning/diabetes/datapackage.json'
# json_data <- fromJSON(paste(readLines(json_file), collapse=""))
# 
# # get list of all resources
# print(json_data$resources$name)
# 
# # download data from csv file
# path_to_file <- str_subset(json_data$resources$path, 'diabetes_csv.csv')
# diabetes_df <- read_csv(path_to_file)
# 
# rm(path_to_file, json_file, json_data)
# 
# # rename variables
# diabetes_df <- diabetes_df %>% 
#   rename(Pregnancies = preg,
#          Glucose = plas,
#          BloodPressure = pres,
#          SkinThickness = skin,
#          Insulin = insu,
#          BMI = mass,
#          DiabetesPedigreeFunction = pedi,
#          Age = age,
#          Outcome = class)


##### EXPLORATORY DATA ANALYSIS AND WRANGLING ##### 
### Exploratory Data Analysis
# structure
glimpse(diabetes_df_raw)

# summary
summary(diabetes_df_raw)


##### DATA TIDYING & EDA #####
# tidy column names, outcome not as factor of numeric data (problems in model_building)
diabetes_df_clean <- diabetes_df_raw %>% 
  janitor::clean_names() %>% 
  mutate(# id = 1:nrow(diabetes_df),
         outcome = if_else(outcome == '0', 'no', 'yes')) %>%
  mutate(outcome = factor(outcome)) %>% 
  rename(diabetes_p_fct = diabetes_pedigree_function) %>%
  mutate(across(where(is.integer), as.double))
# levels(diabetes_df$outcome) <- c('No', 'Yes') # we want to keep outcome variable as 'numeric'

# check how many zero we have in each column
diabetes_df_clean %>%
  mutate_if(is.numeric, ~(. == 0)) %>%
  dplyr::select(-outcome) %>%
  colSums() %>% 
  print()

# percentage of NAs in insulin variable
na_percentage <- 374/nrow(diabetes_df_clean)*100

# plot all variables for diabetes_data_clean
diabetes_df_clean %>% 
  pivot_longer(cols = -outcome, names_to = 'param') %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = 'black') +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
  #       strip.text = element_text(size=8)) + #angle = 45, vjust = 1, hjust = 1
  facet_wrap(~param, scales = 'free')

# # calculate group medians for all relevant numeric variables to replace zero values
# group_medians <- diabetes_df_clean %>% 
#   dplyr::select(-pregnancies, -age, -diabetes_p_fct) %>% 
#   mutate(across(where(is.numeric), ~ na_if(., 0))) %>% 
#   group_by(outcome) %>% 
#   summarise(across(where(is.numeric), .f = list(median = median), na.rm = TRUE)) %>% 
#   rename_with(., ~ str_remove_all(., '_median'))

# which columns to modify? -> blood_pressure, bmi, glucose, insulin, skin_thickness
variables_to_adjust <- c('blood_pressure', 'bmi', 'glucose', 'insulin', 'skin_thickness')

# replace zero values in relevant variables with NA
diabetes_df <- diabetes_df_clean
for (i in seq_along(variables_to_adjust)) {
  diabetes_df[, variables_to_adjust[i]][diabetes_df[, variables_to_adjust[i]] == 0] <- NA
}

# replace NA with model data generated by mice package
mice_mod <- mice(diabetes_df[, variables_to_adjust], method = 'rf', seed = 1234,
                    printFlag = FALSE)
diabetes_df[, variables_to_adjust] <- complete(mice_mod)

### alternative: mice package
# # replace zero values w/ group specific median
# for (i in seq_along(variables_to_adjust)) {
#   diabetes_df[, variables_to_adjust[i]][diabetes_df[, variables_to_adjust[i]] == 0 & diabetes_df$outcome == 0] <- 
#     as.numeric(group_medians[group_medians$outcome == 0, variables_to_adjust[i]])
#   diabetes_df[, variables_to_adjust[i]][diabetes_df[, variables_to_adjust[i]] == 0 & diabetes_df$outcome == 1] <- 
#     as.numeric(group_medians[group_medians$outcome == 1, variables_to_adjust[i]])
# }

# plot all variables for diabetes_data with modified dataset
diabetes_df %>% 
  pivot_longer(cols = -outcome, names_to = 'param') %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = 'black') +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
  #       strip.text = element_text(size=8)) + #angle = 45, vjust = 1, hjust = 1
  facet_wrap(~param, scales = 'free')

# density plots for each param depending on the outcome
diabetes_df %>% 
  pivot_longer(cols = -outcome,
               names_to = 'variable',
               values_to = 'value') %>%
  ggplot(aes(value)) +
  geom_density(aes(fill = outcome), alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        strip.text = element_text(size=8)) + #angle = 45, vjust = 1, hjust = 1
  facet_wrap(~variable, scales = 'free', nrow = 2)

##### SPLIT DATA #####
# split data into train, validation and test sets
# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind='Rounding') # if using R 3.6 or later
test_index <- createDataPartition(y = diabetes_df$outcome, times = 1, p = 0.2, list = FALSE)
diabetes_train <- diabetes_df[-test_index,]
temp <- diabetes_df[test_index,]

# split temp into validation and test sets
test_index <- createDataPartition(y = temp$outcome, times = 1, p = 0.5, list = FALSE)
diabetes_validation <- temp[-test_index,]
diabetes_test <- temp[test_index,]

# rm unnecessary data
rm(test_index, temp)

# inspect train set
table(diabetes_train$outcome)

##### EXPLORATORY DATA ANALYSIS #####
# create matrix of diabetes_train
corr_df <- diabetes_train %>% 
  mutate(outcome = as.factor(ifelse(outcome == 'no', '0', '1')))

# analyze the correlation (Pearson's), include p-values; use Hmisc package
res <- rcorr(as.matrix(corr_df))

# plot the correlation of variables
corrplot(res$r, type = "upper", tl.cex = .7, order = 'FPC',
         tl.col = "black")


##### MODELLING #####
# define fit for control function
fit_control <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)

### Model 1: Guessing (Weighted Guess Classifier)
# percentage of positive diagnoses
ratio <- diabetes_train$outcome %>% 
  as.character() %>% 
  str_replace_all(., c('no'='0','yes'='1')) %>% 
  as.numeric() %>% 
  mean()

# build a model
set.seed(1, sample.kind = 'Rounding')
predicted_outcomes <- diabetes_validation %>% 
  mutate(guess = sample(c(0, 1), 
                        size = n(), 
                        replace = TRUE, 
                        prob = c((1-ratio), ratio))) %>% 
  mutate(guess = as.factor(str_replace_all(guess, c('0'='no','1'='yes')))) %>%
  pull(guess)

# evaluation metrics
params_model_1 <- confusionMatrix_params(data = predicted_outcomes, 
                                         reference = diabetes_validation$outcome)


### Model 2: Logistic Regression (generalized linear model)
# build a model based on all variables
# set the seed
set.seed(1, sample.kind = 'Rounding')

# do linear model with caret package glm
model_2 <- caret::train(outcome ~ ., 
                        method = 'glm',
                        family = 'binomial',
                        metric = 'ROC',
                        tuneLength=10,
                        preProcess = c('center', 'scale'),
                        trControl = fit_control,
                        data = diabetes_train)

# predict outcomes using model 2
predicted_outcomes <- predict(model_2, diabetes_validation)

# evaluation metrics
params_model_2 <- confusionMatrix_params(data = predicted_outcomes, 
                                         reference = diabetes_validation$outcome)

# probabilities of predicted outcomes using model 2
predicted_outcomes_prob <- predict(model_2, diabetes_validation, type = 'prob') 

# # plot roc
# roc_glm <- roc(diabetes_validation$outcome, predicted_outcomes)
# colAUC(predicted_outcomes_prob$`1`, diabetes_validation$outcome, plotROC = TRUE)


### Model 3: random forest
# set the seed
set.seed(1, sample.kind = 'Rounding')

# train the model
model_3 <- train(outcome ~ .,
                diabetes_train,
                method = 'rf',
                tuneLength = 2,
                trControl = fit_control)

# predict outcomes using model 3
predicted_outcomes <- predict(model_3, diabetes_validation)

# evaluation metrics
params_model_3 <- confusionMatrix_params(data = predicted_outcomes, 
                                         reference = diabetes_validation$outcome)

### Model 4: Fitting XGBoost
# create tuning grid
xgb_grid = expand.grid(
  nrounds = 50,
  eta = c(0.03),
  max_depth = 1,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 1,
  subsample = 0.5
)

# create model
set.seed(1, sample.kind = 'Rounding')
model_4 <- train(outcome ~ .,
                 diabetes_train,
                 method = 'xgbTree',
                 metric = 'ROC',
                 tuneGrid = xgb_grid,
                 trControl = fit_control)

# predict outcomes using model 4
predicted_outcomes <- predict(model_4, diabetes_validation)

# evaluation metrics
params_model_4 <- confusionMatrix_params(data = predicted_outcomes, 
                                         reference = diabetes_validation$outcome)


# # https://www.kaggle.com/code/pelkoja/visual-xgboost-tuning-with-caret/report
# # https://ml-course.kazsakamoto.com/Labs/hyperparameterTuning.html

### Model 5: K-nearest neighbors
# tune k, perform cross-validation
set.seed(3, sample.kind = 'Rounding')
model_5 <- train(outcome ~ .,
                 diabetes_train,
                 method = 'knn',
                 metric = 'ROC',
                 trControl = fit_control,
                 tuneGrid = expand.grid(k = seq(1, 101, 2))
)

# predict outcomes using model 5
predicted_outcomes <- predict(model_5, diabetes_validation)

# evaluation metrics
params_model_5 <- confusionMatrix_params(data = predicted_outcomes, 
                                         reference = diabetes_validation$outcome)


### Model 6: Naive Bayes
# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
set.seed(3, sample.kind = 'Rounding')
model_6 <- train(outcome ~ .,
                 diabetes_train,
                 method = 'nb',
                 trControl = fit_control,
                 tuneGrid = search_grid,
                 preProc = c("BoxCox", "center", "scale", "pca"))

# predict outcomes using model 6
predicted_outcomes <- predict(model_6, diabetes_validation)

# evaluation metrics
params_model_6 <- confusionMatrix_params(data = predicted_outcomes, 
                                         reference = diabetes_validation$outcome)


### Model 7: Classification Tree
# train model
set.seed(3, sample.kind = 'Rounding')
model_7 <- train(outcome ~ .,
                 diabetes_train,
                 method = 'rpart',
                 metric = 'ROC',
                 tuneLength = 20,
                 trControl = fit_control)

# predict outcomes using model 7
predicted_outcomes <- predict(model_7, diabetes_validation)

# evaluation metrics
params_model_7 <- confusionMatrix_params(data = predicted_outcomes, 
                                         reference = diabetes_validation$outcome)


##### RESULTS AND DISCUSSION #####
# plot model_5_k_tuning to find ideal k
plot(model_5)
model_5$bestTune
model_5$finalModel

# plot
rpart.plot(model_7$finalModel, type = 4, extra = 2,
           cex = .45, fallen.leaves = TRUE)

# bind evaluation params
eval_params <- rbind(params_model_1, 
                     params_model_2,
                     params_model_3,
                     params_model_4,
                     params_model_5,
                     params_model_6,
                     params_model_7
) %>% 
  mutate(`Model No.` = c(1:7),
         `Model name` = c('Guess','glm','RF','XGBoost','kNN', 'NB', 'rpart'),
         .before = f_value) 
# --> kNN best model based on F value


### do it for the final test set
# merge diabetes_train and diabetes_validation
diabetes_df_final <- full_join(
  diabetes_train, diabetes_validation
)

# train the final model, therefore tune k, perform cross-validation
set.seed(2, sample.kind = 'Rounding')
model_final <- train(outcome ~ .,
                     diabetes_df_final,
                     method = 'knn',
                     metric = 'ROC',
                     trControl = fit_control,
                     tuneGrid = expand.grid(k = seq(1, 101, 2))
)

# predict outcomes for diabetes_test using model_final
predicted_outcomes <- predict(model_final, diabetes_test)

# evaluation metrics
params_model_final <- confusionMatrix_params(data = predicted_outcomes, 
                                             reference = diabetes_test$outcome)

# generate confusion matrix
cm <- confusionMatrix(data = predicted_outcomes,
                      reference = diabetes_test$outcome)

# plot confusion matrix
cm$table # %>% 
  # data.frame()

# plot the results for the final test
print(params_model_final)

##### EXPORT #####
# save all data using a relative path
save.image(here('cyo_capstone_project.RData'))
