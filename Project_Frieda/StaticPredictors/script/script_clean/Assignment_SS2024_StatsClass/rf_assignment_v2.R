rm(list = ls())

library(caret)
library(caretEnsemble)
library(dplyr)
library(ranger)

# Load Workspace
load("Assignment_ws_rfFuncs.RData")

# Preprocess the data
dat1_v3 <- dat1_v2 %>%
  na.omit() %>%
  select(-Telfer_1_2, -verbatim_name)

# Create training (80%) and testing (20%) sets (5 resamples)
set.seed(42)
train_index <- createDataPartition(
  dat1_v3$log_R2_1,
  p = 0.8,
  list = TRUE,
  times = 5
)

# Set seeds for reproducibility
my_seeds <- vector(mode = "list", length = 31)
for (i in 1:30) my_seeds[[i]] <- sample.int(1000, 7)
my_seeds[[31]] <- 42  # Last model

# Define training control
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  savePredictions = "final",
  index = train_index  # For 5 resamples of data splitting
  #seeds = my_seeds    # This leads to errors right now
)

train_control_fast <- trainControl(
  method = "adaptive_cv",
  number = 10,
  repeats = 5,
  adaptive = list(min = 3, # minimum number of resamples before elimination is possible
                  alpha = 0.05, # confidence level used to eliminate hyperparameter combos
                  method = "gls",
                  complete = TRUE))
                  # tells caret to run all the resamples for the best parameter combo

# Define the list of models
model_list <- caretList(
  log_R2_1 ~ .,
  data = dat1_v3,
  trControl = train_control,
  #continue_on_fail = TRUE,
  metric = "RMSE",
  tuneList = list(
    rf = caretModelSpec(
      method = "ranger",
      importance = "permutation",
      local.importance = TRUE,
      oob.error = TRUE,
      respect.unordered.factors = TRUE,
      num.trees = 1000,
      preProcess = c('corr'),
      tuneGrid = expand.grid(
        mtry = seq(from = 1, to = 72, by = 5),
        splitrule = "extratrees",
        min.node.size = 5)),
    gbm = caretModelSpec(
      method = "gbm",
      distribution = "gaussian",
      keep.data = TRUE,
      preProcess = c('corr'),
      tuneGrid = expand.grid(
        n.trees = 1000,
        interaction.depth = 1:5,
        shrinkage = seq(from = 0.001, to = 0.1, by = 0.005),
        n.minobsinnode = 1:5)),
    xgboost = caretModelSpec(
      method = "xgbTree",
      preProcess = c('corr'),
      verbose = 1,
      tuneGrid = expand.grid(
        nrounds = 1000,
        eta = c(0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
        max_depth = c(3:6),
        gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
        colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
        min_child_weight = 1:3,
        subsample = 0.8
        )
  )
))
