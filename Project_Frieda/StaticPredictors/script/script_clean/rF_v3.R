## Standardized Workflow for randomForest regression models ##
### 29.04.2024
### Friederike Wölke
rm(list=ls())

# Libraries
pacman::p_load(dplyr, randomForestSRC, ggplot2, tidyr, viridis, ggRandomForests, randomForestExplainer)



# Data
### read raw model df with NAs
dat <- readRDS("out/rds/Final_data.rds") 

dat2 <- impute(data = dat, fast = T)

## We still have some redundand columns in the data. Let's exclude them:
exclude_vars <- c("m_AOO_a",  "b_AOO_a",
                  "Total_area", "Total_Ncells", "Total_area_samp", "mean_area",
                  "rel_occ_Ncells", "mean_relAOO", "occ_Ncells", "mean_prob_cooccur",
                  "Gamma",  "GammaSR",
                  "Range.Size", "GlobRangeSize_m2", 
                  "mean_cell_length", "increment2", "area",
                  "RL_Category", 
                  "atlas_xmax")

dat2 <- dat %>% select(!all_of(c(exclude_vars)))
str(dat2)











# Visual plot of relationship between responses (are they good indicators of each other?)
### This shows that logR seems to predict more positive change than Telfer, especially for CZ and EU (points in upper left quadrat but none in lower right)
ggplot(dat2, aes(y = log_R2_1, x = Telfer_1_2, col = factor(dataset)))+
  geom_point(alpha = 0.1)+
  geom_smooth(method = "lm")+
  geom_vline(xintercept=0, col = "red")+
  geom_hline(yintercept=0, col = "red")+
  scale_color_viridis(discrete = T)+
  theme_classic()












caret::nearZeroVar(dat2, names = T) # IUCN


# Split data for response (main & sensitivity):
dat_lR <- dat2 %>% select(-Telfer_1_2)
dat_T <- dat2 %>% select(-log_R2_1)

# Split data for modelling future & past change
dat_lR1 <- dat_lR %>% filter(tp == 1) %>% select(-tp, -verbatim_name)
dat_lR2 <- dat_lR %>% filter(tp == 2)%>% select(-tp, -verbatim_name)

dat_T1 <- dat_T %>% filter(tp == 1)%>% select(-tp, -verbatim_name)
dat_T2 <- dat_T %>% filter(tp == 2)%>% select(-tp, -verbatim_name)


# p = 52 (-verbatim_name)
# n = 1051 obs.

## 1. impute missing data:
# colSums(is.na(dat_lR1))
# dat_lR1_imp <- impute(log_R2_1 ~., data = dat_lR1, ntree=1000, nimpute = 20)


### Test for 1 model ####

model_df <- dat_lR1

## 2 Train/Test split: 80/20
set.seed(42)
samp <- sample(nrow(model_df), 0.8 * nrow(model_df))
train <- model_df[samp, ]; dim(train) 
test <- model_df[-samp, ]; dim(test) 

## 3. tune parameters:
param_tuned <- tune(log_R2_1~., train, ntrees = 1000)
mtry = param_tuned$optimal[2]
ns = param_tuned$optimal[1]

## 4. Initial model:
m0 <- rfsrc(log_R2_1~., train, ntrees = 1000, mtry = mtry, nodesize = ns)

# select important variables:
selected_vars3 <- var.select(log_R2_1 ~., train, ntrees = 1000, mtry = mtry, nodesize = ns,
                             method="md")
length(selected_vars3$topvars)
topvars <- selected_vars3$topvars


### Quick var selection:

## run shallow trees to find variables that split any tree
xvar.used <- rfsrc(log_R2_1 ~., train, ntree = 1000, nodedepth = 4,
                   var.used="all.trees", mtry = Inf, nsplit = 100)$var.used

## now fit forest using filtered variables
xvar.keep  <- names(xvar.used)[xvar.used >= 1]
o <- rfsrc(log_R2_1~., train[, c("log_R2_1", xvar.keep)])
print(o)


### Test: Rerun model only with selected topvars
train2 <-  train%>% select(all_of(topvars), log_R2_1)
param_tuned <- tune(log_R2_1 ~ ., train2, ntrees = 1000)
mtry = param_tuned$optimal[2]
ns = param_tuned$optimal[1]
m1 <- rfsrc(log_R2_1 ~., train2, ntrees = 1000, mtry = mtry, nodesize = ns)
m1

# find interactions
m1_int1 <- find.interaction.rfsrc(m1)
m1_int2 <- find.interaction.rfsrc(m1, method = "vimp", nrep = 3)
m1_int2 <- m1_int2 %>% as.data.frame() %>% round(3)
m1_int2$interaction <- rownames(m1_int2)

## Interactions:
int_dat <- gg_interaction(m1_int1) 
plot(int_dat)
### Plot model:
gg_dta<- gg_rfsrc(m1)
plot(gg_dta)+
  geom_hline(yintercept = 0)








## Preprocessing: imputing missing values, tune parameters
selected_vars1 <- var.select(log_R2_1 ~., data = dat_lR1,
                            method="vh.vimp")
vhvimp_top <- selected_vars1$topvars
selected_vars2 <- var.select(log_R2_1 ~., data = dat_lR1,
                             method="vh")
selected_vars3 <- var.select(log_R2_1 ~., data = dat_lR1,
                             method="md")
md_top <- selected_vars3$topvars[1:10]


# Some Variable Relationships:

par(mfrow=c(2,5))
ggplot(data = dat_lR1, aes(y = log_R2_1,x =log(Mass), col = dataset))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, col = "red")
ggplot(data = dat_lR1, aes(y = log_R2_1,x = D_AOO_a, col = dataset))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, col = "red")
ggplot(data = dat_lR1, aes(y = log_R2_1,x = AlphaSR_sp, col = dataset))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, col = "red")
ggplot(data = dat_lR1, aes(y = log_R2_1,x = Southernness, col = dataset))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, col = "red")
ggplot(data = dat_lR1, aes(y = log_R2_1,x = IUCN, col = dataset))+
  geom_point()+
  geom_hline(yintercept=0, col = "red")
ggplot(data = dat_lR1, aes(y = log_R2_1,x = HWI, col = dataset))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, col = "red")
ggplot(data = dat_lR1, aes(y = log_R2_1,x = rel_AOO, col = dataset))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, col = "red")
ggplot(data = dat_lR1, aes(y = log_R2_1,x = circ, col = dataset, col = dataset))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, col = "red")
ggplot(data = dat_lR1, aes(y = log_R2_1,x = moran, col = dataset))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, col = "red")
ggplot(data = dat_lR1, aes(y = log_R2_1,x = AOO, col = dataset))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=0, col = "red")



### Modelling ###

# Steps:

# I. BAGGING: 
# a) bootstrap the data set  
## same size as original dataset
## randomly select rows from dataset (with replication: same sample more than once --> this leads to some sample being left out)
## i.e., in practice: shuffle the rows of the dataset so that they are random, but if replacement = T, some rows may be selected multiple times while  some of them are not selected at all (->out of bag sample!)

# b) create the decision tree from bootstrap data using a random selection of columns at each step (step = each node of the tree)
## mtry - can be determined optimally using hyperparameter tuning

# c). repeat 1-2 ~ 1000 times --> produces the forest that can be averaged
## ntrees = 1000

# II. How to use the forest:
## - get new data
## - predict the new data through all of the trees and average results


# III. How to determine if the model is good?
## - Use the OOB sample (those that have not been bootstrapped) and run it through the forest. 
## - Predict for each OOB sample for all trees
## - Measure how accurate the model predicted the OOB samples by the proportion of OOB error (i.e., the proportion of falsely classified samples)


# IIII. Hyperparameter tuning:
## - repeat I-III. for different settings of the hyperparameters:
## - mtry, ntrees, min node size
## - chose the one that is the most accurate (lowest error)
## - typically start by using n_var^2 and then trying a few settings above and below that value





###########


## Trying something else: I'll model only one atlas 

CZ_model_df1 <- dat2 %>% filter(dataset == "Birds_Atlas_Czechia" & tp == 1) %>% select(-Telfer_1_2, -verbatim_name)
CZ_model_df2 <- dat2 %>% filter(dataset == "Birds_Atlas_Czechia" & tp == 2) %>% select(-Telfer_1_2, -verbatim_name)

EBBA_model_df1 <- dat2 %>% filter(dataset == "Birds_atlas_EBBA" & tp == 1) %>% select(-Telfer_1_2, -verbatim_name)
EBBA_model_df2 <- dat2 %>% filter(dataset == "Birds_atlas_EBBA" & tp == 2) %>% select(-Telfer_1_2, -verbatim_name)

dfs_list <- list(CZ_model_df1, CZ_model_df2, EBBA_model_df1, EBBA_model_df2)


topvars_list <- list()
models_list <- list()
for (dd in seq_along(dfs_list)){
  
  ##
  model_df <- dfs_list[[dd]]
  
  ## 2 Train/Test split: 80/20
  set.seed(42)
  samp <- sample(nrow(model_df), 0.8 * nrow(model_df))
  train <- model_df[samp, ]; dim(train) 
  test <- model_df[-samp, ]; dim(test) 
  
  ## 3. tune parameters:
  param_tuned <- tune(log_R2_1~., train, ntrees = 1000)
  mtry = param_tuned$optimal[2]
  ns = param_tuned$optimal[1]
  
  ## 4. Initial model:
  m0 <- rfsrc(log_R2_1~., train, ntrees = 1000, mtry = mtry, nodesize = ns)
  
  # select important variables:
  selected_vars3 <- var.select(log_R2_1 ~., train, ntrees = 1000, mtry = mtry, nodesize = ns,
                               method="md")
  length(selected_vars3$topvars)
  topvars <- selected_vars3$topvars
  
  topvars_list[[dd]] <- topvars
  
  # Reduced models:
  model_df2 <- dfs_list[[dd]] %>% select(all_of(topvars), log_R2_1)
  
  ## 2 Train/Test split: 80/20
  set.seed(42)
  samp2 <- sample(nrow(model_df2), 0.8 * nrow(model_df2))
  train2 <- model_df2[samp2, ]; dim(train2) 
  test2 <- model_df2[-samp2, ]; dim(test2) 
  
  ## 3. tune parameters:
  param_tuned <- tune(log_R2_1~., data=train2)
  mtry = param_tuned$optimal[2]
  ns = param_tuned$optimal[1]
  
  m1 <- rfsrc(log_R2_1~., train2, ntrees = 1000, mtry = mtry, nodesize = ns)
  
  models_list[[dd]] <- list(m0, m1)
  
}

names(models_list) <- c("CZ1", "CZ2", "EBBA1", "EBBA2")




########





library(randomForest)
forest <- randomForest(log_R2_1 ~ ., train, ntrees = 1000, localImp = T, na.action = "na.omit" )
importance_frame <- measure_importance(forest)
(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))
interactions_frame <- min_depth_interactions(forest, vars)
plot_min_depth_interactions(interactions_frame)
