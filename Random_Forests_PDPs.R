getRversion()
packageVersion("caret")

set.seed(321)

# ordinal rf seed should be a list of length 26 with 25 integer vectors of 
# size 33 and the last list element having at least a single integer

seeds_ord <- vector(mode = "list", length = 26) # 5 fold * 5 repeats + final fit

for(i in 1:25) seeds_ord[[i]]<- sample.int(n=1000, 33) # 33 tuning models

seeds_ord[[26]]<-sample.int(1000, 1) # for the last model (all data)

# binary rf seed should be a list of length 26 with 25 integer vectors of 
# size 19 and the last list element having at least a single integer

seeds_bin <- vector(mode = "list", length = 26)

for(i in 1:25) seeds_bin[[i]]<- sample.int(n=1000, 19)  # for 19 comparisons

##### NOTE: single set of '[]' (to store the final seed as an integer) ensures 
##### full reproducibility of final binary RF model. However, some versions of R
##### may not recognize as a valid seed
#seeds_bin[[26]]<-sample.int(1000, 1) # for R v4.5.1 / caret vX.X.X
seeds_bin[26]<-sample.int(1000, 1) # for R v4.4.3 / caret v7.0.1

setwd(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Input_datasets\Tabular_data_by_species)")

library(dplyr)
library(tidyr)
library(data.table)
library(party)
library(randomForest)
require(caret)
library(MLmetrics)
library(pdp)
library(parallel)
library(doParallel)
source("http://www.ibe.med.uni-muenchen.de/organisation/mitarbeiter/070_drittmittel/janitza/rf_ordinal/novel_vims.txt")
library(extrafont)
#loadfonts(device = "win")
library(forcats)
library(ggplot2)

# set up parallel processing
no_cores <- 7
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)


#load("rfdata.RData")

# check distribution of classes

rfdata %>%
  count(category) %>%
  mutate(proportion = n / sum(n))

rfdata %>%
  count(bin_category) %>%
  mutate(proportion = n / sum(n))

# leave out a test dataset
train_index <- createDataPartition(rfdata$num_category, p = 0.8, list = FALSE)

train_data <- rfdata[ train_index, ]
test_data  <- rfdata[-train_index, ]

train_data %>%
  count(category) %>%
  mutate(proportion = n / sum(n))

train_data %>%
  count(bin_category) %>%
  mutate(proportion = n / sum(n))

# Define predictor and response variables
predictors <- subset(train_data, select = -c(category,num_category,bin_category))
cat_response <- train_data$category
ord_response <- train_data$num_category
bin_response <- train_data$bin_category
txt_response <- ifelse(train_data$bin_category == 1,"Nonimperiled", "Imperiled")

predictors_test <- subset(test_data, select = -c(category,num_category,
                                                bin_category))
cat_response_test <- test_data$category
ord_response_test <- test_data$num_category
bin_response_test <- test_data$bin_category
txt_response_test <- ifelse(bin_response_test == 1,"Nonimperiled", "Imperiled")

################################################################################
################################# Ordinal RF ###################################
################################################################################

is.factor(ord_response)
is.ordered(ord_response)

# define function for performance metrics

ord_metrics <- function(data, lev = NULL, model = NULL) 
  {
  obs  <- ordered(data$obs,  levels = lev)
  pred <- ordered(data$pred, levels = lev)

  k_minus1 <- max(length(lev) - 1, 1)
  
  macroMAE <- mean(abs(as.numeric(pred) - as.numeric(obs))) / k_minus1
  names(macroMAE) <- "macroMAE"
  
  macroMSE <- mean((as.numeric(pred) - as.numeric(obs))^2) / k_minus1^2
  names(macroMSE) <- "macroMSE"

  F1 <-  MLmetrics::F1_Score(y_pred=data$pred,y_true=data$obs)
  
  AK <- defaultSummary(data, lev, model)
  
  out <- c(macroMAE,macroMSE,F1,AK)
}

### # cross validation parameters
## random repeated cv

train_control_ord <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  summaryFunction = ord_metrics,
  predictionBounds = c(TRUE,TRUE),
  verboseIter = TRUE,
  returnResamp = 'all',
  savePredictions = 'all',
  seeds=seeds_ord
  )

## group K fold cv (groups train/test based on order)
# train_data.folds = groupKFold(train_data$order, k = 10)
# 
# # cross validation parameters
# train_control_ord <- trainControl(
#   method = "repeatedcv",
#   repeats = 5,
#   index = train_data.folds,
#   summaryFunction = ord_metrics,
#   predictionBounds = c(TRUE,TRUE),
#   verboseIter = TRUE,
#   returnResamp = 'all',
#   savePredictions = 'all'
# )

# 
# class_weights_ord =  ifelse(ord_response == 1, 14,
#                             ifelse(ord_response == 2, 12,
#                             ifelse(ord_response == 3, 2,
#                             ifelse(ord_response == 4, 2,
#                             ifelse(ord_response == 5,1,0)))))

mtry_grid <- expand.grid(mtry=20:length(predictors))

# Train and tune ordinal Random Forest model

ord_rf <- train(
  x = predictors,
  y = ord_response,
  method = "cforest",
  controls = cforest_unbiased(ntree = 1500),
  trControl = train_control_ord,
  tuneLength = 5,
  metric = "macroMAE",
  maximize = FALSE,
  tuneGrid = mtry_grid
  #weights = class_weights_ord
)

# load RF object
#load("ord_rf.RData")

# save RF object
setwd(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Output_data\Models)")
save(ord_rf, file = "ord_rf_no_kfold_cv.RData")

# Output model results
print(ord_rf)

# Evaluate ordinal classification performance

ord_predictions <- predict(ord_rf, newdata = predictors_test) 

conf_mat_ord <- confusionMatrix(ord_predictions, 
                                ord_response_test,
                                mode="everything")
print(conf_mat_ord)

setwd(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Output_data\Analyses\Model_Results)")
write.csv(conf_mat_ord$byClass, "performance_ord_no_rf_kfold_cv.csv", row.names = TRUE)
write.csv(conf_mat_ord$table, "confusion_ord_rf_no_kfold_cv.csv", row.names = TRUE)
write.csv(ord_rf$results, "performance_tuning_ord_no_rf_kfold_cv.csv", row.names = FALSE)

# novel variable importance measures for ordinal response data (Janitza et al.)
# https://www.ibe.med.uni-muenchen.de/organisation/mitarbeiter/070_drittmittel/janitza/rf_ordinal/tutorial.pdf

OrdRF_ER_VI <- varimp(ord_rf$finalModel) # error rate based variable importance (standard measure)
OrdRF_RPS_VI <- varimpRPS(ord_rf$finalModel) # RPS-based variable importance
OrdRF_MAE_VI <- varimpMAE(ord_rf$finalModel) # MAE-based variable importance
OrdRF_MSE_VI <- varimpMSE(ord_rf$finalModel) # MSE-based variable importance

# plot 15 most important variables for each measure
par(mfrow = c(4, 1))

barplot(sort(OrdRF_ER_VI,decreasing=TRUE)[1:15], 
        ylab = "Error rate based variable importance", las = 2)
barplot(sort(OrdRF_RPS_VI,decreasing=TRUE)[1:15], 
        ylab = "RPS-based", las = 2)
barplot(sort(OrdRF_MAE_VI,decreasing=TRUE)[1:15], 
        ylab = "MAE-based", las = 2)
barplot(sort(OrdRF_MSE_VI,decreasing=TRUE)[1:15], 
        ylab = "MSE-based", las = 2)
mtext(side = 3, text = "Variable importance by different measures", 
      outer = TRUE, line = -2)

importance_ordRF <- cbind(OrdRF_ER_VI,OrdRF_RPS_VI,OrdRF_MAE_VI,OrdRF_MSE_VI)

################################################################################
####################### Binary Categorical RF ##################################
################################################################################

# define function for performance metrics

bin_metrics  <- function(data, lev = NULL, model = NULL){
  F1 <-  MLmetrics::F1_Score(y_pred=data$pred,y_true=data$obs,positive=lev[1])
  PR <- prSummary(data, lev, model)
  ROC <- twoClassSummary(data, lev, model)
  AK <- defaultSummary(data, lev, model)
  out <- c(F1, PR, ROC, AK)
  out
}

# set up parallel processing

# no_cores <- 7
# cl <- makePSOCKcluster(no_cores)
# registerDoParallel(cl)

### # cross validation parameters

## group K fold cv (groups train/test based on order)

# train_control_bin <- trainControl(
#   method = "repeatedcv",
#   repeats = 5,
#   index = train_data.folds,
#   summaryFunction = bin_metrics,
#   predictionBounds = c(TRUE,TRUE),
#   classProbs=TRUE,
#   returnResamp = 'all',
#   savePredictions = 'all',
#   verboseIter = TRUE,
#   seeds = seeds
# )

## random repeated cv

train_control_bin <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  summaryFunction = bin_metrics,
  predictionBounds = c(TRUE,TRUE),
  classProbs=TRUE,
  returnResamp = 'all',
  savePredictions = 'all',
  verboseIter = TRUE,
  seeds = seeds_bin
)

class_weights_bin <- ifelse(txt_response == 'Imperiled',1,3)

mtry_grid <- expand.grid(mtry=2:20)

# Train Random Forest model (Binary categorical)
bin_rf <- train(
  x = predictors,
  y = txt_response,
  method = "rf",
  trControl = train_control_bin,
  metric = "AUC",
  tuneGrid=mtry_grid,
  tuneLength = 5,
  importance = TRUE,
  ntree = 1500,
  weights = class_weights_bin
)

# stopCluster(cl)
# registerDoSEQ()
# rm(cl)
# gc()

################################################################################
############### NOT RUN: TESTING REPRODUCIBILITY FOR DEV #######################
# RE FIT ORDINAL RF
ord_rf_rep <- train(
  x = predictors,
  y = ord_response,
  method = "cforest",
  controls = cforest_unbiased(ntree = 1500),
  trControl = train_control_ord,
  tuneLength = 5,
  metric = "macroMAE",
  maximize = FALSE,
  tuneGrid = mtry_grid
  #weights = class_weights_ord
)

# RE FIT BINARY RF
bin_rf_rep <- train(
  x = predictors,
  y = txt_response,
  method = "rf",
  trControl = train_control_bin,
  metric = "AUC",
  tuneGrid=mtry_grid,
  tuneLength = 5,
  importance = TRUE,
  ntree = 1500,
  weights = class_weights_bin
)

## TEST IF REP MODELS ARE IDENTICAL TO PREVIOUS RUNS

# Ordinal RF
ord_rf$bestTune == ord_rf_rep$bestTune
all(ord_rf$pred == ord_rf_rep$pred)
ord_rf$modelInfo

# Binary RF
all(bin_rf$finalModel$predicted == bin_rf_rep$finalModel$predicted)
all(bin_rf$finalModel$confusion == bin_rf_rep$finalModel$confusion)
all(bin_rf$finalModel$importance == bin_rf_rep$finalModel$importance)

################################################################################
################################################################################

# save RF object
setwd(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Output_data\Models)")
save(bin_rf, file = "bin_rf_no_kfold_cv.RData")

# Output model results
print(bin_rf)

# load RF object
#load("bin_rf.RData")

# Evaluate model accuracy and classification performance

predictions_bin <- predict(bin_rf, newdata=predictors_test)
conf_mat_bin <- confusionMatrix(predictions_bin, 
                            as.factor(txt_response_test),
                            mode="everything")

# save confusion and performance
setwd(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Output_data\Analyses\Model_Results)")
write.csv(bin_rf$results, "performance_tuning_bin_rf_no_kfold_cv.csv", row.names = FALSE)
write.csv(conf_mat_bin$table, "confusion_bin_rf_no_kfold_cv.csv", row.names = TRUE)

## variable importance Binary RF
BinRF_MDAG_VI <- importance(bin_rf$finalModel) # mean decrease accuracy + gini
colnames(BinRF_MDAG_VI) <- c("MDA_Imperiled","MDA_Nonimperiled", "BinRF_MDA",
                             "BinRF_Gini")

BinRF_AUC_VI <- caret::varImp(bin_rf$finalModel) # AUC-based variable importance
BinRF_AUC_VI$Nonimperiled <- NULL
colnames(BinRF_AUC_VI) <- c("BinRF_AUC_VI")

importance_binRF = cbind(BinRF_AUC_VI,BinRF_MDAG_VI)

# plot importance binary RF

varImpPlot(bin_rf$finalModel)

# Join VIMs from binary and ordinal models
importance_RFs <- cbind(importance_binRF,importance_ordRF)

###### join variable importance table with metadata ######

importance_RFs <- as.data.frame(importance_RFs)

metadata<-read.csv(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Input_datasets\Metadata_table.csv)", header=TRUE, 
                   stringsAsFactors = FALSE, strip.white=TRUE)

colnames(metadata)

importance_RFs$Category <- metadata$Category[match(rownames(importance_RFs), metadata$Field)]
importance_RFs$Subcategory <- metadata$Subcategory[match(rownames(importance_RFs), metadata$Field)]
importance_RFs$Description <- metadata$Label[match(rownames(importance_RFs), metadata$Field)]
importance_RFs$Label <- metadata$Label[match(rownames(importance_RFs), metadata$Field)]

# print table for Ivan
setwd(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Output_data\Analyses\Model_Results)")
write.csv((importance_RFs), "VIMs_wMetadata_July2025_no_kfold_cv.csv")

################# Print partial dependence plots and tables ####################

# Back convert float predictors standardized before RF

impvar <- rownames(importance_RFs)[order(importance_RFs$BinRF_AUC_VI, 
                                           decreasing=TRUE)]

convert10k<- c("Class_count_p","Symp_count_p",
               "RA_Dams_p","Ramsar_count_p")
convert100k<- c("Dams_p")
convert100 <- c("CAB_Dams_p")

# output directory for PD tables
setwd(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Output_data\Analyses\PD_BinRF_no_kfold_cv)")

# print PD tables
for (i in impvar) {
  
  csv <- bin_rf %>%
    partial(
      pred.var = c(i), 
      which.class = "Imperiled",
      prob = TRUE,
      chull = TRUE,
      trim.outliers = TRUE,
      progress = TRUE
    )
  
  colnames(csv)[which(names(csv) == "yhat")] <- "imperil_prob"
  
  if(any(grepl(i, unlist(convert10k))) == TRUE) {
    csv[1] <- csv[1]/10000
  }
  
  if(any(grepl(i, unlist(convert100k))) == TRUE) {
    csv[1] <- csv[1]/100000
  }
  
  if(any(grepl(i, unlist(convert100))) == TRUE) {
    csv[1] <- csv[1]/100
  }
  
  write.csv((csv),paste0("Partial_Dependence_on_", i,".csv"), row.names = FALSE)
  
}

# create PDP plots for numeric variables

impvar_num <- names(Filter(is.numeric, predictors))

for (i in impvar_num) {
  pdt = read.csv(paste0("Partial_Dependence_on_", i,".csv"))
  
  category <- importance_RFs[i, "Category"]
  
  label <- importance_RFs[i, "Label"]
  
  low_color <- ifelse(category == 'Environmental', 'darkslategray1',
                       ifelse(category=='Socioeconomic','orange','yellow'))


  pdp = ggplot(pdt,aes(x=.data[[i]], y=imperil_prob, color=.data[[i]])) +
    geom_path(linewidth=1,   lineend="round", linejoin="round") +
    scale_color_gradient(low=low_color, high = "black") +
    labs(
      x     = label,
      y     = "Imperilment probability"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(axis.title = element_text(family = "sans", face = "bold", size = 6),
          axis.text = element_text(family = "sans", size = 5,color = "black"),
          axis.line = element_line(linewidth = 0.25, color = "black"),
          axis.ticks = element_line(linewidth = 0.25, color = "black"),
          panel.grid = element_blank(),
          legend.position = "none",
          plot.margin = margin(2, 2, 2, 2))
  
  ggsave(filename = paste0("PDP ", i, ".jpg"), plot = pdp, width = 2, 
         height = 2,units = "in", dpi = 300)
  
}

# create PDP plots for categorical variables
impvar_cat <- names(Filter(is.factor, predictors))

for (i in impvar_cat) {

  pdt = read.csv(paste0("Partial_Dependence_on_", i,".csv"))
  
  category <- importance_RFs[i, "Category"]
  
  label <- importance_RFs[i, "Label"]
  
  barcol <- ifelse(category == 'Environmental', 'cyan4',
                   ifelse(category=='Socioeconomic','chocolate1','yellow3'))
  
  pdp = ggplot(pdt,aes(x=fct_rev(fct_reorder(as.factor(.data[[i]]),imperil_prob)),
                       y=imperil_prob)) +
    geom_col(fill=barcol) +
    labs(
      x     = label,
      y     = "Imperilment probability"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(axis.title = element_text(family = "sans", face = "bold", size = 6),
          axis.text = element_text(family = "sans", size = 5,color = "black"),
          axis.line = element_line(linewidth = 0.25, color = "black"),
          axis.ticks = element_line(linewidth = 0.25, color = "black"),
          axis.text.x=element_text(size=4, angle = 90,hjust = 1,vjust=0.5),
          panel.grid = element_blank(),
          legend.position = "none")

  ggsave(filename = paste0("PDP ", i, ".jpg"), plot = pdp, width = 2, height = 2,
         units = "in", dpi = 300)

}
