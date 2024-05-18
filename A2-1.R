################################################################################
#packages 

library(readr)
library(dplyr)
library(powerjoin)
library(skimr)
library(vtreat)
library(randomForest)
library(caret)
library(MLmetrics)
library(pROC)
library(corrplot)
library(ggplot2)
library(gridExtra)
################################################################################
#reading the data

HITest <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTest.csv')
HITrain <-read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTrain.csv')
DBTest <-read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTest.csv')
DBTrain <-read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTrain.csv')
DPTest <-read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTest.csv')
DPTrain <-read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTrain.csv')

################################################################################

# Combine the test CSV files using tmpID
Test <- merge(HITest, DBTest, by = "tmpID")
Test <- merge(Test, DPTest, by = "tmpID")
str(Test) # print the combined data frame

# Combine the train CSV files using tmpID
Train <- merge(HITrain, DBTrain, by = "tmpID")
Train <- merge(Train, DPTrain, by = "tmpID")
str(Train) # print the combined data frame


################################################################################
# Joining both datasets

# Add identifier column to train and test data frames
Train$set_id <- "train" 
Test$set_id <- "test"

Test

# Combine the two new data frames
diabetesData <- rbind(Test,Train)
diabetesData # print the combined data frame
skim(diabetesData)

#write.csv(diabetesData, 'diabetesData.csv', row.names = F)
################################################################################

#transform data types

diabetesData$admission_type_id <- as.factor(diabetesData$admission_type_id)
diabetesData$discharge_disposition_id <- as.factor(diabetesData$discharge_disposition_id)
diabetesData$admission_source_id <- as.factor(diabetesData$admission_source_id)
diabetesData$medical_specialty  <- as.factor(diabetesData$medical_specialty)
diabetesData$diag_1_desc <- as.factor(diabetesData$diag_1_desc) 
diabetesData$diag_2_desc <- as.factor(diabetesData$diag_2_desc)                    
diabetesData$diag_3_desc <- as.factor(diabetesData$diag_3_desc)
diabetesData$A1Cresult <- as.factor(diabetesData$A1Cresult)
diabetesData$max_glu_serum <- as.factor(diabetesData$max_glu_serum)
diabetesData$metformin <- as.factor(diabetesData$metformin)
diabetesData$repaglinide <- as.factor(diabetesData$repaglinide)
diabetesData$nateglinide <- as.factor(diabetesData$nateglinide)
diabetesData$chlorpropamide <- as.factor(diabetesData$chlorpropamide)
diabetesData$glimepiride <- as.factor(diabetesData$glimepiride)
diabetesData$acetohexamide <- as.factor(diabetesData$acetohexamide)
diabetesData$glipizide <- as.factor(diabetesData$glipizide)
diabetesData$glyburide <- as.factor(diabetesData$glyburide)
diabetesData$tolbutamide <- as.factor(diabetesData$tolbutamide)
diabetesData$pioglitazone  <- as.factor(diabetesData$pioglitazone)
diabetesData$rosiglitazone  <- as.factor(diabetesData$rosiglitazone)
diabetesData$acarbose  <- as.factor(diabetesData$acarbose)
diabetesData$miglitol  <- as.factor(diabetesData$pioglitazone)
diabetesData$troglitazone  <- as.factor(diabetesData$troglitazone)
diabetesData$tolazamide  <- as.factor(diabetesData$tolazamide)
diabetesData$examide  <- as.factor(diabetesData$examide)
diabetesData$citoglipton  <- as.factor(diabetesData$citoglipton)
diabetesData$insulin  <- as.factor(diabetesData$insulin)
diabetesData$change  <- as.factor(diabetesData$change)
diabetesData$diabetesMed  <- as.factor(diabetesData$diabetesMed)
diabetesData$race   <- as.factor(diabetesData$race)
diabetesData$gender  <- as.factor(diabetesData$gender)
diabetesData$payer_code  <- as.factor(diabetesData$payer_code)
diabetesData$set_id  <- as.factor(diabetesData$set_id)
# diabetesData$readmitted_y <- factor(diabetesData$readmitted_y)

skim(diabetesData)



################################################################################

#deal with missing values

#replace empties,?, not available with NA
diabetesData[diabetesData == "" | diabetesData== "Not Available"| diabetesData== "Not Mapped"| diabetesData== "?"] <- NA


# Printing NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
missing_values <- apply(diabetesData,2,pMiss)

# percentage of nulls in each column
missing_values

################################################################################
#drop payer_id and medical_specialty because they have more than 40% NA

diabetesData$payer_code <- NULL
diabetesData$medical_specialty <- NULL
dim(diabetesData)

################################################################################
# check for columns where all rows have the same value and drop them
sapply(diabetesData, function(x) length(unique(x)))

#examide, citoglipton, troglitazone, acetohexamide
diabetesData$examide<- NULL
diabetesData$citoglipton<- NULL
diabetesData$troglitazone<- NULL
diabetesData$acetohexamide<- NULL

dim(diabetesData)

################################################################################
# deleting rows where patients died, as they cannot return 

diabetesData <- diabetesData[diabetesData$discharge_disposition_id != "Expired", ]
dim(diabetesData)

################################################################################
# EDA

numeric_cols <- sapply(diabetesData, is.numeric)
selected_cols <- names(diabetesData)[numeric_cols]
clean_data <- diabetesData[complete.cases(diabetesData[, selected_cols]), ]
correlation_matrix <- cor(clean_data[, selected_cols])

corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("white", "steelblue"))(100), addCoef.col = "black", tl.cex = 0.8, tl.col = "black", title = "Correlation Matrix of Diabetes Data")


################################################################################
#split test and train data again

train2 <- diabetesData %>% filter(set_id == 'train')
test2 <- diabetesData %>% filter(set_id == 'test')

dim(train2)
dim(test2)
dim(diabetesData)

################################################################################

# Randomize the entire data set to ensure no auto-correlation 
set.seed(2023)
modelingDF <- train2[sample(1:nrow(train2),nrow(train2)),]

# Segment the prep data
set.seed(1234)
idx         <- sample(1:nrow(modelingDF),.1*nrow(modelingDF))
prepData    <- modelingDF[idx,]
nonPrepData <- modelingDF[-idx,]

# Create design treatment Plan
plan <- designTreatmentsC(prepData, 
                          colnames(prepData)[!colnames(prepData) %in% c("tmpID","readmitted_y")],
                          "readmitted_y",TRUE)

# Apply treatment plan to data

treatedTrain <- prepare(plan, nonPrepData)
treatedTest  <- prepare(plan, test2)
idx         <- sample(1:nrow(treatedTrain),.7*nrow(treatedTrain))
train3    <- treatedTrain[idx,]
validation <- treatedTrain[-idx,]

################################################################################
#Random Forest
################################################################################
#fit random forest model
formula <- as.factor(readmitted_y) ~ .
rf_model <- randomForest(formula, data = train3)

#Make predictions on the validation data
rf_preds <- predict(rf_model, newdata = validation, type = "prob")

# cut-off
rf_preds <- rf_preds[, 2]
rf_preds_logical <- ifelse(rf_preds > 0.5, TRUE, FALSE)
rf_preds_factor <- factor(rf_preds_logical, levels = c(FALSE,TRUE ))

# reorder levels
validation$readmitted_y_factor <- factor(validation$readmitted_y, levels = c(FALSE, TRUE))

################################################################################
# Confusion Matrix

rf_cm <- confusionMatrix(rf_preds_factor, validation$readmitted_y_factor)
rf_cm

################################################################################
# comparing actual and results

results <- data.frame(actual  = nonPrepData[-idx,]$readmitted_y,
                      id    = nonPrepData[-idx,]$tmpID,probs   = rf_preds)
head(results)

################################################################################
# Calculate AUC

numpredictions <- as.numeric(rf_preds_factor)-1
numvalid <- as.numeric(validation$readmitted_y)

auc <- roc(numvalid, numpredictions)$auc

roc_obj <- roc(numvalid, numpredictions)
auc <- auc(roc_obj)

plot(roc_obj, main = paste("ROC Curve (AUC =", auc, ")"))
################################################################################
#GLM
################################################################################

# Fit GLM model
glm_model <- glm(readmitted_y ~ ., data = train3, family = 'binomial')

# Backward step wise selection 
best_fit <- step(glm_model, direction = "backward")

# Make predictions on the validation data
glm_preds <- predict(best_fit, validation, type = 'response')

# Classify the predictions based on a cutoff value of 0.5
cutoff <- 0.5

glmClasses <- ifelse(glm_preds >= cutoff, 1, 0)

################################################################################
# Confusion Matrix

# Compute the confusion matrix
glm_cm <- ConfusionMatrix(factor(glm_preds), factor(validation$readmitted_y))

# Print the confusion matrix
glm_cm

################################################################################

# comparing actual and results
results <- data.frame(actual  = nonPrepData[-idx,]$readmitted_y,
                      patientID    = nonPrepData[-idx,]$tmpID,
                      classes = glmClasses,
                      probs   = glm_preds)
head(results)
################################################################################

# Accuracy
Accuracy(results$classes, results$actual)

################################################################################
# Visually how well did we separate our classes?
ggplot(results, aes(x=probs, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'darkgreen')

# ROC; chg boolean to 1/0
ROCobj <- roc(results$classes, results$actual*1)
plot(ROCobj)

# AUC; chg boolean to 1/0
AUC(results$actual*1,results$classes)

################################################################################
# Adjusted
newCutoff <- .41
newClasses <- ifelse(glm_preds >= newCutoff, 1,0)


adjusted_results <- data.frame(actual  = nonPrepData[-idx,]$readmitted_y,
                               patientID    = nonPrepData[-idx,]$tmpID,
                               classes = newClasses,
                               probs   = glm_preds)

(confMat <- ConfusionMatrix(newClasses, adjusted_results$actual))

Accuracy(newClasses, adjusted_results$actual)

# Visually how well did we separate our classes?
ggplot(adjusted_results, aes(x=probs, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = newCutoff), color = 'darkgreen')

# ROC; chg boolean to 1/0
ROCobj <- roc(adjusted_results$classes, adjusted_results$actual*1)
plot(ROCobj)

# AUC; chg boolean to 1/0
AUC(adjusted_results$actual*1,adjusted_results$classes)


################################################################################
# Real predictions on test data with Random Forest
################################################################################

#Make predictions on test data
test_preds <- predict(rf_model, newdata = treatedTest, type = "prob")
test_preds <- data.frame(test_preds)

head(test_preds)

################################################################################
# comparing actual and results

test_results <- data.frame(actual  = test2$readmitted_y,
                           tmpID    = test2$tmpID,
                           probs   = test_preds)

test_results


################################################################################

# Sort by probabilities
ordered <- test_results[order(test_results$probs.TRUE., decreasing = TRUE),]
top100 <- head(ordered,100)
head(top100)

################################################################################
# join the top 100 with the full dataset

t100 <- inner_join(test2, top100, by = "tmpID")
dim(t100)

################################################################################
# Write top 100 CSV

write.csv(t100, 'all100.csv', row.names = FALSE)

################################################################################
#EDA Top 100
################################################################################
summary(t100)

################################################################################
#Feature Importance

fi <- varImp(rf_model)

head(fi)
varImpPlot(rf_model)


################################################################################
#number of lab procedures

ggplot() +
  geom_density(data = t100, aes(x = num_lab_procedures, color = "blue", fill ="blue"), alpha = 0.5) +
  geom_density(data = train2, aes(x = num_lab_procedures, color = "red", fill = "red" ), alpha = 0.5) +
  labs(title = "Number of lab procedures",
       x = "num_lab_procedure",
       y = "Density",
       color = "Data Source") +
  scale_color_discrete(labels = c("All training data", "Top 100")) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  guides(fill = FALSE)  # remove the fill legend

################################################################################

#Weight Distribution 

ggplot() +
  geom_density(data = t100, aes(x = wgt, color = "blue", fill = "blue"), alpha = 0.5) +
  geom_density(data = treatedTrain, aes(x = wgt, color = "red", fill = "red"), alpha = 0.5) +
  labs(title = "Weight Distribution",
       x = "Weight",
       y = "Density",
       color = "Data Source") +
  scale_color_discrete(labels = c("All training data", "Top 100")) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  guides(fill = FALSE)  # remove the fill legend


################################################################################
#Age Distribution 

ggplot() +
  geom_density(data = t100, aes(x = age, color = "blue", fill = "blue"), alpha = 0.5) +
  geom_density(data = treatedTest, aes(x = age, color = "red", fill = "red"), alpha = 0.5) +
  labs(title = "Age Distribution Comparison",
       x = "Age",
       y = "Density",
       color = "Data Source") +
  scale_color_discrete(labels = c("All training data","Top 100")) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  guides(fill = FALSE)  


################################################################################
#gender
# create plot for t100 data
df_counts_g <- count(t100, gender)
plot_t100 <- ggplot(data = df_counts_g, aes(x = gender, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  geom_text(aes(label = scales::percent(n/sum(n)), y = n), 
            stat = "identity", vjust = -0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = "t100")

# create plot for test2 data
df_counts_2 <- count(test2, gender)
plot_test2 <- ggplot(data = df_counts_2, aes(x = gender, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  geom_text(aes(label = scales::percent(n/sum(n)), y = n), 
            stat = "identity", vjust = -0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = "test2")

# display plots side by side using gridExtra
grid.arrange(plot_t100, plot_test2, ncol = 2)

################################################################################
#race 

# create plot for t100 data

df_counts_t <- count(t100, race)
plot_t100 <- ggplot(data = df_counts_t, aes(x = race, y = n)) + 
  geom_bar(stat = "identity", fill = "#2c7bb6") +
  theme_minimal() +
  theme(panel.grid = element_blank())

# create plot for test2 data
df_counts_2 <- count(test2, race)
plot_test2 <- ggplot(data = df_counts_2, aes(x = race, y = n)) + 
  geom_bar(stat = "identity", fill = "#2c7bb6") +
  theme_minimal() +
  theme(panel.grid = element_blank())

# display plots side by side using gridExtra
grid.arrange(plot_t100, plot_test2, ncol = 2)

################################################################################
#number of medications

ggplot()+
  geom_density(data = t100, aes(x = num_medications, color = "blue", fill= "blue"), alpha = 0.5) +
  geom_density(data = treatedTrain, aes(x = num_medications, color = "red", fill = "red" ), alpha = 0.5) +
  labs(title = "Number of medications",
       x = "num_medications",
       y = "Density",
       color = "Data Source") +
  scale_color_discrete(labels = c("All training data", "Top 100")) +
  theme_minimal()+
  theme(panel.grid = element_blank()) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) 

################################################################################
#time in hospital

ggplot()+
  geom_density(data = t100, aes(x = time_in_hospital, color = "blue", fill= "blue"), alpha = 0.5) +
  geom_density(data = treatedTrain, aes(x = time_in_hospital, color = "red", fill = "red" ), alpha = 0.5) +
  labs(title = "Time in Hospital",
       x = "Time in Hospital",
       y = "Density",
       color = "Data Source") +
  scale_color_discrete(labels = c("All training data", "Top 100")) +
  theme_minimal()+
  theme(panel.grid = element_blank())
theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  guides(fill = FALSE)  

################################################################################
#number diagnoses

ggplot()+
  geom_density(data = t100, aes(x = number_diagnoses, color = "blue", fill= "blue"), alpha = 0.5) +
  geom_density(data = treatedTrain, aes(x = number_diagnoses, color = "red", fill = "red" ), alpha = 0.5) +
  labs(title = "Number of Diagnoses ",
       x = "Number of Diagnose",
       y = "Density",
       color = "Data Source") +
  scale_color_discrete(labels = c("All training data", "Top 100")) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))   

