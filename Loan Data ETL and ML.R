# Load necessary libraries
library(rpart)        # For classification decision trees
library(rpart.plot)   # For visualizing decision trees
library(ROCR)         # For performance evaluation (ROC curves, AUC)

# STEP 1: Read in Data

# Define file path (modify as needed for your system)
PATH = "rstudioforML/HMEQ_Scrubbed_1.csv"
FILE_NAME = "HMEQ_Scrubbed_1.csv"  # Data file

# Create a full path to the file
INFILE = paste(PATH, FILE_NAME, sep="/")

# Set working directory to the file location
setwd(PATH)

# Read the CSV file into a dataframe
# Ensure the file is present at the specified location

df = read.csv((FILE_NAME))

# Inspect the dataset
head(df)       # Display first few rows
str(df)        # Show structure (column names, types, missing values)
summary(df)    # Provide summary statistics

# STEP 2: Classification Decision Tree

# Set a seed for reproducibility
SEED = 1
set.seed(SEED)

# Create a copy of the dataset and remove the target loss amount column (not needed for classification)
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL  

# Split data into training (70%) and testing (30%) sets
FLAG = sample(c(T,F), nrow(df_flag), replace = T, prob = c(0.7,0.3))
df_train = df[FLAG, ]  # Training dataset
df_test = df[!FLAG, ]  # Testing dataset

dim(df_flag)  # Check dimensions of full dataset
dim(df_train) # Check dimensions of training dataset
dim(df_test)  # Check dimensions of testing dataset

# Set tree control parameters (max depth of 6)
tr_set = rpart.control(maxdepth = 6)

# Train a classification decision tree using the Gini impurity criterion
t1G = rpart(data = df_train,
            TARGET_BAD_FLAG ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp,
            control = tr_set,
            method = "class",
            parms = list(split= 'gini'))

# Train a classification decision tree using the Information Gain (Entropy) criterion
t1E = rpart(data = df_train,
            TARGET_BAD_FLAG ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp,
            control = tr_set,
            method = "class",
            parms = list(split= 'information'))

# Visualize both decision trees,
rpart.plot(t1G)  # Gini tree
rpart.plot(t1E)  # Entropy tree

# Print variable importance
print(t1G$variable.importance)
print(t1E$variable.importance)

# Predictions on training data
pG = predict(t1G, df_train)
pG2 = prediction(pG[,2], df_train$TARGET_BAD_FLAG)
pG3 = performance(pG2,"tpr","fpr")

pE = predict(t1E, df_train)
pE2 = prediction(pE[,2], df_train$TARGET_BAD_FLAG)
pE3 = performance(pE2,"tpr","fpr")

# Plot ROC curves for training data
plot(pG3, col="red")
plot(pE3, col="green", add=T)
abline(0,1,lty=2) # Diagonal reference line
legend("bottomright",c("TRAIN GINI","TRAIN ENTROPY"),
       col=c("red","green"),
       bty = "y", lty = 1)

# Compute AUC (Area Under Curve) for training data
aucG = performance(pG2, "auc")@y.values
aucE = performance(pE2, "auc")@y.values
print(paste("TRAIN AUC GINI", aucG))
print(paste("TRAIN AUC ENTROPY", aucE))

# Confusion matrices for training data
fG = predict(t1G, df_train, type = "class")
fE = predict(t1E, df_train, type = "class")

table(fG,df_train$TARGET_BAD_FLAG)
table(fE,df_train$TARGET_BAD_FLAG)

# Predictions on test data
pG = predict(t1G, df_test)
pG2 = prediction(pG[,2], df_test$TARGET_BAD_FLAG)
pG3 = performance(pG2,"tpr","fpr")

pE = predict(t1E, df_test)
pE2 = prediction(pE[,2], df_test$TARGET_BAD_FLAG)
pE3 = performance(pE2,"tpr","fpr")

# Plot ROC curves for test data
plot(pG3, col="red")
plot(pE3, col="green", add=T)
abline(0,1,lty=2)
legend("bottomright",c("TEST GINI","TEST ENTROPY"),
       col=c("red","green"),
       bty = "y", lty = 1)

# Compute AUC (Area Under Curve) for test data
aucG = performance(pG2, "auc")@y.values
aucE = performance(pE2, "auc")@y.values
print(paste("TEST AUC GINI", aucG))
print(paste("TEST AUC ENTROPY", aucE))

# Confusion matrices for test data
fG = predict(t1G, df_test, type = "class")
fE = predict(t1E, df_test, type = "class")

table(fG,df_test$TARGET_BAD_FLAG)
table(fE,df_test$TARGET_BAD_FLAG)

# Train a decision tree using Gini impurity

t1G = rpart(data = df_flag,
            TARGET_BAD_FLAG ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+
              FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+
              FLAG.REASON.DebtCon+FLAG.REASON.HomeImp,
            control = tr_set,
            method = "class",
            parms = list(split= 'gini'))

# Train a decision tree using Information Gain (Entropy)

t1E = rpart(data = df_flag,
            TARGET_BAD_FLAG ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+
              FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+
              FLAG.REASON.DebtCon+FLAG.REASON.HomeImp,
            control = tr_set,
            method = "class",
            parms = list(split= 'information'))

# Visualize the trained decision trees
rpart.plot(t1G)  # Plot Gini-based tree
t1G$variable.importance  # Show feature importance for Gini-based tree

rpart.plot(t1E)  # Plot Entropy-based tree
t1E$variable.importance  # Show feature importance for Entropy-based tree

# Make predictions using Gini-based model
pG = predict(t1G, df_flag)
pG2 = prediction(pG[,2], df_flag$TARGET_BAD_FLAG)
pG3 = performance(pG2, "tpr", "fpr")

# Make predictions using an Entropy-based model
pE = predict(t1E, df_flag)
pE2 = prediction(pE[,2], df_flag$TARGET_BAD_FLAG)
pE3 = performance(pE2, "tpr", "fpr")

# Plot ROC curves to compare models
plot(pG3, col="red")  # Plot Gini ROC curve
plot(pE3, col="green", add=T)  # Add Entropy ROC curve to same plot
abline(0,1,lty=2)  # Diagonal reference line
legend("bottomright",c("TRAIN GINI","TRAIN ENTROPY"),
       col=c("red","green"),
       bty = "y", lty = 1)

# Compute AUC values for both models
aucG = performance(pG2, "auc")@y.values
aucE = performance(pE2, "auc")@y.values

# Print AUC results
print(paste("ALL DATA AUC GINI", aucG))
print(paste("ALL DATA AUC ENTROPY", aucE))

# Make final class predictions using both models
fG = predict(t1G, df_flag, type = "class")
fE = predict(t1E, df_flag, type = "class")

# Generate confusion matrices to evaluate model performance
table(fG,df_flag$TARGET_BAD_FLAG)
table(fE,df_flag$TARGET_BAD_FLAG)

# Load necessary libraries
library(rpart)
library(rpart.plot)

# Step 1: Data Preparation
# Create a copy of the original dataset, excluding the TARGET_BAD_FLAG column
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

# Split data into training (70%) and testing (30%) sets
FLAG = sample(c(T,F), nrow(df_amt), replace = T, prob = c(0.7,0.3))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]

# Display the first 10 values of the FLAG vector
head(FLAG, 10)

# Compute and print the mean of TARGET_LOSS_AMT for full, train, and test datasets
mean(df_amt$TARGET_LOSS_AMT)
mean(df_train$TARGET_LOSS_AMT)
mean(df_test$TARGET_LOSS_AMT)

# Print the dimensions of the datasets
dim(df_amt)
dim(df_train)
dim(df_test)

# Step 2: Train Regression Decision Tree Models
# Set control parameters for decision trees
tr_set = rpart.control(maxdepth = 6)

# Train a regression tree using the ANOVA method
t1a = rpart(data = df_train,
            TARGET_LOSS_AMT ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+
              FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+
              FLAG.REASON.DebtCon+FLAG.REASON.HomeImp,
            control = tr_set,
            method = "anova")

# Train a regression tree using the Poisson method
t1p = rpart(data = df_train,
            TARGET_LOSS_AMT ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+
              FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+
              FLAG.REASON.DebtCon+FLAG.REASON.HomeImp,
            control = tr_set,
            method = "poisson")

# Visualize the decision trees
rpart.plot(t1a)
rpart.plot(t1p)

# Print variable importance from each model
t1a$variable.importance
t1p$variable.importance

# Step 3: Compute RMSE for Train Dataset
# Predict on training data using the ANOVA model
p1a = predict(t1a, df_train)
RMSE1a = sqrt(mean((df_train$TARGET_LOSS_AMT - p1a)^2))

# Predict on training data using the Poisson model
p1p = predict(t1p, df_train)
RMSE1p = sqrt(mean((df_train$TARGET_LOSS_AMT - p1p)^2))

# Print RMSE values for the train dataset
print(paste("TRAIN RMSE ANOVA =", RMSE1a))
print(paste("TRAIN RMSE poisson =", RMSE1p))

# Step 4: Evaluate Models on Test Dataset
# Train the models again using the test dataset
tr_set = rpart.control(maxdepth = 6)

t1a = rpart(data = df_test, TARGET_LOSS_AMT ~ ., control = tr_set, method = "anova")
t1p = rpart(data = df_test, TARGET_LOSS_AMT ~ ., control = tr_set, method = "poisson")

# Visualize the decision trees for the test dataset
rpart.plot(t1a)
rpart.plot(t1p)

# Compute RMSE for test dataset
p1a = predict(t1a, df_test)
RMSE1a = sqrt(mean((df_test$TARGET_LOSS_AMT - p1a)^2))

p1p = predict(t1p, df_test)
RMSE1p = sqrt(mean((df_test$TARGET_LOSS_AMT - p1p)^2))

# Print RMSE values for the test dataset
print(paste("TEST RMSE ANOVA =", RMSE1a))
print(paste("TEST RMSE poisson =", RMSE1p))

# Step 5: Train and Evaluate Models on Full Dataset
tr_set = rpart.control(maxdepth = 6)

t1a = rpart(data = df_amt, TARGET_LOSS_AMT ~ ., control = tr_set, method = "anova")
t1p = rpart(data = df_amt, TARGET_LOSS_AMT ~ ., control = tr_set, method = "poisson")

# Visualize the decision trees for the full dataset
rpart.plot(t1a)
rpart.plot(t1p)

# Compute RMSE for full dataset
p1a = predict(t1a, df_amt)
RMSE1a = sqrt(mean((df_amt$TARGET_LOSS_AMT - p1a)^2))

p1p = predict(t1p, df_amt)
RMSE1p = sqrt(mean((df_amt$TARGET_LOSS_AMT - p1p)^2))

# Print RMSE values for full dataset
print(paste("AD RMSE ANOVA =", RMSE1a))
print(paste("AD RMSE poisson =", RMSE1p))

# STEP 4: Probability/Severity Model Decision Tree

# Building a decision tree model to predict the probability of default (TARGET_BAD_FLAG)
t2_f = rpart(data=df_flag, TARGET_BAD_FLAG ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp, control = tr_set)
rpart.plot(t2_f)  # Visualizing the decision tree
p2_f = predict(t2_f, df)  # Predicting probabilities
head(p2_f)

# Subsetting data for customers who defaulted
df_amt_2 = subset(df, TARGET_BAD_FLAG == 1)
head(df_amt_2)
df_amt_2$TARGET_BAD_FLAG = NULL  # Removing the flag column as it's no longer needed
head(df_amt_2)

# Building a Poisson regression decision tree model to predict loss amount (TARGET_LOSS_AMT)
t2_a = rpart(data = df_amt_2, TARGET_LOSS_AMT ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp, control = tr_set, method = "poisson")
rpart.plot(t2_a)  # Visualizing the decision tree
p2_a = predict(t2_a, df)  # Predicting loss amount

# Combining probability and severity models
p2 = p2_f * p2_a
head(p2)

# Calculating RMSE for model evaluation
RMSE2 = sqrt(mean((df$TARGET_LOSS_AMT - p2)^2))
print(RMSE2)

# Splitting data into training and testing sets
FLAG = sample(c(T,F), nrow(df_amt_2), replace = T, prob = c(0.7,0.3))
df_train = df_amt_2[FLAG, ]
df_test = df_amt_2[!FLAG, ]

# Training decision tree models on training dataset
tr_set = rpart.control(maxdepth = 10)

# ANOVA decision tree model for loss amount
t1a = rpart(data = df_train, TARGET_LOSS_AMT ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp, control = tr_set)

# Poisson decision tree model for loss amount
t1p = rpart(data = df_train, TARGET_LOSS_AMT ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp, control = tr_set, method = "poisson")

# Visualizing decision trees
rpart.plot(t1a)
t1a$variable.importance  # Checking feature importance
rpart.plot(t1p)
t1p$variable.importance  # Checking feature importance

# Evaluating models on training set
p1a = predict(t1a, df_train)
RMSE1a = sqrt(mean((df_train$TARGET_LOSS_AMT - p1a)^2))
RMSE1a

p1p = predict(t1p, df_train)
RMSE1p = sqrt(mean((df_train$TARGET_LOSS_AMT - p1p)^2))
RMSE1p

print(paste("TRAIN RMSE ANOVA =", RMSE1a))
print(paste("TRAIN RMSE Poisson =", RMSE1p))

# Evaluating models on test set
tr_set = rpart.control(maxdepth = 10)

# ANOVA decision tree model
t1a = rpart(data = df_test, TARGET_LOSS_AMT ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp, control = tr_set)

# Poisson decision tree model
t1p = rpart(data = df_test, TARGET_LOSS_AMT ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp, control = tr_set, method = "poisson")

# Visualizing decision trees
rpart.plot(t1a)
t1a$variable.importance
rpart.plot(t1p)
t1p$variable.importance

# Calculating RMSE for test set
p1a = predict(t1a, df_test)
RMSE1a = sqrt(mean((df_test$TARGET_LOSS_AMT - p1a)^2))
RMSE1a

p1p = predict(t1p, df_test)
RMSE1p = sqrt(mean((df_test$TARGET_LOSS_AMT - p1p)^2))
RMSE1p

print(paste("TEST RMSE ANOVA =", RMSE1a))
print(paste("TEST RMSE Poisson =", RMSE1p))

# Training on full dataset
t1a = rpart(data = df_amt_2, TARGET_LOSS_AMT ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp, control = tr_set)

t1p = rpart(data = df_amt_2, TARGET_LOSS_AMT ~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.JOB.Mgr+FLAG.JOB.Office+FLAG.JOB.Other+FLAG.JOB.ProfExe+FLAG.JOB.Sales+FLAG.JOB.Self+FLAG.REASON.DebtCon+FLAG.REASON.HomeImp, control = tr_set, method = "poisson")

# Final RMSE calculation
p1a = predict(t1a, df_amt_2)
RMSE1a = sqrt(mean((df_amt_2$TARGET_LOSS_AMT - p1a)^2))
RMSE1a

p1p = predict(t1p, df_amt_2)
RMSE1p = sqrt(mean((df_amt_2$TARGET_LOSS_AMT - p1p)^2))
RMSE1p

print(paste("ALL DATA RMSE ANOVA =", RMSE1a))
print(paste("ALL DATA RMSE Poisson =", RMSE1p))
