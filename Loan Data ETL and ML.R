library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(gbm)
library(MASS)
library(Rtsne)
SEED = 1
set.seed(SEED)

PATH = "C:/Users/kapad/OneDrive/Desktop/Trine/DS"
FILE_NAME = "HMEQ_Scrubbed.csv"

INFILE = paste(PATH, FILE_NAME, sep = "/")

setwd(PATH)
df=read.csv(FILE_NAME)

str(df)
summary(df)
head(df, 6)

#STEP 2: PCA Analysis
df_pca = df
df_pca$TARGET_BAD_FLAG = NULL
df_pca$TARGET_LOSS_AMT = NULL
df_pca$M_MORTDUE = NULL
df_pca$M_VALUE = NULL
df_pca$M_YOJ = NULL
df_pca$IMP_DEROG = NULL
df_pca$M_DEROG = NULL
df_pca$IMP_DELINQ = NULL
df_pca$M_CLAGE = NULL
df_pca$IMP_NINQ = NULL
df_pca$M_NINQ = NULL
df_pca$M_CLNO = NULL
df_pca$M_DEBTINC = NULL
df_pca$FLAG.Job.Mgr = NULL
df_pca$FLAG.Job.Office = NULL
df_pca$FLAG.Job.Other = NULL
df_pca$FLAG.Job.ProfExe = NULL
df_pca$FLAG.Job.Sales = NULL
df_pca$FLAG.Job.Self = NULL
df_pca$FLAG.Reason.DebtCon = NULL
df_pca$FLAG.Reason.HomeImp = NULL
df_pca$M_DELINQ = NULL
head(df_pca)

pca = prcomp(df_pca, center = TRUE, scale =TRUE)
summary(pca)
plot(pca, type = "lines")
df_new = predict(pca, df_pca)
head(df_new)

pca

df_no_flags = df
df_no_flags$PC1 = df_new[,"PC1"]
df_no_flags$PC2 = df_new[,"PC2"]
df_no_flags$PC3 = df_new[,"PC3"]
head(df_no_flags)

colors <- c("#00AFBB","#E7B800")
colors <- c("red","black")
colors <- colors[df_no_flags$TARGET_BAD_FLAG + 1]
plot(df_no_flags$PC1, df_no_flags$PC2, col=colors, pch = 16)
legend("bottomright", c("defaults","non-defaults"), col=c("black","red"), bty="y", lty=1)


#Random dataframe
df_no_flags$RAND1 = sample(100, size = nrow(df_no_flags), replace = TRUE)
df_no_flags$RAND2 = sample(100, size = nrow(df_no_flags), replace = TRUE)

head(df_no_flags)

df_no_flags0 = df_no_flags[which(df_no_flags$TARGET_BAD_FLAG == 0), ]
df_no_flags1 = df_no_flags[which(df_no_flags$TARGET_BAD_FLAG == 1), ]
dim(df_no_flags0)
dim(df_no_flags1)

df_no_flags0 = df_no_flags0[df_no_flags0$RAND1 < 25, ]
df_no_flags1 = df_no_flags1[df_no_flags0$RAND1 < 75, ]
dim(df_no_flags0)
dim(df_no_flags1)

df_no_flagsx = rbind(df_no_flags0, df_no_flags1)
dim(df_no_flagsx)
df_no_flagsx = df_no_flagsx[df_no_flagsx$RAND2 < 15, ]
dim(df_no_flagsx)

#df_no_flagsx = df_no_flags

colors <- c("#00AFBB","#E7B800")
colors <- c("red","black")
colors <- colors[df_no_flagsx$TARGET_BAD_FLAG + 1]
plot(df_no_flagsx$PC1, df_no_flagsx$PC2, col=colors, pch = 16)
legend("bottomright", c("defaults","non-defaults"), col=c("black","red"), bty="y", lty=1)



#STEP 3: tSNE Analysis

dfu = df
dfu$TARGET_LOSS_AMT = NULL
dfu = unique(dfu)
head(dfu)

theTSNE = Rtsne(dfu[,c(2,3,5,7,13,17,19)], dims = 2, perplexity = 30, versobe = TRUE, max_iter = 500)

dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]

colors <- c("#00AFBB","#E7B800")
colors <- c("red","black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot(dfu$TS1, dfu$TS2, col=colors, pch = 16)
legend("bottomleft", c("defaults","non-defaults"), col=c("black","red"), bty="y", lty=1)


#Random number Dataframe

dfu$RAND1 = sample(100, size = nrow(dfu), replace = TRUE)
dfu$RAND2 = sample(100, size = nrow(dfu), replace = TRUE)


dfu0 = dfu[which(dfu$TARGET_BAD_FLAG == 0), ]
dfu1 = dfu[which(dfu$TARGET_BAD_FLAG == 1), ]

dfu0 = dfu0[dfu$RAND1 < 25, ]
dfu1 = dfu1[dfu$RAND1 < 75, ]


dfux = rbind(dfu0, dfu1)
dfux = dfux[dfux$RAND2 < 15, ]

head(dfu)

colors <- c("#00AFBB","#E7B800")
colors <- c("red","black")
colors <- colors[dfux$TARGET_BAD_FLAG + 1]
plot(dfux$TS1, dfux$TS2, col=colors, pch = 16)
legend("bottomleft", c("defaults","non-defaults"), col=c("black","red"), bty="y", lty=1)


#Model with perplexity = 45

theTSNE = Rtsne(dfu[,c(2,3,5,7,13,17,19)], dims = 2, perplexity = 100, versobe = TRUE, max_iter = 500)

dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]

colors <- c("#00AFBB","#E7B800")
colors <- c("red","black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot(dfu$TS1, dfu$TS2, col=colors, pch = 16)
legend("bottomright", c("defaults","non-defaults"), col=c("black","red"), bty="y", lty=1)

##Model with perplexity = 10
theTSNE = Rtsne(dfu[,c(2,3,5,7,13,17,19)], dims = 2, perplexity = 10, versobe = TRUE, max_iter = 500)

dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]

colors <- c("#00AFBB","#E7B800")
colors <- c("red","black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot(dfu$TS1, dfu$TS2, col=colors, pch = 16)
legend("bottomright", c("defaults","non-defaults"), col=c("black","red"), bty="y", lty=1)
head(dfu)


#RandomForest Model
p = paste(colnames(dfu)[c(2,3,5,7,13,17,19)], collapse = "+")
F1 = as.formula(paste0("TS1 ~", p))
F2 = as.formula(paste0("TS2 ~", p))

print(F1)
print(F2)

ts1_model_rf = randomForest(data=dfu, F1, ntree=500, importance = TRUE)
ts2_model_rf = randomForest(data=dfu, F2, ntree=500, importance = TRUE)

df_tsne = df

df_tsne$TS1M_RF = predict(ts1_model_rf, df_tsne)
df_tsne$TS2M_RF = predict(ts2_model_rf, df_tsne)

head(df_tsne)

colors <- c("#00AFBB","#E7B800")
colors <- c("red","black")
colors <- colors[df_tsne$TARGET_BAD_FLAG + 1]
plot(df_tsne$TS1M_RF, df_tsne$TS2M_RF, col=colors, pch = 16)
legend("bottomright", c("defaults","non-defaults"), col=c("black","red"), bty="y", lty=1)


df_tsne$RAND1 = sample(100, size = nrow(df_tsne), replace = TRUE)
df_tsne$RAND2 = sample(100, size = nrow(df_tsne), replace = TRUE)


df_tsne0 = df_tsne[which(df_tsne$TARGET_BAD_FLAG == 0), ]
df_tsne1 = df_tsne[which(df_tsne$TARGET_BAD_FLAG == 1), ]

df_tsne0 = df_tsne0[df_tsne$RAND1 < 25, ]
df_tsne1 = df_tsne1[df_tsne$RAND1 < 75, ]


df_tsnex = rbind(df_tsne0, df_tsne1)
df_tsnex = df_tsnex[df_tsnex$RAND2 < 15, ]

head(df_tsne)

colors <- c("#00AFBB","#E7B800")
colors <- c("red","black")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot(df_tsnex$TS1M_RF, df_tsnex$TS2M_RF, col=colors, pch = 16)
legend("bottomright", c("defaults","non-defaults"), col=c("black","red"), bty="y", lty=1)



#STEP 4: Tree and Regression Analysis on the Original Data

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample(c(TRUE ,FALSE), nrow(df_flag), replace = TRUE, prob = c(0.7,0.3))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

dim(df_flag)
dim(df_train)
dim(df_test)

#DECISION TREE
tr_set = rpart.control(maxdepth = 10)
tr_model = rpart(data = df_train, TARGET_BAD_FLAG~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.Job.Mgr+FLAG.Job.Office+FLAG.Job.Other+FLAG.Job.ProfExe+FLAG.Job.Sales+FLAG.Job.Self+FLAG.Reason.DebtCon+FLAG.Reason.HomeImp, 
                 control = tr_set, method = "class", parms = list(split='information'))
rpart.plot(tr_model)
tr_model$variable.importance

pt = predict(tr_model, df_test, type="prob")
head(pt)
pt2 = prediction(pt[,2], df_test$TARGET_BAD_FLAG)
pt3 = performance(pt2, "tpr","fpr")


#LOGISTIC REGRESSION MODEL
theUpper_LR = glm(TARGET_BAD_FLAG~ LOAN+IMP_MORTDUE+IMP_VALUE+IMP_YOJ+IMP_DEROG+IMP_DELINQ+IMP_CLAGE+IMP_NINQ+IMP_CLNO+IMP_DEBTINC+FLAG.Job.Mgr+FLAG.Job.Office+FLAG.Job.Other+FLAG.Job.ProfExe+FLAG.Job.Sales+FLAG.Job.Self+FLAG.Reason.DebtCon+FLAG.Reason.HomeImp,
                  family = "binomial", data = df_train)
theLower_LR = glm(TARGET_BAD_FLAG~ 1, family = "binomial", data = df_train)

summary(theUpper_LR)
summary(theLower_LR)

#LOGISTIC REGRESSION MODEL USING BACKWARD VARIABLES SELECTION
lrB_model = stepAIC(theUpper_LR, direction = "backward", scope = list(lower = theLower_LR, upper= theUpper_LR))
summary(lrB_model)


plrb = predict(lrB_model, df_test, type = "response")
plrb2 = prediction(plrb, df_test$TARGET_BAD_FLAG)
plrb3 = performance(plrb2, "tpr","fpr")


plot(pt3, col = "green")
plot(plrb3, col = "pink", add=T)
abline(0,1, lty=2)
legend("bottomright", c("DECISION TREE","LOGISTIC REGRESSION BWD"), col = c("green","pink"), bty = "y", lty=1)

aucT = performance(pt2, "auc")@y.values
aucLRB = performance(plrb2, "auc")@y.values
print(paste("TREE AUC =", aucT))
print(paste("LOGISTIC REGRESSION BWD AUC =", aucLRB))


#STEP 5: Tree and Regression Analysis on PCA/tSNE Data

head(df)
df1 = df

#Principal Component Values from Step 2:
df1$PC1 = df_new[,"PC1"]
df1$PC2 = df_new[,"PC2"]
df1$PC3 = df_new[,"PC3"]

#Principal Component Values from Step 3:
df1$TS1M = df_tsne[,"TS1M_RF"]
df1$TS2M = df_tsne[,"TS2M_RF"]

#Removing cont variables 
df1$TARGET_LOSS_AMT = NULL
df1$LOAN = NULL
df1$IMP_MORTDUE = NULL
df1$IMP_VALUE = NULL
df1$IMP_YOJ = NULL
df1$IMP_DELINQ = NULL
df1$IMP_CLAGE = NULL
df1$IMP_CLNO = NULL
df1$IMP_DEBTINC = NULL
df1$M_MORTDUE = NULL
df1$M_VALUE = NULL
df1$M_YOJ = NULL
df1$M_DEROG = NULL
df1$M_DELINQ = NULL
df1$M_CLAGE = NULL
df1$M_CLAGE = NULL
df1$M_NINQ = NULL
df1$M_CLNO = NULL
df1$M_DEBTINC = NULL
df1$IMP_DEROG = NULL
df1$IMP_NINQ =NULL

summary(df1)
head(df1)

#DECISION TREE

FLAG = sample(c(TRUE ,FALSE), nrow(df1), replace = TRUE, prob = c(0.7,0.3))
df_train = df1[FLAG, ]
df_test = df1[! FLAG, ]

dim(df1)
dim(df_train)
dim(df_test)

tr_set = rpart.control(maxdepth = 10)
tr_model = rpart(data = df_train, TARGET_BAD_FLAG~ ., 
                 control = tr_set, method = "class", parms = list(split='information'))
rpart.plot(tr_model)
tr_model$variable.importance

pt = predict(tr_model, df_test, type="prob")
head(pt)
pt2 = prediction(pt[,2], df_test$TARGET_BAD_FLAG)
pt3 = performance(pt2, "tpr","fpr")


#LOGISTIC REGRESSION MODEL
theUpper_LR = glm(TARGET_BAD_FLAG~ .,
                  family = "binomial", data = df_train)
theLower_LR = glm(TARGET_BAD_FLAG~ 1, family = "binomial", data = df_train)

summary(theUpper_LR)
summary(theLower_LR)

#LOGISTIC REGRESSION MODEL USING BACKWARD VARIABLES SELECTION
lrB_model = stepAIC(theUpper_LR, direction = "backward", scope = list(lower = theLower_LR, upper= theUpper_LR))
summary(lrB_model)


plrb = predict(lrB_model, df_test, type = "response")
plrb2 = prediction(plrb, df_test$TARGET_BAD_FLAG)
plrb3 = performance(plrb2, "tpr","fpr")


plot(pt3, col = "green")
plot(plrb3, col = "pink", add=T)
abline(0,1, lty=2)
legend("bottomright", c("DECISION TREE","LOGISTIC REGRESSION BWD"), col = c("green","pink"), bty = "y", lty=1)

aucT = performance(pt2, "auc")@y.values
aucLRB = performance(plrb2, "auc")@y.values
print(paste("TREE AUC =", aucT))
print(paste("LOGISTIC REGRESSION BWD AUC =", aucLRB))

