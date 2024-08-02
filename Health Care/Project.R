library("readxl")
dfprojsz <- read_excel("C:/Users/zheng/Downloads/ssc_case_study_2_inflammatory_bowel_disease.xlsx", sheet = 1)
varname <- read_excel("C:/Users/zheng/Downloads/ssc_case_study_2_inflammatory_bowel_disease.xlsx", sheet = 2)

# Use paste0() to concatenate columns Gene.Symbol and Probe.Set.ID
varname$var <- paste0(varname$Gene.Symbol, substr(varname$Probe.Set.ID, start = 1, stop = 6))

#transpose the df
df2 <-t(dfprojsz)

#replace the variable name in row 1 to row 2
df2[2,5:313]<-df2[1,5:313]
#drop the row 1
df3<-df2[-1,]
#View(df3)

#Reset index
rownames(df3) <- NULL
df4<-data.frame(df3)

#set the name as header:
colnames(df4) <-df4[1,]
#delete the dup row of the header
df.clean<-df4[-1,]

#Assign the Gene name with Gene ID:
indexvarname <- match(varname$Probe.Set.ID, names(df.clean))
names(df.clean)[indexvarname] <- varname$var
View(df.clean)

# Use ifelse() to  replace values Ulcerative to Ulcerative Colitis.
df.clean$Group <- ifelse(df.clean$Group == "Ulcerative", "Ulcerative Colitis", df.clean$Group)
df.clean$Ethnicity <- ifelse(df.clean$Ethnicity == "cacuasian", "caucasian", df.clean$Ethnicity)
#############################################################################################################
#select all numeric variables by drop first 4 columns
temp <- df.clean[,-c(1:4)]
temp2 <- as.numeric(unlist(temp))
#converting all numeric data into true numeric format.
df.clean[,-c(1:4)] <- as.numeric(unlist(temp))

# plot age distribution
hist(as.numeric(df.clean$Age))

# plot sex and ethnicity
ggplot(df.clean, aes(x = Group, fill = Sex)) + geom_histogram(alpha = 0.5, position = "dodge", bins = 5, stat="count") + scale_fill_manual(values = c("pink", "blue")) +
labs(title = "Histogram of IBS status by Gender", x = "IBS status", y = "Frequency")

ggplot(df.clean, aes(x = Group, fill = Ethnicity)) + geom_histogram(alpha = 0.5, position = "dodge", bins = 5, stat="count") +
labs(title = "Histogram of IBS status by Ethnicity", x = "IBS status", y = "Frequency")
#df.clean$Age <- as.numeric(df.clean$Age)
#by analysis of Age, it not normally distributed with outlier, so we keep treat it as categorical data.  
###################################################
#Select the numeric columns from the given dataset, and factor the response variable
num.cols <- unlist(lapply(df.clean, is.numeric)) 
data.num <- df.clean[ , num.cols] 
df.clean[,1]<-factor(df.clean[,1])




#LASSO:
# Load required packages
library(glmnet)

# One-hot encode categorical predictor variables
encoded_data <- model.matrix(Group~ ., data = df.clean)



totalCVacc<-0
for ( j in 1:10){
# Define the training and testing data
train_index <- sample(nrow(df.clean), 0.8 * nrow(df.clean))
x_train <- encoded_data[train_index, ]
y_train <- as.numeric(df.clean[,1])[train_index]
x_test <- encoded_data[-train_index, ]
y_test <- as.numeric(df.clean[,1])[-train_index]

# Fit LASSO logistic regression
lasso_model <- glmnet(x_train, y_train, family = "multinomial", alpha = 1)

# Select the optimal lambda value using cross-validation
cv_model <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1)

# Get the coefficients for the optimal lambda value
lasso_coef <- coef(cv_model, s = "lambda.min")

# Predict the response variable for the test data

predicted_y <- predict(lasso_model, newx = x_test, s = cv_model$lambda.min, type = "response")

# Calculate the accuracy of the prediction
predicted_class <- ifelse(predicted_y > 0.5, 1, 0)
accuracylasso <- sum(predicted_class == y_test) / length(y_test)
##
totalCVacc <- totalCVacc +accuracylasso

}
averageCVerrors<-totalCVacc/10
averageCVerrors
# Print the accuracy
print(accuracylasso)
cat("LASSO Model Accuracy is:", paste0(100*round(accuracylasso, 3)),"%")





####################
#select all numeric variables by drop first 4 columns
temp <- df.clean[,-c(1:4)]
temp2 <- as.numeric(unlist(temp))
#converting all numeric data into true numeric format.
df.clean[,-c(1:4)] <- as.numeric(unlist(temp))

#

#df.clean$Age <- as.numeric(df.clean$Age)
#by analysis of Age, it not normally distributed with outlier, so we keep treat it as categorical data.  
###################################################
#Select the numeric columns from the given dataset, and factor the response variable
num.cols <- unlist(lapply(df.clean, is.numeric)) 
data.num <- df.clean[ , num.cols] 
df.clean[,1]<-factor(df.clean[,1])

#####Random Forest
#Random Forest:
#library(e1071)

#Split dataset:
n = dim(df.clean)[1]
n.training <- round(n*0.8)
n.testing <- n-n.training


library(randomForest)
rep = 10

err.rf = dim(rep)



set.seed(214603138)
for (k in 1:rep) {
  train = sample(1:n,n.training)
  
  datatrain = df.clean[train,]
  x.train = datatrain[,-1]
  y.train = datatrain[,1]
  datatest = df.clean[-train,]
  x.test = datatest[,-1]
  y.test = datatest[,1]
  
  mtry <- floor(sqrt(ncol(x.train)))

  rf_classifier = randomForest(Group ~.,data=datatrain, ntree=201, mtry=mtry, importance=TRUE)
  table.rf = table(y.test, predict(rf_classifier, datatest))
  err.rf[k] = 1 - (sum(diag(table.rf)))/n.testing
  
}






# Make predictions on test set
predictions <- predict(rf_classifier, datatest)

# Evaluate accuracy 
library(caret)
accuracyrf <- confusionMatrix(predictions, datatest$Group)$overall["Accuracy"]

# Print results
cat("Random Forest Model Accuracy is:", paste0(100*round(accuracyrf, 3)),"%")

merrrf = mean(err.rf)
merrrf



# extract variable importance
var_importance <- importance(rf_classifier)

# plot variable importance
varImpPlot(rf_classifier)


###############################################################################################



encoded_data <- model.matrix(~ Ethnicity + Sex + Age-1, data = df.clean)
IBSstatus <-df.clean[,1]
combined_data <- cbind(IBSstatus,encoded_data, data.num)


library(neuralnet)
nn <- neuralnet(IBSstatus ~ combined_data[,2:ncol(combined_data)],data=combined_data,hidden=c(2,1), linear.output=FALSE, threshold=0.01)



train = sample(1:n,n.training)

datatrain = data.num[train,]
x.train = datatrain[,-1]
y.train = datatrain[,1]
datatest = data.num[-train,]
x.test = datatest[,-1]
y.test = datatest[,1]

datatrain <- df.clean[1:round(126*0.8),]
datatest <- df.clean[]

##LASSO:
library(glmnet)
library(lars)
encoded_data <- model.matrix(~ Ethnicity + Sex + Age-1, data = df.clean)
IBSstatus <-as.numeric(df.clean[,1])
combined_data <- cbind(IBSstatus,encoded_data, data.num)


totalCVacc<-0
for ( j in 1:100){
# Define the training and testing data
train_index <- sample(nrow(combined_data), 0.8 * nrow(combined_data))
x_train <- combined_data[train_index, ]
y_train <- combined_data$IBSstatus[train_index]
x_test <- combined_data[-train_index, ]
y_test <- combined_data$IBSstatus[-train_index]



lasso <-lars(x=as.matrix(combined_data[,2:ncol(combined_data)]),y=as.numeric(IBSstatus),trace=TRUE)   
#Based on the output, we know the sequence of models are: fX6g, fX6;X2g, fX6;X2;X3g,
#fX6;X2;X3;X4g, fX6;X2;X3;X4;X5g, fX6;X2;X3;X4;X5;X1g:
cv.lars(x=as.matrix(combined_data[,2:ncol(combined_data)]),y=as.numeric(IBSstatus))
########plot below:

coef(lasso,s=0.32,mode="fraction")


# Fit LASSO logistic regression
lasso_model <- glmnet(x_train, y_train, family = "multinomial", alpha = 1)

# Select the optimal lambda value using cross-validation
cv_model <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1)

# Get the coefficients for the optimal lambda value
lasso_coef <- coef(cv_model, s = "lambda.min")
# Predict the response variable for the test data

predicted_y <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")

# Calculate the accuracy of the prediction
predicted_class <- ifelse(predicted_y > 0.5, 1, 0)
accuracy <- sum(predicted_class == y_test) / length(y_test)
totalCVacc <- totalCVacc +accuracy

}
averageCVerrors<-totalCVacc/100


# Print the accuracy
print(accuracy)

