# P.N. Vamshi (13EC10044), Manognya Deepthi G (13IM10008), Nikhil Kashyap (13EE10033), Mukesh Sahani (13EC10039), Manoj Meena (13EE10028)
# Data Analysis (ASSIGNMENT 4)
# Group: DA-04
# Customer Churn Data

# Clearing the Environment
rm(list = ls())

# Libraries needed to run the code
library(caret)
library(rpart)
library(C50)
library(rattle)
library(party)
library(partykit)
library(randomForest)
library(ROCR)
library(ggplot2)
library(reshape2)
library(car)
library(corrplot)
library(e1071)
library(SDMTools)

# Reading the data to dataframe
table_data <- read.csv("Customer Churn Data.csv")

# Changing data to numeric form for analysis
table_data$churn <- as.integer(table_data$churn)
table_data$international_plan <- as.integer(table_data$international_plan)
table_data$voice_mail_plan <- as.integer(table_data$voice_mail_plan)

# Correcting integer values, converting values in 0's(false) and 1's(true)
table_data$churn[table_data$churn == "1"] <- 0
table_data$churn[table_data$churn == "2"] <- 1

table_data$international_plan[table_data$international_plan == "1"] <- 0
table_data$international_plan[table_data$international_plan == "2"] <- 1

table_data$voice_mail_plan[table_data$voice_mail_plan == "1"] <- 0
table_data$voice_mail_plan[table_data$voice_mail_plan == "2"] <- 1

# Removing unnecessary attributes
table_data$Id <- NULL
table_data$state <- NULL
table_data$area_code <- NULL
table_data$phone_number <- NULL

# Taking care of missing values
na.omit(table_data)

# Analysis of the Data
# Summary method to fetch the summary of table_data
summary(table_data)

# Applying standard deviation on table_data
sapply(table_data, sd)

# Finding the Correlation matrix
correlation_matrix <- round(cor(table_data), digits = 2)
correlation_matrix
# Looking at correlation matrix we can see that voice_mail_plan and number_vmail_messages are highly correlated by factor of 0.95

#heatmap of Correlation using ggplot2
#ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) + geom_raster()
# Get lower triangle of the correlation matrix
get_lower_tri<-function(correlation_matrix){
  correlation_matrix[upper.tri(correlation_matrix)] <- NA
  return(correlation_matrix)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(correlation_matrix){
  correlation_matrix[lower.tri(correlation_matrix)]<- NA
  return(correlation_matrix)
}
#usage
upper_tri <- get_upper_tri(correlation_matrix)
#upper_tri

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed()

# Building the Model
# Divding data set for training(70%) and testing(30%) maintaing the proportion
#set.seed(1234)
ind <- sample(2, nrow(table_data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- table_data[ind == 1, ]
testingData <- table_data[ind == 2, ]

# Using Naive Bayes Classifier model
naiveBayesModel <- naiveBayes(as.factor(churn) ~ ., data = trainData)
#predict(naiveBayesModel, testingData[1:10, -1], type = "raw") 
#print(naiveBayesModel)
summary(naiveBayesModel)

# Using SVM model
svmModel <- svm(as.factor(churn) ~ ., data = trainData, gamma = 0.1, cost = 15)
#print(svmModel)
summary(svmModel)

# Using Decision tree C5.0
#trainData$churn <- as.factor(trainData$churn)
c50Model <- C5.0(as.factor(churn) ~ ., data = trainData)
summary(c50Model)

# Predicting the values 
naiveResult <- predict(naiveBayesModel, testingData, type = c("class"))
svmResult <- predict(svmModel, testingData, type = c("class"))
c50Result <- predict(c50Model, testingData, type = c("class"))

# Creating new column in test data set for storing predicted values
testingData$yhat1 <- naiveResult
testingData$yhat2 <- svmResult
testingData$yhat3 <- c50Result

# Contructing the ROC curve
predict_var1 <- prediction(as.numeric(testingData$yhat1), as.numeric(testingData$churn))
predict_var2 <- prediction(as.numeric(testingData$yhat2), as.numeric(testingData$churn))
predict_var3 <- prediction(as.numeric(testingData$yhat3), as.numeric(testingData$churn))

perform1 <- performance(predict_var1, "tpr", "fpr")
perform2 <- performance(predict_var2, "tpr", "fpr")
perform3 <- performance(predict_var3, "tpr", "fpr")


# Confusion matrix
cm1 <- confusion.matrix(naiveResult, testingData$churn, threshold = 0.5)
cm2 <- confusion.matrix(svmResult, testingData$churn, threshold = 0.5)
cm3 <- confusion.matrix(c50Result, testingData$churn, threshold = 0.5)

N <- nrow(testingData)
# Confusion matrix for Naive Bayes Classifier model
# Calculating accuracy
TN <- cm1[1,1]
FN <- cm1[1,2]
FP <- cm1[2,1]
TP <- cm1[2,2]
# Accuracy
acc <- (TN+FP)/N
# Precision
ppv <- TP/(TP+FP)
# Recall
rec <- TP/(TP+FN)
fpr <- FP/(FP+TN)
# Print confusion matrix
print("Naive Bayes Classifier model: ")
print(cm1)
cat("Accuracy: ",acc)
cat("Precision: ",ppv)
cat("Recall: ",rec)
cat("fpr = ", fpr)

# Confusion matrix for SVM model
# Calculating accuracy
TN <- cm2[1,1]
FN <- cm2[1,2]
FP <- cm2[2,1]
TP <- cm2[2,2]
# Accuracy
acc <- (TN+FP)/N
# Precision
ppv <- TP/(TP+FP)
# Recall
rec <- TP/(TP+FN)
#rec = tpr
fpr <- FP/(FP+TN)
#display confusion matrix
print("SVM model: ")
print(cm2)
cat("Accuracy: ",acc)
cat("Precision: ",ppv)
cat("Recall: ",rec)
cat("fpr = ", fpr)

# Confusion matrix for C5.0
# Calculating accuracy
TN <- cm3[1,1]
FN <- cm3[1,2]
FP <- cm3[2,1]
TP <- cm3[2,2]
# Accuracy
acc <- (TN+FP)/N
# Precision
ppv <- TP/(TP+FP)
# Recall
rec <- TP/(TP+FN)
fpr <- FP/(FP+TN)
# Print confusion matrix
print("C5.0 Decision Tree: ")
print(cm3)
cat("Accuracy: ",acc)
cat("Precision: ",ppv)
cat("Recall: ",rec)
cat("fpr = ", fpr)

# Drawing ROC Curve
plot(perform1, col = "green", lwd = 2.5, main = "ROC Curve")
plot(perform2, add = TRUE, col = "blue", lwd = 2.5)
plot(perform3, add = TRUE, col = "orange", lwd = 2.5)
abline(0,1,col = "red", lwd = 2.5, lty = 2)

# Conclusion
# C5.0 is close to perfect classifer so select model3
table_data$voice_mail_plan <- table_data$voice_mail_plan - table_data$number_vmail_messages
table_data$number_vmail_messages <- NULL
table_data$total_day_charge <- NULL
table_data$total_eve_charge <- NULL
table_data$total_night_charge <- NULL
table_data$total_intl_charge <- NULL
#table_data$number_vmail_messages <- NULL

temp <- table_data$churn
table_data <- as.data.frame(scale(table_data, center = TRUE, scale = TRUE))
table_data$churn <- temp
#splitting of data set into training(70%) and testing(30%)

ind <- sample(2, nrow(table_data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- table_data[ind == 1, ]
testingData <- table_data[ind == 2, ]
c50Model <- C5.0(as.factor(churn) ~ ., data = trainData, trials = 25)
c50Result <- predict(c50Model, testingData, type = c("class"))
cm3 <- confusion.matrix(c50Result, testingData$churn, threshold = 0.5)
# Calculating Accuracy
TN <- cm3[1,1]
FN <- cm3[1,2]
FP <- cm3[2,1]
TP <- cm3[2,2]
# Accuracy
acc <- (TN+FP)/N
# Precision
ppv <- TP/(TP+FP)
# Recall
rec <- TP/(TP+FN)
# Print confusion matrix
print("C5.0 Decision Tree: ")
print(cm3)
cat("Accuracy: ",acc)
cat("Precision: ",ppv)
cat("Recall: ",rec) 