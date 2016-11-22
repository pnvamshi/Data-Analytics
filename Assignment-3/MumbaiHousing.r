# P.N. Vamshi (13EC10044), Manognya Deepthi G (13IM10008), Nikhil Kashyap (13EE10033), Mukesh Sahani (13EC10039), Manoj Meena (13EE10028)
# Data Analysis (ASSIGNMENT 3)
# Group: DA-04
# Mumbai Housing Prediction

#loading xlsx library to read xlsx file
library(xlsx)
df <- read.xlsx("Mumbai_Housing.xlsx", sheetIndex = 1)
head(df)
names(df)
dim(df)

#splitting data into train data = tdata and test data = vdata
ind <- sample(2, nrow(df), replace = T, prob = c(0.8, 0.2))
tdata <- df[ind == 1,]
vdata <- df[ind == 2,]

# applying multiple linear regression on whole variable
lm.fit <- lm(MEDV ~ ., data = tdata)

#summary of multiple linear regression model
summary(lm.fit)
#beta is the coef of fitted model
beta <- coef(lm.fit)

# calculating r squared
# adding extra column of value 1 and saving in X
X = cbind(b = 1, vdata)
#remove target variable
X = X[, 0:14]
# y = target variable
y = vdata$MEDV
Syy = t(y)%*%y
SSRes=Syy-t(beta)%*%t(X)%*%y

# predict
#y.predict = predict(lm.fit, vdata)

TSS <- sum((vdata$MEDV - mean(vdata$MEDV))^2)
#RSS <- sum(vdata$MEDV - y.predict)^2

RSq=1-SSRes/TSS
print(RSq)

# correlation matrix of data frame using pearson method
cor(df, method = "pearson")

# removing corelated feature
res <- lm(MEDV ~ .-RAD, data = tdata)

beta <- coef(res)

# rough work
vdata$RAD <- NULL

X = cbind(b = 1, vdata)
X = X[, 0:13]
y = vdata$MEDV
Syy = t(y)%*%y
SSRes=Syy-t(beta)%*%t(X)%*%y

# predict
#y.predict = predict(lm.fit, vdata)

TSS <- sum((vdata$MEDV - mean(vdata$MEDV))^2)
#RSS <- sum(vdata$MEDV - y.predict)^2

RSq=1-SSRes/TSS
print(RSq)
