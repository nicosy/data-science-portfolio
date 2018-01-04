# READ DATA

# set working directory and import data
setwd("C:/Users/Nick/Desktop/Data Analysis/Wine")
Wine <- read.csv("C:/Users/Nick/Desktop/Data Analysis/Wine/Wine.csv", header=FALSE)

# identify the variables
names(Wine) <- c("Type", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")

# take a look at the target variable
table(Wine$Type)

# categorise wines are either "variety 1 or not"
Wine$Variety1[Wine$Type == 1] <- 1
Wine$Variety1[Wine$Type != 1] <- 0

# EXPLORATORY DATA ANALYSIS

# apparently features 'Alcohol' and 'Flavanoids' are good indicators of Variety 1
# EDA to check these assumptions
p <- ggplot(Wine, aes(Alcohol, Variety1)) + geom_point()
p
p <- ggplot(Wine, aes(Flavanoids, Variety1)) + geom_point()
p

a <- ggplot(Wine, aes(Flavanoids)) + geom_histogram() + facet_grid(.~Variety1)
a
a <- ggplot(Wine, aes(Alcohol)) + geom_histogram() + facet_grid(.~Variety1)
a
# certainly seems that way!

# create a matrix of scatterplots to see which other variables may be correlated to Variety1
pairs(Wine)
# Proline looks like a better candidate than Alcohol and Flavanoids, doube check with histogram...
a <- ggplot(Wine, aes(Proline)) + geom_histogram() + facet_grid(.~Variety1)
a

# MODEL FITTING

# split data into training and testing
ind <- sample(2, nrow(Wine), replace=TRUE, prob = c(0.5,0.5))
train <- Wine[ind==1,]
test <- Wine[ind==2,]

# Support Vector Machine
install.packages("e1071")
library(e1071)
model_svm <- svm(Variety1 ~ Alcohol + Flavanoids + Proline, train)
pred <- predict(model_svm, test)
# calculate model numerical error/accuracy (std deviation)
error <- test$Variety1 - pred
svm_error <- sqrt(mean(error^2)) # = 0.14684
# this is a classification problem ("Variety1 or not?") so convert pred to class and assess accuracy
pred_class <- pred
pred_class[pred_class >= 0.5] <- 1
pred_class[pred_class < 0.5] <- 0
error <- test$Variety1 - pred_class
summary(error)
# Min. 1st Qu.  Median  Mean    3rd Qu.    Max. 
# -1       0       0    -1.25       0       0 
#calculate the proportion of errors
prop.table(table(test$Variety1, pred_class))
#     0      1
# 0 0.6625 0.0125
# 1 0.0000 0.3250
# SVM gave a 1.25% error rate, all false positive

# Decision Tree
library(rpart)
model_dtree <- rpart(Variety1 ~ Alcohol + Flavanoids + Proline, data=train, method="class")
plot(model_dtree)
text(model_dtree)
# doesn't seem to care too much about Alcohol
# make prediction:
pred_dtree <- predict(model_dtree, test, type="class")
#calculate the proportion of errors
prop.table(table(test$Variety1, pred_dtree))
#     0      1
# 0 0.6625 0.0125
# 1 0.0750 0.2500
# decision tree gave a 8.75% error rate 
# 1.25% false positive (same as SVM) + 8.5% false negative 

# wine 74 is the only wine incorrectly predicted by our SVM, it is also the only wine our d_tree incorrectly predicts to be Variety1
# using our d_tree's plot we can see this is because wine 74 is unusually high in both Proline & Flavanoids for a non-Variety1 wine
# although the SVM is the more accurate model, the decision tree enables us to understand why the SVM made its only error

# let's see what affect incorporating all available variables have on our predictions
# will use SVM & d_tree again, plus a conditional forest due to the increased number of variables
# data will be split into indentical train and test sets as before, with the "Type" variable removed otherwise this becomes a little easy!

train$Type <- NULL
test$Type <- NULL

# SVM - fit to all variables
model_svm <- svm(Variety1 ~ ., train)
pred <- predict(model_svm, test)
# calculate model numerical error/accuracy (std deviation)
error <- test$Variety1 - pred
svm_error <- sqrt(mean(error^2)) # = 0.13914, marginally more accurate than before
# convert to class and assess accuracy
pred_class <- pred
pred_class[pred_class >= 0.5] <- 1
pred_class[pred_class < 0.5] <- 0
error <- test$Variety1 - pred_class
summary(error)
# Min. 1st Qu.  Median  Mean    3rd Qu.    Max. 
#  0       0       0     0        0         0 
# by incorporating all variables we have been able to overcome the false positive prediction of wine 74

# Conditional Forest
install.packages("party")
library(party)
set.seed(415)
model_CF <- cforest(as.factor(Variety1) ~ ., data=train, controls=cforest_unbiased(ntree=2000,mtry=4))
#make prediction
pred_CF <- predict(model_CF, test, OOB=TRUE, type="response")
# calculate the error proportion
prop.table(table(test$Variety1, pred_CF))
#      0      1
# 0 0.6625 0.0125
# 1 0.0250 0.3000
# 3.75% is better than a decision tree but falls short of SVM's perfect record when using all available variables

