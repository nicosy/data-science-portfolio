# Can I recreate SVM's perfect predictions with the second variety of wine?

# READ DATA

# set working directory and import data
setwd("C:/Users/Nick/Desktop/Data Analysis/Wine")
Wine <- read.csv("C:/Users/Nick/Desktop/Data Analysis/Wine/Wine.csv", header=FALSE)

# identify the variables
names(Wine) <- c("Type", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "ColorIntensity", "Hue", "OD280/OD315 of diluted wines", "Proline", "Variety2")

# take a look at the target variable
table(Wine$Type)

# categorise wines are either "variety 2 or not"
Wine$Variety2[Wine$Type == 2] <- 1
Wine$Variety2[Wine$Type != 2] <- 0

# EXPLORATORY DATA ANALYSIS

# see if Proline, Alcohol and Flavanoids could also be good indicators of Variety 2
p <- ggplot(Wine, aes(Alcohol, Variety2)) + geom_point()
p
p <- ggplot(Wine, aes(Flavanoids, Variety2)) + geom_point()
p
p <- ggplot(Wine, aes(Proline, Variety2)) + geom_point()
p

a <- ggplot(Wine, aes(Flavanoids)) + geom_histogram() + facet_grid(.~Variety2)
a
a <- ggplot(Wine, aes(Alcohol)) + geom_histogram() + facet_grid(.~Variety2)
a
a <- ggplot(Wine, aes(Proline)) + geom_histogram() + facet_grid(.~Variety2)
a
# Alcohol and Proline seem to be strong indicators, Flavanoids is more confusing 

# create a matrix of scatterplots to see which other variables may be correlated to Variety2
pairs(Wine)
# Color intensity looks like a good indicator for Variety 2, let's take a closer look...
a <- ggplot(Wine, aes(ColorIntensity)) + geom_histogram() + facet_grid(.~Variety2)
a

# MODEL FITTING

# split data into training and testing
ind <- sample(2, nrow(Wine), replace=TRUE, prob = c(0.5,0.5))
train <- Wine[ind==1,]
test <- Wine[ind==2,]

# remove Type variable
train$Type <- NULL
test$Type <- NULL

# SVM - fit to all variables (our reigning champion!)
model_svm <- svm(Variety2 ~ ., train)
pred <- predict(model_svm, test)
# calculate model numerical error/accuracy (std deviation)
error <- test$Variety2 - pred
svm_error <- sqrt(mean(error^2)) # = 0.19033, worse than for Variety 1 
# convert to class and assess accuracy
pred_class <- pred
pred_class[pred_class >= 0.5] <- 1
pred_class[pred_class < 0.5] <- 0
error <- test$Variety2 - pred_class
summary(error)
# Min. 1st Qu.  Median  Mean    3rd Qu.    Max. 
#  0       0       0    0.01163        0         1 
# we have a single false negative prediction (wine 71), so the model seems to fit more than Variety 1
# I'll run a SVM on limited variables and Conditional Forest on all variables to see if either is a better fit here

# SVM
model_svm <- svm(Variety2 ~ Alcohol + Flavanoids + ColorIntensity + Proline, train)
pred <- predict(model_svm, test)
# calculate model numerical error/accuracy (std deviation)
error <- test$Variety2 - pred
svm_error <- sqrt(mean(error^2)) # = 0.17058
# convert to class and assess accuracy
pred_class <- pred
pred_class[pred_class >= 0.5] <- 1
pred_class[pred_class < 0.5] <- 0
error <- test$Variety2 - pred_class
summary(error)
# Min. 1st Qu.  Median  Mean    3rd Qu.    Max. 
#  0       0       0    0.01163        0         1 
# once again, only a single false negative prediction (this time wine 122)

# Conditional Forest
library(party)
set.seed(415)
model_CF <- cforest(as.factor(Variety2) ~ ., data=train, controls=cforest_unbiased(ntree=2000,mtry=4))
#make prediction
pred_CF <- predict(model_CF, test, OOB=TRUE, type="response")
# calculate the error proportion
prop.table(table(test$Variety2, pred_CF))
#      0      1
# 0 0.6046 0.0116
# 1 0.0349 0.3488
# Looks like we have one false positive and two false negatives

# SVM is still the winner (and we now know that Variety 2 is a more intensely colourful wine!)

