# The final test is to see if our classifiers can correctly predict the wine varieties when all 3 are on offer, i.e. a multivariate test
# READ DATA

# set working directory and import data
setwd("C:/Users/Nick/Desktop/Data Analysis/Wine")
Wine <- read.csv("C:/Users/Nick/Desktop/Data Analysis/Wine/Wine.csv", header=FALSE)

# identify the variables
names(Wine) <- c("Type", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")

# EXPLORATORY DATA ANALYSIS
# we know from previous tests the key variables seem to be a different mixture of Alcohol, Flavanoids, Proline, Color Intensity, Hue and OD280/OD315 of diluted wines
# with that in mind I expect that a SVM may not perform as well as a decision tree or conditional forest this time, but let's see...

# Model Fitting

# split data into training and testing
ind <- sample(2, nrow(Wine), replace=TRUE, prob = c(0.5,0.5))
train <- Wine[ind==1,]
test <- Wine[ind==2,]

# SVM - fit to all variables
model_svm <- svm(Type ~ ., train)
pred <- predict(model_svm, test)
# calculate model numerical error/accuracy (std deviation)
error <- test$Type - pred
svm_error <- sqrt(mean(error^2)) # = 0.18026...not too bad
# convert to class and assess accuracy
pred_class <- pred
pred_class[pred_class < 1.5] <- 1
pred_class[pred_class >= 1.5 & pred_class < 2.5] <- 2
pred_class[pred_class >= 2.5] <- 3
prop.table(table(test$Type, pred_class))
#        1          2          3
# 1 0.27906977 0.00000000 0.00000000
# 2 0.00000000 0.43023256 0.00000000
# 3 0.00000000 0.02325581 0.26744186
# wines 131 and 135 have been falsely predicted as Variety 2 when in fact they were Variety 3
# looking at the prediction they were both on the borderline value of being a Variety 3
# borderline = 2.5
# wine 131 = 2.44
# wine 135 = 2.47
# if I had set the borderline for Variety 3 to 2.4 SVM would've had a perfect score

# Decision Tree
library(rpart)
model_dtree <- rpart(Type ~ ., data=train, method="class")
plot(model_dtree)
text(model_dtree)
# make prediction
pred_dtree <- predict(model_dtree, test, type="class")
#calculate the proportion of errors
prop.table(table(test$Type, pred_dtree))
#        1          2          3
# 1 0.25581395 0.02325581 0.00000000
# 2 0.03488372 0.38372093 0.01162791
# 3 0.00000000 0.10465116 0.18604651
# quite a few errors in there, I'm not surprised when the third node was Alcalinity of ash, a variable that has shown no importance before!
# looks like we dtree's weighting on Alcohol led it into a local maxima, let's see if a conditional forest can see the wood through the trees (algorithm humour, groan...)

# Conditional Forest
library(party)
set.seed(415)
model_CF <- cforest(as.factor(Type) ~ ., data=train, controls=cforest_unbiased(ntree=2000,mtry=4))
#make prediction
pred_CF <- predict(model_CF, test, OOB=TRUE, type="response")
# calculate the error proportion
prop.table(table(test$Type, pred_CF))
#        1          2          3
# 1 0.27906977 0.00000000 0.00000000
# 2 0.01162791 0.40697674 0.01162791
# 3 0.00000000 0.00000000 0.29069767
# two errors, same as SVM and better than Decision Tree
# it incorrectly predicted wine 67 was Variety 1 and 69 was Variety 3

# So it seems our best model is SVM with the categorisation borderline reduced to 2.4 for Variety 3
# let's see if this holds when we partition the data into different train/test sets...

# split data into different train and test sets
ind <- sample(2, nrow(Wine), replace=TRUE, prob = c(0.5,0.5))
train <- Wine[ind==1,]
test <- Wine[ind==2,]

# SVM - fit to all variables
model_svm <- svm(Type ~ ., train)
pred <- predict(model_svm, test)
# convert to class and assess accuracy
pred_class <- pred
pred_class[pred_class < 1.5] <- 1
pred_class[pred_class >= 1.5 & pred_class < 2.4] <- 2
pred_class[pred_class >= 2.4] <- 3
prop.table(table(test$Type, pred_class))
#        1          2          3
# 1 0.37037037 0.00000000 0.00000000
# 2 0.00000000 0.33333333 0.02469136
# 3 0.00000000 0.00000000 0.27160494
# SVM incorrectly predicts wines 62 and 71 as Variety 3 when both are Variety 2 
# looking at their numerical predictions poses a problem for SVM: 
# wine 62 (V2) = 2.56
# wine 71 (V2) = 2.5
# wine 131 (V3) = 2.44
# wine 135 (V3) = 2.47
# this blows apart our attempt to fit the model by reducing Variety 3's lower borderline to 2.4!

# this is why I ran the models through new train/test sets
# let's see how conditional forest does on this batch...

# Conditional Forest
set.seed(415)
model_CF <- cforest(as.factor(Type) ~ ., data=train, controls=cforest_unbiased(ntree=2000,mtry=4))
#make prediction
pred_CF <- predict(model_CF, test, OOB=TRUE, type="response")
# calculate the error proportion
prop.table(table(test$Type, pred_CF))
#        1          2          3
# 1 0.37037037 0.00000000 0.00000000
# 2 0.00000000 0.32098765 0.03703704
# 3 0.00000000 0.00000000 0.27160494
# two errors again, this time two false positives on Variety 3: wine 69 again but also wine 61 this time

# to get a conclusive winner would need to repeat this process many times,for the sake of my learning objectives there is  no need 

