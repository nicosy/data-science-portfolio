# Let's see if we can make it third time's a charm with the final variety of wine...

# READ DATA

# set working directory and import data
setwd("C:/Users/Nick/Desktop/Data Analysis/Wine")
Wine <- read.csv("C:/Users/Nick/Desktop/Data Analysis/Wine/Wine.csv", header=FALSE)

# identify the variables
names(Wine) <- c("Type", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "ColorIntensity", "Hue", "OD280/OD315 of diluted wines", "Proline")

# take a look at the target variable
table(Wine$Type)

# categorise wines are either "variety 3 or not"
Wine$Variety3[Wine$Type == 3] <- 1
Wine$Variety3[Wine$Type != 3] <- 0

# see if previous predictor variables could also be good indicators of Variety 2
p <- ggplot(Wine, aes(Alcohol, Variety3)) + geom_point()
p
p <- ggplot(Wine, aes(Flavanoids, Variety3)) + geom_point()
p
p <- ggplot(Wine, aes(Proline, Variety3)) + geom_point()
p
p <- ggplot(Wine, aes(ColorIntensity, Variety3)) + geom_point()
p

a <- ggplot(Wine, aes(Flavanoids)) + geom_histogram() + facet_grid(.~Variety3)
a
a <- ggplot(Wine, aes(Alcohol)) + geom_histogram() + facet_grid(.~Variety3)
a
a <- ggplot(Wine, aes(Proline)) + geom_histogram() + facet_grid(.~Variety3)
a
a <- ggplot(Wine, aes(ColorIntensity)) + geom_histogram() + facet_grid(.~Variety3)
a
# Flavanoids definitely, Proline & Color Intensity partially, Alcohol doesn't seem to be important for our final variety

# let's create a matrix of scatterplots to see which other variables may be in play
pairs(Wine)
# Looks like there's a correlation with Hue and the wonderfully named OD280/OD315 of diluted wines

# MODEL FITTING

# split data into training and testing
ind <- sample(2, nrow(Wine), replace=TRUE, prob = c(0.5,0.5))
train <- Wine[ind==1,]
test <- Wine[ind==2,]

# remove Type variable
train$Type <- NULL
test$Type <- NULL

# SVM - fit to all variables
model_svm <- svm(Variety3 ~ ., train)
pred <- predict(model_svm, test)
# calculate model numerical error/accuracy (std deviation)
error <- test$Variety3 - pred
svm_error <- sqrt(mean(error^2)) # = 0.13332 
# convert to class and assess accuracy
pred_class <- pred
pred_class[pred_class >= 0.5] <- 1
pred_class[pred_class < 0.5] <- 0
error <- test$Variety3 - pred_class
summary(error)
# Min. 1st Qu.  Median  Mean    3rd Qu.    Max. 
#  0       0       0     0        0         0 
# perfect score first time
# so SVM's are well suited to identifying a single wine, let's see what happens in a multivariate test when all three wine varieties need to be categorised

