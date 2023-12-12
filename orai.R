####################################################################################################
# Machine Learning in Econometrics Lecture 8-9 Practicing Code (For Registered Students Only) 
# Based on textbook: 'An Introduction to Statistical Learning with Applications in R'
# Edited by: Chaoyi Chen (NJE & MNB)
# Tree based method
####################################################################################################
library(tree) # The tree library is used to construct classification and regression trees
library(ISLR2)

##################### Regression Trees ####################
# we fit a regression tree to the Boston data set
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2) # training sample
tree.boston <- tree(medv ~ ., Boston , subset = train) # fit the tree to the training sample
summary(tree.boston)

# show tree structure
plot(tree.boston)
text(tree.boston , pretty = 0)

# use the cv.tree() function to see whether pruning the tree will improve performance.
cv.boston <- cv.tree(tree.boston) # Runs a K-fold cross-validation experiment
plot(cv.boston$size , cv.boston$dev, type = "b")

# prune the tree
prune.boston <- prune.tree(tree.boston , best = 5)
plot(prune.boston)
text(prune.boston , pretty = 0)

# In keeping with the cross-validation results, we use the unpruned tree to make predictions on the test set.
yhat <- predict(tree.boston , newdata = Boston[-train , ])
boston.test <- Boston[-train, "medv"]
plot(yhat , boston.test)
abline(0, 1)
mean((yhat - boston.test)^2)

# Comments: the test set MSE associated with the regression tree is 35.29.
# The square root of the MSE is therefore around 5.941, indicating that this
# model leads to test predictions that are (on average) within approximately
# $5,941 of the true median home value for the census tract.

##################### Bagging ####################
library(randomForest) # perform both random forests and bagging
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 12, importance = TRUE)
# mtry = 12 indicates that all 12 predictors should be considered for each split of the tree

# How well does this bagged model perform on the test set?
yhat.bag <- predict(bag.boston , newdata = Boston[-train , ])
plot(yhat.bag , boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2)

# Comments: The test set MSE associated with the bagged regression tree is 23.42, about
# two-thirds of that obtained using an optimally-pruned single tree.

# Manually change the number of trees grown by randomForest() using the ntree argument
bag.boston <- randomForest(medv ~ ., data = Boston , subset = train, mtry = 12, ntree = 25)
yhat.bag <- predict(bag.boston , newdata = Boston[-train , ])
mean((yhat.bag - boston.test)^2)


##################### Random Forest ####################
# By default, randomForest() uses p/3 variables when building a random forest of regression trees
# The random forest function inputs are the same as bagging, the difference is the called output
# We use mtry = 6.
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train , mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2)

# Comments: The test set MSE is 20.07; this indicates that random forests yielded an
# improvement over bagging in this case.

# Using the importance() function, we can view the importance of each variable.
importance(rf.boston) # Table of two measures of variable importance. The first is based upon the mean decrease of accuracy in predictions on the out of bag samples when a given variable is permuted. The second is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees
varImpPlot(rf.boston) # Plots of these importance measures

##################### Boosting ####################
# we use the gbm package, and within it the gbm() function, to fit boosted regression trees to the Boston data set.
library(gbm)
set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train , ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)
plot(boost.boston , i = "rm")
plot(boost.boston , i = "lstat")
yhat.boost <- predict(boost.boston , newdata = Boston[-train , ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

# Comments: The test MSE obtained is 18.39: this is superior to the test MSE of random
# forests and bagging.

boost.boston <- gbm(medv ~ ., data = Boston[train , ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.boston , newdata = Boston[-train , ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

# In this case, using lambda = 0.2 leads to a lower test MSE than lambda = 0.001.

