#Load packages and run libraries
##not sure to use randomForest or rpart so will load both
Packages = c("tree", "randomForest", "rpart", "dplyr", "rsample", "rpart.plot", "ipred", "caret")
lapply(Packages, library, character.only = TRUE)

#load data as crime 
crime = read.table('uscrime.txt', header = TRUE)
head(crime)

######################### 10.1a Regression Tree Model ########################
##partition the data into train/test

set.seed(22)
crime_split = initial_split(crime, prob= .7)
crime_train = training(crime_split)
crime_test = testing(crime_split)
##sanity check to check data split
head(crime_train)
head(crime_test)

#Breiman CART Approach
##partition data set into smaller subgroups and then fit a simple constant 
##for each observation in the group. This RECURSIVE PARTITIONING
##the model looks to minimize the overall sums of squares. The root of the tree
#will be the smallest sum of residuals 

model1 = tree(Crime~., data = crime, method = "anova")
summary(model1)
model1$frame
#Regression tree:
#        tree(formula = Crime ~ ., data = crime)
#Variables actually used in tree construction:
#        [1] "Po1" "Pop" "LF"  "NW" 
#Number of terminal nodes:  7 
#Residual mean deviance:  47390 = 1896000 / 40 
#Distribution of residuals:
#        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-573.900  -98.300   -1.545    0.000  110.600  490.100 

##partitioning of varaibles are done in a top-down, greedy fashion. 
##This means that the prior partitioning will not change based on later partiitons

#plot tree (only four variables used)
plot(model1)
text(model1)
print(model1, digits = 2)

#Technically, for regression modeling, the split cutoff is defined so that the 
#residual sum of squared error (RSS) is minimized across the training samples 
#that fall within the subpartition.

##RSS = sum((Observeds - Predicteds)^2

######Partition methods
#Gini = sum(p(1-p)) and 
#entropy = -1*sum(p*log(p)), 
#where p is the proportion of misclassified observations within the subpartition.
##Result will be between 0 (greatest purity) and 1 (maximum degree of impurity)


#########Pruning#########
#goal of pruning is to see if a smaller subtree can give us comparable results to 
#fully grown tree
##rpart package imposes a penalty to the tree having too many splits 
##called COMPLEXITY PARAMETER (cp). the higher the cp, the smaller the tree 
##This is very similar to lasso norm penalty. As a tree grows larger the reduction
##in the SSE must be greater than the cost complexity penalty. 

term_nodes1 = 3
term_nodes2 = 5
term_nodes3 = 7

prune_data1 = prune.tree(model1, best = term_nodes1)
plot(prune_data1) 
text(prune_data1)
title("Terminal Nodes: 3")
prune_data2 = prune.tree(model1, best = term_nodes2)
plot(prune_data2)
text(prune_data2)
title("Terminal Nodes: 5")
prune_data3 = prune.tree(model1, best = term_nodes3)
plot(prune_data3)
text(prune_data3)
title("Terminal Nodes: 7")


#cross validation
cv.data = cv.tree(model1)
plot(cv.data$size, cv.data$dev, type = "b", xlab = "Terminal Nodes", ylab = 'Deviance of Trees')
##Looking at this plot it appears 4 terminal nodes is optimal 

yhat = predict(model1)
yhat
SSres = sum((yhat-data$Crime)^2)
SSres
plot(crime$Crime, yhat)


SStot = sum((crime$Crime - mean(crime$Crime))^2)
R2 = 1 - SSres/SStot
R2 #### 0.7244962


model2 = rpart(Crime~., data = crime)
summary(model2)
rpart.plot(model2)
plotcp(model2)


yhat_2 = predict(model2)
SSres_2 = sum((yhat_2-crime$Crime)^2)
SStot_2 = sum((data$Crime - mean(data$Crime))^2)
R2_2 <- 1 - SSres_2/SStot_2
R2_2 ####0.5628378







