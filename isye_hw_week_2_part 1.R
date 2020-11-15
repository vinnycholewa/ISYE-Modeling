#QUESTION 3.1 
#model setup
library(kknn)
library(ggplot2)
library(caret)
library(caTools)
set.seed(25)
#loading data & checking data set
df_credit=read.table("credit_card_data-headers.txt",header = T,sep='\t')
head(df_credit)
summary(df_credit)

#KNN Model ()
check_accuracy = function(X){
        predicted = rep(0,(nrow(df_credit))) # predictions: start with a vector of all zeros
        
        # for each row, estimate its response based on the other rows
        
        for (i in 1:nrow(df_credit)){
                
                # data[-i] means we remove row i of the data when finding nearest neighbors...
                #...otherwise, it'll be its own nearest neighbor!
                
                kknn_model=kknn(R1 ~.,df_credit[-i,],df_credit[i,],k=X, scale = TRUE) # use scaled data
                kknn_val = kknn
                # record whether the prediction is at least 0.5 (round to one) or less than 0.5 (round to zero)
                
                predicted[i] = as.integer(fitted(kknn_model)+0.5) # round off to 0 or 1
        }
        
        # calculate fraction of correct predictions
        
        acc = sum(predicted == df_credit[,11]) / nrow(df_credit)
        return(acc)
}



#
# Now call the function for values of k from 1 to 20 (you could try higher values of k too)
#
# test knn with X neighbors
accuracy =rep(0,50)
for (X in 1:50){
        accuracy[X] = check_accuracy(X)
}
accuracy #max accuracy achieved at k = 12, 85%
plot(accuracy, xlab = "Accuracy Rate", ylab = "K", main = "KNN Accuracy")
max(accuracy)
which.max(accuracy)

#data_rows = nrow(df_credit)
#credit_sample = sample(1:data_rows, size = round(data_rows/3), replace = FALSE)
#data_train = df_credit[-credit_sample]
#data_test = df_credit[credit_sample]

#xval = train.kknn(R1~., data = data_train, kmax=100, 
#                  kernel = "optimal", scale = TRUE)
#xval


#KNN Cross Validation using library(caret)
intrain = createDataPartition(df_credit$R1, p = .80, list = FALSE) #80/20 split
training = df_credit[intrain,] #training set
testing = df_credit[-intrain,] #testing set
dim(training); dim(testing) #check dimensions of data

set.seed(21)
trctrl = trainControl(method = "repeatedcv", number = 10, repeats =5)
knn_fit = train(factor(R1)~., data = training, method = "knn",
                trControl = trctrl, 
                preProcess = c("center", "scale"),
                metric = "Accuracy",
                tuneLength = 50)

knn_fit #optimal K value is 47 with accuracy rate of 
plot(knn_fit)

test_pred = predict(knn_fit, newdata = testing)
test_pred
pred_accuracy = table(test_pred,testing$R1)
pred_accuracy
confusionMatrix(test_pred, factor(testing$R1))
sum(test_pred==testing$R1)/length(testing$R1)








