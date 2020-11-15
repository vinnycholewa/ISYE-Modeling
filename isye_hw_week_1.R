#Question 2.1: 
#What are the factors that produce a successful sales person? What does that profile look like?
        #predictor 1: tenure in role
        #predictor 2: hours worked per week
        #predictor 3: percent of total communication that is digital
        #predictor 4: percent of total communication that is via phone
        #predictor 5: average engagements per deal
        #predictor 6: engagement number where commercial terms are first discussed
        #predictor 7: batting ratio for closing deals
#Question 2.2:
#load library
install.packages("kernlab")
library(kernlab)
#load data
df = read.delim('~/Downloads/data 2.2/credit_card_data-headers.txt',header = TRUE)
#m = as.matrix(df)
head(df_text, 20)

x_len = nrow(df)
y_len = ncol(df)

#ksvm model
svm = ksvm(as.matrix(df[,1:10]), as.factor(df[,11]), scaled=TRUE, type='C-svc', kernel='vanilladot', C=1000)
a = colSums(svm@xmatrix[[1]]*svm@coef[[1]])
a0 = svm@b
pred = predict(svm,df[,1:10]) #accuracy preduction
sum(pred == df[,11]) / nrow(df)


#failed for loop (bring up at office hours). wanted to plot the C, accuracy for the range of 1 to 1000
#svm_c = c()
#acc_c = c()
#for i in seq(1,1000,2){
 #       ksvm(as.matrix(df[,1:10]), as.factor(df[,11]), scaled=TRUE, type='C-svc', kernel='vanilladot', C=i)
  #      append(svm_c,) }


#c optimization 
library("e1071")
model <- ksvm(df$R1 ~ ., data = df)
tune.out <- tune(model, 
                 df$R1 ~ ., 
                 data = df,
                 ranges = list(epsilon = seq(0,1,0.1), cost = c(1:1000)))

#tuning (albeit it had to use a different library)
tuning_result <- tune(e1071::svm, df$R1 ~ df$A1, data = df, ranges = list(cost = c(10:1000, 10)))
tuning_result

tuning_result <- tune(e1071::svm, df$R1 ~ df$A1:df$A15, 
                      data = df, ranges = list(cost = c(10:1000, 10)))
tuning_result
