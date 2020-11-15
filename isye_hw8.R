Packages <- c("ggplot2", "reshape2", "corrplot", "glmnet")
lapply(Packages, library, character.only = TRUE)


crime = read.table(file = 'uscrime.txt', header = TRUE, sep = "")
head(crime)
#variable	 	Description
#M		percentage of males aged 14–24 in total state population
#So		indicator variable for a southern state
#Ed		mean years of schooling of the population aged 25 years or over
#Po1		per capita expenditure on police protection in 1960
#Po2		per capita expenditure on police protection in 1959
#LF		labour force participation rate of civilian urban males in the age-group 14-24
#M.F		number of males per 100 females
#Pop		state population in 1960 in hundred thousands
#NW		percentage of nonwhites in the population
#U1		unemployment rate of urban males 14–24
#U2		unemployment rate of urban males 35–39
#Wealth		wealth: median value of transferable assets or family income
#Ineq		income inequality: percentage of families earning below half the median income
#Prob		probability of imprisonment: ratio of number of commitments to number of offenses
#Time		average time in months served by offenders in state prisons before their first release
#Crime		crime rate: number of offenses per 100,000 population in 1960

crime_cor= cor(crime)
head(round(crime_cor,2))
corrplot(crime_cor, method = 'shade')
pairs(crime)

#let's have a look at crime to educaiton, wealth, and capital expenditure to police force
qplot(x = crime$Ed, y = crime$Crime, data = crime,
      xlab = 'Education', ylab = 'Crime Per Hundred Thousand', col = 'red')
qplot(x = crime$Wealth, y = crime$Crime, data = crime,
      xlab = 'Wealth', ylab = 'Crime Per Hundred Thousand', col = 'red')
qplot(x = crime$Po1, y = crime$Crime, data = crime,
      xlab = 'Police Protection CapEx', ylab = 'Crime Per Hundred Thousand', 
      col = 'red')
qplot(x = crime$So, y = crime$Crime, geom = "boxplot", data = crime,
      xlab = "Southern State", ylab = 'Crime Per Hundred Thousand', col = 'red')
#lets run a box plot to see if any outliers reveal themselves 
qplot(crime$So, crime$Crime, geom = "boxplot", data = crime, col = 'red')

set.seed(123)
########scale data###########
s_data = as.data.frame(scale(crime[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
scaled_data = cbind(crime[,2],s_data,crime[,16])


###########Stepwise Regression##################
##There are many embedded issues with using stepwise regression. 
##This classical, greedy method fits the model for what's best at t0 without
##consideration of a broader picture. This tends to inflate the R-squared and can have severe problems
##with multicollinearity (as I should above many factors correlate with one another). 

#stepup model by adding only the intercept
step_start =lm(scaled_data$`crime[, 16]` ~ 1, data=scaled_data)
fit_all = lm(scaled_data$`crime[, 16]` ~., data=scaled_data)


sw = step(step_start, direction = "forward", scope = formula(fit_all))
#Step:  AIC=504.79
#Df Sum of Sq     RSS    AIC
#<none>                      1611057 504.79
#+ Wealth        1     59910 1551147 505.00
#+ U1            1     54830 1556227 505.16
#+ Pop           1     51320 1559737 505.26
#+ M.F           1     30945 1580112 505.87
#+ Po2           1     25017 1586040 506.05
#+ `crime[, 2]`  1     17958 1593098 506.26
#+ LF            1     13179 1597878 506.40
#+ Time          1      7159 1603898 506.58
#+ NW            1       359 1610698 506.78
sw = step(step_start, direction = "backward", scope = formula(fit_all))
##AIC is 561.02, this is worse than forward step
sw = step(step_start, direction = "both", scope = formula(fit_all))
##AIC is 504.79 (same as forward)
##includes variables wealth, U1, Pop, M.F, Po2, So, LF, Time, NW
#let's create the model using these factors
sw_mod = lm(scaled_data$`crime[, 16]`~ Po2 + Wealth + U1 + Pop + M.F 
            + Po2 + crime$So + LF + Time + NW, data = scaled_data)
#comparing results of stepwise regression vs starting regression 
summary(fit_all)
summary(sw)
#basic linear regression model has better R-squared (80.31% vs 76.59%) 
#but worse adjusted R-squared (70.78% vs 73.07%)
BIC(fit_all)
BIC(sw)
AIC(sw)

###########Lasso##################
##alpha=1 to call Lasso method

#[Lasso method] - add constraint to standard regression equation so we want to choose coefficients 
#that minimize SSE while adding a constraint that the sum of the coefficients cannot be too large.
#This is essentially giving the regression a budget to use on the coefficients. 
#The largest coefficients will get the majority of that budget whereas 
#insignificant coefficients will get zero making them obsolete. Let's review the coefficients. 
lasso=cv.glmnet(x=as.matrix(scaled_data[,-16]),
                y=as.matrix(scaled_data$`crime[, 16]`),
                alpha=1,
                nfolds = 5,type.measure="mse",
                family="gaussian")
lasso  
plot(lasso)
coef(lasso)
#> coef(lasso)
#16 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept) 896.890242
#crime[, 2]   24.072416
#M            66.662106
#Ed           67.721273
#Po1         304.124300
#Po2           .       
#LF            .       
#M.F          50.349548
#Pop           .       
#NW            3.246832
#U1            .       
#U2           16.915176
#Wealth        .       
#Ineq        142.036136
#Prob        -69.397587
#Time          .     
lasso_mod = lm(scaled_data$`crime[, 16]` ~ crime[,2] + M + Ed + Po1 + M.F + NW
               + U2 + Ineq + Prob, data = scaled_data)
summary(lasso_mod)
##R-squared -> 77.52% and adjusted R-squred -> 72.05% (worse adjusted R-squared)
#let's remove Is South (crime[,2], M.F, NW, and U2 since they are not significant
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  871.394     50.552  17.238  < 2e-16 ***
# crime[, 2]    98.967    119.891   0.825 0.414395    
# M            109.922     48.931   2.246 0.030730 *  
# Ed           197.053     62.019   3.177 0.002998 ** 
# Po1          333.443     48.855   6.825 4.85e-08 ***
# M.F           40.508     38.974   1.039 0.305384    
#NW             4.098     57.400   0.071 0.943465    
#U2            63.300     36.911   1.715 0.094719 .  
#Ineq         237.685     66.071   3.597 0.000935 ***
#Prob        -101.929     39.465  -2.583 0.013892 *  
#        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
new_lasso_mod = lm(scaled_data$`crime[, 16]` ~ M + Ed + Po1 
                   + Ineq + Prob, data = scaled_data)
summary(new_lasso_mod)
##With the exception of removing U2, it's practically the same model is the stepwise reg
##and actually results in a decline.
##Multiple R-squared:  0.7379,	Adjusted R-squared:  0.706 


########### elastic net ##################
#where λ≥0 is a complexity parameter and 0≤α≤1 is a compromise between ridge (α=0) and lasso (α=1).
##-elastic method is able to combine the variable selection benefits of lasso with the predictive 
##benefits of ridge regression but also combines the drawbacks of each too. 

en1=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.05,
              nfolds = 5,type.measure="mse",family="gaussian")
en2=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.10,
              nfolds = 5,type.measure="mse",family="gaussian")
en3=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.20,
              nfolds = 5,type.measure="mse",family="gaussian")
en4=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
                alpha=0.30,
                nfolds = 5,type.measure="mse",family="gaussian")
en5=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.40,
              nfolds = 5,type.measure="mse",family="gaussian")
en6=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.50,
              nfolds = 5,type.measure="mse",family="gaussian")
en7=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.60,
              nfolds = 5,type.measure="mse",family="gaussian")
en8=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.70,
              nfolds = 5,type.measure="mse",family="gaussian")
en9=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.80,
              nfolds = 5,type.measure="mse",family="gaussian")
en10=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.90,
              nfolds = 5,type.measure="mse",family="gaussian")
en11=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$`crime[, 16]`),
              alpha=0.95,
              nfolds = 5,type.measure="mse",family="gaussian")
#It includes the cross-validation curve (red dotted line), 
#and upper and lower standard deviation curves along the λ sequence (error bars). 
#Two selected λ’s are indicated by the vertical dotted lines (see below).
plot(en1)
plot(en2)
plot(en10)
#Output the coefficients of the variables selected by Elastic Net
summary(en1)

coef(en1)
coef(en2)
coef(en3)
coef(en4)
coef(en5)
coef(en6)
coef(en7)
coef(en8)
coef(en9)
coef(en10)
coef(en11)

en_mod1 = lm(scaled_data$`crime[, 16]` ~M + Po1 + Po2 + M.F + NW + Ineq + Prob, data = scaled_data)
en_mod2 = lm(scaled_data$`crime[, 16]` ~M + Po1 + Po2 + M.F + NW + Ineq + Prob, data = scaled_data)
en_mod3 = lm(scaled_data$`crime[, 16]` ~M + Po1 + Po2 + M.F + NW + Ineq + Prob, data = scaled_data)
en_mod4 = lm(scaled_data$`crime[, 16]` ~M + Ed + Po1 + Po2 + LF + M.F + NW + U2 + Ineq + Prob, data = scaled_data)
en_mod5 = lm(scaled_data$`crime[, 16]` ~M + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U2 + Ineq + Prob, data = scaled_data)
en_mod6 = lm(scaled_data$`crime[, 16]` ~M + Ed + Po1 + Po2 + LF + M.F + NW + U2 + Ineq + Prob, data = scaled_data)
en_mod7 = lm(scaled_data$`crime[, 16]` ~M + Po1 + Po2 + M.F + NW + Ineq + Prob, data = scaled_data)
en_mod8 = lm(scaled_data$`crime[, 16]` ~M + Po1 + Po2 + M.F + NW + Ineq + Prob, data = scaled_data)
en_mod9 = lm(scaled_data$`crime[, 16]` ~M + Po1 + Po2 + M.F + NW + Ineq + Prob, data = scaled_data)
en_mod10 = lm(scaled_data$`crime[, 16]` ~Po1, data = scaled_data)
en_mod11 = lm(scaled_data$`crime[, 16]` ~M + Ed + Po1 + LF + M.F + NW + U2 + Ineq + Prob, data = scaled_data)


summary(en_mod1) #Multiple R-squared:  0.7112,	Adjusted R-squared:  0.6594 
#AIC[1] 652.0283
summary(en_mod2) #Multiple R-squared:  0.7112,	Adjusted R-squared:  0.6594 
#AIC[1] 652.0283
summary(en_mod3) #Multiple R-squared:  0.7112,	Adjusted R-squared:  0.6594
#AIC[1] 652.0283
summary(en_mod4) #Multiple R-squared:  0.7739,	Adjusted R-squared:  0.7111
#AIC[1] 646.5171
summary(en_mod5) #Multiple R-squared:  0.7788,	Adjusted R-squared:  0.7093 
#AIC[1] 647.4955
summary(en_mod6) #Multiple R-squared:  0.7739,	Adjusted R-squared:  0.7111 
#AIC[1] 646.5171
summary(en_mod7) #Multiple R-squared:  0.7112,	Adjusted R-squared:  0.6594 
#AIC[1] 652.0283
summary(en_mod8) #Multiple R-squared:  0.7112,	Adjusted R-squared:  0.6594 
#AIC[1] 652.0283
summary(en_mod9) ##Multiple R-squared:  0.7112,	Adjusted R-squared:  0.6594 
#AIC[1] 652.0283
summary(en_mod10) #Multiple R-squared:  0.4728,	Adjusted R-squared:  0.4611 
#AIC[1] 668.3155
summary(en_mod11) #Multiple R-squared:  0.7712,	Adjusted R-squared:  0.7156 
#AIC[1] 645.0818

##The best model, both as a measure of R-Squared, Adjusted R-Squared, and AIC is model 11 which has an 
##alpha of 0.95. The fact that the best performing model's alpha is so close to 1 would suggest an alpha 
#greater than 1 would produce the most robust model. 

##Note, all the elastic net models created result in using insignificant factors 

################## SUMMARY ###################
##Stepwise Regression model:
#6 factors: R-squared:  0.7659,	Adjusted R-squared:  0.7307
##Lasso Model
#8 factors: R-squared:  0.7752,	Adjusted R-squared:  0.7205 
##Elastic Net Model
#9 factors: #Multiple R-squared:  0.7712,	Adjusted R-squared:  0.7156 
##Despite the Lasso Model having the best R-Squared, and the utter disdain reflected among the 
##data science community towards using a stepwise process, I would suggest using the Stepwise Regression 
##Model because it has the highest adjusted R-Squared and is the most parsimounous (sp?)


