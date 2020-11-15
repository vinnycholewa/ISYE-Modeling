Packages <- c("ggplot2", "reshape2", "corrplot", "DAAG")
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

#Linear Regressin Model Setup

crime_lm = lm(crime$Crime ~ ., data = crime)
crime_lm
#R automatically denotes in scientific notation. options(scipen=4) will make the 
#coefficients easier to read 
options(scipen=4)       
crime_lm

#Coefficients:
#        (Intercept)            M           So           Ed          Po1          Po2  
#-5984.28760     87.83017     -3.80345    188.32431    192.80434   -109.42193  
#LF          M.F          Pop           NW           U1           U2  
#-663.82615     17.40686     -0.73301      4.20446  -5827.10272    167.79967  
#Wealth         Ineq         Prob         Time  
#0.09617     70.67210  -4855.26582     -3.47902  


summary(crime_lm)

#Residuals:
#        Min      1Q  Median      3Q     Max 
#-395.74  -98.09   -6.69  112.99  512.67 
#Coefficients:
        #Estimate  Std. Error t value Pr(>|t|)    
#(Intercept) -5984.28760  1628.31837  -3.675 0.000893 ***
#M              87.83017    41.71387   2.106 0.043443 *  
#So             -3.80345   148.75514  -0.026 0.979765    
#Ed            188.32431    62.08838   3.033 0.004861 ** 
#Po1           192.80434   106.10968   1.817 0.078892 .  
#Po2          -109.42193   117.47754  -0.931 0.358830    
#LF           -663.82615  1469.72882  -0.452 0.654654    
#M.F            17.40686    20.35384   0.855 0.398995    
#Pop            -0.73301     1.28956  -0.568 0.573845    
#NW              4.20446     6.48089   0.649 0.521279    
#U1          -5827.10272  4210.28904  -1.384 0.176238    
#U2            167.79967    82.33596   2.038 0.050161 .  
#Wealth          0.09617     0.10367   0.928 0.360754    
#Ineq           70.67210    22.71652   3.111 0.003983 ** 
#Prob        -4855.26582  2272.37462  -2.137 0.040627 *  
#Time           -3.47902     7.16528  -0.486 0.630708    
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#probability of imprisonment seems to be the predictor variable which has the
#greatest impact and is significantly significant 
#Residual standard error: 209.1 on 31 degrees of freedom
#Multiple R-squared:  0.8031,	Adjusted R-squared:  0.7078 
#F-statistic: 8.429 on 15 and 31 DF,  p-value: 0.0000003539
#R^2 at 80% is seemingly very good as is the F-stat greater than 1
crime_lm
#plot Linear Model
#Look for constant variance and uncorrelated residuals 
plot(crime_lm)




#scatter plot should show a linear relationship between predictor variable & 
#response variable (crime)
#1. prob of imprisonment
#No true linear relationship
scatter.smooth(x=crime$Prob, y=crime$Crime, main="Dist ~ Prob of Imprisonment",
               xlab = 'Prob of Imprisonment', ylab = 'Crime Rate')
#2. Percent males aged 14-24
#Exhibits more of a j-curve
scatter.smooth(x=crime$Prob, y=crime$M, main="Dist ~ Percent Males",
               xlab = 'Percent Males', ylab = 'Crime Rate')
#3. Mean years of education (over age 25)
#negative linear relationship
scatter.smooth(x=crime$Prob, y=crime$Ed, main="Dist ~ Education",
               xlab = 'Education', ylab = 'Crime Rate')

#4. Police Funding
#Negative Linear relationship
scatter.smooth(x=crime$Prob, y=crime$Po1, main="Dist ~ Police Funding",
               xlab = 'Police Funding', ylab = 'Crime Rate')
#5. Unemployment Rate >35 years old
#Exhibits linear relationship
scatter.smooth(x=crime$Prob, y=crime$U2, main="Dist ~ 35+ Unemployment Rate",
               xlab = '35+ Unemployment Rate', ylab = 'Crime Rate')

#6. Income Inequality
#Linear relationship 
scatter.smooth(x=crime$Prob, y=crime$Ineq, main="Dist ~ Income Inequality",
               xlab = 'Income Inequality', ylab = 'Crime Rate')

#Density plot to determine if probablility of imprisonment is normally distributed [it's skewed to the right]
plot(density(crime$Prob), main="Density Plot: Prob of Imprisonment", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(crime$Prob), 2)))
polygon(density(crime$Prob), col="red")
plot(density(crime$Prob), main="Density Plot: Prob of Imprisonment", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(crime$Prob), 2)))
#Density plot for Percent Male 
#Appears to be normally Distributed 
plot(density(crime$M), main="Density Plot: Percent Male", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(crime$M), 2)))
polygon(density(crime$M), col="red")
#Density plot for Education
#Negatively skewed to the left
plot(density(crime$Ed), main="Density Plot: Education", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(crime$Ed), 2)))
polygon(density(crime$Ed), col="red")

#Predict Model

test_data = data.frame(M = 14.0,So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,LF = 0.640, M.F = 94.0, 
                               Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, 
                                Ineq = 20.1, Prob = 0.040,Time = 39.0)

model_pred = predict(crime_lm,test_data)
model_pred
#155.4349 
min(crime$Crime)
#minimum crime in data set is 342

crime_lm_new = lm(Crime ~  M + Ed + Po1 + U2 + Ineq + Prob, data = crime)
summary(crime_lm_new)
#Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -5040.50     899.84  -5.602 1.72e-06 ***
#        crime$Prob  -3801.84    1528.10  -2.488  0.01711 *  
#        crime$M       105.02      33.30   3.154  0.00305 ** 
#        crime$Ed      196.47      44.75   4.390 8.07e-05 ***
#        crime$Po1     115.02      13.75   8.363 2.56e-10 ***
#        crime$U2       89.37      40.91   2.185  0.03483 *  
#        crime$Ineq     67.65      13.94   4.855 1.88e-05 ***
#        ---
#        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 200.7 on 40 degrees of freedom
#Multiple R-squared:  0.7659,	Adjusted R-squared:  0.7307 
#F-statistic: 21.81 on 6 and 40 DF,  p-value: 3.418e-11

model_pred_new = predict(crime_lm_new, test_data)
model_pred_new
#1304.245

plot(crime_lm_new)

set.seed(55)
cross_val = cv.lm(crime,crime_lm_new,m=5)
#Response: Crime
#Df  Sum Sq Mean Sq F value  Pr(>F)    
#M          1   55084   55084    1.37 0.24914    
#Ed         1  725967  725967   18.02 0.00013 ***
#        Po1        1 3173852 3173852   78.80 5.3e-11 ***
#        U2         1  217386  217386    5.40 0.02534 *  
#        Ineq       1  848273  848273   21.06 4.3e-05 ***
#        Prob       1  249308  249308    6.19 0.01711 *  
#        Residuals 40 1611057   40276   

#model suggests ratio of males is less likely to have an effect on crime. 
#This output suggests Education, Police Funding, Unemployment >35, 
#Inequality, and probablity of imprisonment 

crime_lm_cv = lm(Crime ~  Ed + Po1 + U2 + Ineq + Prob, data = crime)
summary(crime_lm_cv)
#Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -3380.3      805.5   -4.20  0.00014 ***
#        Ed             168.6       48.4    3.48  0.00120 ** 
#        Po1            112.0       15.1    7.39  4.6e-09 ***
#        U2              44.4       42.3    1.05  0.29980    
#Ineq            81.0       14.7    5.53  2.0e-06 ***
#        Prob         -3625.1     1685.5   -2.15  0.03743 *  
#        ---
#        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 222 on 41 degrees of freedom
#Multiple R-squared:  0.708,	Adjusted R-squared:  0.672 
#F-statistic: 19.8 on 5 and 41 DF,  p-value: 5.26e-10


model1_aic = AIC(crime_lm)
model2_aic = AIC(crime_lm_new)
model3_aic = AIC(crime_lm_cv)

model1_aic
model2_aic
model3_aic
BIC(crime_lm)
BIC(crime_lm_new)
BIC(crime_lm_cv)

#> model1_aic
#[1] 650
#> model2_aic
#[1] 640
#> model3_aic
#[1] 649
#> BIC(crime_lm)
#[1] 681
#> BIC(crime_lm_new)
#[1] 655
#> BIC(crime_lm_cv)
#[1] 662

#BIC's penalty term is greater than AIC, hence drastic increase vs model 1
#In general terms |BICa-BICb| > 10, the model is very likely to be better.
#Although Model3 has the best AIC, the peanlty term rectifies the overfitting 
#resulting in a hampered output at 662
#in our case, |655 - 681| = 26 so we can state model 2 is better


exp((model2_aic-model1_aic)/2)
#> exp((model2_aic-model1_aic)/2)
#[1] 0.00722
#the first model which incoproates all factors is 0.72% likely to be as good as the second model
#which uses only the statistically significant factors 





