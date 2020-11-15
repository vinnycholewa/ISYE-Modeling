
###My understanding of the steps to solve this problem
#1. Apply PCA
#2. run regression on top principal components
#2a.      -this is done through rotation
#3. specificy new model in terms of the original variables
#3.a:    -scale the data back to original  
#4. Compare quality to past hw assigment 


#load packages
packages = c('ggplot2','GGally','corrplot', 'ggfortify', 'tidyverse', 'gridExtra', 'devtools', 'ggbiplot')
lapply(packages, library, character.only = TRUE)
#set set
set.seed(42)


#load data and run preliminary assessment of data
crime = read.table(file = "uscrime.txt" , header = TRUE, sep = "")
head(crime)
summary(crime)
crime_cor= cor(crime)
round(crime_cor, 2)
corrplot(crime_cor, method = 'shade')
pairs(crime)
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

#PCA setup
pca = prcomp(crime[,1:15], scale. = TRUE, center = TRUE)
        #scale is true because you first scale the data
#need to multiple by standard deviation and add mean do de-scale 
##then you rotate, and we will need to de-scale/rotate in later steps
summary(pca)
#PCA finds the best fitting line by maximizing the sum of the squared 
#distances from the projected points to the origin. I.e. the maximized sum of squared distances 
#this maximized SS Distance is called the Eigen Value

#when looking at the summary information, the cumulative proportion explains the variance explained.
#PC! explains 40.1%, PC2 explains 18%, PC3 explains 13.4%, and PC4 explains ~6%

#Importance of components:
#        PC1   PC2   PC3    PC4    PC5    PC6    PC7    PC8    PC9   PC10   PC11    PC12    PC13   PC14
#Standard deviation     2.453 1.674 1.416 1.0781 0.9789 0.7438 0.5673 0.5544 0.4849 0.4471 0.4191 0.35804 0.26333 0.2418
#Proportion of Variance 0.401 0.187 0.134 0.0775 0.0639 0.0369 0.0215 0.0205 0.0157 0.0133 0.0117 0.00855 0.00462 0.0039
#Cumulative Proportion  0.401 0.588 0.722 0.7992 0.8631 0.9000 0.9214 0.9419 0.9576 0.9709 0.9826 0.99117 0.99579 0.9997
#PC15
#Standard deviation     0.06793
#Proportion of Variance 0.00031
#Cumulative Proportion  1.00000

pca$center
pca$scale
pca$sdev
#the center and scale components correspond to the means and standard
#deviations of the variables that were used for scaling prior to implementing PCA


screeplot(pca, type = "barplot", col = 'red')
screeplot(pca, type ='lines' , col = 'red')
#Variance calculation and proportion of variance. This can
#support PC selection

#Biplot illustrates the use of PCA of Crime. Our data seems to concentrate on the postive side
biplot(pca, scale=0, cex=.5)


var = pca$sdev^2
propvar <- var/sum(var)
propvar

#Scree Plot of the percent variation captured by each component 
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

# Plot the proportion of variances from PCA

plot(propvar, xlab = "Principal Components", 
     ylab = "Proportion of Variance", ylim = c(0,1), type = "b")

#create top four principal components
pcs = pca$x[,1:4]
summary(pcs)
pcs
#in reviewing the loadings, PC1 places much weight on male, southern state, education, and Po1
#PC2 places the most weight on popuation, PC3 is U1 which is unemployment rate, and 
#PC4 looks to be time spent in jail

# Build linear regression model with the first 4 principal components
pc_crime = cbind(pcs, data[,16]) 
summary(pc_crime)
pc_crime
as.data.frame(pc_crime) #create as data frame
pc_model = lm(V5~., data = as.data.frame(pc_crime)) #Create regression model on data frame
summary(pc_model)


##the R^2 and Adjusted R^2 is 0.309 and 0.243. Given the reduction
#in dimensionalities, this was expected but I did not expect this much
#since our previous OLS model had a R^2 of 0.809

##summary pc_model
#Residual standard error: 336 on 42 degrees of freedom
#Multiple R-squared:  0.309,	Adjusted R-squared:  0.243 
#F-statistic:  4.7 on 4 and 42 DF,  p-value: 0.00318


#coefficients stated in terms of original data
# PCA Coefficients for this linear regression model
pc_intercept = pc_model$coefficients[1]
betas = pc_model$coefficients[2:5]
pc_intercept
#intercept 905 
betas
#> betas
#PC1   PC2   PC3   PC4 
#65.2 -70.1  25.2  69.4 

pca$rotation[,1:4]
##############
#Looking at PC1 - percent male, southern states, Education, Police Funding
##These variables dominate the proportion 
#PC2 - is driven primarily from population
#PC3 - Unemploymet Rates
#PC4 - probability of imprisonment & time in prison
#> pca$rotation[,1:4]
#PC1      PC2       PC3     PC4
#M      -0.3037  0.06280  0.172420 -0.0204
#So     -0.3309 -0.15837  0.015543  0.2925
#Ed      0.3396  0.21461  0.067740  0.0797
#Po1     0.3086 -0.26982  0.050646  0.3333
#Po2     0.3110 -0.26396  0.053065  0.3519
#LF      0.1762  0.31943  0.271530 -0.1433
#M.F     0.1164  0.39434 -0.203162  0.0105
#Pop     0.1131 -0.46723  0.077021 -0.0321

# Transform the PC coefficients into coefficients for the original variables
#multiple two matrixes (%*%) to calculate alphas
alphas = pca$rotation[,1:4] %*% betas
t(alphas)

#          M   So   Ed  Po1  Po2  LF   M.F  Pop   NW    U1   U2 Wealth  Ineq Prob  Time
# [1,] -21.3 10.2 14.4 63.5 64.6 -14 -24.4 39.8 15.4 -27.2 1.43   38.6 -27.5  3.3 -6.61

#Convert data back to original data (step 3a) by scaling data 
original_alpha = alphas/sapply(data[,1:15],sd)
original_intercept = pc_intercept - sum(alphas*sapply(data[,1:15],mean)/sapply(data[,1:15],sd))


t(original_alpha)
#> t(original_alpha)
#M   So   Ed  Po1  Po2   LF   M.F  Pop  NW    U1   U2 Wealth Ineq Prob   Time
#[1,] -16.9 21.3 12.8 21.4 23.1 -347 -8.29 1.05 1.5 -1510 1.69   0.04 -6.9  145 -0.933

original_intercept
#This intercept of 1666 compares to the previous model we created which was -3380
#> original_intercept
#(Intercept) 
#1666


# calculate model estimates to find new R^2 and Adjusted R^2
estimates = as.matrix(data[,1:15]) %*% original_alpha + original_intercept
estimates
SSE = sum((estimates - data[,16])^2)
SStot = sum((data[,16] - mean(data[,16]))^2)
1 - SSE/SStot
#> 1 - SSE/SStot
#[1] 0.309

R2 = 1 - SSE/SStot
R2 - (1 - R2)*4/(nrow(data)-4-1)
#> R2 - (1 - R2)*4/(nrow(data)-4-1)
#[1] 0.243

#note, the R^2 and Adjusted R^2 are the same as the pca dimensions scaled
#back to the original data. 


############################
#model comparison to previous week's homework

#run regression on drime data
crime_lm = lm(crime$Crime ~ ., data = crime)
crime_lm
#R automatically denotes in scientific notation. options(scipen=4) will make the 
#coefficients easier to read 
options(scipen=4)       
crime_lm
summary(crime_lm)
# This model has R^2 = 0.803 and R^2_adj = 0.708.


AIC(crime_lm)
#650
AIC(model_pc)
#687
BIC(crime_lm)
#681
BIC(model_pc)
#698

###################
##Conclusion##
###################
#The straightforward OLS model is better at capturing variance
#and predicting the crime data vs the PCA method. Though the PCA
#properly reduces the dimensionality, the previous week's assignment 
#of variable reduction is better vs this week's process of feature extraction.
#This is expected since PCA is not known to, out of the box, work with binary data such as
#is southern state 











