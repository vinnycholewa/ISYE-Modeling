################# Homework 10 ###################
setwd("/Users/vincentcholewa/Documents/GAT/ISYE/isye_wd")
# # Data Set Information:
# # Samples arrive periodically as Dr. Wolberg reports his clinical cases. The database therefore reflects this chronological 
# #grouping of the data. This grouping information appears immediately below, having been removed from the data itself:
# # Group 1: 367 instances (January 1989)
# # Group 2: 70 instances (October 1989)
# # Group 3: 31 instances (February 1990)
# # Group 4: 17 instances (April 1990)
# # Group 5: 48 instances (August 1990)
# # Group 6: 49 instances (Updated January 1991)
# # Group 7: 31 instances (June 1991)
# # Group 8: 86 instances (November 1991)
# 
# 1. Sample code number            id number
# 2. Clump Thickness               1 - 10
# 3. Uniformity of Cell Size       1 - 10
# 4. Uniformity of Cell Shape      1 - 10
# 5. Marginal Adhesion             1 - 10
# 6. Single Epithelial Cell Size   1 - 10
# 7. Bare Nuclei                   1 - 10
# 8. Bland Chromatin               1 - 10
# 9. Normal Nucleoli               1 - 10
# 10. Mitoses                       1 - 10
# 11. Class:                    (2 for benign, 4 for malignant)

bc = read.table(file = "breast_cancer.data", header = FALSE, sep = ",",
                                 col.names = c('code_num','thickness','uniformity_size', 'uniformity_shape', 'adhesion', 
                                               'epithelial_size',
                                               'nuclei', 'chromatin', 'nucleoli', 'mitoses', 'class' ))
bc_missing = read.table(file = "breast_cancer.data", header = FALSE, sep = ",",
                col.names = c('code_num','thickness','uniformity_size', 'uniformity_shape', 'adhesion', 
                              'epithelial_size',
                              'nuclei', 'chromatin', 'nucleoli', 'mitoses', 'class' ))

#bc
bc_q <- bc == "?"
# replace elements with NA
is.na(bc) = bc_q
colSums(is.na(bc))
# > colSums(is.na(bc))
# code_num        thickness  uniformity_size uniformity_shape 
# 0                0                0                0 
# adhesion  epithelial_size           nuclei        chromatin 
# 0                0               16                0 
# nucleoli          mitoses            class 
# 0                0                0 
# Bare nuclei appears to be the only variable that contains values we need to impute for 
# and has a total of 16 instances that reflect NA.

## There are two types of missing data:
# 1. MCAR: missing completely at random. This is the desirable scenario in case of missing data.
# 2. MNAR: missing not at random. Missing not at random data is a more serious issue and in this case it
# might be wise to check the data gathering process further and try to understand why the information is missing. 

# Let's have a look at the bare nuclei dataset
bc$nuclei
# When looking at the data set a large portion of NAs are covered from rows 136 to rows 316.
# This would be something to examine in more detail to see if a trend occured during a survey
# that is creating the missing fields
length((bc$nuclei))
# 699

# Per the lecture videos, the amount of n/a should not exceed 5%. As you will see below, this dataset's
# NA count amounts to only 2.2%
sum(is.na(bc$nuclei))/length((bc$nuclei))*100
# 2.288984%

# Let's review by looking at the summary statstics and charting a pairwise visual to assess correlations.
summary(bc)
pairs(bc)

library(mice)
library(VIM)

#T he package MICE is a good library to handle missing data. 
# The package creates multiple imputations (replacement values) for multivariate missing data. 
# The method is based on Fully Conditional Specification, where each incomplete variable is imputed by a separate model. 
# The MICE algorithm can impute mixes of continuous, binary, unordered categorical and ordered categorical data. 
# In addition, MICE can impute continuous two-level data, and maintain consistency between imputations by means of passive imputation. 
# Many diagnostic plots are implemented to inspect the quality of the imputations.

# Main Functions of Mice
# mice()	Impute the missing data *m* times
# with()	Analyze completed data sets
# pool()	Combine parameter estimates
# complete()	Export imputed data
# ampute()	Generate missing data
md.pairs(bc)
md.pattern(bc)
# > md.pattern(bc)
# code_num thickness uniformity_size uniformity_shape adhesion epithelial_size chromatin nucleoli mitoses class nuclei   
# 683        1         1               1                1        1               1         1        1       1     1      1  0
# 16         1         1               1                1        1               1         1        1       1     1      0  1
# 0         0               0                0 
# The MICE pattern function first states that we have 683 complete variables and 16 missing. It then delineates which
# variables are missing information. 

# I found this from a tutorial that went through the MICE library. What this plot shows is the graphical representation of
# missing data. In our data set Bare Nuclei is the only variable that is missing data and accounts for a litte over 2%. 
aggr_plot = aggr(bc$nuclei, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bc), 
                  ylab=c("Histogram of missing data","Pattern"))

# Convert dataset into data frame
bc_as_data = as.data.frame(bc)
# Plot pbox using pos = 7 (nuclei)
pbox(x = bc_as_data, pos = 7)

###################### Mean/Mode Imputation #############
# 14.1.1 Use the mean/mode imputation method to impute values for the missing data
bc_mean_data = mice(bc,m=5,maxit=5,method ='pmm',seed=500)
#### imputs ####
# m -> number of multiple imputations, maxit -> scaler giving the number of iterations, 
# pmm -> predictive mean matching
summary(bc_mean_data)

# Let's check to determine if the mean forumula above worked correctly to replace NAs with 
# the mean. 

bc_mean_data$imp$nuclei
# > bc_data$imp$nuclei
# 1  2  3 4  5
# 24  10  4 10 5 10
# 41   1  1  3 1  1
# 140  1  1  1 1  1
# 146  1  5  1 3  1
# 159  1  1  1 1  1
# 165  1  1  1 1  3
# 236  1  1  1 1  1
# 250  1  1  1 2  1
# 276  1  1  1 1  1
# 293  4 10  3 1  1
# 295  1  3  1 1  1
# 298  1  1  5 1  1
# 316  5  1  1 3 10
# 322  5  1  1 1  1
# 412  1  1  1 1  1
# 618  1  1  1 1  1

bc_clean = complete(bc_mean_data, 1)
#bc_clean
md.pattern(bc_clean)
# > md.pattern(bc_clean)
#  /\     /\
# {  `---'  }
# {  O   O  }
# ==>  V <==  No need for mice. This data set is completely observed.
#  \  \|/  /
#   `-----'
#         
# code_num thickness uniformity_size uniformity_shape adhesion epithelial_size nuclei chromatin
# 699        1         1               1                1        1               1      1         1
# 0         0               0                0        0               0      0         0
# nucleoli mitoses class  
# 699        1       1     1 0
# 0       0     0 0


################## 14.1.2 ########################################
####  2. Use regression to impute values for the missing data ####
bc_ln_table = bc[1:10]
#bc_ln_table
bc_ln_data = mice(bc_ln_table,m=4, maxit = 5 ,method ='norm.predict',seed=50)
summary(bc_ln_data)
bc_ln_data$imp$nuclei
## This approach results in filling the missing values with NA as opposed to values derive
## from a linear regression (I tried both linear predictive and linear ignoring model errors). 
## I will now look to solve for the missing variables manually using the approach discussed
## in office hours.

# Let's create a variable, missing, that holds the instances of ?.
missing = which(bc_missing$nuclei == "?",arr.ind = TRUE) 
missing

# Let's now create a variable that removes the categorical column, class, and missing data. 
continuous_data = bc[-missing,2:10]
continuous_data$nuclei = as.integer(continuous_data$nuclei)

# let's now build the linear model
lm_mod = lm(nuclei~thickness+uniformity_size+
                    uniformity_shape+adhesion+epithelial_size+
                    chromatin+nucleoli+mitoses, 
            data = continuous_data)
summary(lm_mod)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       1.862817   0.162497  11.464  < 2e-16 ***
# continuous_data$thickness         0.068118   0.034746   1.960  0.05035 .  
# continuous_data$uniformity_size   0.087939   0.063482   1.385  0.16643    
# continuous_data$uniformity_shape  0.110046   0.061190   1.798  0.07255 .  
# continuous_data$adhesion         -0.076950   0.038270  -2.011  0.04475 *  
# continuous_data$epithelial_size   0.043216   0.052123   0.829  0.40733    
# continuous_data$chromatin         0.044536   0.049211   0.905  0.36579    
# continuous_data$nucleoli          0.119422   0.037076   3.221  0.00134 ** 
# continuous_data$mitoses           0.001405   0.049448   0.028  0.97733    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.896 on 674 degrees of freedom
# Multiple R-squared:  0.2326,	Adjusted R-squared:  0.2235 
# F-statistic: 25.54 on 8 and 674 DF,  p-value: < 2.2e-16

## As you can see, the robustness of the model is questionable (R-Squared of 0.23). 
## Let's remove the insignificant variables and re-run the model

lm_mod1 = lm(nuclei~thickness + uniformity_shape + adhesion +
                     nucleoli, data = continuous_data)
summary(lm_mod1)
# This results in very little improvement but is more parsimonous so let's continue 
# with this model.

nuclei_pred =  predict(lm_mod1, newdata = bc[missing,])
# Let's look at our predicted values for the missing data. As you will see below,
# all figures are floating point and need to be rounded to integers. 
nuclei_pred
# > nuclei_pred
# 24       41      140      146      159      165      236      250      276      293      295 
# 3.967619 4.322290 2.322981 2.723996 2.523488 2.642191 3.084108 2.482586 2.883601 5.563110 2.322981 
# 298      316      322      412      618 
# 3.327197 4.252752 2.482586 2.322981 2.322981 
round_nuclei_pred = round(nuclei_pred)
round_nuclei_pred
# > round_nuclei_pred
# 24  41 140 146 159 165 236 250 276 293 295 298 316 322 412 618 
# 4   4   2   3   3   3   3   2   3   6   2   3   4   2   2   2

# Now, let's impute the rounded predicted values into a new dataset we'll create specifically for 
# this linear imputation method. 
bc_data_imputaton = bc
bc_data_imputaton[missing,]$nuclei = round_nuclei_pred
bc_data_imputaton$nuclei = as.numeric(bc_data_imputaton$nuclei)
# Let's view our imputed data as a sanity check to ensure there are no missing data or decimals.
bc_data_imputaton$nuclei
# Let's also make sure our data set is completely observed using mice pattern.
md.pattern(bc_data_imputaton)
# No need for mice. This data set is completely observed.

################## 14.1.3 ########################################
#### 3. Use regression with perturbation to impute values for the missing data 

# What is perturbation? The definition states: a deviation of a system, moving object, 
# or process from its regular or normal state or path, caused by an outside influence.

# Using regression (above) is more complex but leads to less biased data. 
# With that said, regression also has the disadvantage of using the same data twice which could 
# lead to overfitting the data. 
# Ultimately, This doesn't capture all the variability in said data rows. 
# An approach to solve this is perturbation – adding a random amount up or down for each imputed estimate. 
# One final note, professor acknowledges that this approach often leads to less accuracy...

mu = mean(nuclei_pred)
# 3.096715
sd_hat = sd(nuclei_pred)
# 0.9522
pertub_val = rnorm(n = length(nuclei_pred), mean = mu, sd = sd_hat)
pertub_val
# As before we need to round these figures
round_pertub_val = round(pertub_val)

bc_data_pertub = bc
bc_data_pertub[missing,]$nuclei = round_pertub_val
bc_data_pertub$nuclei = as.numeric(bc_data_pertub$nuclei)
# Data check
md.pattern(bc_data_pertub)


################## 15.1 ########################################
# Describe a situation or problem from your job, everyday life, current events, etc., for which optimization
# would be appropriate. What data would you need? 

# I work in the asset owner community building asset allocation models through numerous optimization methods.
# My work was described in class (essentially picking the index/universe you want to select securities from, 
# adding constraints to ensure no position is too little, too large, or unattainable (when there is float issues)
# and then solving using mean & variance). Since that was described in class notes, I will pivot to a hobby of mine,
# nutrition. How do you optimize your diet? This was described using the army's dillemnia but it was more a 
# method of providing the soldiers just enough to accomplish their stated missions. The questin I'd be looking to 
# solve pertains to sports science - specifically what can you eat to improve your optimal performance within a 
# specific sports competition. 

# I'd need numerous randomized trials involving student athletes. I'd examine their gut biome, total caloric exertion 
# on a day of an event, and allergens. I'd likely start with a quantifiable event, say sprinting, and design the 
# experiment around a trial of 15-20 races using at least 5 athletes. The variables in my experiment would be composition sources 
# (i.e. protein, carbohydrates, minerates, vitamins, etc). I'd add constraints to appease certain allergies,
# min/max intake (to prevent illness), and limit the intake to only natural foods (i.e. no synethetics).
# My optimization function will solve for the sum of each food source that minimizes the race time. I'd need to control
# for externalities such as sleep, tests, social life etc. 

