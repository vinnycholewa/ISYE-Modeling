my_packages = c('outliers', 'ggplot2', 'gridExtra', 'corrplot')
lapply(my_packages, library, character.only = TRUE)

crime = read.table(file = 'http://www.statsci.org/data/general/uscrime.txt', 
                   header =TRUE, sep = "")
#colnames(crime) = c('% Males', 'Southern St', 'Edu', 'CAPEX 59', '
#CAPEX 60', 'Labor Force Participation Rate', 'Males to 100 Females', 
#'Population', 'Nonwhite Population', 'Unemployment < 25', 'Undemployment > 35', 
#'#'Median Wealth', 'Income Inequality', 'Prob Imprisonment', 'Avg Time Served', 
#''Crime Rate') 
head(crime)

#correlation analysis
crime_corr = cor(crime, method = 'pearson')
c =as.matrix(crime_corr)
corrplot(c, title = 'Correlation Matrix', method = 'color')
#southern states & edu seem to be correlated the most to crime whereas wealth and wealth inequality are detractors from crime 

#let's have a look at crime to educaiton, wealth, and capital expenditure to police force
qplot(x = crime$Ed, y = crime$Crime, data = crime,
      xlab = 'Education', ylab = 'Crime Per Hundred Thousand', col = 'red')
qplot(x = crime$Wealth, y = crime$Crime, data = crime,
      xlab = 'Wealth', ylab = 'Crime Per Hundred Thousand', col = 'red')
qplot(x = crime$Po1, y = crime$Crime, data = crime,
      xlab = 'Police Protection CapEx', ylab = 'Crime Per Hundred Thousand', 
      col = 'red')

#lets run a box plot to see if any outliers reveal themselves 
qplot(crime$So, crime$Crime, geom = "boxplot", data = crime, col = 'red')

#both wealth & education seem to exhibit odd data points 
#that may be outliers but certainly worth analyzing further

##Question 5.1: using grubbs.test to check for outliers in col 'Crime'#
grubbs.test(x = crime$Crime,type = "11", opposite = FALSE, two.sided = FALSE )
#results come in with 342 & 1993. With a p-value of 1 we cannot reject the null
#hypothesis meaning one, if not both, are NOT outliers
#this data may not be norm dist either

#Let's run a one-tail test to see if any of the cities are 
#indeed significant (i.e. outliers)
grubbs.test(x = crime$Crime,type = "10", opposite = FALSE, two.sided = FALSE )
grubbs.test(x = crime$Crime,type = "10", opposite = TRUE, two.sided = FALSE )
#1993 shows again as an outlier but a p-value of 0.07 
#normally wouldnt cross the 0.05 threshold

##Question 6.2.1
library(qcc)
temps = read.table(file = 'temps.txt', header = TRUE, sep = "")
head(temps)

cusum(temps)



