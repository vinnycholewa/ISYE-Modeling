library(ggplot2)
library(dplyr)
library(xlsx)


temps = read.table(file = 'temps.txt', header = TRUE)
set.seed(23)
nrow(temps)
#read in unmodified (unlist) datafile 
temps_vct <- as.vector(unlist(temps[,2:21]))


#start on 1996 and set frequency to make 123 observations per year
temps_ts = ts(data = temps_vct, start=1996, end=2015, frequency = 123)
head(temps_ts)
plot.ts(temps_vct, xlab="Time", ylab="Temperature",
        xlim = c(1996,2015), 
        ylim = c(80,100),
        col = "red")


#HoltWinters Smoothing
#First test is exponentional smoothing (beta = false), 
#and non-seasonaity (gamma = false) and no trend
hw_1 = HoltWinters(temps_ts, beta = FALSE, gamma = FALSE)
hw_1
#HoltWinters(x = temps_ts, beta = FALSE, gamma = FALSE)
#Smoothing parameters:
 #       alpha: 0.8876596
#beta : FALSE
#gamma: FALSE


hw_2 = HoltWinters(temps_ts, beta = FALSE)
hw_2
#Smoothing parameters:
#alpha: 0.03407497
#beta : FALSE
#gamma: 1
#Coefficients:
 #       [,1]
#a     79.3119685

hw_3 = HoltWinters(temps_ts)
hw_3
#Smoothing parameters:
#alpha: 0.6677614
#beta : 0
#gamma: 0.6297674


#additive method is preferred when the seasonal variations are roughly constant. 
# This is not a good indicator given the seasonal effects of Sept & Oct.
# Since the variation in seasonal patterns is inherent in our data, 
# and shown in the plot, a multiplicative method is preferred


hw_4 = HoltWinters(temps_ts, seasonal = "additive")
hw_4
#Smoothing parameters:
#        alpha: 0.03408409
#beta : 0
#gamma: 1

temps_ts_summer = ts(data = temps, start = 1996, end = 2015, frequency = 64)
hw_summer = HoltWinters(temps_ts_summer, seasonal = "additive")
hw_summer
#HoltWinters(x = temps_ts_summer, seasonal = "additive")
#Smoothing parameters:
#        alpha: 0.6073352
#beta : 0
#gamma: 0.5367355

hw_5 = HoltWinters(temps_ts, 
                   seasonal = "multiplicative")
#Smoothing parameters:
#alpha: 0.6219732
#beta : 0
#gamma: 0.5521032


summary(hw_5)
head(hw_5)
tail(hw_5)
hw_5
plot(hw_5,
     ylim = c(50,130),
     ylab = "Temperature")

m_fit = matrix(hw_5$fitted[,4],ncol=123)
head(m_fit)
model_5 = hw_5$fitted

#xhat represents the model output including all variables
#trend is the contribution from trend
#level - described as residential from TA
#season - the multiplicative factor being used to calculate seasonality


write.table(hw_5$fitted, 
            file = "fitted_output.csv",
            row.names=FALSE, 
            col.names=TRUE, sep=",")


