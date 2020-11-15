############## Question 12.1 ####################
##Describe a situation or problem from your job, everyday life, current events, etc., for which a design of
##experiments approach would be appropriate.

##ANSWER
##DOE is a method of collecting the best subset of data we can get, quicky, and efficiently.
##Two important aspects of this include control/comparison & blocking. I work in the consulting field and
##part of the business model is scalable webinars. Our marketing team deploys multiple emails 
##inviting clients to the webinar and follow-up emails as reminders. One challenge is increasing
##the attendance rate to compensate for the cost of production (time spent researching, bandwidth
##server costs, etc). Coupled with this challenge, our clients are deludged with emails and 
##invitations. To address this issue, we could employ DOE to test the efficacy of each email and the
##the coresponding reminder emails. Since my firm has finite clients and limited email attempts (a practice 
##to ensure we do not inundate our own clients) we set standards of no more than 3 total emails. Given this 
##information, a multi-armed bandit approach would help determine the most effective emails while
##combating the trade-off between exporation and exploitating. 

############## Question 12.2 ####################
##To determine the value of 10 different yes/no features to the market value of a house (large yard, solar
##roof, etc.), a real estate agent plans to survey 50 potential buyers, showing a fictitious house with
##different combinations of features. To reduce the survey size, the agent wants to show just 16 fictitious
##houses. Use R’s FrF2 function (in the FrF2 package) to find a fractional factorial design for this
##experiment: what set of features should each of the 16 fictitious houses have? Note: the output of
##FrF2 is “1” (include) or “-1” (don’t include) for each feature

##ANSWER
library(FrF2)
#nruns -> 16, since the real estate agent wants to include 16 fictitious house
#nfactors -> 10, since we want to determine the value of 10 different yes/no features
#factor.names -> TBD by real estate agent 

set.seed(123)

house_design = FrF2(nruns = 16, nfactors = 10)
house_design
# > house_design
# A  B  C  D  E  F  G  H  J  K
# 1  -1  1  1 -1 -1 -1  1  1 -1  1
# 2   1  1 -1  1  1 -1 -1  1 -1 -1
# 3   1 -1 -1  1 -1 -1  1  1  1  1
# 4   1 -1  1 -1 -1  1 -1 -1  1  1
# 5  -1 -1  1 -1  1 -1 -1  1  1 -1
# 6   1  1  1  1  1  1  1  1  1  1
# 7  -1  1 -1  1 -1  1 -1 -1 -1  1
# 8  -1 -1  1  1  1 -1 -1 -1 -1  1
# 9  -1 -1 -1  1  1  1  1 -1  1 -1
# 10 -1  1 -1 -1 -1  1 -1  1  1 -1
# 11  1  1  1 -1  1  1  1 -1 -1 -1
# 12 -1 -1 -1 -1  1  1  1  1 -1  1
# 13  1 -1 -1 -1 -1 -1  1 -1 -1 -1
# 14  1  1 -1 -1  1 -1 -1 -1  1  1
# 15  1 -1  1  1 -1  1 -1  1 -1 -1
# 16 -1  1  1  1 -1 -1  1 -1  1 -1
# class=design, type= FrF2 

############## Question 13.1 ####################
# For each of the following distributions, give an example of data that you would expect to follow this
# distribution (besides the examples already discussed in class).
# a. Binomial -> binomial distribution describes the number of successes k achieved in n trials (the opposite is true
#for a negative binomial) ((basically anything with two outcomes)). An example of this would be the distribution following
##target range with the outcomes being either hit target or miss.
# b. Geometric ->Similar to binomial distribution, geometric looks to answer the question of how many trials, n, do we need
#until we arrive at an answer of a certain type (a lot of times looking at a sequence of negative results and waiting for that
#positive outcome). An example of this would be how many people outside a voting booth would we need to question before we 
#find a person who voted for the independent party.
# c. Poisson -> The Poisson distribution is the discrete probability distribution of the number of events occurring in a given time 
#period, given the average number of times the event occurs over that time period. An example of this distribution would be the 
#consecutive topics studying for ISYE homework before becoming distracted and looking at instragram or facebook.
# d. Exponential -> Exponential distributions are very closely tied to poisson. If lambda in poisson measures the change of study
#topics, the exponential distribution measures the time in between the switching of topics.  
# e. Weibull -> The Weibull distribution is very good at measuring the amount of time it takes something to fail,
#specifically, the time in between failures. This may not be right but I look at Weibull as the geometric version of poisson to exponential.
#An example of this would be measuring the distribution of the time one can spend studying for ISYE until their computer
#will run out of battery and shutdown. 

