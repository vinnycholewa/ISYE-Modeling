## Summary: 
# This solution is a continuation of question 15.2 so please jump down to step 1 where 
# you will see the code for the new constraints. You'll see I run two optimizations, the first 
# simply solves given these newly added constraints and produces an unporportional recommendation of 
# broccoli while gaming the optimization and only selecting a trivial portion of the
# protein constraints. 


# To counter this, I include a fourth constraint that no food amount should be greater than 5.
# You will see my results commented at the very bottom but what instantly stands out is the cost
# jumps from ~$5 to over $10 and the amount of foods selected jumps to 9 sources

# One final note, I was fairly new to the pulp package so I relied heavily on both the office hours
# in addition to a few tutorials. One in particular stood out which walked through a similar
# optimization process which is found in a towardsdatascience write up. You can find that link below:
# https://towardsdatascience.com/linear-programming-and-discrete-optimization-with-python-using-pulp-449f3c5f6e99

## Newly Added Constraints
# 1.  If a food is selected, then a minimum of 1/10 serving must be chosen. (Hint: now you will
# need two variables for each food i: whether it is chosen, and how much is part of the diet.
#You’ll also need to write a constraint to link them.)
# 2.  Many people dislike celery and frozen broccoli. So at most one, but not both, can be
#selected.
#3.  To get day-to-day variety in protein, at least 3 kinds of meat/poultry/fish/eggs must be
#selected.

##### library setup #####
import pandas as pd 
import numpy as np 
from pulp import *
import seaborn
help('/n')




##### data arrangement and labeling #####
data = pd.read_excel('/Users/vincentcholewa/Documents/GAT/ISYE/isye_wd/diet.xls', header = 0)
#print(diet_data.head(5))

len_diet = len(data)
#print(len_diet)
# 67 
print(data.tail(25))

# 64                   NaN             NaN   ...           NaN      NaN
# 65                   NaN             NaN   ...         700.0     10.0
# 66                   NaN             NaN   ...        1500.0     40.0
# Looking at the tail it appears rows 65 & 66 contain the min and max daily intake. Let's create a variable to
# store both those constraints 
min_cal = data[65:66].values.tolist() 
max_cal = data[66:67].values.tolist()

# slice only food composition data and create list
diet_data = data[0:64]
# create lists of list (per office hour recommendations)
diet_data = diet_data.values.tolist()


# create a list comprehension to store food labels
food_names = [x[0] for x in diet_data]

# create dictionaries to store both cost & calories of food
food_cost = dict([(x[0], float(x[1])) for x in diet_data]) 
#food_cals = dict([[x[0], float(x[3])) for x in diet_data])

# setup nutrient names and create a list to hold each nutrient per food 
nutrient_names = list(data.columns.values)
#print(nutrient_names)
nutrients = []
for i in range(0,11): 
    nutrients.append(dict([(x[0], float(x[i+3])) for x in diet_data])) 



##### defining optimization and solving for minimization #####
problem = LpProblem("Diet Optimization", LpMinimize)

# define variables(the optimal amount of recommended food. we use a constraint, 0, to indicate 
# that we cannot have negative values)

food_amount = LpVariable.dicts("Food Amounts", food_names,lowBound=0,cat='Continuous')
food_chosen = LpVariable.dicts("Chosen Food",food_names,0,1,cat='Integer')


# add the objective function which you can see is the sum product of the cost x amount of food 
problem += lpSum([food_cost[x] * food_amount[x] for x in food_names])


# add constraints for min & max values for each food with the range being >= 1500 and <+ 2500 through list comprehension
for i in range(0, 11):
	problem += lpSum([nutrients[i][j] * food_amount[j] for j in food_names]) >= min_cal[0][i+3], 'minimum calories' + nutrient_names[i]
	problem += lpSum([nutrients[i][j] * food_amount[j] for j in food_names]) >= max_cal[0][i+3], 'maximum calories' + nutrient_names[i]

#################### Step 1 ###################
### Additional Constraints
## part 2: if a food is selected, it must comprise at least 10% of the total
# food intake. The following code looks to ensure that if the food product is used
# it will represent at least 10% of the meal. 
for f in food_names:
    problem += food_amount[f]>= food_chosen[f]*0.1
    problem += food_amount[f]<= food_chosen[f]*1e5

 

## Part 2: limit the meal to include only frozen brocolli or celery but not both
problem += food_chosen['Frozen Broccoli'] + food_chosen['Celery, Raw'] <= 1 

## Part 3: at least 3x protein source must be included. I'll use a similar method as the 
# constraint built in part 2 but I am certain there is a more efficient way of doing this with
# pandas using iloc/loc.

problem += food_chosen['Tofu'] + food_chosen['Roasted Chicken'] + food_chosen['Poached Eggs'] \
	+food_chosen['Scrambled Eggs'] + food_chosen['Bologna,Turkey'] \
	+food_chosen['Frankfurter, Beef'] \
	+food_chosen['Ham,Sliced,Extralean'] + food_chosen['Kielbasa,Prk'] + food_chosen['Taco'] \
	+food_chosen['Hamburger W/Toppings'] + food_chosen['Hotdog, Plain'] + food_chosen['Pork'] \
	+food_chosen['Sardines in Oil'] + food_chosen['White Tuna in Water'] + food_chosen['New E Clamchwd,W/Mlk'] \
	+food_chosen['New E Clamchwd,W/Mlk'] + food_chosen['Beanbacn Soup,W/Watr'] >= 3
# Admittely this is including food sources that traditonally would not be considered a true protein
# source such as the clamchowders, bean bacon soupssss, etc. 	


# Constraint 4 (Optionally Created, remove comment to run code
#problem += food_amount['Frozen Broccoli'] <= 5 

# run optimization 
# These are the results of my first optimization. Remove the notes to my second
# constraint above to run the second optimization to ensure broccoli doesn't 
# monopolize the food source. 
problem.solve()

food_dictionary = {}
for f in problem.variables():
	if f.varValue > 0:
		print(f, f.varValue) 
# Chosen_Food_Peanut_Butter 1.0
# Chosen_Food_Poached_Eggs 1.0
# Chosen_Food_Potatoes,_Baked 1.0
# Chosen_Food_Scrambled_Eggs 1.0
# Food_Amounts_Frozen_Broccoli 31.186798
# Food_Amounts_Kielbasa,Prk 0.1
# Food_Amounts_Peanut_Butter 2.4021706
# Food_Amounts_Poached_Eggs 1.0266667
# Food_Amounts_Potatoes,_Baked 0.21249543
# Food_Amounts_Scrambled_Eggs 0.1
# 5.278922683799999 ##Total Cost
food_dictionary[f.name] = f.varValue
total_cost = (value(problem.objective))
print(total_cost)
# total cost comes to 5.265700029

############# Results including constraint #4 ##################
# Looks like more of a balanced diet but far more expensive

# Chosen_Food_Frozen_Broccoli 1.0
# Chosen_Food_Kielbasa,Prk 1.0
# Chosen_Food_Oranges 1.0
# Chosen_Food_Peanut_Butter 1.0
# Chosen_Food_Poached_Eggs 1.0
# Chosen_Food_Popcorn,Air_Popped 1.0
# Chosen_Food_Potatoes,_Baked 1.0
# Chosen_Food_Scrambled_Eggs 1.0
# Chosen_Food_Tomato_Soup 1.0
# Food_Amounts_Frozen_Broccoli 5.0
# Food_Amounts_Kielbasa,Prk 0.1
# Food_Amounts_Oranges 58.276276
# Food_Amounts_Peanut_Butter 2.6575765
# Food_Amounts_Poached_Eggs 1.0266667
# Food_Amounts_Popcorn,Air_Popped 1.9191368
# Food_Amounts_Potatoes,_Baked 3.8872987
# Food_Amounts_Scrambled_Eggs 0.1
# Food_Amounts_Tomato_Soup 0.57099033

#### Total cost over $10!!!!! 
# 10.3682947137
# [Finished in 2.0s]
