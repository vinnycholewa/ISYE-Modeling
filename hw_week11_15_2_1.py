## Summary: 
# My summary findings yield a different result than what was expected and I will admit there is likely a flaw inherent in my code
# but I must report not what I believe is the answer but rather my summary. My optimization formula derived the optimal diet to be 
# frozen broccoli, peanut butter, poached eggs, and potatoes (practically my diet to cope with this course coupled with CSE) with a cost of approximately $5.27. 
# Please see code/notes below. Good luck graders on the midterm!

##### library setup #####
import pandas as pd 
import numpy as np 
from pulp import *
import seaborn


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
food_amount = LpVariable.dicts("Food Amounts", food_names, 0)

# add the objective function which you can see is the sum product of the cost x amount of food 
problem += lpSum([food_cost[x] * food_amount[x] for x in food_names])


# add constraints for min & max values for each food with the range being >= 1500 and <+ 2500 through list comprehension
for i in range(0, 11):
	problem += lpSum([nutrients[i][j] * food_amount[j] for j in food_names]) >= min_cal[0][i+3], 'minimum calories' + nutrient_names[i]
	problem += lpSum([nutrients[i][j] * food_amount[j] for j in food_names]) >= max_cal[0][i+3], 'maximum calories' + nutrient_names[i]

# run optimization 
problem.solve()

food_dictionary = {}
for f in problem.variables():
	if f.varValue > 0:
		print(f, f.varValue) 
# Food_Amounts_Frozen_Broccoli 31.191075
# Food_Amounts_Peanut_Butter 2.4582804
# Food_Amounts_Poached_Eggs 1.1347518
# Food_Amounts_Potatoes,_Baked 0.20447095
food_dictionary[f.name] = f.varValue
total_cost = (value(problem.objective))
print(total_cost)
# total cost comes to 5.265700029
