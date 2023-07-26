#Use the information below to answer the following five questions.

#A restaurant manager wants to advertise that his lunch special offers 
#enough choices to eat different meals every day of the year. 
#He doesn't think his current special actually allows that number of 
#choices, but wants to change his special if needed to allow at least 
#365 choices.

#A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. 
#He currently offers a choice of 1 entree from a list of 6 options, 
#a choice of 2 different sides from a list of 6 options, and a choice 
#of 1 drink from a list of 2 options.

#How many meal combinations are possible with the current menu?

sides_options <- 6
entrees_options <- 6
drinks_options <- 2

sides_choices <- 2
entrees_choices <- 1
drinks_choices <- 1

meal_combinations <- choose(entrees_options, entrees_choices) * choose(sides_options, sides_choices) * choose(drinks_options, drinks_choices)

#How many combinations are possible if he expands his original special to 3 drink options?
drinks_options <- 3
meal_combinations_3 <- choose(entrees_options, entrees_choices) * choose(sides_options, sides_choices) * choose(drinks_options, drinks_choices)

#How many meal combinations are there if customers can choose from 
#6 entrees, 3 drinks, and select 3 sides from the current 6 options?
sides_choices <- 3
meal_combinations_3 <- choose(entrees_options, entrees_choices) * choose(sides_options, sides_choices) * choose(drinks_options, drinks_choices)