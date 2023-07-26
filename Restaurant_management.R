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
sides_options <- 6
entrees_options <- 6
drinks_options <- 3

sides_choices <- 2
entrees_choices <- 1
drinks_choices <- 1

meal_combinations_2 <- choose(entrees_options, entrees_choices) * choose(sides_options, sides_choices) * choose(drinks_options, drinks_choices)

#How many meal combinations are there if customers can choose from 
#6 entrees, 3 drinks, and select 3 sides from the current 6 options?
sides_options <- 6
entrees_options <- 6
drinks_options <- 3

sides_choices <- 3
entrees_choices <- 1
drinks_choices <- 1
meal_combinations_3 <- choose(entrees_options, entrees_choices) * choose(sides_options, sides_choices) * choose(drinks_options, drinks_choices)

#Write a function that takes a number of entree choices and returns 
#the number of meal combinations possible given that number of entree 
#options, 3 drink choices, and a selection of 2 sides from 6 options.

meal_combinations_function <- function(entrees_options){
  sides_options <- 6
  drinks_options <- 3
  
  sides_choices <- 2
  entrees_choices <- 1
  drinks_choices <- 1
  
  choose(entrees_options, entrees_choices) * choose(sides_options, sides_choices) * choose(drinks_options, drinks_choices)
}

#Use sapply() to apply the function to entree option counts ranging 
#from 1 to 12.

entrees_options_seq <- seq(1, 12)
sapply(entrees_options_seq, meal_combinations_function)

#Write a function that takes a number of side choices and returns the 
#number of meal combinations possible given 6 entree choices, 3 drink choices, 
#and a selection of 2 sides from the specified number of side choices.

meal_combinations_function_2 <- function(sides_options){
  entree_options <- 6
  drinks_options <- 3
  
  sides_choices <- 2
  entrees_choices <- 1
  drinks_choices <- 1
  
  choose(entrees_options, entrees_choices) * choose(sides_options, sides_choices) * choose(drinks_options, drinks_choices)
}

#Use sapply() to apply the function to side counts ranging from 2 to 12.

sides_options_seq <- seq(2, 12)
sapply(sides_options_seq, meal_combinations_function_2)
