#The SAT is a standardized college admissions test used in the United States. 
#The following two multi-part questions will ask you some questions about SAT 
#testing.

#This is a 6-part question asking you to determine some probabilities of what 
#happens when a student guessed for all of their answers on the SAT. Use the 
#information below to inform your answers for the following questions.

#An old version of the SAT college entrance exam had a -0.25 point penalty 
#for every incorrect answer and awarded 1 point for a correct answer. The 
#quantitative test consisted of 44 multiple-choice questions each with 5 answer 
#choices. Suppose a student chooses answers by guessing for all questions 
#on the test.

#What is the probability of guessing correctly for one question?

p_correct <- 1/5

#What is the expected value of points for guessing on one question?

expected_question <- 1 * p_correct + (-0.25 * (1 - p_correct))

#What is the expected score of guessing on all 44 questions?

expected_question * 44

#What is the standard error of guessing on all 44 questions?

n <- 44

standard_error <- sqrt(n) * abs(-0.25 - 1) * sqrt(p_correct * (1 - p_correct))

#Use the Central Limit Theorem to determine the probability that a guessing 
#student scores 8 points or higher on the test.

1-pnorm(8, 0, standard_error)

#Set the seed to 21, then run a Monte Carlo simulation of 10,000 students 
#guessing on the test.
x <- 21
set.seed(x, sample.kind = "Rounding")
B <- 10000

#What is the probability that a guessing student scores 8 points or higher?
p_guessing_8_higher <- replicate(B, {
  X <- sample(c(1, -0.25), n, replace = TRUE, prob = c(p_correct, 1 - p_correct))
  sum(X)
})

mean(p_guessing_8_higher >= 8)



