library(tidyverse)
library(dslabs)
data(death_prob)

#Use death_prob to determine the death probability of a 50 year old female, p.
p <- death_prob %>% filter(sex == "Female" & age == 50) %>% pull(prob)

#What is the expected value of the company’s net profit on one policy for a 50 year 
#old female?
loss_death <- -150000
premium <- 1150

expected_female_50 <- p * loss_death + premium * (1 - p)

#Calculate the standard error of the profit on one policy for a 
#50 year old female.
standard_error_female_50 <- abs(loss_death - premium) * sqrt(p * (1 - p))

#What is the expected value of the company’s profit over all 1,000 policies for 
#50 year old females?
1000 * expected_female_50

#What is the standard error of the sum of the expected value over all 1,000 
#policies for 50 year old females?
standard_error_female_50 * sqrt(1000)

#Use the Central Limit Theorem to calculate the probability that the insurance company 
#loses money on this set of 1,000 policies.
pnorm(0, 1000 * expected_female_50, standard_error_female_50 * sqrt(1000))

#50 year old males have a different probability of death than 50 year old 
#females. We will calculate a profitable premium for 50 year old males in the 
#following four-part question.

# Use death_prob to determine the probability of death within one year for a 
#50 year old male
p_male <- death_prob %>% filter(sex == "Male" & age == 50) %>% pull(prob)

#Suppose the company wants its expected profits from 1,000 50 year old males 
#with $150,000 life insurance policies to be $700,000.

n <- 1000
loss_death <- -150000
expected_profit <- 700000

premium_male_50 <- ((expected_profit / n) - loss_death * p_male) / (1 - p_male)

#Using the new 50 year old male premium rate, calculate the standard error of 
#the sum of 1,000 premiums.
standard_error_male_50 <- abs(loss_death - premium_male_50) * sqrt(p_male * (1 - p_male))
sqrt(n) * standard_error_male_50

#What is the probability of losing money on a series of 1,000 policies to 50 
#year old males?
pnorm(0, expected_profit, sqrt(n) * standard_error_male_50)

#In this 6-part question, we’ll look at a scenario in which a lethal pandemic 
#disease increases the probability of death within 1 year for a 50 year old 
#to .015. Unable to predict the outbreak, the company has sold 
#1,000 $150,000 life insurance policies for $1,150.

#What is the expected value of the company’s profits over 1,000 policies?
n <- 1000
loss_death_pandemic <- -150000
premium_pandemic <- 1150
p_pandemic_50 <- 0.015

expected_profits_pandemic <- p_pandemic_50 * loss_death_pandemic + (1 - p_pandemic_50) * premium_pandemic
n * expected_profits_pandemic

# What is the standard error of the expected value of the company’s profits 
#over 1,000 policies?
standard_error_pandemic <- abs(loss_death_pandemic - premium_pandemic) * sqrt(p_pandemic_50 * (1 - p_pandemic_50))
standard_error_pandemic * sqrt(n)

#What is the probability of the company losing money?
pnorm(0, n * expected_profits_pandemic, standard_error_pandemic * sqrt(n))

#Suppose the company can afford to sustain one-time losses of $1 million, 
#but larger losses will force it to go out of business.

#What is the probability of losing more than $1 million?
pnorm(-1000000, n * expected_profits_pandemic, standard_error_pandemic * sqrt(n))

#Investigate death probabilities p <- seq(.01, .03, .001).

#What is the lowest death probability for which the chance of losing money 
#exceeds 90%?
p_seq <- seq(.01, .03, .001)
loss_death_pandemic <- -150000
premium_pandemic <- 1150
n <- 1000

values <- function(x){
  mu <- (x * loss_death_pandemic + (1 - x) * premium_pandemic) * n
  sd <- abs(loss_death_pandemic - premium_pandemic) * sqrt(x * (1 - x)) * sqrt(n)
  pnorm(0, mu, sd)
}

p_values <- sapply(p_seq, values)

pandemic <- data.frame(prob_death = p_seq, prob_lose_money = p_values)

pandemic_90 <- pandemic %>% filter(prob_lose_money > 0.9)

min(pandemic_90$prob_death)

# Investigate death probabilities p <- seq(.01, .03, .0025).

#What is the lowest death probability for which the chance of losing over 
#$1 million exceeds 90%?
p_seq <- seq(.01, .03, .0025)
loss_death_pandemic <- -150000
premium_pandemic <- 1150
n <- 1000

values_million <- function(x){
  mu <- (x * loss_death_pandemic + (1 - x) * premium_pandemic) * n
  sd <- abs(loss_death_pandemic - premium_pandemic) * sqrt(x * (1 - x)) * sqrt(n)
  pnorm(-1000000, mu, sd)
}

p_values_million <- sapply(p_seq, values_million)

lose_million <- data.frame(prob_death = p_seq, prob_lose_money = p_values_million)
lose_million <- lose_million %>% filter(prob_lose_money > 0.9)
min(lose_million$prob_death)

#Define a sampling model for simulating the total profit over 1,000 loans 
#with probability of claim p_loss = .015, loss of -$150,000 on a claim, 
#and profit of $1,150 when there is no claim. Set the seed to 25, then run 
#the model once.
set.seed(25, sample.kind = "Rounding")
n <- 1000
p_loss <- 0.015
loss <- -150000
profit <- 1150

#What is the reported profit (or loss) in millions (that is, divided by 10^6
sum(sample(c(loss, profit), n, prob = c(p_loss, 1 - p_loss), replace = TRUE))/10^6

#Set the seed to 27, then run a Monte Carlo simulation of your sampling model
#with 10,000 replicates to simulate the range of profits/losses over 1,000 
#loans.
set.seed(27, sample.kind = "Rounding")
B <- 10^4

results <- replicate(B, {
  sum(sample(c(loss, profit), n, prob = c(p_loss, 1 - p_loss), replace = TRUE))
})

#What is the observed probability of losing $1 million or more?
mean(results < -(10^6))

#Suppose that there is a massive demand for life insurance due to the 
#pandemic, and the company wants to find a premium cost for which the 
#probability of losing money is under 5%, assuming the death rate stays 
#stable at p=0.015

n <- 1000
p_loss <- 0.015
loss <- -150000

#Calculate the premium required for a 5% chance of losing money given n = 1000, 
#loans, probability of death p = 0.015, and loss per claim l = -150000. 
#Save this premium as x for use in further questions
z <- qnorm(0.05)
x <- -loss * ( n * p_loss - z * sqrt(n * p_loss * (1 - p_loss))) / ( n * (1 - p_loss) + z * sqrt(n * p_loss * (1 - p_loss)))
x

# What is the expected profit per policy at this rate?
p_loss * loss + (1 - p_loss) * x

#What is the expected profit over 1,000 policies?
expected_profit <- (p_loss * loss + (1 - p_loss) * x) * n
expected_profit

#Run a Monte Carlo simulation with B = 10000 to determine the probability of 
#losing money on 1,000 policies given the new premium x, loss on a claim of 
#$150,000, and probability of claim p = .015. Set the seed to 28 before 
#running your simulation.
set.seed(28, sample.kind = "Rounding")
B <- 10^4
n <- 10^3
loss <- -15 * 10^4
p_loss <- 0.015

profits <- replicate(B, {
  sum(sample(c(loss, x), n, prob = c(p_loss, 1 - p_loss), replace = TRUE))
  })

mean(profits < 0)

#The company cannot predict whether the pandemic death rate will stay stable.
#Set the seed to 29, then write a Monte Carlo simulation that for each of 
#B = 10000 iterations:
#randomly changes p by adding a value between -0.01 and 0.01 with 
#sample(seq(-0.01, 0.01, length = 100), 1)
set.seed(29, sample.kind = "Rounding")
B <- 10^4
p_loss <- 0.015
loss <- -15 * 10^4
n <- 10^3

#What is the expected value over 1,000 policies
results <- replicate(B, {
  new_p_loss <- p_loss + sample(seq(-0.01, 0.01, length = 100), 1)
  var <- sample(c(loss, x), n, prob = c(new_p_loss, 1 - new_p_loss), replace = TRUE)
  sum(var)
})

#What is the probability of losing money?
mean(results < 0)

#What is the probability of losing more than $1 million?
mean(results < -(10^6))
