set.seed(16)

act_avg <- 20.9
act_sd <- 5.7

act_scores <- data.frame(scores = rnorm(10000, act_avg, act_sd))

#What is the mean of act_scores?
act_mean <- mean(act_scores$scores)

#What is the standard deviation of act_scores?
act_sd <- sd(act_scores$scores)

#In act_scores, how many perfect scores are there 
#out of 10,000 simulated tests?
perfect_scores <- sum(act_scores >= 36)

#In act_scores, what is the probability of an ACT score 
#greater than 30?
1 - pnorm(30, act_mean, act_sd)

#In act_scores, what is the probability of an ACT score 
#less than or equal to 10?
pnorm(10, act_mean, act_sd)

#Set x equal to the sequence of integers 1 to 36. 
#Use dnorm to determine the value of the probability 
#density function over x given a mean of 20.9 and 
#standard deviation of 5.7; save the result as f_x. 
#Plot x against f_x.
x <- seq(1, 36)

f_x <- dnorm(x, 20.9, 5.7)

data.frame(x, f_x) %>% ggplot(aes(x, f_x)) + 
  geom_line()
