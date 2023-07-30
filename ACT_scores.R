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

#What is the probability of a Z-score greater than 2 
z_scores <- (act_scores - act_mean) / act_sd

mean(z_scores > 2)

#What ACT score value corresponds to 2 standard deviations 
#above the mean (Z = 2)?
act_sd * 2 + act_mean

#What is the 97.5th percentile of act_scores?
qnorm(0.975, act_mean, act_sd)

#Write a function that takes a value and produces the probability 
#of an ACT score less than or equal to that value (the CDF)
#Apply this function to the range 1 to 36 

act_cdf <- function(n){
  mean(act_scores <= n)
}

act_cdf <- sapply(x, act_cdf)

#What is the minimum integer score such that the probability of that 
#score or lower is at least .95?

min(which(act_cdf >= 0.95))

#Use qnorm() to determine the expected 95th percentile, the value for 
#which the probability of receiving that score or lower is 0.95, given a 
#mean score of 20.9 and standard deviation of 5.7.

#What is the expected 95th percentile of ACT scores?

qnorm(0.95, 20.9, 5.7)

#Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), 
#the 1st through 99th percentiles of the act_scores data. Save these as 
#sample_quantiles.
p <- seq(0.01, 0.99, 0.01)
act_scores <- rnorm(10000, 20.9, 5.7)
sample_quantiles <- quantile(act_scores, p)

#In what percentile is a score of 26?

sample_quantiles[max(which(sample_quantiles < 26))]

#Make a corresponding set of theoretical quantiles using qnorm() over the 
#interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard 
#deviation 5.7. Save these as theoretical_quantiles.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)

#Make a QQ-plot graphing sample_quantiles on the y-axis versus 
#theoretical_quantiles on the x-axis.
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()




