#How many different ways can the 3 medals be distributed across 
#8 runners?

medals_8 <- permutations(8, 3)

medals_8_length <- length(medals_8)/3

#How many different ways can the three medals be distributed 
#among the 3 runners from Jamaica?

medals_3_Jamaica <- permutations(3, 3)

medals_3_Jamaica_length <- length(medals_3_Jamaica)/3

#What is the probability that all 3 medals are won by Jamaica?

P_3_Jamaica <- medals_3_Jamaica_length / medals_8_length

#Run a Monte Carlo simulation 

B <- 10000
set.seed(1)

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

Jamaica_3_winners <- replicate(B, {
  medalists  <- sample(runners, 3)
  unique_countries <- unique(medalists)
  Jamaica_3_winners <- length(unique_countries) == 1 && unique_countries[1] == "Jamaica"
})

mean(Jamaica_3_winners)

