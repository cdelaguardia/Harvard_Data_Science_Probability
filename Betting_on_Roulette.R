#A casino offers a House Special bet on roulette, which is a bet on five pockets
#(00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other 
#words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants 
#to know the chance of losing money if he places 500 bets on the roulette House 
#Special.

#The following 7-part question asks you to do some calculations related to this 
#scenario.

#What is the expected value of the payout for one bet?

p_win <- 5/38
p_lose <- 1 - p_win

expected_one_bet <- 6 * p_win + (-1) * p_lose

#What is the standard error of the payout for one bet?
stand_err_one_bet <- abs(-1 - 6) * sqrt(p_win * p_lose)

#What is the expected value of the average payout over 500 bets?
expected_one_bet

#What is the standard error of the average payout over 500 bets?
stand_err_one_bet / sqrt(500)

#What is the expected value of the sum of 500 bets?
expected_one_bet * 500

#What is the standard error of the sum of 500 bets?
stand_err_one_bet * sqrt(500)

#Use pnorm() with the expected value of the sum and standard error of the sum 
#to calculate the probability of losing money over 500 bets

pnorm(0, expected_one_bet * 500, stand_err_one_bet * sqrt(500))