library(tidyverse)
library(profvis)

profvis({

# Define colors for lines in graphs to follow
my_colors <- RColorBrewer::brewer.pal(6, "Dark2")

# Expected Logarithmic Growth
EV <- function(odds, probability, bankroll, bet){
  # TODO: fix logic for floating point errors
  adjusted_fraction = pmin(bet / bankroll, 1)
  return(probability * log(1 + (odds * adjusted_fraction)) + (1 - probability) * log(1 - adjusted_fraction))
}

# Find the two values in haystack that 'bookend' needle
# Returns a vector of length 2
find_bookends <- function(needle, haystack){
  # If there are no values in haystack, return 0
  if(length(haystack) == 0){
    return(0)
  }
  # If there is only one value in haystack, return that value
  if(length(haystack) == 1){
    return(haystack[1])
  }
  # If all of the values in haystack are bigger than needle, return the max value
  else if((maximum <- max(haystack)) < needle){
    return(maximum)
  }
  # If all of the values in haystack are smaller than needle, return the min value
  else if((minimum <- min(haystack)) > needle){
    return(minimum)
  }
  # Otherwise, find the bookend values
  else {
    lower <- max(haystack[haystack <= needle])
    higher <- min(haystack[haystack >= needle])
    return(c(lower, higher)) 
  }
}

# TODO: fix issue with floating point storage
precision <- .000001

# Betting strategy: pick a random bet out of the set of discrete options
random_bet <- function(odds = NA, probability = NA, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  
  if(length(allowed_bets) = 0){
    return(0)
  }
  else if(length(allowed_bets) == 1){
    return(allowed_bets)
  }
  else{
    return(sample(allowed_bets, 1))
  }
}

# Betting strategy: pick the highest bet from the discrete options that can be afforded
max_bet <- function(odds = NA, probability = NA, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) > 0){
    return(max(allowed_bets))
  }
  else{
    return(0)
  }
}

# Betting strategy: bet the kelly bet, regardless of the discrete options
kelly_bet <- function(odds, probability, bankroll, possible_bets = NA) {
  return(((probability * (odds + 1) - 1) / odds) * bankroll)
}

# Betting strategy: use suggested methodology to make kelly bet
best_fixed_bet <- function(odds, probability, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) > 0){
    return(allowed_bets[which.max(EV(odds, probability, bankroll, allowed_bets))])
  }
  else{
    return(0)
  }
}

# Betting strategy: select randomly from the bets that bookend the kelly bet
best_two_bets <- function(odds, probability, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) > 0){
    best_bets <- find_bookends(kelly_bet(odds, probability, bankroll), allowed_bets)
    return(best_bets[sample(length(best_bets), 1)])
    }
  else{
    return(0)
  }
}

# Betting strategy: select the bookend bet closest to the kelly bet
closest_bookend_bet <- function(odds, probability, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) > 0){
    k_bet <- kelly_bet(odds, probability, bankroll, allowed_bets)
    best_bets <- find_bookends(k_bet, allowed_bets)
    return(best_bets[which(abs(best_bets - k_bet) == min(abs(best_bets - k_bet)))])
  }
  else{
    return(0)
  }
}


# ATTEMPT AT CLASSES
# kelly <- list(bankroll = c(initial_bankroll, integer(length(num_bets) - 1)),
#               bet_fn = kelly_bet)

# Simulate a run and output the bankrolls associated with various betting strategies
bet_sim <- function(initial_bankroll, odds, probability, possible_bets, num_bets){
  # Determine wins and losses
  bet_result = c(NA, rbinom(num_bets, 1, probability))
  
  # Initialize bankroll vectors
  kelly_bankroll = integer(length(num_bets))
  discrete_kelly_bankroll = integer(length(num_bets))
  modified_discrete_kelly_bankroll = integer(length(num_bets))
  random_discrete_bankroll = integer(length(num_bets))
  max_discrete_bankroll = integer(length(num_bets))
  closest_bookend_bankroll = integer(length(num_bets))
  
  # Set the initial bankrolls
  kelly_bankroll[1] = initial_bankroll
  discrete_kelly_bankroll[1] = initial_bankroll
  modified_discrete_kelly_bankroll[1] = initial_bankroll
  random_discrete_bankroll[1] = initial_bankroll
  max_discrete_bankroll[1] = initial_bankroll
  closest_bookend_bankroll[1] = initial_bankroll
  
  # Bet num_bets times
  for (i in 2:(num_bets + 1)){
    
    kelly_bankroll[i] <- kelly_bankroll[i-1] + ifelse(bet_result[i], odds, -1) * kelly_bet(odds, probability, kelly_bankroll[i-1], possible_bets)
    discrete_kelly_bankroll[i] <- discrete_kelly_bankroll[i-1] + ifelse(bet_result[i], odds, -1) * best_fixed_bet(odds, probability, discrete_kelly_bankroll[i-1], possible_bets)
    modified_discrete_kelly_bankroll[i] <- modified_discrete_kelly_bankroll[i-1] + ifelse(bet_result[i], odds, -1) * best_two_bets(odds, probability, modified_discrete_kelly_bankroll[i-1], possible_bets)
    random_discrete_bankroll[i] <- random_discrete_bankroll[i-1] + ifelse(bet_result[i], odds, -1) * random_bet(odds, probability, random_discrete_bankroll[i-1], possible_bets)
    max_discrete_bankroll[i] <- max_discrete_bankroll[i-1] + ifelse(bet_result[i], odds, -1) * max_bet(odds, probability, max_discrete_bankroll[i-1], possible_bets)
    closest_bookend_bankroll[i] <- closest_bookend_bankroll[i-1] + ifelse(bet_result[i], odds, -1) * closest_bookend_bet(odds, probability, closest_bookend_bankroll[i-1], possible_bets)

    }
  return (
    data.frame(
      time = 0:num_bets, 
      bet_result, 
      kelly_bankroll, 
      discrete_kelly_bankroll, 
      modified_discrete_kelly_bankroll, 
      random_discrete_bankroll, 
      max_discrete_bankroll, 
      closest_bookend_bankroll
    )
  )
}

# Simulate a series of runs and take the average bankroll at each step
multiple_sim <- function(initial_bankroll, odds, probability, possible_bets, num_bets, num_sims){
  result = data.frame()
  for(sim in 1:num_sims){
    simdf = bet_sim(initial_bankroll, odds, probability, possible_bets, num_bets)
    result = rbind(result, simdf)
  }
  result %>%
    group_by(time) %>%
    summarise(kelly_bankroll = median(kelly_bankroll),
              discrete_kelly_bankroll = median(discrete_kelly_bankroll),
              modified_discrete_kelly_bankroll = median(modified_discrete_kelly_bankroll),
              random_discrete_bankroll = median(random_discrete_bankroll),
              max_discrete_bankroll = median(max_discrete_bankroll),
              closest_bookend_bankroll = median(closest_bookend_bankroll))
}

set.seed(2)

data_run = bet_sim(
  initial_bankroll = 100, 
  odds = .8, 
  probability = .6, 
  possible_bets = 2^seq(1, 100), 
  num_bets = 10^5
)

})

ggplot(data_run, aes(x = time)) + 
  # geom_line(aes(y = kelly_bankroll, colour = "Theoretical Kelly"), linetype = "dashed") + 
  geom_line(aes(y = discrete_kelly_bankroll, colour = "Discrete Kelly")) + 
  geom_line(aes(y = modified_discrete_kelly_bankroll, colour = "Random Bookend Bet: Modified Discrete Kelly")) + 
  geom_line(aes(y = random_discrete_bankroll, colour = "Random Discrete Bet")) + 
  geom_line(aes(y = max_discrete_bankroll, colour = "Max Discrete Bet")) + 
  geom_line(aes(y = closest_bookend_bankroll, colour = "Closest Bookend Bet: Modified Discrete Kelly")) + 
  ggtitle("Simulation Study (Single Run)", subtitle = "(Probability = .6, Odds = .8, Bets = {1, 2, 5, 10, 25, 50})") + 
  scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
  ylab("Bankroll") + 
  xlab("Time") + 
  theme_bw() + 
  theme(legend.position = c(.2, .6))

# set.seed(1000)
# multiple_runs = multiple_sim(initial_bankroll = 100, 
#                         odds = .8, 
#                         probability = .6, 
#                         possible_bets = c(1, 2, 5, 10, 25, 50), 
#                         num_bets = 100, 
#                         num_sims = 100)
# 
# ggplot(multiple_runs, aes(time)) + 
#   geom_line(aes(y = kelly_bankroll, colour = "Theoretical Kelly"), linetype = "dashed") + 
#   geom_line(aes(y = discrete_kelly_bankroll, colour = "Discrete Kelly")) + 
#   geom_line(aes(y = modified_discrete_kelly_bankroll, colour = "Random Bookend Bet: Modified Discrete Kelly")) + 
#   geom_line(aes(y = random_discrete_bankroll, colour = "Random Discrete Bet")) + 
#   geom_line(aes(y = max_discrete_bankroll, colour = "Max Discrete Bet")) + 
#   geom_line(aes(y = closest_bookend_bankroll, colour = "Closest Bookend Bet: Modified Discrete Kelly")) +  
#   scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
#   ggtitle("Simulation Study (Averaged Over 1000 Runs)", subtitle = "(Probability = .6, Odds = .8, Bets = {1, 2, 5, 10, 25, 50})") + 
#   ylab("Bankroll") + 
#   xlab("Time") + 
#   ylim(0, 200) +
#   theme_bw() + 
#   theme(legend.position = c(.8, .3))
