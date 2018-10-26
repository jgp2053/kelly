library(tidyverse)

EV <- function(odds, probability, bankroll, bet){
  probability * log(1 + (odds * bet / bankroll)) + (1 - probability) * log(1 - (bet / bankroll))
}

random_bet <- function(bankroll, bet){
  bet_possible = bet[bet <= bankroll]
  if(length(bet_possible) > 0){
    sample(bet_possible, 1)
  }
  else{
    0
  }
}

max_bet <- function(bankroll, bet){
  bet_possible = bet[bet <= bankroll]
  if(length(bet_possible) > 0){
    max(bet_possible)
  }
  else{
    0
  }
}

kelly_bet <- function(odds, probability, bankroll) {
  finalbet = ((probability * (odds + 1) - 1) / odds) * bankroll
  if(bankroll >= finalbet){
    finalbet
  }
  else{
    0
  }
}

best_fixed_bet <- function(odds, probability, bankroll, bet){
  result = data.frame(bet) %>%
    mutate(expected_value = EV(odds, probability, bankroll, bet))
  result <- result[order(-result$expected_value),]
  finalbet = result$bet[[1]]
  if(bankroll >= finalbet){
    finalbet
  }
  else{
    0
  }
}

best_two_bets <- function(odds, probability, bankroll, bet){
  kelly_bet = kelly_bet(odds, probability, bankroll)
  result = data.frame(bet) %>%
    mutate(kelly_difference = abs(bet - kelly_bet))
  result <- result[order(result$kelly_difference),]
  best_bets <- c(result$bet[[1]], result$bet[[2]])
  decision_RV <- sample(1:2, 1)
  finalbet = best_bets[decision_RV]
  if(bankroll >= finalbet){
    finalbet
  }
  else{
    0
  }
}

bet_sim <- function(initial_bankroll, odds, probability, possible_bets, num_bets){
  bet_result = c("N/A", rbinom(num_bets, 1, probability))
  
  kelly_bankroll = integer(length(num_bets))
  discrete_kelly_bankroll = integer(length(num_bets))
  modified_discrete_kelly_bankroll = integer(length(num_bets))
  random_discrete_bankroll = integer(length(num_bets))
  max_discrete_bankroll = integer(length(num_bets))
  
  kelly_bankroll[1] = initial_bankroll
  discrete_kelly_bankroll[1] = initial_bankroll
  modified_discrete_kelly_bankroll[1] = initial_bankroll
  random_discrete_bankroll[1] = initial_bankroll
  max_discrete_bankroll[1] = initial_bankroll
  
  for (i in 2:(num_bets + 1)){
    if(bet_result[i - 1] == 1){
      kelly_bankroll[i] = (kelly_bankroll[i-1] + odds * kelly_bet(odds, probability, kelly_bankroll[i-1]))
      discrete_kelly_bankroll[i] = (discrete_kelly_bankroll[i-1] + odds * best_fixed_bet(odds, probability, discrete_kelly_bankroll[i-1], possible_bets))
      modified_discrete_kelly_bankroll[i] = (modified_discrete_kelly_bankroll[i-1] + odds * best_two_bets(odds, probability, modified_discrete_kelly_bankroll[i-1], possible_bets))
      random_discrete_bankroll[i] = (random_discrete_bankroll[i-1] + odds * random_bet(random_discrete_bankroll[i-1], possible_bets))
      max_discrete_bankroll[i] = (max_discrete_bankroll[i-1] + odds * max_bet(max_discrete_bankroll[i-1], possible_bets))
    }
    else{
      kelly_bankroll[i] = (kelly_bankroll[i-1] - kelly_bet(odds, probability, kelly_bankroll[i-1]))
      discrete_kelly_bankroll[i] = (discrete_kelly_bankroll[i-1] - best_fixed_bet(odds, probability, discrete_kelly_bankroll[i-1], possible_bets))
      modified_discrete_kelly_bankroll[i] = (modified_discrete_kelly_bankroll[i-1] - best_two_bets(odds, probability, modified_discrete_kelly_bankroll[i-1], possible_bets))
      random_discrete_bankroll[i] = (random_discrete_bankroll[i-1] - random_bet(random_discrete_bankroll[i-1], possible_bets))
      max_discrete_bankroll[i] = (max_discrete_bankroll[i-1] - max_bet(max_discrete_bankroll[i-1], possible_bets))
      }
  }
  data.frame(time = 0:(num_bets), bet_result, kelly_bankroll, discrete_kelly_bankroll, modified_discrete_kelly_bankroll, random_discrete_bankroll, max_discrete_bankroll)
}

multiple_sim <- function(initial_bankroll, odds, probability, possible_bets, num_bets, num_sims){
  result = data.frame()
  for(sim in 1:num_sims){
    simdf = bet_sim(initial_bankroll, odds, probability, possible_bets, num_bets)
    result = rbind(result, simdf)
  }
  result %>%
    group_by(time) %>%
    summarise(kelly_bankroll = mean(kelly_bankroll),
              discrete_kelly_bankroll = mean(discrete_kelly_bankroll),
              modified_discrete_kelly_bankroll = mean(modified_discrete_kelly_bankroll),
              random_discrete_bankroll = mean(random_discrete_bankroll),
              max_discrete_bankroll = mean(max_discrete_bankroll))
}

set.seed(11)
data_run = bet_sim(initial_bankroll = 10, odds = .8, probability = .6, possible_bets = c(1, 2, 5, 10, 25, 50), num_bets = 100)

ggplot(data_run, aes(time)) + 
  geom_line(aes(y = kelly_bankroll, colour = "Theoretical Kelly"), linetype = "dashed") + 
  geom_line(aes(y = discrete_kelly_bankroll, colour = "Discrete Kelly")) + 
  geom_line(aes(y = modified_discrete_kelly_bankroll, colour = "Modified Discrete Kelly")) + 
  geom_line(aes(y = random_discrete_bankroll, colour = "Random Discrete Bet")) + 
  geom_line(aes(y = max_discrete_bankroll, colour = "Max Discrete Bet")) + 
  ggtitle("Simulation Study (Single Run)", subtitle = "(Probability = .6, Odds = .8, Bets = {1, 2, 5, 10, 25, 50})") + 
  scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
  ylab("Bankroll") + 
  xlab("Time") + 
  theme_bw() + 
  theme(legend.position = c(.2, .6))

set.seed(0)
multiple_runs = multiple_sim(initial_bankroll = 100, 
                        odds = .8, 
                        probability = .6, 
                        possible_bets = c(1, 2, 5, 10, 25, 50), 
                        num_bets = 100, 
                        num_sims = 1000)

ggplot(multiple_runs, aes(time)) + 
  geom_line(aes(y = kelly_bankroll, colour = "Theoretical Kelly"), linetype = "dashed") + 
  geom_line(aes(y = discrete_kelly_bankroll, colour = "Discrete Kelly")) + 
  geom_line(aes(y = modified_discrete_kelly_bankroll, colour = "Modified Discrete Kelly")) + 
  geom_line(aes(y = random_discrete_bankroll, colour = "Random Discrete Bet")) + 
  geom_line(aes(y = max_discrete_bankroll, colour = "Max Discrete Bet")) + 
  scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
  ggtitle("Simulation Study (Averaged Over 1000 Runs)", subtitle = "(Probability = .6, Odds = .8, Bets = {1, 2, 5, 10, 25, 50})") + 
  ylab("Bankroll") + 
  xlab("Time") + 
  theme_bw() + 
  theme(legend.position = c(.8, .3))
