EV <- function(odds, probability, bankroll, bet){
  probability * log(1 + (odds * bet / bankroll)) + (1 - probability) * log(1 - (bet / bankroll))
}

ten_bet <- function(bankroll){
  finalbet = 10
  if(bankroll >= finalbet){
    finalbet
  }
  else{
    0
  }
}

half_bet <- function(bankroll){
  finalbet = .5 * bankroll
  if(bankroll >= finalbet){
    finalbet
  }
  else{
    0
  }
}

random_bet <- function(bankroll){
  finalbet = runif(1, 0, 1) * bankroll
  if(bankroll >= finalbet){
    finalbet
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

best_fixed_bet <- function(odds, probability, bankroll, bet = c(1, 2, 10, 50)){
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

best_two_bets <- function(odds, probability, bankroll, bet = c(1, 2, 10, 50)){
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
  best_fixed_bankroll = integer(length(num_bets))
  best_two_bankroll = integer(length(num_bets))
  ten_bankroll = integer(length(num_bets))
  random_bankroll = integer(length(num_bets))
  half_bankroll = integer(length(num_bets))
  kelly_bankroll[1] = initial_bankroll
  best_fixed_bankroll[1] = initial_bankroll
  best_two_bankroll[1] = initial_bankroll
  ten_bankroll[1] = initial_bankroll
  random_bankroll[1] = initial_bankroll
  half_bankroll[1] = initial_bankroll
  for (i in 2:(num_bets + 1)){
    if(bet_result[i - 1] == 1){
      kelly_bankroll[i] = (kelly_bankroll[i-1] + odds * kelly_bet(odds, probability, kelly_bankroll[i-1]))
      best_fixed_bankroll[i] = (best_fixed_bankroll[i-1] + odds * best_fixed_bet(odds, probability, best_fixed_bankroll[i-1], possible_bets))
      best_two_bankroll[i] = (best_two_bankroll[i-1] + odds * best_two_bets(odds, probability, best_two_bankroll[i-1], possible_bets))
      ten_bankroll[i] = (ten_bankroll[i-1] + odds * ten_bet(ten_bankroll[i-1]))
      random_bankroll[i] = (random_bankroll[i-1] + odds * random_bet(random_bankroll[i-1]))
      half_bankroll[i] = (half_bankroll[i-1] + odds * half_bet(half_bankroll[i-1]))
    }
    else{
      kelly_bankroll[i] = (kelly_bankroll[i-1] - kelly_bet(odds, probability, kelly_bankroll[i-1]))
      best_fixed_bankroll[i] = (best_fixed_bankroll[i-1] - best_fixed_bet(odds, probability, best_fixed_bankroll[i-1], possible_bets))
      best_two_bankroll[i] = (best_two_bankroll[i-1] - best_two_bets(odds, probability, best_two_bankroll[i-1], possible_bets))
      ten_bankroll[i] = ten_bankroll[i-1] - ten_bet(ten_bankroll[i-1])
      random_bankroll[i] = (random_bankroll[i-1] - random_bet(random_bankroll[i-1]))
      half_bankroll[i] = (half_bankroll[i-1] - half_bet(half_bankroll[i-1]))
    }
  }
  data.frame(time = 0:(num_bets), bet_result, kelly_bankroll, best_fixed_bankroll, best_two_bankroll, ten_bankroll, random_bankroll, half_bankroll)
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
              best_fixed_bankroll = mean(best_fixed_bankroll),
              best_two_bankroll = mean(best_two_bankroll),
              ten_bankroll = mean(ten_bankroll),
              random_bankroll = mean(random_bankroll),
              half_bankroll = mean(half_bankroll))
}

data_run = bet_sim(initial_bankroll = 10, odds = .8, probability = 2/3, possible_bets = c(1, 2, 5, 10, 25, 50), num_bets = 100)

data_run = multiple_sim(initial_bankroll = 10, odds = .8, probability = 2/3, possible_bets = c(1, 2, 5, 10, 25, 50), num_bets = 100, num_sims = 100)

ggplot(data_run, aes(time)) + 
  # geom_line(aes(y = kelly_bankroll, colour = "(Theoretical) Kelly")) + 
  # geom_line(aes(y = ten_bankroll, colour = "Always Bet 10")) + 
  # geom_line(aes(y = random_bankroll, colour = "Bet a Random Fraction")) + 
  # geom_line(aes(y = half_bankroll, colour = "Bet Half of Bankroll")) + 
  geom_line(aes(y = best_fixed_bankroll, colour = "Discrete Kelly")) + 
  geom_line(aes(y = best_two_bankroll, colour = "Best Two Bets")) + 
  ggtitle("Evolution of Bankroll Over Time with Different Strategies", subtitle = "(Probability = 2/3, Odds = .8, Bets = {1, 2, 5, 10, 25, 50}), Averaged Over 100 Sims") + 
  scale_colour_discrete(name  = "Bet Strategy") + 
  ylab("Bankroll") + 
  xlab("Time")
