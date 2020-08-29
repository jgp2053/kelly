library(tidyverse)

find_intersection_points <- function(probability, odds, bets){
  
  # initialize log utility function
  log_utility <- function(bankroll, amount, probability, odds) {
    ifelse(
      amount >= bankroll, 
      NA, 
      probability * log(1 + odds * amount / bankroll) + 
      (1 - probability) * log(1 - amount / bankroll))
  }
  
  num_bets <- length(bets)
  
  # for each bet, find at which bankroll it matches kelly strategy
  kelly <- (probability * (odds + 1) - 1) / odds
  bet.df <- data.frame(bets, optimal = bets / kelly)
  
  # build a data frame of consecutive bets
  dual.df <- cbind(head(bet.df, num_bets - 1), tail(bet.df, num_bets - 1))
  colnames(dual.df) <- c('bet_1', 'newton_lower', 'bet_2', 'newton_higher')
  
  # run newton-ralphson method on consecutive bets using each curve's
  # maximum to bound the algorithm
  dual.df %>%
    rowwise() %>%
    mutate(
      intersection =
        uniroot(
          function(x) 
            log_utility(x, bet_2, probability, odds) - 
            log_utility(x, bet_1, probability, odds), 
            interval = c(newton_lower, newton_higher)
          ) %>% 
        magrittr::extract2('root')
      ) %>%
    select(bet_1, bet_2, intersection)
}
