find_intersection_points <- function(probability, odds, bets){
  num_bets <- length(bets)
  kelly <- (probability * (odds + 1) - 1) / odds
  bet.df <- data.frame(bets, optimal = bets / kelly)
  dual.df <- cbind(head(df, num_bets - 1), tail(df, num_bets - 1))
  colnames(dual.df) <- c('bet_1', 'optimal_bet_1', 'bet_2', 'optimal_bet_2')
  intersection.df <- dual.df %>%
    rowwise() %>%
    mutate(newton_start = mean(c(optimal_bet_1, optimal_bet_2))) %>%
    select(bet_1, bet_2, newton_start)
}