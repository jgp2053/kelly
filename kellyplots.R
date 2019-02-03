library(tidyverse)
library(RColorBrewer)

log_utility <- function(bankroll, amount, probability, odds) {
  ifelse(amount >= bankroll, NA, 
         probability * log(1 + odds * amount / bankroll) + 
           (1 - probability) * log(1 - amount / bankroll))
}

kelly <- function(odds, probability) {
  (probability * (odds + 1) - 1) / odds
}

intersection <- function(f1, f2, x_lower, x_higher, accuracy = .001, epsilon = .001){
  result = data.frame(x_values = seq(x_lower, x_higher, by = accuracy)) %>%
    mutate(f1_values = f1(x_values), f2_values = f2(x_values), difference = abs(f2_values - f1_values))
  min_difference = min(result$difference)
  min_entry = result[result$difference == min_difference,]
  if(min_difference < epsilon){
    min_entry$x_values
  }
  else{
    NA
  }
}

my_colors <- RColorBrewer::brewer.pal(8, 'Set1')

# Varying Probability

odds = .8
probability = seq(0, 1, length.out = 1001)
bankroll = 100
amount = c(1, 2, 5, 10, 25, 50, "kelly")
univ = expand.grid(odds = odds, probability = probability, 
                   bankroll = bankroll, amount = amount, 
                   stringsAsFactors = FALSE) %>%
  mutate(
    amount_color = as.factor(amount),
    amount = ifelse(amount == "kelly", kelly(odds, probability) * bankroll, as.integer(amount)),
    utility = log_utility(bankroll = bankroll, odds = odds, 
                          probability = probability, amount = amount)) %>%
  mutate(is_kelly = ifelse(amount_color == "kelly", "Kelly", "non-Kelly"))

maxes = univ %>%
  filter(is_kelly == "non-Kelly") %>%
  group_by(odds, probability, bankroll) %>%
  slice(which.max(utility)) %>%
  select(odds, probability, bankroll, best_bet = amount_color, best_utility = utility)

univ %>%
  left_join(maxes) %>%
  ggplot(aes(x = probability, y = utility)) +
  geom_line(aes(color = amount_color, linetype = is_kelly), size = 1) + 
  # geom_ribbon(aes(ymin = 0, ymax = best_utility, fill = best_bet), alpha = .2) +
  geom_ribbon(aes(ymin = 0, ymax = .1, fill = best_bet), alpha = .1) +
  xlim(.55, .75) + 
  ylim(0, .1) +
  scale_colour_manual(name  ="Bet Amount", breaks = c("1", "2", "5", "10", "25", "50", "kelly"), values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_linetype_manual(values = c("Kelly" = "dashed", "non-Kelly" = "solid")) + 
  guides(linetype = FALSE, fill = FALSE) +
  xlab("Probability") + 
  ylab("Logarithmic Utility") + 
  ggtitle("Expected Logarithmic Utility with Varying Probability", subtitle = "(Odds = .8, Bankroll = 100)") + 
  theme_bw() 

# Varying Odds

odds = seq(1, 3, length.out = 1001)
probability = .4
bankroll = 100000
amount = c(500, 1000, 5000, 10000, "kelly")
univ = expand.grid(odds = odds, probability = probability, 
                   bankroll = bankroll, amount = amount, 
                   stringsAsFactors = FALSE) %>%
  mutate(
    amount_color = as.factor(amount),
    amount = ifelse(amount == "kelly", kelly(odds, probability) * bankroll, as.integer(amount)),
    utility = log_utility(bankroll = bankroll, odds = odds, 
                          probability = probability, amount = amount)) %>%
  mutate(is_kelly = ifelse(amount_color == "kelly", "Kelly", "non-Kelly"))

maxes = univ %>%
  filter(is_kelly == "non-Kelly") %>%
  group_by(odds, probability, bankroll) %>%
  slice(which.max(utility)) %>%
  select(odds, probability, bankroll, best_bet = amount_color, best_utility = utility)

univ %>%
  left_join(maxes) %>%
  ggplot(aes(x = odds, y = utility)) +
  geom_line(aes(color = amount_color, linetype = is_kelly), size = 1) + 
  # geom_ribbon(aes(ymin = 0, ymax = best_utility, fill = best_bet), alpha = .2) +
  geom_ribbon(aes(ymin = 0, ymax = .01, fill = best_bet), alpha = .1) +
  xlim(1.5, 2) + 
  ylim(0, .01) +
  scale_colour_manual(name  ="Bet Amount", breaks = c("500", "1000", "5000", "10000", "kelly"), values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_linetype_manual(values = c("Kelly" = "dashed", "non-Kelly" = "solid")) + 
  guides(linetype = FALSE, fill = FALSE) +
  xlab("Odds") + ylab("Logarithmic Utility") + 
  ggtitle("Expected Logarithmic Utility with Varying Odds", subtitle = "(Probability = .4, Bankroll = 100000)") + 
  theme_bw()

# Varying Bankroll

odds = .9
probability = .6
bankroll = seq(1, 1000, length.out = 1001)
amount = c(1, 2, 5, 10, 25, 50, 100, "kelly")
univ = expand.grid(odds = odds, probability = probability, 
                   bankroll = bankroll, amount = amount, 
                   stringsAsFactors = FALSE) %>%
  mutate(
    amount_color = as.factor(amount),
    amount = ifelse(amount == "kelly", kelly(odds, probability) * bankroll, as.integer(amount)),
    utility = log_utility(bankroll = bankroll, odds = odds, 
                          probability = probability, amount = amount)) %>%
  mutate(is_kelly = ifelse(amount_color == "kelly", "Kelly", "non-Kelly"))

maxes = univ %>%
  filter(is_kelly == "non-Kelly") %>%
  group_by(odds, probability, bankroll) %>%
  slice(which.max(utility)) %>%
  select(odds, probability, bankroll, best_bet = amount_color, best_utility = utility)

univ %>%
  left_join(maxes) %>%
  ggplot(aes(x = bankroll, y = utility)) +
  geom_line(aes(color = amount_color, linetype = is_kelly), size = 1) + 
  # geom_ribbon(aes(ymin = 0, ymax = best_utility, fill = best_bet), alpha = .2) +
  geom_ribbon(aes(ymin = 0, ymax = .011, fill = best_bet), alpha = .1) +  
  ylim(0, .011) + 
  xlim(0, 600) +
  scale_colour_manual(name  ="Bet Amount", breaks = c("1", "2", "5", "10", "25", "50", "100", "kelly"), values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_linetype_manual(values = c("Kelly" = "dashed", "non-Kelly" = "solid")) + 
  guides(linetype=FALSE, fill = FALSE) +
  xlab("Bankroll") + ylab("Logarithmic Utility") + 
  ggtitle("Expected Logarithmic Utility with Varying Bankroll", subtitle = "(Probability = .6, Odds = .9)") + 
  theme_bw()
