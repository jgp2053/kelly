library(tidyverse)
library(RColorBrewer)

log_utility <- function(bankroll, amount, probability, odds) {
  return_this <- ifelse(amount >= bankroll, NA, 
                        probability * log(1 + odds * amount / bankroll) + 
                          (1 - probability) * log(1 - amount / bankroll))
  return_this
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

EV <- function(odds, probability, bet, bankroll){
  probability * log(1 + (odds * bet / bankroll)) + (1 - probability) * log(1 - (bet / bankroll))
}

my_colors <- c("#000000", "#56B4E9", "#E69F00", "#F0E442", "#009E73", "#0072B2", "#D55E00", "#CC7947")

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
                          probability = probability, amount = amount))
univ = mutate(univ, is_kelly = ifelse(amount_color == "kelly", "Kelly", "non-Kelly"))


ggplot(univ, aes(x = probability, y = utility, color = amount_color, linetype = is_kelly)) +
  geom_line(size = 1) + xlim(.5, .75) + ylim(0, .1) +
  scale_colour_manual(name  ="Bet Amount", breaks = c("1", "2", "5", "10", "25", "50", "kelly"), values = my_colors) + 
  scale_linetype_manual(values = c("Kelly" = "dashed", "non-Kelly" = "solid")) + guides(linetype=FALSE) +
  xlab("Probability") + ylab("Logarithmic Utility") + 
  ggtitle("Expected Logarithmic Utility with Varying Probability", subtitle = "(Odds = .8, Bankroll = 100)") + theme_bw() + theme(legend.position = c(.2, .6))

