library(tidyverse)

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

my_colors <- c("#000000", "#56B4E9", "#E69F00", "#F0E442", 
               "#009E73", "#0072B2", "#D55E00", "#CC7947")

# Varying Probability

x_offset = .005
y_offset = -.001

odds = .8
probability = seq(0, 1, length.out = 1001)
bankroll = 100
amount = c(1, 2, 5, 10, 25, 50, "kelly")
univ = expand.grid(odds = odds, probability = probability, 
                   bankroll = bankroll, amount = amount, 
                   stringsAsFactors = FALSE) %>%
  mutate(
    amount_color = as.factor(amount),
    amount = ifelse(amount == "kelly", 
                    kelly(odds, probability) * bankroll, 
                    as.integer(amount)),
    utility = log_utility(bankroll = bankroll, odds = odds, 
                          probability = probability, amount = amount)) %>%
  mutate(is_kelly = ifelse(amount_color == "kelly", "Kelly", "non-Kelly"))

maxes = univ %>%
  filter(is_kelly == "non-Kelly") %>%
  group_by(odds, probability, bankroll) %>%
  slice(which.max(utility)) %>%
  select(odds, 
         probability, 
         bankroll, 
         best_bet = amount_color, 
         best_utility = utility)

graph_varyp <- univ %>%
  left_join(maxes) %>%
  ggplot(aes(x = probability, y = utility)) +
  geom_line(aes(color = amount_color, linetype = is_kelly), size = 1) +
  # geom_ribbon(aes(ymin = 0, ymax = best_utility, fill = best_bet), alpha = .1) +
  geom_ribbon(aes(ymin = 0, ymax = .1, fill = best_bet), alpha = .1) +
  xlim(.55, .75) + 
  ylim(0, .1) +
  scale_colour_manual(name  ="Bet Amount", 
                      breaks = c("1", "2", "5", "10", "25", "50", "kelly"), 
                      values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_linetype_manual(values = c("Kelly" = "dashed", "non-Kelly" = "solid")) + 
  guides(linetype = FALSE, fill = FALSE, size = FALSE) +
  xlab("Probability") + 
  ylab("Logarithmic Utility") + 
  # ggtitle("Expected Logarithmic Utility with Varying Probability", 
  #         subtitle = "(Odds = .8, Bankroll = 100)") + 
  theme_bw() +
  geom_point(aes(x=0.7245419, y=0.05285527)) +
  geom_text(aes(x=0.7245419 + x_offset, y=0.05285527 + y_offset, label = 'B')) +
  geom_point(aes(x=0.6337606, y=0.0101877)) +
  geom_text(aes(x=0.6337606 + x_offset, y=0.0101877 + y_offset, label = 'A')) +
  geom_point(aes(x=2/3, y=0.02565368)) +
  geom_text(aes(x=2/3 + x_offset, y=0.02565368 + y_offset, label = 'C'))

ggsave(
  file = 'varyp.png', 
  plot = graph_varyp, 
  width= 6, 
  height = 4, 
  path = 'intersection_plots'
)

# Varying Odds

x_offset = .01
y_offset = -.00025

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

graph_varya <- univ %>%
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
  # ggtitle("Expected Logarithmic Utility with Varying Odds", subtitle = "(Probability = .4, Bankroll = 100000)") + 
  theme_bw() +
  geom_point(aes(x=1.62132, y=0.0004030681)) +
  geom_text(aes(x=1.62132 + x_offset, y=0.0004030681 + y_offset, label = 'B')) +
  geom_point(aes(x=1.845514, y=0.004529356)) +
  geom_text(aes(x=1.845514 + x_offset, y=0.004529356 + y_offset, label = 'C')) +
  geom_point(aes(x=1.8, y=0.003695102)) +
  geom_text(aes(x=1.8 + x_offset, y=0.003695102 + y_offset, label = 'A'))

ggsave(
  file = 'varya.png', 
  plot = graph_varya, 
  width= 6, 
  height = 4, 
  path = 'intersection_plots'
)

# Varying Bankroll

odds <- .9
probability <- .6
bankroll <- seq(1, 1000, length.out = 1001)
amount <- c(1, 2, 5, 10, 25, 50, 100, "kelly")
univ <- 
  expand.grid(
  odds = odds, 
  robability = probability, 
  bankroll = bankroll, 
  amount = amount, 
  stringsAsFactors = FALSE
  ) %>%
  mutate(
    amount_color = as.factor(amount),
    amount = iflelse(amount == "kelly", kelly(odds, probability) * bankroll, as.integer(amount)),
    utility = log_utility(
      bankroll = bankroll, 
      odds = odds, 
      probability = probability, 
      amount = amount
      )
    ) %>%
  mutate(is_kelly = iflelse(amount_color == "kelly", "Kelly", "non-Kelly"))

maxes <- univ %>%
  filter(is_kelly == "non-Kelly") %>%
  group_by(odds, probability, bankroll) %>%
  slice(which.max(utility)) %>%
  select(odds, probability, bankroll, best_bet = amount_color, best_utility = utility)

x_offset = 5
y_offset = -.0005

my_colors <- RColorBrewer::brewer.pal(8, "Dark2")
names(my_colors) <- univ$amount_color %>%
  levels()

graph_varyb <- 
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
  xlab("Bankroll") + ylab("Expected Logarithmic Utility") + 
  # ggtitle("Expected Logarithmic Utility with Varying Bankroll", subtitle = "(Probability = .6, Odds = .9)") + 
  theme_bw() + 
  geom_point(aes(x=241.618691, y=0.009733776)) +
  geom_text(aes(x=241.618691 + x_offset, y=0.009733776 + y_offset, label = 'B')) +
  geom_point(aes(x=483.237382, y=0.009733776)) +
  geom_text(aes(x=483.237382 + x_offset, y=0.009733776 + y_offset, label = 'C')) +
  geom_point(aes(x=400, y=0.01055328)) +
  geom_text(aes(x=400 + x_offset, y=0.01055328 + y_offset, label = 'A'))

ggsave(
  file = 'varyb.png', 
  plot = graph_varyb, 
  width= 6, 
  height = 4, 
  path = 'intersection_plots'
)
