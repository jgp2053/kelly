library(tidyverse)

# function to calculate expected logarithmic growth
log_utility <- function(bet_amount, bankroll, probability, odds) {
  if_else(
    bet_amount >= bankroll,
    # return NA if bet cannot be made
    NA_real_, 
    probability * log(1 + odds * bet_amount / bankroll) + 
      (1 - probability) * log(1 - bet_amount / bankroll)
    )
}

# get theoretical kelly fraction given odds and probability
kelly <- function(odds, probability) {
  (probability * (odds + 1) - 1) / odds
}

# create data set with varying odds, bankroll, and probability
# calculate the utility of each discrete bet bet_amount and label the best bet
create_universal_set <- function(bankroll, bet_amount, probability, odds){
  # add kelly to the list of supplied bet bet_amounts
  bet_amount = c(bet_amount, "Theoretical Kelly")
  univ = expand_grid(
    bet_amount = bet_amount,
    bankroll = bankroll, 
    probability = probability, 
    odds = odds
    ) %>%
    mutate(
      is_kelly = if_else(bet_amount == "Theoretical Kelly", "Kelly", "non-Kelly"),
      # keep a factorized version of the bet bet_amount around to label graphs
      bet_amount_color = as_factor(bet_amount),
      # calculate the kelly bet amount
      bet_amount = if_else(
        is_kelly == "Kelly", 
        kelly(odds, probability) * bankroll, 
        as.numeric(bet_amount)
        ),
      # calculate the expected log utility for each situation and bet amount
      utility = log_utility(
        bet_amount = bet_amount,
        bankroll = bankroll, 
        probability = probability, 
        odds = odds
        )
      )
  
  # at each point, find the discrete bet amount that gives the best
  # expected logarithmic growth
  maxes <- univ %>%
    filter(is_kelly == "non-Kelly") %>%
    group_by(odds, probability, bankroll) %>%
    slice(which.max(utility)) %>%
    select(
      odds, 
      probability, 
      bankroll, 
      best_bet_color = bet_amount_color,
      best_bet = bet_amount,
      best_utility = utility
      )
  
  univ %>%
    left_join(maxes)
}


my_colors <- c("#000000", "#56B4E9", "#E69F00", "#F0E442", 
               "#009E73", "#0072B2", "#D55E00", "#CC7947")

### Varying Probability

intersection_varyp <- function(b, a, w1, w2){
  log((b - w2)/(b - w1)) / (
    log((b + a*w1)/(b + a*w2)) +
    log((b - w2)/(b - w1))
  )
}

varyp_point_i_x <- 2/3
varyp_point_i_y <- log_utility(kelly(.9, 2/3) * 100, 100, varyp_point_i_x, .9)

varyp_point_ii_x <- intersection_varyp(100,.9, 10, 25)
varyp_point_ii_y <- log_utility(25, 100, varyp_point_ii_x, .9)

varyp_point_iii_x <- intersection_varyp(100,.9, 25, 50)
varyp_point_iii_y <- log_utility(25, 100, varyp_point_iii_x, .9)

varyp_points <- tibble(
  x = c(varyp_point_i_x, varyp_point_ii_x, varyp_point_iii_x),
  y = c(varyp_point_i_y, varyp_point_ii_y, varyp_point_iii_y),
  label = c('I', 'II', 'III')
)

# offsets for point labeling
x_offset = .01
y_offset = -.001

graph_varyp <- create_universal_set(
    bankroll = 100, 
    bet_amount = c(1, 2, 5, 10, 25, 50), 
    probability = seq(0, 1, length.out = 1001), 
    odds = .9
  ) %>%
  ggplot(aes(x = probability, y = utility)) +
  # lines for each bet amount
  geom_line(aes(color = bet_amount_color, linetype = is_kelly), size = 1) +
  # shaded area for best bet
  geom_ribbon(aes(ymin = 0, ymax = .1, fill = best_bet_color), alpha = .1) +
  scale_colour_brewer(
    name = "Bet Amount",
    # need to order bets appropriately
    breaks = c("1", "2", "5", "10", "25", "50", "Theoretical Kelly"), 
    type = 'qual',
    palette = 'Set2'
    ) +
  scale_fill_brewer(type = 'qual', palette = 'Set2') +
  scale_linetype_manual(values = c("Kelly" = "dashed", "non-Kelly" = "solid")) + 
  guides(linetype = FALSE, fill = FALSE, size = FALSE) +
  xlab("Probability") + 
  ylab("Logarithmic Utility") + 
  xlim(.55, .75) + 
  ylim(0, .1) +
  theme_bw() +
  geom_point(
    data = varyp_points,
    aes(x = x, y = y)
    ) +
  geom_text(
    data = varyp_points,
    aes(x = x + x_offset, y = y + y_offset, label = label)
    )

ggsave(
  file = 'varyp.png', 
  plot = graph_varyp, 
  width = 6, 
  height = 4, 
  path = 'intersection_plots'
)

### Varying Odds

x_offset = .025
y_offset = 0

intersection_varya <- function(b, p, w1, w2){
  (b * ((b - w2)/(b - w1))^((1 - p) / p) - b) /
    (w1 - w2 * ((b - w2)/(b - w1))^((1 - p) / p))
}

varya_point_i_x <- .9
varya_point_i_y <- log_utility(kelly(.9, 2/3) * 100000, 100000, 2/3, varya_point_i_x)

varya_point_ii_x <- intersection_varya(100000, 2/3, 10000, 25000)
varya_point_ii_y <- log_utility(25000, 100000, 2/3, varya_point_ii_x)

varya_point_iii_x <- intersection_varya(100000, 2/3, 25000, 50000)
varya_point_iii_y <- log_utility(25000, 100000, 2/3, varya_point_iii_x)

varya_points <- tibble(
  x = c(varya_point_i_x, varya_point_ii_x, varya_point_iii_x),
  y = c(varya_point_i_y, varya_point_ii_y, varya_point_iii_y),
  label = c('I', 'II', 'III')
)

graph_varya <- create_universal_set(
  bankroll = 100000, 
  bet_amount = c(1000, 2000, 5000, 10000, 25000, 50000), 
  probability = 2/3, 
  odds = seq(.5, 1.5, length.out = 1001)
) %>%
  ggplot(aes(x = odds, y = utility)) +
  # lines for each bet amount
  geom_line(aes(color = bet_amount_color, linetype = is_kelly), size = 1) +
  # shaded area for best bet
  geom_ribbon(aes(ymin = 0, ymax = .15, fill = best_bet_color), alpha = .1) +
  scale_colour_brewer(
    name = "Bet Amount",
    # need to order bets appropriately
    breaks = c("1000", "2000", "5000", "10000", "25000", "50000", "Theoretical Kelly"), 
    type = 'qual',
    palette = 'Set2'
  ) +
  scale_fill_brewer(type = 'qual', palette = 'Set2') +
  scale_linetype_manual(values = c("Kelly" = "dashed", "non-Kelly" = "solid")) + 
  guides(linetype = FALSE, fill = FALSE, size = FALSE) +
  xlab("Odds") + 
  ylab("Logarithmic Utility") + 
  xlim(.5, 1.5) +
  ylim(0, .15) +
  theme_bw() +
  geom_point(
    data = varya_points,
    aes(x = x, y = y)
  ) +
  geom_text(
    data = varya_points,
    aes(x = x + x_offset, y = y + y_offset, label = label)
  )

ggsave(
  file = 'varya.png', 
  plot = graph_varya, 
  width = 6, 
  height = 4, 
  path = 'intersection_plots'
)

graph_varya_casino <- create_universal_set(
  bankroll = 100000, 
  bet_amount = c(1000, 2000, 5000, 10000, 25000, 50000), 
  probability = 2/3, 
  odds = seq(.5, 1.5, length.out = 1001)
) %>%
  filter(bet_amount_color == 'Theoretical Kelly') %>%
  mutate(
    discrete_profit = best_bet * (1 - odds),
    theoretical_profit = bet_amount * (1 - odds)
    ) %>%
  select(odds, best_bet_color, discrete_profit, theoretical_profit) %>%
  pivot_longer(
    cols = ends_with('profit'),
    names_to = 'scenario_type',
    values_to = 'casino_profit'
  ) %>%
  mutate(
    scenario_type = if_else(
      scenario_type == 'discrete_profit', 
      'Discrete', 
      'Continuous'
      )
    ) %>%
  ggplot(aes(x = odds, y = casino_profit, linetype = scenario_type)) +
  # lines for discrete and continuous profit
  geom_line() +
  # shaded area for best bet
  geom_ribbon(aes(ymin = -25000, ymax = 10000, fill = best_bet_color), alpha = .1) +
  theme_bw() +
  scale_fill_brewer(
    type = 'qual',
    palette = 'Set2',
    name = 'Kelly Discrete Bet'
    ) +
  scale_linetype_manual(
    values = c("dotted", "solid"),
    name = 'Scenario'
    ) +
  xlab("Odds") + 
  ylab("Casino Profit per Kelly Bettor")

ggsave(
  file = 'varya_casino.png', 
  plot = graph_varya_casino, 
  width = 6, 
  height = 4, 
  path = 'intersection_plots'
)

### Varying Bankroll

# find intersection points for illustration using Newton's method
source("intersection.R")

intersections <- find_intersection_points(2/3, .9, c(10, 25, 50))

varyb_point_i_x <- 100
varyb_point_i_y <- log_utility(kelly(.9, 2/3) * 100, varyb_point_i_x, 2/3, .9)

varyb_point_ii_x <- intersections$intersection[1]
varyb_point_ii_y <- log_utility(25, varyb_point_ii_x, 2/3, .9)

varyb_point_iii_x <- intersections$intersection[2]
varyb_point_iii_y <- log_utility(25, varyb_point_iii_x, 2/3, .9)

varyb_points <- tibble(
  x = c(varyb_point_i_x, varyb_point_ii_x, varyb_point_iii_x),
  y = c(varyb_point_i_y, varyb_point_ii_y, varyb_point_iii_y),
  label = c('I', 'II', 'III')
)

x_offset = 15
y_offset = 0

graph_varyb <- create_universal_set(
  bankroll = seq(1, 350, length.out = 1001), 
  bet_amount = c(1, 2, 5, 10, 25, 50), 
  probability = 2/3,
  odds = .9
) %>%
  ggplot(aes(x = bankroll, y = utility)) +
  # lines for each bet amount
  geom_line(aes(color = bet_amount_color, linetype = is_kelly), size = 1) +
  # shaded area for best bet
  geom_ribbon(aes(ymin = 0, ymax = 0.04045989, fill = best_bet_color), alpha = .1) +
  scale_colour_brewer(
    name = "Bet Amount",
    # need to order bets appropriately
    breaks = c("1", "2", "5", "10", "25", "50", "Theoretical Kelly"), 
    type = 'qual',
    palette = 'Set2'
  ) +
  scale_fill_brewer(type = 'qual', palette = 'Set2') +
  scale_linetype_manual(values = c("Kelly" = "dashed", "non-Kelly" = "solid")) + 
  guides(linetype = FALSE, fill = FALSE, size = FALSE) +
  xlab("Bankroll") + 
  ylab("Logarithmic Utility") + 
  ylim(0, 0.04045989) + 
  xlim(0, 250) +
  theme_bw() +
  geom_point(
    data = varyb_points,
    aes(x = x, y = y)
  ) +
  geom_text(
    data = varyb_points,
    aes(x = x + x_offset, y = y + y_offset, label = label)
  )

ggsave(
  file = 'varyb.png', 
  plot = graph_varyb, 
  width = 6, 
  height = 4, 
  path = 'intersection_plots'
)
