library(tidyverse)
library(directlabels)
library(purrr)

# Define colors for lines in graphs to follow
my_colors <- RColorBrewer::brewer.pal(6, "Dark2")

# Expected Logarithmic Growth
EV <- function(odds, probability, bankroll, bet){
  # TODO: fix logic for floating point errors
  adjusted_fraction = pmin(bet / bankroll, 1)
  return(
    probability * log(1 + (odds * adjusted_fraction)) + 
    (1 - probability) * log(1 - adjusted_fraction)
    )
}

# Find the two values in haystack that 'bookend' needle
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
  if(length(allowed_bets) == 0){
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
kelly_bet <- function(odds, probability, bankroll, possible_bets) {
  return(((probability * (odds + 1) - 1) / odds) * bankroll)
}

# Betting strategy: use suggested methodology to make kelly bet
best_fixed_bet <- function(odds, probability, bankroll, possible_bets){
  allowed_bets <- possible_bets[possible_bets <= (bankroll + precision)]
  if(length(allowed_bets) > 0){
    best_bets <- find_bookends(kelly_bet(odds, probability, bankroll), allowed_bets)
    return(best_bets[which.max(EV(odds, probability, bankroll, best_bets))])
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

# Simulate a run and output the bankrolls associated with various betting strategies
bet_sim <- function(initial_bankroll, odds, probability, possible_bets, num_bets){
  # Determine wins and losses
  bet_result = c(NA, rbinom(num_bets, 1, probability))

  # Initialize bankroll vectors
  kelly_bankroll = integer(length(num_bets))
  discrete_kelly_bankroll = integer(length(num_bets))
  random_bookend_bankroll = integer(length(num_bets))
  closest_bookend_bankroll = integer(length(num_bets))
  # random_discrete_bankroll = integer(length(num_bets))
  # max_discrete_bankroll = integer(length(num_bets))
  
  # Set the initial bankrolls
  kelly_bankroll[1] = initial_bankroll
  discrete_kelly_bankroll[1] = initial_bankroll
  closest_bookend_bankroll[1] = initial_bankroll
  random_bookend_bankroll[1] = initial_bankroll
  # random_discrete_bankroll[1] = initial_bankroll
  # max_discrete_bankroll[1] = initial_bankroll

  # check if the discrete kelly and closest bookend strategies differ
  diff_bet = integer(length(num_bets))
  diff_bet[1] = NA
  
  # Bet num_bets times
  for (i in 2:(num_bets + 1)){
    
    # if the result is a win, the bankroll increases by odds * the bet
    # otherwise it decreases by the bet
    result_multiplier <- ifelse(bet_result[i], odds, -1)
    
    kelly_bankroll[i] <- 
      kelly_bankroll[i-1] + (
        result_multiplier *
        kelly_bet(odds, probability, kelly_bankroll[i-1], possible_bets)
      )
    
    discrete_kelly_bankroll[i] <- 
      discrete_kelly_bankroll[i-1] + (
        result_multiplier *
        best_fixed_bet(odds, probability, discrete_kelly_bankroll[i-1], possible_bets)
      )
    
    closest_bookend_bankroll[i] <- 
      closest_bookend_bankroll[i-1] + (
        result_multiplier *
        closest_bookend_bet(odds, probability, closest_bookend_bankroll[i-1], possible_bets)
      )
    
    diff_bet[i] <- 
      near(
        best_fixed_bet(odds, probability, discrete_kelly_bankroll[i-1], possible_bets),
        closest_bookend_bet(odds, probability, discrete_kelly_bankroll[i-1], possible_bets)
      )
    
    random_bookend_bankroll[i] <- 
      random_bookend_bankroll[i-1] + (
        result_multiplier *
        best_two_bets(odds, probability, random_bookend_bankroll[i-1], possible_bets)
      )
    
    
    # random_discrete_bankroll[i] <- 
    #   random_discrete_bankroll[i-1] + (
    #     result_multiplier *
    #     random_bet(odds, probability, random_discrete_bankroll[i-1], possible_bets)
    #   )
    # max_discrete_bankroll[i] <- 
    #   max_discrete_bankroll[i-1] + (
    #     result_multiplier *
    #     max_bet(odds, probability, max_discrete_bankroll[i-1], possible_bets)
    #   )
    }
  return (
    data.frame(
      time = 0:num_bets, 
      bet_result, 
      kelly_bankroll, 
      discrete_kelly_bankroll, 
      closest_bookend_bankroll,
      random_bookend_bankroll,
      # random_discrete_bankroll, 
      # max_discrete_bankroll,
      diff_bet
    )
  )
}

# Simulate a series of runs and take the average bankroll at each step
multiple_sim <- function(initial_bankroll, odds, probability, possible_bets, num_bets, num_sims){
  replicate(
    n = num_sims, 
    expr = bet_sim(initial_bankroll, odds, probability, possible_bets, num_bets),
    simplify = FALSE
  ) %>%
    # bind the simulations together
    # (we use the identity function since nothing needs to be done to the dfs)
    map_dfr(identity, .id = "sim_no")
}

set.seed(0)

multiple_run <- multiple_sim(
  initial_bankroll = 100, 
  odds = 1, 
  probability = .525, 
  possible_bets = 2^seq(1, 1023),
  num_bets = 10^4,
  num_sims = 2.5 * 10^3
)

save.image(file = "2500_runs.Rdata")

# median bankroll at each point in time

medians <- multiple_run %>%
  group_by(time) %>%
  summarise(discrete_kelly_bankroll = median(discrete_kelly_bankroll),
            random_bookend_bankroll = median(random_bookend_bankroll),
            closest_bookend_bankroll = median(closest_bookend_bankroll),
            kelly_bankroll = median(kelly_bankroll)) %>%
  gather(key = 'strategy', value = 'bankroll', -time) %>%
  # clean up column value for graph
  mutate(
    strategy = ifelse(
      strategy == 'discrete_kelly_bankroll', 'Discrete Kelly',
      ifelse(
        strategy == 'closest_bookend_bankroll', 'Closest Bookend',
        ifelse(
          strategy == 'random_bookend_bankroll', 'Random Bookend',
          'Kelly'
        )
      )
    )
  )

ggplot(
  data = medians,
  mapping = aes(x = time, y = bankroll, color = strategy)
) +
  geom_line() +
  scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
  ggtitle(
    "Median bankroll over time (2500 runs)", 
    "(Probability = .525, Odds = 1, Bets = powers of 2, Initial Bankroll = 100)"
  ) +
  ylab("Median bankroll") +
  xlab("Time") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_dl(aes(label = strategy), method = list(dl.trans(x = x + .3), "last.bumpup")) +
  scale_x_continuous(expand = expand_scale(mult = c(0.05, .5))) +
  scale_y_continuous(trans='log10')

# mean bankroll at each point in time

means <- multiple_run %>%
  group_by(time) %>%
  summarise(discrete_kelly_bankroll = mean(discrete_kelly_bankroll),
            random_bookend_bankroll = mean(random_bookend_bankroll),
            closest_bookend_bankroll = mean(closest_bookend_bankroll),
            kelly_bankroll = mean(kelly_bankroll)) %>%
  gather(key = 'strategy', value = 'bankroll', -time) %>%
  # clean up column value for graph
  mutate(
    strategy = ifelse(
      strategy == 'discrete_kelly_bankroll', 'Discrete Kelly',
      ifelse(
        strategy == 'closest_bookend_bankroll', 'Closest Bookend',
        ifelse(
          strategy == 'random_bookend_bankroll', 'Random Bookend',
          'Kelly'
        )
      )
    )
  )
  
ggplot(
  data = means,
  mapping = aes(x = time, y = bankroll, color = strategy)
) +
  geom_line() +
  scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
  ggtitle(
    "Mean bankroll (2500 runs)", 
    "(Probability = .525, Odds = 1, Bets = powers of 2, Initial Bankroll = 100)"
  ) +
  ylab("Mean bankroll") +
  xlab("Time") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_dl(aes(label = strategy), method = list(dl.trans(x = x + .3), "last.bumpup")) +
  scale_x_continuous(expand = expand_scale(mult = c(0.05, .5))) +
  scale_y_continuous(trans='log10')

# at this point we narrow down to discrete kelly and closest bookend
# and see how many times each is ahead

percent_win <- multiple_run %>%
  select(sim_no, time, discrete_kelly_bankroll, closest_bookend_bankroll) %>%
  gather(key = 'strategy', value = 'bankroll', -one_of(c('sim_no', 'time'))) %>%
  group_by(sim_no, time) %>%
  filter(bankroll == max(bankroll)) %>%
  group_by(time, strategy) %>%
  summarise(best_count = n()) %>%
  # clean up column value for graph
  mutate(
    strategy = ifelse(
      strategy == 'discrete_kelly_bankroll', 'Discrete Kelly',
      ifelse(
        strategy == 'closest_bookend_bankroll', 'Closest Bookend',
        ifelse(
          strategy == 'random_bookend_bankroll', 'Random Bookend',
          'Kelly'
        )
      )
    )
  )

ggplot(
  data = percent_win,
  mapping = aes(x = time, y = best_count, color = strategy)
) +
  geom_line() +
  scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
  ggtitle(
    "Which strategy 'wins' at each point in time? (2500 runs)", 
    "(Probability = .525, Odds = 1, Bets = powers of 2, Initial Bankroll = 100)"
  ) +
  ylab("# of Runs w/ Maximal Bankroll") +
  xlab("Time") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_dl(aes(label = strategy), method = list(dl.trans(x = x + .3), "last.bumpup")) +
  scale_x_continuous(expand = expand_scale(mult = c(0.05, .5)))

# now we look at the final distribution -- as total $, and as a % of discrete kelly

ending_distribution <- multiple_run %>%
  filter(time == max(time)) %>%
  select(sim_no, time, discrete_kelly_bankroll, closest_bookend_bankroll, random_bookend_bankroll, kelly_bankroll) %>%
  gather(key = 'strategy', value = 'bankroll', -one_of(c('sim_no', 'time'))) %>%
  # clean up column value for graph
  mutate(
    strategy = ifelse(
      strategy == 'discrete_kelly_bankroll', 'Discrete Kelly',
      ifelse(
        strategy == 'closest_bookend_bankroll', 'Closest Bookend',
        ifelse(
          strategy == 'random_bookend_bankroll', 'Random Bookend',
          'Kelly'
        )
      )
    )
  )

ggplot(
  data = ending_distribution,
  mapping = aes(x = bankroll, color = strategy)
) +
  geom_density(size = 1) +
  scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
  ggtitle(
    "Ending bankroll distribution (2500 runs)", 
    "(Probability = .525, Odds = 1, Bets = powers of 2, Initial Bankroll = 100)"
  ) +
  xlab("Bankroll") +
  theme_bw() +
  scale_x_continuous(trans='log10')

# (0/0) -> NaN # if both bankrolls are zero, output is NaN
# (x/0) -> Inf
# (0/x) -> 0

ending_distribution_as_percent <- multiple_run %>%
  filter(time == max(time)) %>%
  mutate(
    closest_bookend_frac = closest_bookend_bankroll / discrete_kelly_bankroll,
    random_bookend_frac = random_bookend_bankroll / discrete_kelly_bankroll,
    kelly_frac = kelly_bankroll / discrete_kelly_bankroll
  ) %>%
  select(sim_no, time, closest_bookend_frac, random_bookend_frac, kelly_frac) %>%
  gather(key = 'strategy', value = 'bankroll', -one_of(c('sim_no', 'time'))) %>%
  # clean up column value for graph
  mutate(
    strategy = ifelse(
      strategy == 'kelly_frac', 'Kelly',
      ifelse(
        strategy == 'closest_bookend_frac', 'Closest Bookend',
        ifelse(
          strategy == 'random_bookend_frac', 'Random Bookend',
          'Kelly'
        )
      )
    )
  )


ending_distribution_as_percent %>%
  filter(strategy == 'Closest Bookend') %>%
  ggplot(
    mapping = aes(x = bankroll)
  ) +
  geom_density(size = 1) +
  scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
  ggtitle(
    "Bankroll distribution - closest bookend vs. discrete kelly (2500 runs)", 
    "(Probability = .525, Odds = 1, Bets = powers of 2, Initial Bankroll = 100)"
  ) +
  xlab("Closest Bookend Bankroll / Discrete Kelly Bankroll") +
  theme_bw()

ending_distribution_as_percent %>%
  filter(strategy == 'Random Bookend') %>%
  ggplot(
    mapping = aes(x = bankroll)
  ) +
  geom_density(size = 1) +
  scale_colour_manual(name  ="Bet Strategy", values = my_colors) +
  ggtitle(
    "Bankroll distribution - random bookend vs. discrete kelly (2500 runs)", 
    "(Probability = .525, Odds = 1, Bets = powers of 2, Initial Bankroll = 100)"
  ) +
  xlab("Random Bookend Bankroll / Discrete Kelly Bankroll") +
  theme_bw()
