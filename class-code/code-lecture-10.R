if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rlang, nycflights13, tidyverse
)


# Generate cons_df
set.seed(12345)
cons_df <- tibble(
  state = seq(1, 8), # state identifier
  Y1 = floor(state/2) + -3:4 + floor(runif(8)*10), # treatment potential outcome
  Y0 = floor(runif(8)*10), # control potential outcome
  delta = Y1 - Y0
)

cons_df %>% summarise(mean(delta))