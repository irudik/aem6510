# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, tidylog, lubridate
)
options("tidylog.display" = NULL)

storms <- read_csv("data/storms.csv")
damages <- read_csv("data/damages.csv") %>% 
  pivot_wider(names_from = "name", values_from = "damages")
write_csv(damages, "data/damages.csv")
df <- storms %>% 
  unite(day_of_storm, year:hour, sep = "-") %>% 
  mutate(day_of_storm = ymd_h(day_of_storm)) %>% 
  filter(status == "hurricane") %>% 
  inner_join(damages) %>% 
  group_by(name) %>% 
  summarise(wind = mean(wind, na.rm = TRUE), damages = min(damages)) %>% 
  mutate(damage_per_mph = damages/wind) %>% 
  arrange(desc(damage_per_mph))

reg_output <- lm(damages ~ wind, data = df)
estimate <- reg_output$coefficients[2]