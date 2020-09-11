# load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggthemes, fixest, broom, tidyverse
)


########################################
## Read in blood lead data
bll_df <- read_csv("data/hr_2021_bll.csv")

########################################
## Look at data
bll_df

########################################
## Plot raw data trends
bll_df %>% 
  group_by(race, year) %>% 
  summarise(percent_high_conditional_tested = weighted.mean(percent_high_conditional_tested, weight, na.rm = T)) %>% 
  filter(year >= 2005 & year <= 2009) %>% 
  ggplot(aes(x = year, y = percent_high_conditional_tested)) +
    geom_vline(xintercept = 2006.5, color = "grey55", linetype = "dashed") +
    geom_point(aes(shape = as.factor(race), color = as.factor(race)), size = 5) +
    annotate("text", size = 8, label = "Treated", x = 2005.25, y = 2.85) +
    annotate("text", size = 8, label = "Border", x = 2005.25, y = 2.5) +
    annotate("text", size = 8, label = "Control", x = 2005.25, y = 1.4) +
    annotate("text", size = 10, label = "Leaded Fuel", x = 2005.50, y = 0.25) +
    annotate("text", size = 10, label = "Unleaded Fuel", x = 2008, y = 0.25) +
    theme_minimal() +
    scale_color_colorblind() +
    labs(
      x = "Year",
      y = "Percent Lead Poisoned",
      title = "Average lead poisoning rates by type (balanced panel)"
    ) +
    theme(
      legend.position = "none",
      title = element_text(size = 24),
      axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
      axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
      panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = "#eeeeee", colour = NA),
      plot.background = element_rect(fill = "#eeeeee", colour = NA),
      axis.line = element_line(colour = "black")
    ) 



########################################
## Difference-in-differences estimate
fixest::feols(ihs_bll ~ as.factor(race)*I(year > 2007) | county_fips + year, 
              weights = ~weight,
              data = bll_df) %>% 
  tidy(cluster = "state_fips")


########################################
## Event study

# Need to factor year so we can omit 2007
bll_df$year <- factor(bll_df$year, ordered = FALSE)
bll_df$year <- relevel(bll_df$year, 3)

# Estimate event study
event_study <- fixest::feols(ihs_bll ~ as.factor(race)*year + as.factor(state_fips)*year | county_fips + year, 
              weights = ~weight,
              data = bll_df) %>% 
  tidy(cluster = "state_fips") %>% 
  filter(str_detect(term, "^as.factor\\(race\\)2:year")) %>% 
  add_row(estimate = 0, std.error = 0, .after = 2) %>% 
  mutate(year = row_number() + 2004)

# Plot event study
ggplot(data = event_study, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, size = 0.5) +
  geom_vline(xintercept = 2006.5, color = "grey55", linetype = "dashed") +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), 
              fill = "darkslateblue", alpha = 0.4) +
  geom_line(size = 1) +
  geom_point(size = 5) +
  annotate("text", size = 8, label = "Leaded Fuel", x = 2005.50, y = -0.26) +
  annotate("text", size = 8, label = "Unleaded Fuel", x = 2012, y = -0.26) +
  theme_minimal() +
  scale_color_colorblind() +
  scale_x_continuous(breaks = seq(2005, 2015, 2)) +
  labs(
    x = "Year",
    y = "Percent Lead Poisoned",
    title = "Percent lead poisoned relative to 2007 (first unleaded year)"
  ) +
  theme(
    legend.position = "none",
    title = element_text(size = 24),
    axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
    panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#eeeeee", colour = NA),
    plot.background = element_rect(fill = "#eeeeee", colour = NA),
    axis.line = element_line(colour = "black")
  )


########################################
## Read in blood lead data
mortality_df <- read_csv("data/hr_2021_mortality.csv")

mortality_df



# Need to factor year so we can omit 2007
mortality_df$year <- factor(mortality_df$year, ordered = FALSE)
mortality_df$year <- relevel(mortality_df$year, 9)

# Estimate event study
event_study <- fixest::feols(age_adjusted_rate ~ as.factor(race)*year + as.factor(state_fips)*year | county_fips + year, 
                             weights = ~weight,
                             data = mortality_df) %>% 
  tidy(cluster = "state_fips") %>% 
  filter(str_detect(term, "^as.factor\\(race\\)2:year")) %>% 
  add_row(estimate = 0, std.error = 0, .after = 8) %>% 
  mutate(year = row_number() + 1998)

# Plot event study
ggplot(data = event_study, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, size = 0.5) +
  geom_vline(xintercept = 2006.5, color = "grey55", linetype = "dashed") +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), 
              fill = "darkslateblue", alpha = 0.4) +
  geom_line(size = 1) +
  geom_point(size = 5) +
  annotate("text", size = 8, label = "Leaded Fuel", x = 2002, y = -200) +
  annotate("text", size = 8, label = "Unleaded Fuel", x = 2012, y = -200) +
  theme_minimal() +
  scale_color_colorblind() +
  scale_x_continuous(breaks = seq(1999, 2015, 2)) +
  labs(
    x = "Year",
    y = "Age-Adjusted Mortality Rate (deaths per 100,000)",
    title = "Mortality rate relative to 2007 (first unleaded year)"
  ) +
  theme(
    legend.position = "none",
    title = element_text(size = 24),
    axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
    panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#eeeeee", colour = NA),
    plot.background = element_rect(fill = "#eeeeee", colour = NA),
    axis.line = element_line(colour = "black")
  )
