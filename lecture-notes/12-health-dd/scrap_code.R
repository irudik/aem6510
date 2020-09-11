# load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggthemes, fixest, broom, tidyverse
)

########################################
# Initialize RNG
set.seed(12345)

########################################
## Initialize data generating parameters

# Fixed state characteristics: ny has more biodiversity
state_fes <- tribble(~state, ~state_fe,
                     "NY", 88,
                     "PA", 44)

# Period characteristics: biodiversity is declining
period_fes <- tribble(~period, ~period_fe,
                     "before", 341,
                     "after", 207)

# True effect of the policy: how many additional species are saved?
treatment_effect <- 55

########################################
## Generate fake data

# Create data frame
bio_df <- crossing(
  state = c("NY", "PA"),
  period = c("before", "after"),
  obs = 1:1000
) %>%
  left_join(period_fes) %>% 
  left_join(state_fes) %>% 
  mutate(treated = state == "NY" & period == "after") %>% 
  arrange(state, period) %>% 
  mutate(
    biodiversity = ifelse(
      treated,
      period_fe + state_fe + treatment_effect + 50*rnorm(n()),
      period_fe + state_fe  + 50*rnorm(n())
    )
  ) %>% 
  group_by(state, period) %>% 
  mutate(mean_biodiversity = mean(biodiversity)) %>% 
  ungroup()

seg_df <- bio_df %>% 
  group_by(state, period) %>% 
  summarise(mean_biodiversity = mean(biodiversity))

diff <- 
  filter(seg_df, 
         period == 'after',
         state =='PA')$mean_biodiversity[1] - 
  filter(seg_df, 
         period =='before', 
         state =='PA')$mean_biodiversity[1]


dffull <- rbind(
  #Step 1: Raw data only
  bio_df %>% mutate(msg = '1. Start with raw data.', msg_idx = 1),
  #Step 2: Add Y-lines
  bio_df %>% mutate(msg = '2. Figure out what differences in Y are explained by Treatment and/or Time.', msg_idx = 2),
  #Step 3: Collapse to means
  bio_df %>% mutate(biodiversity = mean_biodiversity, msg = "3. Keep only what's explained by Treatment and/or Time.", msg_idx = 3),
  #Step 4: Display time effect
  bio_df %>% mutate(biodiversity = mean_biodiversity, msg = "4. See how Control changed over Time.", msg_idx = 4),
  #Step 5: Shift to remove time effect
  bio_df %>% mutate(biodiversity = mean_biodiversity - (period == 'after')*diff,
                    biodiversity = biodiversity - min(biodiversity),
                msg = "5. Remove the Before/After Control difference for both groups.", msg_idx = 5),
  #Step 6: Raw demeaned data only
  bio_df %>% mutate(biodiversity = mean_biodiversity - (period == 'after')*diff,
                    biodiversity = biodiversity - min(biodiversity),
                msg = '6. The remaining Before/After Treatment difference is the effect.', msg_idx = 6) %>% 
    filter(state == "NY") %>% 
    mutate(biodiversity = biodiversity - min(biodiversity)))

dffull$period <- factor(dffull$period,
                        levels = c("before", "after"))

########################################
## Plot the data


ggplot(bio_df, group = interaction(state, period)) +
  geom_jitter(aes(x = period, y = biodiversity, color = interaction(state), shape = interaction(state)), size = 3) +
  annotate("text", size = 8, label = "NY", x = 1, y = 500) +
  annotate("text", size = 8, label = "PA", x = 1, y = 100, color = "orange") +
  scale_color_colorblind() +
  theme_minimal() +
  scale_x_discrete(labels = c("Before Treatment", "After Treatment")) + 
  scale_y_continuous(limits = c(50, 500)) +
  labs(
    x = "Time",
    y = "Biodiversity",
    title = "Start with the raw data"
  ) +
  theme(
    legend.position = "none",
    title = element_text(size = 24),
    axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
    panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black")
  ) 

ggplot(dffull, group = interaction(state, period)) +
  geom_jitter(aes(x = period, y = mean_biodiversity, color = interaction(state), shape = interaction(state)), size = 3, height = 0) +
  scale_color_colorblind() +
  theme_minimal() +
  scale_x_discrete(labels = c("Before Treatment", "After Treatment")) + 
  scale_y_continuous(limits = c(50, 500)) +
  labs(
    x = "Time",
    y = "Biodiversity",
    title = "Take the mean of each state-period"
  ) +
  theme(
    legend.position = "none",
    title = element_text(size = 24),
    axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
    panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black")
  ) 

ggplot(dffull %>% filter(msg_idx == 4), group = interaction(state, period)) +
  geom_jitter(aes(x = period, y = biodiversity, color = interaction(state), shape = interaction(state)), size = 3, height = 0) +
  annotate("segment", x = 1.45, xend = 1.45, y = seg_df$mean_biodiversity[1], yend = seg_df$mean_biodiversity[2],
           size = 1, color = "black") +
  annotate("segment", x = 1.55, xend = 1.55, y = seg_df$mean_biodiversity[3], yend = seg_df$mean_biodiversity[4],
           size = 1, color = "orange") +
  scale_color_colorblind() +
  theme_minimal() +
  scale_x_discrete(labels = c("Before Treatment", "After Treatment")) + 
  scale_y_continuous(limits = c(50, 500)) +
  labs(
    x = "Time",
    y = "Biodiversity",
    title = "Here are the differences in means for NY and PA"
  ) +
  theme(
    legend.position = "none",
    title = element_text(size = 24),
    axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
    panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black")
  ) 

ggplot(dffull %>% filter(msg_idx == 5), group = interaction(state, period)) +
  geom_jitter(aes(x = period, y = biodiversity, color = interaction(state), shape = interaction(state)), size = 3, height = 0) +
  scale_color_colorblind() +
  theme_minimal() +
  scale_x_discrete(labels = c("Before Treatment", "After Treatment")) + 
  labs(
    x = "Time",
    y = "Biodiversity",
    title = "Difference out how the control changed over time\nThis is the first difference"
  ) +
  theme(
    legend.position = "none",
    title = element_text(size = 24),
    axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
    panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black")
  ) 


ggplot(dffull %>% filter(msg_idx == 5), group = interaction(state, period)) +
  geom_jitter(aes(x = period, y = biodiversity, color = interaction(state), shape = interaction(state)), size = 3, height = 0) +
  scale_color_colorblind() +
  theme_minimal() +
  scale_x_discrete(labels = c("Before Treatment", "After Treatment")) + 
  labs(
    x = "Time",
    y = "Biodiversity",
    title = "The black lines each represent a difference"
  ) +
  theme(
    legend.position = "none",
    title = element_text(size = 24),
    axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
    panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black")
  ) 


ggplot(dffull %>% filter(msg_idx == 6), group = interaction(state, period)) +
  geom_jitter(aes(x = period, y = biodiversity, color = interaction(state), shape = interaction(state)), size = 3, height = 0) +
  annotate("segment", x = 1.5, xend = 1.5, y = 0, yend = treatment_effect,
           size = 1, color = "red") +
  annotate("text", size = 8, label = paste0("True treatment effect size = ", treatment_effect), x = 1, y = treatment_effect/2, color = "red") +
  scale_color_colorblind() +
  theme_minimal() +
  scale_x_discrete(labels = c("Before Treatment", "After Treatment")) + 
  labs(
    x = "Time",
    y = "Biodiversity",
    title = "The remaining difference in the treatment group\nis the difference-in-differences"
  ) +
  theme(
    legend.position = "none",
    title = element_text(size = 24),
    axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
    panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black")
  ) 

feols(biodiversity ~ treated | period_fe + state_fe, data = bio_df)



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