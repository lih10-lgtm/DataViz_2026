library(tidyverse)
library(ggplot2)
library(ggridges)
library(readr)
df <- read_csv("C:/Users/janel/ESS10e03_3.csv")

# data cleaning:remove missing values and recode variables
ess10 <- df |>
  filter(
    !is.na(cntry),     # countries
    (netustm <= 1440 | is.na(netustm)),    # Internet use time              
    fampref <= 5,      # preference setting
    famadvs <= 5,      # advanced searching
    yrbrn <= 2006,     # year of birth
    ppltrst <= 10,     # social trust rating
    sclmeet <= 7,      # offline social meeting
  ) |>
  mutate(
    netusoft_num = as.numeric(as.character(netusoft)),
    sclmeet_num  = as.numeric(as.character(sclmeet)),
    social_freq = case_when(
      sclmeet_num == 1 ~ "Never",
      sclmeet_num == 2 ~ "Less than once a month",
      sclmeet_num == 3 ~ "Once a month",
      sclmeet_num == 4 ~ "Several times a month",
      sclmeet_num == 5 ~ "Once a week",
      sclmeet_num == 6 ~ "Several times a week",
      sclmeet_num == 7 ~ "Every day"
    ),
    social_freq = factor(social_freq, levels = c(
      "Never", 
      "Less than once a month", 
      "Once a month", 
      "Several times a month", 
      "Once a week", 
      "Several times a week", 
      "Every day"
    )),
    
    navskill_num = round((fampref + famadvs) / 2),
    navskill = factor(navskill_num, 
                      levels = 1:5, 
                      labels = c("Very Low", "Low", "Moderate", "High", "Very High"))
  )
str(ess10)

# define my own theme
my_theme <- function() {
  theme_minimal(base_family = "sans", base_size = 12) + 
    theme(
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 0)),
      axis.title = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(size = 10, face = "bold", color = "black"),
      plot.caption = element_text(size = 8, color = "grey40", hjust = 1, margin = margin(t = 15)),
      legend.position = "top",
      legend.text = element_text(size = 10, face = "plain"),
      legend.title = element_text(size = 10, face = "plain")
    )
}

# Plot 1: Ridge Plot
# calculate means for ridge plot
trust_means <- ess10 |>
group_by(navskill) |>
  summarize(mean_score = mean(ppltrst, na.rm = TRUE)) |>
  mutate(navskill_rev = fct_rev(navskill))

# plot distribution of social trust by info navigation skill
pdf("plot1.pdf", width = 10, height = 6)
plot1 <- ggplot() +
  geom_density_ridges(data = ess10,
                      aes(x = ppltrst, y = fct_rev(navskill), fill = fct_rev(navskill)),
                      alpha = 0.8, color = "white", scale = 1.2) +
  geom_point(data = trust_means,
             aes(x = mean_score, y = navskill_rev, color = "Average Mean"),
             size = 2) +
  scale_fill_manual(
    name = "Navigation Skill Level",
    values = c("navy", "royalblue", "skyblue2", "lightskyblue1", "lightblue1")
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Average Mean" = "orange")
  ) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.15))) +
  labs(
    title = "Distribution of Social Trust by Information Navigation Skills",
    subtitle = "Density of Social Trust scores (ESS Round 10)",
    x = "\nSocial Trust Score (0-10)",
    y = "Information Navigation Skills\n",
    caption = "Source: ESS Round 10. \nSkills created by merging Preference Settings & Advanced Search familiarity."
  ) +
  my_theme()
print(plot1)
dev.off()

# Plot 2: Line Plot
# sort out data by internet use time and off line social meeting frequency
data2 <- ess10 |>
  filter(!is.na(netustm), !is.na(sclmeet)) |> 
  mutate(
    time_bin = case_when(
      netustm <= 120 ~ "0-2h",
      netustm > 120 & netustm <= 240 ~ "2-4h",
      netustm > 240 & netustm <= 360 ~ "4-6h",
      netustm > 360 & netustm <= 480 ~ "6-8h",
      netustm > 480 ~ "8h+"
    ),
    time_bin = factor(time_bin, levels = c("0-2h", "2-4h", "4-6h", "6-8h", "8h+")),
    social_4groups = case_when(
      sclmeet <= 2 ~ "Less than monthly",
      sclmeet %in% 3:4 ~ "Monthly",
      sclmeet %in% 5:6 ~ "Weekly",
      sclmeet == 7 ~ "Every day"
    ),
    social_4groups = factor(social_4groups, levels = c("Every day", "Weekly", "Monthly", "Less than monthly"))
  ) |>
  group_by(time_bin, social_4groups) |>
  summarize(mean_trust = mean(ppltrst, na.rm = TRUE), .groups = "drop")

# plot relationship between social trust and screen time
pdf("plot2.pdf", width = 10, height = 6)
plot2 <- ggplot(data2, aes(x = time_bin, y = mean_trust, 
                            color = social_4groups, group = social_4groups)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(
    name = "Offline Social Meeting Frequency",
    values = c(
      "Every day"          = "orchid3", 
      "Weekly"             = "orchid4",
      "Monthly"            = "thistle4", 
      "Less than monthly"  = "thistle" 
    )
  ) +
  scale_y_continuous(limits = c(3.5, 6.5), breaks = seq(3.5, 6.5, by = 0.5)) +
  labs(
    title = "Social Trust by Internet Use and Social Frequency",
    subtitle = "Mean Social Trust by Internet use and 4-level Social Frequency",
    x = "\nDaily Internet Use Time",
    y = "Average Social Trust Score\n",
    caption = "Source: ESS Round 10. \nFrequency categories merged: Monthly (Once & Several times), Weekly (Once & Several times)."
  ) +
  my_theme() +
  theme(
    legend.position = "right",
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor = element_blank()
  )
print(plot2)
dev.off()

# Plot 3: Smoothed Proportion Plot 
# sort out data by region, continuous age and navskill
data3 <- ess10 |>
  filter(!is.na(yrbrn), !is.na(navskill)) |>
  mutate(
    age = 2022 - yrbrn,
    region = case_when(
      cntry %in% c("FI", "NO", "SE", "IS", "DK") ~ "Nordic Countries",
      cntry %in% c("AT", "BE", "CH", "DE", "FR", "IE", "NL", "LU", "GB") ~ "Western Europe",
      cntry %in% c("ES", "GR", "IT", "PT", "CY") ~ "Southern Europe",
      cntry %in% c("CZ", "EE", "HU", "PL", "SI", "SK", "LT", "LV", "BG", "HR", "RO", "RS", "ME", "MK", "AL") ~ "Central & Eastern Europe",
      TRUE ~ "Other" 
    ),
    region = factor(region, levels = c("Nordic Countries", "Western Europe", "Southern Europe", "Central & Eastern Europe"))
  ) |>
  filter(region != "Other")

# plot proportion of  skill levels across continuous age
pdf("plot3a.pdf", width = 10, height = 7)
plot3a <- ggplot(data3, aes(x = age, fill = fct_rev(navskill))) +
  geom_density(position = "fill", alpha = 0.9, color = NA) +
  geom_vline(xintercept = 50, color = "darkorange", linetype = "dotted", linewidth = 0.8) +
  facet_wrap(~ region, ncol = 2) +
  facet_wrap(~ region, ncol = 2) + 
  scale_fill_manual(
    name = "Navigation Skill Level",
    values = c("navy", "royalblue", "skyblue2", "lightskyblue1", "lightblue1") 
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8), expand = c(0, 0)) +
  labs(
    title = "Information Navigation Skills Across European Regions",
    subtitle = "Smoothed proportion of skill levels by continuous age\n",
    x = "\nAge of Respondent",
    y = "Proportion of Population\n",
    caption = "Source: ESS Round 10. Regions classified by geographical factors."
  ) +
  my_theme() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )
print(plot3a)
dev.off()


# Plot 4: Smoothed Trend Plot
# sort out data for high skill proportion trend
data4 <- ess10 |>
  filter(!is.na(yrbrn), !is.na(navskill_num)) |>
  mutate(
    age = 2022 - yrbrn,
    is_high_skill = ifelse(navskill_num >= 4, 1, 0),
    region = case_when(
      cntry %in% c("FI", "NO", "SE", "IS", "DK") ~ "Nordic Countries",
      cntry %in% c("AT", "BE", "CH", "DE", "FR", "IE", "NL", "LU", "GB") ~ "Western Europe",
      cntry %in% c("ES", "GR", "IT", "PT", "CY") ~ "Southern Europe",
      cntry %in% c("CZ", "EE", "HU", "PL", "SI", "SK", "LT", "LV", "BG", "HR", "RO", "RS", "ME", "MK", "AL") ~ "Central & Eastern Europe",
      TRUE ~ "Other"
    ),
    region = factor(region, levels = c("Nordic Countries", "Western Europe", "Southern Europe", "Central & Eastern Europe"))
  ) |>
  filter(region != "Other")

# calculate dynamic min and max age from dataset for LOESS prediction
min_age <- min(data4$age, na.rm = TRUE)
max_age <- max(data4$age, na.rm = TRUE)

# find peak and trough points, using pred_age to avoid variable masking in loess
min_max_data <- data4 |>
  group_by(region) |>
  reframe(
    pred_age = min_age:max_age,
    fitted_val = predict(loess(is_high_skill ~ age, span = 0.5), newdata = data.frame(age = min_age:max_age))
  ) |>
  rename(age = pred_age) |>
  group_by(region) |>
  mutate(
    is_max = fitted_val == max(fitted_val, na.rm = TRUE),
    is_min = fitted_val == min(fitted_val, na.rm = TRUE)
  ) |>
  filter(is_max | is_min) |>
  summarise(
    age = age,
    fitted_val = fitted_val,
    label = scales::percent(fitted_val, accuracy = 0.1),
    v_adj = ifelse(is_max, -1, 2),
    .groups = "drop"
  )

# plot smoothed trend with auto-adjusting x-axis and peak/trough labels
pdf("plot3b.pdf", width = 10, height = 7)
plot3b <- ggplot(data4, aes(x = age, y = is_high_skill)) +
  geom_smooth(method = "loess", span = 0.5, se = TRUE, 
              color = "navy", fill = "lightblue1", linewidth = 1.2) +
  geom_point(data = min_max_data, aes(x = age, y = fitted_val), 
             color = "darkorange", size = 2.5) +
  geom_text(data = min_max_data, aes(x = age, y = fitted_val, label = label, vjust = v_adj),
            color = "navy", fontface = "bold", size = 3.5) +
  facet_wrap(~ region, ncol = 2) + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(
    title = "Regional Comparison of Information Navigation Skill Decline",
    subtitle = "Smoothed proportion of High/Very High navigation skills by age\n",
    x = "\nAge of Respondent",
    y = "Proportion of High Skill Group (%)\n",
    caption = "Source: ESS Round 10. \nLines represent LOESS smoothed proportions with 95% confidence intervals."
  ) +
  my_theme() +
  theme(
    panel.grid.minor = element_blank()
  )
print(plot3b)
dev.off()