# Data Manipulation Q3
library(tidyverse)
library(readxl) 
library(scales)
library(ggplot2)

# Import and sort out rcv
rcv <- read_csv("rcv_ep1.txt")
rcv_clean <- rcv |>
  mutate(
    MEPID = as.character(MEPID),
    NP = as.factor(NP),
    EPG = as.factor(EPG))

# Pivot using the cleaned data 
rcv_ep1 <- rcv_clean |>
  pivot_longer(
    cols = starts_with("V"),       
    names_to = "Vote_item",          
    values_to = "Decision") 
head(rcv_ep1)

# Create a summary table 
rcv_ep1 <- mutate(rcv_ep1, 
                  Decision = factor(Decision, 
                                    levels = c(1, 2, 3, 4, 0, 5), 
                                    labels = c("Yes", "No", "Abstain", 
                                               "Present but did not vote", 
                                               "Absent", "Not an MEP")))

vote_sum <- count(rcv_ep1, Decision)
vote_sum

# Data Manipulation Q4
# Import mepinfo and set MEPID for combination
mepinfo_ep1 <- read_excel("mep_info_26Jul11.xls", sheet = "EP1") |>
  mutate(MEPID = as.character(`MEP id`)) |>
# Drop repeated varibale like Name,MS,NP,EPG
  select(MEPID, `NOM-D1`, `NOM-D2`)

# Combine the 2 datasets by MEPID
com_ep1 <- rcv_ep1 |>
  left_join(mepinfo_ep1, by = "MEPID") |>
  mutate(
    `NOM-D1` = as.numeric(`NOM-D1`),
    `NOM-D2` = as.numeric(`NOM-D2`))
head(com_ep1)

# Check for missing values
missing_repo <- colSums(is.na(com_ep1))
missing_repo

# Data Manipulation Q5
# The mean rate of Yes votes across all roll calls
yes_rate <- com_ep1 |>
  filter(Decision %in% c("Yes", "No", "Abstain")) |>
  group_by(EPG) |>
  summarise(Yes_rate = mean(Decision == "Yes")) 
yes_rate

# The mean abstention rate
abs_rate <- com_ep1 |>
  filter(Decision %in% c("Yes", "No", "Abstain")) |> 
  group_by(EPG) |>
  summarise(Abstention_rate = mean(Decision == "Abstain")) 
abs_rate

# The mean vote preferences along the two contested dimensions
which(rcv$EPG == "0") # one MEP don't have EP Group information
dimension_pre <- com_ep1 |>
  filter(EPG != "0") |>  # drop the case without EP Group category
  group_by(EPG) |>
  summarise(
    D1_mean = mean(`NOM-D1`, na.rm = TRUE),
    D2_mean = mean(`NOM-D2`, na.rm = TRUE))
dimension_pre

# Data Visualization
# Q1.Plot the distribution of the first NOMINATE dimension by EP group
# Data Deduplication
data1 <- com_ep1 |>
  filter(EPG != "0") |> 
  select(MEPID, EPG, `NOM-D1`, `NOM-D2`) |> 
  unique() |> 
  na.omit()

# Plot data1 in boxplot by EP group
pdf("plot1.pdf")
plot1 <- ggplot(data1, aes(x = EPG, y = `NOM-D1`, fill = EPG)) +
  geom_boxplot(alpha = 0.7,outlier.size = 1, outlier.alpha = 0.5) +
  scale_fill_brewer(palette = "Set1", name = "EP Group") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
       title = "Distribution of Ideological Positions by EP Group",
       x = "EP Group",
       y = "Traditional Left/Right Score (NOM-D1)") +
  theme_bw() +
  theme(
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title.x = element_text(size = 10, face = "bold"), 
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10),
        legend.position = 'right')
print(plot1)
dev.off()

# Q2. Make a scatterplot of nomdim1 (x-axis) and nomdim2 (y-axis)
pdf("plot2.pdf")
plot2 <- ggplot(data1, aes(x = `NOM-D1`, y = `NOM-D2`, color = EPG)) +
  geom_point(alpha = 0.7, size = 2) +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
  scale_color_brewer(palette = "Set1", name = "EP Group") +
  labs(
    title = "MEPs' NOMINATE Coordinates on Ideological Spectrum and EU Integration",
    x = "Traditional Left/Right Dimension (NOM-D1)",
    y = "Pro-EU/Eurosceptic Dimension (NOM-D2)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12,hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10),
    legend.position = 'right')
print(plot2)
dev.off()

# Q3.Produce a boxplot of the proportion voting Yes by EP group 
# Compute the distribution of yes proportion for each roll call
vote_cal <- com_ep1 |>
  filter(Decision %in% c("Yes", "No", "Abstain") & EPG != "0") |>
  group_by(EPG, Vote_item) |>
  summarise(yes_prop = mean(Decision == "Yes"), .groups = "drop")
head(vote_cal) 

# Plot the boxplot 
pdf("plot3.pdf")
plot3 <- ggplot(vote_cal, aes(x = EPG, y = yes_prop, fill = EPG)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, outlier.alpha = 0.5) +
  scale_fill_brewer(palette = "Set1", name = "EP Group") +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent_format()) +
  labs(
    title = "Proportion of Voting Yes by EP Group (per Roll Call)",
    x = "EP Group",
    y = "Proportion of Yes Votes ") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14,hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10),
    legend.position = 'right')
print(plot3)
dev.off()

# Q4.Display the proportion voting Yes per year by national party
# Merge the year information for each roll call to combined dataset
library(lubridate)
library(janitor)
vote_info <- read_xls("vote_info_Jun2010.xls", sheet = "EP1")
head(vote_info)
vote_dates <- vote_info |>
  mutate(
  Vote_item = paste0("V", `Vote No. in RCV_EP1 file`),
  Year = year(excel_numeric_to_date(as.numeric(Date)))
  ) |>
  select(Vote_item, Year)

com_ep1 <- com_ep1 |>
  left_join(vote_dates, by = "Vote_item")
head(com_ep1)

# Compute the proportion voting Yes per year by national party
# Q4. Display the proportion voting Yes per year by national party
np_votes <- com_ep1 |>
  filter(Decision %in% c("Yes", "No", "Abstain") & !is.na(Year)) |>
  mutate(Year = as.integer(Year)) |>
  group_by(NP, Year) |>
  summarise(np_yes = mean(Decision == "Yes"), .groups = "drop")

# Plot the bar plot
pdf("plot4.pdf")
plot4 <- ggplot(np_votes, aes(x = Year, y = np_yes, fill = NP)) +
  geom_col(position = "dodge", alpha = 0.8, color = NA) + 
  scale_fill_viridis_d(option = "mako", begin = 0.1, end = 0.9)  +
  scale_y_continuous(limits = c(0, 1), labels = percent_format()) +
  labs(
    title = "Proportion of Voting Yes per Year by National Party",
    x = "Year",
    y = "Proportion of Yes Votes",
    fill = "National Party") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 6),          
    legend.title = element_text(size = 8),         
    legend.key.size = unit(0.3, "cm"),
    legend.position = "right") +
  guides(fill = guide_legend(ncol = 2))
print(plot4) 
dev.off()

#Q5.Calculate the average Yes share per year and plot a line graph by EPG
# Compute the average Yes share per year by EP group
epg_votes <- com_ep1 |>
  filter(Decision %in% c("Yes", "No", "Abstain") & EPG != "0" & !is.na(Year)) |>
  mutate(Year = as.integer(Year)) |>
  group_by(EPG, Year) |>
  summarise(mean_yes = mean(Decision == "Yes"), .groups = "drop")
head(epg_votes)

# Plot the line graph
pdf("plot5.pdf")
plot5 <- ggplot(epg_votes, aes(x = Year, y = mean_yes, color = EPG, group = EPG)) +
  geom_line(size = 0.8, alpha = 0.8) +
  geom_point(size = 0.8, alpha = 0.8) +
  scale_color_brewer(palette = "Set1", name = "EP Group") +
  scale_y_continuous(limits = c(0, 1), labels = percent_format()) +
  labs(
    title = "Average Yes Share per Year by EP Group",
    x = "Year",
    y = "Average Yes Share",
    color = "EP Group"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right")
print(plot5)
dev.off()
