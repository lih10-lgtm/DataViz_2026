# Import library
library(tidyverse)
library(scales)
library(ggplot2)

# Import and sort out rcv
# 1.Import and sort out data
raw_data <- read_csv("NCSS_v1.csv")
data <- raw_data |>
  select(CASEID,YEAR,GDREGION,NUMOFFMBR,TRAD6,TRAD12,INCOME)

# 2.Filter dataset using TRAD6
data_fil <- data |>
  filter(TRAD6 %in% c("Chrétiennes", "Juives", "Musulmanes"))

# 3.Compute for the number of congregations by religious classification
stats <- data_fil |>
  group_by(TRAD6, YEAR) |>
  summarise(
    count= n(),
    income_avg = mean(INCOME, na.rm = TRUE),
    income_med = median(INCOME, na.rm = TRUE),
    .groups = "drop")
stats

# 4.Create binary variable AVG_INCOME 
income_data <- data_fil |> 
  filter(!is.na(INCOME)) |>
  group_by(YEAR) |> 
  mutate(
    year_avg = mean(INCOME, na.rm = TRUE),
    AVG_INCOME = if_else(INCOME >= year_avg, 1, 0),
    Income_Level = if_else(AVG_INCOME == 1, "Above Average", "Below Average"),
    Income_Level = factor(Income_Level, levels = c("Below Average", "Above Average"))
  ) |> 
  ungroup() 

# DV1.Create a bar plot
pdf("plot1.pdf")
plot1 <- ggplot(income_data, aes(x = TRAD12, fill = Income_Level)) + 
  geom_bar(position = "fill", width = 0.6) +  
  facet_wrap(~ YEAR) +                 
  coord_flip() +                       
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("Below Average" = "grey", "Above Average" = "skyblue")) + 
  labs(
    title = "Proportion of Congregations Above/Below Average Income",
    x = NULL,           
    y = "\nProportion",
    fill = "Income Group") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 10),          
    legend.title = element_text(size = 10),
    legend.position = "top",          
    panel.grid.major.y = element_blank())
print(plot1) 
dev.off()


# DV2.Make a bar plot with geom_col()
data2 <- data_fil |> 
  filter(!is.na(NUMOFFMBR)) |>
  filter(YEAR == 2022) 

pdf("plot2.pdf")
plot2 <- ggplot(data2, aes(x = TRAD12, y = NUMOFFMBR, fill = TRAD6)) + 
  geom_col(width = 0.6) + 
  coord_flip() + 
  scale_fill_manual(values = c("Chrétiennes" = "cadetblue2",
                               "Juives" = "burlywood2",
                               "Musulmanes" = "cornsilk3")) +
  labs(
    title = "Official Members by Religion Groups",
    x = NULL,
    y = "\nNumber of Official Members",
    fill = "Religon Groups (TRAD6)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.title.y = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 10),          
    legend.title = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.y = element_blank())
print(plot2) 
dev.off()


# DV3. Display the distribution of average yearly income (INCOME) for congregations
data3 <- data_fil |> 
  filter(YEAR == 2022) |> 
  filter(!is.na(INCOME)) |>      
  filter(!is.na(GDREGION))  
summary(data3) # get the income range to decide x-axis range

region_means <- data3 |>
  group_by(GDREGION) |>
  summarise(mean_income = mean(INCOME))

library(ggridges)
pdf("plot3.pdf")
plot3 <- ggplot() + 
  geom_density_ridges(data = data3, 
                      aes(x = INCOME, y = GDREGION, fill = GDREGION), 
                      alpha = 0.6, scale = 1.8, rel_min_height = 0.01) + 
  geom_point(data = region_means, 
             aes(x = mean_income, y = GDREGION, color = "Average Mean"), 
             size = 2) + 
  scale_x_continuous(
    limits = c(0, 9000000)) + 
  scale_fill_viridis_d(option = "mako", name = "Region") +
  scale_color_manual(name = NULL, 
                     values = c("Average Mean" = "orange")) + 
  labs(
    title = "Income Distribution by Region (2022)\n",
    x = "\nTotal Income",
    y = NULL) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),           
    legend.title = element_text(size = 10, face = "bold"),      
    panel.grid.minor = element_blank(),
    legend.position = "right")
print(plot3) 
dev.off()

# DV4. Create a boxplot of official members by TRAD6 and GDREGION
data4 <- data_fil |> 
  filter(YEAR == 2022) |> 
  filter(!is.na(NUMOFFMBR)) |> 
  filter(!is.na(GDREGION))

pdf("plot4.pdf")
plot4 <- ggplot(data4, aes(x = TRAD6, y = NUMOFFMBR, fill = TRAD6)) + 
  geom_boxplot(outlier.size = 1, alpha = 0.8) + 
  facet_wrap(~ GDREGION, scales = "free_x") + 
  scale_y_continuous(limits = c(0, 5000)) +
  scale_fill_manual(values = c("Chrétiennes" = "cadetblue2",
                               "Juives" = "burlywood2",
                               "Musulmanes" = "cornsilk3")) +
  labs(
    title = "Distribution of Official Members by Region and Religion (2022)",
    subtitle = "excluding outliers > 5,000",
    x = NULL,
    y = "Number of Official Members\n",
    fill = "Religon Groups (TRAD6)") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),              
    legend.title = element_text(size = 10),
    legend.position = "top",              
    panel.grid.major.y = element_blank())
print(plot4) 
dev.off()
