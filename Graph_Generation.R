# Load necessary libraries and data

library(readxl)
S_A_Written_Project_Data_Set_2_ <- read_excel("S_A_-_Written_Project_-_Data_Set_(2).xlsx")
View(S_A_Written_Project_Data_Set_2_)

#Plot 1: Weekly Conversion Rate (%)

library(dplyr)
library(ggplot2)
library(janitor)
library(scales)

S_A_Written_Project_Data_Set_2_ <- clean_names(S_A_Written_Project_Data_Set_2_)

weekly_data <- S_A_Written_Project_Data_Set_2_ %>%
  group_by(week) %>%
  summarise(
    total_visits = sum(visits, na.rm = TRUE),
    total_conversions = sum(conversions, na.rm = TRUE)
  ) %>%
  mutate(conversion_rate = total_conversions / total_visits)


p1 <- ggplot(
  weekly_data,
  aes(x = week, y = conversion_rate, color = "Weekly Conversion Rate (%)")
) +
  geom_line(linewidth = 1.6) +
  geom_point(size = 3.2) +
  
  scale_color_manual(values = c("Weekly Conversion Rate (%)" = "#635BFF")) +
  
  scale_x_continuous(
    breaks = seq(0, 19, by = 2)
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.34, 0.42)
  ) +
  
  labs(
    x = "Week Number",
    y = "Conversion Rate (%)",
    color = NULL   # removes legend title box
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#D9DEE7", linewidth = 0.6),
    
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    axis.line = element_line(color = "#7A7A7A"),
    axis.ticks = element_line(color = "#7A7A7A"),
    
    axis.text = element_text(color = "#7A8CA5"),
    
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

p1

# Plot 2: Conversion Rate by Browser (%)

library(dplyr)
library(ggplot2)
library(janitor)
library(scales)

S_A_Written_Project_Data_Set_2_ <- clean_names(S_A_Written_Project_Data_Set_2_)

browser_data <- S_A_Written_Project_Data_Set_2_ %>%
  filter(!is.na(browser), browser != "") %>%
  group_by(browser) %>%
  summarise(
    total_visits = sum(visits, na.rm = TRUE),
    total_conversions = sum(conversions, na.rm = TRUE)
  ) %>%
  mutate(conversion_rate_pct = round((total_conversions / total_visits) * 100, 1))

browser_data

browser_data$browser <- factor(
  browser_data$browser,
  levels = c("safari", "chrome", "ie", "firefox")
)

p2 <- ggplot(browser_data, aes(x = conversion_rate_pct, y = browser, fill = browser)) +
  geom_col(width = 0.55) +   # removed show.legend = FALSE
  
  geom_text(
    aes(label = round(conversion_rate_pct, 0)),
    hjust = -0.2,
    size = 5,
    color = "#1F2937"
  ) +
  
  scale_fill_manual(
    values = c(
      "firefox" = "#14B8A6",
      "ie" = "#3B82F6",
      "chrome" = "#6366F1",
      "safari" = "#EF4444"
    ),
    labels = c(
      "firefox" = "Firefox",
      "ie" = "IE",
      "chrome" = "Chrome",
      "safari" = "Safari"
    )
  ) +
  
  scale_x_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, by = 10),
    expand = c(0, 0)
  ) +
  
  scale_y_discrete(
    labels = c(
      "safari" = "Safari",
      "chrome" = "Chrome",
      "ie" = "IE",
      "firefox" = "Firefox"
    )
  ) +
  
  labs(
    title = "Conversion Rate by Browser (%)",
    x = "Conversion Rate (%)",
    y = "Browser",
    fill = NULL   # removes legend title box
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#D9DEE7", linewidth = 0.6),
    
    axis.line = element_line(color = "#7A7A7A"),
    axis.ticks = element_line(color = "#7A7A7A"),
    
    axis.text = element_text(color = "#374151"),
    axis.title = element_text(color = "#374151"),
    
    plot.title = element_text(face = "bold", size = 17, color = "#1F2937"),
    
   
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 11),
    legend.key.size = unit(1.2, "lines")
  )

p2

# Plot 3: Conversion Rate by Market (%)

country_data <- S_A_Written_Project_Data_Set_2_ %>%
  filter(!is.na(country), country != "") %>%
  group_by(country) %>%
  summarise(
    total_visits = sum(visits, na.rm = TRUE),
    total_conversions = sum(conversions, na.rm = TRUE)
  ) %>%
  mutate(conversion_rate_pct = round((total_conversions / total_visits) * 100, 1)) %>%
  arrange(conversion_rate_pct)


p3 <- ggplot(
  country_data,
  aes(x = conversion_rate_pct, y = reorder(country, conversion_rate_pct),
      fill = "Conversion Rate (%)")
) +
  geom_col(width = 0.55) +
  
  geom_text(
    aes(label = round(conversion_rate_pct, 0)),
    hjust = -0.2,
    size = 5,
    color = "#1F2937"
  ) +
  
  scale_fill_manual(
    values = c("Conversion Rate (%)" = "#14B8A6")
  ) +
  
  scale_x_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 10),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Conversion Rate by Market (%)",
    x = "Conversion Rate (%)",
    y = "Market",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#D9DEE7", linewidth = 0.6),
    
    axis.line = element_line(color = "#7A7A7A"),
    axis.ticks = element_line(color = "#7A7A7A"),
    
    axis.text = element_text(color = "#374151"),
    axis.title = element_text(color = "#374151"),
    
    plot.title = element_text(face = "bold", size = 17, color = "#1F2937"),
    
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 11)
  )

p3

# Plot 4: Conversion Rate by Browser for Week 6 vs Week 7 (%)

library(dplyr)
library(ggplot2)
library(janitor)

S_A_Written_Project_Data_Set_2_ <- clean_names(S_A_Written_Project_Data_Set_2_)

browser_week_data <- S_A_Written_Project_Data_Set_2_ %>%
  filter(week %in% c(6, 7)) %>%
  filter(!is.na(browser), browser != "") %>%
  group_by(browser, week) %>%
  summarise(
    total_visits = sum(visits, na.rm = TRUE),
    total_conversions = sum(conversions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(conversion_rate_pct = round((total_conversions / total_visits) * 100, 0))

browser_week_data$browser <- factor(
  browser_week_data$browser,
  levels = c("chrome", "firefox", "ie", "safari"),
  labels = c("Chrome", "Firefox", "IE", "Safari")
)

browser_week_data$week_label <- factor(
  browser_week_data$week,
  levels = c(6, 7),
  labels = c("Week 6 Rate (%)", "Week 7 Rate (%)")
)

p4 <- ggplot(
  browser_week_data,
  aes(x = browser, y = conversion_rate_pct, fill = week_label)
) +
  geom_col(
    position = position_dodge(width = 0.6),
    width = 0.28
  ) +
  geom_text(
    aes(label = conversion_rate_pct),
    position = position_dodge(width = 0.6),
    vjust = -0.35,
    size = 5,
    color = "#1F2937"
  ) +
  scale_fill_manual(
    values = c(
      "Week 6 Rate (%)" = "#C4CCD8",
      "Week 7 Rate (%)" = "#635BFF"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, by = 10),
    expand = c(0, 0)
  ) +
  labs(
    title = "Conversion Rate: Week 6 vs Week 7 by Browser (%)",
    x = "Browser",
    y = "Conversion Rate (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#D9DEE7", linewidth = 0.6),
    
    axis.line = element_line(color = "#7A7A7A"),
    axis.ticks = element_line(color = "#7A7A7A"),
    
    axis.text = element_text(color = "#374151", size = 11),
    axis.title = element_text(color = "#374151", size = 12),
    
    plot.title = element_text(face = "plain", size = 16, color = "#1F2937", hjust = 0.5),
    
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

p4

# Plot 5: Selected Markets vs Average Conversion Rate (%)

library(dplyr)
library(ggplot2)
library(janitor)

S_A_Written_Project_Data_Set_2_ <- clean_names(S_A_Written_Project_Data_Set_2_)

# Overall average conversion rate
avg_rate <- S_A_Written_Project_Data_Set_2_ %>%
  summarise(avg_rate = round(sum(conversions, na.rm = TRUE) / sum(visits, na.rm = TRUE) * 100, 0)) %>%
  pull(avg_rate)

# Country-level conversion rates
country_data <- S_A_Written_Project_Data_Set_2_ %>%
  filter(!is.na(country), country != "") %>%
  group_by(country) %>%
  summarise(
    total_visits = sum(visits, na.rm = TRUE),
    total_conversions = sum(conversions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(conversion_rate_pct = round((total_conversions / total_visits) * 100, 0))

# Keep only the selected markets from your chart
selected_markets <- country_data %>%
  filter(country %in% c("AI", "AA", "AD", "AH", "AC")) %>%
  transmute(
    market = paste("Market", country),
    conversion_rate_pct = conversion_rate_pct
  )

# Add the average rate row
plot_data <- bind_rows(
  selected_markets,
  data.frame(
    market = "Avg Rate",
    conversion_rate_pct = avg_rate
  )
)

# Order bars exactly as shown in your example (top to bottom)
plot_data$market <- factor(
  plot_data$market,
  levels = c("Market AI", "Market AA", "Market AD", "Market AH", "Market AC", "Avg Rate")
)

# Matching legend labels
plot_data$legend_label <- factor(
  plot_data$market,
  levels = c("Avg Rate", "Market AC", "Market AH", "Market AD", "Market AA", "Market AI")
)

p5 <- ggplot(
  plot_data,
  aes(x = conversion_rate_pct, y = market, fill = legend_label)
) +
  geom_col(width = 0.42) +
  geom_text(
    aes(label = conversion_rate_pct),
    hjust = -0.25,
    size = 5,
    color = "#1F2937"
  ) +
  scale_fill_manual(
    values = c(
      "Avg Rate"   = "#3B82F6",
      "Market AC"  = "#14B8A6",
      "Market AH"  = "#8B5CF6",
      "Market AD"  = "#94A3B8",
      "Market AA"  = "#F59E0B",
      "Market AI"  = "#EF4444"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 85),
    breaks = seq(0, 80, by = 10),
    expand = c(0, 0)
  ) +
  labs(
    title = "Selected Markets vs Average Conversion Rate (%)",
    x = "Conversion Rate (%)",
    y = "Market",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#D9DEE7", linewidth = 0.6),
    
    axis.line = element_line(color = "#7A7A7A"),
    axis.ticks = element_line(color = "#7A7A7A"),
    
    axis.text = element_text(color = "#374151", size = 11),
    axis.title = element_text(color = "#374151", size = 12),
    
    plot.title = element_text(face = "plain", size = 16, color = "#1F2937", hjust = 0.5),
    
    legend.position = "bottom",
    legend.text = element_text(size = 11, face = "bold"),
    legend.key.size = unit(1.1, "lines")
  )

p5