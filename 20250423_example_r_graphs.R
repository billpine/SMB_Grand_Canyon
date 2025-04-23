# R Script for Data Cleaning and Visualization

# Load necessary libraries
library(data.table)    # Efficient data handling
library(dplyr)         # Data manipulation
library(ggplot2)       # Plotting
library(lubridate)     # Date and time handling
library(scales)        # For squish()

#----------------------------------------
# Define consistent ggplot theme
#----------------------------------------
theme_consistent <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10)
    )
}



# Define labels and colors
site_labels <- c(
  jensen = "Jensen",
  gcd = "Glen Canyon Dam",
  cr_gc = "Colorado River near Grand Canyon",
  diamond = "Diamond Creek"
)

site_colors <- c(
  jensen = "#0072B2",
  gcd = "#D55E00",
  cr_gc = "brown",
  diamond = "#CC79A7"
)



#----------------------------------------
# Define output folder for saving figures
#----------------------------------------
output_dir <- "E:/Users/Owner/Documents/SWCA/SMB/20241115_paper"

#----------------------------------------
# DATA CLEANING CODE
#----------------------------------------

# Load CSV data using fread for speed
data <- fread("combined_data.csv")

# Create a datetime column by combining date and time
data[, datetime := as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S")]

# Replace invalid entries with NA:
# - temp: valid range 0–30°C
# - turbidity_fnu: no negatives or -999
# - silt_clay: no negatives or -999
data[, temp := fifelse(temp == -999 | temp < 0 | temp > 30, NA_real_, temp)]
data[, turbidity_fnu := fifelse(turbidity_fnu == -999 | turbidity_fnu < 0, NA_real_, turbidity_fnu)]
data[, silt_clay := fifelse(silt_clay == -999 | silt_clay < 0, NA_real_, silt_clay)]

#----------------------------------------
# Compute daily averages by site and day
#----------------------------------------
daily_means <- data %>%
  mutate(day = as.Date(datetime)) %>%
  group_by(site, day) %>%
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    mean_turbidity = mean(turbidity_fnu, na.rm = TRUE),
    mean_silt_clay = mean(silt_clay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = year(day), month = month(day), date = day)

# Filter to sites of interest
daily_means <- daily_means %>%
  filter(site %in% c("jensen", "gcd", "cr_gc", "diamond")) %>%
  mutate(site = factor(site, levels = c("jensen", "gcd", "cr_gc", "diamond")))

#  Generate all combinations of site, year, and month, and include days
all_combinations <- expand.grid(
  site = unique(data$site),
  year = as.numeric(unique(format(as.Date(data$datetime), "%Y"))),  # Ensure Year is numeric
  month = 1:12,
  day = 1:31  # Include all possible days in a month
) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")) %>%
  filter(!is.na(date))  # Remove invalid dates (e.g., February 30)

#  Merge daily means into the all_combinations grid
result <- all_combinations %>%
  left_join(daily_means, by = c("site" = "site", "date" = "day"))

daily_means2<-result

#----------------------------------------
# Save processed data as RDS
#----------------------------------------

# # Define RDS output path
# rds_path <- "E:/Users/Owner/Documents/SWCA/SMB/SMB_Shiny/daily_means2.rds"
# 
# # Save the processed data
# saveRDS(daily_means, rds_path)
# 
# # Optional message
# message("Processed data saved to: ", rds_path)


###
#Graphing for paper
#
###

#Temperature

temperature_trend <- ggplot(daily_means2, aes(x = date, y = mean_temp, color = site)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 16, linetype = "dotted", color = "black", size = 1.4) +
  scale_color_manual(values = cb_palette) +
  scale_x_date(
    date_breaks = "2 years",
    date_minor_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Site Trends Over Years",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = "Site"
  ) +
  facet_wrap(~ site, labeller = labeller(site = custom_labels)) +
  theme_consistent()

##turbidity
turbidity_trend <- ggplot(daily_means2, aes(x = date, y = mean_turbidity, color = site)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "black", size = 1.4) +
  scale_color_manual(values = cb_palette) +
  scale_x_date(
    date_breaks = "2 years",
    date_minor_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Turbidity Trends Over Years",
    x = "Year",
    y = "Mean Turbidity (FNU)",
    color = "Site"
  ) +
  facet_wrap(~ site, labeller = labeller(site = custom_labels)) +
  theme_consistent()


# silt_clay
silt_clay_trend <- ggplot(daily_means2, aes(x = date, y = mean_silt_clay, color = site)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 16, linetype = "dotted", color = "black", size = 1.4) +
  scale_color_manual(values = cb_palette) +
  scale_x_date(
    date_breaks = "2 years",
    date_minor_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Silt-Clay Trends Over Years",
    x = "Year",
    y = "Mean Daily Silt-Clay",
    color = "Site"
  ) +
  facet_wrap(~ site, labeller = labeller(site = custom_labels)) +
  theme_consistent()


##Below is how F2 and F4 are made
library(tidyverse)
library(patchwork)

# Reorder site factor
daily_means2$site <- factor(daily_means2$site, levels = c("jensen", "gcd", "cr_gc", "diamond"))

# Filter for available data not every station has every measurement
temp_data <- daily_means2 %>% filter(!is.na(mean_temp))
turb_data <- daily_means2 %>% filter(!is.na(mean_turbidity))
silt_data <- daily_means2 %>% filter(!is.na(mean_silt_clay))





# Temperature plot (F2)
# Reorder for temperature plot
temp_data <- temp_data %>%
  filter(site %in% c("jensen", "gcd", "cr_gc", "diamond")) %>%
  mutate(site = factor(site, levels = c("jensen", "gcd", "cr_gc", "diamond")))

F2_temperature_trend <- ggplot(temp_data, aes(x = date, y = mean_temp, color = site)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 16, linetype = "dotted", color = "black", size = 1.4) +
  scale_color_manual(values = cb_palette) +
  scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Mean Daily Temperature (°C)", x = "Year", y = NULL, color = "Site") +
  facet_wrap(~ site, labeller = labeller(site = custom_labels), drop = FALSE) +
  theme_consistent()+
  theme(legend.position = "none")

ggsave(
  filename = file.path(output_dir, "F2_temperature_by_site.jpg"),
  plot = F2_temperature_trend,
  width = 12,
  height = 5,
  dpi = 600
)

# Turbidity plot (F4)
# For this specific plot
turb_data <- turb_data %>%
  filter(site %in% c("gcd", "cr_gc", "diamond")) %>%
  mutate(site = factor(site, levels = c("gcd", "cr_gc", "diamond")))

F4_turbidity_trend <- ggplot(turb_data, aes(x = date, y = mean_turbidity, color = site)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "black", size = 1.4) +
  scale_color_manual(values = cb_palette) +
  scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Mean Daily Turbidity (FNU)", x = "Year", y = NULL, color = "Site") +
  facet_wrap(~ site, labeller = labeller(site = custom_labels), drop = FALSE) +
  theme_consistent()+
  theme(legend.position = "none")

# Silt-clay plot
# Reorder for this specific plot
silt_data <- silt_data %>%
  filter(site %in% c("jensen", "cr_gc", "diamond")) %>%
  mutate(site = factor(site, levels = c("jensen", "cr_gc", "diamond")))

F4_silt_clay_trend <- ggplot(silt_data, aes(x = date, y = mean_silt_clay, color = site)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 16, linetype = "dotted", color = "black", size = 1.4) +
  scale_color_manual(values = cb_palette) +
  scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Mean Daily Silt-Clay", x = "Year", y = NULL, color = "Site") +
  facet_wrap(~ site, labeller = labeller(site = custom_labels), drop = FALSE) +
  theme_consistent()+
  theme(legend.position = "none")

# Combine turbidity + silt-clay with shared legend (Figure B)
F4_combined_turbidity_silt <- F4_turbidity_trend / F4_silt_clay_trend +
  plot_layout(guides = "collect") & theme(legend.position = "none")

ggsave(
  filename = file.path(output_dir, "F4_turbidity_siltclay_by_site.jpg"),
  plot = F4_combined_turbidity_silt,
  width = 12,
  height = 10,
  dpi = 600
)

##
#Polar F3, imported code
library(ggplot2)
library(ggpattern)
library(dplyr)
library(lubridate)
library(patchwork)
library(cowplot)
library(tidyr)
library(purrr)


#----------------------------------------
# Preprocess temperature data
#----------------------------------------
polar_data <- daily_means2 %>%
  mutate(
    Date = as.Date(date),
    Month = factor(month(Date), levels = 1:12, labels = month.abb),
    year = year(Date)
  )

#----------------------------------------
# Shared color scale: centered at 16°C
#----------------------------------------
global_min <- min(polar_data$mean_temp, na.rm = TRUE)
global_max <- max(polar_data$mean_temp, na.rm = TRUE)

shared_fill_scale <- scale_fill_gradientn(
  colors = c("blue", "white", "red"),
  values = scales::rescale(c(global_min, 16, global_max)),
  na.value = "black",
  limits = c(global_min, global_max),
  name = "Mean Temp (°C)"
)

#----------------------------------------
# Polar plot function per site (with custom label)
#----------------------------------------
create_individual_polar_plot <- function(polar_data, site_name, shared_fill_scale, plot_title = site_name) {
  site_data <- polar_data %>% filter(site == site_name)
  
  if (nrow(site_data) == 0) return(NULL)  # Skip if no data for this site
  
  monthly_data <- site_data %>%
    group_by(year, Month) %>%
    summarise(MeanTemp = mean(mean_temp, na.rm = TRUE), .groups = 'drop') %>%
    complete(year, Month, fill = list(MeanTemp = NA)) %>%
    mutate(
      Radius = year - min(year) + 1,
      Pattern = ifelse(!is.na(MeanTemp) & MeanTemp >= 16, "dots", "none")
    )
  
  year_labels <- monthly_data %>%
    group_by(year) %>%
    summarise(Radius = first(Radius)) %>%
    filter(year %% 4 == 0) %>%
    mutate(Month = factor("Dec", levels = levels(monthly_data$Month)))
  
  ggplot(monthly_data, aes(x = Month, y = Radius, fill = MeanTemp)) +
    geom_tile_pattern(
      aes(pattern = Pattern, width = 1, height = 1),
      color = "black",
      pattern_fill = "black",
      pattern_spacing = 0.02
    ) +
    geom_text(
      data = year_labels,
      aes(x = Month, y = Radius, label = year),
      inherit.aes = FALSE,
      color = "black",
      size = 3
    ) +
    scale_y_continuous(breaks = unique(monthly_data$Radius)) +
    shared_fill_scale +
    scale_pattern_manual(values = c("none" = "none", "dots" = "circle"), guide = "none") +
    coord_polar(theta = "x") +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(title = plot_title, x = NULL, y = NULL)
}

#----------------------------------------
# Generate plots in the correct site order with labels
#----------------------------------------
site_order <- c("jensen", "gcd", "cr_gc", "diamond")
site_labels <- c("Jensen", "Glen Canyon Dam", "Colorado River near Grand Canyon", "Diamond Creek")

polar_plots <- map2(
  site_order,
  site_labels,
  ~ create_individual_polar_plot(polar_data, site_name = .x, shared_fill_scale = shared_fill_scale, plot_title = .y)
)
polar_plots <- compact(polar_plots)  # Drop NULLs for sites with no data

#----------------------------------------
# Create shared legend
#----------------------------------------
legend_plot <- ggplot(data.frame(x = 1, y = 1, MeanTemp = c(global_min, 16, global_max)), aes(x = x, y = y, fill = MeanTemp)) +
  geom_tile() +
  shared_fill_scale +
  theme_void() +
  theme(legend.position = "bottom", legend.title = element_text(size = 10))

legend <- cowplot::get_legend(legend_plot)

#----------------------------------------
# Combine plots and save
#----------------------------------------
combined_plot <- cowplot::plot_grid(
  cowplot::plot_grid(plotlist = polar_plots, ncol = 2),
  legend,
  ncol = 1,
  rel_heights = c(4, 0.5)
)

ggsave(
  filename = file.path(output_dir, "F3_polar_temperature_by_site.jpg"),
  plot = combined_plot,
  width = 10,
  height = 8,
  dpi = 300
)




###

#---------------------------
# Figure 6: Days with High Silt-Clay per Year
#---------------------------

# Identify years with complete data for all three sites
valid_years <- daily_means %>%
  filter(!is.na(mean_silt_clay)) %>%
  group_by(year, month) %>%
  summarise(sites_with_data = n_distinct(site), .groups = "drop") %>%
  group_by(year) %>%
  summarise(all_months_complete = all(sites_with_data == 3), .groups = "drop") %>%
  filter(all_months_complete) %>%
  pull(year)

# Filter data for valid years only
df6 <- daily_means %>%
  filter(year %in% valid_years)

# Count days with mean_silt_clay ≥ 200
days_count <- df6 %>%
  filter(mean_silt_clay >= 200) %>%
  group_by(site, year) %>%
  summarise(valid_days = n_distinct(date), .groups = "drop")

# Bar plot
f6 <- ggplot(days_count, aes(x = year, y = valid_days, fill = site)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~ site, scales = "fixed", labeller = labeller(site = site_labels)) +
  scale_fill_manual(values = site_colors, labels = site_labels) +
  labs(
    title = "Number of Days Mean Silt-Clay ≥ 200 Per Year",
    x = "Year",
    y = "Number of Days",
    fill = "Site"
  ) +
  theme_consistent()

ggsave(file.path(output_dir, "f6_days_year_silt_clay_gt200.jpg"), f6, width = 10, height = 8, dpi = 600)


#---------------------------
# Figure 7: Daily Silt-Clay by DOY for Jensen and CR_GC
#---------------------------

#take a look at number of days in each year, month, that are common between sites

# Step 1: Filter to relevant sites and non-missing silt-clay
df_shared <- daily_means %>%
  filter(site %in% c("jensen", "cr_gc"), !is.na(mean_silt_clay)) %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE))  # Add month labels

# Step 2: Count number of days per site/year/month
site_counts <- df_shared %>%
  group_by(site, year, month) %>%
  summarise(days_present = n(), .groups = "drop")

# Step 3: Join Jensen and CR_GC to find shared days
shared_days <- site_counts %>%
  pivot_wider(names_from = site, values_from = days_present) %>%
  mutate(shared = pmin(jensen, cr_gc, na.rm = TRUE)) %>%
  filter(!is.na(shared))  # Drop combos without data from both sites

# Step 4: Format into month x year table
shared_table <- shared_days %>%
  select(year, month, shared) %>%
  pivot_wider(names_from = year, values_from = shared)

# Step 5: Arrange by calendar month order
shared_table <- shared_table %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(month)

# View the table
print(shared_table, n = Inf)



##
# Site labels for facets
site_labels <- c(
  jensen = "Jensen",
  gcd = "Glen Canyon Dam",
  cr_gc = "Colorado River near Grand Canyon",
  diamond = "Diamond Creek"
)

# Calculate shared day counts per year-month
shared_counts <- daily_means %>%
  filter(site %in% c("jensen", "cr_gc"), !is.na(mean_silt_clay)) %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(site, year, month) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  pivot_wider(names_from = site, values_from = n_obs) %>%
  mutate(shared = pmin(jensen, cr_gc, na.rm = TRUE)) %>%
  filter(shared >= 5)

# Step 2: Filter original data using valid year-months with enough shared data
df7 <- daily_means %>%
  mutate(year = year(date), month = month(date)) %>%
  semi_join(shared_counts, by = c("year", "month")) %>%
  filter(site %in% c("jensen", "cr_gc"), year >= 2013) %>%
  mutate(DayOfYear = yday(date))

# Step 3: Plot
f7 <- ggplot(df7, aes(x = DayOfYear, y = mean_silt_clay, color = as.factor(year), group = year)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "black", size = 1) +
  scale_color_viridis_d(option = "C", name = "Year") +
  scale_x_continuous(
    breaks = cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)) + 15,
    labels = month.abb
  ) +
  labs(
    title = "Mean Daily Silt+Clay Concentrations by Day of Year and Site\n(Only Month-Year Pairs with ≥5 Shared Days)",
    x = "Month",
    y = "Mean Daily Silt+Clay (mg/L)"
  ) +
  facet_wrap(~ site, labeller = labeller(site = site_labels)) +
  theme_consistent()

ggsave(file.path(output_dir, "F7_silt_clay_trend_month.jpg"), f7, width = 10, height = 8, dpi = 600)


#---------------------------
# Figure 8: Days ≥ 200 by Month-Year
#---------------------------

days_by_month <- df6 %>%
  filter(mean_silt_clay >= 200) %>%
  filter(!is.na(year) & !is.na(month)) %>%
  group_by(site, year, month) %>%
  summarise(valid_days = n_distinct(date), .groups = "drop")

f8 <- ggplot(days_by_month, aes(x = factor(month), y = valid_days, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, color = "black") +
  facet_wrap(~ year) +
  scale_x_discrete(labels = month.abb) +
  scale_fill_manual(values = site_colors, labels = site_labels) +
  labs(
    title = "Number of Days Mean Silt-Clay ≥ 200 Per Month",
    x = "Month",
    y = "Number of Days",
    fill = "Site"
  ) +
  theme_consistent()

ggsave(file.path(output_dir, "f8_days_month_year_silt_clay_gt200.jpg"), f8, width = 10, height = 8, dpi = 600)


#---------------------------
# Figure 9: Summer Silt-Clay ≥ 200
#---------------------------
summer_days <- df6 %>%
  filter(!is.na(year) & !is.na(month)) %>%
  filter(site != "gcd", month %in% 7:10, mean_silt_clay >= 200) %>%
  group_by(site, year, month) %>%
  summarise(count_days = n_distinct(date), .groups = "drop")

# Only keep non-NA years for the grid
summer_grid <- expand.grid(
  site = unique(df6$site[df6$site != "gcd"]),
  year = unique(df6$year[!is.na(df6$year)]),
  month = 7:10
)

summer_complete <- summer_grid %>%
  left_join(summer_days, by = c("site", "year", "month")) %>%
  mutate(count_days = ifelse(is.na(count_days), 0, count_days))

f9 <- ggplot(summer_complete, aes(x = factor(month), y = count_days, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, color = "black") +
  facet_wrap(~ year) +
  scale_x_discrete(labels = c("Jul", "Aug", "Sep", "Oct")) +
  scale_fill_manual(values = site_colors, labels = site_labels) +
  labs(
    title = "Number of Days Per Month in Summer Silt-Clay ≥ 200",
    x = "Month",
    y = "Number of Days",
    fill = "Site"
  ) +
  theme_consistent()

ggsave(file.path(output_dir, "f9_summer_turbidity_days.jpg"), f9, width = 10, height = 8, dpi = 600)

#
# Define individual site data for Figures 10–12
data_jensen <- daily_means %>%
  filter(site == "jensen", year >= 2017, year <= 2022) %>%
  mutate(
    doy = yday(date),
    scaled_silt_clay = mean_silt_clay / 100
  )

data_cr_gc <- daily_means %>%
  filter(site == "cr_gc", year >= 2017, year <= 2022) %>%
  mutate(
    doy = yday(date),
    scaled_silt_clay = mean_silt_clay / 100
  )

data_diamond <- daily_means %>%
  filter(site == "diamond", year >= 2017) %>%
  mutate(
    doy = yday(date),
    scaled_silt_clay = mean_silt_clay / 100
  )

#---------------------------
# Figure 10: Jensen Temperature + Silt-Clay
#---------------------------
plot_jensen <- ggplot(data_jensen) +
  geom_line(aes(x = doy, y = mean_temp), color = "black", size = 1) +
  geom_bar(
    aes(x = doy, y = scales::squish(scaled_silt_clay, range = c(0, 30))),
    stat = "identity", fill = site_colors["jensen"], alpha = 0.5,
    data = data_jensen %>% filter(!is.na(scaled_silt_clay))
  ) +
  geom_hline(yintercept = 2, color = "orange", linetype = "dashed", size = 0.8) +
  geom_hline(yintercept = 16, color = "brown", linetype = "dashed", size = 0.8) +
  facet_wrap(~ year) +
  scale_y_continuous("Mean Daily Temperature and Mean Silt+Clay ÷ 100", limits = c(0, 30)) +
  scale_x_continuous("Month of Year", breaks = seq(15, 345, 30), labels = month.abb) +
  labs(title = "Site: Jensen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "F10_jensen_temp_turb_ref_line.jpeg"), plot_jensen, width = 10, height = 8, dpi = 600)




#---------------------------
# Figure 11: CR_GC Temperature + Silt-Clay
#---------------------------

plot_cr_gc <- ggplot(data_cr_gc) +
  geom_line(
    aes(x = doy, y = scales::squish(mean_temp, range = c(0, 30))),
    data = data_cr_gc %>% filter(!is.na(mean_temp)), color = "black", size = 1
  ) +
  geom_bar(
    aes(x = doy, y = scales::squish(scaled_silt_clay, range = c(0, 30))),
    data = data_cr_gc %>% filter(!is.na(scaled_silt_clay)), stat = "identity",
    fill = site_colors["cr_gc"], alpha = 0.5
  ) +
  geom_hline(yintercept = 2, color = "orange", linetype = "dashed", size = 0.8) +
  geom_hline(yintercept = 16, color = "brown", linetype = "dashed", size = 0.8) +
  facet_wrap(~ year) +
  scale_y_continuous("Mean Daily Temperature and Mean Silt+Clay ÷ 100", limits = c(0, 30)) +
  scale_x_continuous("Month of Year", breaks = seq(15, 345, 30), labels = month.abb) +
  labs(title = "Site: Colorado River near Grand Canyon") +
  theme_consistent()

ggsave(file.path(output_dir, "F11_cr_gc_temp_turb.jpeg"), plot_cr_gc, width = 10, height = 8, dpi = 600)


#---------------------------
# Figure 12: Diamond Temperature + Silt-Clay
#---------------------------

plot_diamond <- ggplot(data_diamond) +
  geom_line(
    aes(x = doy, y = scales::squish(mean_temp, range = c(0, 30))),
    data = data_diamond %>% filter(!is.na(mean_temp)), color = "black", size = 1
  ) +
  geom_bar(
    aes(x = doy, y = scales::squish(scaled_silt_clay, range = c(0, 30))),
    data = data_diamond %>% filter(!is.na(scaled_silt_clay)), stat = "identity",
    fill = site_colors["diamond"], alpha = 0.5
  ) +
  geom_hline(yintercept = 2, color = "orange", linetype = "dashed", size = 0.8) +
  geom_hline(yintercept = 16, color = "brown", linetype = "dashed", size = 0.8) +
  facet_wrap(~ year) +
  scale_y_continuous("Mean Daily Temperature and Mean Silt+Clay ÷ 100", limits = c(0, 30)) +
  scale_x_continuous("Month of Year", breaks = seq(15, 345, 30), labels = month.abb) +
  labs(title = "Site: Diamond") +
  theme_consistent()

ggsave(file.path(output_dir, "F12_diamond_temp_turb.jpeg"), plot_diamond, width = 10, height = 8, dpi = 600)



#

#----------------------------------------
# Figure 14: Diamond temperature overlay with turbidity threshold coloring
#----------------------------------------

# Filter Diamond Creek data for 2003–2024
diamond_data <- daily_means %>%
  filter(site == "diamond", date >= as.Date("2003-01-01"), date <= as.Date("2024-12-31")) %>%
  arrange(date) %>%
  mutate(
    # Identify if turbidity exceeds 50 FNU threshold
    high_turbidity = mean_turbidity >= 50,
    # Assign group ID when turbidity condition changes
    group_id = cumsum(c(TRUE, diff(high_turbidity) != 0))
  )

# Define x-axis breaks every 2 years for readability
x_breaks <- seq(as.Date("2004-01-01"), as.Date("2024-01-01"), by = "2 years")

# Create the overlay plot
plot_diamond_overlay <- ggplot(diamond_data, aes(x = date, y = mean_temp)) +
  # Plot lines, coloring based on turbidity level
  geom_line(aes(group = group_id, color = high_turbidity), size = 1) +
  # Add horizontal threshold reference line for temperature
  geom_hline(yintercept = 16, linetype = "dotted", color = "black", size = 1) +
  # Set custom colors for turbidity condition
  scale_color_manual(
    values = c("TRUE" = "red", "FALSE" = "black"),
    labels = c("FALSE" = "< 50 Turbidity", "TRUE" = "≥ 50 Turbidity"),
    name = "Turbidity Condition"
  ) +
  # Format x-axis for time
  scale_x_date(
    breaks = x_breaks,
    date_labels = "%Y",
    limits = c(as.Date("2003-01-01"), as.Date("2024-12-31"))
  ) +
  # Add plot title and axis labels
  labs(
    title = "Temperature at Diamond Creek (2003–2024)\nRed = Turbidity ≥ 50 FNU",
    x = "Year",
    y = "Mean Temperature (°C)"
  ) +
  theme_consistent()

# Display the plot
print(plot_diamond_overlay)

# Save the plot to file
ggsave(
  filename = file.path(output_dir, "F14_diamond_temp_overlay_turb_from2003.jpg"),
  plot = plot_diamond_overlay,
  device = "jpeg",
  width = 10,
  height = 6,
  dpi = 600
)


##
# Filter Colorado River near Grand Canyon data for 2003–2024
cr_gc_data <- daily_means %>%
  filter(site == "cr_gc", date >= as.Date("2003-01-01"), date <= as.Date("2024-12-31")) %>%
  arrange(date) %>%
  mutate(
    # Identify if turbidity exceeds 50 FNU threshold
    high_turbidity = mean_turbidity >= 50,
    # Assign group ID when turbidity condition changes
    group_id = cumsum(c(TRUE, diff(high_turbidity) != 0))
  )

# Define x-axis breaks every 2 years for readability
x_breaks <- seq(as.Date("2004-01-01"), as.Date("2024-01-01"), by = "2 years")

# Create the overlay plot
plot_cr_gc_overlay <- ggplot(cr_gc_data, aes(x = date, y = mean_temp)) +
  # Plot lines, coloring based on turbidity level
  geom_line(aes(group = group_id, color = high_turbidity), size = 1) +
  # Add horizontal threshold reference line for temperature
  geom_hline(yintercept = 16, linetype = "dotted", color = "black", size = 1) +
  # Set custom colors for turbidity condition
  scale_color_manual(
    values = c("TRUE" = "red", "FALSE" = "black"),
    labels = c("FALSE" = "< 50 Turbidity", "TRUE" = "≥ 50 Turbidity"),
    name = "Turbidity Condition"
  ) +
  # Format x-axis for time
  scale_x_date(
    breaks = x_breaks,
    date_labels = "%Y",
    limits = c(as.Date("2003-01-01"), as.Date("2024-12-31"))
  ) +
  # Add plot title and axis labels
  labs(
    title = "Temperature at Colorado River near Grand Canyon (2003–2024)\nRed = Turbidity ≥ 50 FNU",
    x = "Year",
    y = "Mean Temperature (°C)"
  ) +
  theme_consistent()

# Display the plot
print(plot_cr_gc_overlay)

##
# 
library(patchwork)

# Stack the two plots vertically
combined_overlay_plot <- plot_cr_gc_overlay / plot_diamond_overlay +
  plot_layout(ncol = 1) +  # 1 column = vertical stacking
  plot_annotation(title = "Temperature and Turbidity Overlay (2003–2024)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

# Display
print(combined_overlay_plot)

# Save to file
ggsave(
  filename = file.path(output_dir, "F14_combined_temp_overlay_two_sites.jpg"),
  plot = combined_overlay_plot,
  device = "jpeg",
  width = 10,
  height = 10,   # Taller to accommodate both stacked plots
  dpi = 600
)

##

# Combine data for both sites and summarize
site_summary <- daily_means %>%
  filter(site %in% c("cr_gc", "diamond"),
         !is.na(mean_temp),
         !is.na(mean_turbidity)) %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(site, year) %>%
  summarise(
    total_days = n(),
    high_temp_high_turbidity_days = sum(mean_temp >= 16 & mean_turbidity >= 50),
    proportion = high_temp_high_turbidity_days / total_days,
    .groups = "drop"
  )

# Plot both sites
ggplot(site_summary, aes(x = year, y = high_temp_high_turbidity_days, color = site)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Number of Days per Year with Temp ≥ 16°C & Turbidity ≥ 50 FNU",
    x = "Year",
    y = "Number of Days",
    color = "Site"
  ) +
  scale_color_manual(values = c("cr_gc" = "firebrick", "diamond" = "steelblue"),
                     labels = c("cr_gc" = "Colorado River (GC)", "diamond" = "Diamond Creek")) +
  theme_minimal()


#Plot faceted by site
ggplot(site_summary, aes(x = year, y = high_temp_high_turbidity_days)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "red", size = 2) +
  facet_wrap(~ site, labeller = as_labeller(site_labels)) +
  labs(
    title = "Number of Days per Year with Temp ≥ 16°C & Turbidity ≥ 50 FNU",
    x = "Year",
    y = "Number of Days"
  ) +
  theme_minimal()
