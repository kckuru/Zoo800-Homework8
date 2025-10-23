##########################
####### Homework 8 #######
##########################
# Group members: Keeley Kuru, Kristine Schoenecker
# Date: 10/23/25

# ===== Objective 1 ===== #

# Load necessary libraries
library(EDIutils)
library(readr)
library(ggplot2)
library(tidyverse)

# Get the newest revision number for the data package
scope <- "knb-lter-ntl"
identifier <- "2"  # Changed from "24" to "2" based on your link
revision <- list_data_package_revisions(scope, identifier, filter = "newest")
package_id <- paste(scope, identifier, revision, sep = ".")

# View all available entity names to find the correct one
entity_names <- read_data_entity_names(package_id)
print(entity_names)  # This will show you all available datasets

# Find the entity ID for the Major Ions dataset
# Look for the exact entity name from the print output above
entity_id <- entity_names$entityId[
  grepl("Major Ions", entity_names$entityName, ignore.case = TRUE)
]

# If there are multiple matches, you may need to be more specific
# or use the first match:
if(length(entity_id) > 1) {
  print("Multiple matches found. Using the first one:")
  print(entity_names$entityName[grepl("Major Ions", entity_names$entityName, ignore.case = TRUE)])
  entity_id <- entity_id[1]
}

# Download the raw data (bytes) and parse with read_csv
raw_bytes <- read_data_entity(package_id, entity_id)
data <- read_csv(file = raw_bytes)

# View the structure of the data
str(data)
head(data)

summary(data)

# See which lakes are in the dataset
unique(data$lakeid)

# Check date range
range(data$sampledate, na.rm = TRUE)

# ======= Objective 2 (parts A and B) ======= #
# Example plots #
# Filter data for Trout Bog only
tb_data <- data %>% filter(lakeid == "TB")

# Identify the top calcium outliers
top_ca_outliers <- tb_data %>%
  filter(!is.na(ca)) %>%
  arrange(desc(ca)) %>%
  slice(1:5)  # Select top 5 highest calcium values

print(top_ca_outliers %>% select(sampledate, ca, depth))

# Identify the bottom calcium outliers (lowest values)
bottom_ca_outliers <- tb_data %>%
  filter(!is.na(ca)) %>%
  arrange(ca) %>%  # Remove desc() to sort ascending (lowest first)
  slice(1:5)  # Select bottom 5 lowest calcium values

print(bottom_ca_outliers %>% select(sampledate, ca, depth))

# 1. TB major ion comparisons including circles around ca outliers
TB_major_ions_comparison <- tb_data %>%
  filter(!is.na(ca) | !is.na(mg) | !is.na(na) | !is.na(k)) %>%
  select(sampledate, year4, depth, ca, mg, na, k) %>%
  pivot_longer(cols = c(ca, mg, na, k), 
               names_to = "ion", 
               values_to = "concentration") %>%
  filter(!is.na(concentration)) %>%
  ggplot(aes(x = sampledate, y = concentration, color = ion)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE) +
  # Add circles around the outliers
  geom_point(data = top_ca_outliers,
             aes(x = sampledate, y = ca),
             color = "black", 
             size = 5, 
             shape = 21,
             stroke = 2,
             fill = NA,
             inherit.aes = FALSE) +
  labs(title = "Trout Bog: Major Cation Concentrations",
       subtitle = paste0("Total observations - Ca: ", nrow(tb_ca), 
                         ", Mg: ", sum(!is.na(tb_data$mg)),
                         ", Na: ", sum(!is.na(tb_data$na)),
                         ", K: ", sum(!is.na(tb_data$k)),
                         "\nBlack circles = top 5 calcium outliers"),
       x = "Date",
       y = "Ion Concentration (mg/L)",
       color = "Ion") +
  theme_minimal()

print(TB_major_ions_comparison)

# Plot 1 annotation: 
# The major cations in Trout Bog Lake show temporal patterns, 
# with calcium generally exhibiting higher concentrations compared to magnesium, sodium, and potassium.
# The black circles indicate the five highest calcium concentration outliers.


# Filter specifically for calcium in TB
tb_ca <- data %>% 
  filter(lakeid == "TB", !is.na(ca))

# 2.Trout Bog Calcium Concentration Over Time
TB_calcium_data_over_years <- ggplot(tb_ca, aes(x = sampledate, y = ca)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "loess", se = TRUE, color = "blue4") +
  geom_point(data = top_ca_outliers,
             aes(x = sampledate, y = ca),
             color = "black", 
             size = 5, 
             shape = 21,
             stroke = 2,
             fill = NA,
             inherit.aes = FALSE) +
  geom_point(data = bottom_ca_outliers,
             aes(x = sampledate, y = ca),
             color = "green", 
             size = 5, 
             shape = 21,
             stroke = 2,
             fill = NA,
             inherit.aes = FALSE) +
  labs(title = "Trout Bog: Calcium Concentration Over Time",
       x = "Date",
       y = "Calcium (mg/L)") +
  theme_minimal()

TB_calcium_data_over_years

# Plot 2 annotation: 
# The calcium concentration in Trout Bog Lake shows variability over time,
# with periods of increasing and decreasing concentrations of calcium (mg/L).
# There are outliers that suggest occasional spikes or decreases in calcium levels.
# Black circles indicate the top 5 highest calcium values, while
# green circles indicate the bottom 5 lowest calcium values.


# 3. Trout Bog Calcium Concentration Over Time Colored by Depth
TB_calcium_data_over_years_by_depth <- ggplot(tb_ca, aes(x = sampledate, y = ca, color = depth)) +
  geom_point(alpha = 0.6) +
  geom_point(data = top_ca_outliers,
             aes(x = sampledate, y = ca),
             color = "black", 
             size = 5, 
             shape = 21,
             stroke = 2,
             fill = NA,
             inherit.aes = FALSE) +
  geom_point(data = bottom_ca_outliers,
             aes(x = sampledate, y = ca),
             color = "green", 
             size = 5, 
             shape = 21,
             stroke = 2,
             fill = NA,
             inherit.aes = FALSE) +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Trout Bog: Calcium Over Time (Colored by Depth)",
       x = "Date",
       y = "Calcium (mg/L)",
       color = "Depth (m)") +
  theme_minimal()

TB_calcium_data_over_years_by_depth

# Plot 3 annotation: 
# Calcium concentrations in Trout Bog Lake vary with depth over time.
# Generally, lower depths tend to have higher calcium concentrations.
# Black circles indicate the top 5 highest calcium values, while
# green circles indicate the bottom 5 lowest calcium values.

