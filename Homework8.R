##########################
####### Homework 8 #######
##########################
# Group members: Keeley Kuru, Kristine Schoenecker
# Date: 10/23/25

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

# Count observations per lake
table(data$lakeid)

# Check date range
range(data$sampledate, na.rm = TRUE)

# ==============
# Example plots
# ==============

# 1. Box plot of conductivity by lake
ggplot(data %>% filter(!is.na(cond)), 
       aes(x = lakeid, y = cond, fill = lakeid)) +
  geom_boxplot() +
  labs(title = "Conductivity by Lake",
       x = "Lake ID",
       y = "Conductivity (µS/cm)") +
  theme_minimal() +
  theme(legend.position = "none")


# 2. Calcium concentrations over time for all lakes
ggplot(data %>% filter(!is.na(ca)), 
       aes(x = sampledate, y = ca, color = lakeid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(title = "Calcium Concentrations Over Time",
       x = "Date",
       y = "Calcium (mg/L)",
       color = "Lake ID") +
  theme_minimal()


# 4. Compare major ions for Trout Bog over years
data %>%
  filter(lakeid == "TB", year4 >= 2020) %>%
  select(sampledate, ca, mg, na, k) %>%
  pivot_longer(cols = c(ca, mg, na, k), 
               names_to = "ion", 
               values_to = "concentration") %>%
  filter(!is.na(concentration)) %>%
  ggplot(aes(x = sampledate, y = concentration, color = ion)) +
  geom_line() +
  geom_point() +
  labs(title = "Major Ion Concentrations - Trout Bog (2020+)",
       x = "Date",
       y = "Concentration (mg/L)",
       color = "Ion") +
  theme_minimal()
