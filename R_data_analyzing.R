## 1. Data Cleaning and Wrangling  
#Describe and show the data cleaning, transformation and wrangling process' that you have completed. 
#Ensure you provide justification of any data handling and transformation you have completed

library(zoo)
library(tidyverse)
library(readr)

# MERGING
# Step 1: import data files
# Import data file/ districts_info.csv
districts <- read_csv("E:/MSC BA - Year 1/Sem 1/BUSA 8000 - Business Analytics Techniques/Asm1_cleaned 1/files/districts_info.csv")
# Step 2: Checking the missing data
colSums(is.na(districts)) #count the NA's by column

# Import data file/ products_info.csv
# Step 1: import products_info.csv
products <- read_csv("E:/MSC BA - Year 1/Sem 1/BUSA 8000 - Business Analytics Techniques/Asm1_cleaned 1/files/products_info.csv")
#View(products_info)

# Step 2: checking the missing data
colSums(is.na(products)) #count the NA's by column

# change the column LP ID to lp_id to have the approriate formate with the file engagement data to merge
library(dplyr)
names(products)[names(products) == "LP ID"] <- "lp_id"

# Importing Engagement data file:
# for the 5 files of district in engagement, the lp_id will be changed to numeric for merging this 5 files into a dataset
# for each file in the engagement data, the column district_id will be added for the corresponding info of the district.
# In other words, the column district_id has to be added to to emerge the file district_info.csv following the district_id

# Import data file district 1000:
df1000 <- read_csv("E:/MSC BA - Year 1/Sem 1/BUSA 8000 - Business Analytics Techniques/Asm1_cleaned 1/Engagement Data/1000.csv")

# change lp_id to numerical:
df1000$lp_id <- as.numeric(df1000$lp_id)
engage1 <- mutate(df1000, district_id = 1000)
engage1

# District 1039:
## Import data file:
df1039 <- read_csv("E:/MSC BA - Year 1/Sem 1/BUSA 8000 - Business Analytics Techniques/Asm1_cleaned 1/Engagement Data/1039.csv")

# change lp_id to numerical:
df1039$lp_id <- as.numeric(df1039$lp_id)

engage2 <- mutate(df1039, district_id = 1039)
engage2

# District 1044:
## Import data file:
df1044 <- read_csv("E:/MSC BA - Year 1/Sem 1/BUSA 8000 - Business Analytics Techniques/Asm1_cleaned 1/Engagement Data/1044.csv")

# change lp_id to numerical:
df1044$lp_id <- as.numeric(df1044$lp_id)
engage3 <- mutate(df1044, district_id = 1044)
engage3

# District 1052:
## Import data file: 
df1052 <- read_csv("E:/MSC BA - Year 1/Sem 1/BUSA 8000 - Business Analytics Techniques/Asm1_cleaned 1/Engagement Data/1052.csv")

# change lp_id to numerical:
df1052$lp_id <- as.numeric(df1052$lp_id)

engage4 <- mutate(df1052, district_id = 1052)
engage4

# District 1131:
## Import data file:
df1131 <- read_csv("E:/MSC BA - Year 1/Sem 1/BUSA 8000 - Business Analytics Techniques/Asm1_cleaned 1/Engagement Data/1131.csv")

# change lp_id to numerical:
df1131$lp_id <- as.numeric(df1131$lp_id)
engage5 <- mutate(df1131, district_id = 1131)
engage5


# Merging data files:
library(tidyverse)
library(dplyr)

# Combine the list of data frames into a single data frame
# name it as engagement
# Remove engage 2 and engage 5 since there is no information of district for those twos
engagement <- bind_rows(engage1,engage2,engage3,engage4,engage5)

# Merge with products data
merged <- merge(engagement, products, by = "lp_id", all.x = TRUE)
colSums(is.na(merged))
# Merge with districts data
final_merge <- merge(merged, districts, by = "district_id", all.x = TRUE)

# this will be the dataset used for the following section of this analytics
## merging district with engagement file (following district_id)
## merging products file to the above data set (following lp_id)


### we have to add a new colum named district for each file in engagement data
### following the district_id for the merging step

### change the column of products from LP ID to lp_id for merging the file

# change time to month
# Load the necessary package



## Check NA in the emerged dataset:
colSums(is.na(final_merge)) #count the NA's by column


#CLEANING:
final_merge <- final_merge %>% drop_na(`Product Name`)

final_merge <- final_merge %>%
  mutate(state = replace (state, state
                          == "don\x92t know", NA))
final_merge <- final_merge %>%
  mutate(state = replace (state, state
                          == "whereabouts", NA))
final_merge <- final_merge %>%
  mutate(state = replace (state, state
                          == "NaN", NA))

## change percentage:
final_merge <- final_merge %>%
  mutate (
    `pct_black/hispanic` = case_when(
      `pct_black/hispanic` == "[0, 0.2[" ~ "0 - 20%",
      `pct_black/hispanic` == "[0.2, 0.4[" ~ "20% - 40%",
      `pct_black/hispanic` == "[0.4, 0.6[" ~ "40% - 60%",
      `pct_black/hispanic` == "[0.6, 0.8[" ~ "60% - 80%",
      `pct_black/hispanic` == "[0.8, 1[" ~ "80% - 100%"
    )
  )

final_merge <- final_merge %>%
  mutate (
    `pct_free/reduced` = case_when(
      `pct_free/reduced` == "[0, 0.2[" ~ "0 - 20%",
      `pct_free/reduced` == "[0.2, 0.4[" ~ "20% - 40%",
      `pct_free/reduced` == "[0.4, 0.6[" ~ "40% - 60%",
      `pct_free/reduced` == "[0.6, 0.8[" ~ "60% - 80%",
      `pct_free/reduced` == "[0.8, 1[" ~ "80% - 100%"
    )
  )

final_merge <- final_merge %>%
  mutate (
    county_connections_ratio = case_when(
      county_connections_ratio == "[0.18, 1[" ~ "<1",
      county_connections_ratio == "[1, 2[" ~ ">1",
    )
  )

final_merge <- final_merge %>%
  mutate(
    pp_total_raw = case_when(
      pp_total_raw == "[10000, 12000[" ~ "10-12",
      pp_total_raw == "[12000, 14000[" ~ "12-14",
      pp_total_raw == "[14000, 16000[" ~ "14-16",
      pp_total_raw == "[16000, 18000[" ~ "16-18",
      pp_total_raw == "[18000, 20000[" ~ "18-20",
      pp_total_raw == "[20000, 22000[" ~ "20-22",
      pp_total_raw == "[22000, 24000[" ~ "22-24",
      pp_total_raw == "[32000, 34000[" ~ "32-34",
      pp_total_raw == "[4000, 6000[" ~ "4-6",
      pp_total_raw == "[6000, 8000[" ~ "6-8",
      pp_total_raw == "[8000, 10000[" ~ "8-10",
      TRUE ~ pp_total_raw
    )
  )

unique(final_merge$`Sector(s)`)
final_merge <- final_merge %>%
  mutate(`Sector(s)` = replace (`Sector(s)`, `Sector(s)`
                                == "PreK-122", "PreK-12"))
final_merge <- final_merge %>%
  mutate(`Sector(s)` = replace (`Sector(s)`, `Sector(s)`
                                == "PPreK-12", "PreK-12"))
final_merge <- final_merge %>%
  mutate(`Sector(s)` = replace (`Sector(s)`, `Sector(s)`
                                == "PreK-112", "PreK-12"))
final_merge <- final_merge %>%
  mutate(`Sector(s)` = replace (`Sector(s)`, `Sector(s)`
                                == "pre kindergarten to yr 12", "PreK-12"))
final_merge <- final_merge %>%
  mutate(`Sector(s)` = replace (`Sector(s)`, `Sector(s)`
                                == "pre kindergarten to year 12", "PreK-12"))


# format time:
final_merge <- final_merge %>%
  mutate(
    time = case_when(
      time == "1/01/2022" ~ "1/01/2020",
      time == "31/12/1020" ~ "31/12/2020",
      time == "1/1/2044" ~ "1/01/2020",
      time == "1/01/2050" ~ "1/01/2020",
      time == "1/01/2033" ~ "1/01/2020",
      time == "2/01/2050" ~ "2/01/2020",
      TRUE ~ time
    )
  )
write.csv(final_merge, "final_merge.csv", row.names = FALSE)

## VISUALIZATION:

# Most used products
# Load the necessary libraries
# Load ggplot2 library (if not already loaded)
library(ggplot2)

# Count the frequency of each product name
product_counts <- final_merge %>%
  group_by(`Product Name`) %>%  # Group by "Product.Name" column
  summarise(count = n()) %>% # Count occurrences in each group
  arrange(desc(count)) %>% # Order by count in descending order
  head(10)  # Select the top 10 rows (most used products)

# Create the bar chart
ggplot(product_counts, aes(y = count, x = `Product Name`)) +
  geom_bar(stat = "identity") +  # Create bars with height representing count
  geom_bar(col = "green4", stat = "identity", width = 0.85, alpha = 0.9) +
  labs(title = "Top 10 Most Used Products in final_merge",
       y = "Frequency",
       x = "Product Name") +  # Add labels and title
  coord_flip()  # Flip the axes to display product names on the y-axis

# Top provider:
top_providers <- final_merge %>% drop_na(`Provider/Company Name`)
#library(tidyverse)
top_providers <- top_providers %>%
  count(`Provider/Company Name`) %>%
  arrange(desc(n)) %>%
  top_n(15, n)

ggplot(top_providers, aes(x = reorder(`Provider/Company Name`, n), y = n, fill = `Provider/Company Name`)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_flip() +  # This flips the axis so the labels are more readable
  labs(x = "Provider/ Company Name", y = "Count", title = "Top 15 Providers") +
  theme(legend.position = "none")  # Remove legend since it's redundant




## MEAN of pct_access during month in 2020:
# Convert `time` to Date type if it's not already

final_merge$time <- as.Date(final_merge$time, format = "%d/%m/%Y")
# Extract month name
final_merge$month <- format(final_merge$time, "%b")
monthly_avg_access <- final_merge %>%
  group_by(month) %>%
  summarise(mean_pct_access = mean(pct_access, na.rm = TRUE)) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
  arrange(month)  # Arrange by month assuming you want it in calendar order

# DRAW LINE GRAPH:
library(ggplot2)
library(dplyr)

# Assuming monthly_avg_access is already calculated and contains the abbreviated month names in order
ggplot(monthly_avg_access, aes(x = month, y = mean_pct_access, group = 1)) +
  geom_rect(aes(xmin = which(levels(month) == "Jun"), xmax = which(levels(month) == "Aug"), ymin = -Inf, ymax = Inf), 
            fill = "#f0e6f7", alpha = 0.3, inherit.aes = FALSE) + # Pastel highlight for Summer Break, behind the line
  geom_line(color = "skyblue") + # Draw the line graph on top of the highlight
  geom_point(color = "yellow3") + # Add points for each data point on top of the highlight
  scale_x_discrete(limits = levels(month)) + # Ensure the x-axis is ordered correctly
  annotate("text", x = which(levels(month) == "Jun"), y = max(monthly_avg_access$mean_pct_access, na.rm = TRUE) * 0.9, label = "Summer Break", size = 10, fontface = "italic") +
  theme_minimal() +
  labs(x = "Month", y = "Mean pct_access", title = "Average Monthly pct_access with Summer Break Highlighted") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of month labels


## MEAN of engagement index during month in 2020:
monthly_avg_engagement <- final_merge %>%
  group_by(month) %>%
  summarise(mean_engagement_index = mean(engagement_index, na.rm = TRUE)) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
  arrange(month)  # Arrange by month assuming you want it in calendar order

library(ggplot2)
library(dplyr)

ggplot(monthly_avg_engagement, aes(x = month, y = mean_engagement_index, group = 1)) +
  geom_rect(aes(xmin = which(levels(month) == "Jun"), xmax = which(levels(month) == "Aug"), ymin = -Inf, ymax = Inf), 
            fill = "#f0e6f7", alpha = 0.3, inherit.aes = FALSE) + # Pastel highlight for Summer Break, behind the line
  geom_line(color = "skyblue") + # Draw the line graph on top of the highlight
  geom_point(color = "yellow3") + # Add points for each data point on top of the highlight
  scale_x_discrete(limits = levels(month)) + # Ensure the x-axis is ordered correctly
  annotate("text", x = which(levels(month) == "Jun"), y = max(monthly_avg_engagement$mean_engagement_index, na.rm = TRUE) * 0.9, label = "Summer Break", size = 4, fontface = "italic") +
  theme_minimal() +
  labs(x = "Month", y = "Mean Engagement Index", title = "Average Monthly Engagement Index with Summer Break Highlighted") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of month labels


## Mean engagement index for the state:
library(dplyr)

# Assuming your data frame is named final_merge
# Step 1: Calculate Mean Engagement Index by State
final_merge1 <- final_merge %>% drop_na(`state`)
## as the 1039 and 1131  the Nan value for the district_id, hence this analysis will ignore and drop NA for the state 

avg_engagement_by_state <- final_merge1 %>%
  group_by(state) %>%
  summarise(mean_engagement_index = mean(engagement_index, na.rm = TRUE)) %>%
  arrange(desc(mean_engagement_index)) # Arrange by descending order of mean engagement index

# Step 2: Plot the Bar Graph
library(ggplot2)

# Plotting the bar graph
ggplot(avg_engagement_by_state, aes(x = reorder(state, -mean_engagement_index), y = mean_engagement_index, fill = state)) +
  geom_bar(stat = "identity", show.legend = FALSE) + # Use identity to indicate heights of bars should represent values in the data
  theme_minimal() +
  labs(x = "State", y = "Average Engagement Index", title = "Average Engagement Index by State") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of state names



# Mean engagement index for state monthly
library(ggplot2)
library(dplyr)
library(lubridate)

# Ensure the 'month' column is ordered properly for plotting
#final_merge$month <- factor(final_merge$month, levels = unique(final_merge$month))

mean_month_state <- final_merge1 %>%
  mutate(month = format(as.Date(time), "%Y-%m")) %>%
  group_by(state, month) %>%
  summarise(average_engagement_index = mean(engagement_index, na.rm = TRUE), .groups = 'drop')

# Plotting
ggplot(mean_month_state, aes(x = month, y = average_engagement_index, group = state, color = state)) +
  geom_line() +  # Draw lines
  theme_minimal() +
  labs(x = "Month", y = "Average Engagement Index", title = "Monthly Average Engagement Index by State") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        legend.title = element_blank()) +  # Adjust text angle for readability
  guides(color = guide_legend(override.aes = list(alpha = 1))) # Ensure legend is readable


## Sectors for most used products:
top_products <- final_merge %>%
  count(`Product Name`) %>%
  arrange(desc(n)) %>%
  top_n(10, n)


# pct_free for each state:
ggplot(final_merge1, aes(x = state, fill = `pct_free/reduced`)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of pct_free/reduced Ranges by State",
       x = "State",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Paired", name = "pct_free/reduced Range")



## mean engagement index for the top provider in each state:
library(dplyr)
library(ggplot2)

top_providers <- final_merge1 %>%
  group_by(`Provider/Company Name`) %>%
  summarise(Count = n()) %>%
  top_n(10, Count) %>%
  pull(`Provider/Company Name`)

# Assuming the previous steps to identify top providers and calculate mean engagement index

state_provider_engagement <- final_merge1 %>%
  filter(`Provider/Company Name` %in% top_providers) %>%
  group_by(state, `Provider/Company Name`) %>%
  summarise(MeanEngagementIndex = mean(engagement_index, na.rm = TRUE)) %>%
  ungroup()

# Updated plot code to rotate the graph and use facet_wrap
ggplot(state_provider_engagement, aes(x = MeanEngagementIndex, y = reorder(`Provider/Company Name`, MeanEngagementIndex), fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Engagement Index by Provider for Top 10 Providers",
       x = "Mean Engagement Index",
       y = "Provider/ Company Name") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1), legend.position = "bottom") +
  facet_wrap(~state, scales = "free_y")  # Use facet_wrap to separate by state
