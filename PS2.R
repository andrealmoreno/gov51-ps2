---
title: "Problem Set 2: The LaCour-Green Study"
author: "Andrea Moreno"
date: "`r Sys.Date()`"
output: pdf_document
---
GitHub Repository:"https://github.com/andrealmoreno/gov51-ps2.git"
---
#Part 1: The origin
#Load data
library(tidyverse)
getwd() 
gay <- read_csv("data/raw/gay.csv")
gay_reshaped <- read_csv("data/raw/gayreshaped.csv")
ccap <- read_csv("data/raw/ccap2012.csv")
head(gay)

#Q1.1
# 1. How many total observations?
nrow(gay)
#ANSWER There are 69,592 total observations in gay.csv.

# 2. How many unique values does 'study' take?
# n_distinct() is a handy tidyverse function for this
n_distinct(gay$study)
# ANSWER: The 'study' variable takes 2 unique values.

# 3. List all treatment conditions
# unique() will show you every distinct label in that column
unique(gay$treatment)
# ANSWER: The treatment conditions are: 
# (1) No Contact 
# (2) Recycling Script by Gay Canvasser 
# (3) Same-Sex Marriage Script by Gay Canvasser 
# (4) Recycling Script by Straight Canvasser 
# (5) Same-Sex Marriage Script by Straight Canvasser

#Q1.2:

# 1. Filter to Study 1 and Wave 1 only
study1_wave1 <- gay %>%
  filter(study == 1, wave == 1)

# 2. Calculate the mean of ssm for each treatment group
q1_2_results <- study1_wave1 %>%
  group_by(treatment) %>%
  summarize(mean_ssm = mean(ssm, na.rm = TRUE))

# 3. View the results
print(q1_2_results)

# ANSWER: In Study 1, Wave 1, the "Recycling Script by Gay Canvasser" group 
# shows the highest average support with a mean ssm of 3.13. 
# However, all groups are very similar (between 3.01 and 3.13), 
# which suggests the randomization worked at the baseline.

#Q1.3

# Calculate the standard deviation of ssm for each treatment group
q1_3_results <- study1_wave1 %>%
  group_by(treatment) %>%
  summarize(sd_ssm = sd(ssm, na.rm = TRUE))

# View the results
print(q1_3_results)

#ANSWER
# The standard deviations are very similar across all treatment groups (ranging from 1.67 to 1.70). 
# This is important in a randomized experiment because it indicates that the groups are 
# "balanced" not just in their average support, but also in their diversity of opinion. 
# It suggests that the random assignment successfully distributed people with different 
# baseline attitudes evenly across all experimental conditions.

# Q1.4:
# create the plot
ggplot(q1_2_results, aes(x = treatment, y = mean_ssm, fill = treatment)) +
  geom_col() +
  coord_flip() + 
  labs(title = "Mean SSM Support by Treatment Group",
       subtitle = "Study 1, Wave 1 (Baseline)",
       x = "Treatment Condition",
       y = "Mean Support (1-5 scale)") +
  theme_minimal() +
  theme(legend.position = "none")

#1.2 Checking Balance and Comparing Outcomes
#Q1.5
balance_check <- study1_wave1 %>%
  filter(treatment %in% c("Same-Sex Marriage Script by Gay Canvasser", "No Contact")) %>%
  group_by(treatment) %>%
  summarize(
    mean_ssm = mean(ssm, na.rm = TRUE),
    sd_ssm = sd(ssm, na.rm = TRUE),
    var_ssm = var(ssm, na.rm = TRUE) # Variance is s^2
  )

stats <- balance_check$mean_ssm
vars <- balance_check$var_ssm

std_diff <- (stats[2] - stats[1]) / sqrt((vars[2] + vars[1]) / 2)
print(std_diff)
# Answer - The standardized difference in means is -0.01, which is significantly 
# lower than the 0.25 threshold. This indicates that the groups are 
# very well-balanced at baseline. This is important 
# because it ensures that any changes we see in later waves are 
# likely due to the treatment (the canvassing) rather than 
# pre-existing differences between the people in each group.






