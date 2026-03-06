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

#Q1.6

study1_wave2 <- gay %>%
  filter(study == 1, wave == 2)
#calculate the mean ssm for the two groups
q1_6_results <- study1_wave2 %>%
  filter(treatment %in% c("Same-Sex Marriage Script by Gay Canvasser", "No Contact")) %>%
  group_by(treatment) %>%
  summarize(mean_ssm = mean(ssm, na.rm = TRUE))
print(q1_6_results)
diff_means_w2 <- q1_6_results$mean_ssm[2] - q1_6_results$mean_ssm[1]
print(diff_means_w2)

#Answer
# The difference in means at Wave 2 is approximately 0.10. 
# Since we established in Q1.5 that these groups were balanced at baseline, 
# this difference suggests that the 'same-Sex marriage Script' delivered 
# by a gay canvasser successfully increased support for same-sex marriage. 
# Because the study used a randomized design, we can attribute this increase 
# to the treatment effect of the conversation rather than pre-existing differences.

# Part 2 The Forensic Discovery
#Q.2.1
# observations and column names for gayreshaped.csv
nrow(gay_reshaped)
colnames(gay_reshaped)
# observations and column names for ccap2012.csv
nrow(ccap)
colnames(ccap)
# Answer
# gayreshaped.csv has 11,948 observations. 
# Its columns are: "study", "treatment", "therm1", "therm2", "therm3", "therm4".
# ccap2012.csv has 43,998 observations. 
# Its columns are: "...1" (index), "caseid", and "gaytherm".

#Q2.2
#Filter Study 1 from gayreshaped and calculate stats for therm1
gay_s1_stats <- gay_reshaped %>%
  filter(study == 1) %>%
  summarize(
    mean = mean(therm1, na.rm = TRUE),
    median = median(therm1, na.rm = TRUE),
    sd = sd(therm1, na.rm = TRUE)
  )

#calculate stats for gaytherm 
ccap_stats <- ccap %>%
  summarize(
    mean = mean(gaytherm, na.rm = TRUE),
    median = median(gaytherm, na.rm = TRUE),
    sd = sd(gaytherm, na.rm = TRUE)
  )

#side by side
print("LaCour Study 1 (therm1) Statistics:")
print(gay_s1_stats)
print("CCAP 2012 (gaytherm) Statistics:")
print(ccap_stats)

# Answer - what i notice
# I notice that the mean (58.4 vs 58.7), median (52 vs 54), and standard 
# deviation (28.5 vs 29.4) are almost identical between the LaCour Study 1 
# and the CCAP 2012 survey. This is highly suspicious for a randomized 
# experiment, as it suggests the experimental data was not collected 
# independently but was likely "sampled" or copied from the existing 
# CCAP distribution to appear realistic.

#Q2.3
#create a combined data frame for plotting
lacour_plot <- gay_reshaped %>%
  filter(study == 1) %>%
  select(therm = therm1) %>%
  mutate(source = "LaCour Study 1 (therm1)")
ccap_plot <- ccap %>%
  select(therm = gaytherm) %>%
  mutate(source = "CCAP 2012 (gaytherm)")
combined_data <- bind_rows(lacour_plot, ccap_plot)
#side-by-side histograms
ggplot(combined_data, aes(x = therm)) +
  geom_histogram(breaks = seq(0, 100, by = 5), 
                 fill = "steelblue", 
                 color = "white") +
  facet_wrap(~source, scales = "free_y") +
  labs(title = "Distribution of Baseline Feeling Thermometers",
       x = "Feeling Thermometer Score (0-100)",
       y = "Frequency") +
  theme_minimal()
#Answer -
#These distributions do not look like they came from different populations. 
#Despite being collected at different times from different groups, the 
#histograms are nearly identical in shape and frequency of specific values. 
# In a real-world experiment, we would expect more variation this  
# level of similarity suggests the LaCour data was likely fabricated by 
# copying the distribution of the existing CCAP survey.



