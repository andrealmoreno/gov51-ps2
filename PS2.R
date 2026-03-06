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

#Q2.4
#no Contact groups in both studies
s1_control <- gay_reshaped %>% filter(study == 1, treatment == "No Contact")
s2_control <- gay_reshaped %>% filter(study == 2, treatment == "No Contact")

#clculate Correlations and SD of Change for Study 1
cor_s1 <- cor(s1_control$therm1, s1_control$therm2, use = "complete.obs")
sd_change_s1 <- sd(s1_control$therm2 - s1_control$therm1, na.rm = TRUE)

#correlations and SD of Change for Study 2
cor_s2 <- cor(s2_control$therm1, s2_control$therm2, use = "complete.obs")
sd_change_s2 <- sd(s2_control$therm2 - s2_control$therm1, na.rm = TRUE)

# side by side
results_2_4 <- tibble(
  Study = c("Study 1", "Study 2"),
  Correlation = c(cor_s1, cor_s2),
  SD_of_Change = c(sd_change_s1, sd_change_s2)
)
print(results_2_4)
#Answer
# Study 2 shows a more plausible pattern of re-test reliability with a correlation 
# of 0.97 and an SD of change of 6.61.Study 1 shows a near-perfect 
# correlation of 0.998 and a tiny SD of change (1.99). This suggests that the 
# Wave 2 data in Study 1 was likely generated by taking Wave 1 and adding a very 
# small amount of random noise, rather than actually re-surveying real people 
# whose opinions naturally fluctuate more than that.

#Q2.5
#combine the control groups 
combined_controls <- bind_rows(
  s1_control %>% mutate(label = "Study 1 (Control)"),
  s2_control %>% mutate(label = "Study 2 (Control)")
)

#side-by-side scatter plots
ggplot(combined_controls, aes(x = therm1, y = therm2)) +
  geom_point(alpha = 0.5) +
  # Add a 45-degree line (y = x)
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  facet_wrap(~label) +
  labs(title = "Wave 1 vs Wave 2 Feeling Thermometers",
       subtitle = "Red dashed line indicates no change (y = x)",
       x = "Wave 1 Score",
       y = "Wave 2 Score") +
  theme_minimal()

# Answer
#Study 2 looks much more like real data because the points form a 
#distinct cloud around the 45-degree line, representing natural human 
#inconsistency and minor shifts in opinion. Study 1 shows 
#almost zero deviation from the 45-degree line. This lack of any noise or 
#measurement error suggests the data was mathematically generated rather than 
#collected from real people, as human respondents would never be this 
#perfectly consistent over multiple weeks.

#2.3 
# Q2.6 
#The combination of these three irregularities provides evidence of 
#data fabrication. First, the baseline "therm1" statistics (Mean: 58.4, SD: 28.5) 
#almost perfectly match the independent CCAP 2012 survey (Mean: 58.7, SD: 29.4), 
#which is statistically impossible for a new, independent sample. Second, the 
#re-test correlation of 0.998 in Study 1 is far above the plausible human 
#reliability range of 0.95–0.97. Third, the tiny SD of within-person change (1.99) 
#shows a lack of natural human "noise" or rounding variation over time. 
#A fabricator likely produced data with these properties because they 
# wanted the results to look clean and authoritative by copying a 
# real-world distribution (CCAP) and adding a minute amount of random 
# noise, they created an illusion of high-quality data that could pass 
# a surface-level check but failed under forensic scrutiny.

#Part 3
#Q3.1

study2_data <- gay_reshaped %>%
  filter(study == 2)
nrow(study2_data)

study2_baseline_stats <- study2_data %>%
  group_by(treatment) %>%
  summarize(
    mean_therm1 = mean(therm1, na.rm = TRUE),
    sd_therm1 = sd(therm1, na.rm = TRUE),
    n = n()
  )

print(study2_baseline_stats)
#answer
#study 2 contains 2,441 observations. The baseline means for the 
#"No Contact" group (57.9) and the "Gay Canvasser" group (59.4) 
# are relatively close, suggesting that randomization successfully 
# balanced the groups, though they are not suspiciously identical.

#Q3.2
s1_plot_data <- gay_reshaped %>%
  filter(study == 1) %>%
  select(therm = therm1) %>%
  mutate(source = "Study 1 (LaCour)")

s2_plot_data <- gay_reshaped %>%
  filter(study == 2) %>%
  select(therm = therm1) %>%
  mutate(source = "Study 2 (Replication)")

combined_studies <- bind_rows(s1_plot_data, s2_plot_data)
#histogramss
ggplot(combined_studies, aes(x = therm)) +
  geom_histogram(breaks = seq(0, 100, by = 5), 
                 fill = "darkgreen", 
                 color = "white") +
  facet_wrap(~source, scales = "free_y") +
  labs(title = "Baseline Distribution Comparison: Study 1 vs Study 2",
       x = "Feeling Thermometer Score (0-100)",
       y = "Frequency") +
  theme_minimal()

#answer 
#Visually both Study 1 and 2 show "heaping" (spikes) at the 
# values of 0, 50, and 100, which is typical for feeling thermometers 
#where respondents round to neutral or extreme values. 
#However, study 1 is a suspiciously perfect match to the national 
# CCAP distribution. Study 2, while similar in its 
# overall range, shows more natural "noise" and variation in the 
# heights of these heaps, suggesting it is an independent sample 
# rather than a copied distribution.

#3.2
# Q3.3
s2_balance_data <- gay_reshaped %>%
  filter(study == 2, 
         treatment %in% c("No Contact", "Same-Sex Marriage Script by Gay Canvasser"))
#Mean, SD, and Variance for each group
s2_balance_stats <- s2_balance_data %>%
  group_by(treatment) %>%
  summarize(
    mean = mean(therm1, na.rm = TRUE),
    sd = sd(therm1, na.rm = TRUE),
    var = var(therm1, na.rm = TRUE)
  )

mean_tr_s2 <- s2_balance_stats$mean[2]
mean_co_s2 <- s2_balance_stats$mean[1]
var_tr_s2  <- s2_balance_stats$var[2]
var_co_s2  <- s2_balance_stats$var[1]

std_diff_s2 <- (mean_tr_s2 - mean_co_s2) / sqrt((var_tr_s2 + var_co_s2) / 2)


print(s2_balance_stats)
cat("Standardized Difference in Means (Study 2):", std_diff_s2, "\n")
#answer
#The standardized difference in means for Study 2 is approximately 0.05, 
#which suggests that the treatment and control groups are well-balanced 
#at baseline. Unlike the too-perfect balance often seen in fabricated 
#data, this small but non-zero difference is exactly what we expect 
#from a legitimate randomization process in a real-world experiment.

#3.4
#mean of therm2 for each group
s2_post_stats <- s2_balance_data %>%
  group_by(treatment) %>%
  summarize(
    mean_therm2 = mean(therm2, na.rm = TRUE),
    n = n()
  )

#means to calculate the difference
mean_tr_post <- s2_post_stats$mean_therm2[2]
mean_co_post <- s2_post_stats$mean_therm2[1]
#ATE
ate_s2 <- mean_tr_post - mean_co_post
print(s2_post_stats)
cat("Difference in post-period means (Study 2):", ate_s2, "\n")
# answer
#In Study 2, the difference in post-treatment means is 3.57 points. 
#While this indicates a positive effect from the gay canvasser script, 
# it is notably smaller than the effect size reported in Study 1. This 
# suggests that the original study inflated the impact of 
# the intervention, which is common when data is fabricated to produce 
# more exciting or headline worthy results.

#Q3.5
#variance and n for each group 
s2_uncertainty_stats <- s2_balance_data %>%
  group_by(treatment) %>%
  summarize(
    var_therm2 = var(therm2, na.rm = TRUE),
    n = n()
  )

#t values for the SE formula
var_tr <- s2_uncertainty_stats$var_therm2[2]
var_co <- s2_uncertainty_stats$var_therm2[1]
n_tr   <- s2_uncertainty_stats$n[2]
n_co   <- s2_uncertainty_stats$n[1]
# SE
se_diff <- sqrt((var_tr / n_tr) + (var_co / n_co))
lower_ci <- ate_s2 - (1.96 * se_diff)
upper_ci <- ate_s2 + (1.96 * se_diff)

cat("Standard Error:", se_diff, "\n")
cat("95% Confidence Interval: [", lower_ci, ",", upper_ci, "]\n")

# answer
#The 95% confidence interval for the difference in post-treatment means 
#is [1.29, 5.84]. Since this interval does not include zero, we can 
#conclude that the difference is statistically significant at the 5% level 
#This confirms that while the "real" effect in Study 2 (3.57) 
#is much smaller than the fabricated 10-point effect in study 1, the 
#gay canvasser intervention still produced a measurable and statistically 
# reliable increase in support for same-sex marriage.

