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