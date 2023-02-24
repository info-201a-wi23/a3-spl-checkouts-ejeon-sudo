# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

# ---------------------------------------------------------------------
# # 1. Loading and Cleaning data 

# Load the data
spl_df <- read.csv("../2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Created a new date column ("date") with the month AND year and a default first day of the month, then converted that column to a date value
spl_df <- spl_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" )) 
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

# Filtered spl_df dataframe for only author Tedd Arnold
author_df <- spl_df %>% 
  filter(str_detect(Creator, "Tedd")) %>% 
  filter(str_detect(Creator, "Arnold"))

# Filtered author_df dataframe to have Fly Guy in the title
flyguy_df <- author_df %>% 
  filter(str_detect(Title, "(?i)Fly Guy")) %>%
  filter(MaterialType == "BOOK")

# Removed unneccessary brackets and letters in 'flyguy_df' PublicationYear column
flyguy_df <- flyguy_df %>%
  mutate(across('PublicationYear', str_replace, '\\[', '')) %>%
  mutate(across('PublicationYear', str_replace, '\\]', '')) %>%
  mutate(across('PublicationYear', str_replace, '\\c', '')) %>%
  mutate(across('PublicationYear', str_replace, '\\.', ''))

# --------------------------------------------------------
# # 2. DATA ANALYSIS: Relevant Values of Interest

# Find the number of columns and rows in 'flyguy_df' dataframe
obs_flyguy <- ncol(flyguy_df)
feat_flyguy <- nrow(flyguy_df)

# Find the total checkouts for each book in each year
each_flyguy_yearly_checkout <- flyguy_df %>%
  group_by(Title, CheckoutYear) %>% 
  summarize(each_total_checkout = sum(Checkouts))

# For each book, how many total times were they checked out?
each_flyguy_total_checkout <- each_flyguy_yearly_checkout %>%
  group_by(Title) %>%
  summarize(total_checkout = sum(each_total_checkout))

# For each year, which Fly Guy book was checked out the most?
most_popular_flyguy_each_year <- flyguy_df %>% 
  group_by(CheckoutYear) %>% 
  filter(Checkouts == max(Checkouts))

# Which Fly Guy book was checked out the most of all time?
most_popular_flyguy <- flyguy_df %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts, na.rm = TRUE)) %>% 
  pull(Title)

most_popular_flyguy_checkouts <- flyguy_df %>%
  filter(Title == most_popular_flyguy) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts, na.rm = TRUE)) %>%
  pull(total_checkouts)

# Which Fly Guy book was checked out the least of all time?
least_popular_flyguy <- flyguy_df %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == min(total_checkouts, na.rm = TRUE)) %>% 
  pull(Title)

# Find the total checkouts for Fly Guy in each year
total_year_checkouts <- flyguy_df %>%
  group_by(CheckoutYear) %>%
  summarize(total_checkouts = sum(Checkouts))

# How many times has Fly Guy been checked out the most in a year?
highest_checkout <- total_year_checkouts %>%
  filter(total_checkouts == max(total_checkouts, na.rm = TRUE)) %>% 
  pull(total_checkouts)

# What year did Fly Guy have the most checkouts?
highest_checkout_year <- total_year_checkouts %>%
  filter(total_checkouts == max(total_checkouts, na.rm = TRUE)) %>% 
  pull(CheckoutYear)

# How many times has Fly Guy been checked out the least in a year?
lowest_checkouts <- total_year_checkouts %>%
  filter(total_checkouts == min(total_checkouts, na.rm = TRUE)) %>% 
  pull(total_checkouts)

# What year did Fly Guy have the least checkouts?
lowest_checkout_year <- total_year_checkouts %>% 
  filter(total_checkouts == min(total_checkouts, na.rm = TRUE)) %>% 
  pull(CheckoutYear)

# How many checkouts did Fly Guy get in the most recent full year?
recent_total_checkout <- total_year_checkouts %>% 
  filter(CheckoutYear == "2022") %>% 
  pull(total_checkouts)

# How many checkouts did Fly Guy get in the oldest year?
oldest_total_checkout <- total_year_checkouts %>% 
  filter(CheckoutYear == min(CheckoutYear)) %>% 
  pull(total_checkouts)

# How has the number of print book checkouts changed over time
checkout_difference <- oldest_total_checkout - recent_total_checkout
