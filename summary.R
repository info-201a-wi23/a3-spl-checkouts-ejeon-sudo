# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

# ---------------------------------------------------------------------
# # 1. Loading and Cleaning data 

# Load the data
spl_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

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

# ---------------------------------------------------------------------
# # Chart One: Compare The Five Most Popular Fly Guys throughout the Years

# Find the top five most popular Fly Guy Books
most_popular_flyguys <- flyguy_df %>%
  group_by(Title) %>%
  summarize(total_checkouts = sum(Checkouts)) %>%
  arrange(-total_checkouts) %>%
  slice_head(n = 5) %>%
  pull(Title)

# Get the five books' in one data frame
my_popular_books <- flyguy_df %>%
  filter(Title == most_popular_flyguys) %>%
  select(CheckoutYear, Title, Checkouts) %>%
  group_by(CheckoutYear, Title) %>% 
  summarize(total_checkouts = sum(Checkouts))

# Plot a chart that shows the checkout history for them
ggplot(my_popular_books) + geom_line(aes(
  x = CheckoutYear, 
  y = total_checkouts, 
  color = Title)) + 
  scale_color_brewer(palette = "Set2") +
  labs(title = "Fly Guy Series' Collective Doom", 
       x = "Year", 
       y = "Number of Checkouts",
       color = "Top 5 Most Popular Fly Guy Books") +
  scale_y_continuous(breaks = seq(0, 240, 20), labels = label_number_si())

# --------------------------------------------------------
# # Chart Two: Compare The 2022 Checkouts for Tedd Arnold's Book Series Fly Guy and Noodlehead

# Get a data frame of Tedd Arnold's Fly Guy and Noodlehead series
arnolds_books <- author_df %>%
  filter(CheckoutYear == "2022") %>%
  filter(str_detect(Title, "(?i)Fly Guy|(?i)Noodlehead")) %>%
  group_by(Title, CheckoutYear) %>%
  summarize(total_checkouts = sum(Checkouts)) %>%
  ungroup()

# Get Fly Guy's most checked out book
top_flyguy <- arnolds_books %>%
  filter(str_detect(Title, "(?i)Fly Guy")) %>%
  filter(total_checkouts == max(total_checkouts)) %>%
  pull(Title)
top_flyguy

# Get Noodlehead's most checked out book
top_noodleheads <- arnolds_books %>%
  filter(str_detect(Title, "(?i)Noodlehead")) %>%
  filter(total_checkouts == max(total_checkouts)) %>%
  pull(Title)
top_noodleheads

# Create a dataframe that combines the total_checkouts for both Fly Guy and Noodle's most popular book
top_flyguy_and_noodleheads <- author_df %>%
  filter(CheckoutYear == "2022") %>%
  filter(Title == "Fly Guy vs. the flyswatter! / Tedd Arnold." | Title == "Noodleheads lucky day / by Tedd Arnold, Martha Hamilton, and Mitch Weiss ; illustrated by Tedd Arnold.") %>%
  group_by(date, Title) %>%
  summarize(total_checkouts = sum(Checkouts))

# Plot a chart that shows the checkout history for them
ggplot(top_flyguy_and_noodleheads) + 
  geom_line(aes(
    x = date, 
    y = total_checkouts, 
    color = Title)) + 
  scale_color_brewer(palette = "Set2") +
  labs(title = "Fly Guy Continuously Flies Overhead Noodleheads", 
       x = "Year", 
       y = "Number of Checkouts",
       color = "Todd Arnold's Most Popular Books From His Two Book Series") +
  scale_y_continuous(breaks = seq(0, 50, 2), labels = label_number_si())

# ---------------------------------------------------------------------
# # Chart Three: Find the Total Fly Guy Checkouts Over the Years

# What was the total checkouts each year?
total_year_checkouts <- flyguy_df %>%
  group_by(CheckoutYear) %>%
  summarize(total_checkouts = sum(Checkouts))

# Plot a chart that shows the checkout history for Fly Guy
ggplot(total_year_checkouts) + 
  geom_col(aes(x = CheckoutYear,
               y = total_checkouts,
               fill = "Fly Guy Series")) + 
  labs(title = "Fly Guy Series Checkouts Each Year",
       x = "Year",
       y = "Total Checkouts",
       fill = "Total Fly Guy Series Checkouts") +
  scale_x_continuous(breaks = seq(2017, 2023)) +
  scale_y_continuous(breaks = seq(0, 13000, 2000), labels = label_number_si())

# --------------------------------------------------------
# # POTENTIAL Chart Four: Compare the checkouts of the first Fly Guy book to the newest

# Created data frame 'flyguy_books_publication' that joins each Book and their PublicationYear and Checkouts together

flyguy_books_publication <- flyguy_df %>%
  group_by(Title, PublicationYear) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% ungroup()

# Find the oldest Fly Guy book
oldest_flyguy <- flyguy_books_publication %>%
  filter(PublicationYear == min(PublicationYear)) %>%
  pull(Title)

# Find the newest Fly Guy book
newest_flyguy <- flyguy_books_publication %>%
  filter(PublicationYear == max(PublicationYear)) %>%
  slice_head(n = 1) %>%
  pull(Title)

# Find the checkouts each year with only the oldest and newest flyguy
my_books <- flyguy_df %>% 
  filter(Title == oldest_flyguy | Title == newest_flyguy) %>%
  group_by(date, Title) %>%
  summarize(total_checkouts = sum(Checkouts))

ggplot(my_books) + 
  geom_line(aes(
    x = date, 
    y = total_checkouts, 
    color = Title)) + 
  scale_color_brewer(palette = "Set2") +
  labs(title = "Newest Fly Guy Book Checkouts vs. Oldest", 
       x = "Year", 
       y = "Number of Checkouts",
       color = "Fly Guy Book Title")

