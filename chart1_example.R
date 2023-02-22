source("summary.R")

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


# Plot a chart that shows this
five_flyguys_checkouts <- ggplot(my_popular_books) + geom_line(aes(
  x = CheckoutYear, 
  y = total_checkouts, 
  color = Title)) + 
  scale_color_brewer(palette = "Set2") +
  labs(title = "Fly Guy Series' Collective Doom Entering The 2020's", 
       x = "Year", 
       y = "Number of Checkouts",
       color = "Top 5 Most Popular Fly Guy Books") +
  scale_y_continuous(breaks = seq(0, 260, 20))
