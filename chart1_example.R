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
my_popular_books <- each_flyguy_yearly_checkout %>%
  filter(Title %in% (most_popular_flyguys)) 

# Plot a chart that shows this
five_flyguys_checkouts <- ggplot(my_popular_books) + geom_line(aes(
  x = CheckoutYear, 
  y = each_total_checkout, 
  color = Title)) + 
  scale_color_brewer(palette = "Set2") +
  labs(title = "Fly Guy Series' Collective Doom Entering The 2020's", 
       x = "Year", 
       y = "Number of Checkouts",
       color = "Top 5 Most Popular Fly Guy Books") +
  scale_y_continuous(breaks = seq(0, 840, 100)) +
  scale_x_continuous(breaks = seq(2017, 2023))
