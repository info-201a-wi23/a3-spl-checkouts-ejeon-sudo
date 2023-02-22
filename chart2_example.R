source("summary.R")

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
  filter(Title %in% c("Fly Guy vs. the flyswatter! / Tedd Arnold.", "Noodleheads lucky day / by Tedd Arnold, Martha Hamilton, and Mitch Weiss ; illustrated by Tedd Arnold.")) %>%
  group_by(date, Title) %>%
  summarize(total_checkouts = sum(Checkouts))

# Shorten the title of Noodlhead (it shrinks the graph to the left)
top_flyguy_and_noodleheads <- top_flyguy_and_noodleheads %>%
  mutate(across('Title', str_replace, 'Noodleheads lucky day / by Tedd Arnold, Martha Hamilton, and Mitch Weiss ; illustrated by Tedd Arnold.', 'Noodleheads lucky day / by Tedd Arnold and more'))

# Plot a chart that shows the checkout history for them
flyguy_vs_noodleheads <- ggplot(top_flyguy_and_noodleheads) + 
  geom_line(aes(
    x = date, 
    y = total_checkouts, 
    color = Title)) + 
  scale_color_brewer(palette = "Set2") +
  labs(title = "Fly Guy Consistently Flies Overhead The Noodleheads", 
       x = "Year", 
       y = "Number of Checkouts",
       color = "Todd Arnold's Two Book Series") +
  scale_y_continuous(breaks = seq(0, 50, 5))
