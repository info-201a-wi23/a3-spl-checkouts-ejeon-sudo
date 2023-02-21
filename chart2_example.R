source("summary.R")

# # Chart Two: Compare The 2022 Checkouts for Tedd Arnold's Book Series Fly Guy and Noodlehead

# Create a dataframe that combines the total_checkouts for both Fly Guy and Noodle's most popular book
top_flyguy_and_noodleheads <- author_df %>%
  filter(CheckoutYear == "2022") %>%
  filter(Title == "Fly Guy vs. the flyswatter! / Tedd Arnold." | Title == "Noodleheads lucky day / by Tedd Arnold, Martha Hamilton, and Mitch Weiss ; illustrated by Tedd Arnold.") %>%
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
  scale_y_continuous(breaks = seq(0, 50, 5), labels = label_number_si())
