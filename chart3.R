source("summary.R")

# # Chart Three: Find the Total Fly Guy Checkouts Over the Years

# What was the total checkouts each year?
total_year_checkouts <- flyguy_df %>%
  group_by(CheckoutYear) %>%
  summarize(total_checkouts = sum(Checkouts))

# Plot a chart that shows the checkout history for Fly Guy
checkout_history <- ggplot(total_year_checkouts) + 
  geom_col(aes(x = CheckoutYear,
               y = total_checkouts,
               fill = "Every Fly Guy book")) + 
  scale_fill_manual(values=c("darkblue")) +
  labs(title = "Total Fly Guy Checkouts Over the Years",
       x = "Year",
       y = "Total Checkouts",
       fill = "The Fly Guy Series") +
  scale_x_continuous(breaks = seq(2017, 2023)) +
  scale_y_continuous(breaks = seq(0, 13000, 2000), labels = label_number_si())
