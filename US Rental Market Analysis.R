apartments <- read.csv("apartments_for_rent_classified_10K.csv", sep=";")

library(dplyr)
library(ggplot2)

data <- apartments %>%
  group_by(state, na.rm = TRUE) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    avg_square_feet = mean(square_feet, na.rm = TRUE)
  )

ggplot(data, aes(x = avg_square_feet, y = avg_price)) +
  geom_point(size = 2, color = "blue") +
  geom_text(aes(label = state), vjust = -0.8, hjust = 0.5, size = 3) +  
  labs(
    title = "Average Rent vs. Average Square Footage by State",
    x = "Average Square Footage",
    y = "Average Monthly Rent (in dollars)"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle=0))


# Washington DC regression model
dc_data <- apartments %>%
  filter(state == "DC" & !is.na(price) & !is.na(square_feet)) 

dc_model <- lm(price ~ square_feet, data = dc_data)

summary(dc_model)


# Montana regression model
mt_data <- apartments %>%
  filter(state == "MT" & !is.na(price) & !is.na(square_feet))

mt_model <- lm(price ~ square_feet, data = mt_data)

summary(mt_model)


xlimit <- c(0, 2000) 
ylimit <- c(0, 5000)

ggplot(mt_data, aes(x = square_feet, y = price)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(
    title = "Regression Line for Average Rent vs. Square Footage in Montana",
    x = "Square Feet",
    y = "Price"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0)) +
  lims(x = xlimit, y = ylimit)


ggplot(dc_data, aes(x = square_feet, y = price)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(
    title = "Regression Line for Average Rent vs. Square Footage in DC",
    x = "Square Feet",
    y = "Price"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0)) +
  lims(x = xlimit, y = ylimit)


# California regression model
ca_data <- apartments %>%
  filter(state == "CA" & !is.na(price) & !is.na(square_feet))

ca_model <- lm(price ~ square_feet, data = ca_data)

summary(ca_model)


# Alabama regression model
al_data <- apartments %>%
  filter(state == "AL" & !is.na(price) & !is.na(square_feet))

al_model <- lm(price ~ square_feet, data = al_data)

summary(al_model)