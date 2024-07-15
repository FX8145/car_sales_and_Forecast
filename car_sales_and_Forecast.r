library(forecast)
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)

set.seed(123)  
dates <- seq(as.Date("2023-01-01"), by = "month", length.out = 6)
car_brands <- c("Toyota", "Benz", "Audi", "BMW", "Suzuki", "Mazda")
sales_data <- expand.grid(date = dates, car_brand = car_brands) %>%
  mutate(sales = round(rnorm(n(), mean = 200, sd = 50)))  # Simulated sales data

# Function to forecast sales for a given brand
forecast_sales_by_brand <- function(brand_sales) {
  ts_data <- ts(brand_sales$sales, frequency = 12)
  model <- auto.arima(ts_data)
  forecasted_values <- forecast(model, h = 6)$mean
  return(forecasted_values)
}

# Generating forecast for each brand
sales_forecasts <- sales_data %>% 
  group_by(car_brand) %>%
  summarise(forecasted_sales = list(forecast_sales_by_brand(.))) %>%
  unnest(forecasted_sales)

# Add future dates to the forecasts
future_dates <- seq(as.Date("2023-07-01"), by = "month", length.out = 6)
sales_forecasts$date <- rep(future_dates, times = length(unique(sales_forecasts$car_brand)))

# Visualization
combined_sales <- bind_rows(
  sales_data %>% mutate(Type = "Historical"),
  sales_forecasts %>% rename(sales = forecasted_sales) %>% mutate(Type = "Forecasted")
)

# Bar Plot for Sales Data
ggplot(combined_sales, aes(x = date, y = sales, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ car_brand, scales = "free_y") +
  scale_fill_manual(values = c("Historical" = "steelblue", "Forecasted" = "firebrick")) +
  labs(title = "Car Sales and Forecast by Brand", x = "Date", y = "Sales") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Box Plot for Sales Distribution by Brand
ggplot(sales_data, aes(x = car_brand, y = sales, fill = car_brand)) +
  geom_boxplot() +
  labs(title = "Distribution of Sales by Car Brand", x = "Car Brand", y = "Sales") +
  theme_minimal()

# Histogram for Overall Sales Distribution
ggplot(sales_data, aes(x = sales)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Sales", x = "Sales", y = "Frequency") +
  theme_minimal()
