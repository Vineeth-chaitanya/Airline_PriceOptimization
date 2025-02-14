library(ggplot2)
flight_data <- read.csv('Flight_data.csv', stringsAsFactors = FALSE)

str(flight_data)
summary(flight_data)
head(flight_data, 10)

colSums(is.na(flight_data))

same_airports <- which(flight_data$Origin == flight_data$Destination)
length(same_airports)

library(ggplot2)

ggplot(flight_data, aes(x = Origin)) +
  geom_bar(fill = 'blue') +
  theme_minimal() +
  labs(title = 'Number of flights from each Origin Airport')

# creating Day_of_week
flight_data <- flight_data %>%
  mutate(
    Day_of_week = wday(Departure_Date, label= TRUE)
  )
flight_data <- flight_data %>%
  mutate(
    Departure_Date = as.Date(Departure_Date, format="%Y-%m-%d"),
    Day_of_week = wday(Departure_Date, label = TRUE, abbr = FALSE)
  )
head(flight_data$Day_of_week)
colnames(flight_data)
print(flight_dat)

# Average price per day of the week
flight_data %>%
  group_by(Day_of_week) %>%
  summarize(Average_Price = mean(Price, na.rm = TRUE)) %>%
  ggplot(aes(x = Day_of_week, y= Average_Price)) +
  geom_bar(stat = "identity", fill='steelblue') +
  theme_minimal()+
  labs(
    title = "Average Flight price by day of the week",
    x = "Day of the week",
    y = "Average Price"
  )
  