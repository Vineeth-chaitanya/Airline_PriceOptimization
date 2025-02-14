install.packages("dplyr")
install.packages("lubridate")

library(dplyr)
library(lubridate)

set.seed(123)

#define possible values
airlines <- c("American Airlines","Delta","United","Southwest","JetBlue","Alaska Airlines","Spirit Airlines","Frontier Airlines","SkyWest Airlines","Endeavor Air","Republic Airways","Hawaiian Airlines")
airports <- c("JFK","LAX","ORD","ATL","DFW","MIA","SEA","SFO","DEN","BOS", "MCO","LAS","CLT","EWR","PHX","IAH","DTW","BWI","DCA","MDW") #Major US airports
seat_class <- c("Economy","Business","First Class")

# Generating 10,000 flight records
num_flights <- 10000
flight_data <- data.frame(
  Flight_ID = paste0("FL", sprintf('%05d', 1:num_flights)),
  Airline = sample(airlines, num_flights, replace = TRUE),
  Origin = sample(airports, num_flights, replace = TRUE),
  Destination = sample(airports, num_flights, replace = TRUE),
  Departure_Date = sample(seq(as.Date("2024-03-01"), as.Date("2025-02-12"), by="day"), num_flights, replace = TRUE),
  Seat_Class = sample(seat_class, num_flights, replace= TRUE),
  Days_to_Departure = sample(1:180, num_flights, replace= TRUE),
  Layovers = sample(0:2, num_flights, replace= TRUE, prob = c(0.4, 0.4, 0.2)), # 40% direct and one layover, 20% two layover
  Layover_Airport1 = NA,
  Layover_Airport2 = NA,
  Price = 0 # Placeholder for price calculation
  
)

same_indices <- which(flight_data$Origin == flight_data$Destination)
if (length(same_indices) > 0){
  flight_data$Destination[same_indices] <- sapply(same_indices, function(i) {
    # Sample a random airport that is different from the Origin for the current row
    sample(airports[airports != flight_data$Origin[i]], 1)
  })
}

# Assign layovers while ensuring uniqueness
flight_data <- flight_data %>%
  rowwise() %>%
  mutate(
    Layover_Airport1 = ifelse(Layovers >= 1, 
                              sample(airports[!airports %in% c(Origin, Destination)], 1), NA),
    Layover_Airport2 = ifelse(Layovers == 2, 
                              sample(airports[!airports %in% c(Origin, Destination, Layover_Airport1)], 1), NA)
  ) %>%
  ungroup()

# Ensuring all airports are distinct
repeat {
  common_indices <- which(
    flight_data$Origin == flight_data$Destination |
      flight_data$Origin == flight_data$Layover_Airport1 |
      flight_data$Origin == flight_data$Layover_Airport2 |
      flight_data$Destination == flight_data$Layover_Airport1 |
      flight_data$Destination == flight_data$Layover_Airport2 |
      flight_data$Layover_Airport1 == flight_data$Layover_Airport2
  )
  
  if (length(common_indices) == 0) break  # Stop if no duplicates
  
  # Reassign Layover_Airport1 and Layover_Airport2 where conflicts exist
  flight_data[common_indices, "Layover_Airport1"] <- sapply(common_indices, function(i) {
    sample(airports[!airports %in% c(flight_data$Origin[i], flight_data$Destination[i])], 1)
  })
  
  flight_data[common_indices, "Layover_Airport2"] <- sapply(common_indices, function(i) {
    sample(airports[!airports %in% c(flight_data$Origin[i], flight_data$Destination[i], flight_data$Layover_Airport1[i])], 1)
  })
}

# Adjust price based on various factors
flight_data <- flight_data %>%
  mutate(
    Base_Price = 50 + runif(num_flights, 50, 500),
    Weekend_Surcharge = ifelse(weekdays(Departure_Date) %in% c("Saturday", "Sunday"), runif(num_flights, 20, 80), 0),
    Holiday_Surcharge = ifelse(month(Departure_Date) %in% c(1, 10, 12), runif(num_flights, 50, 150), 0), # Higher in Jan, Oct, Dec
    Class_Surcharge = case_when(
      Seat_Class == "Economy" ~ 0,
      Seat_Class == "Business" ~ runif(num_flights, 100, 300),
      Seat_Class == "First Class" ~ runif(num_flights, 300, 800)
    ),
    Layover_Surcharge = ifelse(Layovers == 1, runif(num_flights, 50, 150), ifelse(Layovers == 2, runif(num_flights, 100, 300), 0)),
    Demand_Factor = 1 + (180 - Days_to_Departure) / 200,
    Final_Price = round((Base_Price + Weekend_Surcharge + Holiday_Surcharge + Class_Surcharge + Layover_Surcharge) * Demand_Factor, 2),
    Price = Final_Price
  ) %>%
  select(-Base_Price, -Weekend_Surcharge, -Holiday_Surcharge, -Class_Surcharge, -Layover_Surcharge, -Demand_Factor, -Final_Price)

# Show the data
head(flight_data)

write.csv(flight_data, "Flight_data.csv", row.names = FALSE)