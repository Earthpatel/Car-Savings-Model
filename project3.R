# Earth Patel
# 9/22/2024
# New Car Affordability 

# initialize time
start_time <- 0
stop_time <- 20 
  delta_t <- 1/12 #1 month in years
  x <- seq(start_time, stop_time, delta_t)

# model parameters 
yearly_inflation_rate <- 0.05
yearly_interest_rate <- 0.04
yearly_depreciation_rate <- 0.08
monthly_payment <- 600 
new_car_initial_price <- 30000
used_car_initial_value <- 13000
savings_account_initial <- 6000

inflation_ratePerStep <- yearly_inflation_rate * delta_t
depreciation_ratePerStep <- yearly_depreciation_rate * delta_t
interest_ratePerStep <- yearly_interest_rate * delta_t 

# stock variables
oldCar <- vector(mode = "numeric", length = length(x))
oldCar[1] <- used_car_initial_value

newCar <- vector(mode = "numeric", length = length(x))
newCar[1] <- new_car_initial_price

savings <- vector(mode = "numeric", length = length(x))
savings[1] <- savings_account_initial

carPrice <- vector(mode = "numeric", length = length(x))
carPrice[1] <- new_car_initial_price - used_car_initial_value



# running the simulation
for (i in 2:length(x)) {
  oldCar[i] <- oldCar[i-1] - oldCar[i-1] * depreciation_ratePerStep
  newCar[i] <- newCar[i-1] + newCar[i-1] * inflation_ratePerStep
  savings[i] <- savings[i-1] + monthly_payment + savings[i-1] * interest_ratePerStep
  
  carPrice[i] <- newCar[i] - oldCar[i]
  
}


# plotting the graph
y_min <- min(0, min(carPrice), min(savings)) 
y_max <- max(max(carPrice), max(savings)) 


plot(
  x, 
  carPrice,
  type = 'l',
  col = 'red',
  xlab = 'Years',
  ylim = c(y_min, y_max),
  ylab = 'Dollars',
  lwd = 2
)


lines(x, savings, lwd = 2, lty = "dashed", col = 'blue')

legend("topright", legend = c("Car Price", "Savings"), col = c("red", "blue"), lty = c(1, 2), lwd = 2)

result <- x[which(savings > carPrice)[1]] 
cat("The lines cross after", result, "years")