#B1

R <- 10
r <- 3
V_paralelipiped <- (2 * R) * (2 * R) * (2 * r)
V_exact <- 2 * pi^2 * R * r^2

mc <- function(R, r, num_points) {
  x1 <- runif(num_points, -R, R)
  x2 <- runif(num_points, -R, R)
  x3 <- runif(num_points, -r, r)
  
  distances <- sqrt(x1^2 + x2^2) - R
  interior <- sum(x3^2 + distances^2 < r^2)
  V_estimat <- V_paralelipiped * (interior / num_points)
  return(V_estimat)
}

esantioane <- c(10000, 20000, 50000)
results <- data.frame(SampleSize = esantioane, EstimatedVolume = NA, RelativeError = NA)

for (i in 1:length(esantioane)) {
  num_points <- esantioane[i]
  V_estimat <- mc(R, r, num_points)
  eroare_rel <- abs(V_estimat - V_exact) / V_exact
  results$EstimatedVolume[i] <- V_estimat
  results$RelativeError[i] <- eroare_rel
}

print(results)

# B2
in_triunghi <- function(x, y) {
  return(y >= 0 & y <= 2 * x & y <= 6 - 3 * x)
}

a <- 0
b <- 2
c <- 0
d <- 6

n <- 20000

set.seed(123) 
x_puncte <- runif(n, a, b)
y_puncte <- runif(n, c, d)

in_triunghi_puncte <- sapply(1:n, function(i) in_triunghi(x_puncte[i], y_puncte[i]))
numar_puncte_in_triunghi <- sum(in_triunghi_puncte)

aria_dreptunghiului <- (b - a) * (d - c)

aria_triunghiului <- aria_dreptunghiului * (numar_puncte_in_triunghi / n)

print(aria_triunghiului)

#B3 a)
MC_integral <- function(N) {
  sum = 0
  for(i in 1:N) {
    x = runif(1, -1, 1) 
    sum = sum + (2*x - 1) / ((x - 3) * (x + 2)) 
  }
  return(sum * (2 / N))
}

MC_integral_average <- function(k, N) {
  estimates = numeric(k)
  for(i in 1:k) {
    estimates[i] = MC_integral(N) 
  }
  mean_estimate = mean(estimates)  
  sd_estimate = sd(estimates)      
  return(list(mean = mean_estimate, sd = sd_estimate))
}
exact_value <- log(3) - log(2)
result = MC_integral_average(20, 30000)

cat("Monte Carlo Estimate: ", result$mean, "\n")
cat("Standard Deviation: ", result$sd, "\n")
cat("Exact Value: ", exact_value, "\n")
cat("Difference: ", abs(result$mean - exact_value), "\n")

#b)
MC_integral <- function(N) {
  sum = 0
  for(i in 1:N) {
    x = runif(1, 3, 11) 
    sum = sum + (x + 4) / ((x - 3) ^ (1/3)) 
  }
  return(sum * (8 / N))
}

MC_integral_average <- function(k, N) {
  estimates = numeric(k)
  for(i in 1:k) {
    estimates[i] = MC_integral(N) 
  }
  mean_estimate = mean(estimates)  
  sd_estimate = sd(estimates)      
  return(list(mean = mean_estimate, sd = sd_estimate))
}
exact_value <- 68.8
result = MC_integral_average(20, 30000)

cat("Monte Carlo Estimate: ", result$mean, "\n")
cat("Standard Deviation: ", result$sd, "\n")
cat("Exact Value: ", exact_value, "\n")
cat("Difference: ", abs(result$mean - exact_value), "\n")

#c)
MC_integral <- function(N) {
  sum = 0
  for(i in 1:N) {
    x = rexp(1) 
    sum = sum + x * exp(-x^2)  
  }
  return(sum / N)  
}

MC_integral_average <- function(k, N) {
  estimates = numeric(k) 
  for(i in 1:k) {
    estimates[i] = MC_integral(N)  
  }
  mean_estimate = mean(estimates)  
  sd_estimate = sd(estimates)      
  return(list(mean = mean_estimate, sd = sd_estimate))
}
exact_value <- 1/2
result = MC_integral_average(20, 30000)

cat("Monte Carlo Estimate: ", result$mean, "\n")
cat("Standard Deviation: ", result$sd, "\n")
cat("Exact Value: ", exact_value, "\n")
cat("Difference: ", abs(result$mean - exact_value), "\n")

#B4 a)
initial_users <- 10000
target_users <- 15000
n <- 1000
p <- 0.25
q <- 0.01
num_simulations <- 1000

simulate_year <- function(current_users) {
  new_users <- rbinom(1, n, p)
  remaining_users <- rbinom(1, current_users, 1 - q)
  return(remaining_users + new_users)
}

simulate_until_target <- function() {
  current_users <- initial_users
  years <- 0
  while (current_users < target_users) {
    current_users <- simulate_year(current_users)
    years <- years + 1
  }
  return(years)
}

set.seed(123) 
years_to_target <- replicate(num_simulations, simulate_until_target())
mean_years_to_target <- mean(years_to_target)

print(mean_years_to_target)

#b)
years <- 40
months <- 10
total_years <- years + months / 12

simulate_years <- function(total_years) {
  current_users <- initial_users
  for (year in 1:total_years) {
    current_users <- simulate_year(current_users)
  }
  return(current_users)
}

set.seed(123) 
final_users <- replicate(num_simulations, simulate_years(total_years))
probability_target <- mean(final_users >= target_users)

print(probability_target)

#c)
estimate_probability_with_precision <- function(total_years, epsilon, confidence_level) {
  z <- qnorm((1 + confidence_level) / 2)
  p_hat <- 0
  n <- 0
  while (TRUE) {
    n <- n + 1
    current_users <- simulate_years(total_years)
    p_hat <- ((n - 1) * p_hat + (current_users >= target_users)) / n
    se <- sqrt((p_hat * (1 - p_hat)) / n)
    margin_of_error <- z * se
    if (margin_of_error <= epsilon) {
      break
    }
  }
  return(list(probability = p_hat, num_simulations = n))
}

epsilon <- 0.01
confidence_level <- 0.99

set.seed(123) 
result <- estimate_probability_with_precision(total_years, epsilon, confidence_level)

print(result$probability)
print(result$num_simulations)
