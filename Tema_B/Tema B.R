#B1

monte_carlo_torus_volume <- function(R, r, num_samples) {
  count_inside <- 0
  
  for (i in 1:num_samples) {
    x1 <- runif(1, -R-r, R+r)
    x2 <- runif(1, -R-r, R+r)
    x3 <- runif(1, -r, r)
    
    if (x3^2 + (sqrt(x1^2 + x2^2) - R)^2 <= r^2) {
      count_inside <- count_inside + 1
    }
  }
  
  volume_cube <- (2*(R+r))^2 * (2*r)
  P <- count_inside / num_samples
  volume_torus <- volume_cube * P
  
  return(volume_torus)
}

R <- 10
r <- 3
volum_exact <- 2 * pi^2 * R * r^2

sample_sizes <- c(10000, 20000, 50000)

set.seed(123)  

results <- sapply(sample_sizes, function(n) {
  volum_estimare <- monte_carlo_torus_volume(R, r, n)
  eroare_relativa <- abs(volum_estimare - volum_exact) / volum_exact
  c(volum_estimare, eroare_relativa)
})

results <- t(results)
colnames(results) <- c("Volum Estimat", "Eroare Relativa")
rownames(results) <- sample_sizes

print(results)

cat("Volumul exact al torului este:", volum_exact, "\n")


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
