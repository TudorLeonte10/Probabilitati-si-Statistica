#D1

data <- read.csv("probabilitati.csv", skip = 1, header = FALSE) 

sample_mean <- mean(data$V1)
n <- length(data$V1)
sigma_patrat <- 92.16
sigma <- sqrt(sigma_patrat)
se <- sigma / sqrt(n)
alpha_95 <- 0.05
alpha_99 <- 0.01
z_critical_95 <- qnorm(1 - alpha_95 / 2)
z_critical_99 <- qnorm(1 - alpha_99 / 2)

error_95 <- z_critical_95 * se
low95 <- sample_mean - error_95
upp95 <- sample_mean + error_95

error_99 <- z_critical_99 * se
low99 <- sample_mean - error_99
upp99 <- sample_mean + error_99

results <- list(
  "Media eșantionului" = sample_mean,
  "95% Interval de încredere" = c(low95, upp95),
  "99% Interval de încredere" = c(low99, upp99)
)

results


#D2

data <- read.csv("statistica.csv", skip = 1, header = FALSE)

data$V1 <- as.numeric(data$V1)
sample_mean <- mean(data$V1, na.rm = TRUE)  # Ignorăm valorile NA
n <- length(na.omit(data$V1))  # Numărăm doar valorile non-NA

se <- sd(data$V1) / sqrt(n) 
alpha_95 <- 0.05
alpha_99 <- 0.01
z_critical_95 <- qnorm(1 - alpha_95 / 2)
z_critical_99 <- qnorm(1 - alpha_99 / 2)

error_95 <- z_critical_95 * se
low95 <- sample_mean - error_95
upp95 <- sample_mean + error_95

error_99 <- z_critical_99 * se
low99 <- sample_mean - error_99
upp99 <- sample_mean + error_99

results <- list(
  "Media eșantionului" = sample_mean,
  "95% Interval de încredere" = c(low95, upp95),
  "99% Interval de încredere" = c(low99, upp99)
)

print(results)

#D3
test_proportion <- function(alpha, n, num_successes, p0) {
  p_hat <- num_successes / n
  
  z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
  
  z_critical <- qnorm(1 - alpha)
  
  return(list(z_critical = z_critical, z = z))
}

n <- 100
num_failures <- 14
p0 <- 0.15  

alpha1 <- 0.01
alpha2 <- 0.05

result1 <- test_proportion(alpha1, n, num_failures, p0)
cat("Nivel de semnificație 1%:\n")
cat("Valoarea critică:", result1$z_critical, "\n")
cat("Scorul testului (z-score):", result1$z, "\n")

if (result1$z < -result1$z_critical) {
  cat("Se poate trage concluzia că schimbarea nu a fost utilă la nivel de semnificație de 1%.\n")
} else {
  cat("Nu se poate trage concluzia că schimbarea nu a fost utilă la nivel de semnificație de 1%.\n")
}

result2 <- test_proportion(alpha2, n, num_failures, p0)
cat("\nNivel de semnificație 5%:\n")
cat("Valoarea critică:", result2$z_critical, "\n")
cat("Scorul testului (z-score):", result2$z, "\n")

if (result2$z < -result2$z_critical) {
  cat("Se poate trage concluzia că schimbarea nu a fost utilă la nivel de semnificație de 5%.\n")
} else {
  cat("Nu se poate trage concluzia că schimbarea nu a fost utilă la nivel de semnificație de 5%.\n")
}
