
t_conf_interval_from_file <- function(file_path, alfa) {
  data <- scan(file_path)
  
  n <- length(data)
  xn <- mean(data)
  s <- sd(data)
  
  se <- s / sqrt(n)
  
  critical_t <- qt((1 - alfa / 2), df = (n - 1))
  
  a <- xn - critical_t * se
  b <- xn + critical_t * se
  
  interval <- c(a, b)
  return(interval)
}

file_path <- "history.txt"

interval_95 <- t_conf_interval_from_file(file_path, 0.05)
print("Interval de incredere de 95%:")
print(interval_95)

interval_99 <- t_conf_interval_from_file(file_path, 0.01)
print("Interval de incredere de 99%:")
print(interval_99)
