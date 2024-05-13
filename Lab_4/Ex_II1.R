
# b)
f <- function(x) exp(x)

a <- 1
b <- 4
exact <- integrate(f, a, b)$value

n <- 10000
x <- seq(a, b, length.out = n+1)
y <- f(x)
aproximare <- (b-a)/(2*n) * (sum(y) - y[1] - y[n+1] + 2 * sum(y[2:n]))

eroare_absoluta <- abs(exact - aproximare)
eroare_relativa <- abs_error / exact * 100

cat("Valoare exacta: ", exact, "\n")
cat("Aproximare: ", aproximare, "\n")
cat("Eroare absoluta: ", eroare_absoluta, "\n")
cat("Eroare relativa: ", eroare_relativa, "%\n")
# d)
monte_carlo_integral <- function() {
  f <- function(x) 1 / (4*x^2 - 1)
  
  a <- 1
  b <- Inf
  exact_value <- log(3/4)
  
  set.seed(123)
  n <- 100000
  x <- runif(n, a, b)
  
  integral <- (b - a) * mean(f(x))
  
  abs_error <- abs(integral - exact_value)
  rel_error <- abs_error / exact_value
  
  result <- list(integral = integral, exact_value = exact_value, abs_error = abs_error, rel_error = rel_error)
  return(result)
}

result <- monte_carlo_integral()

cat("Estimated value:", format(result$integral, digits = 8), "\n")
cat("Exact value:", format(result$exact_value, digits = 8), "\n")
cat("Absolute error:", format(result$abs_error, digits = 8), "\n")
cat("Relative error:", format(result$rel_error, digits = 2), "%\n")