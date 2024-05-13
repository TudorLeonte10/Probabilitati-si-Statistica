f <- function(u) {
  exp(-2*u^2)
}

lambda <- 3
N <- 50000
x <- rexp(N, lambda)

estimate <- sum(f(x))/(lambda*N)

true_value <- sqrt(pi)/8
error <- abs(estimate - true_value)

cat("Estimate:", estimate, "\n")
cat("True value:", true_value, "\n")
cat("Error:", error, "\n")
