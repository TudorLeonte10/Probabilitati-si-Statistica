parabola_area <- function() {
  f <- function(x) -2*x^2 + 5*x - 2
  a <- 0
  b <- 2
  exact_area <- integrate(f, a, b)$value 
  set.seed(123) 
  n <- 10000
  x <- runif(n, a, b)     
  y <- runif(n, 0, max(f(x)))
  
  inside <- y <= f(x)
  prop_inside <- mean(inside)
  
  area <- (b - a) * max(f(x)) * prop_inside
  rel_error <- abs(exact_area - area) / exact_area
  
  result <- list(area = area, exact_area = exact_area, rel_error = rel_error)
  return(result)
}

result <- parabola_area()

cat("Estimated area:", format(result$area, digits = 8), "\n")
cat("Exact area:", format(result$exact_area, digits = 8), "\n")
cat("Relative error:", format(result$rel_error, digits = 2), "%\n")
