test_proportion <- function(alpha, n, num_successes, p0) {
  p_hat <- num_successes / n
  z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
  z_critical <- qnorm(1 - alpha)
  
  return(list(z_critical = z_critical, z = z))
}

# Exemplu de utilizare pentru exercițiul IV.2
alpha <- 0.05
n <- 150
num_successes <- 20
p0 <- 0.10

result <- test_proportion(alpha, n, num_successes, p0)

cat("Valoarea critică:", result$z_critical, "\n")
cat("Scorul testului (z-score):", result$z, "\n")

if (result$z > result$z_critical) {
  cat("Se poate afirma că procentul componentelor defecte a crescut.\n")
} else {
  cat("Nu se poate afirma că procentul componentelor defecte a crescut.\n")
}
