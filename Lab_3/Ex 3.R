GammaTCL <- function(alpha, lambda, n = 50, N = c(5000, 10000, 20000), z = c(-1.5, 0, 1.5)) 
{
  for (n_samples in N) 
  {
    for (z_val in z) 
    {
      samples <- matrix(rgamma(n_samples * n, shape = alpha, rate = lambda), ncol = n)
      
      sample_means <- apply(samples, 1, mean)
      
      mu <- n * alpha / lambda
      sigma <- sqrt(n * alpha) / lambda
      
      z_quantile <- qnorm(pnorm(z_val))
      
      hist(sample_means, breaks = 30, freq = FALSE, main = paste0("n = ", n, ", N = ", n_samples, ", z = ", z_val))
      curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, col = "red")
      abline(v = mu + z_quantile * sigma, col = "blue")
    }
  }
}

GammaTCL(1, 2)