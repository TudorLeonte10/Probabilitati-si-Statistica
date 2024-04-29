Student_LNM <- function(n = c(1000, 10000, 100000, 1000000), r = c(2, 3, 4, 5), n_samples, sample_sizes) 
{
  for (n_samples in n) 
  {
    for (r_val in r) 
    {
      sample_averages <- matrix(nrow = n_samples, ncol = length(sample_sizes))
      
      for (i in 1:length(sample_sizes)) 
      {
        samples <- matrix(rt(n_samples * sample_sizes[i], df = r), ncol = sample_sizes[i])
        sample_averages[, i] <- apply(samples, 1, mean)
      }
      
      plot(sample_sizes, colMeans(sample_averages), type = "l", xlab = "Sample size", ylab = "Sample average")
      
      abline(h = 0, col = "red", lty = 2)
    }
  }
}

Student_LNM(n_samples = 100, sample_sizes = c(100, 200, 500, 1000))