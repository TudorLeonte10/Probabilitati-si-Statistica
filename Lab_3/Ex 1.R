#a)#####################################################
Gamma <- function(alpha, lambda) 
{
  x <- seq(0, 20, length.out = 1000)
  densitate <- dgamma(x, shape = alpha, rate = lambda)
  
  plot(x, densitate, type = "l", xlab = "x", ylab = "Densitate",
       main = paste0("Distributia Gamma (alpha = ", alpha, ", lambda = ", lambda, ")"))
}

Gamma(2,1)
#b)#####################################################
Student <- function(r) {
  
  x <- seq(-5, 5, length.out = 1000)
  densitate <- dt(x, df = r)
  
  plot(x, densitate, type = "l", xlab = "x", ylab = "Density",
       main = paste0("Student's t Density (", r, " degrees of freedom)"))
}
Student(5)
#c)#####################################################
N <- function(mu, sigma) {
  
  x <- seq(-5, 5, length.out = 1000)
  densitate <- dnorm(x, mean = mu, sd = sigma)
  
  plot(x, densitate, type = "l", xlab = "x", ylab = "Densitate",
       main = paste0("Densitate Normala (mu = ", mu, ", sigma^2 = ", sigma^2, ")"))
}
N(0, 1)