n <- 100000
a <- rgeom(n, 0.3)
b <- rgeom(n, 0.5)

p <- mean(x > y^2)

n <- 100
error <- 1
while (error > 0.005) {
  a <- c(a, rgeom(n, 0.3))
  b <- c(b, rgeom(n, 0.5))
  p_new <- mean(x > y^2)
  error <- qnorm(0.975)*sqrt(p_new*(1-p_new)/length(x))
  n <- n + 100 
}

cat("Estimated probability:", p, "\n")
cat("Number of runs:", length(x), "\n")