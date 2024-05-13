a <- 1/4
b <- 3/4

l1 <- 4
l2 <- 12

N <- 10000
sum <- 0
for (i in 1:N) {
  x <- a*rexp(1, l1) + b*rexp(1, l2)
  sum <- sum + x
}
expectation <- sum/N

cat("Estimated expectation:", expectation, "\n")
