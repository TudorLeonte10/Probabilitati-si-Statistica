binomial_probability = function(n, p, k) 
{
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q = (k + 0.5)/standard_deviation;
  return(1 - pnorm(q));
}

binomial_probability(50, 0.3, 7)