
# II.6 
zconfidence_interval_fisier <- function(fisier, alfa) 
{
  x=scan(fisier);
  xn=mean(x);
  n=length(x);
  sigma=sqrt(9);
  critical_z=qnorm((1-alfa/2), 0, 1);
  a=xn-critical_z*sigma/sqrt(n);
  b=xn+critical_z*sigma/sqrt(n);
  interval=c(a, b);
  return(interval);
}

zconfidence_interval_fisier("history.txt", 0.05); 