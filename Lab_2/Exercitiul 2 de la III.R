fisier=scan("sample2.txt")

outliers_iqr<- function(fisier)
{
  Q1=quantile(fisier)[2]
  Q3=quantile(fisier)[4]
  
  IQR= Q3-Q1
  v=vector()
  
  for(i in fisier)
    if(i<Q1 -1.5*IQR || i>Q3 +1.5*IQR)
      v=c(v,i)
  return (v)
}
rez_iqr=outliers_iqr(fisier)
print(rez_iqr)