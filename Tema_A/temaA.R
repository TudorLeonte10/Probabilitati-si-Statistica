
#A1 a)
calcul_probabilitati <- function(distributie, parametri, m) {
  k <- 0:m
  if(distributie == "Poisson") {
    probabilitati <- dpois(k, parametri)
  } 
  else if(distributie == "Geometric") {
    probabilitati <- dgeom(k, parametri)
  } 
  else if(distributie == "B") {
    probabilitati <- dbinom(k, parametri[1], parametri[2])
  }
  return(probabilitati)
}

#exemplu de utilizare
prob_poisson <- calcul_probabilitati("Poisson", 2, 10)
prob_geometric <- calcul_probabilitati("Geometric", 0.3, 10)
prob_binomial <- calcul_probabilitati("B", c(5, 0.4), 10)

print(prob_poisson)
print(prob_geometric)
print(prob_binomial)

# A1 b)
plot_probability_mass_function <- function(x, pmf, title="Probabilitatea functiilor de masa")
  
{
  plot(x, pmf, type="h", lwd=2, xlab="Valori", ylab="Probabilitati", main=title)
}

#exemplu de utilizare
x <- c(1, 2, 3, 4, 5)
pmf <- c(0.1, 0.2, 0.3, 0.2, 0.2)

plot_probability_mass_function(x, pmf, title="Exemplu de Funcție de Masă de Probabilitate")


#A1 c) 
PoissonK0 <- function(lambda) {
  k0 <- 0
  p <- dpois(k0, lambda)
  
  while(p <= 1 - 0.000001) {
    k0 <- k0 + 1
    p <- sum(dpois(0:k0, lambda)) 
  }
  
  return(k0)
}

#exemplu de utilizare
lambda <- 2
k0 <- PoissonK0(lambda)
print(k0)

#A2 a)
Functie_a=function(fisier)
{
  
  date=read.csv(fisier)
  
  esantion1=date$P
  esantion2=date$S
  
  
  frec_abs1=as.vector(table(esantion1))
  frec_abs2=as.vector(table(esantion2))
  
  frec_rel1=as.vector(table(esantion1)/length(esantion1))
  frec_rel2=as.vector(table(esantion2)/length(esantion2))
  
  media1=mean(esantion1)
  media2=mean(esantion2)
  
  print("Frecvente absolute esantion1:")
  print(frec_abs1)
  print("Frecvente absolute esantion2:")
  print(frec_abs2)
  print("Frecvente relative esantion1:")
  print(frec_rel1)
  print("Frecvente relative esantion2:")
  print(frec_rel2)
  print("Media esantion1:")
  print(media1)
  print("Media esantion2:")
  print(media2)
  
}
Functie_a("note_PS.csv")

#A2 b)
Functie_b=function(fisier,esantion3)
{
  date=read.csv(fisier,header=TRUE)
  
  esantion=date[[esantion3]]
  
  q1=quantile(esantion,0.25)
  q3=quantile(esantion,0.75)
  iqr=q3-q1
  
  lim_inf=q1-1.5*iqr
  lim_sup=q3+1.5*iqr
  
  newesantion=esantion[esantion>=lim_inf & esantion<=lim_sup]
  hist(newesantion,breaks=seq(1,10,by=1),xlab=esantion3,ylab="Frecventa")
  
}
Functie_b("note_PS.csv","P")
