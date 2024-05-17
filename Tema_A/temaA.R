
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
citeste_si_calculeaza_medii <- function(fisier) {
  data <- read.table(fisier, header = TRUE)
  
  echantion_P <- data$P
  echantion_S <- data$S
  
  frecvente_absolute_P <- table(echantion_P)
  frecvente_absolute_P_vector <- as.vector(frecvente_absolute_P)
  
  frecvente_relative_P <- prop.table(frecvente_absolute_P)
  frecvente_relative_P_vector <- as.vector(frecvente_relative_P)
  
  frecvente_absolute_S <- table(echantion_S)
  frecvente_absolute_S_vector <- as.vector(frecvente_absolute_S)
  
  frecvente_relative_S <- prop.table(frecvente_absolute_S)
  frecvente_relative_S_vector <- as.vector(frecvente_relative_S)
  
  media_echantion_P <- mean(echantion_P)
  media_echantion_S <- mean(echantion_S)
  
  rezultate <- list(
    frecvente_absolute_P = frecvente_absolute_P_vector,
    frecvente_relative_P = frecvente_relative_P_vector,
    frecvente_absolute_S = frecvente_absolute_S_vector,
    frecvente_relative_S = frecvente_relative_S_vector,
    media_echantion_P = media_echantion_P,
    media_echantion_S = media_echantion_S
  )
  
  return(rezultate)
}

rezultate <- citeste_si_calculeaza_medii("note_PS.txt")
print(rezultate)

#A2 b)
elimina_valori_aberante_si_grafic <- function(nume_fisier, nume_esantion) {
  data <- read.table(nume_fisier, header = TRUE)
  
  esantion <- data[[nume_esantion]]
  
  Q1 <- quantile(esantion, 0.25)
  Q3 <- quantile(esantion, 0.75)
  IQR <- Q3 - Q1
  
  limita_inferioara <- Q1 - 1.5 * IQR
  limita_superioara <- Q3 + 1.5 * IQR
  
  valori_aberante <- esantion[esantion < limita_inferioara | esantion > limita_superioara]
  
  esantion_curatat <- esantion[esantion >= limita_inferioara & esantion <= limita_superioara]
  
  intervale <- cut(esantion_curatat, breaks = 1:10, include.lowest = TRUE, right = TRUE)
  distributie_frecvente <- table(intervale)
  
  barplot(distributie_frecvente, main = paste("Distribuția Frecvențelor după Eliminarea Valorilor Aberante\n", nume_esantion),
          xlab = "Intervale", ylab = "Frecvență", col = "skyblue")
  
  return(esantion_curatat)
}

# Exemplu de utilizare a funcției
esantion_curatat_P <- elimina_valori_aberante_si_grafic("note_PS.txt", "P")
esantion_curatat_S <- elimina_valori_aberante_si_grafic("note_PS.txt", "S")
