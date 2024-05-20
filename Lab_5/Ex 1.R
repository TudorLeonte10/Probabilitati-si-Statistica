variabila_discreta <- function(valori, probabilitati) 
{
  if(length(valori) != length(probabilitati)) 
  {
    stop("E necesar sa fie egale.")
  }
  
  if(any(probabilitati < 0) || !isTRUE(all.equal(sum(probabilitati), 1)))
  {
    stop("Probabilitatile nu pot fi negative si suma nu trebuie sa depaseasca 1")
  }
  
  nr_rand <- runif(1)
  probabil <- cumsum(probabilitati)
  indexSelectat <- match(TRUE, nr_rand <= probabil)
  
  return(valori[indexSelectat])
}

valori <- c(4, 2, 3)
probabilitati <- c(0.1, 0.3, 0.6)
valoare_sim <- variabila_discreta(valori, probabilitati)
print(valoare_sim)
