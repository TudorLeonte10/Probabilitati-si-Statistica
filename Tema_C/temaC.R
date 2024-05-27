#C1 a)
set.seed(123)

generate_random_permutation <- function(n) {
  U <- runif(n)
  
  permutation <- order(U)
  
  return(permutation)
}

generate_random_bit_strings <- function(n, k) {
  bit_strings <- matrix(sample(c(0, 1), n * k, replace = TRUE), nrow = n, ncol = k)
  
  return(bit_strings)
}

n <- 10  
k <- 4   

random_permutation <- generate_random_permutation(n)
print("Random Permutation:")
print(random_permutation)

random_bit_strings <- generate_random_bit_strings(n, k)
print("Random Bit Strings:")
print(random_bit_strings)

#b)
set.seed(123)

generate_random_bit <- function() {
  return(sample(c(0, 1), 1))
}

compare_lexicographically <- function(Wi, Wj) {
  len_i <- length(Wi)
  len_j <- length(Wj)
  Lij <- min(len_i, len_j)
  
  for (l in 1:Lij) {
    if (Wi[l] < Wj[l]) {
      return(TRUE)
    } else if (Wi[l] > Wj[l]) {
      return(FALSE)
    }
  }
  
  if (len_i < len_j) {
    for (h in (len_i + 1):len_j) {
      Wi <- c(Wi, generate_random_bit())
      if (Wi[h] < Wj[h]) {
        return(TRUE)
      } else if (Wi[h] > Wj[h]) {
        return(FALSE)
      }
    }
  } else if (len_i > len_j) {
    for (h in (len_j + 1):len_i) {
      Wj <- c(Wj, generate_random_bit())
      if (Wi[h] < Wj[h]) {
        return(TRUE)
      } else if (Wi[h] > Wj[h]) {
        return(FALSE)
      }
    }
  }
  
  while (TRUE) {
    bit_i <- generate_random_bit()
    bit_j <- generate_random_bit()
    Wi <- c(Wi, bit_i)
    Wj <- c(Wj, bit_j)
    if (bit_i < bit_j) {
      return(TRUE)
    } else if (bit_i > bit_j) {
      return(FALSE)
    }
  }
}

Wi <- c(1, 0, 0, 1)
Wj <- c(1, 0, 1)

result <- compare_lexicographically(Wi, Wj)
print("Wi is lexicographically strictly less than Wj:")
print(result)

#c)
set.seed(123)

generate_random_bit <- function() {
  return(sample(c(0, 1), 1))
}

compare_lexicographically <- function(Wi, Wj) {
  len_i <- length(Wi)
  len_j <- length(Wj)
  Lij <- min(len_i, len_j)
  
  for (l in 1:Lij) {
    if (Wi[l] < Wj[l]) {
      return(TRUE)
    } else if (Wi[l] > Wj[l]) {
      return(FALSE)
    }
  }
  
  if (len_i < len_j) {
    for (h in (len_i + 1):len_j) {
      Wi <- c(Wi, generate_random_bit())
      if (Wi[h] < Wj[h]) {
        return(TRUE)
      } else if (Wi[h] > Wj[h]) {
        return(FALSE)
      }
    }
  } else if (len_i > len_j) {
    for (h in (len_j + 1):len_i) {
      Wj <- c(Wj, generate_random_bit())
      if (Wi[h] < Wj[h]) {
        return(TRUE)
      } else if (Wi[h] > Wj[h]) {
        return(FALSE)
      }
    }
  }
  
  while (TRUE) {
    bit_i <- generate_random_bit()
    bit_j <- generate_random_bit()
    Wi <- c(Wi, bit_i)
    Wj <- c(Wj, bit_j)
    if (bit_i < bit_j) {
      return(TRUE)
    } else if (bit_i > bit_j) {
      return(FALSE)
    }
  }
}

quicksort <- function(words) {
  if (length(words) <= 1) {
    return(words)
  }
  
  pivot_index <- sample(1:length(words), 1)
  pivot <- words[[pivot_index]]
  
  less <- list()
  equal <- list()
  greater <- list()
  
  for (word in words) {
    if (identical(word, pivot)) {
      equal <- append(equal, list(word))
    } else if (compare_lexicographically(word, pivot)) {
      less <- append(less, list(word))
    } else {
      greater <- append(greater, list(word))
    }
  }
  
  sorted_less <- quicksort(less)
  sorted_greater <- quicksort(greater)
  
  return(c(sorted_less, equal, sorted_greater))
}

n <- 10  # Number of elements
k <- 4   # Length of each bit string

generate_random_bit_strings <- function(n, k) {
  bit_strings <- vector("list", n)
  for (i in 1:n) {
    bit_strings[[i]] <- sample(c(0, 1), k, replace = TRUE)
  }
  return(bit_strings)
}

random_bit_strings <- generate_random_bit_strings(n, k)
print("Original Bit Strings:")
print(random_bit_strings)

sorted_bit_strings <- quicksort(random_bit_strings)
print("Sorted Bit Strings:")
print(sorted_bit_strings)

#d)

set.seed(123)

generate_random_bit <- function() {
  return(sample(c(0, 1), 1))
}

compare_lexicographically <- function(Wi, Wj) {
  len_i <- length(Wi)
  len_j <- length(Wj)
  Lij <- min(len_i, len_j)
  
  for (l in 1:Lij) {
    if (Wi[l] < Wj[l]) {
      return(TRUE)
    } else if (Wi[l] > Wj[l]) {
      return(FALSE)
    }
  }
  
  if (len_i < len_j) {
    for (h in (len_i + 1):len_j) {
      Wi <- c(Wi, generate_random_bit())
      if (Wi[h] < Wj[h]) {
        return(TRUE)
      } else if (Wi[h] > Wj[h]) {
        return(FALSE)
      }
    }
  } else if (len_i > len_j) {
    for (h in (len_j + 1):len_i) {
      Wj <- c(Wj, generate_random_bit())
      if (Wi[h] < Wj[h]) {
        return(TRUE)
      } else if (Wi[h] > Wj[h]) {
        return(FALSE)
      }
    }
  }
  
  while (TRUE) {
    bit_i <- generate_random_bit()
    bit_j <- generate_random_bit()
    Wi <- c(Wi, bit_i)
    Wj <- c(Wj, bit_j)
    if (bit_i < bit_j) {
      return(TRUE)
    } else if (bit_i > bit_j) {
      return(FALSE)
    }
  }
}

quicksort <- function(words, indices) {
  if (length(words) <= 1) {
    return(list(words = words, indices = indices))
  }
  
  pivot_index <- sample(1:length(words), 1)
  pivot <- words[[pivot_index]]
  pivot_idx <- indices[[pivot_index]]
  
  less <- list()
  equal <- list()
  greater <- list()
  less_indices <- list()
  equal_indices <- list()
  greater_indices <- list()
  
  for (i in 1:length(words)) {
    word <- words[[i]]
    idx <- indices[[i]]
    if (identical(word, pivot)) {
      equal <- append(equal, list(word))
      equal_indices <- append(equal_indices, list(idx))
    } else if (compare_lexicographically(word, pivot)) {
      less <- append(less, list(word))
      less_indices <- append(less_indices, list(idx))
    } else {
      greater <- append(greater, list(word))
      greater_indices <- append(greater_indices, list(idx))
    }
  }
  
  sorted_less <- quicksort(less, less_indices)
  sorted_greater <- quicksort(greater, greater_indices)
  
  sorted_words <- c(sorted_less$words, equal, sorted_greater$words)
  sorted_indices <- c(sorted_less$indices, equal_indices, sorted_greater$indices)
  
  return(list(words = sorted_words, indices = sorted_indices))
}

generate_random_permutation <- function(n, k) {
  generate_random_bit_strings <- function(n, k) {
    bit_strings <- vector("list", n)
    for (i in 1:n) {
      bit_strings[[i]] <- sample(c(0, 1), k, replace = TRUE)
    }
    return(bit_strings)
  }
  
  random_bit_strings <- generate_random_bit_strings(n, k)
  
  indices <- as.list(1:n)
  
  sorted_result <- quicksort(random_bit_strings, indices)
  
  return(unlist(sorted_result$indices))
}

n <- 10  # Number of elements
k <- 4   # Length of each bit string

random_permutation <- generate_random_permutation(n, k)
print("Random Permutation:")
print(random_permutation)

#C2
#a)

if (!require(igraph)) {
  install.packages("igraph", dependencies=TRUE)
  library(igraph)
}
#este nevoie pentru generarea grafurilor; daca nu este instalata biblioteca, o va instala

graph <- function(n, m) {
  V <- 2*n  # sau 2*n + 1
  g <- erdos.renyi.game(V, m, type="gnm", directed=FALSE)
  return(g)
}

random <- function(g, n) {
  V <- vcount(g)
  A <- sample(V, n)
  B <- setdiff(1:V, A)
  return(list(A=A, B=B))
}

cardinal <- function(g, A, B) {
  E_AB <- ecount(subgraph.edges(g, E(g)[which(ends(g, E(g))[,1] %in% A & ends(g, E(g))[,2] %in% B)]))
  return(E_AB)
}

n <- 5  
m <- 10
g <- graph(n, m)
plot(g)
partition <- random(g, n)
A <- partition$A
B <- partition$B
cut_size <- cardinal(g, A, B)

cat("Mulțimea A:", A, "\n")
cat("Mulțimea B:", B, "\n")
cat("Cardinalul tăieturii E(A, B):", cut_size, "\n")

#C2
#b)

max_cut <- function(g, n, num_iterations) {
  max_cut_size <- 0
  best_partition <- NULL
  for (i in 1:num_iterations) {
    partition <- random_partition(g, n)
    A <- partition$A
    B <- partition$B
    cut_size <- cut_cardinality(g, A, B)
    if (cut_size > max_cut_size) {
      max_cut_size <- cut_size
      best_partition <- partition
    }
  }
  return(list(max_cut_size=max_cut_size, best_partition=best_partition))
}

n <- 5
m <- 10  
num_iterations <- 1000  # Numărul de iterații pentru a găsi tăietura maximă


