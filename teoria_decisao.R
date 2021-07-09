library(tidyverse)

tempo <- as.matrix(read.csv('tempo.csv', header = F))

distancia <- as.matrix(read.csv('distancia.csv', header = F))

r0 <- 1:250


percurso <- function(matriz, vetor){
  dist <- (0)
  for(i in 1:(nrow(matriz) - 1)){
    inicio <- vetor[i]
    fim <- vetor[i+1]
    dist <- (dist) + matriz[[fim,inicio]]
  }
  return(dist)
}


get_neighbours <- function(vetor, type, k){
vizinhos <- list()
  if(type==1){
      p <- sample(1:(length(vetor) - k + 1), 1)
      shift <- rev(vetor[p:(p+k-1)])
      if(p==1){
        novo <- c(shift,vetor[(k+1):length(vetor)])
      }
      if((p+k-1)==length(vetor)){
        novo <- c(vetor[1:(p-1)],shift)
      }
      if((p!=1) & ((p+k-1)<length(vetor))){
        novo <- c(vetor[1:(p-1)],shift,vetor[(p+k):length(vetor)])
      }
      vizinhos <- novo
      }
  if(type==2){
    novo <- vetor
    positions <- sample(vetor, k)
    perm <- sample(positions)
    i <- 1
    copy <- novo
    for(pos in positions){
      novo[pos] <-perm[i] 
      i <- i + 1
    }
    vizinhos <- novo
  }
  if(type==3){
    start <- sample(1:(length(vetor)-k-1), 1)
    to <- sample(1:(length(vetor)), 1)
    print(start)
    print(to)
    move <- vetor[start:(start+k-1)]
    if(to==start){
      novo <- vetor
    }
    if(to<start){
      novo <- c(vetor[1:(to - 1)], move, vetor[to:(start-1)],
                vetor[(start+k):length(vetor)])
    }
    if(to>start){
      novo <- c(vetor[1:(start - 1)], vetor[(start+k):(to-1)],
                move,vetor[(to):length(vetor)])
    }
    vizinhos <- novo
  }
return(vizinhos)
}

get_neighbours(1:10,3,5)


vns <- function(matriz, vetor, type, k, limit){
  lista <- list()
  iter <- 0
  lista[[1]] <- list(vetor, percurso(matriz,vetor))
  flag <- TRUE
  while(flag==TRUE){
  min <- percurso(matriz, vetor)
  neighbours <- get_neighbours(vetor,type, k)
  dist <- percurso(matriz,neighbours)
  if(dist<min){
    lista[[length(lista)+1]] <- list(neighbours,dist)
    vetor <- neighbours
    iter <- 0
  }
  else{
    iter <- iter + 1
  }
  if(iter>limit){
    flag <- FALSE
    return(lista)
  }
  }
  }

a <- vns(tempo, r0,1,15, 10)

a
