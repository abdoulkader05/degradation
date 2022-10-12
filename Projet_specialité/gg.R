source("simulation.R")

#Calcule les Delta_Wi et les delta_T
compute_delta = function(W, T){ 
  
  n=length(W)
  delta_T <- 1:n
  delta_W<- 1:n
  delta_W[1] = W[1]
  delta_T[1] = T[1]
  
  for (i in 2:n){
    delta_T[i] = T[i]- T[i-1]
    delta_W[i]=W[i] - W[i-1] 
  }
  
  return (list(delta_W,delta_T))
  
}

compute_gen_delta = function(W, T){ 
  
  n=length(W)
  delta_T <- 1:n
  delta_W<- 1:n
  delta_W[1] = W[1]
  delta_T[1] = T[1]
  
  for (i in 2:n){
    delta_T[i] = T[i]- T[i-1]
    delta_W[i]=W[i] - W[i-1] 
  }
  
  return (list(delta_W,delta_T))
  
}


estimate_gen <-function(list_W, T){
  nb_curves<- length(list_W)
  time = 0
  S = 0 
  for( k in 1:nb_curves) {
    somme = list_W[[k]][1]
    temps = T[k,1]
    #calcul d'increment d'une trajectoire k
    for (i in 2:length(list_W[[k]])){
      somme = somme + list_W[[k]][i] - list_W[[k]][i-1]
      temps = temps + T[k,i] - T[k,i-1]
    }
    S = S+ somme
    time = time + temps
  }
  
  mu_estimated <- S/time
  S_sigma = 0
  somme_observation = 0
  for( k in 1:nb_curves) {
    somme_sigma = list_W[[k]][1]
    temps_sigma = T[k,1]
    #calcul d'increment d'une trajectoire k
    for (i in 2:length(list_W[[k]])){
      somme_sigma = somme_sigma + (((list_W[[k]][i] - list_W[[k]][i-1]) - mu_estimated*(T[k,i] - T[k,i-1]))^2/(T[k,i] - T[k,i-1]))
    }
    S_sigma = S_sigma + somme_sigma
    somme_observation = somme_observation + length(list_W[[k]])
  }
  sigma2_estimated <- S_sigma / somme_observation
  return (list(mu_estimated, sigma2_estimated ))
}

#Estime mu et sigma pour plusieurs courbes de Wiener
estimate_all_wiener = function(list_W, T){ 
  
  nb_curves<- length(list_W)
  
  #On tire au hasard la courbe pour laquelle on va prendre le W et T pour le calcul des estimateurs
  
  #index_curve <- sample(1:nb_curves, 1)
  somme = 0
  temps = 0
  for( i in 1:nb_curves) {
    W = list_W[[i]]
    deltas <- compute_delta(W, T)
    delta_W <- deltas[[1]]
    delta_T <- deltas[[2]]
    somme = somme + sum(delta_W)
    temps = sum(delta_T)
    
  }
  
  mu_estimated<- somme/(nb_curves * temps)
  somme_sigma = 0
  taille = 0
  for(i in 1:nb_curves) {
    W = list_W[[i]]
    deltas <- compute_delta(W, T)
    delta_W <- deltas[[1]]
    delta_T <- deltas[[2]]
    taille = length(delta_W)
    somme_sigma = somme_sigma + sum((delta_W - (mu_estimated*delta_T))^2 /delta_T)
  }
  sigma2_estimated = somme_sigma / (taille * nb_curves)
  #sigma2_estimated = 5
  return(list(mu_estimated, sigma2_estimated ))
  
}



