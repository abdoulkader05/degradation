source("wiener.R")

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

estimate_all_wiener = function(list_X, list_T){
  
  
  n = length(list_X)
  
  #Estimation mu
  
  sum_times <- 0
  sum_X <- 0
  
  
  for (i in 1:n){
    
    mi <- length(list_T[[i]])
    
    sum_times <- sum_times + as.numeric(list_T[[i]][mi])
    
    sum_X <- sum_X + as.numeric(list_X[[i]][mi])
    
  }
  
  mu_estimated <- sum_X/sum_times
  
  #Estimation sigma2
  
  S_sigma = 0
  somme_observation = 0
  for( k in 1:n) {
    somme_sigma = (as.numeric(list_X[[k]][1]) -  mu_estimated*as.numeric(list_T[[k]][1]))/ as.numeric(list_T[[k]][2])
    #calcul d'increment d'une trajectoire k
    for (i in 2:length(list_X[[k]])){
      somme_sigma = somme_sigma + (((as.numeric(list_X[[k]][i]) - as.numeric(list_X[[k]][i-1])) - mu_estimated*(as.numeric(list_T[[k]][i]) - as.numeric(list_T[[k]][i-1])))^2/(as.numeric(list_T[[k]][i]) - as.numeric(list_T[[k]][i-1])))
    }
    S_sigma = S_sigma + somme_sigma
    somme_observation = somme_observation + length(list_X[[k]])
  }
  sigma2_estimated <- S_sigma / somme_observation
  
  return (list(mu_estimated, sigma2_estimated))
  
  
  
}

estimation_EMM_wiener <- function(list_X, list_T){
  n = length(list_X)
  
  #Estimation mu

  sum <- 0
  
  for (i in 1:n){
    mi <- length(list_T[[i]])
    quotient <- as.numeric(list_X[[i]][1])/as.numeric(list_T[[i]][2])
    for (j in 2:mi){
      variation <- (as.numeric(list_X[[i]][j]) - as.numeric(list_X[[i]][j-1]))/(as.numeric(list_T[[i]][j]) - as.numeric(list_T[[i]][j-1]))
      quotient <- quotient + variation
    }
    sum <- sum + quotient
  }
  mu_estime <- sum/(n*length(list_T[[1]]))
  
  #Estimation sigma2
  
  somme_sigma <- 0
  S_sigma = 0
  for( k in 1:n) {
    somme_sigma = (as.numeric(list_X[[k]][1]) -  mu_estime*as.numeric(list_T[[k]][1])) / as.numeric(list_T[[k]][2])
    #calcul d'increment d'une trajectoire k
    for (i in 2:length(list_X[[k]])){
      somme_sigma = somme_sigma + (((as.numeric(list_X[[k]][i]) - as.numeric(list_X[[k]][i-1])) - mu_estime*(as.numeric(list_T[[k]][i]) - as.numeric(list_T[[k]][i-1])))^2/(as.numeric(list_T[[k]][i]) - as.numeric(list_T[[k]][i-1])))
    }
    S_sigma = S_sigma + somme_sigma
    
  }
  sigma2_estimated <- S_sigma / (n*length(list_T[[1]]))
  
  return (list(mu_estime, sigma2_estimated))
}

biais_error_wienner = function(mu,sigma2,nb_data,nb_traj,temps,iteration) {
  data = matrix(0,nrow = iteration, ncol = 2)
  for(i in 1:iteration) {
    
    data[i,1] = estimate_all_wiener(simulate_all_wiener(mu, sigma2, nb_data ,nb_traj,temps)[[1]],temps)[[1]] -mu 
    data[i,2] = estimate_all_wiener(simulate_all_wiener(mu, sigma2, nb_data ,nb_traj,temps)[[1]],temps)[[2]] -sigma2
    
  }
  par(mfrow = c(1,2))
  hist(data[,1], probability = TRUE, main = "Biais de mu (EMV)", xlab="Erreur")
  curve(dnorm(x,mean = mean(data[,1]), sd = sd(data[,1])), add = TRUE, col = 'red')
  hist(data[,2], probability = TRUE, main = "Biais de sigma2 (EMV)", xlab="Erreur")
  curve(dnorm(x,mean = mean(data[,2]), sd = sd(data[,2])), add = TRUE, col = 'blue')
}


##############################################Partie TEST####################################
# mu <- 2
# sigma2 <- 1
# n<- 10
# nb<- 2
# dt<- 1
# 
# T <- periodique_T(n, dt, nb)
# 
# simu <- simulate_all_wiener(mu, sigma2, n ,nb,T)
# 
# list_W <- simu[[1]]
# 
# estim <- estimate_all_wiener(list_W, T)
# 
# mu_estim <- estim[[1]]
# 
# sigma2_estim <- estim[[2]]
# 
# mu_estim
# sigma2_estim

# biais_error_wienner(mu, sigma2, nb_data = n, nb_traj = nb , T, iteration = 10 )







