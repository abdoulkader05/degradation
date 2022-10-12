library(utils)
# Simule le processus de Wiener pour une courbe
simulate_wiener = function(mu, sigma2, n,T){
  T = sort(unlist(T))
  
  m = mu*T[1]
  sdd = sqrt(sigma2 * T[1])
  W = 1:n+1
  W[1] = 0
  for (i in 1:n+1){
    m = mu*(T[i] - T[i-1])
    sdd = sqrt(sigma2 * (T[i] - T[i-1]))
    W[i] = W[i-1] + rnorm(1, mean = m, sd = sdd)
  }
  return (W)
}

# Simule le processus de Wiener pour plusieures courbes
simulate_all_wiener = function(mu, sigma2, n ,nb,list_T){
  
  #T = switch(choix, periodique_T(n, dt), exp_T(n, lambda),gamma_T(n, a, b), norm_T(n, m, sigma))
  list_W = list()
  
  for (k in 1:nb){
    
    list_W[[k]]<-simulate_wiener(mu, sigma2, n,list_T[[k]])
  }
  
  return (list(list_W, list_T))
}

plot_wiener = function(W, T, num_curve, x_min, x_max, y_min, y_max ){
  
  if (num_curve == 1){
    plot(T,W, type = "l", xlab="time", main="Wiener Process", xlim = c(x_min, x_max), 
         ylim = c(y_min, y_max))
  }else{
    lines(T,W, type = "l", xlab="time", main="Wiener Process", xlim = c(x_min, x_max), 
          ylim = c(y_min, y_max))
  }
  
  
}

norm_T = function(n, mean, sdd, nb){
  
  list_T <- list()
  
  T <- rnorm(n, mean, sdd)
  T <- cumsum(T)
  T = sort(T)
  
  for(k in 1:nb){
    
    list_T[[k]]<- list()
    
    
    list_T[[k]]<- c(0, T)
    
  }
  
  return (list_T)
  
}

norm_T_random = function(n, mean, sdd, nb){

  list_T <- list()

  

  for(k in 1:nb){

    list_T[[k]]<- list()
    T <- rnorm(n+1, mean, sdd)
    T <- cumsum(T)
    T = sort(T)
   
    list_T[[k]] <- T
 

  }

  return (list_T)

}

periodique_T = function(n, dt, nb){
  
  list_T <- list()
  
  time_limit <- n*dt
  
  T <- seq(0, time_limit, dt)
  
  for(k in 1:nb){
    
    list_T[[k]] <- T
  
  }
  
  return(list_T)
}

periodique_T_random = function(n, dt, nb){
  
  list_T <- list()
  
  vec_dt <- seq(dt/100, dt, dt/100)
  list_dt <- sample(vec_dt, size = nb - 1, replace = TRUE)
  list_dt <- c(dt, list_dt)
 
 for(k in 1:nb){
   
   time_limit <- n*list_dt[k]
   
   T <- seq(0, time_limit, list_dt[k])
   
   list_T[[k]] <- T
   
 }

  return(list_T)
}

exp_T = function(n, lambda, nb){
  
  list_T<- list()
  
  T = rexp(n, rate = lambda)
  T = cumsum(T)
  T = sort(T)
  
  for(k in 1:nb){
    list_T[[k]]<- list()
    
    list_T[[k]]<- c(0, T)
    
  }
  
  
  return (list_T)
}


exp_T_random = function(n, lambda, nb){
  
  list_T<- list()
  
  for(k in 1:nb){
    T = rexp(n+1, rate = lambda)
    T = cumsum(T)
    T = sort(T)

    
      list_T[[k]]<- T

    
  }
  
  
  return (list_T)
}

gamma_T = function(n, a, b, nb){
  
  list_T<-list()
  
  T = rgamma(n, rate = a, shape = b)
  T = cumsum(T)
  T = sort(T)
  
  
  for(k in 1:nb){
    
    list_T[[k]]<- list()
    
    list_T[[k]]<- c(0, T)
    
  }
  return (list_T)
}

gamma_T_random = function(n, a, b, nb){
  
  list_T<-list()
  
  for(k in 1:nb){
    
    
    T = rgamma(n+1, rate = a, shape = b)
    T = cumsum(T)
    T = sort(T)
    
    
   
      
      list_T[[k]] <- T

    
  }
  return (list_T)
}


plot_all_wiener = function(list_w, list_T){
  
  nb = length(list_w)
  x_min = 1000000
  y_min = 1000000
  
  x_max = -1000000
  y_max = -1000000
  num_curve=1
  
  for (i in 1:nb){
    
    if (max(unlist(list_T[[i]])) >= x_max ){
      x_max = max(unlist(list_T[[i]]))
    }
    
    if ( max(list_w[[i]]) >= y_max ){
      y_max = max(unlist(list_w[[i]]))
    }
    
    if (min(unlist(list_T[[i]])) <= x_min ){
      x_min = min(unlist(list_T[[i]]))
    }
    
    if ( min(list_w[[i]]) <= y_min ){
      y_min = min(unlist(list_w[[i]]))
    }
  }
    
  num_curve=1
  for (k in 1:nb){
    plot_wiener(list_w[[k]], list_T[[k]], num_curve,x_min, x_max, y_min, y_max)
    num_curve=num_curve+1
  }
}

EQME = function(list_X,list_T,nb_iteration,mu,sigma2) {
  biais_mu_emv = 0
  biais_mu_emm = 0
  biais_sigma2_emv = 0
  biais_sigma2_emm = 0
  variance_mu_emm = 0
  variance_mu_emv = 0
  variance_sigma2_emm = 0
  variance_sigma2_emv = 0
  biais_vrai_mu = 0
  biais_vrai_sigma2 = 0
  for(i in 1:nb_iteration) {
    biais_vrai_mu = biais_vrai_mu + mu
    biais_vrai_sigma2 = biais_vrai_sigma2 + sigma2
  }
  mu_empirique = (1/nb_iteration)*biais_vrai_mu
  sigma2_empirique = (1/nb_iteration)*biais_vrai_sigma2
  for (i in 1:nb_iteration) {
    mu_emm_simule = estimation_EMM_wiener(list_X,list_T)[[1]]
    mu_emv_simule = estimate_all_wiener(list_X,list_T)[[1]]
    sigma2_emm_simule = estimation_EMM_wiener(list_X,list_T)[[2]]
    sigma2_emv_simule = estimate_all_wiener(list_X,list_T)[[2]]
    biais_mu_emv = biais_mu_emv + mu_emv_simule
    biais_mu_emm = biais_mu_emm + mu_emm_simule
    biais_sigma2_emv = biais_sigma2_emv + sigma2_emv_simule
    biais_sigma2_emm = biais_sigma2_emm + sigma2_emm_simule
    variance_mu_emm = variance_mu_emm + (mu_emm_simule -  mu_empirique)^2
    variance_mu_emv = variance_mu_emv + (mu_emv_simule - mu_empirique)^2
    variance_sigma2_emm = variance_sigma2_emm + (sigma2_emm_simule - sigma2_empirique)^2
    variance_sigma2_emv = variance_sigma2_emv + (sigma2_emv_simule - sigma2_empirique)^2
  }
  eqme_emm_mu = ((1/nb_iteration)*variance_mu_emm) + (((1/nb_iteration)*biais_mu_emm) - mu)^2
  eqme_emv_mu = ((1/nb_iteration)*variance_mu_emv) + (((1/nb_iteration)*biais_mu_emv) - mu)^2
  eqme_sigma2_emm = ((1/nb_iteration)*variance_sigma2_emm) + (((1/nb_iteration)*biais_sigma2_emm) - sigma2)^2
  eqme_sigma2_emv = ((1/nb_iteration)*variance_sigma2_emv) + (((1/nb_iteration)*biais_sigma2_emv) - sigma2)^2
  return (list(eqme_emm_mu, eqme_emv_mu,eqme_sigma2_emm,eqme_sigma2_emv))
}


####################### Partie Test ######################


# n<- 10
# nb<- 2
# dt<- 1
# 
# list_T <- periodique_T_random(n, dt, nb)
# list_T
# 
# 
# simu <- simulate_all_wiener(mu, sigma2, n ,nb,list_T)
# 
# plot_all_wiener(simu[[1]], simu[[2]])















