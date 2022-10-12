library(utils)
# Simule le processus de Wiener pour une courbe
simulate_wiener = function(mu, sigma2, n,T){
  
  m = mu*T[1]
  sdd = sqrt(sigma2 * T[1])
  W = 1:n
  W[1] = 0
  for (i in 2:n){
    m = mu*(T[i] - T[i-1])
    sdd = sqrt(sigma2 * (T[i] - T[i-1]))
    W[i] = W[i-1] + rnorm(1, mean = m, sd = sdd)
  }
  return (W)
}

# Simule le processus de Wiener pour plusieures courbes
simulate_all_wiener = function(mu, sigma2, n ,nb,T){
  
  #T = switch(choix, periodique_T(n, dt), exp_T(n, lambda),gamma_T(n, a, b), norm_T(n, m, sigma))
  
  list_W = list()
  
  for (k in 1:nb){
    list_W[[k]]<-simulate_wiener(mu, sigma2, n,T)
  }
  
  return (list(list_W, T))
}

affichage = function(W, T, num_curve, x_lim, y_lim){
  
  if (num_curve == 1){
    plot(T,W, type = "l", xlab="time", main="Wiener Process", xlim = c(0, x_lim))
  }else{
    lines(T,W, type = "l", xlab="time", main="Wiener Process", ylim = c(0, y_lim))
  }
  
  
}

norm_T = function(n, moy, sdd){
  T = rnorm(n, mean = moy, sd= sdd)
  T[0] = 0
  T = cumsum(T)
  return (T)
}
periodique_T = function(n, dt){
  T = 1:n
 
  for (i in 1:n){
    T[i] = (i)*dt
  }

  return(T)
}
exp_T = function(n, lambda){
  T = rexp(n, rate = 3)
  T[0] = 0
  T = cumsum(T)
  return (T)
}
gamma_T = function(n, a, b){
  T = rgamma(n, rate = a, shape = b)
  T[0] = 0
  T = cumsum(T)
  return (T)
}


affichage_all = function(list_w, T){
  nb = length(list_w)
  
  num_curve=1
  for (W in list_w){
    
    affichage(W, T, num_curve)
    num_curve=num_curve+1
  }
  
}


####################### Partie Test ######################

# res<- simulate_all_wiener(0, 1, 100 ,2, choix = 1)
# affichage_all(res[[1]], res[[2]])



