library(utils)
# Simule le processus de Wiener pour une courbe
simulate_wiener = function(mu, sigma2, n,T){
  T = sort(T)
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
    list_W[[k]]<-simulate_wiener(mu, sigma2, n,T[k,])
  }
  
  return (list(list_W, T))
}

affichage = function(W, T, num_curve, x_lim,y_lim ){
  
  if (num_curve == 1){
    plot(T,W, type = "l", xlab="time", main="Wiener Process", xlim = c(0, x_lim), ylim = c(0, y_lim))
  }else{
    lines(T,W, type = "l", xlab="time", main="Wiener Process", xlim = c(0, x_lim), ylim = c(0, y_lim))
  }
  
  
}

norm_T = function(n, moy, sdd, nb){
  mat_temps <- matrix(0, nrow = nb, ncol = n)
  vec_moy <- sample(1:100, size =nb, replace = FALSE )
  vec_moy <- c(moy, vec_moy)
  print(vec_moy)
  vec_sigma <- sample(3:20, size =nb, replace = FALSE )
  vec_sigma <- c(sdd, vec_sigma)
  print(vec_sigma)
  for(k in 1:nb){
    mat_temps[k,1] <- 0
    for (i in 2:n){
      mat_temps[k,i] <- rnorm(1, vec_moy[k], vec_sigma[k])
    }
    mat_temps[k ,2:n] = sort(mat_temps[k,2:n])
  }
  return (mat_temps)
}

periodique_T = function(n, dt, nb){
  T = 2:n
  mat_temps <- matrix(0, nrow = nb, ncol = n)
  vec_dt <- seq(0.1, 4, 0.1)
 list_dt <- sample(vec_dt, size = nb, replace = FALSE)
 list_dt <- c(dt, list_dt)
 for(k in 1:nb){
   mat_temps[k,1] <- 0
  for (i in 2:n){
    T[i] = (i)*list_dt[k]
   mat_temps[k,i] <- (i-1)*list_dt[k]
  }
 }

  return(mat_temps)
}
exp_T = function(n, lambda){
  T = rexp(n, rate = 3)
  T[0] = 0
  T = cumsum(T)
  T = sort(T)
  return (T)
}
gamma_T = function(n, a, b, nb){
  mat_temps <- matrix(0, nrow = nb, ncol = n)
  vec_a <- sample(1:100, size =nb, replace = FALSE )
  vec_a <- c(a, vec_a)

  vec_b <- sample(3:20, size =nb, replace = FALSE )
  vec_b <- c(b, vec_b)

  for(k in 1:nb){
    mat_temps[k,1] <- 0
    for (i in 2:n){
      mat_temps[k,i] <- rgamma(1, rate = vec_a[k], shape = vec_b[k])
    }
    mat_temps[k,2:n ] = sort(mat_temps[k,2:n ])
  }
  return (mat_temps)
}


affichage_all = function(list_w, T){
  nb = length(list_w)
  list_max_T <- c()
  list_max_W <- c()
  num_curve=1
  for (k in 1:nb){
    list_max_T <- c(list_max_T, max(T[k,]))
    list_max_W <- c(list_max_W, max(list_w[[k]]))
    affichage(list_w[[k]], T[k,], num_curve, max(list_max_T), max(list_max_W))
    num_curve=num_curve+1
  }
  
}


####################### Partie Test ######################

# res<- simulate_all_wiener(0, 1, 100 ,2, choix = 1)
# affichage_all(res[[1]], res[[2]])



