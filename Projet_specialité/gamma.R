source("wiener.R")
#on simule ici un processus de gamma homogène 

  simulation_gamma <- function(a, b, n, T){
    X = 1:n+1
    T <- as.numeric(T)
    X[1] = 0
    for (i in 1:n+1){
      new_a  <- a*(T[i] - T[i-1])
      X[i] = X[i-1] + rgamma(1,new_a ,b )
    }
  return (X)
}


affichage_gamma = function(X, T, num_curve, x_lim, y_lim){
  
  if (num_curve == 1){
    plot(T,X, type = "l", xlab="time", main="Gamma Process", col = 1:num_curve, xlim = c(0, x_lim), ylim = c(0, y_lim))
  }else{
    lines(T,X, type = "l", xlab="time", main="Gamma Process", col = 1:num_curve, xlim = c(0, x_lim), ylim = c(0, y_lim))
  }
}
  





simulate_all_gamma = function(a, b, n ,nb,list_T){

  list_X = list()
  
  # Cas ou on simule des dt périodiques
  for (k in 1:nb){
    list_X[[k]]<-simulation_gamma(a, b, n,list_T[[k]])
  }
  return (list(list_X,list_T))
}


affichage_all_gamma = function(list_X, list_T){
  nb = length(list_X)
  x_max = 0
  y_max = 0
  num_curve=1
  for (i in 1:nb){
    if (max(unlist(list_T[[i]])) >= x_max ){
      x_max = max(unlist(list_T[[i]]))
    }
    if ( max(unlist(list_X[[i]])) >= y_max ){
      y_max = max(unlist(list_X[[i]]))
    }
  }
  for (k in 1:nb){
    affichage_gamma(list_X[[k]], list_T[[k]], num_curve, x_max, y_max)
    num_curve=num_curve+1
  }
}



compute_delta = function(W, T){ 
  
  n=length(W)
  delta_T <- 1:n
  delta_W<- 1:n
  delta_W[1] = as.numeric(W[[1]][1])
  delta_T[1] = as.numeric(T[[1]][1])
  
  for (i in 2:n){
    delta_T[i] = as.numeric(T[[1]][i]) - as.numeric(T[[1]][i-1])
    delta_W[i]=as.numeric(W[[1]][i]) - as.numeric(W[[1]][i-1])
    
  }
  
  return (list(delta_W,delta_T))
  
}

estimation_EMV = function(list_X,T){
  nb_curves<- length(list_X)
  somme=0
  somme_delta=0
  for( i in 1:nb_curves) {
    X = list_X[[i]]
    deltas <- compute_delta(X, T)
    delta_X <- deltas[[1]]
    n=length(delta_X)
    delta_T <- deltas[[2]]
    somme_delta=somme_delta+ sum(log(delta_X)*delta_T)
    somme= somme + X[n]
    
  }
    equation = function(x){return(nb_curves*sum(delta_T)*log(nb_curves*x*as.numeric(T[[1]][n])/somme)  + somme_delta - nb_curves*sum(delta_T*(digamma(delta_T*x))) )}
    a=uniroot(equation,c(1,1000))$root
    return(list(a,nb_curves*a*as.numeric(T[[1]][n])/somme))
  
}

estimation_gen_gamma <- function(list_X, T){
  nb_curves <- length(list_X)
  somme_digamma_final = 0
  somme_increment_final = 0
  somme_mix_final = 0
  somme_time_final = 0
  for (k in 1:nb_curves){
    somme_mix = as.numeric(T[[k]][1])*log(as.numeric(list_X[[k]][1]))
    somme_time = as.numeric(T[[k]][1])
    somme_increment = list_X[[k]][1]
    for (i in 2:length(list_X[[k]])){
      somme_increment = somme_increment + as.numeric(list_X[[k]][i]) - as.numeric(list_X[[k]][i-1])
      somme_time = somme_time +as.numeric(T[[k]][i]) - as.numeric(T[[k]][i-1])
      somme_mix = somme_mix + (as.numeric(T[[k]][i])-as.numeric(T[[k]][i-1]))*log(as.numeric(list_X[[k]][i]) - as.numeric(list_X[[k]][i-1]))
    }
    somme_time_final = somme_time_final + somme_time
    somme_mix_final = somme_mix_final + somme_mix
    somme_increment_final = somme_increment_final + somme_increment
    
  }
  
    equation = function(x){ 
      for (k in 1:nb_curves){
        somme_digamma = 0
        for (i in 2:length(list_X[[k]])){
          somme_digamma = somme_digamma + (as.numeric(T[[k]][i]) - as.numeric(T[[k]][i-1]))*(digamma(x*(as.numeric(T[[k]][i]) - as.numeric(T[[k]][i-1]))) -log(x))
        }
        somme_digamma_final = somme_digamma_final + somme_digamma
      }
      return (somme_digamma_final  - somme_time_final * log(somme_time_final/somme_increment_final) + somme_mix_final)
    }
    
    a_estimated=uniroot(equation,lower = -1000 , upper = 1000 )$root
    b_estimated = a_estimated * (somme_time_final/somme_increment_final)
    
    return (list(a_estimated, b_estimated))
}

estimation_EMV9 = function(X,T){
    n=length(X)
    deltas <- compute_delta(X, T)
    delta_X <- deltas[[1]]
    delta_T <- deltas[[2]]
    print(X[n])
    print(sum(log(delta_X)*delta_T))
    equation = function(x){return(sum(delta_T)*log(x*T[n]/X[n])  + sum(log(delta_X)*delta_T) - sum(delta_T*(digamma(delta_T*x))) )}
    a=uniroot(equation,c(1,1000))$root
    return(a)
  
}

estimation_EMM_gen = function(list_X, list_T){

  nb_curves <- length(list_X)
  n <- length(list_X[[1]])
  S_mu <- 0
  S_sig <- 0
  somme_observation = 0
  for (k in 1:nb_curves){
    #somme_div_mu <- 0 
    somme_div_mu <- as.numeric(list_X[[k]][1])/as.numeric(list_T[[k]][2])
    for (i in 2:length(list_X[[k]])){
      somme_div_mu <- somme_div_mu + (as.numeric(list_X[[k]][i]) - as.numeric(list_X[[k]][i-1]))/(as.numeric(list_T[[k]][i]) - as.numeric(list_T[[k]][i-1]))
    }
    somme_observation = somme_observation + length(list_X[[k]])
    S_mu = S_mu+ somme_div_mu
  }
  mu_s <-S_mu/somme_observation
  for (k in 1:nb_curves){
    #somme_div_sig <-0 
    somme_div_sig <-((as.numeric(list_X[[k]][1]) - as.numeric(list_T[[k]][1])*mu_s)^2)/as.numeric(list_T[[k]][2])
    for (i in 2:length(list_X[[k]])){
      Y_ij <- as.numeric(list_X[[k]][i]) - as.numeric(list_X[[k]][i-1]) 
      s_ij <- as.numeric(list_T[[k]][i]) - as.numeric(list_T[[k]][i-1])
      somme_div_sig <- somme_div_sig + ((Y_ij - s_ij*mu_s)^2)/s_ij
    }
    S_sig <- S_sig + somme_div_sig
  }
  sig_s <- S_sig / somme_observation
  a_EMM <- mu_s^2/sig_s
  b_EMM <- mu_s /(sig_s)
  return (list(a_EMM, b_EMM))
}

estimation_EMM = function(list_X,T){
  nb_curves<- length(list_X)
  m=1:nb_curves
  v=1:nb_curves
  for( i in 1:nb_curves) {
    X = list_X[[i]]
    deltas <- compute_delta(X, T)
    delta_X <- deltas[[1]]
    delta_T <- deltas[[2]]
    m[i]=mean(delta_X/delta_T)
    v[i]=var(delta_X/sqrt(delta_T))
    
  }
    return(list(mean(m^2/v),mean(m/v)))
}

biais_error_gamma = function(a,b,nb_data,nb_traj,temps,iteration) {
  data = matrix(0,nrow = iteration, ncol = 2)
  for(i in 1:iteration) {
    
    data[i,1] = estimation_EMM_gen(simulate_all_gamma(a, b, nb_data ,nb_traj,temps)[[1]],temps)[[1]] -a 
    data[i,2] = estimation_EMM_gen(simulate_all_gamma(a, b, nb_data ,nb_traj,temps)[[1]],temps)[[2]] -b
    
  }
  par(mfrow = c(1,2))
  print(data[,1])
  hist(data[,1], probability = TRUE, main = "biais_de_a_EMM", xlab="Erreur")
  curve(dnorm(x,mean = mean(data[,1]), sd = sd(data[,1])), add = TRUE, col = 'red')
  hist(data[,2], probability = TRUE, main = "biais_de_b_EMM", xlab="Erreur")
  curve(dnorm(x,mean = mean(data[,2]), sd = sd(data[,2])), add = TRUE, col = 'blue')
}
plot_exced = function(seuil, number_of_sim, n, T, liste_W){
  h = c()
  for ( i in 1:number_of_sim){
    W = liste_W[i]
    W = W[[1]]
    for (i in 1:n){
      if ( W[i] > seuil){
        h = c(h,T[i])
        break;
        
      }
    }
  }
  return(h)
}
histoeff <- function(x, xlim=NULL, col=NULL, ...)
{
sx <- sort(x)
n <- length(x)
k <- round(log(n)/log(2)+1)
rangex <- max(x)-min(x)
breaks <- c(min(x)-0.025*rangex, quantile(x, seq(1, k-1)/k), max(x)+0.025*rangex)
col <- 0
if (is.null(xlim)) xlim<-c(breaks[1], breaks[k+1])
hist(x, breaks=breaks, col=col, xlim=xlim, probability=T, ...)
}

plot_density_exceeding_gamma<- function(seuil, a, b){
  
  f = function(x){pgamma(seuil*b,shape=a*x,scale=1)}

integrate(f, lower=0, upper=Inf)
}

time_exceeding_gamma = function(seuil, list_T, list_X){
  
  number_of_sim <- length(list_X)
  h = c()
  
  for ( k in 1:number_of_sim){
    W = list_X[[k]]
    T = list_T[[k]]
    
    n<- length(W)
    
    for (i in 1:n){
      if ( W[i] > seuil){
        h = c(h,T[i])
        break;
        
      }
    }
  }
  
  if (length(h) >0){
    return(mean(h))
  }else{
    return("None observed")
  }
}


histoeff <- function(x, xlim=NULL, col=NULL, ...)
{
  sx <- sort(x)
  n <- length(x)
  k <- round(log(n)/log(2)+1)
  rangex <- max(x)-min(x)
  breaks <- c(min(x)-0.025*rangex, quantile(x, seq(1, k-1)/k), max(x)+0.025*rangex)
  col <- 0
  if (is.null(xlim)) xlim<-c(breaks[1], breaks[k+1])
  hist(x, breaks=breaks, col=col, xlim=xlim, probability=T, ...)
}

first_instant_exceeding_gamma<- function(seuil, a, b){
  
  f = function(x){pgamma(seuil*b,shape=a*x,scale=1)}
  
  integrate(f, lower=0, upper=Inf)$value
  
}


# plot_exced_gamma = function(seuil,list_T, liste_W, a, b){
#   h = c()
#   
#   number_of_sim <- length(liste_W)
#   for ( k in 1:number_of_sim){
#     
# 
#     W = liste_W[[k]]
#     
#     T = list_T[[k]]
#     
#     n<- length(T)
#     
#     
#     for (i in 1:n){
#       if ( W[i] > seuil){
#         h = c(h, as.numeric(T[i]))
#         break;
#         
#       }
#     }
#   }
#   
#   
#   
#   s = sort(h)
#   taille = length(h)
#   k_sturges = round(log2(taille)+1)
#   a_inf = s[1]-0.025*(s[taille]-s[1])
#   a_sup = s[taille]+0.025*(s[taille]-s[1])
#   borne = c(a_inf,quantile(h,seq(1,k_sturges-1)/k_sturges),a_sup)
#   hist(h, probability = TRUE, breaks = borne)
#   
#   
#   f = function(x){dgamma(seuil*b,shape=a*x,scale=1)}
#   
#   n<- length(list_T[[1]])
# 
#   
#   curve(f, from = 0, to = as.numeric(list_T[[1]][n]), add = TRUE, col = 'blue')
#   
#   
#   
# }

#############################__test___#############################

# n<- 10
# nb<- 2
# dt<- 1
# 
# mu<- 1
# sigma2 <- 1
# 
# list_T <- periodique_T(n, dt, nb)
# 
# 
# simu <- simulate_all_wiener(mu, sigma2, n ,nb,list_T)
# 
# list_W <- simu[[1]]
# simu
# 
# t <- time_exceeding_gamma(5, list_T, list_W)
# t









