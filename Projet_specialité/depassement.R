library("statmod")
source("wiener.R")
source("estimation.R")

plot_exced_wiener = function(seuil, number_of_sim, n,T, mu, sigma2, liste_W){
  s = c()
  for ( k in 1:number_of_sim){
    W = liste_W[[k]]
    
    for (i in 1:n){
      
      if ( W[i] > seuil){
        s = c(s,T[[k]][i])
        break;
        
      }
    }
  }
  
  a = mu
  mu_estimated = estimate_all_wiener(liste_W, T)[[1]]
  sigma2_estimated = estimate_all_wiener(liste_W, T)[[2]]
  
  mu = mean(s)
  landa = 2*sum(((s-mu)**2)/((2*s)*(mu ** 2)))/n
  s = sort(s)
  hist(s, probability = TRUE)
  curve(dinvgauss(x , mean = mu , dispersion = landa ), from = 0, to = T[[1]][n], add = TRUE, col = 'red')
  curve(dinvgauss(x , mean = seuil/a , dispersion = sigma2/(seuil ** 2)), from = 0, to = T[[1]][n], add = TRUE, col = 'orange')
  curve(dinvgauss(x , mean = seuil/mu_estimated , dispersion = sigma2_estimated/(seuil ** 2)), from = 0, to = T[[1]][n], add = TRUE, col = 'blue') #debug
  return(seuil/mu_estimated)
  
}



first_instant_exceeding_wiener<- function(seuil, mu){
  
  return(seuil/mu)
}


plot_exced_gamma = function(seuil, number_of_sim, n,T,liste_W){
  s = c()
  for ( k in 1:number_of_sim){
    W = liste_W[[k]]
    
    for (i in 1:n){
      
      if ( W[i] > seuil){
        s = c(s,T[[k]][i])
        break;
        
      }
    }
  }
  
  s = sort(s)
  hist(s, probability = TRUE)
  
}


############################################################PARTIE TEST#############################"

# seuil = 10
# number_of_sim = 500
# n = 1000
# mu = 1
# sigma2 = 1
# T <- periodique_T(n, 0.1, number_of_sim)
# simu <- simulate_all_wiener(1,1, n ,number_of_sim, T)
# list_W = simu[[1]]
# T =  simu[[2]]
# plot_exced_wiener(seuil, number_of_sim, n, T, mu, sigma2, list_W  )
