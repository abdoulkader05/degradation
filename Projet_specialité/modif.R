


library(reshape2)

dt <- read.csv("takeda.csv")
dt
for(i in 2: length(dt)){
  names(dt)[i] <- i-1
}
dtt <- melt(dt, id = c("time"))
names(dtt)[2] <- "indice"
names(dtt)[3] <- "obs"
write.csv(dtt,"takeda2.csv")

#p <- periodique_T(100, 0.1, 5)
#s <- simulate_all_wiener(1, 1, 100, 5, p)

#p <- periodique_T(500, 100, 15)
#s <- simulate_all_wiener(2, 4, 500, 15, p)



#p <- periodique_T_random(200, 100, 10)
#s <- simulate_all_wiener(2, 8, 200, 10 , p)

p <- exp_T(600, 10, 20)
s <- simulate_all_wiener(4, 0.1, 600, 20 , p)

### Gamma

#p <- periodique_T(50, 0.1, 10)
#s <- simulate_all_gamma(1, 1, 50, 10, p)

#p <- periodique_T_random(100, 100, 10)
#s <- simulate_all_gamma(3, 1, 100, 10, p)


list_X <- s[[1]]

list_T <- s[[2]]
data <- data.frame(1, list_T[[1]], list_X[[1]])
colnames(data) <- c("indice", "T", "X")
for (k in 2:length(list_X)){
 
  dt <- data.frame(k,list_T[[k]], list_X[[k]])
  colnames(dt) <- c("indice", "T", "X")
  data <- rbind(data, dt)
}
#write.csv(data, "test_wienner_periodique5.csv", row.names = FALSE)
#write.csv(data, "test_wienner_periodique15.csv", row.names = FALSE)
#write.csv(data, "test_wienner_non_periodique10.csv", row.names = FALSE)
write.csv(data, "test_wienner_exp20.csv", row.names = FALSE)
#write.csv(data, "test_gamma_periodique10.csv", row.names = FALSE)
#write.csv(data, "test_gamma_non_periodique10.csv", row.names = FALSE)