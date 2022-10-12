#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("ggplot2")
library("statmod")



source("depassement.R")
source("wiener.R")
source("estimation.R")
source("gamma.R")


# Define server logic

shinyServer(function(input, output) {
  
  ################## Etat initial
  output$initialCurve <- renderPlot({
    T <- periodique_T(1000, 0.1, 5)
    simu <- simulate_all_wiener(1,1, 1000 ,5, T)
    
    list_W = simu[[1]]
    T =  simu[[2]]
    plot_all_wiener(list_W, T)
    abline(80, 0, col = 'red')
  })
  
  output$initial_instant_exceeding_wiener<- renderText({
    80
  })

  
 
  ###########################################
  
simu<- eventReactive(input$buttonParameters, {
  
  if (input$check == FALSE ){
    if (input$dtchoix == "Periodic"){
      T = periodique_T(input$n, input$deltat, input$nb)
      }
    
    if (input$dtchoix == "Exponential"){
      T <- exp_T(input$n, input$Lambda, input$nb)
     
    }
    if (input$dtchoix == "Gamma"){
      T <- gamma_T(input$n, input$aa, input$bb, input$nb)
    }
    if (input$dtchoix == "Normal"){
      T <- norm_T(input$n, input$mean, sqrt(input$sigma_carre), input$nb)
    }
  }else{
    if (input$dtchoix == "Periodic"){
      T = periodique_T_random(input$n, input$deltat, input$nb)
    }
    
    if (input$dtchoix == "Exponential"){
      T <- exp_T_random(input$n, input$Lambda, input$nb)
      
    }
    if (input$dtchoix == "Gamma"){
      T <- gamma_T(input$n, input$aa, input$bb, input$nb)
    }
    if (input$dtchoix == "Normal"){
      T <- norm_T_random(input$n, input$mean, sqrt(input$sigma_carre), input$nb)
    }
  }
    if (input$ModelChoice == "Gamma Process"){
      simulation <- simulate_all_gamma(input$a, input$b, input$n , input$nb, T)
    }
    if ( input$ModelChoice == "Wiener Process"){
      simulation <- simulate_all_wiener(input$mu, input$sigma2, input$n, input$nb, T)
    }
    
  
    return(simulation)
  })
  
  simu2<- eventReactive(input$buttonParameters2, {
    
    if (input$dtchoix2 == "Periodic"){
      T = periodique_T(input$n2, input$deltat2, input$nb2)
    }
    
    if (input$dtchoix2 == "Exponential"){
      T <- exp_T(input$n2, input$Lambda2 )
      
    }
    if (input$dtchoix2 == "Gamma"){
      T <- gamma_T(input$n2, input$aa2, input$bb2, input$nb2)
    }
    if (input$dtchoix2 == "Normal"){
      T <- norm_T(input$n2, input$mean2, input$sigma_carre2, input$nb2)
    }
    if (input$ModelChoice == "Gamma Process"){
      simulation <- simulate_all_gamma(input$a2, input$b2, input$n2 , input$nb2, T)
    }
    if ( input$ModelChoice == "Wiener Process"){
      simulation <- simulate_all_wiener(input$mu2, input$sigma22, input$n2, input$nb2, T)
    }
    
    return(simulation)
  })
  
 outlist <- eventReactive(input$buttonParameters, {
   
   if (input$ModelChoice == "Wiener Process"){
     out = list(input$seuil, input$nb, input$n, input$deltat, input$mu, input$sigma2)
   }
   
   if (input$ModelChoice == "Gamma Process"){
     out = list(input$seuil, input$a, input$b,input$nb, input$n)
   }
    
    return(out)
  })
 
 outlist2 <- eventReactive(input$buttonParameters2, {
   if (input$ModelChoice2 == "Wiener Process"){
     out =list(input$mu2, input$sigma22, input$n2, input$nb2)
   }
   
   if (input$ModelChoice2 == "Gamma Process"){
     out = list(input$a2, input$b2, input$n2, input$nb2)
   }
   
   return(out)
 })
 
 parameters_to_print<- eventReactive(input$buttonParameters, {
   
   if (input$ModelChoice == "Gamma Process"){
     return(list("a", "b"))
   }
   
   if (input$ModelChoice == "Wiener Process"){
     return(list("mu", "sigma2"))
   }
   
 })
 
  output$WienerCurve <-renderPlot({ 
    
    list_W = simu()[[1]]
    T = simu()[[2]]
    plot_all_wiener(list_W, T)
    abline(input$seuil, 0, col = 'red')
    
  })
  
  output$GammaCurve <- renderPlot({
    list_W = simu()[[1]]
    T = simu()[[2]]
    affichage_all_gamma(list_W, T)
    abline(input$seuil, 0, col = 'red')
  })
  
  output$Graph_of_exceeding <- renderPlot({
    list_param = outlist()
    seuil = list_param[[1]]
    number_of_sim = list_param[[2]]
    n = list_param[[3]]
    dt = list_param[[4]]
    mu = list_param[[5]]
    sigma2 = list_param[[6]]
    list_W <- simu()[[1]]
    T<- simu()[[2]]
    plot_exced_wiener(seuil, number_of_sim , n, T, mu, sigma2, list_W)
  })
  
  output$Graph_of_exceeding_gamma <- renderPlot({
    list_param = outlist()
    
    seuil = list_param[[1]]
    
    number_of_sim = list_param[[4]]
    n = list_param[[5]]
    
    list_W <- simu()[[1]]
    T<- simu()[[2]]
    plot_exced_gamma(seuil, number_of_sim , n, T, list_W)
  })
  
  output$Title_true_mean <- renderText({
    
    "True mean of the time of exceeding the threshold:"
  })
  
  output$first_instant_exceeding_wiener_theory <- renderText({
    
    seuil<- outlist()[[1]]
    
    mu <- outlist()[[5]]
    
    first_instant<-first_instant_exceeding_wiener(seuil, mu)
    first_instant
  })
  
  output$Title_estimated_mean <- renderText({
    
    "Estimated mean of the time of exceeding the threshold:"
  })
  
  output$first_instant_exceeding_wiener <- renderText({
    
    simulation <- simu()
    
    list_X <- simulation[[1]]
    list_T <- simulation[[2]]
    
    seuil<- outlist()[[1]]
    
    mu_estimated = estimate_all_wiener(list_X,list_T)[[1]]
    
    first_instant<-first_instant_exceeding_wiener(seuil, mu_estimated)
    first_instant
  })
  
  output$first_instant_exceeding_gamma <- renderText({
    list_param = outlist()
    
    seuil <- list_param[[1]]
    a <- list_param[[1]]
    b <- list_param[[2]]
    
    first_instant<-first_instant_exceeding_gamma(seuil, a, b)
    first_instant
  })
  
  output$first_instant_exceeding_observed <- renderText({
    
    simulation <- simu()
    
    list_X <- simulation[[1]]
    list_T <- simulation[[2]]
    
    seuil<- outlist()[[1]]
    
    mean_times <- time_exceeding_gamma(seuil, list_T, list_X)
    return(mean_times)
  })
  
  
  output$Table_estimator <- renderTable({
    list_W <- simu()[[1]]
    T<- simu()[[2]]
    m<- matrix(0, 2, 3)
    m[1, 1] <- "EMV"
    m[2,1] <- "EMM"
    m[1, 2] <- round(estimate_all_wiener(list_W, T)[[1]], 5)
    m[1, 3] <- round(estimate_all_wiener(list_W,T)[[2]], 5)
    m[2, 2] <- round(estimation_EMM_wiener(list_W, T)[[1]], 5)
    m[2, 3] <- round(estimation_EMM_wiener(list_W,T)[[2]], 5)
    b <- data.frame(m)
    colnames(b) <- c("Estimations", "mu", "sigma2")
    b
  }, type = "html", bordered = TRUE, striped = TRUE, align = "c", width = "200%",
  )

  
  output$a_simu<- renderText({
    list_W <- simu()[[1]]
    T<- simu()[[2]]
    round(estimation_EMM_gen(list_W, T)[[1]], 3)
    
  })
  
  output$b_simu<- renderText({
    list_W <- simu()[[1]]
    T<- simu()[[2]]
    round(estimation_EMM_gen(list_W, T)[[2]], 3)
    
  })
  
  output$First_parameter<-renderText({
    parameters_to_print()[[1]]
  })
  
  output$Second_parameter<-renderText({
    parameters_to_print()[[2]]
  })
  
  
  
  observeEvent(input$buttonParameters, {
    hide("initialCurve")
    hide("initial_first_parameter")
    hide("initial_second_parameter")
    hide("initial_instant_exceeding_wiener")
    
    if (input$ModelChoice == "Gamma Process"){
      hide("Table_estimator")
      hide("WienerCurve")
      hide("Graph_of_exceeding")
      hide("first_instant_exceeding_wiener")
      hide("first_instant_exceeding_wiener_theory")
      hide("Title_true_mean")
      hide("Title_estimated_mean")
      
      show("GammaCurve")
      show("a_simu")
      show("b_simu")
      show("First_parameter")
      show("Second_parameter")
      show("Graph_of_exceeding_gamma")
      
    }
    if ( input$ModelChoice == "Wiener Process"){
      
      hide("GammaCurve")
      hide("Graph_of_exceeding_gamma")
      hide("First_parameter")
      hide("Second_parameter")
      hide("a_simu")
      hide("b_simu")
      hide("first_instant_exceeding_gamma")
      show("Table_estimator")
      show("WienerCurve")
      show("Graph_of_exceeding")
      
      show("first_instant_exceeding_wiener")
      show("first_instant_exceeding_wiener_theory")
      show("Title_true_mean")
      show("Title_estimated_mean")
      
      
      
    }
  })
  
  observeEvent(input$buttonParameters2, {
    
    if (input$ModelChoice2 == "Gamma Process"){
     
      hide("BiaisWiener")
      show("BiaisGamma")
      
    }
    if ( input$ModelChoice2 == "Wiener Process"){
      
      hide("BiaisGamma")
      show("BiaisWiener")
    }
  })
  
  
  output$BiaisWiener <-renderPlot({ 
    list_X <- simu2()[[1]]
    list_T = simu2()[[2]]
    
    list_param = outlist2()
    
    mu = list_param[[1]]
    sigma2 = list_param[[2]]
    
    nb_data = list_param[[3]]
    nb_traj = list_param[[4]]
   
    
    biais_error_wienner(mu, sigma2, nb_data, nb_traj, list_T, iteration= 1000)
    
  })
  
  
  output$BiaisGamma <-renderPlot({ 
    
    list_T = simu2()[[2]]
    
    list_param = outlist2()
    
    a = list_param[[1]]
    b = list_param[[2]]
    
    nb_data = list_param[[3]]
    nb_traj = list_param[[4]]
    
    biais_error_gamma(a,b,nb_data,nb_traj,list_T,iteration=1000)
    
  })
  
  
  
  
  
  ############################## Second Page "Data" #################
  
  ## Plot the data
  filecsv <- reactive({
    req(input$file1)
    pathh <- input$file1
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return (df)
  })
  
  
 
  

  output$dataCurve<- renderPlot({
    
    data <- filecsv()
    
    n <- length(unique(data[,"indice"]))
    
    
    T1 <- data[data["indice"] == 1, "T"]
    X1 <- data[data["indice"] == 1, "X"]
    
    Tmin = min(data[,"T"]) 
    Tmax = max(data[,"T"]) 
    
    Xmin = min(data[,"X"]) 
    Xmax = max(data[,"X"]) 
    
    plot(T1, X1, xlab="T", ylab = "X", type="l", main="Evolution of the degradation", 
         xlim=c(Tmin, Tmax), ylim=c(Xmin, Xmax))
   
    
    # Case where we don't have only one path
    
    Ti<- NULL
    Xi<-NULL
    
    if (n>1){
      
      for (i in 2:n){
        
        Ti <- data[data["indice"] == i, "T"]
        Xi <- data[data["indice"] == i, "X"]
        
        lines(Ti, Xi)
        
      }
      
    }
  

  })
  
  recuperer_T_list_X<- function(){
    
    data <- filecsv()
    
    n <- length(unique(data[,"indice"]))
    
    list_T<-list()
    list_X<-list()
  
    k<-1
    
    for (i in 1:n){
    
      list_T[[k]] <- data[data["indice"] == i, "T"]
      list_X[[k]] <- data[data["indice"] == i, "X"]
        
      k <- k + 1
      
    }
   
   
    return(list(list_X, list_T))
    
    
  }
  

  
  output$mu_value<-renderText({
    
    data <- recuperer_T_list_X()
    
    list_T <- data[[1]]
    list_X <- data[[2]]
    
    round(estimate_all_wiener(list_T, list_X)[[1]], 8)

  })

  output$sigma2_value<- renderText({


    data <- recuperer_T_list_X()
    list_T <- data[[1]]
    list_X <- data[[2]]

 
    round(estimate_all_wiener(list_T, list_X)[[2]], 8)
  })
  
  output$a_value<-renderText({
    
    data <- recuperer_T_list_X()
    
    list_T <- data[[1]]
    list_X <- data[[2]]
    
    round(estimation_EMM_gen(list_T, list_X)[[1]], 8)
    
  })
  output$b_value<-renderText({
    
    data <- recuperer_T_list_X()
    
    list_T <- data[[1]]
    list_X <- data[[2]]
    
    round(estimation_EMM_gen(list_T, list_X)[[2]], 8)
    
  })
  output$prediction<-renderText({
    
    data <- recuperer_T_list_X()
    
    list_T <- data[[1]]
    list_X <- data[[2]]
    
    round(input$seuildata/estimate_all_wiener(list_T, list_X)[[1]], 8)
    
  })
  
})
