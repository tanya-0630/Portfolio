print("Tanya Fitkiriwala")

  Annual_Demand<- 15000
  Unit_price <- 80
  unit_host_price <- 18 / 100
  Supplier_price <- 220
  #Using the formula to generate the inventry level Q
  Level_inventory <- round(sqrt(((2 * Annual_Demand * Supplier_price) / (Unit_price *
                                                                       unit_host_price))),0)
  Level_inventory
  
  
  #double inventory calculation
  Level_inventory_2 <- 2 * Level_inventory
  Level_inventory_2
  
  #calculating order number
  Order_Num <- round(Annual_Demand / Level_inventory_2,0)
  Order_Num
  
  #annual order cost
  Annual_OrderPrice<- Supplier_price * (Annual_Demand/Level_inventory_2)
  Annual_OrderPrice
  
  
  #annual holding cost
  Annual_HoldCost <- (Level_inventory_2/2) * Unit_price * unit_host_price
  Annual_HoldCost
  
  
  #total order cost 
  TotalPrice_In<- Annual_OrderPrice + Annual_HoldCost
  TotalPrice_In
  
  #Random Sequence for inventory
  InvtLvl_Q <- seq(100, 7500, by = 100)
  InvtLvl_Q
  InvtLvl_Q2 <- InvtLvl_Q * 2
  InvtLvl_Q2
  
  Cost_Q <-
    data.frame(
      InvtLvl_Q,
      InvtLvl_Q2
    )
  Cost_Q$Total_Price <- Supplier_price * (Annual_Demand/InvtLvl_Q2) + (Unit_price *
                                                                         unit_host_price * InvtLvl_Q)
  min(Cost_Q$Total_Price)
  Cost_Q$InvtLvl_Q[which.min(Cost_Q$Total_Price)]
  
  library(ggplot2)
  ggplot(Cost_Q, aes(x=InvtLvl_Q2, y=Total_Price)) +
    geom_point(alpha=0.5)+ labs(title = 'Quantity vs Total Cost')
  plot(Cost_Q$InvtLvl_Q2, Cost_Q$Total_Price)
  
  

  #PART 2Value
  #numcases <- 1000
  library(triangle)
  min_x_value <- 13000
  max_x_value<-17000
  Peak_x_value <- 15000
  random <- rtriangle(1000, a = 13000, b = 17000, c = 15000)
  random <- (runif(1000))
  random <- round(random,2)
  random
  x <- (Peak_x_value - min_x_value) / (max_x_value - min_x_value)
  x
  options(scipen = 999)
  m <- (max_x_value - min_x_value) * (Peak_x_value - min_x_value)
  m
  n <- (max_x_value - min_x_value) * (max_x_value - Peak_x_value)
  n
  
  ax = min_x_value + sqrt(random*m)
  ax
  ay = max_x_value - sqrt((1-random)*n)
  Annual_x_simul <- ifelse(ax <= x, ax, ay)

    Annual_x_simul
  Annual_x_simul <- round(Annual_x_simul,0)
  summary(Annual_x_simul)
  round(sd(Annual_x_simul),0)
  Annual_x_2 <- Annual_x_simul
  Annual_x_2
  Simulation_Inv_Lvl <- round(sqrt((2 * Annual_x_2 * Supplier_price) / (Unit_price *
                                                                              unit_host_price)),0)
  Simulation_Inv_Lvl
  Simulation_Inv_Lvl_2 <- 2 * Simulation_Inv_Lvl
  Simulation_Inv_Lvl_2
  Simulation_Order_N <- round(Annual_x_2 / Simulation_Inv_Lvl_2,2)
  Simulation_Order_N
  Simulation_A_Ordering_Price <- Supplier_price *
    (Annual_x_2/Simulation_Inv_Lvl_2)
  Simulation_A_Ordering_Price
  Simulation_A_Holding_Price <- Simulation_Inv_Lvl * Unit_price * unit_host_price
  Simulation_A_Holding_Price
  Simulation_Total_Inv_Price <- Simulation_A_Ordering_Price +
    Simulation_A_Holding_Price
  Simulation_Total_Inv_Price
  simulated_v <-
    data.frame(
      x,
      Annual_x_2,Simulation_Total_Inv_Price,Simulation_Inv_Lvl_2,Simulation_Order_N
    )
  summary(simulated_v)
  View(simulated_v)
  
  
  #sd(simulated_value)
  #Total inventory cost
  
  TotalCost_Inventory<-
    rnorm(n=1000,m=mean(simulated_v$Simulation_Total_Inv_Price),sd=sd(simulated_v$Simulation_Total_Inv_Price))
  
  
  hist(simulated_v$Simulation_Total_Inv_Price,freq=F,main=" Total cost
Distribution",col= "orange", xlab = 'Total Inventory Price', ylab = 'Frequency')
  lines(density(simulated_v$Simulation_Total_Inv_Price), col = 4, lwd = 2)
  
  t_test <- t.test(TotalCost_Inventory, conf.level = 0.95)
  t_test$conf.int[1] #lower bound
  t_test$conf.int[2] #upper bound
  
  
  ### Order Quantity plot
  order_Qty<-
    rnorm(n=1000,m=mean(simulated_v$Simulation_Inv_Lvl_2),sd=sd(simulated_v$
                                                                         Simulation_Inv_Lvl_2))
  #ord <-  rnorm(n=1000,m=mean(simulated_v$Simulation_Order_N),sd=sd(simulated_v$Simulation_Order_N))
  #hist(simulated_v$Simulation_Order_N,freq=F,main=" Order Quantity",col='violet',xlab = 'Order Quantity')
  
  hist(simulated_v$Simulation_Inv_Lvl_2,freq=F,main=" Order Quantity",col='violet',
       xlab = 'Order Quantity')
  lines(density(order_Qty), col = 2, lwd = 2)
  lines(density(simulated_v$Simulation_Inv_Lvl_2), col = 4, lwd = 2)
  
  t_test2 <- t.test(order_Qty, conf.level = 0.95)
  t_test2$conf.int[1] #lower bound
  t_test2$conf.int[2] #upper bound

  simulated_v$Simulation_Order_N
  
 ##annual demand
  Ann_Dem <- rnorm(n=1000,m=mean(simulated_v$Annual_x_2),sd=sd(simulated_v$Annual_x_2))
  
  hist(simulated_v$Annual_x_2, freq = F, main = 'Annual Orders', col = 'red', xlab = 'Annual no. of orders')
  lines(density(simulated_v$Annual_x_2), col = 8, lwd = 2)
  
  t_test3 <- t.test(Ann_Dem, conf.level = 0.95)
  t_test3$conf.int[1] #lower bound
  t_test3$conf.int[2] #upper bound
