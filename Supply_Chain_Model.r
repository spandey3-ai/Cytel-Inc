#Function to generate Inventory by Uniform Distribution
generate_tablet= function(max_value){
  return(floor(runif(1,1,max_value)))
}



#Function to generate Sales Data by Poisson Distribution
generate_random=function(n,lambda){
  rv = floor(rpois(n,lambda))
  return(rv)
}


#Function to calculate to EOD inventory
EOD_inventory = function(stock_on_hand,orders_in_pipeline,inven){
  y=stock_on_hand + orders_in_pipeline
  #orders_in_pipeline = inven[i,j]-inven[i+Lead_Time,j]
  return(y)
}


#Function to calculate remaining enrollment
remaining_enrollment = function(sales,day,total_inventory){
  numsales = 0
  i=0
  for(i in 1:(day)){
    numsales=numsales+sales[i]
  }
  k1=total_inventory-numsales
  return (k1)
}

# Function for the inventory model
# cmpute_clean = function(dem, inven,supply, stockout, Lead_Time, restock_level,resupply_level,patient_count,total_inventory,patient_arrive) {
cmpute_clean = function(demand, Lead_Time, restock_level,resupply_level,total_inventory) {
  no_of_products = dim(demand)[2] 
  No_of_days = nrow(demand)
  
  stk_bod = data.frame(0, ncol = no_of_products, nrow = No_of_days)
  stk_eod = data.frame(0, ncol = no_of_products, nrow = No_of_days)
  y_at_eod = data.frame(0, ncol = no_of_products, nrow = No_of_days)
  ord_placed = data.frame(0, ncol = no_of_products, nrow = No_of_days)
  ord_qty = data.frame(0, ncol = no_of_products, nrow = No_of_days)
  ord_deliv_bod = data.frame(0, ncol = no_of_products, nrow = No_of_days)
  remain_enroll_eod = as.data.frame(matrix(0, ncol = no_of_products, nrow = No_of_days))
  stockout = as.data.frame(matrix(0, ncol = no_of_products, nrow = No_of_days))
  
  for(j in 1:no_of_products) {
    stk_bod[1,j] = resupply_level
    for(i in 1:No_of_days) {
      ord_deliv_bod[i,j] = 0
    }
    
    for( i in 1:No_of_days) {
      if (i > Lead_Time) {
        ord_deliv_bod[i,j] = ord_qty[i - Lead_Time, j]
      }
      if (i > 1) {
        if (remain_enroll_eod[i-1,j] > 0) {
          stk_bod[i,j] = stk_eod[i-1,j] + ord_deliv_bod[i,j]
        }
        else {
          stk_bod[i,j] = 0
        }
      }
      stk_eod[i,j] = stk_bod[i,j] - demand[i,j]
      if (i == 1) {
        remain_enroll_eod[i,j] =max(total_inventory-demand[i,j],0)
        y_at_eod[1,j] = stk_eod[1,j]
      }
      else {
        remain_enroll_eod[i,j] =max(remain_enroll_eod[i-1,j]-demand[i,j],0)
        cum_ord_qty = 0
        days_to_stkord=min(Lead_Time,i)
        
        for (k in 1:(days_to_stkord-1)) {
          cum_ord_qty = cum_ord_qty + ord_qty[i-k, j]
        }
        
        y_at_eod[i,j] = stk_eod[i,j] + cum_ord_qty
      }
      
      restock_chk1 = FALSE
      restocK_chk2 = FALSE
      #restocK_chk3 = FALSE
      if (y_at_eod[i,j] <= restock_level) {
        restock_chk1 = TRUE
      }
      if (y_at_eod[i,j] < remain_enroll_eod[i,j]) {
        restock_chk2 = TRUE
      }
      
      restock_ind = restock_chk1*restock_chk2
      ord_placed[i,j] = restock_ind
      if (restock_ind == FALSE) {
        ord_qty[i,j] = 0
      }
      else {
        qty = min(resupply_level, remain_enroll_eod[i,j])-y_at_eod[i,j]
        ord_qty[i,j] = qty
      }
      
      if (stk_eod[i,j] < 0) {
        stockout[i,j]= 1
      } 
      else {
        stockout[i,j]= 0
      }
      Stockout_per_run= colSums(stockout)
    }
  }
  return(list(stk_bod,ord_qty,stockout,Stockout_per_run))
}

# Function to compute overage
compute_overage = function(A,demand_dataframe,No_of_days,total_inventory,resupply_level){
  stk_bod= A[[1]]
  ord_qty =A[[2]]
  stockout = A[[3]]
  Stockout_per_run =A[[4]]
  no_of_products=1
  counter=0
  for (j in 1:no_of_products)
  {
    for (i in 1:No_of_days)
    {
      if(ord_qty[i,j]>0)
        counter=counter+1
    }
  }
  
  
  
  #}
  #totalsupply_data=sum(supply_data)
  #print(supply_data)
  #total = cbind(demand_dataframe,inv_data[,])
  #Stockout_per_run
  #No_of_days
  #print(total)
  #print(No_of_days)
  #print(totalsupply_data)
  #No of Consignments
  #Overage_Matrix=(((cumsum(supply_data)[nrow(supply_data),] -cumsum(demand_dataframe)[nrow(demand_dataframe),] ))/cumsum(demand_dataframe)[nrow(demand_dataframe),])*100
  #Overage_Matrix=((((cumsum(ord_qty)[nrow(ord_qty),]+resupply_level) -total_inventory))/total_inventory)*100
  #print(cumsum(demand_dataframe)[nrow(demand_dataframe),])
  #print("error")
  #print(total_inventory)
  Overage_Matrix=(((cumsum(demand_dataframe)[nrow(demand_dataframe),] -total_inventory))/total_inventory)*100
  return(list(OM=Overage_Matrix,ND=No_of_days,S=stockout,SR=A[[4]],C=counter))
  #return(No_of_days)
  #return(stockout)
}



### Function to run one simulation
run_simulation =function(no_of_products, lambda, Lead_Time, restock_level, resupply_level,total_inventory){
  
  #patient_arrive =(matrix(0, ncol =1, nrow = 1))
  patient_arrive = c()
  numsales=0
  #no_of_rows=0
  while(numsales<total_inventory)
  { 
    patient_count = generate_random(1,lambda)
    #print(patient_count)
    remaining_sales = total_inventory-numsales
    if(patient_count > remaining_sales)
    {
      #patient_count = remaining_sales
      patient_arrive  = c(patient_arrive,remaining_sales)
      #numsales = numsales + patient_count
      numsales = numsales + remaining_sales 
    }
    else
    {
      patient_arrive  = c(patient_arrive,patient_count)
      numsales = numsales + patient_count
    }
    
    
    
  }
  #print(paste("Print_Sum_patient_arrive =",sum(patient_arrive)))
  if(sum(patient_arrive)>total_inventory)
  {
    #print(error)
    #print(patient_arrive)
    #print(sum(patient_arrive))
    stop()
  }
  
  #print(numsales)
  
  No_of_days =length(patient_arrive)
  tablet_demand_df = as.data.frame(matrix(0, ncol = no_of_products, nrow = 0))
  No_of_demand_days = 0
  dim(tablet_demand_df)
  for (i in 1:No_of_days){
    assigned_tablet_per_day_df= as.data.frame(matrix(0, ncol = no_of_products, nrow = 0))
    No_of_sales_per_day = patient_arrive[i]
    for (j in 1:No_of_sales_per_day){
      prs = runif(no_of_products)
      #prs = generate_tablet(no_of_products)
      tab_patient=rmultinom(1, size = 1, prob = prs/sum(prs))
      assigned_tablet_per_day_df=rbind(assigned_tablet_per_day_df,matrix(tab_patient,nrow=1))
    }
    #      if(tablet_demand_per_day_df >No_of_sales_per_day)
    #      { 
    #        print("error")
    #        stop()
    #      }
    if (No_of_sales_per_day > 0) {
      tablet_demand_per_day_df = colSums(assigned_tablet_per_day_df)
      
      tablet_demand_df =rbind(tablet_demand_df,(tablet_demand_per_day_df))
      No_of_demand_days = No_of_demand_days + 1
    }
  }
  
  inv_data=data.frame(matrix(0, nrow = No_of_demand_days, ncol = no_of_products)) # Defining the dataframe for inventory days 
  supply_data=data.frame(matrix(0, nrow =No_of_demand_days, ncol = no_of_products))
  #for (i in 1:No_of_days){
  for (j in 1:no_of_products){
    inv_data[1,j] = resupply_level # Setting up the initial level per tablet per day
    supply_data[1,j] = resupply_level 
  }
  #}
  #  A=cmpute_clean(tablet_demand_df, inv_data,supply_data,stockout, Lead_Time, restock_level,resupply_level,patient_count,total_inventory,patient_arrive)
  A=cmpute_clean(tablet_demand_df, Lead_Time, restock_level,resupply_level,total_inventory)
  result = compute_overage(A,tablet_demand_df,No_of_demand_days,total_inventory,resupply_level)
  return(result)
}                                                                          

# Function to run and calculate multiple simulations
#dem, inven,supply, stockout, Lead_Time, restock_level,resupply_level,patient_count,total_inventory,patient_arrive
run_simulations = function(sim_runs, no_of_products, lambda, Lead_Time, restock_level, resupply_level, total_inventory) {
  Overage_Matrix = data.frame(matrix(0, ncol = no_of_products, nrow = 0))
  stockout = data.frame(matrix(0, ncol = no_of_products, nrow = 0))
  Stockout_per_run = data.frame(matrix(0, ncol = no_of_products, nrow = 0))
  No_of_days = data.frame(matrix(0, ncol = 1, nrow = 0))
  NoofConsignments = data.frame(matrix(0, ncol = 1, nrow = 0))
  for(p in 1:sim_runs){
    #print(p)
    rvalue =run_simulation(no_of_products, lambda, Lead_Time, restock_level, resupply_level,total_inventory)
    
    Overage_Matrix = rbind(Overage_Matrix, rvalue$OM)    #Overage_Matrix[p,] = rvalue$OM
    NoofConsignments =rbind(NoofConsignments,rvalue$C)
    Stockout_per_run =rbind(Stockout_per_run,rvalue$SR)
    
    #NoofConsignments=rbind(NoofConsignments,run_simulation(no_of_products, lambda, Lead_Time, restock_level, resupply_level,stockout,NoofConsignments,total_inventory))  
    No_of_days=rbind(No_of_days,rvalue$ND)
  }
  
  print(quantile(Overage_Matrix,na.rm = TRUE))
  print(paste("Lead_Time =", Lead_Time))
  print(paste("no_of_products =",no_of_products))
  print(paste("lambda =", lambda))
  print(paste("sim_runs =", sim_runs))
  print(paste("restock_level =", restock_level))
  print(paste("resupply_level =", resupply_level))
  print(paste("total_inventory =",total_inventory))
  
  p2<-(summary(No_of_days,na.rm=TRUE))
  names(dimnames(p2)) <- list("", "No_of_days Summary")
  print(p2)
  print(quantile(No_of_days,na.rm = TRUE))
  p3<-(summary(NoofConsignments,na.rm=TRUE))
  names(dimnames(p3)) <- list("", "No_of_Consignments Summary")
  print(p3)
  print(quantile(NoofConsignments,na.rm = TRUE))
  p4<-(summary(Overage_Matrix,na.rm=TRUE))
  names(dimnames(p4)) <- list("", "Overage_Matrix Summary")
  print(p4)
  p1<-((summary((Stockout_per_run),na.rm=TRUE)))
  names(p1)=c('A') 
  names(dimnames(p1)) <- list("", "Stockout Summary")
  print(p1)
  print(quantile((Stockout_per_run),na.rm = TRUE))
  return(list(OM=Overage_Matrix))
  
}



#user Inputs
user_input = function(){
  Lead_Time = readline("Enter Lead Time ") # Enter value of the Lead Time
  sim_runs = readline("Enter Simulation Runs ") # Enter value of the Simulation Runs
  lambda = readline("Enter lambda for poisson distribution ") #Enter value of Lambda for Poisson distribution
  total_inventory = readline("Enter total_inventory for every simulation run ")
  restock_level = readline("Enter Restock_level ")#Enter value of Restock_level 
  resupply_level = readline("Enter Resupply_level ") #Enter value of Resupply_level 
  no_of_products = readline("Enter number of products ")
  print("--1")
  Lead_Time = as.numeric(Lead_Time) 
  sim_runs= as.numeric(sim_runs)
  lambda  = as.numeric(lambda)
  restock_level =as.numeric( restock_level)
  resupply_level =as.numeric( resupply_level)
  total_inventory = as.numeric(total_inventory)
  no_of_products = as.numeric(no_of_products)
  Overage_stats = run_simulations(sim_runs,no_of_products, lambda, Lead_Time, restock_level, resupply_level,total_inventory)
  print("=====")
  print(Overage_stats)
}

user_input()


#debug(cmpute_clean)
#debug(compute_overage)
#debug(run_simulation)
#debug(remaining_enrollment)
# for(k in 1:10)
# {
#   test_Overage_stats = run_simulations(sim_runs=1,no_of_products=1, lambda=3, Lead_Time=3, restock_level=17, resupply_level=20,total_inventory=24)
#   
# }