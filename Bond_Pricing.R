

BondPricing = function(C, t_eval ,t_emission, t_maturity, r, C_freq){
  
  
  # Find all coupon dates 
  start_date = as.Date(t_emission, "%d-%m-%Y")
  end_date = as.Date(t_maturity, "%d-%m-%Y")
  
  if(C_freq=='month'){
    all_payments = format(seq(start_date,end_date,by="month"), "%d-%m-%Y")
    
  } else if(C_freq=='trimester'){
    x = format(seq(start_date,end_date,by="month"), "%d-%m-%Y")
    all_payments = x[seq(1, length(x), 3)]
    
  } else if(C_freq=='semester'){
    x = format(seq(start_date,end_date,by="month"), "%d-%m-%Y")
    all_payments = x[seq(1, length(x), 6)]
    
    
  } else if(C_freq=='year'){
    all_payments = format(seq(start_date,end_date,by="year"), "%d-%m-%Y")
    
  } else {
    return('Not proper coupon payment frequency ! \n Choose from: month, trimester, semester, year')
    
  }
  
  
  #Position yourself at the evaluation date + Evualuation of the bond
  if(t_eval!=t_emission){
    diffs = list()
    
    for(j in 1:length(all_payments)){
      diffs[[j]] = as.numeric(difftime(as.Date(all_payments[[j]],"%d-%m-%Y"), as.Date(t_eval,"%d-%m-%Y") , units = "days"))
    }

    payments =  all_payments[match(min(unlist(diffs[diffs>0])), diffs):length(all_payments)]
    
    hold_period = as.numeric(difftime(as.Date(t_eval,"%d-%m-%Y"),
                                      as.Date(all_payments[(length(all_payments) - length(payments))], "%d-%m-%Y"),
                                      units = "days"))
    total_period = as.numeric(difftime(as.Date(payments[1], "%d-%m-%Y"),
                                    as.Date(all_payments[(length(all_payments) - length(payments))], "%d-%m-%Y"),
                                    units = "days"))
    
    #Bond Price, Cotation and accrued interest 
    accrued_interest = hold_period/total_period * C
    bond_price = (C + C*(1 - (1+r)**(-(length(payments)-1)))/r + 1*(1+r)**(-(length(payments)-1))) * (1+r)**(-(total_period-hold_period)/total_period) 
    bond_cotation = bond_price - accrued_interest
    
    
    #Sensitivity
    post_hold = (total_period-hold_period)/total_period
    sensitivity_mat = matrix(nrow=(length(payments) + 1), ncol=4)
    colnames(sensitivity_mat) = c('t', 'Ft', 'Ft discount', 't Ft discount -1')
    rownames(sensitivity_mat) = c( payments, 'Sum')
    sensitivity_mat[,1] = c(post_hold + seq(0, length(payments)-1), 0)
    sensitivity_mat[,2] = c(rep(C, length(payments)-1), 1+C, 0)*100
    sensitivity_mat[1:length(payments),3] = sensitivity_mat[1:length(payments),2]*(1+r)**(-1*sensitivity_mat[1:length(payments),1])
    sensitivity_mat[1:length(payments),4] = sensitivity_mat[1:length(payments),1] * sensitivity_mat[1:length(payments),2]*(1+r)**(-sensitivity_mat[1:length(payments),1]-1)
    sensitivity_mat[(length(payments)+1),4]  = sum(sensitivity_mat[1:length(payments),4])  
    sensitivity_mat[(length(payments)+1),3]  = sum(sensitivity_mat[1:length(payments),3])  
    Sensitivity = as.numeric(-sensitivity_mat[(length(payments)+1),4]/sensitivity_mat[(length(payments)+1),3])
    
    
  } else {
    payments = all_payments

    bond_cotation = bond_price =  C*(1 - (1+r)**(-length(payments)))/r + 1*(1+r)**(-length(payments))
    accrued_interest = 0 
    
    #Sensitivity
    sensitivity_mat = matrix(nrow=(length(payments) + 1), ncol=4)
    colnames(sensitivity_mat) = c('t', 'Ft', 'Ft discount', 't Ft discount -1')
    rownames(sensitivity_mat) = c( payments, 'Sum')
    sensitivity_mat[,1] = c(seq(1, length(payments)), 0)
    sensitivity_mat[,2] = c(rep(C, length(payments)-1), 1+C, 0)*100
    sensitivity_mat[1:length(payments),3] = sensitivity_mat[1:length(payments),2]*(1+r)**(-1*sensitivity_mat[1:length(payments),1])
    sensitivity_mat[1:length(payments),4] = sensitivity_mat[1:length(payments),1] * sensitivity_mat[1:length(payments),2]*(1+r)**(-sensitivity_mat[1:length(payments),1]-1)
    sensitivity_mat[(length(payments)+1),4]  = sum(sensitivity_mat[1:length(payments),4])  
    sensitivity_mat[(length(payments)+1),3]  = sum(sensitivity_mat[1:length(payments),3])  
    Sensitivity = as.numeric(-sensitivity_mat[(length(payments)+1),4]/sensitivity_mat[(length(payments)+1),3])
    
  }
  
  
  return(list('P'= bond_price*100, 'C'= bond_cotation*100, 'AC'= accrued_interest*100, 'S' = Sensitivity, 'S_table' = sensitivity_mat))
} 


YTM = function(bond_cotation, C, t_eval ,t_emission, t_maturity, C_freq){
  
  
  # Find all coupon dates 
  start_date = as.Date(t_emission, "%d-%m-%Y")
  end_date = as.Date(t_maturity, "%d-%m-%Y")
  
  if(C_freq=='month'){
    all_payments = format(seq(start_date,end_date,by="month"), "%d-%m-%Y")
    
  } else if(C_freq=='trimester'){
    x = format(seq(start_date,end_date,by="month"), "%d-%m-%Y")
    all_payments = x[seq(1, length(x), 3)]
    
  } else if(C_freq=='semester'){
    x = format(seq(start_date,end_date,by="month"), "%d-%m-%Y")
    all_payments = x[seq(1, length(x), 6)]
    
    
  } else if(C_freq=='year'){
    all_payments = format(seq(start_date,end_date,by="year"), "%d-%m-%Y")
    
  } else {
    return('Not proper coupon payment frequency ! \n Choose from: month, trimester, semester, year')
    
  }
  
  
  #Position yourself at the evaluation date + Evualuation of the bond
  if(t_eval!=t_emission){
    diffs = list()
    
    for(j in 1:length(all_payments)){
      diffs[[j]] = as.numeric(difftime(as.Date(all_payments[[j]],"%d-%m-%Y"), as.Date(t_eval,"%d-%m-%Y") , units = "days"))
    }
    
    payments =  all_payments[match(min(unlist(diffs[diffs>0])), diffs):length(all_payments)]
    
    hold_period = as.numeric(difftime(as.Date(t_eval,"%d-%m-%Y"),
                                      as.Date(all_payments[(length(all_payments) - length(payments))], "%d-%m-%Y"),
                                      units = "days"))
    total_period = as.numeric(difftime(as.Date(payments[1], "%d-%m-%Y"),
                                       as.Date(all_payments[(length(all_payments) - length(payments))], "%d-%m-%Y"),
                                       units = "days"))
    
    #Bond Price, Cotation and accrued interest 
    accrued_interest = hold_period/total_period * C
    bond_price = bond_cotation/100 + accrued_interest
    
    r = seq(-0.2, 0.2, 0.00001)
    temp = (C + C*(1 - (1+r)**(-(length(payments)-1)))/r + 1*(1+r)**(-(length(payments)-1))) * (1+r)**(-(total_period-hold_period)/total_period) - bond_price
    temp = na.omit(temp)
    to_reach = min(abs(temp))
    
    indexes = match(c(to_reach, -to_reach), temp)
    
    for (i in indexes){
      if(is.na(i)==F){
        my_yield = r[[i]]
      }
    }
  }
  
  return(my_yield)
}

YTM(bond_cotation=100, C=0.03, t_eval='17-06-2010' ,t_emission='01-03-2007', t_maturity='01-03-2021', C_freq='trimester')

X1 =BondPricing(C=0.04, t_eval='17-06-2010' ,t_emission='01-01-2009', t_maturity='01-01-2020', r=0.03, C_freq='year')
X2 =BondPricing(C=0.01, t_eval='17-06-2010' ,t_emission='01-03-2007', t_maturity='01-03-2021', r=0.03, C_freq='trimester')
X3 =BondPricing(C=0.07, t_eval='17-06-2010' ,t_emission='01-01-2005', t_maturity='01-01-2020', r=0.03, C_freq='year')
X4 =BondPricing(C=0.1, t_eval='17-06-2010' ,t_emission='01-06-2009', t_maturity='01-06-2027', r=0.03, C_freq='semester')
X5 =BondPricing(C=0.02, t_eval='17-06-2010' ,t_emission='01-01-2009', t_maturity='01-01-2022', r=0.03, C_freq='year')


BondPortfolio = function(list_of_bonds, weights){
  
  PortPrice = 0
  for(i in 1:length(list_of_bonds)){
    PortPrice = PortPrice + list_of_bonds[[i]]$P * weights[i]
  }
  
  PortSensi = 0 
  for(i in 1:length(list_of_bonds)){
    PortSensi = PortSensi + list_of_bonds[[i]]$S * weights[i]
  }
 
  return(list('Port_Price'= PortPrice, 'Port_Sensi'= PortSensi))   
}

BondPortfolio(list_of_bonds =list(X1,X2,X3,X4,X5), weights= c(0.1, 0.3, 0.2, 0.1, 0.3))









#------------------------------------
#
# Plot simulations for portoflio  in price/sensitivity space
#
#------------------------------------

library(arrayhelpers)
library(ggplot2)

n_simu = 100000
mc_port = data.frame(matrix(nrow=n_simu, ncol=3))
for (i in 1:n_simu){
  w = as.matrix(sample(1:1000, 5, replace=F))
  w = w/sum(w)
  BP = BondPortfoio(list_of_bonds =list(X1,X2,X3,X4,X5), weights= w)
  mc_port[i,1] = BP$Port_Price
  mc_port[i,2] = BP$Port_Sensi
} 
colnames(mc_port)=c('Price', 'Sensi')


p <- ggplot() + geom_jitter(data=mc_port, aes(x=Sensi, y=Price)) + geom_vline(xintercept = 0)
p







