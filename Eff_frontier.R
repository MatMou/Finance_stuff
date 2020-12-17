# -------------------------------------------------------------------------------
#   Author: Mathis Mourey
#   Date: 17/12/2020
#   Function: Evalute some efficient portfolios and the efficient frontier
# -------------------------------------------------------------------------------

# --------------------------------------------------
#   Define functions 
# --------------------------------------------------



library(quantmod)


#get adj close
get_data = function(tickers, nb_obs){
  df = data.frame(matrix(nrow=nb_obs , ncol=length(tickers)))
  
  for(i in 1:length(tickers)){
    df[,i] = tail(getSymbols(tickers[[i]], env=NULL)[,6], nb_obs)
  }
  colnames(df) = tickers
  return(df)
}


# takes a price dataframe to a return dataframe 
p_to_r = function(df){
  rt = data.frame(matrix(nrow=nrow(df) ,ncol= ncol(df)))
  
  for(i in 1:ncol(df)){
    rt[,i] = diff(log(df[,i]))
  }
  
  rt = rt[-1, ] #remove the first row of NAs
  colnames(rt) = colnames(df)
  return(rt)
}


# Minimum variance portfolio 
MVP = function(my_cov){
  id.vec = rep(1, ncol(my_cov))
  top = (solve(my_cov) %*% id.vec )
  bottom = ( t(id.vec) %*% solve(my_cov) %*% id.vec)
  opt_w = top/ as.numeric(bottom)
  
  return(opt_w)
}


# Maximum return portfolio 
MaxP = function(my_cov, my_esp, target){
  #create matrix A 
  id.vec = rep(1, ncol(my_cov))
  
  top_mat = cbind(2*my_cov, my_esp, id.vec)
  mid_mat = c(t(my_esp), 0, 0)
  bottom_mat = c(t(id.vec), 0, 0)
  
  A = rbind(top_mat, mid_mat, bottom_mat)
  B = c(rep(0, ncol(my_cov)), target, 1)
  
  w_opti = solve(A) %*% B
  w_opti = w_opti[1:ncol(my_cov)]
  
  return(w_opti)
}


# Uses the functions above to plot the efficient frontier and the efficient portfolios
Plot_eff_frontier = function(rt, nb_simu){
  
  # Compute cov matrix and er vector
  library(arrayhelpers)
  my_esp = colMeans(rt)
  my_cov = cov(rt)
 
  # generate simulation of portfolios 
  mc_port = data.frame(matrix(nrow=nb_simu, ncol=3))
  for(i in 1:nb_simu){
    w = as.matrix(sample(1:1000, length(my_esp)))
    w = w/sum(w)
    
    mc_port[i, 1] = t(w) %*% my_esp  #expected return
    mc_port[i, 2] = sqrt( t(w) %*% my_cov %*% w   )  #sd 
    mc_port[i, 3] =  mc_port[i, 1]/mc_port[i, 2]
  }
  
  colnames(mc_port) = c('ER', 'SD', 'SHARPE')
  
  # Minimum variance portfolio 
  x_min = MVP(my_cov)
  min_var_port = data.frame(matrix(nrow=1, ncol=3))
  min_var_port[1,1] = t(x_min) %*% my_esp 
  min_var_port[1,2] = sqrt(t(x_min) %*% my_cov %*% x_min)
  min_var_port[1,3] = min_var_port[1,1] / min_var_port[1,2] 
  colnames(min_var_port) = c('ER', 'SD', 'SHARPE')
  
  # Maximum return portfolio 
  x_max = MaxP(my_cov, my_esp, max(my_esp))
  max_port = data.frame(matrix(nrow=1, ncol=3))
  max_port[1,1] = t(x_max) %*% my_esp 
  max_port[1,2] = sqrt(t(x_max) %*% my_cov %*% x_max)
  max_port[1,3] = min_var_port[1,1] / min_var_port[1,2] 
  colnames(max_port) = c('ER', 'SD', 'SHARPE')
  
  # Maximum Shapre portfolio 
  max_sh_port = data.frame(matrix(nrow=1, ncol=3))
  temp = sort(mc_port[,'SHARPE'], decreasing=T, index.return=T)
  
  max_sh_port[1,1] = mc_port[temp$ix[1], 'ER']
  max_sh_port[1,2] = mc_port[temp$ix[1], 'SD']
  max_sh_port[1,3] = mc_port[temp$ix[1], 'SHARPE']
  colnames(max_sh_port) = c('ER', 'SD', 'SHARPE')
  
  # Building the effecient frontier 
  all_returns = seq(min_var_port[,'ER'], max_port[,'ER'], 0.00001 )
  eff_frontier = data.frame(matrix(nrow= length(all_returns), ncol=2))
  
  r = 1 
  for(i in all_returns){
    x_opti = MaxP(my_cov, my_esp, i) 
    eff_frontier[r,1] = t(x_opti) %*% my_esp #ER
    eff_frontier[r,2] = sqrt( t(x_opti) %*% my_cov %*% x_opti) #SD
    
    r = r + 1
  }
  
  colnames(eff_frontier) = c('ER', 'SD')
  
  # render ggplot
  p = ggplot() + geom_jitter(data=mc_port , aes(x=SD , y=ER, colour=SHARPE)) + 
    geom_point(data= min_var_port, aes(x=SD, y=ER), color='darkred') + 
    geom_point(data= max_port, aes(x=SD, y=ER), color='darkred') + 
    geom_point(data= max_sh_port, aes(x=SD, y=ER), color='darkblue') + 
    geom_line(data=eff_frontier, aes(x=SD, y=ER)) + 
    scale_color_gradient(low='orange3', high = 'gold')
  
  return(p)
}



# --------------------------------------------------
#   Evaluate functions
# --------------------------------------------------


tickers = c('DIS', 'VTVT', 'PFE', 'NIO', 'LULU', 'SNOA')
price = get_data(tickers, 400)
rt = p_to_r(price)

Plot_eff_frontier(rt, nb_simu=30000)

