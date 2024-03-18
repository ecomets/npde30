##############################
### Simulation
lPSA<-function(day, r, PSA_0, epsilon, Tesc,a ) {
  d = 0.046; delta = 0.23
  
  PSATesc=delta*PSA_0/(r*(1-epsilon)-d+delta)*exp((r*(1-epsilon)-d)*Tesc)+
    (PSA_0-delta*PSA_0/(r*(1-epsilon)-d+delta))*exp(-delta*Tesc)
  
  PSA = ifelse (day<=Tesc ,  delta*PSA_0/(r*(1-epsilon)-d+delta)*exp((r*(1-epsilon)-d)*day)+
                  (PSA_0-delta*PSA_0/(r*(1-epsilon)-d+delta))*exp(-delta*day), 
                
                delta*PSA_0*exp((r*(1-epsilon)-d)*Tesc) /(r-d+delta)*exp((r-d)*(day-Tesc))+
                  (PSATesc-delta*PSA_0*exp((r*(1-epsilon)-d)*Tesc)/(r-d+delta))*exp(-delta*(day-Tesc)) )
  return(rnorm(1,log(PSA+1),sd=a))
}




simulation_psa = function(parameter, group, nrep, output, settings = list(seed=123456),type="PSA",
                          path_model = "/media/share/U738/marc.cerou/These/Projet_3_NPDE_joint/PSA/scriptR/"){
  
  if(type=="PSA"){
    jointModel = "jointModel_PSA.txt"
  }
  else if(type=="lPSA"){
    jointModel = "jointModel_lPSA.txt"
  }
  else if(type=="dPSA"){
    jointModel = "jointModel_dPSA.txt"
  }
  else if(type=="AUClPSA"){
    jointModel = "jointModel_AUCPSA.txt"
  }
  else if(type=="PSA0"){
    jointModel = "jointModel_PSA0.txt"
  }
  else if(type=="Tesc"){
    jointModel = "jointModel_Tesc.txt"
  }
  else if(type=="dlPSA"){
    jointModel = "jointModel_dlPSA.txt"
  }
  jointModel = paste0(path_model,jointModel)
  
  
  res <- simulx(model     = jointModel, 
                parameter = parameter,
                group = group,
                nrep = nrep,
                output    = list(output,c("r","PSA0","epsilon","Tesc","lambda","a")),settings = list(settings,disp.iter =T))
  res = res[1:(length(output)+1)]
  
  if("rep"%!in%names(res$y)){
    res[[1]]$rep=1
    # res[[2]]$rep=1
    res[[3]]$rep=1
  } 
  
  ### Replace Nan value
  print("Replace Nan value")
  data_to_resimulated = res[[1]][c(which(is.nan(res[[1]]$y))),]
  if(dim(data_to_resimulated)[1]>0){
    for(i in 1:dim(data_to_resimulated)[1]){
      param=res$parameter[res$parameter$id==data_to_resimulated$id[i]&res$parameter$rep==data_to_resimulated$rep[i],]
      
      new_y=lPSA(day = data_to_resimulated$time[i],r = param$r,PSA_0 = param$PSA0,epsilon = param$epsilon,Tesc=param$Tesc,a=param$a)
      if(is.na(new_y)) print(i)
      res[[1]]$y[is.nan(res[[1]]$y)&res[[1]]$id==data_to_resimulated$id[i]&res[[1]]$rep==data_to_resimulated$rep[i] &
                   res[[1]]$time==data_to_resimulated$time[i]] = new_y
      
    }
    
  }
  
  
  col = sapply(1:length(output),function(x){
    if(sum(c("e","event")%in%output[[x]]$name)>0){
      return(x)
    }else{
      return(NA)
    }
  })
  col_event = col[!is.na(col)]
  
  print("Output formatting")
  res_TTE = res[[col_event]]
  names(res_TTE)[(dim(res_TTE)[2]-1):(dim(res_TTE)[2])] = c("TFT","status")
  res_TTE = data.table(res_TTE)
  res_TTE = res_TTE[TFT>0]
  res_TTE[,OFT:=ifelse(TFT>735,735,TFT)]
  res_TTE[,status:=ifelse(TFT>735,0,1)]
  
  res_psa = data.table(res[[which(is.na(col))]])
  
  res_param = res[[3]]
  # res_psa$rep=NULL
  final = merge(res_psa,res_TTE)
  final = merge(final,res_param)
  
  final_with_dropout = final
  final_with_dropout = final_with_dropout[time<=OFT]
  
  return(list(TTE=res_TTE,psa_wo_dropout=final,psa_with_dropout=final_with_dropout))
  
  
}

complete_obs = function(obs,time){
  
}
##############################
### Data management

data_censored = function(res){
  # res2=res
  #res=res2
  res = res[1:2]
  ncol = dim(res[[2]])[2]
  names(res[[2]])[(ncol-1):ncol] = c("TTE","dropout")
  res[[3]] = res[[2]][res[[2]]$TTE>0,]
  
  
  res_hamd_e = data.table(merge(res[[1]],res[[3]]))
  res_hamd_e = res_hamd_e[time<=TTE]
  
  res_hamd_e
}










##############################
### Npde
prediction_discrepancie3_old = function(dat,simulated_data,test_cens=F,plot=F,old=F,ties=F,response="TFT",...){
  #dat=obs_TTE
  #simulated_data=sim_TTE
  
  
  
  # if(test_cens){
  y.name <- c("y","Y")
  TTE.name <- response
  rep.name <- c("rep","REP","Rep")
  id.name <- c("ID","id","Id","Num")
  cens.name <- c("cens","status","STATUS","CENS")
  
  
  names(dat)[names(dat) %in% y.name] <- c("Y")
  names(dat)[names(dat) %in% TTE.name] <- c("X")
  names(dat)[names(dat) %in% rep.name] <- c("rep")
  names(dat)[names(dat) %in% id.name] <- c("id")
  names(dat)[names(dat) %in% cens.name] <- c("status")
  
  if(response=="TFT") dat$status=1
  
  
  TTE.name <- c("TTE","tft","TFT")
  
  names(simulated_data)[names(simulated_data) %in% TTE.name] <- c("X")
  names(simulated_data)[names(simulated_data) %in% rep.name] <- c("rep")
  names(simulated_data)[names(simulated_data) %in% id.name] <- c("id")
  names(simulated_data)[names(simulated_data) %in% cens.name] <- c("status")
  # }else{
  
  simulated_data=data.frame(simulated_data)
  dat=data.frame(dat)
  
  
  K = length(unique(simulated_data$rep))
  n_ID=length(unique(dat$id))
  
  
  TTE_simulated = simulated_data[,c("rep","id","X","status")]
  
  TTE_simulated = TTE_simulated[order(TTE_simulated$X),]
  
  if(n_ID >length(unique(simulated_data$id))){
    stop(paste("Error: ",n_ID," ID in the data > ",length(unique(simulated_data$id))," ID in the prediction distribution dataset",sep=""))
  }
  
  quant=NULL
  
  data = dat[,c("id","X","status")]
  
  library(data.table)
  data=data.table(data)
  setkey(data,"id")
  TTE_simulated = data.table(TTE_simulated)
  setkeyv(TTE_simulated,c("id","rep"))
  
  data2 = merge(data,TTE_simulated,by="id")
  data3 = data2[,list(quant=mean(X.y<unique(X.x))),by="id"]
  
  data = merge(data,data3,by="id")
  data$quant0=data$quant
  if(test_cens) data[status==0]$quant = runif(length(data[status==0]$quant0),data[status==0]$quant0,1)
  data$quant0=NULL
  
  
  
  pd_wo_ties = data$quant
  pd=data$quant
  if(ties){
    
    pd[pd>=1] = sort(runif(length(pd[pd>=1]),1-1/K,1))
    pd[pd==0] = sort(runif(length(pd[pd==0]),0,1/K))
    npde = qnorm(pd)
    
  }else{
    idx = which(pd_wo_ties<1 & pd_wo_ties>0 & !is.na(pd_wo_ties))
    pd_wo_ties[idx] = pd_wo_ties[idx] + runif(length(idx),0,1/K)
    pd_wo_ties[pd_wo_ties>=1] = sort(runif(length(pd_wo_ties[pd_wo_ties>=1]),1-1/K,1))
    pd_wo_ties[pd_wo_ties==0] = sort(runif(length(pd_wo_ties[pd_wo_ties==0]),0,1/K))
    npde = qnorm(pd_wo_ties)
  }
  
  
  data$pd = pd
  data$pd_wo_ties=pd_wo_ties
  data$npde=npde
  
  
  adj = reject_stat(data$npde)
  ks = ks.test(data$npde,"pnorm")
  
  # if(plot){
  #   print(gg_qq(data$npde)$plot)
  # }
  data$status=factor(data$status)
  res = list(tab=data,test_adj=adj,test_ks=ks)
  
  if(plot){
    res_plot = plot_npde(npde=res,plot = F,cens = test_cens,dist_pred = simulated_data,...)
    res= list(tab=data,test_adj=adj,test_ks=ks,graph=res_plot)
    return(res)
  }else{
    return(res)
  }
  
  
  
  
}


prediction_discrepancie3 = function(dat,simulated_data,test_cens=F,plot=F,old=F,ties=F,response="TFT",...){
  #dat=obs_TTE
  #simulated_data=sim_TTE
  
  
  
  # if(test_cens){
  y.name <- c("y","Y")
  TTE.name <- response
  rep.name <- c("rep","REP","Rep","isim","ISIM","SIM")
  id.name <- c("ID","id","Id","Num")
  cens.name <- c("cens","status","STATUS","CENS","DV")
  
  
  names(dat)[names(dat) %in% y.name] <- c("Y")
  names(dat)[names(dat) %in% TTE.name] <- c("X")
  names(dat)[names(dat) %in% rep.name] <- c("rep")
  names(dat)[names(dat) %in% id.name] <- c("id")
  names(dat)[names(dat) %in% cens.name] <- c("status")
  
  if(response=="TFT") dat$status=1
  
  
  TTE.name <- c("TTE","tft","TFT")
  
  names(simulated_data)[names(simulated_data) %in% TTE.name] <- c("X")
  names(simulated_data)[names(simulated_data) %in% rep.name] <- c("rep")
  names(simulated_data)[names(simulated_data) %in% id.name] <- c("id")
  names(simulated_data)[names(simulated_data) %in% cens.name] <- c("status")
  # }else{
  
  simulated_data=data.frame(simulated_data)
  dat=data.frame(dat)
  
  
  K = length(unique(simulated_data$rep))
  n_ID=length(unique(dat$id))
  
  
  TTE_simulated = simulated_data[,c("rep","id","X","status")]
  
  TTE_simulated = TTE_simulated[order(TTE_simulated$X),]
  
  if(n_ID >length(unique(simulated_data$id))){
    stop(paste("Error: ",n_ID," ID in the data > ",length(unique(simulated_data$id))," ID in the prediction distribution dataset",sep=""))
  }
  
  quant=NULL
  
  data = dat#[,c("id","X","status")]
  
  library(data.table)
  data=data.table(data)
  setkey(data,"id")
  TTE_simulated = data.table(TTE_simulated)
  setkeyv(TTE_simulated,c("id","rep"))
  
  data2 = merge(data,TTE_simulated,by="id")
  # data2 = data2[,quant:=mean(X.y<unique(X.x)),by="id"]
  data3 = data2[,list(quant=mean(X.y<unique(X.x)),medTime=mean(X.y)),by="id"]
  
  data = merge(data,data3,by="id")
  data$quant0=data$quant
  if(test_cens) data[status==0]$quant = runif(length(data[status==0]$quant0),data[status==0]$quant0,1)
  data$quant0=NULL
  
  
  
  pd_wo_ties = data$quant
  pd=data$quant
  if(ties){
    
    pd[pd>=1] = sort(runif(length(pd[pd>=1]),1-1/K,1))
    pd[pd==0] = sort(runif(length(pd[pd==0]),0,1/K))
    npde = qnorm(pd)
    
  }else{
    idx = which(pd_wo_ties<1 & pd_wo_ties>0 & !is.na(pd_wo_ties))
    pd_wo_ties[idx] = pd_wo_ties[idx] + runif(length(idx),0,1/K)
    pd_wo_ties[pd_wo_ties>=1] = sort(runif(length(pd_wo_ties[pd_wo_ties>=1]),1-1/K,1))
    pd_wo_ties[pd_wo_ties==0] = sort(runif(length(pd_wo_ties[pd_wo_ties==0]),0,1/K))
    npde = qnorm(pd_wo_ties)
  }
  
  
  
  data$pd = pd
  data$pd_wo_ties=pd_wo_ties
  data$npde=npde
  
  data2.2=merge(data2,data)
  imp = unique(data2.2[,list(imputed_time=quantile(X.y,pd_wo_ties)),by="id"])
  data=merge(imp,data,by="id")
  
  adj = reject_stat(data$npde)
  ks = ks.test(data$npde,"pnorm")
  
  # if(plot){
  #   print(gg_qq(data$npde)$plot)
  # }
  data$status=factor(data$status)
  res = list(tab=data,test_adj=adj,test_ks=ks)
  
  # if(plot){
  #   res_plot = plot_npde(npde=res,plot = F,cens = test_cens,dist_pred = simulated_data,...)
  #   res= list(tab=data,test_adj=adj,test_ks=ks,graph=res_plot)
  #   return(res)
  # }else{
    return(res)
  # }
  
  
  
  
}


computenpde2<-function(obs,sim,verbose=F,response="y") {
  
  tab<-data.table(obs) # non-missing data
  tabsim<-data.table(sim) # corresponding simulated data    
  # print(names(tabsim))
  names(tab)[names(tab)==response] = "y"
  names(tabsim)[names(tabsim)==response] = "y"
  
  
  tabsim$id=factor(tabsim$id)
  tab$id=factor(tab$id)
  setkeyv(tabsim,"id")
  nrep = length(unique(tabsim$rep))
  
  
  id_vec = unique(tab$id)
  pde = tab[order(id,time)]
  pde$pde=-99
  
  if(verbose){
    pb = txtProgressBar(min = 0, max = length(id_vec), style = 3)
    # pb <- tkProgressBar(min = 0, max = length(id_vec), width = 300)
    # pb = winProgressBar(title = "progress bar", min = 0,
    #                max = length(id_vec), width = 300)
    
  }
  for(i in 1:length(id_vec)){
    # if(verbose) cat(paste0("computation for subject: ",i,sep="\n"))
    if(verbose){
      setTxtProgressBar(pb, i)
      # setTkProgressBar(pb, i,label=paste( round(i/length(id_vec)*100, 0),"% done"))
      # setWinProgressBar(pb, i, title=paste( round(i/length(id_vec)*100, 0),
      #                                       "% done"))
      
    }
    
    obs_i = tab[id==id_vec[i]]
    sim_i = tabsim[id==id_vec[i]]
    # print(names(sim_i))
    pde_i = calcnpde2(obs_i,sim_i,nrep,decorr.method = "cholesky")
    # pde[id==id_vec[i]]$pde = pde_i$pde
    pde[id==id_vec[i]] = pde_i
    
    
    
  }
  if(verbose){
    close(pb)
  } 
  # 
  # idx<-which(pde$pde>0 & pde$pde<1)
  # pde[pde==0]$pde<-runif(sum(pde$pde==0),0,1/(2*nrep))
  # pde[pde==1]$pde<-runif(sum(pde$pde==1),1-1/(2*nrep),1)
  # pde[,pde_wo_ties:=pde]
  # pde[idx]$pde_wo_ties<-pde[idx]$pde_wo_ties+runif(length(idx),0,1/nrep)
  # 
  # npde=pde
  # npde$npde = qnorm(npde$pde_wo_ties)
  
  # npde$nk=NULL
  return(pde)
  
}

computenpde_missing<-function(obs,sim,verbose=F,response="y",method="same_vpc",compute_npde=T,compute_npd=T) {
  
  tab<-data.table(obs) # non-missing data
  
  
  # sim = sim$psa_wo_dropout
  
  
  
  tabsim<-data.table(sim) # corresponding simulated data    
  # print(names(tabsim))
  names(tab)[names(tab)==response] = "y"
  names(tabsim)[names(tabsim)==response] = "y"
  
  if(method=="same_vpc"){
    TTD = unique(tab[,c("id","OFT"),with=F],by="id")
    names(TTD)=c("id","obsOFT")
    tabsim$id = as.num(tabsim$id)
    tabsim=merge(tabsim,TTD,by="id")
    tabsim=tabsim[time<=obsOFT]
  }
  
  tabsim$id=factor(tabsim$id)
  tab$id=factor(tab$id)
  setkeyv(tabsim,"id")
  K = length(unique(tabsim$rep))
  
  
  id_vec = unique(tab$id)
  npde = tab[order(id,time)]
  npde$dobs=-99
  npde$pd=-99
  npde$pd_wo_ties=-99
  npde$npd=-99
  npde$pde=-99
  npde$pde_wo_ties=-99
  npde$npde=-99
  
  if(!compute_npde){
    npde$dobs=NULL
    npde$pde=NULL
    npde$pde_wo_ties=NULL
    npde$npde=NULL
  }

  if(!compute_npd){
    npde$pd=NULL
    npde$pd_wo_ties=NULL
    npde$npd=NULL
  }
  
  
  if(verbose)   pb = txtProgressBar(min = 0, max = length(id_vec), style = 3) 
  
  
  for(i in 1:length(id_vec)){
    method2=method
    # if(verbose) cat(paste0("computation for subject: ",i,sep="\n"))
    if(verbose) setTxtProgressBar(pb, i)
    
    obs_i = tab[id==id_vec[i]]
    sim_i = tabsim[id==id_vec[i]]
    
    
    nrep = length(unique(sim_i$rep))
    # print(names(sim_i))
    if(compute_npde) pde_i = try(calcnpde2(obs_i,sim_i,nrep,decorr.method = "cholesky",method=method2),T)
    if(compute_npd) pd_i = calcnpd(obs_i,sim_i)
    
    if(compute_npde){
      if(!is.data.table(pde_i)){
        pde_i = obs_i[,c("id","time"),with=F]
        # pde_i$pd=NA
        # pde_i$pd_wo_ties=NA
        # pde_i$npd=NA
        pde_i$dobs=NA
        pde_i$pde=NA
        pde_i$pde_wo_ties=NA
        pde_i$npde=NA
        
      }
    }
    
    if(compute_npd) npde[id==id_vec[i],c("pd","pd_wo_ties","npd"):=pd_i[,c("pd","pd_wo_ties","npd"),with=F]]
    
    if(compute_npde) npde[id==id_vec[i],c("dobs","pde","pde_wo_ties","npde"):=pde_i[,c("dobs","pde","pde_wo_ties","npde"),with=F]]
    
    
    
  }
  if(verbose){
    close(pb)
  } 
  
  
  
  # npde$nk=NULL
  return(npde)
  
}

computenpde_missing2<-function(obs,sim,verbose=F,response="y",method="same_vpc",compute_npde=T,compute_npd=T,decorr.method = "polar") {
  
  tab<-data.table(obs) # non-missing data
  
  
  # sim = sim$psa_wo_dropout
  
  
  
  tabsim_decorr<-data.table(sim) # corresponding simulated data
  tabsim<-data.table(sim) # corresponding simulated data    
  # print(names(tabsim))
  names(tab)[names(tab)==response] = "y"
  names(tabsim)[names(tabsim)==response] = "y"
  names(tabsim_decorr)[names(tabsim_decorr)==response] = "y"
  
  
  
  tabsim_decorr$id = as.num(tabsim_decorr$id)
  tabsim=tabsim[time<=OFT]
  
  tabsim$id=factor(tabsim$id)
  tabsim_decorr$id=factor(tabsim_decorr$id)
  tab$id=factor(tab$id)
  setkeyv(tabsim,"id")
  setkeyv(tabsim_decorr,"id")
  K = length(unique(tabsim$rep))
  
  
  id_vec = unique(tab$id)
  npde = tab[order(id,time)]
  npde$dobs=-99
  npde$pd=-99
  npde$pd_wo_ties=-99
  npde$npd=-99
  npde$pde=-99
  npde$pde_wo_ties=-99
  npde$npde=-99
  
  if(!compute_npde){
    npde$dobs=NULL
    npde$pde=NULL
    npde$pde_wo_ties=NULL
    npde$npde=NULL
  }
  
  if(!compute_npd){
    npde$pd=NULL
    npde$pd_wo_ties=NULL
    npde$npd=NULL
  }
  
  
  if(verbose)   pb = txtProgressBar(min = 0, max = length(id_vec), style = 3) 
  
  
  for(i in 1:length(id_vec)){
    method2=method
    # if(verbose) cat(paste0("computation for subject: ",i,sep="\n"))
    if(verbose) setTxtProgressBar(pb, i)
    
    obs_i = tab[id==id_vec[i]]
    sim_i = tabsim[id==id_vec[i]]
    sim_i_decorr = tabsim_decorr[id==id_vec[i]]
    
    nrep = length(unique(sim_i$rep))
    # print(names(sim_i))
    if(compute_npde) pde_i = try(calcnpde2(obs_i,sim_i_decorr,nrep,decorr.method = decorr.method,method=method2),T)
    if(compute_npd) pd_i = calcnpd(obs_i,sim_i)
    
    if(compute_npde){
      if(!is.data.table(pde_i)){
        pde_i = obs_i[,c("id","time"),with=F]
        # pde_i$pd=NA
        # pde_i$pd_wo_ties=NA
        # pde_i$npd=NA
        pde_i$dobs=NA
        pde_i$pde=NA
        pde_i$pde_wo_ties=NA
        pde_i$npde=NA
        
      }
    }
    
    if(compute_npd) npde[id==id_vec[i],c("pd","pd_wo_ties","npd"):=pd_i[,c("pd","pd_wo_ties","npd"),with=F]]
    
    if(compute_npde) npde[id==id_vec[i],c("dobs","pde","pde_wo_ties","npde"):=pde_i[,c("dobs","pde","pde_wo_ties","npde"),with=F]]
    
    
    
  }
  if(verbose){
    close(pb)
  } 
  
  
  
  # npde$nk=NULL
  return(npde)
  
}


computenpde_missing_test<-function(obs,sim,verbose=F,response="y",method="same_vpc",compute_npde=T,compute_npd=T) {
  
  tab<-data.table(obs) # non-missing data
  
  
  # sim = sim$psa_wo_dropout
  
  
  
  tabsim_decorr<-data.table(sim) # corresponding simulated data
  tabsim<-data.table(sim) # corresponding simulated data    
  # print(names(tabsim))
  names(tab)[names(tab)==response] = "y"
  names(tabsim)[names(tabsim)==response] = "y"
  names(tabsim_decorr)[names(tabsim_decorr)==response] = "y"
  
  
  
  tabsim_decorr$id = as.num(tabsim_decorr$id)
  tabsim=tabsim[time<=OFT]
  
  tabsim$id=factor(tabsim$id)
  tabsim_decorr$id=factor(tabsim_decorr$id)
  tab$id=factor(tab$id)
  setkeyv(tabsim,"id")
  setkeyv(tabsim_decorr,"id")
  K = length(unique(tabsim$rep))
  
  
  id_vec = unique(tab$id)
  npde = tab[order(id,time)]
  npde$dobs=-99
  npde$pd=-99
  npde$pd_wo_ties=-99
  npde$npd=-99
  npde$pde=-99
  npde$pde_wo_ties=-99
  npde$npde=-99
  
  if(!compute_npde){
    npde$dobs=NULL
    npde$pde=NULL
    npde$pde_wo_ties=NULL
    npde$npde=NULL
  }
  
  if(!compute_npd){
    npde$pd=NULL
    npde$pd_wo_ties=NULL
    npde$npd=NULL
  }
  
  
  if(verbose)   pb = txtProgressBar(min = 0, max = length(id_vec), style = 3) 
  
  
  for(i in 1:length(id_vec)){
    method2=method
    # if(verbose) cat(paste0("computation for subject: ",i,sep="\n"))
    if(verbose) setTxtProgressBar(pb, i)
    
    obs_i = tab[id==id_vec[i]]
    sim_i = tabsim[id==id_vec[i]]
    sim_i_decorr = tabsim_decorr[id==id_vec[i]]
    
    nrep = length(unique(sim_i$rep))
    # print(names(sim_i))
    if(compute_npde) pde_i = try(calcnpde3(obs_i,sim_i,sim_i_decorr,nrep,decorr.method = "cholesky",method=method2),T)
    if(compute_npd) pd_i = calcnpd(obs_i,sim_i)
    
    if(compute_npde){
      if(!is.data.table(pde_i)){
        pde_i = obs_i[,c("id","time"),with=F]
        # pde_i$pd=NA
        # pde_i$pd_wo_ties=NA
        # pde_i$npd=NA
        pde_i$dobs=NA
        pde_i$pde=NA
        pde_i$pde_wo_ties=NA
        pde_i$npde=NA
        
      }
    }
    
    if(compute_npd) npde[id==id_vec[i],c("pd","pd_wo_ties","npd"):=pd_i[,c("pd","pd_wo_ties","npd"),with=F]]
    
    if(compute_npde) npde[id==id_vec[i],c("dobs","pde","pde_wo_ties","npde"):=pde_i[,c("dobs","pde","pde_wo_ties","npde"),with=F]]
    
    
    
  }
  if(verbose){
    close(pb)
  } 
  
  
  
  # npde$nk=NULL
  return(npde)
  
}

computenpde_missing_test2<-function(obs,sim,verbose=F,response="y",method="same_vpc",compute_npde=T,compute_npd=T) {
  
  tab<-data.table(obs) # non-missing data
  
  
  # sim = sim$psa_wo_dropout
  
  
  
  tabsim_decorr<-data.table(sim) # corresponding simulated data
  tabsim<-data.table(sim) # corresponding simulated data    
  # print(names(tabsim))
  names(tab)[names(tab)==response] = "y"
  names(tabsim)[names(tabsim)==response] = "y"
  names(tabsim_decorr)[names(tabsim_decorr)==response] = "y"
  
  
  
  tabsim_decorr$id = as.num(tabsim_decorr$id)
  tabsim=tabsim[time<=OFT]
  
  tabsim$id=factor(tabsim$id)
  tabsim_decorr$id=factor(tabsim_decorr$id)
  tab$id=factor(tab$id)
  setkeyv(tabsim,"id")
  setkeyv(tabsim_decorr,"id")
  K = length(unique(tabsim$rep))
  
  
  id_vec = unique(tab$id)
  npde = tab[order(id,time)]
  npde$dobs=-99
  npde$pd=-99
  npde$pd_wo_ties=-99
  npde$npd=-99
  npde$pde=-99
  npde$pde_wo_ties=-99
  npde$npde=-99
  
  if(!compute_npde){
    npde$dobs=NULL
    npde$pde=NULL
    npde$pde_wo_ties=NULL
    npde$npde=NULL
  }
  
  if(!compute_npd){
    npde$pd=NULL
    npde$pd_wo_ties=NULL
    npde$npd=NULL
  }
  
  
  if(verbose)   pb = txtProgressBar(min = 0, max = length(id_vec), style = 3) 
  
  
  for(i in 1:length(id_vec)){
    method2=method
    # if(verbose) cat(paste0("computation for subject: ",i,sep="\n"))
    if(verbose) setTxtProgressBar(pb, i)
    
    obs_i = tab[id==id_vec[i]]
    sim_i = tabsim[id==id_vec[i]]
    sim_i_decorr = tabsim_decorr[id==id_vec[i]]
    
    nrep = length(unique(sim_i$rep))
    # print(names(sim_i))
    if(compute_npde) pde_i = try(calcnpde4(obs_i,sim_i,decorr.method = "cholesky",method=method2),T)
    if(compute_npd) pd_i = calcnpd(obs_i,sim_i)
    
    if(compute_npde){
      if(!is.data.table(pde_i)){
        pde_i = obs_i[,c("id","time"),with=F]
        # pde_i$pd=NA
        # pde_i$pd_wo_ties=NA
        # pde_i$npd=NA
        pde_i$dobs=NA
        pde_i$pde=NA
        pde_i$pde_wo_ties=NA
        pde_i$npde=NA
        
      }
    }
    
    if(compute_npd) npde[id==id_vec[i],c("pd","pd_wo_ties","npd"):=pd_i[,c("pd","pd_wo_ties","npd"),with=F]]
    
    if(compute_npde) npde[id==id_vec[i],c("dobs","pde","pde_wo_ties","npde"):=pde_i[,c("dobs","pde","pde_wo_ties","npde"),with=F]]
    
    
    
  }
  if(verbose){
    close(pb)
  } 
  
  
  
  # npde$nk=NULL
  return(npde)
  
}



correlation_matrix<-function(obs_i=NULL,sim_i,nrep,decorr.method,verbose=FALSE,method="all_sim") {
  # print(names(sim_i))
  # time=unique(obs_i$time)
  matsim=dcast(sim_i,rep~time,drop=T,value.var="y") 
  matsim$rep=NULL
  
  
  matsim = matsim[apply((is.na(matsim)*1),1,sum)==0,]
  varsim=cov(matsim)#,use = "na.or.complete")
  
  
  moysim=apply(matsim,2,function(x) mean(x,na.rm = T))
  
  
  #computing V-1/2
  xerr<-0
  if(length(moysim)>1) {
    y<-switch(decorr.method,
              cholesky=decorr.chol(varsim),
              polar=decorr.polar(varsim),
              inverse=decorr.inverse(varsim))
    if(y$xerr==0) ymat<-y$y else xerr<-y$xerr
  } else ymat<-1/sqrt(varsim)
  if(xerr==0) {
    #decorrelation of the simulations
    return(ymat)
  }else{
    return(xerr)
  }
  
}

calcnpde4<-function(obs_i,sim_i,decorr.method,verbose=FALSE,method="all_sim") {
  # print(names(sim_i))
  # time=unique(obs_i$time)
  matsim=dcast(sim_i,rep~time,drop=T,value.var="y") 
  matsim$rep=NULL
  
  
  # matsim = matsim[apply((is.na(matsim)*1),1,sum)==0,]
  # varsim=cov(matsim)#,use = "na.or.complete")
  
  
  
  moysim=apply(matsim,2,function(x) mean(x,na.rm = T))
  
  k=1
  left = as.matrix(matsim[k]-moysim)
  Vi = as.matrix(t(left) %*% left)
  Vi[is.na(Vi)]=0
  for(k in 2:1000){
    left = as.matrix(matsim[k]-moysim)
    product = as.matrix(t(left) %*% left)
    product[is.na(product)]=0
    Vi = Vi+product
  }
  # Vi/(1000-1)
  
  
  nrep = apply(matsim,2,function(x) sum(!is.na(x)))
  nrep = melt(nrep)
  nrep$time=as.num(row.names(nrep))
  names(nrep)[1]="nrep"
  
  Vi2 = data.table(melt(Vi))
  Vi2[,time:=as.num(max(Var1,Var2)),by="value"]
  
  Vi3 = merge(Vi2,nrep,by="time")
  Vi3[,value:=value/(nrep-1)]
  # Vi3$time=NULL
  
  Vi4 = as.matrix(dcast(Vi3,Var1~Var2,drop=T,value.var="value"),colnames=NULL,rownames = NULL )
  Vi4 = Vi4[,2:length(Vi4[1,])]
  
  
  varsim=Vi4
 
  #computing V-1/2
  xerr<-0
  if(length(moysim)>1) {
    y<-switch(decorr.method,
              cholesky=decorr.chol(varsim),
              polar=decorr.polar(varsim),
              inverse=decorr.inverse(varsim))
    if(y$xerr==0) ymat<-y$y else xerr<-y$xerr
  } else ymat<-1/sqrt(varsim)
  if(xerr==0) {
    #decorrelation of the simulations
    # mat = ((t(matsim)-moysim))
    # mat_without_na = apply(mat_with_na,2,function(x){
    #   x[is.na(x)]=0
    #   return(x)
    # })
    if(length(ymat)>1){
      sim_i[,ydsim:=ymat[1:length(y),1:length(y)]%*%(y-moysim[1:length(y)]),by=c("rep")]
      decobs<-ymat[1:dim(obs_i)[1],1:dim(obs_i)[1]]%*%(obs_i[["y"]]-moysim[1:dim(obs_i)[1]])
    }else{
      sim_i[,ydsim:=ymat%*%(y-moysim),by=c("rep")]
      decobs<-ymat%*%(obs_i[["y"]]-moysim)
    }
    
    
    
    
    
    
    if(dim(obs_i)[1]==1) row.names(decobs)="0"
    ydsim<-sim_i[,c("rep","time","ydsim"),with=F]
    names(ydsim)[3]="y"
    #decorrelation of the observations
    # ydobs<-decobs
    ydobs = data.table(id=unique(obs_i$id),time=row.names(decobs),decobs)
    names(ydobs)[3]="y"
    ydobs[,time:=as.num(time)]
    ydsim[,time:=as.num(time)]
    ydsim[,nrep:=length(unique(rep)),by="time"]
    
    tot = merge(ydsim,ydobs,by=c("time"))
    
    
    #Computing the pde
    pde=tot[,list(pde=mean(y.x<y.y),y.y=unique(y.y),nrep=unique(nrep)),by=c("id","time")]
    # pde=tot[,pde:=mean(y.x<y.y),by=c("id","time")]
    
    pde[is.na(pde),pde:=runif(length(pde),0,1)]
    
    
    idx<-which(pde$pde>0 & pde$pde<1)
    pde[,pde_wo_ties:=pde]
    pde[idx,pde_wo_ties:=pde_wo_ties+runif(length(idx),0,1/(nrep))]
    
    pde[pde_wo_ties==0,pde_wo_ties:=runif(sum(pde_wo_ties==0),0,1/(nrep))]
    pde[pde_wo_ties>=1,pde_wo_ties:=runif(sum(pde_wo_ties>=1),1-1/(nrep),1)]
    
    
    npde=pde
    npde[,npde := qnorm(pde_wo_ties)]
    npde$time=as.num(npde$time)
    names(npde)[names(npde)=="y.y"] = "dobs"
    
  }
  
  return(npde)
}



calcnpde3.M3<-function(obs_i,sim_i,sim_i_decorr,nrep,decorr.method,verbose=FALSE,method="all_sim") {
  # print(names(sim_i))
  # time=unique(obs_i$time)
  
  ymat = t(correlation_matrix(obs_i,sim_i_decorr,nrep,decorr.method,verbose=FALSE,method="all_sim"))
  
  matsim_decorr=dcast(sim_i_decorr,rep~time,drop=T,value.var="y") 
  matsim_decorr$rep=NULL
  
  
  # matsim = matsim[apply((is.na(matsim)*1),1,sum)==0,]
  
  matsim=dcast(sim_i,rep~time,drop=T,value.var="y") 
  matsim$rep=NULL
  
  moysim=apply(matsim,2,function(x) mean(x,na.rm = T))
  
  
  #computing V-1/2
  
    #decorrelation of the simulations
  sim_i[,ydsim:=ymat[1:length(y),1:length(y)]%*%(y-moysim[1:length(y)]),by=c("rep")]
    
    
    decobs<-ymat[1:dim(obs_i)[1],1:dim(obs_i)[1]]%*%(obs_i[["y"]]-moysim[1:dim(obs_i)[1]])
    
    
    if(dim(obs_i)[1]==1) row.names(decobs)="0"
    ydsim<-sim_i[,c("rep","time","ydsim"),with=F]
    names(ydsim)[3]="y"
    #decorrelation of the observations
    # ydobs<-decobs
    ydobs = data.table(id=unique(obs_i$id),time=row.names(decobs),decobs)
    names(ydobs)[3]="y"
    ydobs[,time:=as.num(time)]
    ydsim[,time:=as.num(time)]
    ydsim[,nrep:=length(unique(rep)),by="time"]
    
    tot = merge(ydsim,ydobs,by=c("time"))
    
    
    #Computing the pde
    pde=tot[,list(pde=mean(y.x<y.y),y.y=unique(y.y),nrep=unique(nrep)),by=c("id","time")]
    # pde=tot[,pde:=mean(y.x<y.y),by=c("id","time")]
    
    # pde[is.na(pde),pde:=runif(length(pde),0,1)]
    
    
    idx<-which(pde$pde>0 & pde$pde<1)
    pde[,pde_wo_ties:=pde]
    pde[idx,pde_wo_ties:=pde_wo_ties+runif(length(idx),0,1/(nrep))]
    
    pde[pde_wo_ties==0,pde_wo_ties:=runif(sum(pde_wo_ties==0),0,1/(nrep))]
    pde[pde_wo_ties>=1,pde_wo_ties:=runif(sum(pde_wo_ties>=1),1-1/(nrep),1)]
    
    
    npde=pde
    npde[,npde := qnorm(pde_wo_ties)]
    npde$time=as.num(npde$time)
    names(npde)[names(npde)=="y.y"] = "dobs"
    

  
  return(npde)
}


calcnpde3.M2<-function(obs_i,sim_i,sim_i_decorr,nrep,decorr.method,verbose=FALSE,method="all_sim") {
  # print(names(sim_i))
  # time=unique(obs_i$time)
  
  ymat = t(correlation_matrix(obs_i,sim_i_decorr,nrep,decorr.method,verbose=FALSE,method="all_sim"))
  
  matsim_decorr=dcast(sim_i_decorr,rep~time,drop=T,value.var="y") 
  matsim_decorr$rep=NULL
  
  
  # matsim = matsim[apply((is.na(matsim)*1),1,sum)==0,]
  
  matsim=dcast(sim_i,rep~time,drop=T,value.var="y") 
  matsim$rep=NULL
  
  moysim=apply(matsim_decorr,2,function(x) mean(x,na.rm = T))
  
  
  #computing V-1/2
  
  #decorrelation of the simulations
  sim_i[,ydsim:=ymat[1:length(y),1:length(y)]%*%(y-moysim[1:length(y)]),by=c("rep")]
  
  
  decobs<-ymat[1:dim(obs_i)[1],1:dim(obs_i)[1]]%*%(obs_i[["y"]]-moysim[1:dim(obs_i)[1]])
  
  
  if(dim(obs_i)[1]==1) row.names(decobs)="0"
  ydsim<-sim_i[,c("rep","time","ydsim"),with=F]
  names(ydsim)[3]="y"
  #decorrelation of the observations
  # ydobs<-decobs
  ydobs = data.table(id=unique(obs_i$id),time=row.names(decobs),decobs)
  names(ydobs)[3]="y"
  ydobs[,time:=as.num(time)]
  ydsim[,time:=as.num(time)]
  ydsim[,nrep:=length(unique(rep)),by="time"]
  
  tot = merge(ydsim,ydobs,by=c("time"))
  
  
  #Computing the pde
  pde=tot[,list(pde=mean(y.x<y.y),y.y=unique(y.y),nrep=unique(nrep)),by=c("id","time")]
  # pde=tot[,pde:=mean(y.x<y.y),by=c("id","time")]
  
  # pde[is.na(pde),pde:=runif(length(pde),0,1)]
  
  
  idx<-which(pde$pde>0 & pde$pde<1)
  pde[,pde_wo_ties:=pde]
  pde[idx,pde_wo_ties:=pde_wo_ties+runif(length(idx),0,1/(nrep))]
  
  pde[pde_wo_ties==0,pde_wo_ties:=runif(sum(pde_wo_ties==0),0,1/(nrep))]
  pde[pde_wo_ties>=1,pde_wo_ties:=runif(sum(pde_wo_ties>=1),1-1/(nrep),1)]
  
  
  npde=pde
  npde[,npde := qnorm(pde_wo_ties)]
  npde$time=as.num(npde$time)
  names(npde)[names(npde)=="y.y"] = "dobs"
  
  
  
  return(npde)
}

calcnpde3<-function(obs_i,sim_i,sim_i_decorr,nrep,decorr.method,verbose=FALSE,method="all_sim") {
  # print(names(sim_i))
  # time=unique(obs_i$time)
  
  ymat = t(correlation_matrix(obs_i,sim_i_decorr,nrep,decorr.method,verbose=FALSE,method="all_sim"))
  
  # matsim_decorr=dcast(sim_i_decorr,rep~time,drop=T,value.var="y") 
  # matsim_decorr$rep=NULL
  
  
  
  
  # matsim=dcast(sim_i,rep~time,drop=T,value.var="y") 
  # matsim$rep=NULL
  
  # moysim=apply(matsim_decorr,2,function(x) mean(x,na.rm = T))
  
  
  #computing V-1/2
  
  #decorrelation of the simulations
  # sim_i[,ydsim:=ymat[1:length(y),1:length(y)]%*%(y-moysim[1:length(y)]),by=c("rep")]
  sim_i[,ydsim:=ymat[1:length(y),1:length(y)]%*%(y),by=c("rep")]
  
  
  decobs<-ymat[1:dim(obs_i)[1],1:dim(obs_i)[1]]%*%(obs_i[["y"]])
  
  
  if(dim(obs_i)[1]==1) row.names(decobs)="0"
  ydsim<-sim_i[,c("rep","time","ydsim"),with=F]
  names(ydsim)[3]="y"
  #decorrelation of the observations
  # ydobs<-decobs
  ydobs = data.table(id=unique(obs_i$id),time=row.names(decobs),decobs)
  names(ydobs)[3]="y"
  ydobs[,time:=as.num(time)]
  ydsim[,time:=as.num(time)]
  ydsim[,nrep:=length(unique(rep)),by="time"]
  
  tot = merge(ydsim,ydobs,by=c("time"))
  
  
  #Computing the pde
  pde=tot[,list(pde=mean(y.x<y.y),y.y=unique(y.y),nrep=unique(nrep)),by=c("id","time")]
  # pde=tot[,pde:=mean(y.x<y.y),by=c("id","time")]
  
  # pde[is.na(pde),pde:=runif(length(pde),0,1)]
  
  
  idx<-which(pde$pde>0 & pde$pde<1)
  pde[,pde_wo_ties:=pde]
  pde[idx,pde_wo_ties:=pde_wo_ties+runif(length(idx),0,1/(nrep))]
  
  pde[pde_wo_ties==0,pde_wo_ties:=runif(sum(pde_wo_ties==0),0,1/(nrep))]
  pde[pde_wo_ties>=1,pde_wo_ties:=runif(sum(pde_wo_ties>=1),1-1/(nrep),1)]
  
  
  npde=pde
  npde[,npde := qnorm(pde_wo_ties)]
  npde$time=as.num(npde$time)
  names(npde)[names(npde)=="y.y"] = "dobs"
  
  
  
  return(npde)
}


calcnpde2<-function(obs_i,sim_i,nrep,decorr.method,verbose=FALSE,method="all_sim") {
  # print(names(sim_i))
  # time=unique(obs_i$time)
  matsim=dcast(sim_i,rep~time,drop=T,value.var="y") 
  matsim$rep=NULL
  
  
  matsim = matsim[apply((is.na(matsim)*1),1,sum)==0,]
  varsim=cov(matsim)#,use = "na.or.complete")
  
  
  moysim=apply(matsim,2,function(x) mean(x,na.rm = T))
  
  
  #computing V-1/2
  xerr<-0
  if(length(moysim)>1) {
    y<-switch(decorr.method,
              cholesky=decorr.chol(varsim),
              polar=decorr.polar(varsim),
              inverse=decorr.inverse(varsim))
    if(y$xerr==0) ymat<-y$y else xerr<-y$xerr
  } else ymat<-1/sqrt(varsim)
  if(xerr==0) {
    #decorrelation of the simulations
    mat = ((t(matsim)-moysim))
    # mat_without_na = apply(mat_with_na,2,function(x){
    #   x[is.na(x)]=0
    #   return(x)
    # })
    decsim<-t(ymat)%*%mat
    row.names(decsim)=obs_i$time
    # decsim<-t(ymat)%*%mat_with_na
    
    decobs<-t(ymat[1:dim(obs_i)[1],1:dim(obs_i)[1]])%*%(obs_i[["y"]]-moysim[1:dim(obs_i)[1]])
    row.names(decobs)=obs_i$time
    
    
    if(dim(obs_i)[1]==1) row.names(decobs)="0"
    ydsim<-data.table(t(decsim))
    ydsim$rep=1:nrep
    ydsim=melt(ydsim,variable.name = "time",value.name = "y",id.vars = "rep")
    #decorrelation of the observations
    # ydobs<-decobs
    ydobs = data.table(id=unique(obs_i$id),time=row.names(decobs),decobs)
    names(ydobs)[3]="y"
    ydobs[,time:=as.num(time)]
    ydsim[,time:=as.num(time)]
    
    tot = merge(ydsim,ydobs,by=c("time"))
    
    
    #Computing the pde
    pde=tot[,list(pde=mean(y.x<y.y),y.y=unique(y.y)),by=c("id","time")]
    # pde=tot[,pde:=mean(y.x<y.y),by=c("id","time")]
    
    pde[is.na(pde),pde:=runif(length(pde),0,1)]
    
    
    idx<-which(pde$pde>0 & pde$pde<1)
    pde[,pde_wo_ties:=pde]
    pde[idx,pde_wo_ties:=pde_wo_ties+runif(length(idx),0,1/(nrep))]
    
    pde[pde_wo_ties==0,pde_wo_ties:=runif(sum(pde_wo_ties==0),0,1/(nrep))]
    pde[pde_wo_ties>=1,pde_wo_ties:=runif(sum(pde_wo_ties>=1),1-1/(nrep),1)]
    
    
    npde=pde
    npde[,npde := qnorm(pde_wo_ties)]
    npde$time=as.num(npde$time)
    names(npde)[names(npde)=="y.y"] = "dobs"
    
  }
 
  return(npde)
}


calcnpd<-function(obs_i,sim_i,verbose=FALSE,method="all_sim") {
  # print(names(sim_i))
  # time=unique(obs_i$time)
  
    #Computing the pd
    # tot = merge()
    nrep = dim(sim_i)[1]
    obs_i$rep=NULL
    # names(obs_i)[3]="y"
    total_pd = merge(sim_i,obs_i,by=c("id","time"))
    total_pd[,nrep:=length(unique(rep)),by="time"]
    pd=total_pd[,list(pd=mean(y.x<y.y),nrep=unique(nrep)),by=c("id","time")]
    pd[is.na(pd),pd:=runif(length(pd),0,1)]
    ### pd_wo_ties
    idx<-which(pd$pd>0 & pd$pd<1)
    pd[,pd_wo_ties:=pd]
    pd[idx,pd_wo_ties:=pd_wo_ties+runif(length(idx),0,1/(nrep))]
    
    pd[pd_wo_ties==0,pd_wo_ties:=runif(sum(pd_wo_ties==0),0,1/(nrep))]
    pd[pd_wo_ties>=1,pd_wo_ties:=runif(sum(pd_wo_ties>=1),1-1/(nrep),1)]
    
    ### 
    npd=pd
    npd[,npd := qnorm(pd_wo_ties)]
    
    
    
    
  
  
  # npde = merge(npd,npde,by=c("id","time"))
  return(npd)
}



computenpde_eco = function(obs,sim,ties=T,response="y",verbose=F){
  
  colomn = c("id","time",response)
  if(is.data.table(obs)){
    obs_rearranged = data.frame(obs[,c(colomn,names(obs)[names(obs)%!in%colomn]),with=F])
    sim_rearranged = data.frame(sim[,c(colomn,names(sim)[names(sim)%!in%colomn]),with=F])
    
  }else{
    obs_rearranged = obs[,c(colomn,names(obs)[names(obs)%!in%colomn])]
    sim_rearranged = sim[,c(colomn,names(sim)[names(sim)%!in%colomn])]
  }
  
  npde = autonpde(obs_rearranged,sim_rearranged,iid = 1,ix = 2,iy = 3,boolsave = F,verbose = verbose,ties=ties)
  # plot(npde)
  test_adj = reject_stat(npde@results@res$npde)
  test_ks = ks.test(npde@results@res$npde,"pnorm")$p.val
  return(list(npde=npde,adjusted=test_adj,ks=test_ks))
}


main_compute_npde = function(obs,sim,type_outcome="continuous",
                             dependancies=NULL,verbose=F,options=NULL){
  
  
  
  noutcome=length(obs)
  
  if(is.null(dependancies)) dependancies = lapply(1:noutcome,function(x) 0 )
  
  
  res=list()
  for(o in 1:noutcome){
    
    #### Load data and sim ####
    # print(paste0("computation outcome: ", type_outcome[[o]]))
    obs_o = obs[[o]]
    
    
    sim_full_o = sim[[o]]
    sim_o = copy(sim_full_o)
    if(is.null(dependancies[[o]])) dependancies[[o]]=0
    if(dependancies[[o]]!=0) sim_dependancies = sim[[dependancies[[o]]]][,1:4,with=F]
    
    
    #### Load options ####
    order_cat = paste0(sort(unique(c(obs_o$y,sim_full_o$y))))
    # compute_pvalue=F
    censoring_type="right"
    ties=F
    compute_npde=T
    compute_npd=T
    decorr.method="cholesky"
    if(length(options)>0){
      if(!is.null(options[[o]])){
        opt = options[[o]]
        if("order_cat"%in%names(opt))  order_cat=opt[[which(names(opt)%in%"order_cat")]]
        # if("compute_pvalue"%in%names(opt))  compute_pvalue=opt[[which(names(opt)%in%"compute_pvalue")]]
        if("censoring_type"%in%names(opt))  censoring_type=opt[[which(names(opt)%in%"censoring_type")]]
        if("ties"%in%names(opt))  ties=opt[[which(names(opt)%in%"ties")]]
        if("compute_npde"%in%names(opt))  compute_npde=opt[[which(names(opt)%in%"compute_npde")]]
        if("compute_npd"%in%names(opt))  compute_npd=opt[[which(names(opt)%in%"compute_npd")]]
        if("decorr.method"%in%names(opt))  decorr.method=opt[[which(names(opt)%in%"decorr.method")]]
        
      }
    }
    
    
    #### Compute npde  ####
    if(type_outcome[o]=="continuous"){
      
      if(dependancies[[o]]==0){
        npde_o = computenpde(obs_o,sim_full_o,compute_npde = compute_npde,compute_npd = compute_npd,decorr.method = decorr.method,verbose=verbose)
      }else{
        names(sim_dependancies)[1:4]=c("rep","id","tte","status")
        sim_o = merge(sim_full_o,sim_dependancies,by=c("rep","id"))
        sim_o=sim_o[time<tte]
        
        npde_o = computenpde_missing(obs = obs_o,sim = sim_o,sim_full = sim_full_o,compute_npde = T,compute_npd = T,decorr.method = decorr.method,verbose=verbose)
        
        # stat = reject_stat(npde_o$npde,parametric = "F")
        
        # npde_o = list(npde_o,stat)
      }
      
      
    }else if(type_outcome[o]=="TTE"){
      
      if(dependancies[[o]]==0){
        npde_o = prediction_discrepancie(obs_o,sim_full_o,ties = ties,censoring_type=censoring_type)
      }else{
        
        # names(sim_dependancies)[1:4]=c("rep","id","tte","status")
        # sim_o = merge(sim_full_o,sim_dependancies,by=c("rep","id"))
        # sim_o=sim_o[time<tte]
        # 
        # npde_o = prediction_discrepancie(obs_o,sim_o,ties = ties,censoring_type=censoring_type)
        stop("Error: not  yet implemented !")
      }
      
      
      
    }else if(type_outcome[o]=="categorical"){
      
      if(dependancies[[o]]==0){
        
        
        
        npde_o <- compute_npde_discrete_m2(obs=obs_o,sim = sim_full_o,response = "y",order_cat = order_cat)
        
      }else{
        
        names(sim_dependancies)[1:4]=c("rep","id","tte","status")
        sim_o = merge(sim_full_o,sim_dependancies,by=c("rep","id"))
        sim_o=sim_o[time<tte]
        
        
        npde_o <- compute_npde_discrete_m2(obs=obs_o,sim = sim_o,response = "y",order_cat = order_cat)
        
      }
    }else if(type_outcome[o]=="prop_test"){
      npde_o <- compute_test_prop(obs=obs_o,sim = sim_full_o,order_cat = order_cat)
    }else{
      stop(paste0("Error: type of outcome not recognised. Must be continuous/categorical/TTE/prop_test. input: ",type_outcome[o]))
    }
    res[[o]] = list(npde_o,sim_o,sim_full_o)
  }
  
  return(res)
}



main_compute_pvalue = function(res,type_outcome="continuous",compute_pvalue=F,options=NULL){
  
  noutcome=length(res)
  res_test = list()
  if(length(compute_pvalue)==1) compute_pvalue = rep(compute_pvalue,noutcome)
  
  ##############  Computation of pvalue
  for(o in 1:noutcome){
    
    #### Load data and sim ####
    print(paste0("test for outcome: ", type_outcome[[o]]))
    if(compute_pvalue[[o]]){
      obs_o = res[[o]][[1]] ## obs + npde
      
      
      sim_full_o = res[[o]][[2]]  ## sim + VPC like
      
      order_cat = paste0(sort(unique(c(obs_o$y,sim_full_o$y))))
      if(length(options)>0){
        if(!is.null(options[[o]])){
          opt = options[[o]]
          if("order_cat"%in%names(opt))  order_cat=opt[[which(names(opt)%in%"order_cat")]]
        }
      }
      
      if(type_outcome[[o]]%in%c("continuous","TTE")){
        npde = res[[o]][[1]] ## obs + npde
        test = reject_stat(npde$npde,parametric = F)
        
        res_test[[o]] = test
      }else{
        
        npde_o <- res[[o]][[1]] ## obs + npde
        sim_o <- res[[o]][[2]] ## sim VPC like or full depending on dependancies
        
        npd_sim <- compute_npde_discrete_m2(obs=sim_o,sim = sim_o,response = "y",order_cat = order_cat)
        D_obs = ks.test(npde_o$npd,"pnorm")$statistic
        D = sapply(1:length(unique(npd_sim$rep)),function(x) ks.test(npd_sim[rep==x]$npd,"pnorm")$statistic[[1]])
        
        res_test[[o]] = list(all_sim=npd_sim,D=D,p.value=mean(D_obs<D))
        
      }
    }
    
    
  }
  
  pvalue = sapply(1:noutcome,function(x) ifelse(!is.null(res_test[[x]]$p.value),res_test[[x]]$p.value[[1]],NA ))
  global_pvalue = min(min(pvalue)*noutcome,1)
  res_test[[noutcome+1]]=global_pvalue
  
  
  return(res_test)
}




decorr.chol<-function(x) {
  xerr<-0
  xmat<-try(chol(x))
  if(is.numeric(xmat)) {
    ymat<-try(solve(xmat))
    if(!is.numeric(ymat)) 
      xerr<-2
  } else 
    xerr<-1
  return(list(y=ymat,xerr=xerr))
}

decorr.inverse<-function(x) {
  xerr<-0
  var.eig<-eigen(x)
  xmat<-try(var.eig$vectors %*% diag(sqrt(var.eig$values)) %*% solve(var.eig$vectors))
  if(is.numeric(xmat)) {
    ymat<-try(solve(xmat))
    if(!is.numeric(ymat)) 
      xerr<-2
  } else 
    xerr<-1
  return(list(y=ymat,xerr=xerr))
}

decorr.polar<-function(x) {
  xerr<-0
  xmat<-try(chol(x))
  if(is.numeric(xmat)) {
    svdec<-svd(xmat)
    umat<-svdec$u %*% t(svdec$v)
    vmat<-t(umat) %*% xmat
    ymat<-try(solve(vmat))
    if(!is.numeric(ymat)) 
      xerr<-2
  } else 
    xerr<-1
  return(list(y=ymat,xerr=xerr))
}

compute_typeIerror = function(sim,obs,response,method="all_sim",censored_data=T,compute_npde=T){
  
  
  for(j in 1:length(obs)){
    obs[[j]]$rep = as.num(obs[[j]]$rep)
    obs[[j]]$id = as.num(obs[[j]]$id)
    sim[[j]]$rep = as.num(sim[[j]]$rep)
    sim[[j]]$id = as.num(sim[[j]]$id)
  }
  
  
  pb = txtProgressBar(min = 0, max = 200, style = 3)
  typeIerror_censored = list(longitudinal = list(adjusted = list(c()), ks=c()),
                             TTE = list(adjusted = list(), ks=c()))
  
  if(censored_data){
    obs$data=obs$psa_with_dropout
    print("obs$psa_with_dropout")
  }else{
    print("obs$psa_wo_dropout")
    obs$data=obs$psa_wo_dropout
  }
  
  if(compute_npde){
    sim2 = sim$psa_wo_dropout
    print("sim$psa_wo_dropout")
  }else{
    sim2 = sim$psa_with_dropout
    print("sim$psa_with_dropout")
  }

  # if(method=="all_sim") 
  sim2 = sim$psa_wo_dropout
  # if(method=="all_sim"){
  #   sim2 = sim$psa_wo_dropout
  #   print("sim$psa_wo_dropout")
  # }else{
  #   sim2 = sim$psa_with_dropout
  #   print("sim$psa_with_dropout")
  # }
  
  for(i_in_obs in 1:200){
    
    
    setTxtProgressBar(pb, i_in_obs)
    
    # #### Censored event time  
    #   
    # system.time(npde_m1 <- computenpde2(sim = sim$hamd_with_dropout,obs=obs$hamd_with_dropout[rep==i],response = "hamd17",verbose=F))
    npde <- computenpde_missing(sim = sim2,obs=obs$data[rep==i_in_obs],response = response,verbose=F,method=method,compute_npde = compute_npde)
    npde_TTE = prediction_discrepancie3(dat = obs$TTE[rep==i_in_obs],simulated_data =sim$TTE,ties = F,test_cens=T,response = ifelse(censored_data,"OFT","TFT"))
    
    # p1 = (gg_qq(npde$npde)$p)
    # p2 = plot_npde_time(npde)
    
    if(compute_npde){
      typeIerror_censored$longitudinal$ks = c(typeIerror_censored$longitudinal$ks,ks.test(npde$npde,y = "pnorm")$p.val)
      typeIerror_censored$longitudinal$adjusted[i_in_obs] = list(reject_stat(npde$npde)$p.value)
    }else{
      typeIerror_censored$longitudinal$ks = c(typeIerror_censored$longitudinal$ks,ks.test(npde$npd,y = "pnorm")$p.val)
      typeIerror_censored$longitudinal$adjusted[i_in_obs] = list(reject_stat(npde$npd)$p.value)
    }
    
    
    
    typeIerror_censored$TTE$ks = c(typeIerror_censored$TTE$ks,npde_TTE$test_ks$p.value)
    typeIerror_censored$TTE$adjusted[i_in_obs] = list(npde_TTE$test_adj$p.value)
    
  }
  typeIerror_censored
}

compute_typeIerror_test = function(sim,obs,response,method="all_sim",censored_data=T,compute_npde=T){
  
  
  for(j in 1:length(obs)){
    obs[[j]]$rep = as.num(obs[[j]]$rep)
    obs[[j]]$id = as.num(obs[[j]]$id)
    sim[[j]]$rep = as.num(sim[[j]]$rep)
    sim[[j]]$id = as.num(sim[[j]]$id)
  }
  
  
  pb = txtProgressBar(min = 0, max = 200, style = 3)
  typeIerror_censored = list(longitudinal = list(adjusted = list(c()), ks=c()),
                             TTE = list(adjusted = list(), ks=c()))
  
  
  obs$data=obs$psa_with_dropout
  
  sim2 = sim$psa_wo_dropout
 
  for(i_in_obs in 1:200){
    
    
    setTxtProgressBar(pb, i_in_obs)
    
    # #### Censored event time  
    #   
    # system.time(npde_m1 <- computenpde2(sim = sim$hamd_with_dropout,obs=obs$hamd_with_dropout[rep==i],response = "hamd17",verbose=F))
    npde <- computenpde_missing_test(sim = sim2,obs=obs$data[rep==i_in_obs],response = response,verbose=F,method=method,compute_npde = compute_npde)
    npde_TTE = prediction_discrepancie3(dat = obs$TTE[rep==i_in_obs],simulated_data =sim$TTE,ties = F,test_cens=T,response = ifelse(censored_data,"OFT","TFT"))
    
    # p1 = (gg_qq(npde$npde)$p)
    # p2 = plot_npde_time(npde)
    
    if(compute_npde){
      typeIerror_censored$longitudinal$ks = c(typeIerror_censored$longitudinal$ks,ks.test(npde$npde,y = "pnorm")$p.val)
      typeIerror_censored$longitudinal$adjusted[i_in_obs] = list(reject_stat(npde$npde)$p.value)
    }else{
      typeIerror_censored$longitudinal$ks = c(typeIerror_censored$longitudinal$ks,ks.test(npde$npd,y = "pnorm")$p.val)
      typeIerror_censored$longitudinal$adjusted[i_in_obs] = list(reject_stat(npde$npd)$p.value)
    }
    
    
    
    typeIerror_censored$TTE$ks = c(typeIerror_censored$TTE$ks,npde_TTE$test_ks$p.value)
    typeIerror_censored$TTE$adjusted[i_in_obs] = list(npde_TTE$test_adj$p.value)
    
  }
  typeIerror_censored
}





compute_typeIerror_test_keep_npde = function(sim,obs,response,method="all_sim",censored_data=T,compute_npde=T){
  
  
  for(j in 1:length(obs)){
    obs[[j]]$rep = as.num(obs[[j]]$rep)
    obs[[j]]$id = as.num(obs[[j]]$id)
    sim[[j]]$rep = as.num(sim[[j]]$rep)
    sim[[j]]$id = as.num(sim[[j]]$id)
  }
  obs$data=obs$psa_with_dropout
  
  pb = txtProgressBar(min = 0, max = length(unique(obs$data$rep)), style = 3)
  
  
 
  
  
  typeIerror_censored = list(longitudinal = list(adjusted = list(c()), ks=c()),
                             TTE = list(adjusted = list(), ks=c()))
  
  obs$data[,c("dobs","pd","pd_wo_ties","npd","pde","pde_wo_ties","npde","pd_TTE","pd_wo_ties_TTE","npde_TTE"):=-99]
  sim2 = sim$psa_wo_dropout
  
  
  for(i_in_obs in 1:length(unique(obs$data$rep))){
    
    
    setTxtProgressBar(pb, i_in_obs)
    
    # #### Censored event time  
    #   
    # system.time(npde_m1 <- computenpde2(sim = sim$hamd_with_dropout,obs=obs$hamd_with_dropout[rep==i],response = "hamd17",verbose=F))
    npde <- computenpde_missing_test(sim = sim2,obs=obs$data[rep==i_in_obs,names(obs$data)[names(obs$data)%!in%paste0(c("pd","pd_wo_ties","npde"),"_TTE")],with=F],
                                     response = response,verbose=F,method=method,compute_npde = compute_npde)
    npde_TTE = prediction_discrepancie3(dat = obs$TTE[rep==i_in_obs],simulated_data =sim$TTE,ties = F,test_cens=T,response = ifelse(censored_data,"OFT","TFT"))
    
    temp = npde_TTE$tab[,c("rep","id","pd","pd_wo_ties","npde"),with=F]
    names(temp)[3:5]=paste0(names(temp)[3:5],"_TTE")
    npde[,id:=as.num(id)]
    temp[,id:=as.num(id)]
    npde = merge(npde,temp,by=c("rep","id"))
    
    obs$data[rep==i_in_obs]=npde
    # p1 = (gg_qq(npde$npde)$p)
    # p2 = plot_npde_time(npde)
    
    if(compute_npde){
      typeIerror_censored$longitudinal$ks = c(typeIerror_censored$longitudinal$ks,ks.test(npde$npde,y = "pnorm")$p.val)
      typeIerror_censored$longitudinal$adjusted[i_in_obs] = list(reject_stat(npde$npde)$p.value)
    }else{
      typeIerror_censored$longitudinal$ks = c(typeIerror_censored$longitudinal$ks,ks.test(npde$npd,y = "pnorm")$p.val)
      typeIerror_censored$longitudinal$adjusted[i_in_obs] = list(reject_stat(npde$npd)$p.value)
    }
    
    
    
    typeIerror_censored$TTE$ks = c(typeIerror_censored$TTE$ks,npde_TTE$test_ks$p.value)
    typeIerror_censored$TTE$adjusted[i_in_obs] = list(npde_TTE$test_adj$p.value)
    
  }
  typeIerror_censored$npde=obs$data
  return(typeIerror_censored)
}



compute_typeIerror_test2 = function(sim,obs,response,method="all_sim",censored_data=T,compute_npde=T){
  
  
  for(j in 1:length(obs)){
    obs[[j]]$rep = as.num(obs[[j]]$rep)
    obs[[j]]$id = as.num(obs[[j]]$id)
    sim[[j]]$rep = as.num(sim[[j]]$rep)
    sim[[j]]$id = as.num(sim[[j]]$id)
  }
  
  
  pb = txtProgressBar(min = 0, max = 200, style = 3)
  typeIerror_censored = list(longitudinal = list(adjusted = list(c()), ks=c()),
                             TTE = list(adjusted = list(), ks=c()))
  
  
  obs$data=obs$psa_with_dropout
  
  sim2 = sim$psa_wo_dropout
  
  for(i_in_obs in 1:200){
    
    
    setTxtProgressBar(pb, i_in_obs)
    
    # #### Censored event time  
    #   
    # system.time(npde_m1 <- computenpde2(sim = sim$hamd_with_dropout,obs=obs$hamd_with_dropout[rep==i],response = "hamd17",verbose=F))
    npde <- computenpde_missing_test2(sim = sim2,obs=obs$data[rep==i_in_obs],response = response,verbose=F,method=method,compute_npde = compute_npde)
    npde_TTE = prediction_discrepancie3(dat = obs$TTE[rep==i_in_obs],simulated_data =sim$TTE,ties = F,test_cens=T,response = ifelse(censored_data,"OFT","TFT"))
    
    # p1 = (gg_qq(npde$npde)$p)
    # p2 = plot_npde_time(npde)
    
    if(compute_npde){
      typeIerror_censored$longitudinal$ks = c(typeIerror_censored$longitudinal$ks,ks.test(npde$npde,y = "pnorm")$p.val)
      typeIerror_censored$longitudinal$adjusted[i_in_obs] = list(reject_stat(npde$npde)$p.value)
    }else{
      typeIerror_censored$longitudinal$ks = c(typeIerror_censored$longitudinal$ks,ks.test(npde$npd,y = "pnorm")$p.val)
      typeIerror_censored$longitudinal$adjusted[i_in_obs] = list(reject_stat(npde$npd)$p.value)
    }
    
    
    
    typeIerror_censored$TTE$ks = c(typeIerror_censored$TTE$ks,npde_TTE$test_ks$p.value)
    typeIerror_censored$TTE$adjusted[i_in_obs] = list(npde_TTE$test_adj$p.value)
    
  }
  typeIerror_censored
}


compute_typeIerror_cox = function(path_res,cens=T,ties=F){
  
  pval = NULL
  pb = txtProgressBar(min = 0, max = 200, style = 3)
  indiv_fit = NULL
  for(rep in 1:200){
    setTxtProgressBar(pb, rep)
    data = fread(paste0(path_res,"/rep",rep,"/rep.txt"),na.strings = ".")
    p = fread(paste0(path_res,"/rep",rep,"/m/tables.txt"))
    names(p) = c("time","id","coxsnell_SAEM","coxsnell_mode")
    p$rep=rep
    
    temp = data[YTYPE==2&time>0,c("id","y"),with=F]
    p = merge(p,temp,by="id")
    indiv_fit = rbind(indiv_fit,p)
  }
  
  indiv_fit[,coxsnell:=coxsnell_mode[time==max(time)],by=c("rep","id")]
  
  final = indiv_fit[,c("rep","id","coxsnell","y"),with=F]
  setkeyv(final,c("id","rep"))
  final = unique(final)
  
  for(i in 1:200){
    
    setTxtProgressBar(pb, i)
    
    temp = final[rep==i]
    temp$coxsnell_imp = temp$coxsnell
    temp[y==0,coxsnell_imp:=-log(runif(length(coxsnell_imp),0,exp(-coxsnell_imp)))]
    if(!ties) temp[duplicated(coxsnell_imp),coxsnell_imp:=coxsnell_imp+runif(length(coxsnell_imp),0,1/(length(coxsnell_imp)-1))]
    
    pval = c(pval,ks.test(temp$coxsnell_imp,"pexp")$p.val)
  }
  
  # save(pval,file = paste0(path_res,"pval_coxsnell.rda"))
  return(pval)
  
}

typeIerror = function(res,alpha=rep(0.05,4)){
  tte_adj_mean = mean(sapply(1:200,function(x) res$TTE$adjusted[[x]][1])<=alpha[1]/6,na.rm = T)
  tte_adj_var = mean(sapply(1:200,function(x) res$TTE$adjusted[[x]][2])<=alpha[1]/6,na.rm = T)
  tte_adj_norm = mean(sapply(1:200,function(x) res$TTE$adjusted[[x]][3])<=alpha[1]/6,na.rm = T)
  tte_adj = mean(sapply(1:200,function(x) res$TTE$adjusted[[x]][4])<=alpha[1],na.rm = T)
  tte_ks = mean(res$TTE$ks<=alpha[2],na.rm = T)
  if(is.null(res$longitudinal$ks)){
    l_adj=NA
    l_ks=NA
    combi_adj=NA
    combi_ks=NA
  }else{
    l_adj_mean = mean(sapply(1:200,function(x) res$longitudinal$adjusted[[x]][1])<=alpha[3]/6,na.rm = T)
    l_adj_var = mean(sapply(1:200,function(x) res$longitudinal$adjusted[[x]][2])<=alpha[3]/6,na.rm = T)
    l_adj_norm = mean(sapply(1:200,function(x) res$longitudinal$adjusted[[x]][3])<=alpha[3]/6,na.rm = T)
    l_adj = mean(sapply(1:200,function(x) res$longitudinal$adjusted[[x]][4])<=alpha[3],na.rm = T)
    l_ks = mean(res$longitudinal$ks<=alpha[4],na.rm = T)
    
    # l = sapply(1:200,function(x) res$longitudinal$adjusted[[x]][4])
    # tte = sapply(1:200,function(x) res$TTE$adjusted[[x]][4])
    # tmp = data.table(rep=1:200,l=l,tte=tte)
    # tmp[,pvalue:=min(1,min(c(l,tte)*2)),by="rep"]
    # combi_adj=mean(tmp$pvalue<=0.05,na.rm=T)
    # 
    # l = sapply(1:200,function(x) res$longitudinal$ks[x])
    # tte = sapply(1:200,function(x) res$TTE$ks[x])
    # tmp = data.table(rep=1:200,l=l,tte=tte)
    # tmp[,pvalue:=min(1,min(c(l,tte)*2)),by="rep"]
    # combi_ks=mean(tmp$pvalue<=0.05,na.rm=T)
    
    
    combi_adj = mean(sapply(1:200,function(x) res$TTE$adjusted[[x]][4]<= (alpha[1]/2)|res$longitudinal$adjusted[[x]][4]<= (alpha[3]/2)),na.rm = T)
    combi_ks = mean(sapply(1:200,function(x) res$TTE$ks[x] <= (alpha[1]/2) | res$longitudinal$ks[x]<=(alpha[3]/2)),na.rm = T)
  }
  
  # combi_ks = mean(sapply(1:200,function(x) min(res$TTE$ks[x],res$longitudinal$ks[x])*2)<=min(alpha[1],alpha[3]),na.rm = T)
  return(list(tte_adj=tte_adj,tte_ks=tte_ks,l_adj=l_adj,l_ks=l_ks,combi_adj=combi_adj,combi_ks=combi_ks,
              l_adj_mean=l_adj_mean,
              l_adj_var=l_adj_var,
              l_adj_norm=l_adj_norm,
              tte_adj_mean=tte_adj_mean,
              tte_adj_var=tte_adj_var,
              tte_adj_norm=tte_adj_norm))
}

typeIerror_fast = function(res,alpha=rep(0.05,4)){
  
  
  res[,combi_adj:=min(1,min(longitudinal_adj,TTE_adj)*2),by="rep"]
  res[,combi_ks:=min(1,min(longitudinal_ks,TTE_ks)*2),by="rep"]
  
  final=res[,list(power_adj=mean(combi_adj<0.05),
            power_ks=mean(combi_ks<0.05))]
  
  # combi_ks = mean(sapply(1:200,function(x) min(res$TTE$ks[x],res$longitudinal$ks[x])*2)<=min(alpha[1],alpha[3]),na.rm = T)
  return(final)
}

typeIerror_v2 = function(res,alpha=rep(0.05,4)){
  tte_adj = mean(sapply(1:200,function(x) res$TTE$adjusted[[x]][4])<=alpha[1],na.rm = T)
  tte_ks = mean(res$TTE$ks<=alpha[2],na.rm = T)
  if(is.null(res$longitudinal$ks)){
    l_adj=NA
    l_ks=NA
    combi_adj=NA
    combi_ks=NA
  }else{
    l_adj = mean(sapply(1:200,function(x) res$longitudinal$adjusted[[x]][4])<=alpha[3],na.rm = T)
    l_ks = mean(res$longitudinal$ks<=alpha[4],na.rm = T)
    combi_adj = mean(sapply(1:200,function(x) min(res$TTE$adjusted[[x]][4],res$longitudinal$adjusted[[x]][4])<= (alpha[1]/2)),na.rm = T)
    combi_ks = mean(sapply(1:200,function(x) min(res$TTE$ks[x],res$longitudinal$ks[x])<=(alpha[3]/2)),na.rm = T)
  }
  
  # combi_ks = mean(sapply(1:200,function(x) min(res$TTE$ks[x],res$longitudinal$ks[x])*2)<=min(alpha[1],alpha[3]),na.rm = T)
  return(list(tte_adj=tte_adj,tte_ks=tte_ks,l_adj=l_adj,l_ks=l_ks,combi_adj=combi_adj,combi_ks=combi_ks))
}


lancement_power = function(template,n_launch,name,nk,Krep,N,plage_obs,plage_sim,method){
  combi=1
  newfile = template
  for(type in 1:length(name)){
    for(ntime in nk[[type]]){
      for(K in Krep[[type]]){
        for(n_subject in N[[type]]){
          for(param_obs in plage_obs[[type]]){
            path_obs = paste(c(paste0("ntime_",ntime),paste0("K_",K),paste0("N_",n_subject),paste0(name[type],"_",param_obs),""),collapse = "/")
            newfile[template=='path_obs = flag_path_obs'] = paste0("path_obs='",path_obs,"'")
            newfile[template=='param_obs=flag'] = paste0("param_obs='",param_obs,"'")
            for(param_sim in plage_sim[[type]]){
              path_sim = paste(c(paste0("ntime_",ntime),paste0("K_",K),paste0("N_",n_subject),paste0(name[type],"_",param_sim),""),collapse = "/")
              newfile[template=='path_sim = flag_path_sim'] =paste0("path_sim='",path_sim,"'")
              newfile[template=='param_sim=flag'] = paste0("param_sim='",param_sim,"'")
              for(m in method[[type]]){
                newfile[template=="method=0"] = paste0("method=",m)
                path_res = path_obs
                newfile[template=='path_res = flag_path_res'] =paste0("path_res='",path_res,"'")
                # if(!file.exists(paste0(path_res,"/power_",param_obs,"_",param_sim,"_m",method,".rda"))){
                #   creation_launch_file(newfile,param=c(ntime,K,n_subject,Drem_obs,Drem_sim,method),
                #                        path_res=path_res,n_launch=n_launch)
                # }
                combi = combi + 1
              }
            }
          }
        }
      }
    } 
  }
  
  pb = txtProgressBar(min = 0, max = combi, style = 3)
  combi=1
  for(type in 1:length(name)){
    for(ntime in nk[[type]]){
      for(K in Krep[[type]]){
        for(n_subject in N[[type]]){
          for(param_obs in plage_obs[[type]]){
            path_obs = paste(c(paste0("ntime_",ntime),paste0("K_",K),paste0("N_",n_subject),paste0(name[type],"_",param_obs),""),collapse = "/")
            newfile[template=='path_obs = flag_path_obs'] = paste0("path_obs='",path_obs,"'")
            newfile[template=='param_obs=flag'] = paste0("param_obs='",param_obs,"'")
            for(param_sim in plage_sim[[type]]){
              path_sim = paste(c(paste0("ntime_",ntime),paste0("K_",K),paste0("N_",n_subject),paste0(name[type],"_",param_sim),""),collapse = "/")
              newfile[template=='path_sim = flag_path_sim'] =paste0("path_sim='",path_sim,"'")
              newfile[template=='param_sim=flag'] = paste0("param_sim='",param_sim,"'")
              for(m in method[[type]]){
                setTxtProgressBar(pb, combi)
                newfile[template=="method=0"] = paste0("method=",m)
                path_res = path_obs
                newfile[template=='path_res = flag_path_res'] =paste0("path_res='",path_res,"'")
                if(!file.exists(paste0(path_res,"/power_",param_obs,"_",param_sim,"_m",m,".rda"))){
                  creation_launch_file(newfile,param=c(ntime,K,n_subject,param_obs,param_sim,m),
                                       path_res=path_res,n_launch=n_launch)
                   print(paste0(path_res,"/power_",param_obs,"_",param_sim,"_m",m,".rda"))
                }
                combi = combi + 1
              }
            }
          }
        }
      }
    } 
  }
  
  close(pb) 
}      

lancement_power_cox = function(template,n_launch,name,nk,Krep,N,plage_obs,plage_sim){
  combi=1
  newfile = template
  for(type in 1:length(name)){
    for(ntime in nk[[type]]){
      for(K in Krep[[type]]){
        for(n_subject in N[[type]]){
          for(param_obs in plage_obs[[type]]){
            for(param_sim in plage_sim[[type]]){#[plage_sim[[type]]>=param_obs]){
              path_res = paste(c(paste0("../monolix/ntime_",ntime),paste0("K_",K),
                                 paste0("N_",n_subject),paste0(name[type],"_obs_",param_obs),paste0(name[type],"_sim_",param_sim),""),collapse = "/")
              if(!file.exists(paste0(path_res,"/power.rda")))  combi = combi + 1
            }
          }
        }
      }
    } 
  }
  
  pb = txtProgressBar(min = 1, max = combi, style = 3)
  combi=1
  for(type in 1:length(name)){
    for(ntime in nk[[type]]){
      for(K in Krep[[type]]){
        for(n_subject in N[[type]]){
          for(param_obs in plage_obs[[type]]){
            for(param_sim in plage_sim[[type]]){#[plage_sim[[type]]>=param_obs]){
              
              setTxtProgressBar(pb, combi)
              
              path_res = paste(c(paste0("../monolix/ntime_",ntime),paste0("K_",K),
                                 paste0("N_",n_subject),paste0(name[type],"_obs_",param_obs),paste0(name[type],"_sim_",param_sim),""),collapse = "/")
              
              newfile[template=='path_res = flag_path_res'] =paste0("path_res='",path_res,"'")
              
              if(!file.exists(paste0(path_res,"/power.rda"))){
                creation_launch_file(newfile,param=c(ntime,K,n_subject,param_obs,param_sim),
                                     path_res=path_res,n_launch=n_launch)
                combi = combi + 1
              }
              
              
            }
          }
        }
      }
    } 
  }
  
  close(pb) 
}      


##############################
### Graph

plot_npde_time = function(tab){
  # tab=data.frame(tab)
  quant_npde = ddply(tab,"time",summarise,
                     q5=quantile(npde,0.05),q50=quantile(npde,0.5),q95=quantile(npde,0.95))
  
  temp_th = gg_qq(tab$npde)
  th = temp_th$tab[temp_th$ord,]
  names(th)[1]="npde"
  th_fin = merge(tab,th,by="npde")
  quant_th_med = ddply(th_fin,"time",summarise,
                       q5=quantile(z,0.05),q50=quantile(z,0.5),q95=quantile(z,0.95))
  quant_th_low = melt(ddply(th_fin,"time",summarise,
                            q5=quantile(lower,0.05),q50=quantile(lower,0.5),q95=quantile(lower,0.95)),id.vars="time")
  quant_th_up = melt(ddply(th_fin,"time",summarise,
                           q5=quantile(upper,0.05),q50=quantile(upper,0.5),q95=quantile(upper,0.95)),id.vars="time")
  # quant_th_med$type="med"
  # quant_th_low$type="low"
  names(quant_th_low)[3]="low"
  names(quant_th_up)[3]="up"
  # quant_th_up$type="up"
  ribbon = merge(quant_th_low,quant_th_up,by=c("time","variable"))
  
  
  p4 = ggplot() + geom_point(data=tab,aes(x=time,y=npde)) +theme_bw() + scale_x_continuous(name="Time") +
    geom_line(data=data.frame(melt(data.table(quant_npde),id.vars = "time")),
              aes(x=time,y=value,group=variable,color=variable)) + 
    # geom_line(data=data.frame(melt(data.table(quant_th_med),id.vars = "time")),
    # aes(x=time,y=value,group=variable,color=variable),lty=2) +
    geom_ribbon(data=ribbon,aes(x=time,ymin=low,ymax=up,group=variable,fill=variable),alpha=0.4) + 
    scale_fill_manual(values = c("blue","red","blue"))+
    scale_color_manual(values = c("blue","red","blue"))
  
  return(p4)
}
findClosest <- function(vec, val){
  tmp <- abs(val-vec)
  idx=which(tmp==min(tmp))[1]
  # [~,idx] <- min(tmp)
  return(idx)
}
plot_npde_scatter_dt_old = function(tab,variable="OFT",type="TTE",outcome="npde",adj=T,bin=F){
  if(type!="TTE"){
    
    # tab=npde_L
    
    if(bin){
      tab$old_var=tab[[variable[1]]]
      bounds = auto_bin(npde_L[[variable[1]]],plot.opt=list(bin.method="equal",
                                                            bin.numer=NULL,
                                                            bin.breaks=NULL,
                                                            bin.extreme=NULL,
                                                            xlog=NULL),verbose=FALSE)$xbound 
      tab[[variable[1]]] = sapply(tab$old_var,function(x) bounds[findClosest(bounds,x)])
    }
    
    
    quant_npde = tab[,list(q5=quantile(get(outcome),0.05),
                           q50=quantile(get(outcome),0.5),
                           q95=quantile(get(outcome),0.95)),by=c(variable)]
    # quant_npde = melt(quant_npde,id.vars=variable)
    # temp_th = gg_qq(tab[[outcome]])
    # th = temp_th$tab[temp_th$ord,]
    # names(th)[1]=outcome
    # th_fin = merge(tab,th,by=outcome)
    # 
    # quant_th_med = th_fin[,list(q5=quantile(z,0.05),q50=quantile(z,0.5),q95=quantile(z,0.95)),by=variable]
    # quant_th_low = melt(th_fin[,list(q5=quantile(lower,0.05),q50=quantile(lower,0.5),q95=quantile(lower,0.95)),by=variable],id.vars = variable,variable.name = "cat")
    # quant_th_up = melt(th_fin[,list(q5=quantile(upper,0.05),q50=quantile(upper,0.5),q95=quantile(upper,0.95)),by=variable],id.vars = variable,variable.name = "cat")
    # # quant_th_med$type="med"
    # # quant_th_low$type="low"
    # names(quant_th_low)[length(variable)+2]="low"
    # names(quant_th_up)[length(variable)+2]="up"
    # quant_th_up$type="up"
    
    
    
    
    
    # ribbon = merge(quant_th_low,quant_th_up,by=c(variable,"cat"))
    
    if(length(variable)==1){
      huron = tab[,list(N=length(unique(id))),by=c(variable)]
      huron$p1=0.05
      huron$p2=0.5
      huron$p3=0.95
      huron[,q5.5:=qnorm(p1-qnorm(0.95)*sqrt(p1*(1-p1)/N)),by=variable]
      huron[,q5.95:=qnorm(p1+qnorm(0.95)*sqrt(p1*(1-p1)/N)),by=variable]
      
      huron[,q50.5:=qnorm(p2-qnorm(0.95)*sqrt(p2*(1-p2)/N)),by=variable]
      huron[,q50.95:=qnorm(p2+qnorm(0.95)*sqrt(p2*(1-p2)/N)),by=variable]
      
      huron[,q95.5:=qnorm(p3-qnorm(0.95)*sqrt(p3*(1-p3)/N)),by=variable]
      huron[,q95.95:=qnorm(p3+qnorm(0.95)*sqrt(p3*(1-p3)/N)),by=variable]
      
      # huron = tab[,list(N=length(unique(id))),by=c(variable)]
      # 
      # huron[,q5.5:=compute.bands(N)[[1]][,1],by=c(variable)]
      # huron[,q5.95:=compute.bands(N)[[2]][,1],by=c(variable)]
      # 
      # huron[,q50.5:=compute.bands(N)[[1]][,2],by=c(variable)]
      # huron[,q50.95:=compute.bands(N)[[2]][,2],by=c(variable)]
      # 
      # huron[,q95.5:=compute.bands(N)[[1]][,3],by=c(variable)]
      # huron[,q95.95:=compute.bands(N)[[2]][,3],by=c(variable)]
      # p4 = ggplot() + geom_point(data=th_fin,aes(x=get(variable[1]),y=get(outcome))) +theme_bw() + scale_x_continuous(name=variable[1]) +
      #   geom_line(data=melt(quant_npde,id.vars = variable,variable.name = "cat"),
      #             aes(x=get(variable[1]),y=value,group=cat,color=cat)) +
      #   geom_ribbon(data=ribbon,aes(x=get(variable[1]),ymin=low,ymax=up,group=cat,fill=cat),alpha=0.4) + 
      #   scale_fill_manual(values = c("blue","red","blue"))+
      #   scale_color_manual(values = c("blue","red","blue"))
      # 
      p4 = ggplot() +theme_bw() + scale_x_continuous(name=variable[1]) +
        # geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)),se = F,col=2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q5.5,ymax=q5.95),fill="blue",alpha=0.2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q50.5,ymax=q50.95),fill="red",alpha=0.2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q95.5,ymax=q95.95),fill="blue",alpha=0.2)+
        geom_line(data=melt(quant_npde,id.vars = variable,variable.name = "cat"),
                  aes(x=get(variable[1]),y=value,group=cat,lty=cat))+
        geom_point(data=tab,aes(x=get(variable[1]),y=get(outcome)))+
        # geom_ribbon(data=th_fin,aes(x=get(variable[1]),ymin=lower-z,ymax=upper-z),alpha=0.4);p4 #+ 
        # scale_fill_manual(values = c("blue","red","blue"))+
        scale_linetype_manual(values = c(2,1,2));p4
    }else{
      huron = tab[,list(N=length(unique(id))),by=c(variable)]
      huron$p1=0.05
      huron$p2=0.5
      huron$p3=0.95
      huron[,q5.5:=qnorm(p1-qnorm(0.95)*sqrt(p1*(1-p1)/N)),by=variable]
      huron[,q5.95:=qnorm(p1+qnorm(0.95)*sqrt(p1*(1-p1)/N)),by=variable]
      
      huron[,q50.5:=qnorm(p2-qnorm(0.95)*sqrt(p2*(1-p2)/N)),by=variable]
      huron[,q50.95:=qnorm(p2+qnorm(0.95)*sqrt(p2*(1-p2)/N)),by=variable]
      
      huron[,q95.5:=qnorm(p3-qnorm(0.95)*sqrt(p3*(1-p3)/N)),by=variable]
      huron[,q95.95:=qnorm(p3+qnorm(0.95)*sqrt(p3*(1-p3)/N)),by=variable]
      
      p4 = ggplot() + theme_bw() + scale_x_continuous(name=variable[1]) +
        geom_line(data=melt(quant_npde,id.vars = variable,variable.name = "cat"),
                  aes(x=get(variable[1]),y=value,group=cat,color=cat)) +
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q5.5,ymax=q5.95),fill="blue",alpha=0.2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q50.5,ymax=q50.95),fill="red",alpha=0.2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q95.5,ymax=q95.95),fill="blue",alpha=0.2)+
        facet_wrap(reformulate(".",variable[2:length(variable)]))
        # geom_line(data=data.frame(melt(data.table(quant_th_med),id.vars = "time")),
        # aes(x=time,y=value,group=variable,color=variable),lty=2) +
        # geom_ribbon(data=ribbon,aes(x=get(variable[1]),ymin=low,ymax=up,group=cat,fill=cat),alpha=0.4) + 
        # scale_fill_manual(values = c("blue","red","blue"))+
        # scale_color_manual(values = c("blue","red","blue"))
    }
  }else{
    
    
    temp_th = gg_qq(tab[[outcome]])
    th = temp_th$tab[temp_th$ord,]
    names(th)[1]=outcome
    th_fin = merge(tab,th,by=outcome)
    
    # quant_th_med = th_fin[,list(q5=quantile(z,0.05),q50=quantile(z,0.5),q95=quantile(z,0.95)),by=variable]
    # quant_th_low = melt(th_fin[,list(q5=quantile(lower,0.05),q50=quantile(lower,0.5),q95=quantile(lower,0.95)),by=variable],id.vars = variable,variable.name = "cat")
    # quant_th_up = melt(th_fin[,list(q5=quantile(upper,0.05),q50=quantile(upper,0.5),q95=quantile(upper,0.95)),by=variable],id.vars = variable,variable.name = "cat")
    # quant_th_med$type="med"
    # quant_th_low$type="low"
    # names(quant_th_low)[length(variable)+2]="low"
    # names(quant_th_up)[length(variable)+2]="up"
    # quant_th_up$type="up"
    # ribbon = merge(quant_th_low,quant_th_up,by=c(variable,"cat"))
    
    if(length(variable)==1){
      if(adj){
        p4 = ggplot(data=th_fin,aes(x=get(variable[1]))) + geom_point(aes(y=get(outcome)-z,col=status)) +theme_bw() + scale_x_continuous(name=variable[1]) +
          # geom_point(data=npde_TTE$tab,aes(x=imputed_time,y=npde,col=status))+
          scale_color_manual(name="Imputed",values=c("red","black"))+
          # geom_line(data=) +
          # geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)-z),se = F,col=2)+
          geom_line(aes(y=z-z),lty=2,col="blue") +
          geom_ribbon(aes(ymin=lower-z,ymax=upper-z,fill="th"),alpha=0.2);p4
      }else{
        p4 = ggplot(data=th_fin,aes(x=get(variable[1]))) + geom_point(aes(y=get(outcome),col=status)) +theme_bw() + scale_x_continuous(name=variable[1]) +
          # geom_point(data=npde_TTE$tab,aes(x=imputed_time,y=npde,col=status))+
          scale_color_manual(name="Imputed",values=c("red","black"))+
          # geom_line(data=) +
          # geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)-z),se = F,col=2)+
          geom_line(aes(y=z),lty=2,col="blue") +
          geom_ribbon(aes(ymin=lower,ymax=upper,fill="th"),alpha=0.2);p4
      }
    
    }else{
      if(adj){
        p4 = ggplot(data=th_fin) + geom_point(aes(x=get(variable[1]),y=get(outcome)-z,col=status)) +theme_bw() + scale_x_continuous(name=variable[1]) +
          scale_color_manual(name="Imputed",values=c("red","black"))+
          geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)-z),se = F,col="blue")+
          geom_line(aes(x=get(variable[1]),y=z-z),col="blue") +
          geom_ribbon(aes(x=get(variable[1]),ymin=lower-z,ymax=upper-z,fill="th"),alpha=0.2) +
          facet_wrap(reformulate(".",variable[2:length(variable)]))#+
      }else{
        p4 = ggplot(data=th_fin) + geom_point(aes(x=get(variable[1]),y=get(outcome),col=status)) +theme_bw() + scale_x_continuous(name=variable[1]) +
          scale_color_manual(name="Imputed",values=c("red","black"))+
          geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)),se = F,col="blue")+
          geom_line(aes(x=get(variable[1]),y=z),col="blue") +
          geom_ribbon(aes(x=get(variable[1]),ymin=lower,ymax=upper),alpha=0.2) +
          facet_wrap(reformulate(".",variable[2:length(variable)]))#+
      }
      
    }
  }
  
  
  
  return(p4)
}

plot_npde_scatter_dt = function(tab,variable="OFT",type="TTE",outcome="npde",adj=T,bin=F,method="equal"){
  if(type!="TTE"){
    
    # tab=npde_L
    tab$old_var=tab[[variable[1]]]
    if(bin){
      
      bounds = auto_bin(tab[[variable[1]]],plot.opt=list(bin.method=method,
                                                         bin.numer=NULL,
                                                         bin.breaks=NULL,
                                                         bin.extreme=NULL,
                                                         xlog=NULL),verbose=FALSE)$xbound 
      tab[[variable[1]]] = sapply(tab$old_var,function(x) bounds[findClosest(bounds,x)])
    }
    
    
    quant_npde = tab[,list(q5=quantile(get(outcome),0.025),
                           q50=quantile(get(outcome),0.5),
                           q95=quantile(get(outcome),0.975)),by=eval(variable)]
    # quant_npde = melt(quant_npde,id.vars=variable)
    # temp_th = gg_qq(tab[[outcome]])
    # th = temp_th$tab[temp_th$ord,]
    # names(th)[1]=outcome
    # th_fin = merge(tab,th,by=outcome)
    # 
    # quant_th_med = th_fin[,list(q5=quantile(z,0.05),q50=quantile(z,0.5),q95=quantile(z,0.95)),by=variable]
    # quant_th_low = melt(th_fin[,list(q5=quantile(lower,0.05),q50=quantile(lower,0.5),q95=quantile(lower,0.95)),by=variable],id.vars = variable,variable.name = "cat")
    # quant_th_up = melt(th_fin[,list(q5=quantile(upper,0.05),q50=quantile(upper,0.5),q95=quantile(upper,0.95)),by=variable],id.vars = variable,variable.name = "cat")
    # # quant_th_med$type="med"
    # # quant_th_low$type="low"
    # names(quant_th_low)[length(variable)+2]="low"
    # names(quant_th_up)[length(variable)+2]="up"
    # quant_th_up$type="up"
    
    
    
    
    
    # ribbon = merge(quant_th_low,quant_th_up,by=c(variable,"cat"))
    
    if(length(variable)==1){
      huron = tab[,list(N=length(unique(id))),by=c(variable)]
      
      huron[,q5.5:=compute.bands(N,quant=c(0.025,0.5,0.975))[[1]][,1],by=c(variable)]
      huron[,q5.95:=compute.bands(N,quant=c(0.025,0.5,0.975))[[2]][,1],by=c(variable)]
      
      huron[,q50.5:=compute.bands(N,quant=c(0.025,0.5,0.975))[[1]][,2],by=c(variable)]
      huron[,q50.95:=compute.bands(N,quant=c(0.025,0.5,0.975))[[2]][,2],by=c(variable)]
      
      huron[,q95.5:=compute.bands(N,quant=c(0.025,0.5,0.975))[[1]][,3],by=c(variable)]
      huron[,q95.95:=compute.bands(N,quant=c(0.025,0.5,0.975))[[2]][,3],by=c(variable)]
      # p4 = ggplot() + geom_point(data=th_fin,aes(x=get(variable[1]),y=get(outcome))) +theme_bw() + scale_x_continuous(name=variable[1]) +
      #   geom_line(data=melt(quant_npde,id.vars = variable,variable.name = "cat"),
      #             aes(x=get(variable[1]),y=value,group=cat,color=cat)) +
      #   geom_ribbon(data=ribbon,aes(x=get(variable[1]),ymin=low,ymax=up,group=cat,fill=cat),alpha=0.4) + 
      #   scale_fill_manual(values = c("blue","red","blue"))+
      #   scale_color_manual(values = c("blue","red","blue"))
      # 
      p4 = ggplot() +theme_bw() + scale_x_continuous(name=variable[1]) +
        # geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)),se = F,col=2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q5.5,ymax=q5.95),fill="blue",alpha=0.2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q50.5,ymax=q50.95),fill="red",alpha=0.2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q95.5,ymax=q95.95),fill="blue",alpha=0.2)+
        geom_line(data=melt(quant_npde,id.vars = variable,variable.name = "cat"),
                  aes(x=get(variable[1]),y=value,group=cat,lty=cat))+
        # geom_point(data=tab,aes(x=old_var,y=get(outcome)))+
        # geom_ribbon(data=th_fin,aes(x=get(variable[1]),ymin=lower-z,ymax=upper-z),alpha=0.4);p4 #+ 
        # scale_fill_manual(values = c("blue","red","blue"))+
        scale_linetype_manual(values = c(2,1,2),name="Quantiles");p4
    }else{
      huron = tab[,list(N=length(unique(id))),by=c(variable)]
      
      huron[,q5.5:=compute.bands(N)[[1]][,1],by=c(variable)]
      huron[,q5.95:=compute.bands(N)[[2]][,1],by=c(variable)]
      
      huron[,q50.5:=compute.bands(N)[[1]][,2],by=c(variable)]
      huron[,q50.95:=compute.bands(N)[[2]][,2],by=c(variable)]
      
      huron[,q95.5:=compute.bands(N)[[1]][,3],by=c(variable)]
      huron[,q95.95:=compute.bands(N)[[2]][,3],by=c(variable)]
      
      p4 = ggplot() + theme_bw() + scale_x_continuous(name=variable[1]) +
        geom_line(data=melt(quant_npde,id.vars = variable,variable.name = "cat"),
                  aes(x=get(variable[1]),y=value,group=cat,lty=cat)) +
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q5.5,ymax=q5.95),fill="blue",alpha=0.2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q50.5,ymax=q50.95),fill="red",alpha=0.2)+
        geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q95.5,ymax=q95.95),fill="blue",alpha=0.2)+
        facet_wrap(reformulate(".",variable[2:length(variable)]),ncol=6)+
        # geom_point(data=tab,aes(x=old_var,y=get(outcome)))+
        scale_linetype_manual(name="Quantiles",values=c(2,1,2))
      # geom_line(data=data.frame(melt(data.table(quant_th_med),id.vars = "time")),
      # aes(x=time,y=value,group=variable,color=variable),lty=2) +
      # geom_ribbon(data=ribbon,aes(x=get(variable[1]),ymin=low,ymax=up,group=cat,fill=cat),alpha=0.4) + 
      # scale_fill_manual(values = c("blue","red","blue"))+
      # scale_color_manual(values = c("blue","red","blue"))
    }
  }else{
    
    
    
    
    # quant_th_med = th_fin[,list(q5=quantile(z,0.05),q50=quantile(z,0.5),q95=quantile(z,0.95)),by=variable]
    # quant_th_low = melt(th_fin[,list(q5=quantile(lower,0.05),q50=quantile(lower,0.5),q95=quantile(lower,0.95)),by=variable],id.vars = variable,variable.name = "cat")
    # quant_th_up = melt(th_fin[,list(q5=quantile(upper,0.05),q50=quantile(upper,0.5),q95=quantile(upper,0.95)),by=variable],id.vars = variable,variable.name = "cat")
    # quant_th_med$type="med"
    # quant_th_low$type="low"
    # names(quant_th_low)[length(variable)+2]="low"
    # names(quant_th_up)[length(variable)+2]="up"
    # quant_th_up$type="up"
    # ribbon = merge(quant_th_low,quant_th_up,by=c(variable,"cat"))
    
    
    if(length(variable)==1){
      dist=ifelse(outcome=="npde","norm","unif")
      temp_th = gg_qq(tab[[outcome]],conf=0.9,distribution = dist)
      th = temp_th$tab[temp_th$ord,]
      names(th)[1]=outcome
      th_fin = merge(tab,th,by=outcome)
      if(adj){
        p4 = ggplot(data=th_fin,aes(x=get(variable[1]))) + geom_point(aes(y=get(outcome)-z,col=status)) +theme_bw(base_size = 18) + scale_x_continuous(name=variable[1]) +
          # geom_line(data=) +
          # geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)-z),se = F,col=2)+
          geom_line(aes(y=z-z),lty=2) +
          geom_ribbon(aes(ymin=lower-z,ymax=upper-z,fill="blue"),alpha=0.4);p4
      }else{
        p4 = ggplot(data=th_fin,aes(x=get(variable[1]))) + geom_point(aes(y=get(outcome),col=status)) +theme_bw(base_size = 18) + scale_x_continuous(name=variable[1]) +
          # geom_line(data=) +
          # geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)-z),se = F,col=2)+
          geom_line(aes(y=z),lty=2) +
          geom_ribbon(aes(ymin=lower,ymax=upper,fill="blue"),alpha=0.4);p4
      }
      
    }else{
      # temp_th = gg_qq(tab[[outcome]])
      # th = temp_th$tab[temp_th$ord,]
      # names(th)[1]=outcome
      # th_fin = merge(tab,th,by=outcome)
      # tab$obs
      tab[,c("ord.x","z","upper","lower"):=gg_qq(npde,conf=0.9)$tab,by=eval(variable[2:length(variable)])]
      th_fin=tab
      # temp_th = tab
      # gg_qq(tab[[outcome]])
      # th = temp_th$tab[temp_th$ord,]
      # names(th)[1]=outcome
      # th_fin = merge(tab,th,by=outcome)
      if(adj){
        p4 = ggplot(data=th_fin) + geom_point(aes(x=get(variable[1]),y=get(outcome)-z,col=status)) +theme_bw() + scale_x_continuous(name=variable[1]) +
          # geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)-z),se = F,col=2)+
          geom_line(aes(x=get(variable[1]),y=z-z)) +
          geom_ribbon(aes(x=get(variable[1]),ymin=lower-z,ymax=upper-z,fill="blue"),alpha=0.2) +
          facet_wrap(reformulate(response=".",termlabels=variable[2:length(variable)]),ncol=5)#+
      }else{
        p4 = ggplot(data=th_fin) + geom_point(aes(x=get(variable[1]),y=get(outcome),col=status)) +theme_bw() + scale_x_continuous(name=variable[1]) +
          # geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)),se = F,col=2)+
          geom_line(aes(x=get(variable[1]),y=z)) +
          geom_ribbon(aes(x=get(variable[1]),ymin=lower,ymax=upper,fill="blue"),alpha=0.2) +
          facet_wrap(reformulate(".",variable[2:length(variable)]),ncol=6)#+
      }
      
    }
  }
  
  
  
  return(p4)
}



compute.bands<-function(nsamp,nseuil=200,quant=c(0.025,0.5,0.975),distrib="norm", alpha=0.95) {
  # Compute a prediction interval around selected quantiles of the normal or uniform distribution, for different sizes of samples by randomly sampling from N(0,1) or U(0,1)
  ### nsamp: a vector giving the sizes of the samples
  ### size: alpha (defaults to a 95% PI)
  ### quantile: quant (defaults to 5th, 50th (median) and 95th percentiles)
  ### distribution: normal (default) or uniform
  # When the number of samples isamp is larger than 200, the PI is computed for n=200 and the size of the PI is then adjusted through sqrt(200/isamp)
  
  # Returns
  ### binf: lower bounds (as many columns as elements in quant) for the PI with level alpha
  ### bmed: median
  ### bsup: upper bounds
  #  msim<-10000
  msim<-1000 # number of replications used to compute the prediction interval
  idx1<-which(nsamp>=nseuil)
  quant.pi<-c((1-alpha)/2,0.5,1-(1-alpha)/2) 
  if(length(idx1)>0) {
    xsamp<-matrix(switch(distrib,norm=rnorm(msim*nseuil),unif=runif(msim*nseuil)), ncol=msim)
    mat<-apply(xsamp,2,quantile,quant)
    xseuil<-apply(mat,1,quantile,quant.pi)
    demi<-(xseuil[3,]-xseuil[1,])/2
  }
  binf<-bsup<-bmed<-matrix(nrow=length(nsamp),ncol=length(quant), dimnames=list(nsamp,quant))
  for(isamp in unique(nsamp)) {
    if(isamp<nseuil) {
      xsamp<-matrix(switch(distrib,norm=rnorm(msim*isamp),unif=runif(msim*isamp)), ncol=msim)
      mat<-apply(xsamp,2,quantile,quant)
      xtab<-apply(mat,1,quantile,quant.pi)
    } else {
      xtab<-matrix(nrow=3,ncol=length(quant))
      xtab[2,]<-switch(distrib,norm=qnorm(quant),unif=quant)
      xtab[1,]<-xtab[2,]-demi*sqrt(nseuil/isamp)
      xtab[3,]<-xtab[2,]+demi*sqrt(nseuil/isamp)
    }
    for(i in which(nsamp==isamp)) {
      binf[i,]<-xtab[1,]
      bmed[i,]<-xtab[2,]
      bsup[i,]<-xtab[3,]
    }
  }
  return(list(binf=binf,bsup=bsup,bmed=bmed))
}


reject_stat = function(npde,parametric=T,which="npde"){
  object=npde
  object<-object[!is.na(object)]
  sev<-var(object)*sqrt(2/(length(object)-1))
  semp<-sd(object)
  n1<-length(object)
  sem<-semp/sqrt(length(object))
  res<-list(mean=mean(object),se.mean=sem,var=var(object),se.var=sev)#, kurtosis=kurtosis(object),skewness=skewness(object))
  object<-object[!is.na(object)]
  myres<-rep(0,4)
  if(parametric) {
    if(which=="pd") y<-t.test(object,mu=0.5) else y<-t.test(object)
  } else {
    if(which=="pd") y<-wilcox.test(object,mu=0.5) else y<-wilcox.test(object)
  }
  myres[1]<-y$p.val
  # ECO TODO: ici utiliser le test de Magalie
  if(which=="pd") y<-ks.test(object,"punif",min=min(object,na.rm=TRUE), max=max(object,na.rm=TRUE)) else y<-shapiro.test(object)
  myres[3]<-y$p.val
  
  # test de variance pour 1 échantillon
  # chi=s2*(n-1)/sigma0 et test de H0={s=sigma0} vs chi2 ? n-1 df
  #    if(parametric) {
  chi<-(semp**2)*(n1-1)
  if(which=="pd") chi<-chi*12 # X~U(0,1) => var(X)=1/12
  y<-2*min(pchisq(chi,n1-1),1-pchisq(chi,n1-1))
  myres[2]<-y
  #    } else {
  # ECO TODO: non-parametric equivalent of variance test for one-sample ?
  #    }
  xcal<-3*min(myres[1:3])
  myres[4]<-min(1,xcal)
  if(parametric) 
    names(myres)<-c("  t-test                    ","  Fisher variance test      ","  SW test of normality      ", "Global adjusted p-value     ") else 
      names(myres)<-c("  Wilcoxon signed rank test ","  Fisher variance test      ", "  SW test of normality      ","Global adjusted p-value     ")
  if(which=="pd") names(myres)[3]<-"KS test of uniformity       "
  res$p.value<-myres
  res$nobs<-n1
  
  return(res)
  
}


gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  # print(str(x))
  if(distribution=="unif"){
    x_old=x
    q.function <- eval(parse(text = paste0("q", distribution)))
    d.function <- eval(parse(text = paste0("d", distribution)))
    x <- na.omit(x)
    ord <- order(x)
    n <- length(x)
    P <- ppoints(length(x))
    df <- data.frame(ord.x = x[ord], z = q.function(P))
    
    zz <- qnorm(1 - (1 - conf)/2)
    lower <- sapply(P,function(xx){
      binom.test(round(xx*n),n,xx,conf.level = conf)$conf[1]
    })
    upper <- sapply(P,function(xx){
      binom.test(round(xx*n),n,xx,conf.level = conf)$conf[2]
    })
    #fit.value <- coef[1] + coef[2] * df$z
    fit.value <- 0 + 1 * df$z
    df$upper <- upper
    df$lower <- lower
    
    
    df1=df
    df1[ord,]=df
    if(!is.null(labels)){ 
      df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
    }
    
    p <- ggplot(df, aes(x=z, y=ord.x)) +
      theme_bw()+
      ylab("Sample Quantiles (npde)") + xlab("Theoretical Quantiles") + 
      geom_point() + geom_line() +
      #geom_abline(intercept = coef[1], slope = coef[2]) +
      geom_abline(intercept = 0, slope = 1,col="blue") +
      geom_ribbon(aes(ymin = lower, ymax = upper,fill="black"), alpha=0.4) + 
      scale_fill_manual(values=c("blue"))+theme(legend.position="none")+
      scale_y_continuous(breaks=-3:3)
  }else{
    x_old=x
    q.function <- eval(parse(text = paste0("q", distribution)))
    d.function <- eval(parse(text = paste0("d", distribution)))
    x <- na.omit(x)
    ord <- order(x)
    n <- length(x)
    P <- ppoints(length(x))
    df <- data.frame(ord.x = x[ord], z = q.function(P,...))
    
    if(is.null(line.estimate)){
      Q.x <- quantile(df$ord.x, c(0.25, 0.75))
      Q.z <- q.function(c(0.25, 0.75), ...)
      b <- diff(Q.x)/diff(Q.z)
      coef <- c(Q.x[1] - b * Q.z[1], b)
    } else {
      coef <- coef(line.estimate(ord.x ~ z))
    }
    
    zz <- qnorm(1 - (1 - conf)/2)
    SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
    #fit.value <- coef[1] + coef[2] * df$z
    fit.value <- 0 + 1 * df$z
    df$upper <- fit.value + zz * SE
    df$lower <- fit.value - zz * SE
    
    
    df1=df
    df1[ord,]=df
    if(!is.null(labels)){ 
      df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
    }
    
    p <- ggplot(df, aes(x=z, y=ord.x)) +
      theme_bw()+
      ylab("Sample Quantiles (npde)") + xlab("Theoretical Quantiles") + 
      geom_point() + geom_line() +
      #geom_abline(intercept = coef[1], slope = coef[2]) +
      geom_abline(intercept = 0, slope = 1,col="blue") +
      geom_ribbon(aes(ymin = lower, ymax = upper,fill="black"), alpha=0.4) + 
      scale_fill_manual(values=c("blue"))+theme(legend.position="none")+
      scale_y_continuous(breaks=-3:3)
  }
  
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  return(list(tab=df1,plot=p,ord=ord))
}



gg_qq2 <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                   labels = names(x),simulate=F){
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P,...))
  
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }
  
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  #fit.value <- coef[1] + coef[2] * df$z
  fit.value <- 0 + 1 * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  
  if(!is.null(labels)){ 
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }
  
  if(simulate){
    library(plyr)
    sim = data.frame(rep=rep(1:length(x),each=1000),x=rnorm(length(x)*1000))
    sim_df = ddply(sim,"rep",summarise,x=x[order(x)],P=ppoints(x))
    sim_df$z = q.function(sim_df$P)
    
    # p <- ggplot(sim_df[sim_df$rep==1,], aes(x=z, y=x)) +
    #   theme_bw()+
    #   ylab("Sample Quantiles (npde)") + xlab("Theoretical Quantiles") + 
    #   geom_point() + geom_line() +
    #   #geom_abline(intercept = coef[1], slope = coef[2]) +
    #   geom_abline(intercept = 0, slope = 1) +
    #   geom_ribbon(aes(ymin = q5, ymax = q95), alpha=0.2) + 
    #   scale_y_continuous(breaks=-3:3)
    
    
    p <- ggplot(sim_df, aes(x=z, y=x)) +
      theme_bw()+
      ylab("Sample Quantiles (npde)") + xlab("Theoretical Quantiles") + 
      geom_point() + geom_line() +
      #geom_abline(intercept = coef[1], slope = coef[2]) +
      geom_abline(intercept = 0, slope = 1) +
      geom_ribbon(aes(ymin = q5, ymax = q95), alpha=0.2) + 
      scale_y_continuous(breaks=-3:3)
    
    
    
    sim_df[sim_df$z< -2.8 & sim_df$z> -3.2,]
    res = ddply(sim_df,"P",summarise,q5 = quantile(z,0.05),q50 = quantile(z,0.5),q95 = quantile(z,0.95))
    sim_df$index = rep(1:length(x),1000)
    
    
    res$z = df$z[res$index]
    res2 = merge(res,df,by="z")
    p <- ggplot(res2, aes(x=z, y=ord.x)) +
      theme_bw()+
      ylab("Sample Quantiles (npde)") + xlab("Theoretical Quantiles") + 
      geom_point() + geom_line() +
      #geom_abline(intercept = coef[1], slope = coef[2]) +
      geom_abline(intercept = 0, slope = 1) +
      geom_ribbon(aes(ymin = q5, ymax = q95), alpha=0.2) + 
      scale_y_continuous(breaks=-3:3)
    
  }else{
    p <- ggplot(df, aes(x=z, y=ord.x)) +
      theme_bw()+
      ylab("Sample Quantiles (npde)") + xlab("Theoretical Quantiles") + 
      geom_point() + geom_line() +
      #geom_abline(intercept = coef[1], slope = coef[2]) +
      geom_abline(intercept = 0, slope = 1) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) + 
      scale_y_continuous(breaks=-3:3)
  }
  
  
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  return(list(tab=df,plot=p,ord=ord))
}

gg_hist <- function(x,p0=ggplot()){
  x=data.table(x)
  nrep=dim(x)[1]
  temp = hist(x$z,breaks = seq(-4,4,0.5),plot = F)
  temp_obs = hist(x$npde,breaks = seq(-4,4,0.5),plot = F)
  d0 = data.table(breaks= temp$breaks[1:(length(temp$breaks)-1)],z=temp$density,npde=temp_obs$density,mids=temp$mids)
  d0$u = qbinom(0.975,nrep,dnorm(d0$mids))/nrep
  d0$l = qbinom(0.025,nrep,dnorm(d0$mids))/nrep
  
  temp2=data.table(x=seq(-4,3.5,0.5),xend=seq(-4,3.5,0.5),y=0,yend=melt(d0,id.vars = c("mids","breaks"))[,list(max=max(value)),by="mids"]$max,
                   yend2=d0[,list(max=max(npde)),by="mids"]$max)
  p=p0 + 
    geom_step(data=d0,aes(x=breaks,y=z),col=2,lty=2)+
    geom_ribbon(data=d0,aes(x=breaks,ymin=l,ymax=u),stat="stepribbon",col="blue",fill="blue",alpha=.2,lty=2)+
    geom_ribbon(data=d0,aes(x=breaks,ymin=0,ymax=l),stat="stepribbon",col="blue",fill="white",alpha=1,lty=2)+
    geom_step(data=d0,aes(x=breaks,y=npde),col="blue",lwd=1)+
    # geom_segment(data=data.table(x=seq(-4,4,0.5),xend=seq(-4,4,0.5),y=0,yend=melt(d0,id.vars = "breaks")[,list(max=max(value)),by="breaks"]$max))+
    geom_segment(data=temp2,
                 aes(x=x,xend=xend,y=y,yend=yend),col="blue",lty=2)+
    geom_segment(data=temp2,
                 aes(x=x,xend=xend,y=y,yend=yend2),lwd=1,col="blue") + 
    geom_hline(yintercept = 0,col="blue",lwd=1)+
    xlab("Sample quantile (npde)") + ylab("density")
  return(p)
}

prediction_interval = function(dd1,time_interval=seq(0,735,5),PI=0.95){
  #dd1=km_list[[1]]
  # time_interval = seq(0,735,5)
  
  borne_inf = (1-PI)/2
  borne_sup = 1-borne_inf
  
  K = length(unique(dd1$REP))
  n_time = length(time_interval)         
  
  indice_row=1
  
  dd1$time2 = ceiling(dd1$time/max(time_interval)*(n_time-1))/(n_time-1)  * max(time_interval)

  # dd3 = dd1[,c(4,2,3)]
  dd3 = dd1[,c(4,3,1),with=F]
  names(dd3) = c("TIME","SURV","REP")
  dd4 = dd3[,list(surv.5 = quantile(SURV,borne_inf),
                  surv.50 = quantile(SURV,0.5),
                  surv.95 = quantile(SURV,borne_sup)),keyby="TIME"]
  # dd4 = ddply(dd3,c("TIME"),summarize,surv.5 = quantile(SURV,0.025),surv.50 = quantile(SURV,0.5),surv.95 = quantile(SURV,0.975)  )
  
  return(dd4)  
  
}

KM_obs = function(data,time_interval=c(0,2,4,6,8,10,14,20,26,34,43,52)/52,p=ggplot(),verbose=F){
  # dd1=km
  a = list()
  km_list = list()
  
  id.name <- c("ID","id")
  rep.name <- c("rep","REP")
  # oft.name <- c("OFT","oft","TTE")
  oft.name <- c("TFT","tft","TTE")
  #print("kaplan meir computation")
  for(j in 1:length(data)){
    #print(j)
    sim_j = data.table(data[[j]])
    
    names(sim_j)[names(sim_j) %in% id.name] <- c("ID")
    names(sim_j)[names(sim_j) %in% rep.name] <- c("REP")
    names(sim_j)[names(sim_j) %in% oft.name] <- c("TTE")
    
    #km_list[[j]] = data.frame(NULL)
    nrep = length(unique(sim_j$REP))
    km_list[[j]] = matrix(-1,ncol = 3,nrow=1000*nrep )
    compt=1
    # data_sim[[j]] = data.table(data_sim[[j]])
    setkey(sim_j,"REP")
    
    a = Sys.time()
    for(i in 1:nrep){
      if(i%%100==0&verbose) print(i)
      dd_temp = sim_j#[REP==i]
      dd_temp$status=1
      km_temp = (summary(survfit(Surv(dd_temp$TTE,dd_temp$status)~1)))
      # km_temp = km_temp[,]
      #km_list[[j]] = rbind(km_list[[j]],data.frame(time=km_temp$time,surv=km_temp$surv,REP=i ))
      km_list[[j]][compt:(compt+length(km_temp$time[!is.nan(km_temp$std.err)])-1),] = as.matrix(data.frame(time=km_temp$time[!is.nan(km_temp$std.err)],surv=km_temp$surv[!is.nan(km_temp$std.err)],REP=i ))
      compt = compt + length(km_temp$time[!is.nan(km_temp$std.err)])
    }
    print(Sys.time()-a)
    km_list[[j]] = km_list[[j]][apply(km_list[[j]],1,sum)!=-3,]
    km_list[[j]] = as.data.frame(km_list[[j]])
    names(km_list[[j]]) = c("time","surv","REP")
    
  }
  
  
  
  
  #print("vpc computation")
  a=NULL
  for(i in 1:length(km_list)){
    #a[[i]] = prediction_interval(km_list[[i]])
    tmp=km_list[[i]]
    tmp$REP=NULL
    a = rbind(a ,rbind(cbind(time=0,surv=1,mod=i),cbind(tmp,mod=i)))
    
  }
  a$mod = paste(a$mod)
  names(a) = c("TIME","med","mod")
  
  
  # p1 = p + xlab("Time (day)") + ylab("Survival probability S(t)") + ylim(c(0,1)) + scale_x_continuous(breaks=c(seq(0,735,200),735)) + 
  #   theme_bw()+ 
  #   theme(axis.title.x = element_text(size=22)) + 
  #   theme(axis.title.y = element_text(size=22)) + 
  #   theme(axis.text.x = element_text(size=18)) + 
  #   theme(axis.text.y = element_text(size=18))
  # 
  # Label appearance
  
  p2 = p + geom_step(data=a,aes(x=TIME,y=med),lty=2,lwd=1) +
    scale_x_continuous(name="Time")+
    scale_y_continuous(name="1 - Prob(dropout)")
  theme(legend.position="none");p2
  
  
  return(list(p2,a)) 
}



VPC3 = function(data_sim,time_interval=c(0,2,4,6,8,10,14,20,26,34,43,52)/52,p=ggplot(),verbose=F){
  # dd1=km
  a = list()
  km_list = list()
  
  id.name <- c("ID","id")
  rep.name <- c("rep","REP")
  tft.name <- c("TFT","tft","TTE")
  #print("kaplan meir computation")
  
  km = function(TTE,status){
    res = (summary(survfit(Surv(TTE,status)~1)))
    time = c(0,res$time)
    surv= c(1,res$surv)
    list(time,surv)
  }
  
  for(j in 1:length(data_sim)){
    #print(j)
    
    
    
    
    sim_j = data.table(data_sim[[j]])
    
    names(sim_j)[names(sim_j) %in% id.name] <- c("ID")
    names(sim_j)[names(sim_j) %in% rep.name] <- c("REP")
    names(sim_j)[names(sim_j) %in% tft.name] <- c("TTE")
    
    
    
    
    
    
    setkey(sim_j,"REP")
    
    
    # sim_j[,c("time","surv"):=km(TTE,status),by="REP"]
    km_list[[j]] = sim_j[,{res=km(TTE,status);list(time=res[[1]],surv=res[[2]])},by="REP"]
    
    # km_list[[j]] = as.data.frame(km_list[[j]])
    # names(km_list[[j]]) = c("time","surv","REP")
    
  }
  
  
  
  
  #print("vpc computation")
  a=NULL
  for(i in 1:length(km_list)){
    #a[[i]] = prediction_interval(km_list[[i]])
    tmp=prediction_interval(km_list[[i]],time_interval = time_interval)
    tmp$mod=i
    a = rbind(a ,tmp)
    # a = rbind(a ,rbind(cbind(TIME=0,surv.5=1,surv.50=1,surv.95=1,mod=i),cbind(tmp,mod=i)))
    
  }
  a$mod = paste(a$mod)
  names(a) = c("TIME","lower","med","upper","mod")
  
  
  # p1 = p + xlab("Time (day)") + ylab("Survival probability S(t)") + ylim(c(0,1)) + scale_x_continuous(breaks=c(seq(0,735,200),735)) + 
  #   theme_bw()+ 
  #   theme(axis.title.x = element_text(size=22)) + 
  #   theme(axis.title.y = element_text(size=22)) + 
  #   theme(axis.text.x = element_text(size=18)) + 
  #   theme(axis.text.y = element_text(size=18))
  # 
  # Label appearance
  
  p2 = p + geom_step(data=a,aes(x=TIME,y=1-med,colour=mod),lty=2,lwd=1) +
    geom_ribbon(data=a,aes(x=TIME,ymin=1-lower,ymax=1-upper,fill=mod,alpha="test"),
                inherit.aes=FALSE,stat="stepribbon")+
    # facet_wrap(~mod)+
    coord_cartesian(xlim=c(0,735));p2#p2+scale_alpha_manual(values=c(0.3,0.3,0.3,0.3))
  
  
  return(list(p2,a)) 
}



VPC_PSA_v3 = function(obs=NULL,sim,p=ggplot(),breaks=c(1,10,100,1000)){
  library(data.table)
  a=NULL
  y.name <- c("y","Y","dv","DV")
  x.name <- c("time","time2","TIME","Time")
  rep.name <- c("rep","REP","isim","ISIM")
  
  #a[[i]] = prediction_interval(km_list[[i]])
  names(sim)[names(sim) %in% y.name] <- c("Y")
  names(sim)[names(sim) %in% x.name] <- c("time")
  names(sim)[names(sim) %in% rep.name] <- c("rep")
  #names(data_list)[names(data_list) %in% x.name] <- c("time2")
  # sim$time2 = sapply(sim$time,function(x){
  #   return(time_interval[min(abs(time_interval-x)) == abs(time_interval-x)])
  # })
  
  sim=data.table(sim)
  tmp1 = sim[,list(lower = quantile(Y,0.05,na.rm=T),
                   med =quantile(Y,0.5,na.rm=T),
                   upper=quantile(Y,0.95,na.rm=T)),by=c("time","rep")]
  
  tmp = tmp1[,list(llow = quantile(lower,0.05,na.rm=T),
                   lmed =quantile(lower,0.5,na.rm=T),
                   lup=quantile(lower,0.95,na.rm=T),
                   mlow = quantile(med,0.05,na.rm=T),
                   mmed =quantile(med,0.5,na.rm=T),
                   mup=quantile(med,0.95,na.rm=T),
                   ulow = quantile(upper,0.05,na.rm=T),
                   umed =quantile(upper,0.5,na.rm=T),
                   uup=quantile(upper,0.95,na.rm=T)),by="time"]
  
  
  tmp_line = tmp[,list(time,lmed,mmed,umed)]
  tmp_line=melt(tmp_line,id.vars = "time")
  names(tmp_line)[2] = "type"
  tmp_line=data.frame(tmp_line)
  
  tmp_ribbon = tmp[,list(time,llow,lup,mlow,mup,ulow,uup)]
  tmp_ribbon = data.frame(tmp_ribbon)
  names(tmp_ribbon) =c("time","low","up","low","up","low","up")
  
  
  tmp_ribbon=rbind(tmp_ribbon[,1:3],tmp_ribbon[,c(1,4,5)],tmp_ribbon[,c(1,6,7)])
  tmp_ribbon$type = rep(c("lmed","mmed","umed"),each=length(unique(tmp$time)))
  
  
  p1 = p + xlab("Time (day)") + ylab("PSA") +  scale_x_continuous(breaks=c(seq(0,735,200),735)) + 
    theme_bw()+ 
    theme(axis.title.x = element_text(size=22)) + 
    theme(axis.title.y = element_text(size=22)) + 
    theme(axis.text.x = element_text(size=18)) + 
    theme(axis.text.y = element_text(size=18)) 
  
  # Label appearance
  
  
  p2 = p1 + geom_line(data=tmp_line,aes(x=time,y=value,group=type,col=type),lty=2,lwd=1) +
    geom_ribbon(data=tmp_ribbon,aes(x=time,ymin=low,ymax=up,fill=type,group=type), alpha=0.3,inherit.aes=FALSE) +
    # geom_point(data=tmp_ribbon,aes(x=time,y=exp(low-1),group=type),inherit.aes = F) +
    # geom_point(data=tmp_ribbon,aes(x=time,y=exp(up-1),group=type),inherit.aes = F) +
    theme(legend.position="none") + 
    scale_fill_manual(values=c("blue","red","blue")) +
    scale_color_manual(values = c("blue","red","blue")) #+ 
    # scale_y_log10(breaks=breaks)
  #print(p2)
  
  if(!is.null(obs)){
    b = data.table(obs)
    b = b[,list(med=quantile(y,0.5),
                q5=quantile(y,0.05),
                q95=quantile(y,0.95)),by="time"]
    b = melt(b,id.vars = "time")
    p2 = p2 + geom_line(data=b,aes(x=time,y=value,group=variable)) +
      geom_point(data=b,aes(x=time,y=value,group=variable))
    
  }
  
  return(p2) 
  
}

VPC_PSA_v4 = function(obs=NULL,sim,p=ggplot(),breaks=c(1,10,100,1000),lty=2){
  library(data.table)
  a=NULL
  y.name <- c("y","Y","dv","DV")
  x.name <- c("time","time2","TIME","Time")
  rep.name <- c("rep","REP","isim","ISIM")
  
  #a[[i]] = prediction_interval(km_list[[i]])
  names(sim)[names(sim) %in% y.name] <- c("Y")
  names(sim)[names(sim) %in% x.name] <- c("time")
  names(sim)[names(sim) %in% rep.name] <- c("rep")
  #names(data_list)[names(data_list) %in% x.name] <- c("time2")
  # sim$time2 = sapply(sim$time,function(x){
  #   return(time_interval[min(abs(time_interval-x)) == abs(time_interval-x)])
  # })
  
  sim=data.table(sim)
  tmp1 = sim[,list(lower = quantile(Y,0.05,na.rm=T),
                   med =quantile(Y,0.5,na.rm=T),
                   upper=quantile(Y,0.95,na.rm=T)),by=c("time","rep")]
  
  tmp = tmp1[,list(llow = quantile(lower,0.05,na.rm=T),
                   lmed =quantile(lower,0.5,na.rm=T),
                   lup=quantile(lower,0.95,na.rm=T),
                   mlow = quantile(med,0.05,na.rm=T),
                   mmed =quantile(med,0.5,na.rm=T),
                   mup=quantile(med,0.95,na.rm=T),
                   ulow = quantile(upper,0.05,na.rm=T),
                   umed =quantile(upper,0.5,na.rm=T),
                   uup=quantile(upper,0.95,na.rm=T)),by="time"]
  
  
  # tmp_line = tmp[,list(time,lmed,mmed,umed)]
  # tmp_line=melt(tmp_line,id.vars = "time")
  # names(tmp_line)[2] = "type"
  # tmp_line=data.frame(tmp_line)
  # 
  # tmp_ribbon = tmp[,list(time,llow,lup,mlow,mup,ulow,uup)]
  # tmp_ribbon = data.frame(tmp_ribbon)
  # names(tmp_ribbon) =c("time","low","up","low","up","low","up")
  # 
  # 
  # tmp_ribbon=rbind(tmp_ribbon[,1:3],tmp_ribbon[,c(1,4,5)],tmp_ribbon[,c(1,6,7)])
  # tmp_ribbon$type = rep(c("lmed","mmed","umed"),each=length(unique(tmp$time)))
  # 
  
  p1 = p + xlab("Time (day)") + ylab("PSA") +  scale_x_continuous(breaks=c(seq(0,735,200),735)) + 
    theme_bw()+ 
    theme(axis.title.x = element_text(size=22)) + 
    theme(axis.title.y = element_text(size=22)) + 
    theme(axis.text.x = element_text(size=18)) + 
    theme(axis.text.y = element_text(size=18)) 
  
  # Label appearance
  
  
  p2 = p1 + 
    geom_line(data=tmp,aes(x=time,y=mmed),col=lty,lwd=1) +
    geom_ribbon(data=tmp,aes(x=time,ymin=lmed,ymax=umed),fill=lty,alpha=0.15) +
    # geom_ribbon(data=tmp_ribbon,aes(x=time,ymin=low,ymax=up,fill=type,group=type), alpha=0.3,inherit.aes=FALSE) +
    # geom_point(data=tmp_ribbon,aes(x=time,y=exp(low-1),group=type),inherit.aes = F) +
    # geom_point(data=tmp_ribbon,aes(x=time,y=exp(up-1),group=type),inherit.aes = F) +
    theme(legend.position="none") + 
    scale_fill_manual(values=c("blue","red","blue")) +
    scale_color_manual(values = c("blue","red","blue")) #+ 
  # scale_y_log10(breaks=breaks)
  #print(p2)
  
  if(!is.null(obs)){
    b = data.table(obs)
    b = b[,list(med=quantile(y,0.5),
                q5=quantile(y,0.05),
                q95=quantile(y,0.95)),by="time"]
    b = melt(b,id.vars = "time")
    p2 = p2 + geom_line(data=b,aes(x=time,y=value,group=variable)) +
      geom_point(data=b,aes(x=time,y=value,group=variable))
    
  }
  
  return(p2) 
  
}



VPC_L = function(obs=NULL,sim,p=ggplot(),breaks=c(1,10,100,1000)){
  library(data.table)
  a=NULL
  y.name <- c("y","Y","dv","DV")
  x.name <- c("time","time2","TIME","Time")
  rep.name <- c("rep","REP","isim","ISIM")
  
  #a[[i]] = prediction_interval(km_list[[i]])
  names(sim)[names(sim) %in% y.name] <- c("Y")
  names(sim)[names(sim) %in% x.name] <- c("time")
  names(sim)[names(sim) %in% rep.name] <- c("rep")
  #names(data_list)[names(data_list) %in% x.name] <- c("time2")
  # sim$time2 = sapply(sim$time,function(x){
  #   return(time_interval[min(abs(time_interval-x)) == abs(time_interval-x)])
  # })
  
  sim=data.table(sim)
  tmp1 = sim[,list(lower = quantile(Y,0.05,na.rm=T),
                   med =quantile(Y,0.5,na.rm=T),
                   upper=quantile(Y,0.95,na.rm=T)),by=c("time","rep")]
  
  tmp = tmp1[,list(llow = quantile(lower,0.05,na.rm=T),
                   lmed =quantile(lower,0.5,na.rm=T),
                   lup=quantile(lower,0.95,na.rm=T),
                   mlow = quantile(med,0.05,na.rm=T),
                   mmed =quantile(med,0.5,na.rm=T),
                   mup=quantile(med,0.95,na.rm=T),
                   ulow = quantile(upper,0.05,na.rm=T),
                   umed =quantile(upper,0.5,na.rm=T),
                   uup=quantile(upper,0.95,na.rm=T)),by="time"]
  
  
  tmp_line = tmp[,list(time,lmed,mmed,umed)]
  tmp_line=melt(tmp_line,id.vars = "time")
  names(tmp_line)[2] = "type"
  tmp_line=data.frame(tmp_line)
  
  tmp_ribbon = tmp[,list(time,llow,lup,mlow,mup,ulow,uup)]
  tmp_ribbon = data.frame(tmp_ribbon)
  names(tmp_ribbon) =c("time","low","up","low","up","low","up")
  
  
  tmp_ribbon=rbind(tmp_ribbon[,1:3],tmp_ribbon[,c(1,4,5)],tmp_ribbon[,c(1,6,7)])
  tmp_ribbon$type = rep(c("lmed","mmed","umed"),each=length(unique(tmp$time)))
  
  
  p1 = p + xlab("Time (day)") + ylab("PSA") +  scale_x_continuous(breaks=c(seq(0,735,200),735)) + 
    theme_bw()+ 
    theme(axis.title.x = element_text(size=22)) + 
    theme(axis.title.y = element_text(size=22)) + 
    theme(axis.text.x = element_text(size=18)) + 
    theme(axis.text.y = element_text(size=18)) 
  
  # Label appearance
  
  
  p2 = p1 + geom_line(data=tmp_line,aes(x=time,y=value,group=type,col=type),lty=2,lwd=1) +
    geom_ribbon(data=tmp_ribbon,aes(x=time,ymin=low,ymax=up,fill=type,group=type), alpha=0.3,inherit.aes=FALSE) +
    # geom_point(data=tmp_ribbon,aes(x=time,y=exp(low-1),group=type),inherit.aes = F) +
    # geom_point(data=tmp_ribbon,aes(x=time,y=exp(up-1),group=type),inherit.aes = F) +
    theme(legend.position="none") + 
    scale_fill_manual(values=c("blue","red","blue")) +
    scale_color_manual(values = c("blue","red","blue")) #+ 
  # scale_y_log10(breaks=breaks)
  #print(p2)
  
  if(!is.null(obs)){
    b = data.table(obs)
    b = b[,list(med=quantile(y,0.5),
                q5=quantile(y,0.05),
                q95=quantile(y,0.95)),by="time"]
    b = melt(b,id.vars = "time")
    p2 = p2 + geom_line(data=b,aes(x=time,y=value,group=variable)) +
      geom_point(data=b,aes(x=time,y=value,group=variable))
    
  }
  
  return(p2) 
  
}




power_graph = function(K,N,name,plage,nk=8,method){
  ###### ntime
  
  res = data.frame()
  for(nom in 1:length(name)){
    print(nom)
    # nom=nom+1
    for(n in N[[nom]]){
      for(k in K[[nom]]){
        for(ntime in nk[[nom]]){
          for(param_obs in plage[[nom]]){
            for(param_sim in plage[[nom]]){
              path_obs = paste0("ntime_",ntime,"/K_",k,"/N_",n,"/",name[nom],"_",param_obs)
              
              
              for(m in method[[nom]]){
                
                # if(param_obs!=param_sim){
                #   if(!file.exists(paste0(path_obs,"/power_",param_obs,"_",param_obs,"_m",m,".rda"))) stop(paste0("File don't exist: ",
                #                                                                                                  paste0(path_obs,"/power_",param_obs,"_",param_obs,"_FALSE_m",m,".rda")))
                #   load(paste0(path_obs,"/power_",param_obs,"_",param_obs,"_m",m,".rda"))
                #   
                #   power = get(paste0("typeIerror_censored_m",m))
                #   alpha=c()
                #   alpha=c(alpha,quantile(sapply(power$TTE$adjusted,function(x) x[[4]]),0.05))
                #   alpha=c(alpha,quantile(power$TTE$ks,0.05))
                #   if(!is.null(power$longitudinal$ks)){
                #     alpha=c(alpha,quantile(sapply(power$longitudinal$adjusted,function(x) x[[4]]),0.05))
                #     alpha=c(alpha,quantile(power$longitudinal$ks,0.05))
                #   }else{
                #     alpha=c(alpha,c(0.05,0.05))
                #   }
                #   lci = binom.test(10,200,0.05)$conf.int[1]
                #   uci = binom.test(10,200,0.05)$conf.int[2]
                #   alpha[alpha>=lci|alpha<=uci]=0.05
                #   # alpha=rep(0.05,4)
                # }else{
                #   alpha=rep(0.05,4)
                # }
                alpha=rep(0.05,4)
                
                
                path_res = path_obs
                load(paste0(path_res,"/power_",param_obs,"_",param_sim,"_m",m,".rda"))
                power = get(paste0("typeIerror_censored_m",m))
                pval = (typeIerror(power,alpha))
                temp=data.frame(K=k,N=n,ntime=ntime,param=name[nom],obs=param_obs,sim=param_sim,method=m,tte_adj=pval$tte_adj,tte_ks=pval$tte_ks,
                                l_adj=pval$l_adj,l_ks=pval$l_ks,combi_adj=pval$combi_adj,combi_ks=pval$combi_ks)
                res = rbind(res,temp)
              }
              
            }
          }
        }
      }
    }
  }
  
  res = data.table(res)
  
  # res$method=factor2(res$method,c("full","same_vpc")[method[[1]]])
  
  res=melt(res,id.vars = c("K","N","ntime","param","obs","sim","method"))
  
  # lci = binom.test(10,200,0.05)$conf.int[1]
  # uci = binom.test(10,200,0.05)$conf.int[2]
  # huron = data.frame(x=plage,lci,uci)
  # 
  # 
  # p = ggplot(data=res) + 
  #   geom_point(aes(x=sim,y=value,col=method))+
  #   facet_grid(variable~obs)+
  #   theme_bw()+
  #   geom_ribbon(data=huron,aes(x=x,ymin=lci,ymax=uci),alpha=0.4) + 
  #   geom_line(data=data.frame(x=c(0,10),y=c(0.05,0.05)),aes(x=x,y=y))+
  #   coord_cartesian(xlim=c(plage[1],last(plage)))+
  #   xlab(name);p
  
  return(res)
}

power_graph_test_sep = function(K,N,name,plage,nk=8,method){
  ###### ntime
  
  res = data.frame()
  for(nom in 1:length(name)){
    print(nom)
    # nom=nom+1
    for(n in N[[nom]]){
      for(k in K[[nom]]){
        for(ntime in nk[[nom]]){
          for(param_obs in plage[[nom]]){
            for(param_sim in plage[[nom]]){
              path_obs = paste0("ntime_",ntime,"/K_",k,"/N_",n,"/",name[nom],"_",param_obs)
              
              
              for(m in method[[nom]]){
                
           
                alpha=rep(0.05,4)
                
                
                path_res = path_obs
                load(paste0(path_res,"/power_",param_obs,"_",param_sim,"_m",m,".rda"))
                power = get(paste0("typeIerror_censored_m",m))
                pval = (typeIerror(power,alpha))
                temp=data.frame(K=k,N=n,ntime=ntime,param=name[nom],obs=param_obs,sim=param_sim,method=m,tte_adj=pval$tte_adj,tte_ks=pval$tte_ks,
                                l_adj=pval$l_adj,l_ks=pval$l_ks,combi_adj=pval$combi_adj,combi_ks=pval$combi_ks,
                                l_adj_mean=pval$l_adj_mean,
                                l_adj_var=pval$l_adj_var,
                                l_adj_norm=pval$l_adj_norm,
                                tte_adj_mean=pval$tte_adj_mean,
                                tte_adj_var=pval$tte_adj_var,
                                tte_adj_norm=pval$tte_adj_var)
                res = rbind(res,temp)
              }
              
            }
          }
        }
      }
    }
  }
  
  res = data.table(res)
  
  # res$method=factor2(res$method,c("full","same_vpc")[method[[1]]])
  
  res=melt(res,id.vars = c("K","N","ntime","param","obs","sim","method"))
  
  # lci = binom.test(10,200,0.05)$conf.int[1]
  # uci = binom.test(10,200,0.05)$conf.int[2]
  # huron = data.frame(x=plage,lci,uci)
  # 
  # 
  # p = ggplot(data=res) + 
  #   geom_point(aes(x=sim,y=value,col=method))+
  #   facet_grid(variable~obs)+
  #   theme_bw()+
  #   geom_ribbon(data=huron,aes(x=x,ymin=lci,ymax=uci),alpha=0.4) + 
  #   geom_line(data=data.frame(x=c(0,10),y=c(0.05,0.05)),aes(x=x,y=y))+
  #   coord_cartesian(xlim=c(plage[1],last(plage)))+
  #   xlab(name);p
  
  return(res)
}

power_graph_test_fast = function(K,N,name,plage,nk=8,method){
  ###### ntime
  
  res = data.frame()
  for(nom in 1:length(name)){
    print(nom)
    # nom=nom+1
    for(n in N[[nom]]){
      for(k in K[[nom]]){
        for(ntime in nk[[nom]]){
          for(param_obs in plage[[nom]]){
            for(param_sim in plage[[nom]]){
              path_obs = paste0("ntime_",ntime,"/K_",k,"/N_",n,"/",name[nom],"_",param_obs)
              
              
              for(m in method[[nom]]){
                
                
                alpha=rep(0.05,4)
                
                
                path_res = path_obs
                # file.remove(paste0(path_res,"/power_",param_obs,"_",param_sim,"_m",m,".rda"))
                if(file.exists(paste0(path_res,"/power_",param_obs,"_",param_sim,"_m",m,".rda"))){
                  load(paste0(path_res,"/power_",param_obs,"_",param_sim,"_m",m,".rda"))
                  power = get(paste0("typeIerror_censored_m",m))
                  names(power)[1]="rep"

                  if("combi_adj"%!in%names(power)){
                    power[,combi_adj:=min(1,min(longitudinal_adj,TTE_adj)*2),by="rep"]
                    power[,combi_ks:=min(1,min(longitudinal_ks,TTE_ks)*2),by="rep"]
                  }

                  cause_adj=power[!(longitudinal_adj>0.05/2 & TTE_adj>0.05/2),list(cause_adj=mean(TTE_adj<0.05/2))]
                  cause_ks=power[!(longitudinal_ks>0.05/2 & TTE_ks>0.05/2),list(cause_ks=mean(TTE_ks<0.05/2))]


                  pval=power[,list(power_adj=mean(combi_adj<0.05),
                                   power_ks=mean(combi_ks<0.05))]

                  temp=data.frame(K=k,N=n,ntime=ntime,param=name[nom],obs=param_obs,sim=param_sim,method=m,
                                  combi_adj=pval[[1]],combi_ks=pval[[2]],cause_adj=cause_adj[[1]],cause_ks=cause_ks[[1]])
                }else{
                  temp=data.frame(K=k,N=n,ntime=ntime,param=name[nom],obs=param_obs,sim=param_sim,method=m,
                                  combi_adj=NA,combi_ks=NA,cause_adj=NA,cause_ks=NA)
                }

                res = rbind(res,temp)
              }
              
            }
          }
        }
      }
    }
  }
  
  res = data.table(res)
  
  # res$method=factor2(res$method,c("full","same_vpc")[method[[1]]])
  
  res=melt(res,id.vars = c("K","N","ntime","param","obs","sim","method"))
  
  # lci = binom.test(10,200,0.05)$conf.int[1]
  # uci = binom.test(10,200,0.05)$conf.int[2]
  # huron = data.frame(x=plage,lci,uci)
  # 
  # 
  # p = ggplot(data=res) + 
  #   geom_point(aes(x=sim,y=value,col=method))+
  #   facet_grid(variable~obs)+
  #   theme_bw()+
  #   geom_ribbon(data=huron,aes(x=x,ymin=lci,ymax=uci),alpha=0.4) + 
  #   geom_line(data=data.frame(x=c(0,10),y=c(0.05,0.05)),aes(x=x,y=y))+
  #   coord_cartesian(xlim=c(plage[1],last(plage)))+
  #   xlab(name);p
  
  return(res)
}

power_plot = function(res,name,limits){
  # name="PSA_0"
  lci = binom.test(10,200,0.05)$conf.int[1]
  uci = binom.test(10,200,0.05)$conf.int[2]
  # limits=get(paste0("plage_",name))
  huron = data.frame(x=limits,lci,uci)

  if(length(grep("=",res$obs))==0){
    res$obs=factor(res$obs)
    levels(res$obs) = paste0(name,"==",levels(res$obs))
  }
  
  res$obs=factor(res$obs)
  res$N=factor(res$N)
  p = ggplot(data=res) +
    geom_line(aes(x=sim,y=value,col=N,lty=method))+
    geom_point(aes(x=sim,y=value,col=N))+
    facet_grid(.~obs,labeller = label_parsed)+
    theme_bw()+
    geom_ribbon(data=huron,aes(x=x,ymin=lci,ymax=uci),alpha=0.4) +
    geom_line(data=data.frame(x=c(limits[1],last(limits)),y=c(0.05,0.05)),aes(x=x,y=y))+
    coord_cartesian(xlim=c(limits[1],last(limits)))+
    xlab(name)
  
  
  p=p+theme(axis.text.x = element_text(angle=0))+
    scale_color_manual(name="Sample size",values=c("blue","orange","red"))+
    # guides(lty=T)+
    scale_x_continuous(name=(paste0("Value of ",name," in M")),breaks = limits)+
    # scale_linetype_manual(name="Cases",values=c(1,2),label=c("combi_adj","combi_ks"))+
    scale_y_continuous(name="Proportion of rejected datasets",limits = c(0,1));p
  return(p)
  
}
# Eco moving legend to top
power_plot_clean = function(res,name,limits, legend="top"){
  # name="PSA_0"
  lci = binom.test(10,200,0.05)$conf.int[1]
  uci = binom.test(10,200,0.05)$conf.int[2]
  # limits=get(paste0("plage_",name))
  huron = data.frame(x=limits,lci,uci)
  
  if(length(grep("=",res$obs))==0){
    res$obs=factor(res$obs)
    levels(res$obs) = paste0(name,"==",levels(res$obs))
  }
  
  res$obs=factor(res$obs)
  res$N=factor(res$N)
  p = ggplot(data=res) +
    geom_line(aes(x=sim,y=value,col=N))+
    geom_point(aes(x=sim,y=value,col=N))+
    facet_grid(.~obs,labeller = label_parsed)+
    theme_bw()+
    geom_ribbon(data=huron,aes(x=x,ymin=lci,ymax=uci),alpha=0.4) +
    geom_line(data=data.frame(x=c(limits[1],last(limits)),y=c(0.05,0.05)),aes(x=x,y=y))+
    coord_cartesian(xlim=c(limits[1],last(limits)))+
    xlab(name) +
    scale_color_manual(name="Sample size",values=c("blue","orange","red"))

  if(length(legend)==2)  p<-p + theme(axis.text.x = element_text(angle=0),legend.position=legend) else {
    if(legend=="top") p<-p + theme(axis.text.x = element_text(angle=0),legend.position="top") 
    if(legend=="none")  p<-p + theme(axis.text.x = element_text(angle=0),legend.position="none")
  }
  p=p+ scale_x_continuous(name=(paste0("Value of ",name," in M")),breaks = limits)+
    # scale_linetype_manual(name="Cases",values=c(1,2),label=c("combi_adj","combi_ks"))+
    scale_y_continuous(name="Proportion of rejected datasets",limits = c(0,1));p
  return(p)
}


power_graph_cox = function(K,N,name,plage,nk=8){
  ###### ntime
  
  res = data.frame()
  for(nom in 1:length(name)){
    print(nom)
    # nom=nom+1
    for(n in N[[nom]]){
      for(k in K[[nom]]){
        for(ntime in nk[[nom]]){
          for(param_obs in plage[[nom]]){
            for(param_sim in plage[[nom]]){
              path_obs = paste0("../monolix/ntime_",ntime,"/K_",k,"/N_",n,"/",name[nom],"_obs_",param_obs)
              
              
              # for(m in method[[nom]]){
              
              if(param_obs!=param_sim){
                # if(!file.exists(paste0(path_obs,"/",name[nom],"_sim_",param_obs,"/power.rda"))) stop(paste0("File don't exist: ",
                # paste0(path_obs,"/",name[nom],"_sim_",param_obs,"/power.rda")))
                a=try(load(paste0(path_res,"/",name[nom],"_sim_",param_obs,"/power.rda")))
                if(length(grep("Error",a))>0) {
                  alpha=0.05
                }else{
                  alpha=quantile(power,0.05)
                }
                
                # alpha=c()
                # alpha=c(alpha,quantile(power,0.05))
                # print(alpha)
                
              }else{
                alpha=0.05
              }
              
              
              path_res = path_obs
              a=try(load(paste0(path_res,"/",name[nom],"_sim_",param_sim,"/power.rda")))
              if(length(grep("Error",a))>0) {
                pval=NA
              }else{
                pval = mean(power<=alpha)
              }
              
              # power = get(paste0("typeIerror_censored_m",m))
              
              temp=data.frame(K=k,N=n,ntime=ntime,param=name[nom],obs=param_obs,sim=param_sim,cox_ks=pval)
              res = rbind(res,temp)
            }
            
            # }
          }
        }
      }
    }
  }
  
  res = data.table(res)
  
  res=melt(res,id.vars = c("K","N","ntime","param","obs","sim"))
  
  # lci = binom.test(10,200,0.05)$conf.int[1]
  # uci = binom.test(10,200,0.05)$conf.int[2]
  # huron = data.frame(x=plage,lci,uci)
  # 
  # 
  # p = ggplot(data=res) + 
  #   geom_point(aes(x=sim,y=value,col=method))+
  #   facet_grid(variable~obs)+
  #   theme_bw()+
  #   geom_ribbon(data=huron,aes(x=x,ymin=lci,ymax=uci),alpha=0.4) + 
  #   geom_line(data=data.frame(x=c(0,10),y=c(0.05,0.05)),aes(x=x,y=y))+
  #   coord_cartesian(xlim=c(plage[1],last(plage)))+
  #   xlab(name);p
  
  return(res)
}


stairstepn <- function( data, direction="hv", yvars="y" ) {
  direction <- match.arg( direction, c( "hv", "vh" ) )
  data <- as.data.frame( data )[ order( data$x ), ]
  n <- nrow( data )
  
  if ( direction == "vh" ) {
    xs <- rep( 1:n, each = 2 )[ -2 * n ]
    ys <- c( 1, rep( 2:n, each = 2 ) )
  } else {
    ys <- rep( 1:n, each = 2 )[ -2 * n ]
    xs <- c( 1, rep( 2:n, each = 2))
  }
  
  data.frame(
    x = data$x[ xs ]
    , data[ ys, yvars, drop=FALSE ]
    , data[ xs, setdiff( names( data ), c( "x", yvars ) ), drop=FALSE ]
  ) 
}

stat_stepribbon <- 
  function(mapping = NULL, data = NULL, geom = "ribbon", position = "identity", inherit.aes = TRUE) {
    ggplot2::layer(
      stat = Stepribbon, mapping = mapping, data = data, geom = geom, 
      position = position, inherit.aes = inherit.aes
    )
  }

StatStepribbon <- 
  ggproto("stepribbon", Stat,
          compute_group = function(., data, scales, direction = "hv", yvars = c( "ymin", "ymax" ), ...) {
            stairstepn( data = data, direction = direction, yvars = yvars )
          },                        
          required_aes = c( "x", "ymin", "ymax" )
  )


##############################
### Homemade function


factor2 = function(vec,label){
  vec = factor(vec)
  levels(vec)=label
  vec
}

as.num = function(x){
  return(as.numeric(as.character(x)))
}

`%!in%` = Negate(`%in%`)


controleur = function(n_launch=10){
  system("kstat|grep marc.cerou|grep batch > mes_jobs.txt")
  mes_jobs = readLines("mes_jobs.txt")
  continue=F
  if(length(mes_jobs)<n_launch) continue=T
  
  return(continue)
  
}

creation_launch_file = function(newfile,param,n_launch=10,path_res = "."){
  input = paste(param,collapse = "_")
  out = paste0(path_res,"/","main_",input,".R")
  cat(newfile,sep="\n",file=out)
  
  batch = character() 
  batch[1] = "cd /media/share/U738/marc.cerou/These/Projet_3_NPDE_joint/PSA/scriptR"
  batch[2] = paste0("/home/progs/R-333/bin/./R --vanilla < ",out)
  
  out2 = paste0("batch_",input,".sh")
  cat(batch,sep="\n",file=out2)
  system(paste0("chmod +x ",out2))
  
  continue=controleur(n_launch)
  
  if(continue){
    print(paste("lancement ksub pour fichier",out))
    system(paste0("ksub -d=rstan ",out2))
    Sys.sleep(2)
  }else{
    while(!continue){
      Sys.sleep(10)
      continue=controleur(n_launch)
    }
    print(paste("lancement ksub pour fichier",out))
    system(paste0("ksub -d=rstan ",out2))
    Sys.sleep(2)
  }
  
}


auto_bin<-function(xvec,plot.opt,verbose=FALSE) {
  # plot.opt should contain the following items
  # bin.method: binning method (one of "optimal","width","user","equal")
  # bin.number: number of bins
  # bin.breaks
  # bin.extreme
  # xlog
  xvec1<-xvec
  xvec<-xvec[!is.na(xvec)]
  if(is.na(pmatch(plot.opt$bin.method,c("optimal","width","user","equal")))) {
    if(verbose) cat("Binning method",plot.opt$bin.method,"not found, reverting to equal binning\n")
    plot.opt$bin.method<-"equal"
  }
  if(!is.na(pmatch(plot.opt$bin.method,"optimal"))) {
    if(!("mclust"%in%.packages(all.available = TRUE))) {
      if(verbose) cat("mclust library not installed, reverting to equal binning\n")
      plot.opt$bin.method<-"equal"
    } else {
      #			require(mclust)
      if(is.null(plot.opt$bin.number) || is.na(plot.opt$bin.number)) plot.opt$bin.number<-10
    }
  }
  if(!is.na(pmatch(plot.opt$bin.method,"user")) & is.null(plot.opt$bin.breaks)) {
    if(verbose) cat("User-defined method specified, but bin.breaks is empty; reverting to equal binning\n")
    plot.opt$bin.method<-"equal"
  }
  if(!is.na(pmatch(plot.opt$bin.method,c("equal","width"))) & is.null(plot.opt$bin.number)) {
    nbin<-length(unique(xvec))
    if(nbin>20) nbin<-20
    plot.opt$bin.number<-nbin
  }
  nbin<-plot.opt$bin.number
  if(is.na(pmatch(plot.opt$bin.method,c("optimal","user"))) && length(unique(xvec))<=nbin) {
    xgrp<-match(xvec,sort(unique(xvec)))
    xpl<-tapply(xvec,xgrp,mean)
  } else { 	
    if(!is.na(pmatch(plot.opt$bin.method,"user"))) {
      bnds<-plot.opt$bin.breaks
      if(min(bnds)>=min(xvec)) bnds<-c(min(xvec)*(1-sign(min(xvec))*0.001),bnds)
      if(max(bnds)<max(xvec)) bnds<-c(bnds,max(xvec))
    }
    if(!is.na(pmatch(plot.opt$bin.method,"equal"))) {
      xvec2<-xvec;xvec2[xvec2==min(xvec)]<-min(xvec)-1      
      if(!is.null(plot.opt$bin.extreme) & length(plot.opt$bin.extreme)==2) {
        xq<-plot.opt$bin.extreme
        xquant<-c(0,seq(xq[1],xq[2],length.out=(nbin-1)),1)
      } else xquant<-(0:nbin)/nbin
      bnds<-unique(quantile(xvec2,xquant,type=8))
    }
    if(!is.na(pmatch(plot.opt$bin.method,"width"))) {
      if(plot.opt$xlog) xvec2<-log(xvec[xvec>0]) else xvec2<-xvec
      if(!is.null(plot.opt$bin.extreme) & length(plot.opt$bin.extreme)==2) {
        xq<-plot.opt$bin.extreme
        xq1<-quantile(xvec2,xq,type=8)
        bnds<-c(min(xvec2),seq(xq1[1],xq1[2],length.out=(nbin-1)),max(xvec2))
        bnds<-sort(unique(bnds))
      } else bnds<-seq(min(xvec2),max(xvec2),length.out=(nbin+1))
      if(plot.opt$xlog) {
        bnds<-exp(bnds)
        bnds[length(bnds)]<-bnds[length(bnds)]*(1+sign(bnds[length(bnds)])*0.001)
        if(sum(xvec<=0)>0) bnds<-c(min(xvec),bnds)
      } 
      bnds[1]<-bnds[1]*(1-sign(bnds[1])*0.001)
    }
    if(!is.na(pmatch(plot.opt$bin.method,"optimal"))) {
      yfit<-mclust::Mclust(xvec,G=((nbin-5):(nbin+5)))
      xgrp<-yfit$classification
      xpl<-yfit$parameters$mean
      xpl<-xpl[match(names(table(xgrp)),names(xpl))]
      minx<-tapply(xvec,xgrp,min)
      maxx<-tapply(xvec,xgrp,max)
      bnds <- c(minx[1],(minx[-c(1)]+maxx[-length(maxx)])/2,maxx[length(maxx)])
      names(xpl)<-paste("[",tapply(xvec,xgrp,min),"-",tapply(xvec,xgrp,max),"]", sep="")
    } else {
      xgrp<-factor(cut(xvec,bnds,include.lowest=F))
      xpl<-tapply(xvec,xgrp,mean)
    }
  }
  nbin<-length(unique(xgrp))
  npl<-tapply(xvec,xgrp,length)
  tab<-cbind(Interval=names(xpl),Centered.On=format(xpl,digits=2),Nb.obs=npl)
  row.names(tab)<-1:dim(tab)[1]
  if(verbose) {
    xnam<-switch(EXPR=plot.opt$bin.method,equal="by quantiles on X", width="equal sized intervals",user="user-defined bins",optimal="clustering algorithm")
    cat("Method used for binning:",xnam,", dividing into the following",nbin,"intervals\n")
    print(tab,quote=F)
  }
  xgrp2<-rep(NA,length(xvec1))
  xgrp2[!is.na(xvec1)]<-xgrp
  return(list(xgrp=xgrp2,xat=xpl,xbound=bnds))
}
