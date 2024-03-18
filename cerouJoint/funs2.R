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




####################################
#### Power computation


compute_typeIerror_vfast = function(sim,obs,response){
  
  pb = txtProgressBar(min = 0, max = 200, style = 3)
  typeIerror_censored = list(longitudinal = list(adjusted = list(c()), ks=c()),
                             TTE = list(adjusted = list(), ks=c()))
  for(o in 1:length(sim)){
    obs[[o]][,`:=`(rep=as.num(rep),id=as.num(id))]
    sim[[o]][,`:=`(rep=as.num(rep),id=as.num(id))]
  }
  
  
  
  for(i_in_obs in 1:10){
    
    
    setTxtProgressBar(pb, i_in_obs)
    data_L = obs$psa_with_dropout[rep==i_in_obs][,c("rep","id","time","y"),with=F]
    data_TTE = obs$TTE[rep==i_in_obs]
    names(data_TTE)[c(1:2,5,4)] = c("rep","id","time","y")
    
    
    
    
    
    sim_L = sim$psa_wo_dropout[,c("rep","id","time","y"),with=F]
    sim_L = merge(sim_L,data_L[,c("id","time"),with=T],by=c("id","time"))
    setkeyv(sim_L,c("rep","id","time"))
    sim_TTE = sim$TTE
    names(sim_TTE)[1:4] = c("rep","id","time","y")
    
    
    
    
    
    npde_rep = main_compute_npde(obs = list(data_L,data_TTE),sim = list(sim_L,sim_TTE),
                                 type_outcome = c("continuous","TTE"),dependancies = list(2,0),verbose = F,
                                 options=NULL)
    
    pval=main_compute_pvalue(npde_rep,type_outcome = c("continuous","TTE"))
    
    
    
    typeIerror_censored$longitudinal$ks = c(typeIerror_censored$longitudinal$ks,ks.test(npde_rep[[1]][[1]]$npde,y = "pnorm")$p.val)
    typeIerror_censored$longitudinal$adjusted[i_in_obs] = pval[[1]]
    
    
    typeIerror_censored$TTE$ks = c(typeIerror_censored$TTE$ks,ks.test(npde_rep[[2]][[1]]$npde,y = "pnorm")$p.val)
    typeIerror_censored$TTE$adjusted[i_in_obs] = pval[[2]]
    
  }
  typeIerror_censored
}

compute_typeIerror_vfast_v2 = function(sim,obs,response,method="all"){
  
  
  for(o in 1:length(sim)){
    obs[[o]][,`:=`(rep=as.num(rep),id=as.num(id))]
    sim[[o]][,`:=`(rep=as.num(rep),id=as.num(id))]
  }
  
  typeIerror_censored = data.table(i_in_obs=1:200)
  
  
  nrep=length(unique(sim[[1]]$rep))
  
  sim_full=sim$psa_wo_dropout[,c("rep","id","time","y"),with=F]
  
  # if(method!="all"){
  #   sim_o=sim$psa_with_dropout[,c("rep","id","time","y"),with=F]
  # }else{
  #   sim_o=sim$psa_wo_dropout[,c("rep","id","time","y"),with=F]
  # }
  # ymat = sim_full[,{
  #   ymat=correlation_matrix(sim_full,ID=id,nrep=nrep,decorr.method ="cholesky")
  # },by="id"]
  # names(ymat)[1]="ID"
  # 
  # tabsim_decorr = sim_o[,{
  #   ymat2=ymat[ID==id]
  #   ymat2$ID=NULL
  #   ydsim=decorr_i(ymat2,sim_o,id,nrep,"cholesky")
  #   names(ydsim)[names(ydsim)=="yd"]="ydsim"
  #   names(ydsim)[names(ydsim)=="y"]="ysim"
  #   ydsim
  # },by="id"]
  # names(tabsim_decorr)[1]="ID"
  
  
  # dependancies=list(2,0)
  # if(method=="all") dependancies=list(0,0)
  
  #i_in_obs=1
  res = typeIerror_censored[i_in_obs%in%1:200,{
    print(paste0(i_in_obs/200*100, "%"))
    data_L = obs$psa_with_dropout[rep==i_in_obs][,c("rep","id","time","y"),with=F]
    data_TTE = obs$TTE[rep==i_in_obs]
    names(data_TTE)[c(1:2,5,4)] = c("rep","id","time","y")
    sim_L = sim$psa_wo_dropout[,c("rep","id","time","y"),with=F]
    sim_L = merge(sim_L,data_L[,c("id","time"),with=T],by=c("id","time"))
    setkeyv(sim_L,c("rep","id","time"))
    sim_TTE = sim$TTE
    names(sim_TTE)[1:4] = c("rep","id","time","y")
    npde_rep = main_compute_npde(obs = list(data_L,data_TTE),sim = list(sim_L,sim_TTE),
                                 type_outcome = c("continuous","TTE"),dependancies = list(2,0),verbose = F,
                                 # options=list(list(ymat=ymat,tabsim_decorr=tabsim_decorr,method=method)))
                                 options=list(list(method=method)))
    # main_compute_plot(npde_rep,type_outcome = c("continuous","TTE"),options=list(list(outcome="npde",type="scatter"),NULL))[[1]]
    pval=main_compute_pvalue(npde_rep,type_outcome = c("continuous","TTE"))
    longitudinal_ks=ks.test(npde_rep[[1]][[1]]$npde,y = "pnorm")$p.val
    TTE_ks=ks.test(npde_rep[[2]][[1]]$npde,y = "pnorm")$p.val
    list(longitudinal_adj = pval[[1]][[5]][4],
         longitudinal_ks = longitudinal_ks,
         TTE_adj = pval[[2]][[5]][4],
         TTE_ks=TTE_ks,
         combi_adj=pval[[3]],
         combi_ks=min(1,min(longitudinal_ks,TTE_ks)*2))
  },by="i_in_obs"]
  
  
  res
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
    scale_y_continuous(name="Proportion of rejected dataset",limits = c(0,1));p
  return(p)
  
}

#############################################################
# Computation of NPDE -------------------------
#############################################################

## Data formatting

# data_to_npdeData


## Main function ########

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
    if(dependancies[[o]]!=0) sim_dependancies = sim[[dependancies[[o]]]][,c("rep","id","time","y"),with=F]
    
    
    #### Load options ####
    if(type_outcome[o]=="categorical") order_cat = paste0(sort(unique(c(obs_o$y,sim_full_o$y))))
    # compute_pvalue=F
    censoring_type="right"
    ties=F
    compute_npde=T
    compute_npd=T
    decorr.method="cholesky"
    method="nall"
    
    if(length(options)>0){
      if(length(options)<o) options[o]=NULL
      if(!is.null(options[o][[1]])){
        opt = options[[o]]
        if("order_cat"%in%names(opt))  order_cat=opt[[which(names(opt)%in%"order_cat")]]
        # if("compute_pvalue"%in%names(opt))  compute_pvalue=opt[[which(names(opt)%in%"compute_pvalue")]]
        if("censoring_type"%in%names(opt))  censoring_type=opt[[which(names(opt)%in%"censoring_type")]]
        if("ties"%in%names(opt))  ties=opt[[which(names(opt)%in%"ties")]]
        if("compute_npde"%in%names(opt))  compute_npde=opt[[which(names(opt)%in%"compute_npde")]]
        if("compute_npd"%in%names(opt))  compute_npd=opt[[which(names(opt)%in%"compute_npd")]]
        if("decorr.method"%in%names(opt))  decorr.method=opt[[which(names(opt)%in%"decorr.method")]]
        if("method"%in%names(opt))  method=opt[[which(names(opt)%in%"method")]]
        
      }
    }
    
    
    #### Compute npde  ####
    if(type_outcome[o]=="continuous"){
      
      if(dependancies[[o]]==0){
        npde_o = computenpde_missing_fast(obs = obs_o,sim_full = sim_full_o,compute_npde = T,
                                          compute_npd = T,decorr.method = decorr.method,verbose=verbose,ties=ties,method=method)
      }else{
        names(sim_dependancies)[1:4]=c("rep","id","tte","status")
        sim_o = merge(sim_full_o,sim_dependancies,by=c("rep","id"))
        sim_o=sim_o[time<tte]
        
        npde_o = computenpde_missing_fast(obs = obs_o,sim = sim_o,sim_full = sim_full_o,compute_npde = T,
                                          compute_npd = T,decorr.method = decorr.method,verbose=verbose,
                                          ties=ties,method=method)

      }
      
      
    }else if(type_outcome[o]=="TTE"){
      
      if(dependancies[[o]]==0){
        npde_o = prediction_discrepancie(obs_o,sim_full_o,ties = ties,censoring_type=censoring_type)
      }else{
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

main_compute_pvalue = function(res,type_outcome="continuous",compute_pvalue=T,options=NULL){
  
  noutcome=length(res)
  res_test = list()
  if(length(compute_pvalue)==1) compute_pvalue = rep(compute_pvalue,noutcome)
  
  ##############  Computation of pvalue
  for(o in 1:noutcome){
    
    #### Load data and sim ####
    # print(paste0("test for outcome: ", type_outcome[[o]]))
    
    if(compute_pvalue[[o]]){
      obs_o = res[[o]][[1]] ## obs + npde
      
      
      sim_full_o = res[[o]][[2]]  ## sim + VPC like
      
      if(type_outcome[o]=="categorical") order_cat = paste0(sort(unique(c(obs_o$y,sim_full_o$y))))
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
  
  pvalue = sapply(1:noutcome,function(x) ifelse(!is.null(res_test[[x]]$p.value[4]),res_test[[x]]$p.value[[4]],NA ))
  global_pvalue = min(min(pvalue)*noutcome,1)
  res_test[[noutcome+1]]=global_pvalue
  
  
  return(res_test)
}

main_compute_plot = function(res,type_outcome="continuous",options=NULL){
  
  
  noutcome=length(res)
  res_plot = list()
  
  
  
  for(o in 1:noutcome){
    
    #### Load data and sim ####
    print(paste0("Plot for outcome: ", type_outcome[[o]]))
    
    
    npde_o = res[[o]][[1]] ## obs + npde
    sim_o = res[[o]][[2]]  ## sim + VPC like
    
    
    #### Load options ####
    if(type_outcome[[o]]=="categorical") order_cat = paste0(sort(unique(c(npde_o$y,sim_o$y))))
    censoring_type="right"
    outcome="npd"
    variable="time"
    conf=0.9
    type="scatter"
    
    if(length(options)>0){
      if(!is.null(options[[o]])){
        opt = options[[o]]
        if("order_cat"%in%names(opt))  order_cat=opt[[which(names(opt)%in%"order_cat")]]
        if("censoring_type"%in%names(opt))  censoring_type=opt[[which(names(opt)%in%"censoring_type")]]
        if("outcome"%in%names(opt))  outcome=opt[[which(names(opt)%in%"outcome")]]
        if("variable"%in%names(opt))  variable=opt[[which(names(opt)%in%"variable")]]
        if("conf"%in%names(opt))  conf=opt[[which(names(opt)%in%"conf")]]
        if("type"%in%names(opt))  type=opt[[which(names(opt)%in%"type")]]
        
        
      }
    }
    
    
    #### compute plot ####
    if(type_outcome[[o]]%in%c("continuous")){
      
      if(type=="scatter"){
        myplot = plot_npde_scatter_dt_L(npde_o,outcome=outcome,method_band = "theo",conf = conf)
      }else if(type=="vpc"){
        myplot = VPC_L(npde_o,sim_o)
      }
      
      
      res_plot[[o]] = myplot  
    }else if(type_outcome[[o]]%in%c("TTE")){
      
      if(type=="scatter"){
        if(outcome=="npd") outcome="npde"
        myplot = plot_npde_scatter_dt_TTE(npde_o,variable = variable,outcome = outcome,adj = T,conf = conf)
      }else if(type=="vpc"){
        
        myplot = VPC_TTE(npde_o,sim_o,censoring_type)[[1]]
      }
      
      res_plot[[o]] = myplot
    }else{
      
      if(type=="scatter"){
        myplot = plot_npde_scatter_dt_L(npde_o,outcome=outcome,method_band = "theo",conf = conf)
      }
      
      res_plot[[o]] = myplot
      
    }
    res_plot[[o]]=res_plot[[o]]+theme_pubr()
    
    
  }
  
  
  
  return(res_plot)
}

## TTE data ======

prediction_discrepancie = function(dat,simulated_data,ties=F,censoring_type="right"){
  #dat=obs_TTEO
  #simulated_data=sim_TTE
  
  
  K = length(unique(simulated_data$rep))
  n_ID=length(unique(dat$id))
  
  
  TTE_simulated = simulated_data[,c("rep","id","time","y"),with=F]
  
  # TTE_simulated = TTE_simulated[order(TTE_simulated$time),]
  
  if(n_ID >length(unique(simulated_data$id))){
    stop(paste("Error: ",n_ID," ID in the data > ",length(unique(simulated_data$id))," ID in the prediction distribution dataset",sep=""))
  }
  
  quant=NULL
  data = copy(dat)
  
  setkeyv(TTE_simulated,c("id","rep"))
  
  data2 = merge(data,TTE_simulated,by="id")
  
  if(censoring_type!="interval"){
    data3 = data2[,list(quant=mean(time.y<unique(time.x)),medTime=mean(time.y)),by="id"]
  }else{
    
    data3 = data2[,list(quant=mean(time.y<unique(time.x)),
                        quant2=mean(time.y<unique(time2)),
                        medTime=mean(time.y)),by="id"]
  }
  data = merge(data,data3,by="id")
  
  data[,pd:=quant]
  
  if(censoring_type=="right"){
    data[y==0,pd:=runif(length(quant),quant,1)]
  }else if(censoring_type=="interval"){
    data[is.na(time2)|is.infinite(time2),pd:=runif(length(quant),quant,1)]
    data[!(is.na(time2)|is.infinite(time2)),pd:=runif(length(quant),quant,quant2)]
  }else{
    data[y==0,pd:=runif(length(quant),quant,1)]
    data[y==1,pd:=runif(length(quant),0,quant)]
  }
  
  
  pd_wo_ties = data$pd
  pd=data$pd
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
  
  data2.2=merge(data2,data,by="id")
  imp = unique(data2.2[,list(imputed_time=quantile(time.y,pd_wo_ties)),by="id"])
  if(censoring_type=="interval"){
    imp = unique(data2.2[y==0,list(imputed_time=quantile(time.y,pd_wo_ties)),by="id"])
    data3.2=copy(data)
    imp2 = unique(data[y!=0,list(imputed_time=mean(c(time,time2))),by="id"])
    imp=rbind(imp,imp2)
    setkeyv(imp,"id")
  }
  
  
  data=merge(imp,data,by="id")
  
  # adj = reject_stat(data$npde)
  # ks = ks.test(data$npde,"pnorm")
  
  
  
  
  # res = list(tab=data,test_adj=adj,test_ks=ks)
  
  
  return(data)
  
  
  
  
  
}




## Categorical data ======


#### For all replicates
compute_npde_discrete_m2 = function(obs,sim,response="y",order_cat = paste0(1:3)){
  
  # 
  # obs$rep = as.num(obs$rep)
  # obs$id = as.num(obs$id)
  # sim$rep = as.num(sim$rep)
  # sim$id = as.num(sim$id)
  
  sim2 = sim[,c("rep","id","time","y"),with=F]
  
  
  
  
  
  if("rep"%!in%names(obs)) obs$rep=1
  
  res=copy(obs)
  res$pd=-1
  res$pd_wo_ties=-1
  res$npd=-1
  
  res[,paste0("P",order_cat):=0]
  # pb = txtProgressBar(min = 0, max = length(unique(obs$rep)), style = 3)
  all_rep = unique(obs$rep)
  for(i_in_obs in 1:length(all_rep)){
    
    
    npde <- calc_npd_discrete(dat=obs[rep==all_rep[i_in_obs]],sim = sim2,response = "y",ties=F,order_cat=order_cat)
    
    
    # obs[rep==i_in_obs]$pd=npde$pd
    # obs[rep==i_in_obs]$pd_wo_ties=npde$pd_wo_ties
    # obs[rep==i_in_obs]$npd=npde$npd
    # 
    res[rep==all_rep[i_in_obs]] = npde[,names(res),with=F]
    
    # setTxtProgressBar(pb, i_in_obs)
    
  }
  obs2 = copy(res)
  
  
  
  obs2
}

#### For one replicate

cat_frac = function(y,name=c(paste0(1:3))){
  p=prop.table(table(y))
  new_p=rep(0,length(name))
  new_p[sapply(names(p), function(x) as.num(which(name%in%x)))]=p
  
  cs = as.list(cumsum(new_p))
  # new_p = as.list(new_p)  
}

cat_frac_prob = function(y,name=c(paste0(1:3))){
  p=prop.table(table(y))
  new_p=rep(0,length(name))
  new_p[sapply(names(p), function(x) as.num(which(name%in%x)))]=p
  
  return(as.list(new_p))
  # new_p = as.list(new_p)  
}

calc_npd_discrete = function(dat,sim,response="DV1",method="unif",ties=F,order_cat = paste0(1:3)){
  
  dat$y = dat[[response]]
  sim$y = sim[[response]]
  
  design_tij = unique(dat[,c("id","time"),with=F])
  sim = merge(sim,design_tij,by=c("id","time"))
  
  # print(unique(dat$rep))
  
  ID = unique(dat$id)
  dat$pd = -1
  # ncat = length(unique(dat$y))
  # pb = txtProgressBar(min = 0, max = length(unique(obs$id)), style = 3)
  # sim[,paste0("P",c(1,3,2)):=NULL]
  # sim[,paste0("P",c(1,3,2)):= cat_frac(y,size=3,name=paste0(c(1,3,2))),by=c("id","time")]
  # sim2<-copy(sim)
  # 
  sim[,paste0("P",order_cat):= cat_frac(y,name=order_cat),by=c("id","time")]
  
  # sim[,list(P0=cat_frac(y,name=order_cat)[[1]]),by=c("id","time")]
  
  prop_sim=unique(sim[,c("id","time",paste0("P",order_cat)),with=F])
  
  dat2 = merge(dat,prop_sim,by=c("id","time"))
  
  dat2[,pd:=ifelse(y==order_cat[1],
                   runif(1,0,get(paste0("P",order_cat[1]))),
                   runif(1,get(paste0("P",y-1)),get(paste0("P",y)))),by=c("id","time")]
  
  nrep=length(unique(sim$rep))
  dat2[,pd_wo_ties:=pd]
  dat2[pd==0,pd_wo_ties:=runif(length(pd),0,1/nrep)]
  dat2[pd==1,pd_wo_ties:=runif(length(pd),1-1/nrep,1)]
  
  # dat[,pd:=calc_pd_discrete(y,sim,time2=time,method=method,ties=ties,id=unique(id),pb,cat = ncat),
  # by=c("id","time")]
  # 
  # 
  # for(i in 1:length(ID)){
  #   obs_i = dat[id==ID[i]]
  #   sim_i = sim[id==ID[i]]
  #   for(j in obs_i$time){
  #     pd_ij = calc_pd_discrete(obs_i[time==j][["y"]],
  #                              sim_i[time==j][["y"]],method=method,ties=ties)
  #     dat[dat$id==ID[i]&time==j]$pd = pd_ij
  #   }
  #   setTxtProgressBar(pb, i)
  # }
  # dat$pd_wo_ties = dat$pd
  # dat[pd==0,pd_wo_ties:=runif(length(pd),0,1/500)]
  # dat[pd==1,pd_wo_ties:=runif(length(pd),1-1/500,1)]
  # 
  # if(!ties) dat[,pd_wo_ties:=pd_wo_ties+runif(length(pd_wo_ties),0,1/1000)]
  dat2$npd = qnorm(dat2$pd_wo_ties)
  dat2
}

compute_test_prop = function(obs,sim,order_cat){
  
  res=copy(obs)
  if("rep"%!in%names(res)) res$rep=1
  
  
  prop = unique(res[,paste0("P",order_cat):= cat_frac_prob(y,name=order_cat),by=c("time")][,c("time",paste0("P",order_cat)),with=F])
  pred = unique(sim[,paste0("P",order_cat):= cat_frac_prob(y,name=order_cat),by=c("time")][,c("time",paste0("P",order_cat)),with=F])
  
  prop=melt(prop,id.vars = "time",value.name = "emp")
  pred=melt(pred,id.vars = "time",value.name = "th")
  res = merge(prop,pred,by=c("time","variable"))
  
  
  statT = res[,list(statT = sum((emp-th)^2/th)),by="time"]
  return(list(time=statT$time,statT=statT$statT))
  
}

## Longitudinal censored by TTE data #####
computenpde<-function(obs,sim,verbose=F,compute_npde=T,compute_npd=T,decorr.method = "cholesky") {
  
  tab<-data.table(obs) # non-missing data
  
  tabsim<-data.table(sim) # corresponding simulated data with all data
  
  
  
  
  
  
  setkeyv(tabsim,c("id"))
  
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
    
    if(verbose) setTxtProgressBar(pb, i)
    
    obs_i = tab[id==id_vec[i]]
    sim_i = tabsim[id==id_vec[i]]
    
    
    nrep = length(unique(sim_i$rep))
    # print(names(sim_i))
    if(compute_npde) pde_i = try(calcnpde(obs_i,sim_i,nrep,decorr.method = decorr.method),T)
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

computenpde_missing<-function(obs,sim,sim_full,verbose=F,compute_npde=T,compute_npd=T,decorr.method = "cholesky") {
  
  tab<-data.table(obs) # non-missing data
  
  tabsim_decorr<-data.table(sim_full) # corresponding simulated data with all data
  tabsim<-data.table(sim) # corresponding simulated data while reproducing the censoring mechanism    
  
  
  
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
    
    if(verbose) setTxtProgressBar(pb, i)
    
    obs_i = tab[id==id_vec[i]]
    sim_i = tabsim[id==id_vec[i]]
    sim_i_decorr = tabsim_decorr[id==id_vec[i]]
    
    nrep = length(unique(sim_i$rep))
    # print(names(sim_i))
    if(compute_npde) pde_i = try(calcnpde(obs_i,sim_i,sim_i_decorr,nrep,decorr.method = decorr.method),T)
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

computenpde_missing_fast<-function(obs,sim=NULL,sim_full,verbose=F,compute_npde=T,
                                   compute_npd=T,decorr.method = "cholesky",ties=F,tabsim_decorr=NULL,ymat=NULL,method="nall") {
  
  tab<-data.table(obs) # non-missing data
  
  if(is.null(sim)) sim=sim_full
  
  tabsim_full<-data.table(sim_full) # corresponding simulated data with all data
  tabsim<-data.table(sim) # corresponding simulated data while reproducing the censoring mechanism    
  
  design_data = unique(tab[,c("id","time")])
  tabsim_full=merge(tabsim_full,design_data,by=c("id","time"))
  tabsim=merge(tabsim,design_data,by=c("id","time"))
  
  setkeyv(tabsim,"id")
  setkeyv(tabsim_full,"id")
  K = length(unique(tabsim$rep))
  
  
  id_vec = unique(tab$id)
  # npde = tab[order(id,time)]
  
  nrep=length(unique(tabsim$rep))
  
  # tabsim_decorr[id==2,{correlation_matrix(data.table(rep=rep,id=id,time=time,y=y),nrep=nrep,decorr.method =decorr.method)},by="id"]
  # tabsim_decorr[id==2,{correlation_matrix(tabsim_decorr[id==.(id)],nrep=nrep,decorr.method =decorr.method)},by="id"]
  # tabsim[,ydec:=decorr(tabsim[id==.(id)],tabsim_decorr[id==.(id)],nrep = nrep,decorr.method =decorr.method ),by=c("id")]
  # if(compute_npd&compute_npde) compute_npd=F
  # if(compute_npd){
  tabsim2=tabsim
  names(tabsim2)[names(tabsim2)=="y"]="ysim"
  # tab$rep=NULL
  tabsim2 = merge(tabsim2,tab[,-c("rep")],by=c("id","time")) ## add info about data in sim
  res_all = tabsim2[,list(pd=mean(ysim<y),nrep=unique(length(rep))),by=c("id","time")]
  # }
  
  if(compute_npde){
    if(method=="all") tabsim2 = tabsim_full
    if(method=="nall") tabsim2 = tabsim
    if(is.null(tabsim_decorr)){
      tabsim_decorr_same_design_as_data = tabsim2[,{
        ymat=t(correlation_matrix(tabsim_full,ID=id,nrep=nrep,decorr.method =decorr.method))
        ydsim=decorr_i(ymat,tabsim2,id,nrep,decorr.method)
        names(ydsim)[names(ydsim)=="yd"]="ydsim"
        names(ydsim)[names(ydsim)=="y"]="ysim"
        yd=decorr_i(ymat,tab,id,nrep,decorr.method)
        yd$rep=NULL
        merge(ydsim,yd,by=c("time"))
        # list(ydsim=ydsim,yd=yd)
      },by="id"]
    }else{
      tabsim_decorr_same_design_as_data = tab[,{
        ymat2=(ymat[ID==id])
        ymat2$ID=NULL
        ymat2=t(ymat2)
        yd=decorr_i(ymat2,tab,id,nrep,decorr.method)
        yd$rep=NULL
        ydsim=tabsim_decorr[ID==id]
        merge(ydsim,yd,by=c("time"))
      },by="id"]
      tabsim_decorr_same_design_as_data$rep=NULL
      # tabsim_decorr_same_design_as_data$nrep=NULL
    }

    # if(method=="nall"){
    # tabsim_decorr_same_design_as_data = merge(tabsim_decorr_same_design_as_data,tab,by=c("id","time"))
    # }
    res_npde = tabsim_decorr_same_design_as_data[,list(yd=unique(yd),pde=mean(ydsim<yd)),by=c("id","time")]
    
    res_all = merge(res_all,res_npde,by=c("id","time"))
  }
  
  
  
  
  
  
  jit = function(n,nrep){
    sapply(1:n,function(x){
      if(length(nrep)==n){
        runif(1,0,1/nrep[x])
      }else{
        runif(1,0,1/nrep)
      }
    })
    
  }
  
  # res_npde[,pde_wo_ties:=pde]
  if(compute_npde){
    idx_pde = which(res_all[,pde>0&pde<1])
    if(!ties) res_all[idx_pde,pde:=pde+jit(length(pde),nrep)]
    res_all[pde==0,pde:=pde+jit(1,nrep)]
    res_all[pde>=1,pde:=1-jit(1,nrep)]
    
    res_all[,npde := qnorm(pde)]
  }
  
  
  idx_pd = which(res_all[,pd>0&pd<1])
  if(!ties) res_all[idx_pd,pd:=pd+jit(1,nrep)]
  res_all[pd==0,pd:=pd+jit(1,nrep)]
  res_all[pd>=1,pd:=1-jit(1,nrep)]
  
  res_all[,npd := qnorm(pd)]
  # npde$time=as.num(npde$time)
  # names(npde)[names(npde)=="y.y"] = "dobs"
  
  
  res = merge(tab,res_all,by=c("id","time"))  ## keep the info of the data: covariate etc.
  
  return(res)
  
}


correlation_matrix<-function(sim_i,ID,nrep,decorr.method,verbose=FALSE) {
  # print(names(sim_i))
  # time=unique(obs_i$time)
  # sim_i=tabsim_decorr
  sim_i=sim_i[id==ID]
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
    ymat=data.table(ymat)
    return(ymat)
  }else{
    return(xerr)
  }
  
}

decorr_i = function(ymat,sim_i,ID,nrep,decorr.method){
  # sim_i=tabsim[id==1]
  # sim_i_decorr=tabsim_decorr[id==1]
  # y=sim_i_decorr[rep==1]$y
  sim_i=sim_i[id==ID]
  ymat = as.matrix(ymat)
  matsim=dcast(sim_i,rep~time,drop=T,value.var="y") 
  matsim$rep=NULL
  setkeyv(sim_i,c("rep","time"))
  sim_i[,yd:=ymat[1:length(y),1:length(y)]%*%(y),by=c("rep")]
  
  sim_i$id=NULL
  
  
  return(sim_i)
}

calcnpde<-function(obs_i,sim_i,sim_i_decorr=NULL,nrep,decorr.method,verbose=FALSE) {
  print(names(sim_i))
  # time=unique(obs_i$time)
  if(is.null(sim_i_decorr)) sim_i_decorr=sim_i
  
  ymat = t(correlation_matrix(obs_i,sim_i_decorr,nrep,decorr.method,verbose=FALSE))
  
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




#############################################################
#####      Graphical representation of NPDE  #####
#############################################################

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

compute.bands.eco<-function(nsamp,nseuil=200,quant=c(0.025,0.5,0.975),distrib="norm", alpha=0.95) {
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
  
  res = list(binf=binf,bsup=bsup,bmed=bmed)
  res2 = list(res[[1]][,1],
              res[[2]][,1],
              res[[1]][,2],
              res[[2]][,2],
              res[[1]][,3],
              res[[2]][,3])
  
  return(res2)
}

compute.bands<-function(nsamp,distrib="norm", alpha=0.95) {
  # Compute a prediction interval around selected quantiles of the normal or uniform distribution, for different sizes of samples by randomly sampling from N(0,1) or U(0,1)
  quant.pi<-c((1-alpha)/2,0.5,1-(1-alpha)/2) 
  if(distrib=="unif"){
    
    q.function <- eval(parse(text = paste0("q", distrib)))
    d.function <- eval(parse(text = paste0("d", distrib)))
    n <- nsamp
    # P <- ppoints(n)
    P <- quant.pi
    df <- data.frame(z = q.function(P))
    
    zz <- qnorm(1 - (1 - alpha)/2)
    lower <- sapply(P,function(xx){
      binom.test(round(xx*n),n,xx,conf.level = alpha)$conf[1]
    })
    upper <- sapply(P,function(xx){
      binom.test(round(xx*n),n,xx,conf.level = alpha)$conf[2]
    })
    
    fit.value <- 0 + 1 * df$z
    df$upper <- upper
    df$lower <- lower
    
  }else{
    
    
    q.function <- eval(parse(text = paste0("q", distrib)))
    d.function <- eval(parse(text = paste0("d", distrib)))
    n <- nsamp
    # P <- ppoints(n)
    P <- quant.pi
    df <- data.frame(z = q.function(quant.pi))
    
    zz <- qnorm(1 - (1 - alpha)/2)
    SE <- 1/sqrt(n)
    
    fit.value <- 0 + 1 * df$z
    df$upper <- fit.value + zz * SE
    df$lower <- fit.value - zz * SE
    
    
    
    
  }
  
  
  
  
  binf<-df[,3]
  bsup<-df[,2]
  bmed<-df[,1]
  
  
  
  return(list(binf=binf,bsup=bsup,bmed=bmed))
}

gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  
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

plot_npde_scatter_dt_L = function(tab,variable="time",outcome="npde",bin=F,method="equal",conf = 0.9,method_band="theo"){
  
  alpha = (1-conf)/2
  tab$old_var=tab[[variable[1]]]
  dist=ifelse(outcome%in%c("npd","npde"),"norm","unif")
  if(bin){
    
    bounds = auto_bin(tab[[variable[1]]],plot.opt=list(bin.method=method,
                                                       bin.numer=NULL,
                                                       bin.breaks=NULL,
                                                       bin.extreme=NULL,
                                                       xlog=NULL),verbose=FALSE)$xbound 
    tab[[variable[1]]] = sapply(tab$old_var,function(x) bounds[findClosest(bounds,x)])
  }
  
  
  quant_npde = tab[,list(q5=quantile(get(outcome),alpha),
                         q50=quantile(get(outcome),0.5),
                         q95=quantile(get(outcome),1-alpha)),by=eval(variable)]
  
  huron2 = tab[,list(N=length(unique(id))),keyby=c(variable)]
  huron2[,q5.5:=compute.bands(N,alpha=conf,distrib = dist)[[1]][1],by=c(variable)]
  huron2[,q5.95:=compute.bands(N,alpha=conf,distrib = dist)[[2]][1],by=c(variable)]
  
  huron2[,q50.5:=compute.bands(N,alpha=conf,distrib = dist)[[1]][2],by=c(variable)]
  huron2[,q50.95:=compute.bands(N,alpha=conf,distrib = dist)[[2]][2],by=c(variable)]
  
  huron2[,q95.5:=compute.bands(N,alpha=conf,distrib = dist)[[1]][3],by=c(variable)]
  huron2[,q95.95:=compute.bands(N,alpha=conf,distrib = dist)[[2]][3],by=c(variable)]
  
  if(method_band=="simul"){
    
    
    huron = tab[,list(N=length(unique(id))),keyby=c(variable)]
    # huron[,q5.5:=compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[1]][,1],by=c(variable)]
    # huron[,q5.95:=compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[2]][,1],by=c(variable)]
    # 
    # huron[,q50.5:=compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[1]][,2],by=c(variable)]
    # huron[,q50.95:=compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[2]][,2],by=c(variable)]
    # 
    # huron[,q95.5:=compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[1]][,3],by=c(variable)]
    # huron[,q95.95:=compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[2]][,3],by=c(variable)]
    
    
    # huron[,c("q5.5","q5.95","q50.5","q50.95","q95.5","q95.95") := 
    #         list(compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[1]][,1],
    #              compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[2]][,1],
    #              compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[1]][,2],
    #              compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[2]][,2],
    #              compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[1]][,3],
    #              compute.bands.eco(N,quant=c(alpha,0.5,1-alpha))[[2]][,3]),by=c(variable)]
    
    huron[,c("q5.5","q5.95","q50.5","q50.95","q95.5","q95.95") := compute.bands.eco(N,quant=c(alpha,0.5,1-alpha)),by=c(variable)]
  }else if(method_band=="prop"){
    dist="unif"
    huron = tab[,list(N=length(unique(id))),keyby=c(variable)]
    huron[,q5.5:=qnorm(compute.bands(N,alpha=conf,distrib = dist)[[1]][1]),by=c(variable)]
    huron[,q5.95:=qnorm(compute.bands(N,alpha=conf,distrib = dist)[[2]][1]),by=c(variable)]
    
    huron[,q50.5:=qnorm(compute.bands(N,alpha=conf,distrib = dist)[[1]][2]),by=c(variable)]
    huron[,q50.95:=qnorm(compute.bands(N,alpha=conf,distrib = dist)[[2]][2]),by=c(variable)]
    
    huron[,q95.5:=qnorm(compute.bands(N,alpha=conf,distrib = dist)[[1]][3]),by=c(variable)]
    huron[,q95.95:=qnorm(compute.bands(N,alpha=conf,distrib = dist)[[2]][3]),by=c(variable)]
  } else{
    huron=huron2
  }
  
  # huron=huron2
  # merge(huron,huron2,by=c("time","N"))
  
  
  
  if(length(variable)==1){
    
    
    p4 = ggplot() +theme_bw() + scale_x_continuous(name=variable[1]) + scale_y_continuous(name=outcome)+
      geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q5.5,ymax=q5.95,fill="blue"),alpha=0.2)+
      geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q50.5,ymax=q50.95,fill="red"),alpha=0.2)+
      geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q95.5,ymax=q95.95,fill="blue"),alpha=0.2)+
      geom_line(data=melt(quant_npde,id.vars = variable,variable.name = "cat"),
                aes(x=get(variable[1]),y=value,group=cat,lty=cat))+
      scale_linetype_manual(values = c(2,1,2),name="Quantiles")+
      scale_fill_manual(name="Theoretical percentiles",values=c("blue","red"),labels=c("90% PI of \n5-95th percentiles","90% PI of median"));p4
  }else{
    
    p4 = ggplot() + theme_bw() + scale_x_continuous(name=variable[1]) + scale_y_continuous(name=outcome)+
      geom_line(data=melt(quant_npde,id.vars = variable,variable.name = "cat"),
                aes(x=get(variable[1]),y=value,group=cat,lty=cat)) +
      geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q5.5,ymax=q5.95,fill="blue"),alpha=0.2)+
      geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q50.5,ymax=q50.95,fill="red"),alpha=0.2)+
      geom_ribbon(data=huron,aes(x=get(variable[1]),ymin=q95.5,ymax=q95.95,fill="blue"),alpha=0.2)+
      # facet_wrap(increased+design~.,ncol=5)+
      facet_wrap(reformulate(response = ".",variable[-1]),ncol=5)+
      scale_linetype_manual(name="Quantiles",values=c(2,1,2))+
      scale_fill_manual(name="Theoretical percentiles",values=c("blue","red"),labels=c("90% PI of \n5-95th percentiles","90% PI of median"));p4
  }
  
  
  
  
  
  
  
  
  
  
  return(p4)
}

plot_npde_scatter_dt_TTE = function(tab2,variable="OFT",outcome="npde",adj=T,method="overall",conf=0.9,covariate="cat",p=ggplot()){
  tab<-tab2
  dist=ifelse(outcome%in%c("npd","npde"),"norm","unif")
  tab[,y:=factor(y)]
  
  if(length(variable)==1){
    
    temp_th = gg_qq(tab[[outcome]],conf=conf,distribution = dist)
    th = temp_th$tab[temp_th$ord,]
    names(th)[1]=outcome
    th_fin = merge(tab,th,by=outcome)
    th_fin[,outlier:=ifelse(get(outcome)>=lower&get(outcome)<=upper,"0","1")]
    
    if(covariate!="cat"){
      adj=F
    }
    if(adj){
      p4 = p + 
        geom_point(data=th_fin,aes(x=get(variable[1]),y=get(outcome)-z,col=outlier,shape=y)) +theme_bw(base_size = 18) + 
        geom_line(data=th_fin,aes(x=get(variable[1]),y=z-z),lty=2) +
        geom_ribbon(data=th_fin,aes(x=get(variable[1]),ymin=lower-z,ymax=upper-z,fill="blue"),alpha=0.4)+
        scale_shape_manual(name="status",values=c(3,16),labels=c("censored","observed"))+
        scale_fill_manual(name="",values=c("black"),labels=c("90% PI"))+
        scale_color_manual(values=c(1,2))+
        # guides(fill=F)+
        scale_y_continuous(name=paste0("Detrended ",outcome))+
        scale_x_continuous(name="Time (days)",breaks=round(seq(0,max(th_fin[[variable[1]]],na.rm = T),length.out=10)))+
        theme(legend.position="none",axis.text.x = element_text(angle=45))+
        geom_hline(yintercept = 0,col="black",lwd=1);p4
    }else{
      if(covariate!="cat"){
        p4 = p + geom_point(data=th_fin,aes(x=get(variable[1]),y=get(outcome),col=outlier,shape=y)) +theme_bw(base_size = 18) +
          geom_hline(yintercept=0,lwd=2)+
          geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)),method = "lm")+
          # guides(fill=F)+
          scale_y_continuous(name=paste0(outcome))+
          scale_x_continuous(name=variable[1],breaks=round(seq(0,max(th_fin[[variable[1]]],na.rm=T),length.out=10)))+
          theme(legend.position="none",axis.text.x = element_text(angle=45));p4
      }else{
        p4 = p + geom_point(data=th_fin,aes(x=get(variable[1]),y=get(outcome),col=outlier,shape=y)) +theme_bw(base_size = 18) + scale_x_continuous(name=variable[1]) +
          geom_line(data=th_fin,aes(x=get(variable[1]),y=z),lty=2) +
          geom_ribbon(data=th_fin,aes(x=get(variable[1]),ymin=lower,ymax=upper,fill="blue"),alpha=0.4)+
          scale_fill_manual(name="",values=c("black"),labels=c("90% PI"))+
          # guides(fill=F)+
          scale_color_manual(name="outliers",values=c(1,2))+
          scale_y_continuous(name=paste0(outcome))+
          scale_x_continuous(name="Time (days)",breaks=round(seq(0,max(th_fin[[variable[1]]]),length.out=10)))+
          theme(legend.position="none",axis.text.x = element_text(angle=45));p4
      }
      
    }
    
  }else{
    if(method=="overall"){
      tab[,c("ord.x","z","upper","lower"):=gg_qq(npde,conf=conf,distribution = dist)$tab,by=eval(variable[1])]
    }else{
      tab[,c("ord.x","z","upper","lower"):=gg_qq(npde,conf=conf,distribution = dist)$tab,by=eval(variable[2:length(variable)])]
    }
    
    
    
    th_fin=tab
    th_fin[,outlier:=ifelse(ord.x>=lower&ord.x<=upper,"0","1")]
    if(covariate[1]!="cat"){
      adj=F
    }
    if(adj){
      p4 = p + geom_point(data=th_fin,aes(x=get(variable[1]),y=get(outcome)-z,col=outlier,shape=y)) +theme_bw() + scale_x_continuous(name=variable[1]) +
        # geom_smooth(data=th_fin,data=th_fin,aes(x=get(variable[1]),y=get(outcome)-z),se = F,col=2)+
        geom_line(data=th_fin,aes(x=get(variable[1]),y=z-z)) +
        geom_ribbon(data=th_fin,aes(x=get(variable[1]),ymin=lower-z,ymax=upper-z,fill="blue"),alpha=0.2) +
        facet_wrap(reformulate(response=".",termlabels=variable[2:length(variable)]),ncol=5)+
        scale_fill_manual(name="",values=c("black"),labels=c("90% PI"))+
        # guides(fill=F)+
        scale_color_manual(name="outliers",values=c(1,2))+
        scale_y_continuous(name=paste0("Detrended ",outcome))+
        scale_x_continuous(name="Time (days)",breaks=round(seq(0,max(th_fin[[variable[1]]]),length.out=10)))+
        theme(legend.position="none",axis.text.x = element_text(angle=45))+
        geom_hline(yintercept = 0,col="black",lwd=1)
    }else{
      if(covariate[1]!="cat"){
        p4 = p + geom_point(data=th_fin,aes(x=get(variable[1]),y=get(outcome),col=outlier,shape=y)) +theme_bw(base_size = 18) +
          geom_hline(yintercept=0,lwd=2)+
          geom_smooth(data=th_fin,aes(x=get(variable[1]),y=get(outcome)),method = "lm")+
          facet_wrap(reformulate(response =".",variable[2:length(variable)]),ncol=6)+
          scale_y_continuous(name=paste0(outcome))+
          scale_x_continuous(name=variable[1],breaks=round(seq(0,max(th_fin[[variable[1]]],na.rm=T),length.out=10)))+
          theme(legend.position="none",axis.text.x = element_text(angle=45));p4
      }else{
        p4 = p + geom_point(data=th_fin,aes(x=get(variable[1]),y=get(outcome),col=outlier,shape=y)) +theme_bw() + scale_x_continuous(name=variable[1]) +
          # geom_smooth(data=th_fin,data=th_fin,aes(x=get(variable[1]),y=get(outcome)),se = F,col=2)+
          geom_line(data=th_fin,aes(x=get(variable[1]),y=z)) +
          geom_ribbon(data=th_fin,aes(x=get(variable[1]),ymin=lower,ymax=upper,fill="blue"),alpha=0.2) +
          facet_wrap(reformulate(response =".",variable[2:length(variable)]),ncol=6)+
          scale_fill_manual(name="",values=c("black"),labels=c("90% PI"))+
          # guides(fill=F)+
          scale_color_manual(name="outliers",values=c(1,2))+
          scale_y_continuous(name=paste0(outcome))+
          scale_x_continuous(name="Time (days)",breaks=round(seq(0,max(th_fin[[variable[1]]]),length.out=10)))+
          theme(legend.position="none",axis.text.x = element_text(angle=45))
      }
      
      
    }
    
  }
  return(p4)
}

#############################################################
#####      VPC      #####
#############################################################

### TTE
prediction_interval = function(dd1,time_interval=seq(0,735,5),PI=0.95){
  #dd1=km_list[[1]]
  # time_interval = seq(0,735,5)
  
  borne_inf = (1-PI)/2
  borne_sup = 1-borne_inf
  
  K = length(unique(dd1$rep))
  n_time = length(time_interval)         
  
  indice_row=1
  
  dd1$time2 = ceiling(dd1$time/max(time_interval)*(n_time-1))/(n_time-1)  * max(time_interval)
  
  # dd3 = dd1[,c(4,2,3)]
  dd3 = dd1[,c(4,3,1),with=F]
  names(dd3) = c("time","SURV","rep")
  dd4 = dd3[,list(surv.5 = quantile(SURV,borne_inf),
                  surv.50 = quantile(SURV,0.5),
                  surv.95 = quantile(SURV,borne_sup)),keyby="time"]
  # dd4 = ddply(dd3,c("TIME"),summarize,surv.5 = quantile(SURV,0.025),surv.50 = quantile(SURV,0.5),surv.95 = quantile(SURV,0.975)  )
  
  return(dd4)  
  
}

VPC_TTE = function(obs,sim,censoring_type,p=ggplot(),verbose=F){
  
  if(censoring_type=="right"){
    sm_km = (survfit(Surv(obs$time,as.num(obs$y))~1))
    dat_km = data.table(time=sm_km$time,y=1-sm_km$surv)
  }else{
    sm_km = (survfit(Surv(obs$time,obs$time2,type = "interval2")~1))
    dat_km = data.table(time=sm_km$time,y=1-sm_km$surv)
  }
  
  time_interval=dat_km$time
  
  p = ggplot()+
    geom_step(data=dat_km,aes(x=time,y=y))
  
  km = function(TTE,status){
    res = (summary(survfit(Surv(TTE,status)~1)))
    time = c(0,res$time)
    surv= c(1,res$surv)
    list(time,surv)
  }
  
  setkey(sim,"rep")
  
  km_list = sim[,{res=km(time,y);list(time=res[[1]],surv=res[[2]])},by="rep"]  
  
  
  a=prediction_interval(km_list,time_interval = time_interval)
  names(a) = c("time","lower","med","upper")
  
  p2 = p + geom_step(data=a,aes(x=time,y=1-med),lty=2,lwd=1) +
    geom_ribbon(data=a,aes(x=time,ymin=1-lower,ymax=1-upper),alpha=0.4,
                inherit.aes=FALSE,stat="stepribbon")+
    coord_cartesian(xlim=c(0,735));p2
  
  
  return(list(p2,a)) 
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

VPC_cat = function(obs,sim,p=ggplot()){
  
}

#############################################################
#####      Homemade function    #####
#############################################################
findClosest <- function(vec, val){
  tmp <- abs(val-vec)
  idx=which(tmp==min(tmp))[1]
  # [~,idx] <- min(tmp)
  return(idx)
}

factor2 = function(vec,label){
  vec = factor(vec)
  levels(vec)=label
  vec
}

as.num = function(x){
  return(as.numeric(as.character(x)))
}

`%!in%` = Negate(`%in%`)

unique2 = function(df,col=NULL){
  df = data.table(df)
  if(is.null(col)){
    setkeyv(df,names(df))
    df=unique(df)
  }else{
    setkeyv(df,col)
    df=unique(df)
  }
  return(df)
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

