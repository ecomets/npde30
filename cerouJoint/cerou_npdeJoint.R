## Loading libraries
library(ggplot2)
library(data.table)
library(gridExtra)
library(ggpubr)
library(cowplot) 

# Setting the working directories (by default, current working directory)
workDir<-"/home/eco/work/npde/jointPaper/zenCMPB"
# workDir <- getwd()
figDir<-workDir
setwd(workDir)

## Set to TRUE to save figures to figDir folder
saveFigs <- TRUE

## Loading the functions to compute npde for joint models
source("funs.R")
source("funs2.R")

##############################################
REP=1

### Observed data
load("obs.rda")

### Simulated data under H0
load("sim_H0.rda")
sim_H0 = sim

### Simulated data under H1 with misspecification on k
load("sim_H1k.rda")
sim_H1_k = sim

### Preparing dataframes by splitting each dataset into longitudinal and event data

data_L = obs$psa_with_dropout[rep==REP][,c("rep","id","time","y"),with=F]
data_TTE = obs$TTE[rep==REP]
names(data_TTE)[c(1:2,5,4)] = c("rep","id","time","y")

sim_H0_L = sim_H0$psa_wo_dropout[,c("rep","id","time","y"),with=F]
sim_H0_L = merge(sim_H0_L,data_L[,c("id","time"),with=T],by=c("id","time"))
setkeyv(sim_H0_L,c("rep","id","time"))
sim_H0_TTE = sim_H0$TTE
names(sim_H0_TTE)[1:4] = c("rep","id","time","y")

sim_H1_k_L = sim_H1_k$psa_wo_dropout[,c("rep","id","time","y"),with=F]
sim_H1_k_L = merge(sim_H1_k_L,data_L[,c("id","time"),with=T],by=c("id","time"))
setkeyv(sim_H1_k_L,c("rep","id","time"))
sim_H1_k_TTE = sim_H1_k$TTE
names(sim_H1_k_TTE)[1:4] = c("rep","id","time","y")

data_L[,id:=as.num(id)];data_L[,time:=as.num(time)];data_L[,rep:=as.num(rep)]
data_TTE[,id:=as.num(id)];data_TTE[,time:=as.num(time)];data_TTE[,rep:=as.num(rep)]

sim_H0_L[,id:=as.num(id)];sim_H0_L[,time:=as.num(time)];sim_H0_L[,rep:=as.num(rep)]
sim_H0_TTE[,id:=as.num(id)];sim_H0_TTE[,time:=as.num(time)];sim_H0_TTE[,rep:=as.num(rep)]

sim_H1_k_L[,id:=as.num(id)];sim_H1_k_L[,time:=as.num(time)];sim_H1_k_L[,rep:=as.num(rep)]
sim_H1_k_TTE[,id:=as.num(id)];sim_H1_k_TTE[,time:=as.num(time)];sim_H1_k_TTE[,rep:=as.num(rep)]

##############################################
# Running the new computations simultaneously for the 4 sets of data

npde = main_compute_npde(obs = list(data_L,data_TTE,data_L,data_TTE), 
                         sim = list(sim_H0_L,sim_H0_TTE, sim_H1_k_L,sim_H1_k_TTE),
                         type_outcome = rep(c("continuous","TTE"),3),dependancies = list(2,0,4,0),verbose = T,
                         options=NULL)

##############################################
# Creating the plots with the appropriate type (either continuous or TTE)

list_p = main_compute_plot(npde,type_outcome = rep(c("continuous","TTE"),2),
                           options=list(list(outcome="npd",variable="time"),
                                        list(outcome="pd_wo_ties",variable="time"),
                                        list(outcome="npd",variable="time"),
                                        list(outcome="pd_wo_ties",variable="time")))
for(p in 1:length(list_p)) assign(paste0("p",p),list_p[[p]])

axis_x = scale_x_continuous(name="Time (day)",breaks = c(seq(0,600,150),735))
axis_y_cont = function(p) p+scale_y_continuous(name="npd")+coord_cartesian(ylim=c(-2,2.2))
axis_y_TTE = function(p) p+scale_y_continuous(name="De-trended pd")+coord_cartesian(ylim=c(-0.07,0.15))

p7 = get_legend(p1+theme(legend.position = "right"));p7
p8 = get_legend(p2+theme(legend.position = "right"));p8
thm = theme(legend.position = "none")
thm_r = theme(legend.position = "right")
pp1 = plot_grid(axis_y_cont(p1+axis_x+ggtitle(label = expression(H[0]))+thm),axis_y_TTE(p2+axis_x+thm),ncol=1,align="hv")
pp2 = plot_grid(axis_y_cont(p3+axis_x+thm+ggtitle(label = expression(H[1]:~misspecified~k))),axis_y_TTE(p4+axis_x+thm),ncol=1,align="hv")
pp4 = plot_grid(p7,p8,ncol=1,align="hv")

# Arranging the graphs on a single page
p_final = grid.arrange(pp1,pp2,pp4,nrow=1)

# Saving to a file (if saveFile is TRUE)
if(saveFigs) {
  ggsave(p_final,filename = file.path(figDir,paste0("example_npd_L_TTE.pdf")),width = 10,height=6)
  ggsave(p_final,filename = file.path(figDir,paste0("example_npd_L_TTE.tiff")),width = 10,height=6)
}


