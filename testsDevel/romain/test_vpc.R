
##########################################################################################################

# Tests Options Graph ggplot plot.type : pred.scatter

##########################################################################################################

###  --------------------------------------------------------------------------------------------------------

### only plot.type

###  --------------------------------------------------------------------------------------------------------

plot(xtheo_nocens,plot.type="vpc")
plot(xvir.full1,plot.type="vpc")
plot(xtheo,plot.type="vpc")
plot(xbase1,plot.type="vpc")
plot(x50.omit,plot.type="vpc")
plot(xtheo_cens_pd_nan,plot.type="vpc")
plot(xtheo_cens,plot.type="vpc")
plot(xrem ,plot.type="vpc")
plot(xcens2,plot.type="vpc")

# --------------------------------------------------------------------------------------------------------
# data : xtheo_nocens
# --------------------------------------------------------------------------------------------------------

plot(xtheo_nocens, plot.type="vpc",
     main = "Visual Predictive Check",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= paste0(xtheo_cens_pd_nan@data@name.predictor," ", "(",xtheo_cens_pd_nan@data@units$x,")"),
     ylab= paste0(xtheo_cens_pd_nan@data@name.response," ", "(",xtheo_cens_pd_nan@data@units$y,")"),

     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=FALSE,
     plot.obs=TRUE,
     impute.loq=FALSE,

     alpha.med = 0.25,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
     lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

    col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE,

    pi.size=0.9,
    vpc.interval=0.95)


# --------------------------------------------------------------------------------------------------------
# data : xtheo_cens_pd_nan
# --------------------------------------------------------------------------------------------------------

plot(xtheo_cens_pd_nan, plot.type="vpc",
     main = "Visual Predictive Check vpc",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= paste0(xtheo_cens_pd_nan@data@name.predictor," ", "(",xtheo_cens_pd_nan@data@units$x,")"),
     ylab= paste0(xtheo_cens_pd_nan@data@name.response," ", "(",xtheo_cens_pd_nan@data@units$y,")"),

     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=FALSE,
     plot.obs=TRUE,
     impute.loq=FALSE,

     alpha.med = 0.25,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
    lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

    col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# --------------------------------------------------------------------------------------------------------
# data : xtheo_cens
# --------------------------------------------------------------------------------------------------------

# plot vpc, npde, censored data, impute.loq=FALSE
# plot(xtheo_cens,plot.type="vpc",impute.loq=FALSE)
plot(xtheo_cens,plot.type="vpc",
     main = "xtheo_cens vpc",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=FALSE,
     bands=TRUE,
     grid=TRUE,
     plot.obs=TRUE,
     impute.loq=FALSE,

     col.line.loq = "orange",
     lty.line.loq  = 4,
     lwd.line.loq  = 1,

     alpha.med = 0.5,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
    lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

     col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# --------------------------------------------------------------------------------------------------------
# Plot : user guide npde
# --------------------------------------------------------------------------------------------------------

plot.NpdeObject(xtheo,plot.type="vpc",
                main = "xtheo_cens vpc",
                size.main = 14,
                sub = "",
                size.sub = "",

                xlab= "Time",
                ylab= "npde",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                approx.pi=FALSE,
                bands=TRUE,
                plot.obs=TRUE,
                impute.loq=FALSE,

                col.line.loq = "orange",
                lty.line.loq  = 4,
                lwd.line.loq  = 1,

                alpha.med = 0.25,
                fill.med = "firebrick4",
                col.med="red",
               lty.med=3,
               lwd.med=1,

                alpha.bands = 0.25,
                fill.bands = "dodgerblue",
                col.bands="green",
               lty.bands=6,
                lwd.bands=1,

                col.med = "red",
                lty.med = 1,
                lwd.med = 1,

                col.bands = "blue",
                lty.bands = 1,
                lwd.bands = 1,

                col.pobs = "orangered3",
                pch.pobs = 12,
                size.pobs = 1.5,

               col.pcens = "yellow",
                pch.pcens = 15,
                size.pcens = 1.75,

                size.text.x = 10,
                size.text.y = 10,

                breaks.x = 10,
                breaks.y = 10,

                xlog = FALSE,
                ylog = FALSE)

# vpc, binning
plot(xtheo,plot.type="vpc",new=FALSE,main='Default binning, 15 bins',bin.number=15,
     main = "xtheo_cens vpc",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=FALSE,
     bands=TRUE,
     plot.obs=TRUE,
     impute.loq=FALSE,
     line.loq = TRUE,
     plot.loq = TRUE,
     col.line.loq = "orange",
     lty.line.loq  = 4,
     lwd.line.loq  = 1,

     alpha.med = 0.25,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
    lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

    col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# vpc, binning
plot(xtheo,plot.type="vpc",new=FALSE,main='Method=width, forcing boundaries',
     bin.method="width",bin.extreme=c(0.01,0.95),
     main = "xtheo_cens vpc",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE,
     plot.obs=TRUE,
     impute.loq=FALSE,

     col.line.loq = "orange",
     lty.line.loq  = 4,
     lwd.line.loq  = 1,

     alpha.med = 0.25,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
    lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

    col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# nb : interactive : removed
plot(xtheo,plot.type="vpc",new=FALSE,main='Method=optimal',vpc.method="optimal", interactive=TRUE, bin.number=10,
     main = "xtheo vpc Method=optimal",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE,
     plot.obs=TRUE,
     impute.loq=FALSE,

     col.line.loq = "orange",
     lty.line.loq  = 4,
     lwd.line.loq  = 1,

     alpha.med = 0.25,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
    lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

    col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

#plot.NpdeObject(xtheo,plot.type="vpc",bin.method = "equal",impute.loq=FALSE)
plot.NpdeObject(xtheo,plot.type="vpc",bin.method = "equal",
                main = "xtheo vpc bin.method = equal",
                size.main = 14,
                sub = "",
                size.sub = "",

                xlab= "Time",
                ylab= "npde",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                bands=TRUE,
                plot.obs=TRUE,
                impute.loq=FALSE,

                col.line.loq = "orange",
                lty.line.loq  = 4,
                lwd.line.loq  = 1,

                alpha.med = 0.25,
                fill.med = "firebrick4",
                col.med="red",
               lty.med=3,
               lwd.med=1,

                alpha.bands = 0.25,
                fill.bands = "dodgerblue",
                col.bands="green",
               lty.bands=6,
                lwd.bands=1,

                col.med = "red",
                lty.med = 1,
                lwd.med = 1,

                col.bands = "blue",
                lty.bands = 1,
                lwd.bands = 1,

                col.pobs = "orangered3",
                pch.pobs = 12,
                size.pobs = 1.5,

               col.pcens = "yellow",
                pch.pcens = 15,
                size.pcens = 1.75,

                size.text.x = 10,
                size.text.y = 10,

                breaks.x = 10,
                breaks.y = 10,

                xlog = FALSE,
                ylog = FALSE)

plot.NpdeObject(xtheo,plot.type="vpc",bin.method = "width",
main = "xtheo vpc bin.method = width",
size.main = 14,
sub = "",
size.sub = "",

xlab= "Time",
ylab= "npde",
size.xlab = 12, ## cex.axis = 12
size.ylab = 12,
#xlim=c(0,25),
#ylim=c(-3,3.5),
xlim=c(),
ylim=c(),

bands=TRUE,
plot.obs=TRUE,
impute.loq=FALSE,

col.line.loq = "orange",
lty.line.loq  = 4,
lwd.line.loq  = 1,

alpha.med = 0.25,
fill.med = "firebrick4",
col.med="red",
lty.med=3,
lwd.med=1,

alpha.bands = 0.25,
fill.bands = "dodgerblue",
col.bands="green",
lty.bands=6,
lwd.bands=1,

col.med = "red",
lty.med = 1,
lwd.med = 1,

col.bands = "blue",
lty.bands = 1,
lwd.bands = 1,

col.pobs = "orangered3",
pch.pobs = 12,
size.pobs = 1.5,

col.pcens = "yellow",
pch.pcens = 15,
size.pcens = 1.75,

size.text.x = 10,
size.text.y = 10,

breaks.x = 10,
breaks.y = 10,

xlog = FALSE,
ylog = FALSE)


#plot.NpdeObject(xtheo,plot.type="vpc",bin.method = "optimal",bin.number=15,impute.loq=FALSE)
plot.NpdeObject(xtheo,plot.type="vpc",bin.method = "optimal",bin.number=15,
main = "xtheo vpc bin.method = optimal",
size.main = 14,
sub = "",
size.sub = "",

xlab= "Time",
ylab= "npde",
size.xlab = 12, ## cex.axis = 12
size.ylab = 12,
#xlim=c(0,25),
#ylim=c(-3,3.5),
xlim=c(),
ylim=c(),

bands=TRUE,
plot.obs=TRUE,
impute.loq=FALSE,

col.line.loq = "orange",
lty.line.loq  = 4,
lwd.line.loq  = 1,

alpha.med = 0.25,
fill.med = "firebrick4",
col.med="red",
lty.med=3,
lwd.med=1,

alpha.bands = 0.25,
fill.bands = "dodgerblue",
col.bands="green",
lty.bands=6,
lwd.bands=1,

col.med = "red",
lty.med = 1,
lwd.med = 1,

col.bands = "blue",
lty.bands = 1,
lwd.bands = 1,

col.pobs = "orangered3",
pch.pobs = 12,
size.pobs = 1.5,

col.pcens = "yellow",
pch.pcens = 15,
size.pcens = 1.75,

size.text.x = 10,
size.text.y = 10,

breaks.x = 10,
breaks.y = 10,

xlog = FALSE,
ylog = FALSE)

#plot.NpdeObject(xtheo,plot.type="vpc",bin.method = "width",bin.breaks=c(0,0.4,0.8,1.5,3,6,8,10,20),impute.loq=FALSE)
plot.NpdeObject(xtheo,plot.type="vpc",bin.method = "width",bin.breaks=c(0,0.4,0.8,1.5,3,6,8,10,20),
                main = "xtheo vpc bin.method = width",
                size.main = 14,
                sub = "",
                size.sub = "",

                xlab= "Time",
                ylab= "npde",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                bands=TRUE,
                plot.obs=TRUE,
                impute.loq=FALSE,

                col.line.loq = "orange",
                lty.line.loq  = 4,
                lwd.line.loq  = 1,

                alpha.med = 0.25,
                fill.med = "firebrick4",
                col.med="red",
               lty.med=3,
               lwd.med=1,

                alpha.bands = 0.25,
                fill.bands = "dodgerblue",
                col.bands="green",
               lty.bands=6,
                lwd.bands=1,

                col.med = "red",
                lty.med = 1,
                lwd.med = 1,

                col.bands = "blue",
                lty.bands = 1,
                lwd.bands = 1,

                col.pobs = "orangered3",
                pch.pobs = 12,
                size.pobs = 1.5,

               col.pcens = "yellow",
                pch.pcens = 15,
                size.pcens = 1.75,

                size.text.x = 10,
                size.text.y = 10,

                breaks.x = 10,
                breaks.y = 10,

                xlog = FALSE,
                ylog = FALSE)

# vpc, xy logscales
# plot.NpdeObject(xtheo,plot.type="vpc",xlog=FALSE, ylog=FALSE,impute.loq=TRUE)
plot.NpdeObject(xtheo,plot.type="vpc",
main = "xtheo vpc",
size.main = 14,
sub = "",
size.sub = "",

xlab= "Time",
ylab= "npde",
size.xlab = 12, ## cex.axis = 12
size.ylab = 12,
#xlim=c(0,25),
#ylim=c(-3,3.5),
xlim=c(),
ylim=c(),

bands=TRUE,
plot.obs=TRUE,
impute.loq=FALSE,

col.line.loq = "orange",
lty.line.loq  = 4,
lwd.line.loq  = 1,

alpha.med = 0.25,
fill.med = "firebrick4", ## = col.fillmed
col.med="red",
lty.med=3,
lwd.med=1,

alpha.bands = 0.25,
fill.bands = "dodgerblue", ## = col.fillpi
col.bands="green",
lty.bands=6,
lwd.bands=1,

col.med = "red",
lty.med = 1,
lwd.med = 1,

col.bands = "blue",
lty.bands = 1,
lwd.bands = 1,

col.pobs = "orangered3",
pch.pobs = 12,
size.pobs = 1.5,

col.pcens = "yellow",
pch.pcens = 15,
size.pcens = 1.75,

size.text.x = 10,
size.text.y = 10,

breaks.x = 10,
breaks.y = 10,

xlog = FALSE,
ylog = TRUE)


# vpc, impute.loq
# plot(x50,plot.type="vpc",impute.loq=TRUE)
plot(x50,plot.type="vpc",
     main = "x50 vpc",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE,
     plot.obs=TRUE,
     impute.loq=FALSE, # TRUE, FALSE

     col.line.loq = "orange",
     lty.line.loq  = 4,
     lwd.line.loq  = 1,

     alpha.med = 0.25,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
    lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

    col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

  #------------------------------------------------------------------------------------------
  # FROM : testsDemo.R
  #------------------------------------------------------------------------------------------

  # --------------------------------------------------------
  # Demo, default plots without censoring
  # --------------------------------------------------------

# vpc, impute.loq
# plot(x50,plot.type="vpc",impute.loq=TRUE)
plot(x50,plot.type="vpc",
     main = "x50 vpc",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE,
     plot.obs=TRUE,
     impute.loq=FALSE, # TRUE, FALSE

     col.line.loq = "orange",
     lty.line.loq  = 4,
     lwd.line.loq  = 1,

     alpha.med = 0.25,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
    lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

    col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)



  plot(xbase1,plot.type="vpc",main="No BQL, VPC",
       main = "xbase1 vpc No BQL, VPC",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= "Time",
       ylab= "npde",
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),

       bands=TRUE,
       plot.obs=TRUE,
       impute.loq=FALSE, # TRUE, FALSE

       col.line.loq = "orange",
       lty.line.loq  = 4,
       lwd.line.loq  = 1,

       alpha.med = 0.25,
       fill.med = "firebrick4",
       col.med="red",
      lty.med=3,
      lwd.med=1,

       alpha.bands = 0.25,
       fill.bands = "dodgerblue",
       col.bands="green",
      lty.bands=6,
       lwd.bands=1,

       col.med = "red",
       lty.med = 1,
       lwd.med = 1,

       col.bands = "blue",
       lty.bands = 1,
       lwd.bands = 1,

       col.pobs = "orangered3",
       pch.pobs = 12,
       size.pobs = 1.5,

      col.pcens = "yellow",
       pch.pcens = 15,
       size.pcens = 1.75,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)

  # --------------------------------------------------------
  # Demo, default plots with censoring
  # --------------------------------------------------------

  #cat("Showing default plots for npde, with censoring (method=cdf)\n")
  #cat("Showing default plots for npde, with censoring (method=cdf), splitting by covariates\n")

  plot(xcens2,plot.type="vpc",main="Method=cdf, VPC")

  plot(xcens2,plot.type="vpc",main="Method=cdf, VPC",
  main = "xcens2 Method=cdf VPC",
  size.main = 14,
  sub = "",
  size.sub = "",

  xlab= "Time",
  ylab= "npde",
  size.xlab = 12, ## cex.axis = 12
  size.ylab = 12,
  #xlim=c(0,25),
  #ylim=c(-3,3.5),
  xlim=c(),
  ylim=c(),

  bands=TRUE,
  plot.obs=TRUE,
  impute.loq=FALSE, # TRUE, FALSE

  col.line.loq = "orange",
  lty.line.loq  = 4,
  lwd.line.loq  = 1,

  alpha.med = 0.25,
  fill.med = "firebrick4",
  col.med="red",
 lty.med=3,
 lwd.med=1,

  alpha.bands = 0.25,
  fill.bands = "dodgerblue",
  col.bands="green",
 lty.bands=6,
  lwd.bands=1,

  col.med = "red",
  lty.med = 1,
  lwd.med = 1,

  col.bands = "blue",
  lty.bands = 1,
  lwd.bands = 1,

  col.pobs = "orangered3",
  pch.pobs = 12,
  size.pobs = 1.5,

 col.pcens = "yellow",
  pch.pcens = 15,
  size.pcens = 1.75,

  size.text.x = 10,
  size.text.y = 10,

  breaks.x = 10,
  breaks.y = 10,

  xlog = FALSE,
  ylog = FALSE)


plot(x50.omit,plot.type="vpc",impute.loq = TRUE)


plot(x50.omit,plot.type="vpc",
     main = "x50.omit VPC",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE,
     plot.obs=TRUE,
     impute.loq=FALSE, # TRUE, FALSE

     col.line.loq = "orange",
     lty.line.loq  = 4,
     lwd.line.loq  = 1,

     alpha.med = 0.25,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
    lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

    col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)



plot(x50,plot.type="vpc",impute.loq = TRUE)


plot(x50,plot.type="vpc",
     main = "x50 VPC",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE,
     plot.obs=TRUE,
     impute.loq=TRUE, # TRUE, FALSE

     col.line.loq = "orange",
     lty.line.loq  = 4,
     lwd.line.loq  = 1,

     alpha.med = 0.25,
     fill.med = "firebrick4",
     col.med="red",
    lty.med=3,
    lwd.med=1,

     alpha.bands = 0.25,
     fill.bands = "dodgerblue",
     col.bands="green",
    lty.bands=6,
     lwd.bands=1,

     col.med = "red",
     lty.med = 1,
     lwd.med = 1,

     col.bands = "blue",
     lty.bands = 1,
     lwd.bands = 1,

     col.pobs = "orangered3",
     pch.pobs = 12,
     size.pobs = 1.5,

    col.pcens = "yellow",
     pch.pcens = 15,
     size.pcens = 1.75,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)


