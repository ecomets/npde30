
##########################################################################################################

# Tests Options Graph ggplot plot.type : pred.scatter

##########################################################################################################

###  --------------------------------------------------------------------------------------------------------

### only plot.type

###  --------------------------------------------------------------------------------------------------------

plot(xtheo_nocens,plot.type="x.scatter")
plot(xvir.full1,plot.type="x.scatter")
plot(xtheo,plot.type="x.scatter")
plot(xbase1,plot.type="x.scatter")
plot(x50.omit,plot.type="x.scatter")
plot(xtheo_cens_pd_nan,plot.type="x.scatter")
plot(xtheo_cens,plot.type="x.scatter")
plot(xrem ,plot.type="x.scatter")
plot(xcens2,plot.type="x.scatter")


# with cov.x.scatter
plot(xtheo_cens,plot.type="cov.scatter", which.cov=c("Wt"))
plot(xrem,plot.type="cov.scatter", which.cov=c("LBM"))

plot(xtheo_cens,plot.type="x.scatter", covsplit=TRUE, which.cov=c("Wt","Sex"),

     main = "Wt data xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,
     grid=FALSE,
     plot.box=FALSE, #FALSE, ## changer en options box.plot

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





plot(xtheo_cens,plot.type="x.scatter", covsplit=TRUE, which.cov="Wt",

     main = "Wt data xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,
     grid=FALSE,
     plot.box=FALSE, #FALSE, ## changer en options box.plot

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

print( "plot xscatter 1 : ")

plot(xtheo_cens,plot.type="x.scatter", covsplit=TRUE, which.cov="Wt",

     main = "Wt data xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,
     grid=TRUE,
     plot.box=FALSE,

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

print( "plot xscatter 2 : ")

# --------------------------------------------------------------------------------------------------------
#  data : xtheo_nocens
# --------------------------------------------------------------------------------------------------------

plot(xtheo_nocens, plot.type="x.scatter",which="npde")
print( "plot xscatter 3 : ")

plot(xtheo_nocens, plot.type="x.scatter",which="npde",

     main = "Scatterplot of npde versus independent variable",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= paste0(xtheo_nocens@data@name.predictor," ", "(",xtheo_nocens@data@units$x,")"),
     ylab= paste0("npde"),
     size.xlab = 10, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,
     grid=TRUE,

     fill.outliers.med = "red",
     fill.outliers.bands = "blue",
     alpha.outliers.med = 2,
     alpha.outliers.bands = 2,


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

print( "plot xscatter 4 : ")
##ggsave("x.scatter.pdf", height=9, width=18, units='cm', dpi=600,device=cairo_pdf)

# plot x.scatter, pd, no censored data

plot(xtheo_nocens, plot.type="x.scatter",which="pd")
print( "plot xscatter 5 : ")

plot(xtheo_nocens, plot.type="x.scatter",which="pd",pi.size=0.95)
print( "plot xscatter 6 : ")

plot(xtheo_cens, plot.type="x.scatter",

     main = "npde vs time data xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Time",
     ylab= "pd",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

     breaks.x = 1,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

print( "plot xscatter 7 : ")

# --------------------------------------------------------------------------------------------------------
# data : xtheo_cens_pd_nan
# --------------------------------------------------------------------------------------------------------

# plot x.scatter, npde, no censored data

plot(xtheo_cens_pd_nan, plot.type="x.scatter",which="npde",
     main = "npde vs time data xtheo_cens",
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

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 8 : ")
# --------------------------------------------------------------------------------------------------------
# data : xtheo_cens
# --------------------------------------------------------------------------------------------------------

# plot x.scatter, npde, no censored data, quantitative covariables

plot(xtheo_cens,plot.type="x.scatter", covsplit=TRUE, which.cov="Wt",which="npde",

     main = "Wt data xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 9 : ")

# plot x.scatter, npde, no censored data, qualitative covariables

plot(xtheo_cens,plot.type="x.scatter", covsplit=TRUE, which.cov="Sex",which="npde",
     main = "Sex M/F data xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     xlim=c(0,25),
     ylim=c(-3,3.5),
     #xlim=c(),
     #ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 10 : ")
# plot x.scatter, npde, no censored data

plot(xtheo_nocens, plot.type="x.scatter",which="npde",
     main = "Sex M/F data xtheo_nocens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 11 : ")
# plot x.scatter : by ID

plot(xtheo_cens,plot.type="x.scatter",xscale=T,ref.prof=list(ID=1),
     main = "Sex M/F data xtheo_nocens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 12 : ")

plot(xtheo_cens,plot.type="x.scatter",xscale=T,ref.prof=list(ID=8),
     main = "xtheo_cens ID=8",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 13 : ")

plot(xtheo_cens,plot.type="x.scatter",xscale=T,ref.prof=list(Sex="M"),new=F,main="Reference profile=male",
     main = "Sex M data xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 14 : ")

plot(xtheo_cens,plot.type="x.scatter",xscale=T,ref.prof=list(Sex="F"),new=F,main="Reference profile=female",
     main = "Sex M data xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 15 : ")
# plot x.scatter with ref.prof

cat("      Plot with subject 1 as reference profile \n")
itim<-unique(xtheo_cens@data@data$Time[xtheo_cens@data@data$ID==1])
itim1<-itim[-c(4,6,8,10)]
plot(xtheo_cens,plot.type="x.scatter",xscale=T,ref.prof=list(Time=itim1),
     main = "xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 16 : ")

plot(xtheo_cens,plot.type="x.scatter",xscale=T,ref.prof=list(Time=c(0.00,0.25,0.57,2.02,5.10,9.05,24.37)),
     main = "xtheo_cens",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 17 : ")
# plot x.scatter : npde by coavriates

plot(xtheo_cens,plot.type="x.scatter", xscale=T , covsplit=TRUE, which.cov="Wt",which="npde",
     main = "xtheo_cens Wt",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 18 : ")

plot(xtheo_cens,plot.type="x.scatter", xscale=T , covsplit=TRUE, which.cov="Sex",which="npde",
     main = "xtheo_cens Sex",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,
     grid=TRUE,
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

print( "plot xscatter 19 : ")
# message error si  covariables et covsplit=TRUE avec which.cov=""
## ! spline not  !! plot(xtheo_cens,plot.type="x.scatter",xscale=T,covsplit=TRUE,which.cov="")

# --------------------------------------------------------------------------------------------------------
# Plot : Remifentanil
# --------------------------------------------------------------------------------------------------------

# plot x.scatter, npde, covsplit=TRUE

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="LBM",which="npde")
print( "plot xscatter 20 : ")

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="LBM",which="npde",
                main = "xrem LBM",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "npde",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
print( "plot xscatter 21 : ")
# plot x.scatter, pd, covsplit=TRUE

plot(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="LBM",which="pd",
     main = "xrem LBM",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "pd",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
     size.ppcens = 1.75, ## pcens ## modifs !

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)
print( "plot xscatter 22 : ")
# plot x.scatter, pd, covsplit=TRUE

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="npde",
                main = "xrem Age",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "npde",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
print( "plot xscatter 23 : ")
# plot x.scatter, pd, covsplit=TRUE

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="pd",
                main = "xrem Age",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "pd",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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

print( "plot xscatter 24 : ")
# plot(xtheo_cens,plot.type="cov.scatter", which.cov="Sex",which="npde")
# xy logscales
plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="pd",xlog=FALSE,ylog=FALSE)
plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="pd",xlog=FALSE,ylog=TRUE)
plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="pd",xlog=TRUE,ylog=FALSE)
plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="pd",xlog=TRUE,ylog=TRUE)
print( "plot xscatter 25 : ")

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="pd",
                main = "xrem Age",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "pd",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
                ylog = TRUE)
print( "plot xscatter 26 : ")
# --------------------------------------------------------------------------------------------------------
# Plot : user guide npde
# --------------------------------------------------------------------------------------------------------

plot.NpdeObject(xtheo, plot.type="x.scatter",which="npde",
                main = "xtheo",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "npde",
                size.xlab = 12,
                size.ylab = 12,

                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
print( "plot xscatter 27 : ")

plot.NpdeObject(xtheo, plot.type="x.scatter",which="pd")
print( "plot xscatter 28 : ")

plot.NpdeObject(xtheo, plot.type="x.scatter",which="pd",
                main = "xtheo",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "pd",
                size.xlab = 12,
                size.ylab = 12,

                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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

print( "plot xscatter 29 : ")
# x.scatter, covsplit

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="LBM",which="npde")
print( "plot xscatter 30 : ")

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="LBM",which="pd")
print( "plot xscatter 31 : ")

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="LBM",which="npde",
                main = "xtheo",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "pd",
                size.xlab = 12,
                size.ylab = 12,

                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
print( "plot xscatter 32 : ")

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="LBM",which="pd",
                main = "xrem LBM",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "pd",
                size.xlab = 12,
                size.ylab = 12,

                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
print( "plot xscatter 33 : ")

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="npde")
print( "plot xscatter 34 : ")

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="pd")
print( "plot xscatter 35 : ")

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="npde",
                main = "xrem LBM",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "pd",
                size.xlab = 12,
                size.ylab = 12,

                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
print( "plot xscatter 36 : ")

plot.NpdeObject(xrem,plot.type="x.scatter", covsplit=TRUE, which.cov="Age",which="pd",
                main = "xrem Age",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "pd",
                size.xlab = 12,
                size.ylab = 12,

                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
print( "plot xscatter 37 : ")

plot.NpdeObject(xvir.full1, plot.type="x.scatter",which="npde")
print( "plot xscatter 38 : ")

plot.NpdeObject(xvir.full1, plot.type="x.scatter",which="pd")
print( "plot xscatter 39 : ")

plot.NpdeObject(xvir.full1, plot.type="x.scatter",which="npde",
                main = "xvir.full1",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "npde",
                size.xlab = 12,
                size.ylab = 12,

                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
print( "plot xscatter 40 : ")

plot.NpdeObject(xvir.full1, plot.type="x.scatter",which="pd",
                main = "xvir.full1",
                size.main = 14,
                sub = "",
                size.sub = 10,

                xlab= "Time",
                ylab= "pd",
                size.xlab = 12,
                size.ylab = 12,

                xlim=c(),
                ylim=c(),

                approx.pi=TRUE,
                bands=TRUE,
                plot.obs=TRUE,

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
print( "plot xscatter 41 : ")

plot(x50.omit, plot.type="x.scatter",which="npde")
print( "plot xscatter 42 : ")

plot(x50.omit, plot.type="x.scatter",which="pd")
print( "plot xscatter 43 : ")

plot(x50.omit, plot.type="x.scatter",which="npde",
     main = "x50.omit",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 44 : ")

plot(x50.omit, plot.type="x.scatter",which="pd",
     main = "x50.omit",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "pd",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 45 : ")

plot(x50, plot.type="x.scatter",which="npde")
print( "plot xscatter 46 : ")

plot(x50, plot.type="x.scatter",which="pd")
print( "plot xscatter 47 : ")

plot(x50, plot.type="x.scatter",which="npde",
     main = "x50.omit",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 48 : ")

plot(x50, plot.type="x.scatter",which="pd",
     main = "x50.omit",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "pd",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 49 : ")

plot(xtheo, plot.type="x.scatter",which="npde")
print( "plot xscatter 50 : ")
plot(xtheo, plot.type="x.scatter",which="pd")
print( "plot xscatter 51 : ")

plot(xtheo, plot.type="x.scatter",which="npde",
     main = "xtheo",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 52 : ")

plot(xtheo, plot.type="x.scatter",which="pd",
     main = "xtheo",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "pd",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 53 : ")


# x.scatter, impute.loq
plot(x50, plot.type="x.scatter",which="npde",impute.loq=TRUE)
print( "plot xscatter 54 : ")

plot(x50, plot.type="x.scatter",which="npde",impute.loq=FALSE)
print( "plot xscatter 55 : ")

plot(x50, plot.type="x.scatter",which="npde",impute.loq=TRUE,
     main = "x50",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 56 : ")

plot(x50, plot.type="x.scatter",which="npde",impute.loq=FALSE,
     main = "x50",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 57 : ")

#------------------------------------------------------------------------------------------
# FROM : testsDemo.R
#------------------------------------------------------------------------------------------

# --------------------------------------------------------
# Demo, default plots without censoring
# --------------------------------------------------------

plot(xbase1,plot.type="x.scatter",main="No BQL, npde vs X")
print( "plot xscatter 58 : ")

plot(xbase1,plot.type="x.scatter",main="No BQL, npde vs X",
     main = "x50",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 59 : ")


plot(xcens2,plot.type="x.scatter",main="Method=cdf, npde vs X")
print( "plot xscatter 60 : ")

plot(xcens2,plot.type="x.scatter",main="Method=cdf, npde vs X",
     main = "x50",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 61 : ")


plot(xcens2,plot.type="x.scatter",covsplit=TRUE,which.cov="Sex")
print( "plot xscatter 62 : ")

plot(xcens2,plot.type="x.scatter",covsplit=TRUE,which.cov="Sex",
     main = "xcens2",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 63 : ")


plot(x20.omit,plot.type="x.scatter",new=F)
print( "plot xscatter 64 : ")

plot(x20.omit,plot.type="x.scatter",
     main = "xcens2",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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

print( "plot xscatter 64 : ")

plot(x20,plot.type="x.scatter",new=F)
print( "plot xscatter 65 : ")

plot(x20,plot.type="x.scatter",
     main = "x20",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 66 : ")

plot(x50,plot.type="x.scatter",new=F)
print( "plot xscatter 67 : ")

plot(x50,plot.type="x.scatter",
     main = "x20",
     size.main = 14,
     sub = "",
     size.sub = 10,

     xlab= "Time",
     ylab= "npde",
     size.xlab = 12,
     size.ylab = 12,

     xlim=c(),
     ylim=c(),

     approx.pi=TRUE,
     bands=TRUE,
     plot.obs=TRUE,

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
print( "plot xscatter 68 : ")

# ##########################################################################################
# END TESTS
# ##########################################################################################











