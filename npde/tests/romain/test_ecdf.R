
##########################################################################################################

# Tests Options Graph ggplot plot.type : pred.scatter

##########################################################################################################

###  --------------------------------------------------------------------------------------------------------

### only plot.type

###  --------------------------------------------------------------------------------------------------------


  # --------------------------------------------------------------------------------------------------------
  # Plot : xtheo_nocens
  # --------------------------------------------------------------------------------------------------------

  # --------------------------------------------------------------------------------------------------------
  # Plot : xtheo_cens_pd_nan
  # --------------------------------------------------------------------------------------------------------

  # --------------------------------------------------------------------------------------------------------
  # Plot : xtheo_cens
  # --------------------------------------------------------------------------------------------------------

  # --------------------------------------------------------------------------------------------------------
  # Plot : Remifentanil
  # --------------------------------------------------------------------------------------------------------

  # --------------------------------------------------------------------------------------------------------
  # Plot : user guide npde
  # --------------------------------------------------------------------------------------------------------

  plot(xtheo_nocens,plot.type="ecdf")
  plot(xvir.full1,plot.type="ecdf")
  plot(xtheo,plot.type="ecdf")
  plot(xbase1,plot.type="ecdf")
  plot(x50.omit,plot.type="ecdf")
  plot(xtheo_cens_pd_nan,plot.type="ecdf")
  plot(xtheo_cens,plot.type="ecdf")
  plot(xrem ,plot.type="ecdf")
  plot(xcens2,plot.type="ecdf" )


  plot(xrem,plot.type="ecdf",covsplit=TRUE,which.cov="LBM",bands=TRUE)

  # ecdf
  plot(xtheo,plot.type="ecdf",

       type="b", ## "b","p"

       main = "xtheo",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= "Sample quantiles (npde)",
       ylab= "Empirical cdf",

       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,

       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),

       bands=TRUE, # TRUE, FALSE
       grid=TRUE,
       #alpha.bands = 0.5,
       fill.bands = "skyblue",
       col.bands = "red",
       lty.bands = 2,
       lwd.bands = 1,

       col.pobs = "darkblue",
       size.pobs = 2,

       col="steelblue4",
       lty=2,
       lwd=1,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10)

  # --------------------------------------------------------
  # Demo, default plots without censoring
  # --------------------------------------------------------

# plot(xbase1,plot.type="ecdf", bands=TRUE)

plot(xbase1,plot.type="ecdf",

     main = "xbase1",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Empirical cdf",

     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,

     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     #alpha.bands = 0.5,
     fill.bands = "skyblue",
     col.bands = "red",
     lty.bands = 2,
     lwd.bands = 1,

     col.pobs = "darkblue",
     size.pobs = 2,

     col="steelblue4",
     lty=2,
     lwd=1,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10)

# --------------------------------------------------------
# Demo, default plots with censoring
# --------------------------------------------------------

#cat("Showing default plots for npde, with censoring (method=cdf)\n")
#cat("Showing default plots for npde, with censoring (method=cdf), splitting by covariates\n")

# censored data : removes
# error message is covsplit=TRUE and no proper number of variables / covariables

plot(xcens2,plot.type="ecdf",covsplit=TRUE,which.cov="Wt",
     main = "xcens2",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Empirical cdf",

     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,

     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     fill.bands = "skyblue",
     col.bands = "red",
     lty.bands = 2,
     lwd.bands = 1,

     col.pobs = "darkblue",
     size.pobs = 2,

     col="steelblue4",
     lty=2,
     lwd=1,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10)


plot(xcens2,plot.type="ecdf",covsplit=TRUE,which.cov="Sex",bands=TRUE)

plot(xcens2,plot.type="ecdf",covsplit=TRUE,which.cov="Sex",
     main = "xcens2",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Empirical cdf",

     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,

     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE


     fill.bands = "skyblue",
     col.bands = "red",
     lty.bands = 2,
     lwd.bands = 1,

     col.pobs = "darkblue",
     size.pobs = 2,

     col="steelblue4",
     lty=2,
     lwd=1,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10)


# plot(x20,plot.type="ecdf",bands=TRUE)
plot(x20,plot.type="ecdf",

     main = "xcens2",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Empirical cdf",

     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,

     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE

     fill.bands = "skyblue",
     col.bands = "red",
     lty.bands = 2,
     lwd.bands = 1,

     col.pobs = "darkblue",
     size.pobs = 2,

     col="steelblue4",
     lty=2,
     lwd=1,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10)


## plot(x50.omit,plot.type="ecdf",bands=TRUE)
plot(x50.omit,plot.type="ecdf",
     main = "x50.omit",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Empirical cdf",

     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,

     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE

     #alpha.bands = 0.5,
     fill.bands = "skyblue",
     col.bands = "red",
     lty.bands = 2,
     lwd.bands = 1,

     col.pobs = "darkblue",
     size.pobs = 2,

     col="steelblue4",
     lty=2,
     lwd=1,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10)

# ##########################################################################################
# END CODE
# ##########################################################################################






