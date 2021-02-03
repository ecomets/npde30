
  # ##########################################################################################

  # Tests plot data

  # ##########################################################################################

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


  # plot data
  plot(x50,plot.type="data",
       main = " ",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= x50@data@name.predictor,
       ylab= x50@data@name.response,
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),

       bands=TRUE, # TRUE, FALSE
       plot.loq = TRUE,

       col.pcens = "red",
       pch.pcens = 20,
       size.pcens = 2,
       alpha.pcens = 0.5,

       col.pobs="steelblue4",
       pch.pobs=20,
       size.pobs=2,
       alpha.pobs = 0.5,

       col="black",		# default colour of plot
       lty=1,
       lwd=0.25,
       alpha = 0.5,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)

  # --------------------------------------------------------
  # Demo, default plots without censoring
  # --------------------------------------------------------

  #cat("Showing default plots for npde, no censoring\n")

  plot(xbase1,plot.type="data",
       main = "....",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= xbase1@data@name.predictor,
       ylab= xbase1@data@name.response,
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),

       bands=TRUE, # TRUE, FALSE
       plot.loq = TRUE,

       col.pcens = "red",
       pch.pcens = 20,
       size.pcens = 1,
       alpha.pcens = 0.5,

       col.pobs="steelblue4",
       pch.pobs=20,
       size.pobs=4,
       alpha.pobs = 0.5,

       col = "black",		# default colour of plot
       lty = 1,
       lwd = 0.25,
       alpha = 0.5,

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

  plot(xcens2,plot.type="data",
       main = "...",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= xcens2@data@name.predictor,
       ylab= xcens2@data@name.response,
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),

       bands=TRUE, # TRUE, FALSE
       plot.loq = TRUE,

       col.pcens = "red",
       pch.pcens = 20,
       size.pcens = 4,
       alpha.pcens = 0.5,

       col.pobs="blue",
       pch.pobs=20,
       size.pobs=4,
       alpha.pobs = 0.5,

       col = "black",		# default colour of plot
       lty = 1,
       lwd = 0.25,
       alpha = 0.5,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)



  plot(xtheo,plot.type="data",
       main = "...",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= xcens2@data@name.predictor,
       ylab= xcens2@data@name.response,
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),

       bands=TRUE, # TRUE, FALSE
       plot.loq = TRUE,

       col.pcens = "red",
       pch.pcens = 20,
       size.pcens = 4,
       alpha.pcens = 0.5,

       col.pobs="blue",
       pch.pobs=20,
       size.pobs=4,
       alpha.pobs = 0.5,

       col = "black",		# default colour of plot
       lty = 1,
       lwd = 0.25,
       alpha = 0.5,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)


plot(x50,plot.type="data",new=F,ylab="Viral load (cp/mL, in log10)",
     main = "...",
     size.main = 14,
     sub = "",
     size.sub = "",


     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),
     plot.loq = FALSE,

     col.pcens = "red",
     pch.pcens = 20,
     size.pcens = 4,
     alpha.pcens = 0.5,

     col.pobs="blue",
     pch.pobs=20,
     size.pobs=4,
     alpha.pobs = 0.5,

     col = "black",		# default colour of plot
     lty = 1,
     lwd = 0.25,
     alpha = 0.5,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)


plot(x50,plot.type="data",ylab="Viral load (cp/mL, in log10)",
       main = "...",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= xcens2@data@name.predictor,
       ylab= xcens2@data@name.response,
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),
       plot.loq = TRUE,
       impute.loq=TRUE,
       col.pcens = "red",
       pch.pcens = 20,
       size.pcens = 4,
       alpha.pcens = 0.5,

       col.pobs="blue",
       pch.pobs=20,
       size.pobs=4,
       alpha.pobs = 0.5,

       col = "black",		# default colour of plot
       lty = 1,
       lwd = 0.25,
       alpha = 0.5,

       size.text.x = 10,
       size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)



  plot(x50.omit,plot.type="data",
       main = "...",
       size.main = 14,
       sub = "",
       size.sub = "",

       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-10,10),
       xlim=c(),
       ylim=c(),
       plot.loq = TRUE,

       col.pcens = "red",
       pch.pcens = 20,
       size.pcens = 4,
       alpha.pcens = 0.5,

       col.pobs="blue",
       pch.pobs=20,
       size.pobs=4,
       alpha.pobs = 0.5,

       col = "black",		# default colour of plot
       lty = 1,
       lwd = 0.25,
       alpha = 0.5,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)


  x50@data@loq=2.5

  plot(x50,plot.type="data",
       main = "...",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= x50@data@name.predictor,
       ylab= x50@data@name.response,
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       xlim=c(),
       ylim=c(),

       plot.loq = TRUE,

       col.pcens = "red",
       pch.pcens = 20,
       size.pcens = 4,
       alpha.pcens = 0.5,

       col.pobs="blue",
       pch.pobs=20,
       size.pobs=4,
       alpha.pobs = 0.5,

       col = "black",		# default colour of plot
       lty = 1,
       lwd = 0.25,
       alpha = 0.5,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)


# ##########################################################################################
# END CODE
# ##########################################################################################






