
##########################################################################################################

# Tests Options Graph ggplot plot.type : pred.scatter

##########################################################################################################

###  --------------------------------------------------------------------------------------------------------

### only plot.type

###  --------------------------------------------------------------------------------------------------------

plot(xtheo_nocens,plot.type="hist")
plot(xvir.full1,plot.type="hist")
plot(xtheo,plot.type="hist")
plot(xbase1,plot.type="hist")
plot(x50.omit,plot.type="hist")
plot(xtheo_cens_pd_nan,plot.type="hist")
plot(xtheo_cens,plot.type="hist")
plot(xrem ,plot.type="hist")
plot(xcens2,plot.type="hist")

# --------------------------------------------------------------------------------------------------------
# Plot : xtheo_nocens
# --------------------------------------------------------------------------------------------------------

# hist, no covariates, with bands / no bands
plot(xtheo_nocens, plot.type="hist",which="npde",
     main = "....",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Frequency",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=FALSE,
     lty.grid="dashed",

     col = "orange",
     fill = "skyblue",
     alpha = 0.95,
     lty = 2,
     lwd = 1,

     col.bands = "yellow",
     fill.bands = "blue" ,
     alpha.bands = 0.5,
     lty.bands = 1,
     lwd.bands = 0.3,

     col.pobs = "darkblue",
     size.pobs = 2,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 20,
     breaks.y = 20,

     xlog = FALSE,
     ylog = FALSE)  # TRUE, FALSE


# --------------------------------------------------------------------------------------------------------
# Plot : xtheo_cens_pd_nan
# --------------------------------------------------------------------------------------------------------

## plot qqplot, hist, no censored data, qualitative covariate "Sex"
plot(xtheo_cens_pd_nan, plot.type="hist", covsplit=TRUE, which.cov="Sex", which="npde",
     main = "....",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Frequency",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     lty.grid="dotted",

     col = "green",
     fill = "skyblue",
    alpha = 0.95,
     lty = 2,
     lwd = 1,

    col.bands = "red",
     fill.bands = "red" ,
     alpha.bands = 0.5,
     lty.bands = 1,
     lwd.bands = 0.5,

     col.pobs = "darkblue",
     size.pobs = 2,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)  # TRUE, FALSE

# --------------------------------------------------------------------------------------------------------
# Plot : Remifentanil
# --------------------------------------------------------------------------------------------------------

# ici meta function cov.hist ...

# plot hist, covsplit=TRUE, band = TRUE / FALSE

plot(xrem,plot.type="hist", which="npde", covsplit=FALSE, which.cov="LBM",
                main = "....",
                size.main = 14,
                sub = "",
                size.sub = "",

                xlab= "Sample quantiles (npde)",
                ylab= "Frequency",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                bands=TRUE, # TRUE, FALSE
                grid=TRUE,
                lty.grid="dotted",

                col = "green",
                fill = "skyblue",
               alpha = 0.95,
                lty = 2,
                lwd = 1,

               col.bands = "red",
                fill.bands = "red" ,
                alpha.bands = 0.5,
                lty.bands = 1,
                lwd.bands = 0.5,

                col.pobs = "darkblue",
                size.pobs = 2,

                size.text.x = 10,
                size.text.y = 10,

     breaks.x = 10,
                breaks.y = 10,

                xlog = FALSE,
                ylog = FALSE)

# plot hist, covsplit=FALSE, band = FALSE
plot(xrem,plot.type="hist", which="pd", covsplit=FALSE, which.cov="LBM",bands=FALSE)

# --------------------------------------------------------------------------------------------------------
# Plot : user guide npde
# --------------------------------------------------------------------------------------------------------

plot(xtheo,plot.type="hist",which="npde",bands=TRUE)
plot(xtheo,plot.type="hist",which="pd",bands=TRUE)

plot(xtheo,plot.type="hist",which="npde",
                main = "....",
                size.main = 14,
                sub = "",
                size.sub = "",

                xlab= "Sample quantiles (npde)",
                ylab= "Frequency",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                bands=TRUE, # TRUE, FALSE
                lty.grid="dotted",

                col = "green",
                fill = "skyblue",
               alpha = 0.95,
                lty = 2,
                lwd = 1,

               col.bands = "red",
                fill.bands = "red" ,
                alpha.bands = 0.5,
                lty.bands = 1,
                lwd.bands = 0.5,

                col.pobs = "darkblue",
                size.pobs = 2,

                size.text.x = 10,
                size.text.y = 10,

     breaks.x = 10,
                breaks.y = 10,

                xlog = FALSE,
                ylog = FALSE)

plot(xrem,plot.type="hist", which="npde", covsplit=TRUE, which.cov="LBM", bands=TRUE)
plot(xrem,plot.type="hist", which="pd", covsplit=TRUE, which.cov="LBM")

plot(xrem,plot.type="hist", which="npde", covsplit=TRUE, which.cov="LBM",
                main = "....",
                size.main = 14,
                sub = "",
                size.sub = "",

                xlab= "Sample quantiles (npde)",
                ylab= "Frequency",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                bands=TRUE, # TRUE, FALSE
                gris=TRUE,
                col = "green",
                fill = "skyblue",
               alpha = 0.95,
                lty = 2,
                lwd = 1,

               col.bands = "red",
                fill.bands = "red" ,
                alpha.bands = 0.5,
                lty.bands = 1,
                lwd.bands = 0.5,

                col.pobs = "darkblue",
                size.pobs = 2,

                size.text.x = 10,
                size.text.y = 10,

     breaks.x = 10,
                breaks.y = 10,

                xlog = FALSE,
                ylog = FALSE)

plot.NpdeObject(xvir.full1,plot.type="hist",which="npde",bands=TRUE)
plot.NpdeObject(xvir.full1,plot.type="hist",which="pd",bands=TRUE)
plot.NpdeObject(x50.omit,plot.type="hist",which="npde",bands=TRUE)
plot.NpdeObject(x50.omit,plot.type="hist",which="pd",bands=TRUE)

plot.NpdeObject(xvir.full1,plot.type="hist",which="npde",
                main = "....",
                size.main = 14,
                sub = "",
                size.sub = "",

                xlab= "Sample quantiles (npde)",
                ylab= "Frequency",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                bands=TRUE, # TRUE, FALSE

                col = "green",
                fill = "skyblue",
               alpha = 0.95,
                lty = 2,
                lwd = 1,

               col.bands = "red",
                fill.bands = "red" ,
                alpha.bands = 0.5,
                lty.bands = 1,
                lwd.bands = 0.5,

                col.pobs = "darkblue",
                size.pobs = 2,

                size.text.x = 10,
                size.text.y = 10,

                breaks.x = 10,
                breaks.y = 10,

                xlog = FALSE,
                ylog = FALSE)



plot.NpdeObject(x50.omit,plot.type="hist",which="npde",
                main = "....",
                size.main = 14,
                sub = "",
                size.sub = "",

                xlab= "Sample quantiles (npde)",
                ylab= "Frequency",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                bands=TRUE, # TRUE, FALSE

                col = "green",
                fill = "skyblue",
               alpha = 0.95,
                lty = 2,
                lwd = 1,

               col.bands = "red",
                fill.bands = "red" ,
                alpha.bands = 0.5,
                lty.bands = 1,
                lwd.bands = 0.5,

                col.pobs = "darkblue",
                size.pobs = 2,

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

  plot(xbase1,plot.type="hist",main="No BQL, histogram",
       main = "....",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= "Sample quantiles (npde)",
       ylab= "Frequency",
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),

       bands=TRUE, # TRUE, FALSE

       col = "green",
       fill = "skyblue",
      alpha = 0.95,
       lty = 2,
       lwd = 1,

      col.bands = "red",
       fill.bands = "red" ,
       alpha.bands = 0.5,
       lty.bands = 1,
       lwd.bands = 0.5,

       col.pobs = "darkblue",
       size.pobs = 2,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)



# plot(xcens2,plot.type="hist",covsplit=TRUE,which.cov="Wt",bands=FALSE)
  plot(xcens2,plot.type="hist",covsplit=TRUE,which.cov="Wt",
       main = "....",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= "Sample quantiles (npde)",
       ylab= "Frequency",
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),

       bands=TRUE, # TRUE, FALSE
       grid=TRUE,
       col = "green",
       fill = "skyblue",
      alpha = 0.95,
       lty = 2,
       lwd = 1,

      col.bands = "red",
       fill.bands = "red" ,
       alpha.bands = 0.5,
       lty.bands = 1,
       lwd.bands = 0.5,

       col.pobs = "darkblue",
       size.pobs = 2,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)



#  plot(xcens2,plot.type="hist",covsplit=TRUE,which.cov="Sex")
  plot(xcens2,plot.type="hist",covsplit=TRUE,which.cov="Sex",
       main = "....",
       size.main = 14,
       sub = "",
       size.sub = "",

       xlab= "Sample quantiles (npde)",
       ylab= "Frequency",
       size.xlab = 12, ## cex.axis = 12
       size.ylab = 12,
       #xlim=c(0,25),
       #ylim=c(-3,3.5),
       xlim=c(),
       ylim=c(),

       bands=TRUE, # TRUE, FALSE

       col = "green",
       fill = "skyblue",
      alpha = 0.95,
       lty = 2,
       lwd = 1,

      col.bands = "red",
       fill.bands = "red" ,
       alpha.bands = 0.5,
       lty.bands = 1,
       lwd.bands = 0.5,

       col.pobs = "darkblue",
       size.pobs = 2,

       size.text.x = 10,
       size.text.y = 10,

       breaks.x = 10,
       breaks.y = 10,

       xlog = FALSE,
       ylog = FALSE)


# ##########################################################################################
# END CODE
# ##########################################################################################






