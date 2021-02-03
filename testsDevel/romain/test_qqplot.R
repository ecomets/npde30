
##########################################################################################################

# Tests Options Graph ggplot plot.type : pred.scatter

##########################################################################################################

###  --------------------------------------------------------------------------------------------------------

### only plot.type

###  --------------------------------------------------------------------------------------------------------

plot(xtheo_nocens,plot.type="qqplot")
plot(xvir.full1,plot.type="qqplot")
plot(xtheo,plot.type="qqplot")
plot(xbase1,plot.type="qqplot")
plot(x50.omit,plot.type="qqplot")
plot(xtheo_cens_pd_nan,plot.type="qqplot")
plot(xtheo_cens,plot.type="qqplot")
plot(xrem ,plot.type="qqplot")
plot(xcens2,plot.type="qqplot")

# --------------------------------------------------------------------------------------------------------
# Plot : xtheo_nocens
# --------------------------------------------------------------------------------------------------------

# qqplot with bands / no bands &  y / ylog
plot(xtheo_nocens, plot.type="qqplot",which="npde",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     type = "p", #b,l,p

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=FALSE, # TRUE, FALSE
     grid=TRUE,
     lty.grid="dashed",
     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)  # TRUE, FALSE





# qqplot with no covariates & covsplit=TRUE -->  covsplit=FALSE + message "No covariates in the dataset\n"
plot(xtheo_nocens, plot.type="qqplot",
     which="npde",
     type = "p", #b,l,p
     covsplit=TRUE,bands=FALSE)


# plot qqplot, npde, no censored data, no covariates
plot(xtheo_nocens, plot.type="qqplot",which="npde",
     main = "QQ-plot versus N(0,1) for npde",

     type = "p", #b,l,p

     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=FALSE,
     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# --------------------------------------------------------------------------------------------------------
# Plot : xtheo_cens_pd_nan
# --------------------------------------------------------------------------------------------------------

# plot qqplot, npde, no censored data, quantitative covariate "Wt"
plot(xtheo_cens_pd_nan, plot.type="qqplot", covsplit=TRUE, which.cov="Wt", which="npde",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

## plot qqplot, npde, no censored data, qualitative covariate "Sex"
plot(xtheo_cens_pd_nan, plot.type="qqplot", covsplit=TRUE, which.cov="Sex", which="npde",
     main = "
       QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# --------------------------------------------------------------------------------------------------------
# Plot : xtheo_cens
# --------------------------------------------------------------------------------------------------------

plot(xtheo_cens,
     plot.type="qqplot",
     covsplit=TRUE,
     which.cov="Wt",
     which="npde",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# --------------------------------------------------------------------------------------------------------
# Plot : Remifentanil
# --------------------------------------------------------------------------------------------------------

plot(xrem,plot.type="qqplot", which="npde", covsplit=FALSE, which.cov="Age",bands=TRUE)

plot(xrem,
     plot.type="qqplot",
     which="npde",
     covsplit=FALSE,
     which.cov="Age",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     alpha.bands = 0.25,
     fill.bands = "orange",
     col.bands = "red",
     lty.bands = 2,
     lwd.bands = 1,

     col.pobs = "darkblue",
     size.pobs = 2,

     col.pcens = "green",
     size.pcens = 2,

     col="steelblue4",
     lty=2,
     lwd=1,

     size.text.x = 10,
     size.text.y = 10,

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# plot covariates with bands
plot(xrem,plot.type="qqplot", which="npde", covsplit=TRUE, which.cov="Age",bands=TRUE)
# plot covariates with no bands
plot(xrem,plot.type="qqplot", which="npde", covsplit=FALSE, which.cov="Age",bands=FALSE)

# --------------------------------------------------------------------------------------------------------
# Plot : user guide npde
# --------------------------------------------------------------------------------------------------------

plot(xtheo,plot.type="qqplot",which="npde",bands=TRUE)
plot(xtheo,plot.type="qqplot",which="pd",bands=TRUE)

plot(xtheo,plot.type="qqplot",which="npde",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE

     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# qqplot, covsplit

plot(xrem,plot.type="qqplot", which="npde", covsplit=TRUE, which.cov="Age",bands=TRUE)

plot(xrem,plot.type="qqplot", which="pd", covsplit=TRUE, which.cov="Age",bands=TRUE)

plot(xrem,plot.type="qqplot", which="pd", covsplit=TRUE, which.cov="Age",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

plot.NpdeObject(xvir.full1,plot.type="qqplot",which="npde",
                main = "QQ-plot versus N(0,1) for npde",
                size.main = 14,
                sub = "",
                size.sub = "",

                xlab= "Sample quantiles (npde)",
                ylab= "Theorical quantiles",
                size.xlab = 12, ## cex.axis = 12
                size.ylab = 12,
                #xlim=c(0,25),
                #ylim=c(-3,3.5),
                xlim=c(),
                ylim=c(),

                bands=TRUE, # TRUE, FALSE

                alpha.bands = 0.25,
                fill.bands = "orange",
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
                breaks.y = 10,

                xlog = FALSE,
                ylog = FALSE)

# qqplot
plot(x50.omit,plot.type="qqplot",which="npde",bands=TRUE)
plot(x50.omit,plot.type="qqplot",which="pd",bands=TRUE)

plot(x50.omit,plot.type="qqplot",which="npde",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# --------------------------------------------------------
# Demo, default plots without censoring
# --------------------------------------------------------

#cat("Showing default plots for npde, no censoring\n")

plot(xbase1,plot.type="qqplot",main="No BQL, QQ-plot")

plot(xbase1,plot.type="qqplot",main="No BQL, QQ-plot",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,
     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# --------------------------------------------------------
# Demo, default plots with censoring
# --------------------------------------------------------

#cat("Showing default plots for npde, with censoring (method=cdf)\n")
#cat("Showing default plots for npde, with censoring (method=cdf), splitting by covariates\n")

plot(xcens2,plot.type="qqplot",covsplit=TRUE,which.cov="Wt",bands=FALSE)

plot(xcens2,plot.type="qqplot",covsplit=TRUE,which.cov="Wt",bands=TRUE)

plot(xcens2,plot.type="qqplot",covsplit=TRUE,which.cov="Wt",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     grid=TRUE,
     bands=TRUE, # TRUE, FALSE

     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 20,

     xlog = FALSE,
     ylog = FALSE)

plot(xcens2,plot.type="qqplot",covsplit=TRUE,which.cov="Sex")

plot(xcens2,plot.type="qqplot",covsplit=TRUE,which.cov="Sex",
     main = "QQ-plot versus N(0,1) for npde",
     size.main = 14,
     sub = "",
     size.sub = "",

     xlab= "Sample quantiles (npde)",
     ylab= "Theorical quantiles",
     size.xlab = 12, ## cex.axis = 12
     size.ylab = 12,
     #xlim=c(0,25),
     #ylim=c(-3,3.5),
     xlim=c(),
     ylim=c(),

     bands=TRUE, # TRUE, FALSE
     grid=TRUE,

     alpha.bands = 0.25,
     fill.bands = "orange",
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
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)

# ##########################################################################################
# END CODE
# ##########################################################################################






