
##########################################################################################################

# Tests Options Graph ggplot plot.type : pred.scatter

##########################################################################################################

###  --------------------------------------------------------------------------------------------------------

### only plot.type

###  --------------------------------------------------------------------------------------------------------

plot(xtheo_nocens,plot.type="pred.scatter")
plot(xvir.full1,plot.type="pred.scatter")
plot(xtheo,plot.type="pred.scatter")
plot(xbase1,plot.type="pred.scatter")
plot(x50.omit,plot.type="pred.scatter")
plot(xtheo_cens_pd_nan,plot.type="pred.scatter")
plot(xtheo_cens,plot.type="pred.scatter")
plot(xrem ,plot.type="pred.scatter")
plot(xcens2,plot.type="pred.scatter")

### --------------------------------------------------------------------------------------------------------

### plot with user options

### --------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------
# Plot : xtheo_nocens
# --------------------------------------------------------------------------------------------------------

plot(xtheo_nocens, plot.type="pred.scatter",which="npde",
main = "xtheo_nocens",
size.main = 14,
sub = "",
size.sub = "",

type="b",

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

alpha.med= 0.25,
fill.med= "firebrick4",
col.med="red",
lty.med=3,
llwd.med=1,

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







plot(xtheo_nocens, plot.type="pred.scatter",which="pd",
     main = "xtheo_nocens",
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

     alpha.med= 0.25,
     fill.med= "firebrick4",
     col.med="red",
     lty.med=3,
     llwd.med=1,

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
# Plot : xtheo_cens_pd_nan
# --------------------------------------------------------------------------------------------------------

plot(xtheo_cens_pd_nan, plot.type="pred.scatter",which="npde",
main = "xtheo_cens_pd_nan",
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

alpha.med= 0.25,
fill.med= "firebrick4",
col.med="red",
lty.med=3,
llwd.med=1,

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
# Plot : xtheo_cens
# --------------------------------------------------------------------------------------------------------

plot(xtheo_nocens, plot.type="pred.scatter",which="npde",
     main = "xtheo_nocens",
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

     alpha.med= 0.25,
     fill.med= "firebrick4",
     col.med="red",
     lty.med=3,
     llwd.med=1,

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

## --------------------------------------------------------------------------------------------------------
## Plot : user guide npde
## --------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------
# Plot : Remifentanil
# --------------------------------------------------------------------------------------------------------

plot.NpdeObject(xtheo,plot.type="pred.scatter",which="npde",
                main = "xtheo",
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

                alpha.med= 0.25,
                fill.med= "firebrick4",
                col.med="red",
                lty.med=3,
                llwd.med=1,

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

plot.NpdeObject(xvir.full1,plot.type="pred.scatter",which="npde",
                main = "xvir.full1",
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

                alpha.med= 0.25,
                fill.med= "firebrick4",
                col.med="red",
                lty.med=3,
                llwd.med=1,

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


plot.NpdeObject(xvir.full1,plot.type="pred.scatter",which="pd",
                main = "xvir.full1",
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

                alpha.med= 0.25,
                fill.med= "firebrick4",
                col.med="red",
                lty.med=3,
                llwd.med=1,

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

plot.NpdeObject(x50.omit,plot.type="pred.scatter",which="npde",
                main = "x50.omit",
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

                alpha.med= 0.25,
                fill.med= "firebrick4",
                col.med="red",
                lty.med=3,
                llwd.med=1,

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


plot.NpdeObject(x50.omit,plot.type="pred.scatter",which="pd",
                main = "x50.omit",
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

                alpha.med= 0.25,
                fill.med= "firebrick4",
                col.med="red",
                lty.med=3,
                llwd.med=1,

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


plot(x50, plot.type="pred.scatter",which="npde",
     main = "x50",
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

     alpha.med= 0.25,
     fill.med= "firebrick4",
     col.med="red",
     lty.med=3,
     llwd.med=1,

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


plot(x50, plot.type="pred.scatter",which="pd",
     main = "x50",
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

     alpha.med= 0.25,
     fill.med= "firebrick4",
     col.med="red",
     lty.med=3,
     llwd.med=1,

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


xtheo@data@loq = 0.5

plot(xtheo,plot.type="pred.scatter",which="npde",
     main = "xtheo",
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
     impute.loq = TRUE,

     alpha.med= 0.25,
     fill.med= "firebrick4",
     col.med="red",
     lty.med=3,
     llwd.med=1,

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


plot(xtheo,plot.type="pred.scatter",which="pd",
     main = "xtheo",
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

     alpha.med= 0.25,
     fill.med= "firebrick4",
     col.med="red",
     lty.med=3,
     llwd.med=1,

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

plot(x50, plot.type="pred.scatter",which="npde",
     main = "x50",
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
     impute.loq=TRUE, # TRUE, FALSE

     alpha.med= 0.25,
     fill.med= "firebrick4",
     col.med="red",
     lty.med=3,
     llwd.med=1,

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
# Demo, default plots without censoring
# --------------------------------------------------------

plot(xbase1,plot.type="pred.scatter",main="No BQL, npde vs pred")

plot(xbase1,plot.type="pred.scatter",main="No BQL, npde vs pred",
  main = "xbase1 No BQL, npde vs pred ",
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
  impute.loq=TRUE, # TRUE, FALSE

  alpha.med= 0.25,
  fill.med= "firebrick4",
  col.med="red",
  lty.med=3,
  llwd.med=1,

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

plot(xcens2,plot.type="pred.scatter",main="Method=cdf, npde vs pred")

plot(xcens2,plot.type="pred.scatter",covsplit=TRUE,which.cov="Wt",
     main = "xcens2",
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
     impute.loq=TRUE, # TRUE, FALSE

     alpha.med= 0.25,
     fill.med= "firebrick4",
     col.med="red",
     lty.med=3,
     llwd.med=1,

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


# ##########################################################################################
# END TESTS
# ##########################################################################################

























