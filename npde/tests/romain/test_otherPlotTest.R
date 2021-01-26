
plot(xtheo_cens,
     plot.type="vpc",
     main = c("Wt","Sex M/F data xtheo_cens"),
     covsplit=TRUE,
     plot.box=TRUE,
     grid=TRUE,
     which.cov=c("Wt","Sex"),
     )

plot(xtheo_cens,

     plot.loq=TRUE,

     plot.type="x.scatter",
     covsplit=TRUE,
     which.cov="Sex",
     which="npde",
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

     xlog = TRUE,
     ylog = FALSE)




if (F){


plot(xtheo,plot.type="vpc",plot.loq=FALSE)

plot(xtheo_cens,plot.type="vpc")

plot(xtheo_cens,plot.type="vpc", main = "Sex M/F data xtheo_cens", covsplit=TRUE, which.cov="Sex")

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

plot(xtheo_cens,plot.type="x.scatter", covsplit=TRUE,
     which.cov="Wt",

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


plot(xtheo_cens,plot.type="x.scatter", covsplit=FALSE, which.cov="Wt",

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

plot(xtheo_cens, plot.type="x.scatter",
     which.cov="Wt",
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

     breaks.x = 10,
     breaks.y = 10,

     xlog = FALSE,
     ylog = FALSE)


}
