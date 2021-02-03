# ---------------------------------------------------------------------------------------------------

# testthat portant sur aux.npdeplot.plot avec un obsmat, un pimat et une liste plot.opt

# ---------------------------------------------------------------------------------------------------

# plot from:
# plot(xtheo_cens,plot.type="x.scatter", covsplit=TRUE, which.cov=c("Wt"),
#
#      main = "Wt data xtheo_cens",
#      size.main = 14,
#      sub = "",
#      size.sub = 10,
#
#      xlab= "Time",
#      ylab= "npde",
#      size.xlab = 12, ## cex.axis = 12
#      size.ylab = 12,
#      #xlim=c(0,25),
#      #ylim=c(-3,3.5),
#      xlim=c(),
#      ylim=c(),
#
#      approx.pi=TRUE,
#      bands=TRUE,
#      plot.obs=TRUE,
#      grid=FALSE,
#      plot.box=FALSE, #FALSE, ## changer en options box.plot
#
#      alpha.med = 0.25,
#      fill.med = "firebrick4",
#      col.med="red",
#      lty.med=3,
#      lwd.med=1,
#
#      alpha.bands = 0.25,
#      fill.bands = "dodgerblue",
#      col.bands="green",
#      lty.bands=6,
#      lwd.bands=1,
#
#      col.med = "red",
#      lty.med = 1,
#      lwd.med = 1,
#
#      col.bands = "blue",
#      lty.bands = 1,
#      lwd.bands = 1,
#
#      col.pobs = "orangered3",
#      pch.pobs = 12,
#      size.pobs = 1.5,
#
#      col.pcens = "yellow",
#      pch.pcens = 15,
#      size.pcens = 1.75,
#
#      size.text.x = 10,
#      size.text.y = 10,
#
#      breaks.x = 10,
#      breaks.y = 10,
#
#      xlog = FALSE,
#      ylog = FALSE)


# data : xtheo_cens
obs_mat <- readRDS( "obs_mat_xtheo_cens.rds")
pi_mat <- readRDS("pi_mat_xtheo_cens.rds")
plot_opt <- readRDS("plot_opt_xtheo_cens_scatter.rds")

# plot function
# aux.npdeplot.plot(obs_mat, pi_mat, plot_opt)

# tests
test_that("Tests on obs.mat and pi.mat", {

cat("Number of censored and no censored data")
expect_equal(length(which(obs_mat$cens==1)), 15)
expect_equal(length(which(obs_mat$cens==0)), 105)

cat("Dimensions of pi.mat")
expect_equal(dim(pi_mat)[1], 30)
expect_equal(dim(pi_mat)[2], 19)

cat("Columns names for of pi.mat")
expect_match( colnames(pi_mat)[1], c("groups"))
expect_match( colnames(pi_mat)[3], c("covariate"))
expect_match( colnames(pi_mat)[4], c("xcent"))
expect_match( colnames(pi_mat)[5], c("binf0025"))
expect_match( colnames(pi_mat)[14], c("per.min"))
expect_match( colnames(pi_mat)[19], c("dotline.max"))

cat("Bining")
h = hist(pi_mat$xcent)
expect_equal(length(h$breaks),6,10e-6)
expect_true(h$equidist)

cat("Tests on plot options")
expect_equal(length(plot_opt), 86)
expect_match(plot_opt$xaxis, "x")
expect_match(plot_opt$which, "npde")
expect_equal(plot_opt$bin.number, 10)
expect_true(plot_opt$covsplit)

cat("Covariate")
expect_identical(as.character(as.factor(pi_mat$covariate))[1],"Wt")
expect_identical(unique(as.character(as.factor(pi_mat$category)))[1],"Wt: <Q1")
expect_identical(unique(as.character(as.factor(pi_mat$category)))[2],"Wt: Q1-Q3")
expect_identical(unique(as.character(as.factor(pi_mat$category)))[3],"Wt: >Q3")

cat("Summary of npde (should be roughly N(0,1))\n")
print(summary(obs_mat$y))

cat("Summary of dotline min and max \n")
expect_equal(round(unique(pi_mat$dotline.min),2),-1.96,10e-6)
expect_equal(round(unique(pi_mat$dotline.max),2), 1.96,10e-6)

})




