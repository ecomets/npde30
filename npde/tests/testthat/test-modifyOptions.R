
wbase<-autonpde(namobs="../../data/warfarin.tab",namsim="../../data/simwarfarinBase.tab",
                iid=1,ix=2,iy=4,icov=c(3,6:8),namsav="warfBase", units=list(x="hr",y="ug/L", covariates=c("mg","kg","-","yr")))

test_that("Testing if the user options are in plot.opt", {
   
   # plot.opt of npde
   plot.opt <- wbase["prefs"]
   # user plot.opt
   userPlotOptions = list( xlab = "xlabel changed", ylab = "ylabel changed" )
   plot.opt <- set.plotoptions.default( wbase )
   # modify the plot.opt with user plot options
   plot.opt <- modifyList( plot.opt, userPlotOptions[ intersect( names( userPlotOptions ), names( plot.opt ) ) ] )
   # check if userPlotOptions are in plot.opt
   expect_equal( all( userPlotOptions %in% plot.opt ), TRUE )
   
 })
 



