
# Waffle plot

hist <- npde.plot.dist(xvir.full1,which="npde",dist.type="hist",plot.default=TRUE)
qqplot <- npde.plot.dist(xvir.full1,which="npde",dist.type="qqplot",plot.default=TRUE)
x.scatter <- npde.plot.meanprofile(xvir.full1,which="npde", xaxis="x", plot.default=TRUE)
pred.scatter <- npde.plot.meanprofile(xvir.full1,which="npde", xaxis="pred", plot.default=TRUE)

grid.arrange(hist, qqplot, x.scatter, pred.scatter, pred.scatter, nrow=3, ncol=2,top="")




