# ------------------------------------------------------------------------------------
# from ECO : croiser une liste d'arguments donnés et passer ... avec des arguments enlevés
# utiliser toujours des arguments nommés (évite les dispatch foireux, qui se font dans l'ordre des arguments non nommés)
# ------------------------------------------------------------------------------------

# Understanding arguments

test_that("Named versus unnamed arguments in call with dots", {
  functionMain.argCall<- function( arg1, arg2="arg2", ...) {
    dots = list(...)
    args <- as.list( match.call() )[-1]
#    print(args, quote=TRUE)
#    print(dots, quote=TRUE)
#    cat("arg2=",arg2,"\n")
    return(list(args=args, dots=dots))
  }
  cat("If arguments are not named, the first two unnamed arguments will be affected to arg1 and arg2, so the default value for arg2 will not be used\n")
  x<-functionMain.argCall(20, arg3="40", arg5="arg 5", 2)
  expect_length(x$args,4)
  expect_length(x$dots,2)
  x<-functionMain.argCall(20, arg3="40", arg5="arg 5", arg6=2)
  expect_length(x$args,4)
  expect_length(x$dots,3)
  x<-functionMain.argCall(20, arg3="40", arg5="arg 5", 2)
  expect_length(x$args,4)
  expect_length(x$dots,2)
})

test_that("Removing one argument from dots", {
  functionMain.argCall<- function( arg1, arg2="arg2", ...) {
    dots = list(...)
    args <- as.list( match.call() )[-1]
    # Remove duplicated arguments to secure code, remove dist.type if given
    dots<-dots[!(names(dots) %in% c("arg1", "arg2", "dist.type"))]
    print(args, quote=TRUE)
    print(dots, quote=TRUE)
    iarg<-unlist(args["dist.type"])
    #  print(iarg, quote=TRUE)
    if(!is.null(iarg)) cat("dist.type=",iarg,"\n")
    return(list(args=args, dots=dots))
  }
  x<-functionMain.argCall(20, dist.type="hist", arg3="40", arg5="arg 5", arg6=2)
  expect_equal(names(x$dots), c("arg3","arg5","arg6"))
})

test_that("Embedding functions", {
  functionMain.dots<- function( arg1, arg2="arg2", ...) {
    dots = list(...)
    args <- as.list( match.call() )[-1]
    # Remove duplicated arguments to secure code
    dots<-dots[!(names(dots) %in% c("arg1", "arg2", "dist.type"))]
#    print(args, quote=TRUE)
#    print(dots, quote=TRUE)
#    print(arg2, quote=TRUE)
    iarg<-unlist(args["dist.type"])
#    if(!is.null(iarg)) cat("dist.type=",iarg,"\n")
    dots$dist.type<-"qqplot"
    x<-do.call(functionSub, dots)
    return(x)
  }
  functionSub<-function(dist.type="hist", ...) {
    dots = list(...)
    args <- as.list( match.call() )[-1]
#    print(dist.type, quote=TRUE)
#    print(dots, quote=TRUE)
    return(list(args=args, dots=dots))
  }
  x4<-functionMain.dots(arg1=20, arg3="40", arg5="arg 5", arg6=2, dist.type="hist")
  expect_equal(x4$args,list(dist.type="qqplot", arg3="40", arg5="arg 5", arg6=2))
  expect_length(x4$dots, 3)
})


# ------------------------------------------------------------------------------------

# test function avec 2 arguments functionTest et n arguments dans ellipsis  

test_that("Testing duplicate arguments in ellipsis", {
  
  functionTest <- function( arg1, arg2, ...)
  {
    # list des arguments
    args <- as.list( match.call() )[-1]
    # 2 refers to the number of arguments (arg1, arg2)
    args = args[1:2]
    # list des options
    dots = list(...)
    # indice pour les slots and remove redundant ones
    ind.dots <- which(  dots %in% args ) 
    if ( length(ind.dots) !=0 )
      dots = dots[-ind.dots]
    # return results with "..." avec des arguments enlevés
    return(  output = c( args = args, dots = dots ) )
  }
  
# test the function  
out = functionTest("x","dist","dist","...", "555", "options")
#out = functionTest("x","dist","hist","...", "555", "options")

# check the output
list.args = out[1:2]
list.ellipsis = out[3:length(out)]
print(paste("list.args = ",list.args))
print(paste("list.ellipsis = ",list.ellipsis))

# compare list des arguments avec "..."  a
expect_equal( length(intersect(list.ellipsis,list.args)) ,0)

})
 
