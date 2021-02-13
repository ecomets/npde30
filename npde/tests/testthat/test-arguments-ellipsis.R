# ------------------------------------------------------------------------------------
# from ECO : croiser une liste d'arguments donnés et passer ... avec des arguments enlevés
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
 


