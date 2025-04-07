################################################################################
# The convex hull finder identifies the vertices of the convex hull for the
# carpet data
################################################################################
convex_hull_finder_carpet <- function(d) {
  
library(lpSolveAPI)
library(lpSolve)
# create a table to save iteration results
(tmp <- data.frame(matrix(NA,nrow=nrow(d),ncol=2)))
(names(tmp) <- c("DataRow","Value"))
tmp

# create a data.frame that has all the non-negativity constraints I need
# Create a 150 x 150 identity matrix
identity_matrix <- diag(1, nrow(d), nrow(d))

# Solve an optimization n different times where at each iteration you're
# (1) changing the objective function and minimizing w(i)
# (2) changing the RHS values to the values for row (i)
for (i in 1:nrow(d)){
  #i=1
  # Create a new lpSolve linear program model object.
  lps.model <- make.lp(nrow=0, ncol=nrow(d))
  # set objective function
  lp.control(lps.model, sense="min")
  # formulate coefficients into objective function
  coefs <- rep(0,nrow(d))
  (coefs[[i]] <- 1)
  set.objfn(lps.model, obj=coefs*0)
  # define constraints
  add.constraint(lps.model, c(d[,1]), "=", d[i,1])    #column 1 constraint
  add.constraint(lps.model, c(d[,2]), "=", d[i,2])    #column 2 constraint
  add.constraint(lps.model, c(d[,3]), "=", d[i,3])    #column 3 constraint
  add.constraint(lps.model, c(rep(1,nrow(d))), "=", 1)  #weights equal to 1
  # non-negativity constraints
  for (j in 1:nrow(d)){
    add.constraint(lps.model, identity_matrix[j,], ">=", 0) #non-negativity
    
  }
  wi = rep(0,nrow(d)); wi[i] = 1 
  add.constraint(lps.model, wi, "=", 0)                # wi = 0
  # review and solve                            
  lps.model # lets review our LP model
  solve(lps.model) # solve the model
  get.objective(lps.model) # optimal obj. value 
  get.variables(lps.model) # optimal solution of d.v.'s
  # save results
  tmp[i,1] <- i # this needs to iterate for each row number
  tmp[i,2] <- get.objective(lps.model)
  tmp
}
# vertices
return(tmp)
}
