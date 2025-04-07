################################################################################
# The convex hull finder identifies the vertices of the convex hull for an
# n-dimensional problem
#
# We want to identify the extreme/corner points of the convex hull of the data 
# set.This requires that we solve one LP problem for each record in the data.
# Data Example:
# x1 x2 x3
# 8  5 10
# 2  6  9
# 5  7  7
# 5  1 10
# 7  9  3
# Consider record #1, where (X1, X2, X3) = (8, 5, 10). 
# We want to determine if this vector can be expressed as a convex combination 
# of the remaining records. To do this, we solve the following problem:
# Min w1
# Subject to
# 8w1 + 2w2 + 5w3 + 5w4 + 7w5 = 8   (the RHS is the value of X1 for row 1)
# 5w1 + 8w2 + 6w3 + 1w4 + 9w5 = 5    (the RHS is the value of X2 for row 1)
# 10w1 + 9w2 + 7w3 + 10w4 + 3w5 = 10 (the RHS is the value of X3 for row 1)
# w1 + w2 + w3 + w4 + w5 = 1 (weights must sum to 1)
# wi >=0  for all i
#
# Note that w(i) indicates the weight that is assigned to record i in the data 
# set (where 0 < wi < 1).
# Also note that there will always be a feasible solution to the above problem 
# (where w1=1 & the other wi =0). However, our objective is to not use 
# (minimize the value of) w1. Thus, if (and only if) the optimal solution to the
# above problem has an objective function value of zero (i.e., w1=0) it must be 
# possible to express record 1 (i.e., the record (8, 5, 10)) as a convex 
# combination of the remaining records. In that case, record 1 is NOT an 
# extreme point of the convex hull of the dataset. On the other hand, if the 
# optimal objective function value to the above problem is greater than zero
# (and in this case it will most likely (and maybe always?) be 1), then we can 
# conclude that record 1 is an extreme point of the convex hull of the data set.
# You would repeat this analysis for each record in the data set, changing the 
# objective function and the RHS values of the constraints for each record.
################################################################################
# Example
x1 = c(8,2,5,5,7)
x2 = c(5,8,6,1,9)
x3 = c(10,9,7,10,3)
d = data.frame(x1,x2,x3)
# x1 x2 x3
# 8  5 10
# 2  6  9
# 5  7  7
# 5  1 10
# 7  9  3
################################################################################
library(lpSolveAPI)
library(lpSolve)
# create a table to save iteration results
(results <- data.frame(matrix(NA,nrow=nrow(d),ncol=2)))
(names(results) <- c("DataRow","Value"))
results

# Solve an optimization n different times where each at each iteration you're
# (1) changing the objective function and minimizing w(i)
# (2) changing the RHS values to the values for row (i)
for (i in 1:nrow(d)){
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
add.constraint(lps.model, c(rep(1,nrow(d))), "=", 1) #weights equal to 1
add.constraint(lps.model, c(1,0,0,0,0), ">=", 0)     #non-negativity
add.constraint(lps.model, c(0,1,0,0,0), ">=", 0)     #non-negativity
add.constraint(lps.model, c(0,0,1,0,0), ">=", 0)     #non-negativity
add.constraint(lps.model, c(0,0,0,1,0), ">=", 0)     #non-negativity
add.constraint(lps.model, c(0,0,0,0,1), ">=", 0)     #non-negativity
wi = rep(0,nrow(d)); wi[i] = 1 
add.constraint(lps.model, wi, "=", 0)                # wi = 0
# review and solve                            
lps.model # lets review our LP model
solve(lps.model) # solve the model
get.objective(lps.model) # optimal obj. value 
get.variables(lps.model) # optimal solution of d.v.'s
# save results
results[i,1] <- i # this needs to iterate for each row number
results[i,2] <- get.objective(lps.model)
results
}

results
# show vertices
results[results$Value != 0,]
################################################################################
# Visualize results
names(d)
summary(d)

par(mfrow=c(1,1))
## FIGURE 1 - just show 3d visual of points
library(scatterplot3d)
?scatterplot3d
plot3d <- scatterplot3d(x = d$x1, y = d$x2, z = d$x3
                        , xlab="x1", ylab="x2", zlab="x3"
                        , color = "black", bg = "black"
                        , xlim=c(2,8), ylim=c(1,9), zlim=c(3,10)
                        , angle=55, scale.y=0.7
                        , cex.symbols=1.6, pch=21
                        , box=F, col.grid="grey", lty.grid=par("lty")
)
summary(d)
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
#source('addgrids3d.r')
addgrids3d(d[,c(1,2,3)], grid=c("xy", "xz", "yz")
           , xlim=c(2,8), ylim=c(1,9), zlim=c(3,10)
           , angle = 55, scale.y=0.7)

par(mfrow=c(1,1))
## FIGURE 2 - show 3d visual of points but color vertices
library(scatterplot3d)
?scatterplot3d
plot3d <- scatterplot3d(x = d$x1, y = d$x2, z = d$x3
                        , xlab="x1", ylab="x2", zlab="x3"
                        , color = "black"
                        , bg = ifelse(results$Value>0,"red","black")
                        , xlim=c(2,8), ylim=c(1,9), zlim=c(3,10)
                        #, bg="black"
                        , angle=55, scale.y=0.7
                        , cex.symbols=1.6, pch=21
                        , box=F, col.grid="grey", lty.grid=par("lty")
)
summary(d)
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
#source('addgrids3d.r')
addgrids3d(d[,c(1,2,3)], grid=c("xy", "xz", "yz")
           , xlim=c(2,8), ylim=c(1,9), zlim=c(3,10)
           , angle = 55, scale.y=0.7)

