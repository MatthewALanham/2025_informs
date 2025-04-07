################################################################################
# Predictive to Prescriptive Analytics
# Matthew A. Lanham
# Section 1. Example ideas and graphs
################################################################################
################################################################################
#               Section 1. Example ideas and graphs
################################################################################
# This example dataset demonstrates the idea of prediction
# and optimization to motivate future considerations
################################################################################
x1 <- c(2,4,4,5,7,7,9,8,10)
x2 <- c(4,8,2,5,7,3,6,10,10)
y <- c(12.5,13.3,16.9,16.4,17.8,23.7,27.8,35,35.1)
d <- data.frame(cbind(y,x1,x2))
rm(x1,x2,y)
cost_parameters = c(10,20)
cost_constraint_param = 225
quality_constraint_param = 29
################################################################################
# Visualize relationship
################################################################################
#3d visual of problem
names(d)
par(mfrow=c(1,1))
library(scatterplot3d)
plot3d <- scatterplot3d(x = d$x1, y = d$x2, z = d$y
                        , xlab="x1", ylab="x2", zlab="y"
                        , xlim=c(0,12), ylim=c(0,12), zlim=c(0,40)
                        , bg="black", color="white", angle=65, scale.y=0.7
                        , cex.symbols=1.6, pch=16
                        , box=F, col.grid="grey", lty.grid=par("lty"))
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
addgrids3d(d[,c(3,2,1)], grid=c("xy", "xz", "yz")
           , angle = 65, xlim=c(0,12), ylim=c(0,12), zlim=c(0,40)
           , scale.y=0.7)
# put points back on top of grid
plot3d$points3d(x=d$x1, y=d$x2, z=d$y, type="p", pch=16, cex=1, col="black")
# fit a multiple linear regression
f <- lm(y ~ x1 + x2, data=d)
summary(f)
# show output nicely
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(jtools)
summ(f, pvals = T, confint=T)
# fit a multiple linear regression WITH interaction terms
fint <- lm(y ~ x1 + x2 + I(x1*x2), data=d)
summary(fint)
# add the fitted regression plane to the 3d-plot
plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=T)
################################################################################
# set up visual for optimization points
################################################################################
#3d visual of problem
par(mfrow=c(1,1))
library(scatterplot3d)
plot3d <- scatterplot3d(x = d$x1, y = d$x2, z = d$y
                        , xlab="x1", ylab="x2", zlab="y"
                        , xlim=c(0,22), ylim=c(0,22), zlim=c(0,60)
                        , bg="black", color="white", angle=75, scale.y=0.7
                        , cex.symbols=1.6, pch=16
                        , box=F, col.grid="grey", lty.grid=par("lty"))
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
addgrids3d(d[,c(3,2,1)], grid=c("xy", "xz", "yz")
           , angle = 75, xlim=c(0,22), ylim=c(0,22), zlim=c(0,60)
           , scale.y=0.7)
# put points back on top of grid
plot3d$points3d(x=d$x1, y=d$x2, z=d$y, type="p", pch=16, cex=1, col="black")
# fit a multiple linear regression
f <- lm(y ~ x1 + x2, data=d)
summary(f)
# add the fitted regression plane to the 3d-plot
plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=F)
# add the quality constraint plane to the 3d-plot
f2 <- f
f2
str(f2$coefficients)
f2$coefficients[["x1"]] <- 0
f2$coefficients[["x2"]] <- 0
f2$coefficients[["(Intercept)"]] <- quality_constraint_param
f2$coefficients
plot3d$plane3d(f2, lty.box="solid", col="black", draw_lines=F, draw_polygon=F)

################################################################################
# maximize quality
################################################################################
library(lpSolveAPI)
library(lpSolve)
# we'll just start with 0 constraints and add them later
# there are two decision variables.
(lps.model <- make.lp(nrow=0, ncol=3))
set.type(lps.model, columns=1, type="real") # decision variable is "real" number
set.type(lps.model, columns=2, type="real") # decision variable is "real" number
set.type(lps.model, columns=3, type="real") # decision variable is "real" number
# set objective function
lp.control(lps.model, sense="max")
set.objfn(lps.model, obj=c(f$coefficients[["(Intercept)"]]
                           ,f$coefficients[["x1"]]
                           ,f$coefficients[["x2"]]))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]])
               , ">=", quality_constraint_param)
add.constraint(lps.model, c(0,cost_parameters), "<=", cost_constraint_param)
add.constraint(lps.model, c(1,0,0), "=", 1) #makes first d.v. 1 (this is to keep
#intercept term in)
# (Optional - can provide some constrain names - not necessary)
dimnames(lps.model) <- list(c("quality constraint", "cost constraint"
                              ,"intercept constraint"),
                            c("intercept scalar", "# x1", "# x2"))

# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value (i.e. our maximum profit)
get.variables(lps.model) # optimal soln of d.v.'s (i.e. our decisions to make)

# save results
(results <- data.frame(matrix(NA,nrow=1,ncol=5)))
(names(results) <- c("Model","x1","x2","Exp[y]","Exp[Cost]"))
results$Model <- "Max[Quality]"
results$x1 <- get.variables(lps.model)[[2]] 
results$x2 <- get.variables(lps.model)[[3]] 
results$"Exp[y]" <- get.objective(lps.model)
results$"Exp[Cost]" <- sum(c(0,cost_parameters)*get.variables(lps.model))
results

# add solution points
plot3d$points3d(x=get.variables(lps.model)[[2]]                    #x1 decision
                , y=get.variables(lps.model)[[3]]                  #x2 decision
                , z=sum(f$coefficients * get.variables(lps.model)) #total quality
                , type="p", pch=16, cex=1, col="red")
pts <- data.frame(x=get.variables(lps.model)[[2]]+3
                  , y=get.variables(lps.model)[[3]]
                  , z=sum(f$coefficients * get.variables(lps.model)))
text(plot3d$xyz.convert(pts), labels="max[quality]",cex=.7, col="red")

################################################################################
# maximize quality w/ min-max constraints
################################################################################
library(lpSolveAPI)
library(lpSolve)
# we'll just start with 0 constraints and add them later
# there are two decision variables.
(lps.model <- make.lp(nrow=0, ncol=3))
set.type(lps.model, columns=1, type="real") # decision variable is "real" number
set.type(lps.model, columns=2, type="real") # decision variable is "real" number
set.type(lps.model, columns=3, type="real") # decision variable is "real" number
get.type(lps.model) # to see what types are defined for each D.V.
# set objective function
lp.control(lps.model, sense="max")
set.objfn(lps.model, obj=c(f$coefficients[["(Intercept)"]]
                           ,f$coefficients[["x1"]],f$coefficients[["x2"]]))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]])
               , ">=", quality_constraint_param)
add.constraint(lps.model, c(0,cost_parameters), "<=", cost_constraint_param)
add.constraint(lps.model, c(1,0,0), "=", 1) #makes first d.v. 1 (this is to 
#keep intercept term in)
add.constraint(lps.model, c(0,1,0), "<=", max(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,1,0), ">=", min(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,0,1), "<=", max(d$x2)) #min-max for x2
add.constraint(lps.model, c(0,0,1), ">=", min(d$x2)) #min-max for x2
# in the lpSolve linear program model object.
dimnames(lps.model) <- list(c("quality constraint", "cost constraint"
                              ,"intercept constraint","min x1 constr."
                              ,"max x1 constr.","min x2 constr.","max x2 constr."),
                            c("intercept scalar", "# x1", "# x2"))

# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value (i.e. our maximum profit)
get.variables(lps.model) # optimal solution of d.v.'s (our decisions to make)

# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=5)))
(names(results2) <- c("Model","x1","x2","Exp[y]","Exp[Cost]"))
results2$Model <- "Max[Quality] + Min-Max Constraints"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$"Exp[y]" <- get.objective(lps.model)
results2$"Exp[Cost]" <- sum(c(0,cost_parameters)*get.variables(lps.model))
results2
(results <- rbind(results,results2))

# add solution points
plot3d$points3d(x=get.variables(lps.model)[[2]]                    #x1 decision
                , y=get.variables(lps.model)[[3]]                  #x2 decision
                , z=sum(f$coefficients * get.variables(lps.model)) #total quality
                , type="p", pch=16, cex=1, col="blue")
pts <- data.frame(x=get.variables(lps.model)[[2]]+4
                  , y=get.variables(lps.model)[[3]]
                  , z=sum(f$coefficients * get.variables(lps.model)))
text(plot3d$xyz.convert(pts), labels="max[quality] + min-max",cex=.7, col="blue")

results

################################################################################
# Find vertices of convex hull
################################################################################
library(tidyverse)
(hull <- chull(d))
(hull <- d %>%
    slice(chull(x1, x2)))
(hull <- hull[,2:3])
(num_vertex_constr <- nrow(hull))

################################################################################
# max quality + convex-hull constraints
################################################################################
# there are two decision variables. Make them "real"
(lps.model <- make.lp(nrow=0, ncol=3+num_vertex_constr))
for (i in 1:(3+num_vertex_constr)){
  set.type(lps.model, columns=i, type="real")
}
get.type(lps.model) # to see what types are defined for each D.V.
# set objective function
lp.control(lps.model, sense="max")
set.objfn(lps.model, obj=c(f$coefficients[["(Intercept)"]]
                           ,f$coefficients[["x1"]],f$coefficients[["x2"]]
                           ,rep(0,num_vertex_constr)))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]]
                            ,rep(0,num_vertex_constr))
               , ">=", quality_constraint_param)
add.constraint(lps.model, c(0,cost_parameters,rep(0,num_vertex_constr))
               , "<=", cost_constraint_param)
# makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0,rep(0,num_vertex_constr)), "=", 1)
add.constraint(lps.model, c(0,1,0,rep(0,num_vertex_constr)), "<=", max(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,1,0,rep(0,num_vertex_constr)), ">=", min(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,0,1,rep(0,num_vertex_constr)), "<=", max(d$x2)) #min-max for x2
add.constraint(lps.model, c(0,0,1,rep(0,num_vertex_constr)), ">=", min(d$x2)) #min-max for x2
add.constraint(lps.model, c(0,-1,0,hull[,1]), "=", 0) #convex hull x1s = dv1
add.constraint(lps.model, c(0,0,-1,hull[,2]), "=", 0) #convex hull x2s = dv2
add.constraint(lps.model, c(0,0,0,rep(1,num_vertex_constr)), "=", 1) #sum of weights = 1

# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value (i.e. our maximum profit)
get.variables(lps.model) # optimal solution of d.v.'s (our decisions to make)

# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=5)))
(names(results2) <- c("Model","x1","x2","Exp[y]","Exp[Cost]"))
results2$Model <- "Max[Quality] + Convex Hull Constraints"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$"Exp[y]" <- get.objective(lps.model)
results2$"Exp[Cost]" <- sum(c(0,cost_parameters)*get.variables(lps.model))
results2
(results <- rbind(results,results2))

# add solution points
plot3d$points3d(x=get.variables(lps.model)[[2]]                    #x1 decision
                , y=get.variables(lps.model)[[3]]                  #x2 decision
                , z=sum(f$coefficients * get.variables(lps.model)) #total quality
                , type="p", pch=19, cex=1, col="darkgreen")
pts <- data.frame(x=get.variables(lps.model)[[2]]-3.5
                  , y=get.variables(lps.model)[[3]]
                  , z=sum(f$coefficients * get.variables(lps.model)))+1
text(plot3d$xyz.convert(pts), labels="max[quality] + convex",cex=.7, col="darkgreen")

results

################################################################################
# minimize cost
################################################################################
# there are two decision variables.
(lps.model <- make.lp(nrow=0, ncol=3))
set.type(lps.model, columns=1, type="real") # decision variable is "real" number
set.type(lps.model, columns=2, type="real") # decision variable is "real" number
set.type(lps.model, columns=3, type="real") # decision variable is "real" number
get.type(lps.model) # to see what types are defined for each D.V.
# set objective function
lp.control(lps.model, sense="min")
set.objfn(lps.model, obj=c(0,10,20))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]])
               , ">=", quality_constraint_param)
add.constraint(lps.model, c(0,cost_parameters), "<=", cost_constraint_param)
#makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0), "=", 1)
# in the lpSolve linear program model object.
dimnames(lps.model) <- list(c("quality constraint", "cost constraint"
                              ,"intercept constraint"),
                            c("intercept scalar", "# x1", "# x2"))
# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value (i.e. our maximum profit)
get.variables(lps.model) # optimal solution of d.v.'s (our decisions to make)

# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=5)))
(names(results2) <- c("Model","x1","x2","Exp[y]","Exp[Cost]"))
results2$Model <- "Min[Cost]"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$"Exp[y]" <- sum(c(f$coefficients[["(Intercept)"]],f$coefficients[["x1"]],f$coefficients[["x2"]])*get.variables(lps.model))
results2$"Exp[Cost]" <- sum(c(0,cost_parameters)*get.variables(lps.model))
results2
(results <- rbind(results,results2))

# add solution points
plot3d$points3d(x=get.variables(lps.model)[[2]]                    #x1 decision
                , y=get.variables(lps.model)[[3]]                  #x2 decision
                , z=sum(f$coefficients * get.variables(lps.model)) #total quality
                , type="p", pch=16, cex=1, col="red")
pts <- data.frame(x=get.variables(lps.model)[[2]]+2
                  , y=get.variables(lps.model)[[3]]-2
                  , z=sum(f$coefficients * get.variables(lps.model)))
text(plot3d$xyz.convert(pts), labels="min[cost]",cex=.7, col="red")

results

################################################################################
# minimize cost + min-max constraints
################################################################################
# there are two decision variables.
(lps.model <- make.lp(nrow=0, ncol=3))
set.type(lps.model, columns=1, type="real") # decision variable is "real" number
set.type(lps.model, columns=2, type="real") # decision variable is "real" number
set.type(lps.model, columns=3, type="real") # decision variable is "real" number
get.type(lps.model) # to see what types are defined for each D.V.
# set objective function
lp.control(lps.model, sense="min")
set.objfn(lps.model, obj=c(0,cost_parameters))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]])
               , ">=", quality_constraint_param)
add.constraint(lps.model, c(0,cost_parameters), "<=", cost_constraint_param)
#makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0), "=", 1) 
add.constraint(lps.model, c(0,1,0), "<=", max(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,1,0), ">=", min(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,0,1), "<=", max(d$x2)) #min-max for x2
add.constraint(lps.model, c(0,0,1), ">=", min(d$x2)) #min-max for x2
# in the lpSolve linear program model object.
dimnames(lps.model) <- list(c("quality constraint", "cost constraint"
                              ,"intercept constraint","min x1 constr."
                              ,"max x1 constr.","min x2 constr."
                              ,"max x2 constr."),
                            c("intercept scalar", "# x1", "# x2"))
# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value (i.e. our maximum profit)
get.variables(lps.model) # optimal solution of d.v.'s (our decisions to make)

# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=5)))
(names(results2) <- c("Model","x1","x2","Exp[y]","Exp[Cost]"))
results2$Model <- "Min[Cost] + Min-Max Constraints"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$"Exp[y]" <- sum(c(f$coefficients[["(Intercept)"]],f$coefficients[["x1"]],f$coefficients[["x2"]])*get.variables(lps.model))
results2$"Exp[Cost]" <- sum(c(0,cost_parameters)*get.variables(lps.model))
results2
(results <- rbind(results,results2))

# add solution points
plot3d$points3d(x=get.variables(lps.model)[[2]]                    #x1 decision
                , y=get.variables(lps.model)[[3]]                  #x2 decision
                , z=sum(f$coefficients * get.variables(lps.model)) #total quality
                , type="p", pch=16, cex=1, col="blue")
pts <- data.frame(x=get.variables(lps.model)[[2]]+3
                  , y=get.variables(lps.model)[[3]]
                  , z=sum(f$coefficients * get.variables(lps.model)))
text(plot3d$xyz.convert(pts), labels="min[cost] + min-max",cex=.7, col="blue")

results

################################################################################
# minimize cost + convex-hull constraints
################################################################################
# there are two "real" decision variables.
(lps.model <- make.lp(nrow=0, ncol=3+num_vertex_constr))
for (i in 1:(3+num_vertex_constr)){
  set.type(lps.model, columns=i, type="real") 
}
get.type(lps.model) # to see what types are defined for each D.V.
# set objective function
lp.control(lps.model, sense="min")
set.objfn(lps.model, obj=c(0,cost_parameters,rep(0,num_vertex_constr)))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]]
                            ,rep(0,num_vertex_constr))
               , ">=", quality_constraint_param)
add.constraint(lps.model, c(0,cost_parameters,rep(0,num_vertex_constr))
               , "<=", cost_constraint_param)
#makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0,rep(0,num_vertex_constr)), "=", 1)
add.constraint(lps.model, c(0,-1,0,hull[,1]), "=", 0) #convex hull x1s = dv1
add.constraint(lps.model, c(0,0,-1,hull[,2]), "=", 0) #convex hull x2s = dv2
add.constraint(lps.model, c(0,0,0,rep(1,num_vertex_constr)), "=", 1) #sum weights=1

lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value (i.e. our maximum profit)
get.variables(lps.model) # optimal solution of d.v.'s (our decisions to make)

# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=5)))
(names(results2) <- c("Model","x1","x2","Exp[y]","Exp[Cost]"))
results2$Model <- "Min[Cost] + Convex Hull Constraints"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$"Exp[y]" <- sum(c(f$coefficients[["(Intercept)"]],f$coefficients[["x1"]],f$coefficients[["x2"]])*get.variables(lps.model)[1:3])
results2$"Exp[Cost]" <- get.objective(lps.model)
results2
(results <- rbind(results,results2))

# add solution points
plot3d$points3d(x=get.variables(lps.model)[[2]]                    #x1 decision
                , y=get.variables(lps.model)[[3]]                  #x2 decision
                , z=sum(f$coefficients * get.variables(lps.model)) #total quality
                , type="p", pch=19, cex=1, col="darkgreen")
pts <- data.frame(x=get.variables(lps.model)[[2]]-2
                  , y=get.variables(lps.model)[[3]]
                  , z=sum(f$coefficients * get.variables(lps.model)))-1
text(plot3d$xyz.convert(pts), labels="min[cost] + convex",cex=.7, col="darkgreen")

results

################################################################################
# Visualize the convex hull
################################################################################
# show how max quality moves with min-max constraints
library(tidyverse)
hull <- chull(d)
hull <- d %>%
  slice(chull(x1, x2))
hull

library(ggplot2)
g <- ggplot(data=d, aes(x=x1, y=x2, z=y)) + geom_point(size=2)
#g <- g + geom_polygon(data=hull, alpha=0, linetype="dashed",colour="red")
g <- g + geom_point(aes(x=results[1,2],y=results[1,3]),colour="red", size=3)
g <- g + geom_point(aes(x=results[2,2],y=results[2,3]),colour="blue", size=3)
g <- g + geom_vline(xintercept=min(d$x1), linetype="dashed", color = "red")
g <- g + geom_vline(xintercept=max(d$x1), linetype="dashed", color = "red")
g <- g + geom_hline(yintercept=min(d$x2), linetype="dashed", color = "red")
g <- g + geom_hline(yintercept=max(d$x2), linetype="dashed", color = "red")
g <- g + geom_segment(aes(x=results[1,2]
                          ,y=results[1,3]
                          ,xend=results[2,2]+.1
                          ,yend=results[2,3]-.01)
                      ,lineend = "round",size = .8
                      ,arrow=arrow(length=unit(0.25,"cm"),type="closed",))
g <- g + annotate("text", x=c(22.5,10), y=c(0.5,6.75), label=c("A","B"))
g
results

# show how one point can drastically increase the min-max area
library(tidyverse)
hull <- chull(d)
hull <- d %>%
  slice(chull(x1, x2))
hull

(d3 <- d)
d3[7,] <- c(27.8,18,6)

hull2 <- chull(d3)
hull2 <- d3 %>%
  slice(chull(x1, x2))
hull2

library(ggplot2)
g <- ggplot(data=d, aes(x=x1, y=x2, z=y)) + geom_point(size=2)
# original hull
g <- g + geom_polygon(data=hull, alpha=.35, linetype="dashed",colour="red")
# add far away point
g <- g + geom_point(aes(x=hull2[1,2],y=hull2[1,3]),colour="black", size=2)
# make a larger hull from one point moving
g <- g + geom_polygon(data=hull2, alpha=.35, linetype="dashed",colour="red")

g <- g + geom_point(aes(x=results[1,2],y=results[1,3]),colour="red", size=3)
g <- g + geom_point(aes(x=results[2,2],y=results[2,3]),colour="blue", size=3)
g
g <- g + geom_vline(xintercept=min(d3$x1), linetype="dashed", color = "red")
g
g <- g + geom_vline(xintercept=max(d3$x1), linetype="dashed", color = "red")
g
g <- g + geom_hline(yintercept=min(d3$x2), linetype="dashed", color = "red")
g
g <- g + geom_hline(yintercept=max(d3$x2), linetype="dashed", color = "red")
g
g <- g + geom_segment(aes(x=hull[1,2]
                          ,y=hull[1,3]
                          ,xend=hull2[1,2]
                          ,yend=hull2[1,3])
                      ,lineend = "round",size = .8
                      ,arrow=arrow(length=unit(0.25,"cm"),type="closed",))
g <- g + annotate("text", x=c(8.6,18.6), y=c(6,6), label=c("C","D"))
g

# show the final path for quality
results
library(ggplot2)
g <- ggplot(data=d, aes(x=x1, y=x2, z=y)) + geom_point(size=2)
g <- g + geom_polygon(data=hull, alpha=0.35, linetype="dashed",colour="red")
g <- g + geom_point(aes(x=results[1,2],y=results[1,3]),colour="red", size=3)
g <- g + geom_point(aes(x=results[2,2],y=results[2,3]),colour="blue", size=3)
g <- g + geom_point(aes(x=results[3,2],y=results[3,3]),colour="darkgreen", size=3)
g <- g + geom_vline(xintercept=min(d$x1), linetype="dashed", color = "red")
g <- g + geom_vline(xintercept=max(d$x1), linetype="dashed", color = "red")
g <- g + geom_hline(yintercept=min(d$x2), linetype="dashed", color = "red")
g <- g + geom_hline(yintercept=max(d$x2), linetype="dashed", color = "red")
g <- g + geom_segment(aes(x=results[1,2]
                          ,y=results[1,3]
                          ,xend=results[2,2]
                          ,yend=results[2,3])
                      ,lineend = "round",size = .8
                      ,arrow=arrow(length=unit(0.25,"cm"),type="closed",))
g <- g + geom_segment(aes(x=results[2,2]
                          ,y=results[2,3]
                          ,xend=results[3,2]
                          ,yend=results[3,3])
                      ,lineend = "round",size = .8
                      ,arrow=arrow(length=unit(0.25,"cm"),type="closed",))
g <- g + annotate("text", x=c(22.5,10,9.16), y=c(.5,6.75,7.16), label=c("A","B","C"))
g <- g + annotate("text", x=c(20, 13, 12.5), y=c(0, 6.25, 7.1)
                  , label=c("No constraints"
                            ,"Min-max constraints"
                            ,"Convex hull constraints"))
g
results


# FINAL FIGURE
library(ggplot2)
g <- ggplot(data=d, aes(x=x1, y=x2, z=y)) + geom_point(size=2)
g <- g + geom_polygon(data=hull, alpha=0.35, linetype="dashed",colour="red")
g <- g + geom_point(aes(x=results[1,2],y=results[1,3]),colour="red", size=3)
g <- g + geom_point(aes(x=results[2,2],y=results[2,3]),colour="blue", size=3)
g <- g + geom_point(aes(x=results[3,2],y=results[3,3]),colour="darkgreen", size=3)
g <- g + geom_point(aes(x=results[4,2],y=results[4,3]),colour="red", size=3)
g <- g + geom_point(aes(x=results[5,2],y=results[5,3]),colour="blue", size=3)
g <- g + geom_point(aes(x=results[6,2],y=results[6,3]),colour="darkgreen", size=3)
g <- g + geom_vline(xintercept=min(d$x1), linetype="dashed", color = "red")
g <- g + geom_vline(xintercept=max(d$x1), linetype="dashed", color = "red")
g <- g + geom_hline(yintercept=min(d$x2), linetype="dashed", color = "red")
g <- g + geom_hline(yintercept=max(d$x2), linetype="dashed", color = "red")
g <- g + geom_segment(aes(x=results[1,2]
                          ,y=results[1,3]
                          ,xend=results[2,2]+.1
                          ,yend=results[2,3]-.01)
                      ,lineend = "round",size = .8
                      ,arrow=arrow(length=unit(0.25,"cm"),type="closed",))
g <- g + geom_segment(aes(x=results[2,2]
                          ,y=results[2,3]
                          ,xend=results[3,2]+.1
                          ,yend=results[3,3]-.01)
                      ,lineend = "round",size = .8
                      ,arrow=arrow(length=unit(0.25,"cm"),type="closed",))
g <- g + annotate("text", x=c(22.5,10,9.16), y=c(.5,6.75,7.16), label=c("A","B","C"))
g <- g + annotate("text", x=c(20, 13, 12.5), y=c(0, 6.25, 7.1)
                  , label=c("No constraints"
                            ,"Min-max constraints"
                            ,"Convex hull constraints"))
g

# add direction path for cost
g <- g + geom_segment(aes(x=results[4,2]
                          ,y=results[4,3]
                          ,xend=results[5,2]
                          ,yend=results[5,3])
                      ,lineend = "round",size = .8
                      ,arrow=arrow(length=unit(0.25,"cm"),type="closed",))
g <- g + geom_segment(aes(x=results[5,2]
                          ,y=results[5,3]
                          ,xend=results[6,2]
                          ,yend=results[6,3])
                      ,lineend = "round",size = .8
                      ,arrow=arrow(length=unit(0.25,"cm"),type="closed",))
g <- g + annotate("text", x=c(9.5,9.17,8.56), y=c(.5,2.5,6.2), label=c("D","E","F"))
g <- g + annotate("text", x=c(7.5, 7.25, 5.5), y=c(0, 1.7, 5.75)
                  , label=c("No constraints"
                            ,"Min-max constraints"
                            ,"Convex hull constraints"))
g

################################################################################
# summarize work in a nice looking table
################################################################################
# decisions (x1,x2), obj fn value, total cost
library(remotes)
#remotes::install_github("ianmoran11/mmtable2")
library(mmtable2)
library(gt)
library(reshape2)
(results2 <- melt(results))
results2$value <- round(results2$value,2)

main_table <- results2 %>%
  arrange(factor(Model, levels=c('Max[Quality]'
                                 ,'Max[Quality] + Min-Max Constraints',
                                 'Max[Quality] + Convex Hull Constraints","Min[Cost]',
                                 'Min[Cost] + Min-Max Constraints'
                                 ,'Min[Cost] + Convex Hull Constraints'))
          ,desc(variable), desc(value), value) %>% 
  mmtable(cells=value) +
  header_top(variable) + 
  header_left(Model)
main_table

################################################################################
# Predictive to Prescriptive Analytics
# Matthew A. Lanham
# Example: Carpet Manufacturing
# For the nylon carpet priority is to achieve stability greater than 80%. 
################################################################################
# clean up environment for new examples
rm(list = ls())

# read data from Excel file
library(readxl)

#setwd("C:\\Users\\lanhamm\\Desktop")
setwd("C:\\Users\\lanhamm\\Dropbox\\_Purdue\\_Teaching\\_Data Science in the Cloud\\Week 1 - Interfacing Areas of Analytics\\2_Prediction to Optimization")
list.files(getwd()) # make sure file you are attempting to read is located in your working directory
excel_sheets('carpet_manufacturing_data.xlsx')
#excel_sheets('carpet_data.xlsx')
(d <- read_xlsx("carpet_manufacturing_data.xlsx", sheet="Sheet1"
                , range="B1:F151", col_names=T))
head(d)
# Define customer specs
stability_constraint_param = 80
# Define input costs
x1.cost = 2.56
x2.cost = 2.50
x3.cost = 21.52
chem_costs <- c(x1.cost, x2.cost, x3.cost)
################################################################################
# Stability
################################################################################
# quick visuals
source("multiplot.R")
library(ggplot2)
p1 <- ggplot(d, aes(x=x1, y=y)) + geom_point()
p1 <- p1 + geom_abline(intercept = lm(y ~ x1, data=d)[[1]][[1]]
                       , slope = lm(y ~ x1, data=d)[[1]][[2]]
                       , color = "red")
p1

p2 <- ggplot(d, aes(x=x2, y=y)) + geom_point()
p2 <- p2 + geom_abline(intercept = lm(y ~ x2, data=d)[[1]][[1]]
                       , slope = lm(y ~ x2, data=d)[[1]][[2]]
                       , color = "red")
p2

p3 <- ggplot(d, aes(x=x3, y=y)) + geom_point()
p3 <- p3 + geom_abline(intercept = lm(y ~ x3, data=d)[[1]][[1]]
                       , slope = lm(y ~ x3, data=d)[[1]][[2]]
                       , color = "red")
p3
multiplot(p1, p2, p3)

# strong corr among pH and stability; slight negative cor for RM_ration and stability
(rho <- round(cor(d, method = "pearson"),2))
#x1   x2   x3    y   y2
#x1 1.00 0.59 0.63 0.87 0.70
#x2 0.59 1.00 0.72 0.89 0.85
#x3 0.63 0.72 1.00 0.84 0.96
#y  0.87 0.89 0.84 1.00 0.92
#y2 0.70 0.85 0.96 0.92 1.00

# histograms of numeric features
g1 <- ggplot(data=d, aes(x=x1)) + geom_histogram(color="white", fill="black"
                                                 , binwidth=0.25)
g1 <- g1 + labs(x ='x1', y='Count', title='x1 Distribution')
g1
g2 <- ggplot(data=d, aes(x=x2)) + geom_histogram(color="white", fill="black"
                                                 , binwidth=0.50)
g2 <- g2 + labs(x ='x2', y='Count', title='x2 Distribution')
g2
g3 <- ggplot(data=d, aes(x=x3)) + geom_histogram(color="white", fill="black"
                                                 , binwidth=0.01)
g3 <- g3 + labs(x ='x3', y='Count', title='x3 Distribution')
g3
multiplot(g1, g2, g3)
y1 <- ggplot(data=d, aes(x=y)) + geom_histogram(color="white", fill="black"
                                                , binwidth=2)
y1 <- y1 + labs(x ='y (Stability)', y='Count', title='y (Stability) Distribution')
y1
y2 <- ggplot(data=d, aes(x=y2)) + geom_histogram(color="white", fill="black"
                                                 , binwidth=0.5)
y2 <- y2 + labs(x ='y2 (Stain Resistance)', y='Count', title='y2 (Stain Resistance) Distribution')
y2
multiplot(g1, g2, g3, y1, y2)

# y is stability; y2 is stain resistance score; sort the data
(d <- d[ order(-d$y, -d$y2), ])

# target
(ggplot(d, aes(x=y)) + geom_histogram())
# 28% of time stability target is achieved
sum(ifelse(d$y >= 80,1,0))/nrow(d)
d$stability_achieved <- ifelse(d$y >= 80,1,0)
d$resistance_achieved <- ifelse(d$y2 >= 8,1,0)

# regression experiment results
results <- data.frame(matrix(nrow=0, ncol=17))
results
names(results) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2"
                    ,"px1x2","Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3"
                    ,"px1x2x3","AdjR2")
results

# get carpet model regression estimated parameters and evaluation stats
getwd()
source("run_carpet_regressions.R")
(results <- run_carpet_regressions(d=d))
#library("xlsx")
#write.xlsx(x=results, file=paste0(Sys.Date(),"_","regression_results.xlsx"))

# The "best" model was y ~ x1 + x2 + x3 check out fit
f <- lm(y ~ x1 + x2 + x3, data=d)
summary(f)
par(mfrow=c(2,2))
plot(f)

# calculate VIFs
library(car)
car::vif(f)
################################################################################
# Regression diagnostics and tests
################################################################################
# pair-wise correlations
X <- d[,1:3]
attach(d)
library(GGally)
ggpairs(X)

# partial correlations
library(corpcor)
cor2pcor(cov(X))

# Farrar-Glauber test for multicollinearity
library(mctest)
?omcdiag
?imcdiag
omcdiag(mod=f) #overall test
imcdiag(mod=f) #individual test
# examining the pattern of multicollinearity, it is required to conduct t-test 
# for correlation coefficient
library(ppcor)
pcor(X, method = "pearson")

# a function to visually check linear regression assumptions
reviewDiag <- function(lmfit) {
  # Diagnostic plots
  par(mfcol=c(2,3), fg="black", bg="white",col.lab="black")
  # cooks distance - check for influential points
  cook<-cooks.distance(lmfit) 
  library(faraway) # library needed for half-normalplot
  halfnorm(cook,3,ylab="Cooks distance", main="Influences", col="blue") 
  boxplot(cook, col="blue", ylab="Cooks distance"
          , main="Boxplot Cooks Distances")
  # constant variance
  plot(fitted(lmfit),residuals(lmfit),xlab="Fitted",ylab="Residuals", col="blue"
       , pch=19, type='p', main="Resid vs. Fitted") 
  abline(h=0) 
  plot(fitted(lmfit),abs(residuals(lmfit)),xlab="Fitted",ylab="Abs(Residuals)"
       , main="Abs(Resid) vs. Fitted", col="blue", pch=19)
  # normality
  qqnorm(residuals(lmfit),ylab="Residuals", pch=19, col="blue") 
  qqline(residuals(lmfit)) 
  hist(residuals(lmfit), col="blue", main="Historgram of Residuals")
}
## Regression diagnostics (influential points and outliers)
reviewDiag(f)

library(olsrr)
# A data point having a large cook’s d indicates that the data point strongly 
# influences the fitted values.
par(mfrow=c(1,2))
ols_plot_cooksd_bar(f)
ols_plot_cooksd_chart(f) # Cooks D Chart
ols_plot_dfbetas(f) # DFBETAs Panel
ols_plot_dffits(f) # DFFITS Plot
# Checking for outliers
ols_plot_resid_stud(f) # studentized Residual Plot
ols_plot_resid_stand(f) #Standardized Residual Chart
ols_plot_resid_stud_fit(f) #Deleted Studentized Residual vs Fitted Values Plot
# Checking for influential observations
ols_plot_resid_lev(f) # Studentized Residuals vs Leverage Plot
ols_plot_hadi(f)
# Plot to aid in classifying unusual observations as high-leverage points, outliers, or a combination of both.
ols_plot_resid_pot(f) # Potential Residual Plot

# Check for Heteroscedasticity
# Ho: the variance is constant   
ols_test_breusch_pagan(f) 
ols_test_breusch_pagan(f, rhs = TRUE)
ols_test_breusch_pagan(f, rhs = TRUE, multiple = TRUE)
ols_test_score(f) # score test
ols_test_f(f) #f-test for heteroscedasticity

# check for normality
# Ho: sample distribution is normal
shapiro.test(f$residuals) #Shapiro-Wilk test
library(nortest); nortest::lillie.test(f$residuals) # Lilliefors (Kolmogorov-Smirnov) normality test
library(nortest); nortest::ad.test(f$residuals) # Anderson-Darling normality test

#clean up
rm(run_carpet_regressions, reviewDiag, g1,g2,g3,p1,p2,p3,results,rho,y1,y2)
################################################################################
# generate some visualizations showing where specificity is satisfied
################################################################################
names(d)
par(mfrow=c(1,2))

## FIGURE 1
library(scatterplot3d)
plot3d <- scatterplot3d(x = d$x1, y = d$x3, z = d$y
                        , xlab="x1", ylab="x3", zlab="y"
                        , color = ifelse(d$stability_achieved==1,"black","red")
                        , xlim=c(4,7), ylim=c(0,0.20), zlim=c(60,100)
                        #, bg="black"
                        , angle=45, scale.y=0.7
                        , cex.symbols=1.6, pch=16
                        , box=F, col.grid="grey", lty.grid=par("lty"))
summary(d)
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
#source('addgrids3d.r')
addgrids3d(d[,c(1,3,4)], grid=c("xy", "xz", "yz")
           , xlim=c(4,7), ylim=c(0,0.20), zlim=c(60,100)
           , angle = 45, scale.y=0.7)
# put points back on top of grid
plot3d$points3d(x=d$x1, y=d$x3, z=d$y, type="p", pch=16, cex=1,
                col = ifelse(d$stability_achieved==1,"black","red"))
# Trick* Have to fit a multiple linear regression so we can easily get the
# the customer spec plane on the figure
f <- lm(y ~ x1 + x3, data=d)
summary(f)
# show output nicely
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(jtools)
summ(f, pvals = T, confint=T)
f$coefficients[["x1"]] <- 0
#f$coefficients[["x2"]] <- 0
f$coefficients[["x3"]] <- 0
#names(f$coefficients)[3] <- "x2"  #need to do this so plot understands
f$coefficients[["(Intercept)"]] <- stability_constraint_param
f
# add the fitted regression plane to the 3d-plot
plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=T)

## FIGURE 2
plot3d <- scatterplot3d(x = d$x1, y = d$x2, z = d$y
                        , xlab="x1", ylab="x2", zlab="y"
                        , color = ifelse(d$stability_achieved==1,"black","red")
                        , xlim=c(4,7), ylim=c(5,8.5), zlim=c(60,100)
                        #, bg="black"
                        , angle=45, scale.y=0.7
                        , cex.symbols=1.6, pch=16
                        , box=F, col.grid="grey", lty.grid=par("lty")
)
summary(d)
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
#source('addgrids3d.r')
addgrids3d(d[,c(1,2,4)], grid=c("xy", "xz", "yz")
           , xlim=c(4,7), ylim=c(5,8.5), zlim=c(60,100)
           , angle = 45, scale.y=0.7)
# put points back on top of grid
plot3d$points3d(x=d$x1, y=d$x2, z=d$y, type="p", pch=16, cex=1,
                col = ifelse(d$stability_achieved==1,"black","red"))
# Trick* Have to fit a multiple linear regression so we can easily get the
# the customer spec plane on the figure
f <- lm(y ~ x1 + x2, data=d)
summary(f)
# show output nicely
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(jtools)
summ(f, pvals = T, confint=T)
f$coefficients[["x1"]] <- 0
f$coefficients[["x2"]] <- 0
#f$coefficients[["x3"]] <- 0
#names(f$coefficients)[3] <- "x2"  #need to do this so plot understands
f$coefficients[["(Intercept)"]] <- stability_constraint_param
f
# add the fitted regression plane to the 3d-plot
plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=T)

################################################################################
# clean up environment for new examples
rm(list = ls())

# read data from Excel file
library(readxl)
setwd("C:\\Users\\lanhamm\\Dropbox\\_Purdue\\_Teaching\\_Data Science in the Cloud\\Week 1 - Interfacing Areas of Analytics\\2_Prediction to Optimization")
list.files(getwd()) # make sure file you are attempting to read is located in your working directory
excel_sheets('carpet_manufacturing_data.xlsx')
(d <- read_xlsx("carpet_manufacturing_data.xlsx", sheet="Sheet1"
                , range="B1:F151", col_names=T))
head(d)

# Define customer specs
stability_constraint_param = 80
# Define input costs
x1.cost = 2.56
x2.cost = 2.50
x3.cost = 21.52
chem_costs <- c(x1.cost, x2.cost, x3.cost)

# multiplot function
source("multiplot.R")
library(ggplot2)

################################################################################
# Maximize Stability (y)
################################################################################
f <- lm(y ~ x1 + x2 + x3, data=d)
summary(f)

library(lpSolveAPI)
library(lpSolve)
# Create a new lpSolve linear program model object.
(lps.model <- make.lp(nrow=0, ncol=4))

# what kind of decision variables do you have? "real", "integer", or "binary"?
set.type(lps.model, columns=1, type="real")
set.type(lps.model, columns=2, type="real")
set.type(lps.model, columns=3, type="real")
set.type(lps.model, columns=4, type="real")
get.type(lps.model) # to see what types are defined for each D.V.
# Give our model a name
name.lp(lps.model, name="Stability")
# set objective function
lp.control(lps.model, sense="max")
# coefficients from linear model
summary(f)$coefficients
# formulate coefficients into objective function
set.objfn(lps.model, obj=c(f$coefficients[["(Intercept)"]]
                           ,f$coefficients[["x1"]],f$coefficients[["x2"]]
                           ,f$coefficients[["x3"]]))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]]
                            ,f$coefficients[["x3"]])
               , ">=", stability_constraint_param)
#makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0,0), "=", 1)
#budgetary constraint
add.constraint(lps.model, c(0,x1.cost,x2.cost,x3.cost), "<=", 1000)
# lets review our LP model
lps.model 
# in the lpSolve linear program model object.
dimnames(lps.model) <- list(c("stability constraint", "intercept constraint"
                              ,"budget"),
                            c("intercept scalar", "# x1","# x2","# x3"))
# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value 
get.variables(lps.model) # optimal solution of d.v.'s (i.e. our decisions to make)
# sensitivity analysis and shadow prices
options(scipen=999) # removes scientific notation in outputs
# sensitivity of the objective function
get.sensitivity.obj(lps.model)[[1]]
get.sensitivity.obj(lps.model)[[2]]
# sensitivity of the constraints
str(get.sensitivity.rhs(lps.model))
# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=6)))
(names(results2) <- c("Model","x1","x2","x3","Exp[y]","Cost"))
results2$Model <- "Max[Stability]"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$x3 <- get.variables(lps.model)[[4]] 
results2$"Exp[y]" <- sum(c(f$coefficients[["(Intercept)"]],f$coefficients[["x1"]]
                           ,f$coefficients[["x2"]]
                           ,f$coefficients[["x3"]])*get.variables(lps.model))
results2$"Cost" <- sum(chem_costs*get.variables(lps.model)[2:4])
results2

(results <- results2)
################################################################################
# Maximize Stability with min-max constraints
################################################################################
library(lpSolveAPI)
library(lpSolve)
# Create a new lpSolve linear program model object.
(lps.model <- make.lp(nrow=0, ncol=4))
# what kind of decision variables do you have? "real", "integer", or "binary"?
set.type(lps.model, columns=1, type="real")
set.type(lps.model, columns=2, type="real")
set.type(lps.model, columns=3, type="real")
set.type(lps.model, columns=4, type="real")
get.type(lps.model) # to see what types are defined for each D.V.
# Give our model a name
name.lp(lps.model, name="Stability")
# set objective function
lp.control(lps.model, sense="max")
# coefficients from linear model
summary(f)$coefficients
# formulate coefficients into objective function
set.objfn(lps.model, obj=c(f$coefficients[["(Intercept)"]]
                           ,f$coefficients[["x1"]],f$coefficients[["x2"]]
                           ,f$coefficients[["x3"]]))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]]
                            ,f$coefficients[["x3"]])
               , ">=", stability_constraint_param)
#makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0,0), "=", 1)
#budgetary constraint
add.constraint(lps.model, c(0,x1.cost,x2.cost,x3.cost), "<=", 1000)
add.constraint(lps.model, c(0,1,0,0), "<=", max(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,1,0,0), ">=", min(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,0,1,0), "<=", max(d$x2)) #min-max for x2
add.constraint(lps.model, c(0,0,1,0), ">=", min(d$x2)) #min-max for x2
add.constraint(lps.model, c(0,0,0,1), "<=", max(d$x3)) #min-max for x3
add.constraint(lps.model, c(0,0,0,1), ">=", min(d$x3)) #min-max for x3
# lets review our LP model
lps.model 
# in the lpSolve linear program model object.
dimnames(lps.model) <- list(c("stability constraint","intercept constraint"
                              ,"budget"
                              ,"max x1 constr.","min x1 constr."
                              ,"max x2 constr.","min x2 constr."
                              ,"max x3 constr.","min x3 constr."),
                            c("intercept scalar", "# x1", "# x2", "# x3"))

# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value 
get.variables(lps.model) # optimal solution of d.v.'s (i.e. our decisions to make)
# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=6)))
(names(results2) <- c("Model","x1","x2","x3","Exp[y]","Cost"))
results2$Model <- "Max[Stability] + Min-Max Constraints"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$x3 <- get.variables(lps.model)[[4]] 
results2$"Exp[y]" <- sum(c(f$coefficients[["(Intercept)"]],f$coefficients[["x1"]]
                           ,f$coefficients[["x2"]]
                           ,f$coefficients[["x3"]])*get.variables(lps.model))
results2$"Cost" <- sum(chem_costs*get.variables(lps.model)[2:4])
results2

(results <- rbind(results,results2))
################################################################################
# Identify convex hull vertices
################################################################################
# here is an example duplicate record
d[3,]
d[d$x1==5 & d$x2==6.5 & d$x3==.13, ]
# first we need to identify unique data points and ignore duplicate rows

#df[duplicated(d[,1:3])
deduped.data <- unique(d[,1:3])
deduped.data <- deduped.data[order(deduped.data$x1,deduped.data$x2,deduped.data$x3),]
head(deduped.data)

# d needs to be a data.frame and not a tibble
d = as.data.frame(d)
str(d)
deduped.data = as.data.frame(deduped.data)
str(deduped.data)

#identify vertices
source("convex_hull_finder_carpet.R")
tmp <- convex_hull_finder_carpet(deduped.data)
deduped.data$vertex <- tmp$Value
# show vertices
(tmp <- deduped.data[deduped.data$vertex > 0,])
tmp$vertex <- NULL
tmp

d$vertex <- NA
d[which(do.call(paste, d[,1:3]) %in% do.call(paste, tmp)),"vertex"] <- 1
d$vertex <- ifelse(is.na(d$vertex),0,1)
d$vertex

# show vertices
#tmp <- tmp[tmp$Value > 0,]
# save number of vertex points
(num_vertex_constr = nrow(tmp))
# actual vertex point coordinates
#(vertices <- d[d$vertex>0,c("x1","x2","x3")])
vertices <- tmp

################################################################################
# Visualize the vertices
names(d)
summary(d)
par(mfrow=c(1,1))
## FIGURE 1 - just show 3d visual of points
library(scatterplot3d)
?scatterplot3d
plot3d <- scatterplot3d(x = d$x1, y = d$x2, z = d$x3
                        , xlab="x1", ylab="x2", zlab="x3"
                        , color = "black", bg = "black"
                        , xlim=c(min(d$x1),max(d$x1))
                        , ylim=c(min(d$x2),max(d$x2))
                        , zlim=c(min(d$x3),max(d$x3))
                        , angle=55, scale.y=0.7
                        , cex.symbols=1.6, pch=21
                        , box=F, col.grid="grey", lty.grid=par("lty")
)
summary(d)
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
#source('addgrids3d.r')
addgrids3d(d[,c(1,2,3)], grid=c("xy", "xz", "yz")
           , xlim=c(min(d$x1),max(d$x1))
           , ylim=c(min(d$x2),max(d$x2))
           , zlim=c(min(d$x3),max(d$x3))
           , angle = 55, scale.y=0.7
           , col.grid = "black"
)

par(mfrow=c(1,1))
# add grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
#source('addgrids3d.r')
addgrids3d(d[,c(1,2,3)], grid=c("xy", "xz", "yz")
           , xlim=c(min(d$x1),max(d$x1))
           , ylim=c(min(d$x2),max(d$x2))
           , zlim=c(min(d$x3),max(d$x3))
           , angle = 55, scale.y=0.7
           , col.grid = "black"
)
# Now adding some points to the "scatterplot3d"
?scatterplot3d
plot3d$points3d(x = tmp$x1, y = tmp$x2, z = tmp$x3
                , col = "red", bg = "red", pch=19
)
## previous way to add points, but the lines covered the points
#plot3d <- scatterplot3d(x = d$x1, y = d$x2, z = d$x3
#                        , xlab="x1", ylab="x2", zlab="x3"
#                        , color = "black"
#                        , bg = ifelse(d$vertex>0,"red","black")
#                        , xlim=c(min(d$x1),max(d$x1))
#                        , ylim=c(min(d$x2),max(d$x2))
#                        , zlim=c(min(d$x3),max(d$x3))
#                        #, bg="black"
#                        , angle=55, scale.y=0.7
#                        , cex.symbols=1.6, pch=21
#                        , box=F#, col.grid="black", lty.grid=par("lty")
#)
summary(d)
# data for table 12 showing vertex points
d[d$vertex==1,c(1:4)]
tmp

################################################################################
# Maximize Stability with convex hull constraints
################################################################################
rm(lps.model)
library(lpSolveAPI)
library(lpSolve)
# Create a new lpSolve linear program model object.
num_variable_dvs <- ncol(vertices)+1
(lps.model <- make.lp(nrow=0, ncol=num_variable_dvs + num_vertex_constr))
for (i in 1:(num_variable_dvs + num_vertex_constr)){
  set.type(lps.model, columns=i, type="real") # real d.v's
}
# set objective function
lp.control(lps.model, sense="max")
# coefficients from linear model
coefficients <- as.numeric(summary(f)$coefficients[,1])
chem_costs <- c(2.56, 2.50, 21.52)
# formulate coefficients into objective function
set.objfn(lps.model, obj=c(coefficients, rep(0,num_vertex_constr)))
# define constraints
#stability_constraint_param = 80
add.constraint(lps.model, c(coefficients, rep(0,num_vertex_constr))
               , ">=", stability_constraint_param) # stability constraint
add.constraint(lps.model, c(0, chem_costs, rep(0,num_vertex_constr))
               , "<=", 1000) # financial cost constraint
#makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0,0,rep(0,num_vertex_constr)), "=", 1)
add.constraint(lps.model, c(0,-1,0,0,vertices[,1]), "=", 0) #x1 constraint
add.constraint(lps.model, c(0,0,-1,0,vertices[,2]), "=", 0) #x2 constraint
add.constraint(lps.model, c(0,0,0,-1,vertices[,3]), "=", 0) #x3 constraint
add.constraint(lps.model, c(0,0,0,0,rep(1,num_vertex_constr)), "=", 1) #ws sum to 1
# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value 
get.variables(lps.model) # optimal solution of d.v.'s (our decisions to make)

# how much was the cost?
sum(chem_costs*get.variables(lps.model)[2:4])
# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=6)))
(names(results2) <- c("Model","x1","x2","x3","Exp[y]","Cost"))
results2$Model <- "Max[Stability] + Convex Hull Constraints"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$x3 <- get.variables(lps.model)[[4]] 
results2$"Exp[y]" <- sum(c(f$coefficients[["(Intercept)"]],f$coefficients[["x1"]]
                           ,f$coefficients[["x2"]]
                           ,f$coefficients[["x3"]])*get.variables(lps.model)[1:4])
results2$"Cost" <- sum(chem_costs*get.variables(lps.model)[2:4])
results2

(results <- rbind(results,results2))
################################################################################
# 
#                   Minimize cost for carpet data
# 
################################################################################
################################################################################
# Minimize cost
################################################################################
# Create a new lpSolve linear program model object.
(lps.model <- make.lp(nrow=0, ncol=4))
# what kind of decision variables do you have? "real", "integer", or "binary"?
set.type(lps.model, columns=1, type="real")
set.type(lps.model, columns=2, type="real")
set.type(lps.model, columns=3, type="real")
set.type(lps.model, columns=4, type="real")
get.type(lps.model) # to see what types are defined for each D.V.
# Give our model a name
name.lp(lps.model, name="Cost")
# set objective function
lp.control(lps.model, sense="min")
# coefficients from linear model
summary(f)$coefficients
# formulate coefficients into objective function
set.objfn(lps.model, obj=c(0,chem_costs))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]]
                            ,f$coefficients[["x3"]])
               , ">=", stability_constraint_param)
#makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0,0), "=", 1)
#budgetary constraint
add.constraint(lps.model, c(0,x1.cost,x2.cost,x3.cost), "<=", 1000)
# lets review our LP model
lps.model 
# in the lpSolve linear program model object.
dimnames(lps.model) <- list(c("stability constraint", "intercept constraint"
                              ,"budget"),
                            c("intercept scalar", "# x1","# x2","# x3"))
# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value 
get.variables(lps.model) # optimal solution of d.v.'s (our decisions to make)
# sensitivity analysis and shadow prices
options(scipen=999) # removes scientific notation in outputs
# sensitivity of the objective function
get.sensitivity.obj(lps.model)[[1]]
get.sensitivity.obj(lps.model)[[2]]
# sensitivity of the constraints
str(get.sensitivity.rhs(lps.model))
# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=6)))
(names(results2) <- c("Model","x1","x2","x3","Exp[y]","Cost"))
results2$Model <- "Min[Cost]"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$x3 <- get.variables(lps.model)[[4]] 
results2$"Exp[y]" <- sum(c(f$coefficients[["(Intercept)"]],f$coefficients[["x1"]]
                           ,f$coefficients[["x2"]]
                           ,f$coefficients[["x3"]])*get.variables(lps.model))
results2$"Cost" <- sum(chem_costs*get.variables(lps.model)[2:4])
results2

(results <- rbind(results,results2))

################################################################################
# Minimize costs with min-max constraints
################################################################################
# Create a new lpSolve linear program model object.
(lps.model <- make.lp(nrow=0, ncol=4))
# what kind of decision variables do you have? "real", "integer", or "binary"?
set.type(lps.model, columns=1, type="real")
set.type(lps.model, columns=2, type="real")
set.type(lps.model, columns=3, type="real")
set.type(lps.model, columns=4, type="real")
get.type(lps.model) # to see what types are defined for each D.V.
# Give our model a name
name.lp(lps.model, name="Cost")
# set objective function
lp.control(lps.model, sense="min")
# coefficients from linear model
summary(f)$coefficients
# formulate coefficients into objective function
set.objfn(lps.model, obj=c(0,chem_costs))
# define constraints
add.constraint(lps.model, c(f$coefficients[["(Intercept)"]]
                            ,f$coefficients[["x1"]],f$coefficients[["x2"]]
                            ,f$coefficients[["x3"]])
               , ">=", stability_constraint_param)
#makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0,0), "=", 1)
#budgetary constraint
add.constraint(lps.model, c(0,x1.cost,x2.cost,x3.cost), "<=", 1000)
add.constraint(lps.model, c(0,1,0,0), "<=", max(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,1,0,0), ">=", min(d$x1)) #min-max for x1
add.constraint(lps.model, c(0,0,1,0), "<=", max(d$x2)) #min-max for x2
add.constraint(lps.model, c(0,0,1,0), ">=", min(d$x2)) #min-max for x2
add.constraint(lps.model, c(0,0,0,1), "<=", max(d$x3)) #min-max for x3
add.constraint(lps.model, c(0,0,0,1), ">=", min(d$x3)) #min-max for x3
# lets review our LP model
lps.model 
# in the lpSolve linear program model object.
dimnames(lps.model) <- list(c("stability constraint","intercept constraint"
                              ,"budget"
                              ,"max x1 constr.","min x1 constr."
                              ,"max x2 constr.","min x2 constr."
                              ,"max x3 constr.","min x3 constr."),
                            c("intercept scalar", "# x1", "# x2", "# x3"))

# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value 
get.variables(lps.model) # optimal solution of d.v.'s (our decisions to make)
# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=6)))
(names(results2) <- c("Model","x1","x2","x3","Exp[y]","Cost"))
results2$Model <- "Min[Cost] + Min-Max Constraints"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$x3 <- get.variables(lps.model)[[4]] 
results2$"Exp[y]" <- sum(c(f$coefficients[["(Intercept)"]],f$coefficients[["x1"]]
                           ,f$coefficients[["x2"]]
                           ,f$coefficients[["x3"]])*get.variables(lps.model))
results2$"Cost" <- sum(chem_costs*get.variables(lps.model)[2:4])
results2

(results <- rbind(results,results2))

################################################################################
# Min costs with convex hull constraints
################################################################################
# Create a new lpSolve linear program model object.
num_variable_dvs <- ncol(vertices)+1
(lps.model <- make.lp(nrow=0, ncol=num_variable_dvs + num_vertex_constr))
for (i in 1:(num_variable_dvs + num_vertex_constr)){
  set.type(lps.model, columns=i, type="real") # real d.v's
}
# set objective function
lp.control(lps.model, sense="min")
# coefficients from linear model
coefficients <- as.numeric(summary(f)$coefficients[,1])
# formulate coefficients into objective function
set.objfn(lps.model, obj=c(0,chem_costs,rep(0,num_vertex_constr)))
# define constraints
add.constraint(lps.model, c(coefficients, rep(0,num_vertex_constr))
               , ">=", stability_constraint_param) # stability constraint
add.constraint(lps.model, c(0, chem_costs, rep(0,num_vertex_constr))
               , "<=", 1000) # financial cost constraint
#makes first d.v. 1 (this is to keep intercept term in)
add.constraint(lps.model, c(1,0,0,0,rep(0,num_vertex_constr)), "=", 1)
add.constraint(lps.model, c(0,-1,0,0,vertices[,1]), "=", 0) #x1 constraint
add.constraint(lps.model, c(0,0,-1,0,vertices[,2]), "=", 0) #x2 constraint
add.constraint(lps.model, c(0,0,0,-1,vertices[,3]), "=", 0) #x3 constraint
add.constraint(lps.model, c(0,0,0,0,rep(1,num_vertex_constr)), "=", 1) #ws sum to 1
# lets review our LP model
lps.model 
# solve the model
solve(lps.model)
get.objective(lps.model) # optimal obj. value 
get.variables(lps.model) # optimal solution of d.v.'s (our decisions to make)
# save results
(results2 <- data.frame(matrix(NA,nrow=1,ncol=6)))
(names(results2) <- c("Model","x1","x2","x3","Exp[y]","Cost"))
results2$Model <- "Min[Cost] + Convex Hull Constraints"
results2$x1 <- get.variables(lps.model)[[2]] 
results2$x2 <- get.variables(lps.model)[[3]] 
results2$x3 <- get.variables(lps.model)[[4]] 
results2$"Exp[y]" <- sum(c(f$coefficients[["(Intercept)"]],f$coefficients[["x1"]]
                           ,f$coefficients[["x2"]]
                           ,f$coefficients[["x3"]])*get.variables(lps.model)[1:4])
results2$"Cost" <- sum(chem_costs*get.variables(lps.model)[2:4])
results2

(results <- rbind(results,results2))
################################################################################
# summarize work in a nice looking table
################################################################################
library(remotes)
#remotes::install_github("ianmoran11/mmtable2")
library(mmtable2)
library(gt)
library(reshape2)
library(tidyverse) # to use arrange() function
(results2 <- melt(results))
results2$value <- round(results2$value,2)

main_table <- results2 %>%
  arrange(factor(Model, levels=c('Max[Stability]'
                                 ,'Max[Stability] + Min-Max Constraints'
                                 ,'Max[Stability] + Convex Hull Constraints'
                                 ,'Min[Cost]'
                                 ,'Min[Cost] + Min-Max Constraints'
                                 ,'Min[Cost] + Convex Hull Constraints'
  ))
  ,desc(variable), desc(value), value) %>% 
  mmtable(cells=value) +
  header_top(variable) + 
  header_left(Model)
main_table

################################################################################
#           Generate some visualizations showing various solutions
################################################################################
################################################################################
# x1 vs x3 vs y 
# Ignoring max(stability) b/c values are too far
################################################################################
names(d)
par(mfrow=c(1,1))
d$stability_achieved <- ifelse(d$y >= 80,1,0)
## FIGURE 1
library(scatterplot3d)
?scatterplot3d
plot3d <- scatterplot3d(x = d$x1, y = d$x3, z = d$y
                        , xlab="x1", ylab="x3", zlab="y"
                        , color = ifelse(d$resistance_achieved==1,"black","red")
                        , xlim=c(4,7), ylim=c(0,0.20), zlim=c(60,100)
                        , bg="black", angle=45, scale.y=0.7
                        , cex.symbols=1.6, pch=16
                        , box=F, col.grid="grey", lty.grid=par("lty")
)
summary(d)
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
#source('addgrids3d.r')
addgrids3d(d[,c(1,3,4)], grid=c("xy", "xz", "yz")
           , xlim=c(4,7), ylim=c(0,0.20), zlim=c(60,100)
           , angle = 45, scale.y=0.7)
# put points back on top of grid
plot3d$points3d(x=d$x1, y=d$x3, z=d$y, type="p", pch=16, cex=1,
                col = ifelse(d$resistance_achieved==1,"black","red"))
# Trick* Have to fit a multiple linear regression so we can easily get the
# the customer spec plane on the figure
f <- lm(y ~ x1 + x3, data=d)
summary(f)
# show output nicely
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(jtools)
summ(f, pvals = T, confint=T)
f$coefficients[["x1"]] <- 0
#f$coefficients[["x2"]] <- 0
f$coefficients[["x3"]] <- 0
#names(f$coefficients)[3] <- "x2"  #need to do this so plot understands
f$coefficients[["(Intercept)"]] <- stability_constraint_param
f
# add the fitted regression plane to the 3d-plot
plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=T)

results
# Ignored adding max[stability] because solution is so far away
# add solution points
plot3d$points3d(x=results[2,"x1"]          #x1 decision
                , y=results[2,"x3"]        #x3 decision
                , z=results[2,"Exp[y]"]    #expected stability
                , type="p", pch=16, cex=1, col="blue")
pts <- data.frame(x=results[2,"x1"] + .50, y=results[2,"x3"], z=results[2,"Exp[y]"])
text(plot3d$xyz.convert(pts), labels=results[2,1], cex=.7, col="blue")
pts <- data.frame(x=results[2,"x1"], y=results[2,"x3"], z=results[2,"Exp[y]"]+1.25)
text(plot3d$xyz.convert(pts), labels="B", cex=1, col="blue", font=2)
# add solution points
plot3d$points3d(x=results[3,"x1"]          #x1 decision
                , y=results[3,"x3"]        #x3 decision
                , z=results[3,"Exp[y]"]    #expected stability
                , type="p", pch=16, cex=1, col="blue")
pts <- data.frame(x=results[3,"x1"] + .55, y=results[3,"x3"], z=results[3,"Exp[y]"])
text(plot3d$xyz.convert(pts), labels=results[3,1],cex=.7, col="blue")
pts <- data.frame(x=results[3,"x1"], y=results[3,"x3"], z=results[3,"Exp[y]"]-1.25)
text(plot3d$xyz.convert(pts), labels="C", cex=1, col="blue", font=2)
# Ignored adding min[cost] because solution is so far away
# add solution points (E)
plot3d$points3d(x=results[5,"x1"]          #x1 decision
                , y=results[5,"x3"]        #x3 decision
                , z=results[5,"Exp[y]"]    #expected stability
                , type="p", pch=16, cex=1, col="dark green")
pts <- data.frame(x=results[5,"x1"] + .05, y=results[5,"x3"], z=results[5,"Exp[y]"]-1)
text(plot3d$xyz.convert(pts), labels=results[5,1],cex=.7, col="dark green")
pts <- data.frame(x=results[5,"x1"], y=results[5,"x3"], z=results[5,"Exp[y]"]+1.25)
text(plot3d$xyz.convert(pts), labels="E", cex=1, col="dark green", font=2)
# add solution points (F)
plot3d$points3d(x=results[6,"x1"]          #x1 decision
                , y=results[6,"x3"]        #x3 decision
                , z=results[6,"Exp[y]"]    #expected stability
                , type="p", pch=16, cex=1, col="dark green")
pts <- data.frame(x=results[6,"x1"], y=results[6,"x3"], z=results[6,"Exp[y]"]-3.5)
text(plot3d$xyz.convert(pts), labels=results[6,1],cex=.7, col="dark green", font=2)
pts <- data.frame(x=results[6,"x1"], y=results[6,"x3"], z=results[6,"Exp[y]"]-1.5)
text(plot3d$xyz.convert(pts), labels="F", cex=1, col="dark green", font=2)

################################################################################
# x1 vs x2 vs y 
# Ignoring max(stability) b/c values are too far
################################################################################
## FIGURE 2
# plot3d <- scatterplot3d(x = d$x1, y = d$x2, z = d$y
#                         , xlab="x1", ylab="x2", zlab="y"
#                         , color = ifelse(d$resistance_achieved==1,"red","black")
#                         , xlim=c(4,7), ylim=c(5,8.5), zlim=c(60,100)
#                         , bg="black", angle=45, scale.y=0.7
#                         , cex.symbols=1.6, pch=16
#                         , box=F, col.grid="grey", lty.grid=par("lty")
# )
# summary(d)
# # add light grey grid
# source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
# #source('addgrids3d.r')
# addgrids3d(d[,c(1,2,4)], grid=c("xy", "xz", "yz")
#            , xlim=c(4,7), ylim=c(5,8.5), zlim=c(60,100)
#            , angle = 45, scale.y=0.7)
# # put points back on top of grid
# plot3d$points3d(x=d$x1, y=d$x2, z=d$y, type="p", pch=16, cex=1,
#                 col = ifelse(d$resistance_achieved==1,"red","black"))
# # Trick* Have to fit a multiple linear regression so we can easily get the
# # the customer spec plane on the figure
# f <- lm(y ~ x1 + x2, data=d)
# summary(f)
# # show output nicely
# # https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
# library(jtools)
# summ(f, pvals = T, confint=T)
# f$coefficients[["x1"]] <- 0
# f$coefficients[["x2"]] <- 0
# #f$coefficients[["x3"]] <- 0
# #names(f$coefficients)[3] <- "x2"  #need to do this so plot understands
# f$coefficients[["(Intercept)"]] <- stability_constraint_param
# f
# # add the fitted regression plane to the 3d-plot
# plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=T)
# 
# results
# # Ignored adding max[stability] because solution is so far away
# # add solution points
# plot3d$points3d(x=results[2,"x1"]          #x1 decision
#                 , y=results[2,"x2"]        #x3 decision
#                 , z=results[2,"Exp[y]"]    #expected stability
#                 , type="p", pch=16, cex=1, col="blue")
# pts <- data.frame(x=results[2,"x1"] + .80
#                   , y=results[2,"x2"]   
#                   , z=results[2,"Exp[y]"])
# text(plot3d$xyz.convert(pts), labels=results[2,1],cex=.7, col="blue")
# # add solution points
# plot3d$points3d(x=results[3,"x1"]          #x1 decision
#                 , y=results[3,"x2"]        #x3 decision
#                 , z=results[3,"Exp[y]"]    #expected stability
#                 , type="p", pch=16, cex=1, col="blue")
# pts <- data.frame(x=results[3,"x1"] + .88
#                   , y=results[3,"x2"]   
#                   , z=results[3,"Exp[y]"])
# text(plot3d$xyz.convert(pts), labels=results[3,1],cex=.7, col="blue")
# # add solution points
# plot3d$points3d(x=results[5,"x1"]          #x1 decision
#                 , y=results[5,"x2"]        #x3 decision
#                 , z=results[5,"Exp[y]"]    #expected stability
#                 , type="p", pch=16, cex=1, col="dark green")
# pts <- data.frame(x=results[5,"x1"] + .88
#                   , y=results[5,"x2"]   
#                   , z=results[5,"Exp[y]"])
# text(plot3d$xyz.convert(pts), labels=results[5,1],cex=.7, col="dark green")
# # add solution points
# plot3d$points3d(x=results[6,"x1"]          #x1 decision
#                 , y=results[6,"x2"]        #x3 decision
#                 , z=results[6,"Exp[y]"]    #expected stability
#                 , type="p", pch=16, cex=1, col="dark green")
# pts <- data.frame(x=results[6,"x1"] + .88
#                   , y=results[6,"x2"]   
#                   , z=results[6,"Exp[y]"])
# text(plot3d$xyz.convert(pts), labels=results[6,1],cex=.7, col="dark green")

# scatterplot of solutions
par(mfrow=c(1,1),mgp = c(3,1,0), mar = c(5,5,4,2))
plot(results$Cost[2:6] ~ results$"Exp[y]"[2:6]
     , pch=19, cex=1.5
     , main="Cost ($) vs Expected Stability (E[Y])"
     , ylab="Cost ($)"
     , xlab="Exp[Stability]"
     , xlim=c(75,95), ylim=c(25,45), yaxt="n"
     , col=c(rep("blue",2),rep("dark green",3)))
axis(side=2, at=c(25,30,35,40,45), labels = paste0("$",c(25,30,35,40,45)))
(pts <- data.frame(x=results$"Exp[y]"[2:6]
                   #, y=results$Cost[2:6])-.75)
                   , y= c(results$Cost[2]+.75,
                          results$Cost[3]-.75,
                          results$Cost[4]+.75,
                          results$Cost[5]-.75,
                          results$Cost[6]+.75)))
text(pts$y ~ pts$x, labels=results$Model[2:6], cex=1
     , col=c(rep("blue",2),rep("dark green",3)))
# add boundary line
abline(v=c(0,0,80), col="red", lty=2, lwd=3)
?abline
# add standard normal curve image
library(jpeg)
img <- readJPEG("standard_normal_curve.jpg")
rasterImage(img, xleft=75, ybottom=39, xright=85, ytop=45.7)

################################################################################
#           Handling expected values in optimization models
################################################################################
f <- lm(y ~ x1 + x2 + x3, data=d)
summary(f)

yhat <- predict(f, d)
errors <- as.data.frame(d$y - yhat); names(errors) = "errors"
head(errors)

# Create histogram with density overlay
ggplot(errors, aes(x=errors)) +
  geom_histogram(aes(y = ..density..), binwidth=.25, center=0
                 , color="black", fill="grey") + xlim(-2.5, 2.5) +
  geom_density(alpha=0.1, fill="blue", bounds=c(-2.5,2.5)) +
  labs(title = "Distribution of Prediction Errors", x="Residual Errors"
       , y="Density") +
  theme_minimal()

# percentage of time predicting greater than 80 when it was actually less than 80
sum(ifelse(yhat>=80 & y<80, 1, 0))/length(yhat)
sum(ifelse(yhat<80 & y>=80, 1, 0))/length(yhat)

# Predict with 95% prediction intervals
predictions <- predict(f, newdata=d, interval="prediction", level=0.95)
predictions2 <- predict(f, newdata=d, interval="confidence", level=0.95)
head(predictions)
head(predictions2)

## Calculate 95% PI from scratch
# Define the input data
x1 <- d$x1
x2 <- d$x2
x3 <- d$x3
y <- d$y
# Combine x1, x2, and x3 into a matrix X, including an intercept term (column of 1's)
X <- cbind(1, x1, x2, x3)
# Calculate the regression coefficients using matrix algebra
(beta <- solve(t(X) %*% X) %*% t(X) %*% y)  # Coefficients
# Define a new observation for which we want to predict y (e.g., x1_new = 2, x2_new = 3, x3_new = 4)
(X_new <- c(1, d$x1[[1]], x2=d$x2[[1]], x3=d$x3[[1]]))  # Including the intercept term (1)
# Calculate the predicted value of y for the new observation
(y_pred <- sum(X_new * beta))
# Calculate residuals and residual variance
(y_hat <- X %*% beta)  # Fitted values from the model
(residuals <- y - y_hat)  # Residuals
(residual_var <- sum(residuals^2) / (length(y) - length(beta)))  # Residual variance
# Calculate the standard error of the prediction
(se_pred <- sqrt(residual_var * (1 + t(X_new) %*% solve(t(X) %*% X) %*% X_new)))
# Determine the critical t-value for a 95% prediction interval with (n - p - 1) degrees of freedom
(t_value <- qt(0.975, df = length(y) - length(beta)))
# Calculate the margin of error
(margin_error <- t_value * se_pred)
# Compute the 95% prediction interval
(lower_bound <- y_pred - margin_error)
(upper_bound <- y_pred + margin_error)
# Display the prediction interval
cat("95% Prediction Interval: [", lower_bound, ", ", upper_bound, "]\n")
head(predictions)

# Combine new data and predictions into a data frame
prediction_df <- cbind(d, predictions)
head(prediction_df)
tail(predictions)

# Prediction interval graphic
ggplot(prediction_df, aes(x=y, y=fit)) +
  geom_point() +
  geom_point(data=prediction_df, aes(x=y, y=fit), color="blue") +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  geom_ribbon(data = prediction_df, aes(x=y, ymin=lwr, ymax=upr), alpha=0.2) +
  labs(title = "95% Prediction Interval", x="Y (Actual)", y="Y (Predicted)") +
  theme_minimal()

# what amount do we need to add to our RHS?
rhs_add <- summary(prediction_df$lwr-prediction_df$fit)["Mean"]
summary(prediction_df$upr-prediction_df$fit)["Mean"]

# Create histogram with density overlay
ggplot(errors, aes(x=errors)) +
  geom_histogram(aes(y = ..density..), binwidth=.25, center=0
                 , color="black", fill="grey") + xlim(-2.5, 2.5) +
  geom_density(alpha=0.1, fill="blue", bounds=c(-2.5,2.5)) +
  labs(title = "Distribution of Prediction Errors", x="Residual Errors"
       , y="Density") +
  geom_vline(xintercept=rhs_add, linetype="dashed", color="red") +
  annotate("text", x=rhs_add, y=0.3
           , label=paste("RHS addition of ",round(abs(rhs_add),2))
           , color="blue", angle=90, vjust=-0.5) +
  theme_minimal()

################################################################################
# Now lets consider if the predictive model in stage 1 is a non-parametric 
# (machine learning) type model. Show how to optimize in stage 2
################################################################################
# Optimization model using genetic algorithm
################################################################################
# Recall the linear regression the model
f <- lm(y ~ x1 + x2 + x3, data=d)
summary(f)

# Define the coefficients from the linear regression model
(coefficients <- coef(f))

# Objective function to maximize
objective_function <- function(params) {
  # params are x1, x2, x3
  intercept <- coefficients[1]
  x1 <- params[1]
  x2 <- params[2]
  x3 <- params[3]
  # Calculate the expected value of y
  expected_y <- (intercept + coefficients[2]*x1 + coefficients[3]*x2 
                 + coefficients[4]*x3)
  # Return the expected value of y
  return(expected_y)  
}

# Define the bounds for x1, x2, and x3
lower_bounds <- c(min(d$x1), min(d$x2), min(d$x3))
upper_bounds <- c(max(d$x1), max(d$x2), max(d$x3))

# Run the genetic algorithm
library(GA)
# Set seed for reproducibility
set.seed(123)
ga_result <- ga(
  type = "real-valued",
  fitness = objective_function,
  lower = lower_bounds,
  upper = upper_bounds,
  popSize = 50,     # Population size
  maxiter = 100,    # Maximum number of iterations
  run = 50          # Number of generations without improvement
)

# Summary of GA results
summary(ga_result)

# Best solution found
best_solution <- ga_result@solution

# Calculate the maximum expected value of y
max_expected_y <- objective_function(best_solution)

# Print results
print(paste("Best solution (x1, x2, x3):", paste(best_solution, collapse = ", ")))
print(paste("Maximum expected value of y:", max_expected_y))

################################################################################
# Integrating a machine learning models into an optimization model using a
# genetic algorithm
################################################################################
# Set seed for reproducibility
set.seed(123)
library(caret)
# defined a 3-fold cross-validation design
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  # if you want probabilities
                     #summaryFunction = twoClassSummary, # for classification
                     summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)
################################################################################
# Train different models
################################################################################
# train various machine learning models
# https://topepo.github.io/caret/available-models.html
rf <- train(y~x1+x2+x3, data=d, method="rf", trControl=ctrl, metric="Rsquared")
svr <- train(y~x1+x2+x3, data=d, method="svmPoly", trControl=ctrl, metric="Rsquared")
xgb <- train(y~x1+x2+x3, data=d, method="xgbLinear", trControl=ctrl, metric="Rsquared")
ann <- train(y~x1+x2+x3, data=d, method="glmnet", trControl=ctrl, metric="Rsquared")

# final model
(ml_stats = data.frame(matrix(nrow=4, ncol=2, data=NA)))
(names(ml_stats) <- c("model","R2"))
top <- tolerance(rf$results, metric="Rsquared", tol=0.01, maximize=TRUE)
ml_stats[1,] <- c(model="rf", R2=rf$results[top,1:6][["Rsquared"]])
top <- tolerance(svr$results, metric="Rsquared", tol=0.01, maximize=TRUE)
ml_stats[2,] <- c(model="svr", R2=svr$results[top,1:6][["Rsquared"]])
top <- tolerance(xgb$results, metric="Rsquared", tol=0.01, maximize=TRUE)
ml_stats[3,] <- c(model="xgb", R2=xgb$results[top,1:6][["Rsquared"]])
top <- tolerance(ann$results, metric="Rsquared", tol=0.01, maximize=TRUE)
ml_stats[4,] <- c(model="ann", R2=ann$results[top,1:6][["Rsquared"]])
ml_stats$R2 <- as.numeric(ml_stats$R2)
ml_stats[,2] <- round(ml_stats[,2],4)
ml_stats

# generating predictions
#predict(rf, newdata=d, type='raw')
#predict(svr, newdata=d, type='raw')
#predict(xgb, newdata=d, type='raw')
#predict(ann, newdata=d, type='raw')

ml_stats[,3:6] <- NA
names(ml_stats)[3:6] <- c("x1","x2","x3","best_soln")
ml_stats
(ml_models <- ml_stats$model)

for (i in 1:4) {
  # Objective function to maximize
  objective_function <- function(params, model=get(ml_models[[i]])) {
    # params are x1, x2, x3
    x1 <- params[1]
    x2 <- params[2]
    x3 <- params[3]
    # Create a new data frame for prediction
    new_data <- data.frame(x1 = x1, x2 = x2, x3 = x3)
    # Predict y using the random forest model
    predicted_y <- predict(model, new_data)
    # Return the predicted value of y
    return(predicted_y)
  }
  
  # Define the bounds for x1, x2, and x3
  lower_bounds <- c(min(d$x1), min(d$x2), min(d$x3))
  upper_bounds <- c(max(d$x1), max(d$x2), max(d$x3))
  
  # Run the genetic algorithm
  ga_result <- ga(
    type = "real-valued",
    fitness = objective_function,
    lower = lower_bounds,
    upper = upper_bounds,
    popSize = 50,     # Population size
    maxiter = 100,    # Maximum number of iterations
    run = 50          # Number of generations without improvement
  )
  
  # Summary of GA results
  summary(ga_result)
  
  # Best solution found
  best_solution <- ga_result@solution
  best_solution <- best_solution[1,]
  best_solution
  ml_stats[i,3:5] <- best_solution
  
  # Calculate the maximum expected value of y
  max_expected_y <- objective_function(best_solution)
  max_expected_y
  ml_stats[i,6] <- max_expected_y
  
}

# final results
ml_stats

################################################################################
# 
#           Generate some visualizations showing various solutions
# 
################################################################################
################################################################################
# x1 vs x3 vs y 
# Ignoring max(stability) b/c values are too far
################################################################################
names(d)
par(mfrow=c(1,1))
d$stability_achieved <- ifelse(d$y >= 80,1,0)
d$resistance_achieved <- ifelse(d$y2 >= 8,1,0)
ml_stats
## FIGURE 1
library(scatterplot3d)
#?scatterplot3d
plot3d <- scatterplot3d(x=ml_stats$x1, y=ml_stats$x3, z=ml_stats$best_soln
                        , xlab="x1", ylab="x3", zlab="y"
                        , color = "blue"
                        , xlim=c(4,7), ylim=c(0,0.20), zlim=c(80,100)
                        , bg="black", angle=60, scale.y=0.7
                        , cex.symbols=1.6, pch=16
                        , box=F, col.grid="grey", lty.grid=par("lty")
)
summary(d)
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
#source('addgrids3d.r')
addgrids3d(d[,c(1,3,4)], grid=c("xy", "xz", "yz")
           , xlim=c(4,7), ylim=c(0,0.20), zlim=c(80,100)
           , angle = 60, scale.y=0.6)
# put points back on top of grid
plot3d$points3d(x=d$x1, y=d$x3, z=d$y, type="p", pch=16, cex=.5,
                col = ifelse(d$resistance_achieved==1,"red","black"))
# Trick* Have to fit a multiple linear regression so we can easily get the
# the customer spec plane on the figure
f <- lm(y ~ x1 + x3, data=d)
summary(f)
# show output nicely
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(jtools)
summ(f, pvals = T, confint=T)
f$coefficients[["x1"]] <- 0
#f$coefficients[["x2"]] <- 0
f$coefficients[["x3"]] <- 0
#names(f$coefficients)[3] <- "x2"  #need to do this so plot understands
f$coefficients[["(Intercept)"]] <- stability_constraint_param
f
# add the fitted regression plane to the 3d-plot
plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=T)

################################################################################
# x1 vs x2 vs y 
# Ignoring max(stability) b/c values are too far
################################################################################
## FIGURE 2
plot3d <- scatterplot3d(x=ml_stats$x1, y=ml_stats$x2, z=ml_stats$best_soln
                        , xlab="x1", ylab="x2", zlab="y"
                        , color = "blue"
                        , xlim=c(4,7), ylim=c(5,8.5), zlim=c(80,100)
                        , bg="black", angle=60, scale.y=0.7
                        , cex.symbols=1.6, pch=16
                        , box=F, col.grid="grey", lty.grid=par("lty")
)
summary(d)
# add light grey grid
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
#source('addgrids3d.r')
addgrids3d(d[,c(1,2,4)], grid=c("xy", "xz", "yz")
           , xlim=c(4,7), ylim=c(5,8.5), zlim=c(80,100)
           , angle = 60, scale.y=0.7)
# put points back on top of grid
plot3d$points3d(x=d$x1, y=d$x2, z=d$y, type="p", pch=16, cex=.5,
                col = ifelse(d$resistance_achieved==1,"red","black"))
# Trick* Have to fit a multiple linear regression so we can easily get the
# the customer spec plane on the figure
f <- lm(y ~ x1 + x2, data=d)
summary(f)
# show output nicely
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(jtools)
summ(f, pvals = T, confint=T)
f$coefficients[["x1"]] <- 0
f$coefficients[["x2"]] <- 0
#f$coefficients[["x3"]] <- 0
#names(f$coefficients)[3] <- "x2"  #need to do this so plot understands
f$coefficients[["(Intercept)"]] <- stability_constraint_param
f
# add the fitted regression plane to the 3d-plot
plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=T)


