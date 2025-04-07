## This function runs the carpet regression experiments
run_carpet_regressions <- function(d,...) {

results <- data.frame(matrix(nrow=0, ncol=17))
results
names(results) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2"
                    ,"px1x2","Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3"
                    ,"px1x2x3","AdjR2")
results
#   1      2     3     4     5     6     7     8      9       10
# "Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2",
#    11      12      13      14      15         16       17
# "Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"

f <- lm(y ~ x1, data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,7,2,8)],3)
                      , rep(NA,12), round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

f <- lm(y ~ x2, data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,7)],3)
                      , rep(NA,2)
                      , round(summary(f)$ coefficients[c(2,8)],3)
                      , rep(NA,10)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

f <- lm(y ~ x3, data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,7)],3)
                      , rep(NA,4)
                      , round(summary(f)$ coefficients[c(2,8)],3)
                      , rep(NA,8)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

#   1      2     3     4     5     6     7     8      9       10
# "Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2",
#    11      12      13      14      15         16       17
# "Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"
f <- lm(y ~ x1 + x2, data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,10,2,11,3,12)],3)
                      , rep(NA,10)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

f <- lm(y ~ x1 + x3, data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,10,2,11)],3)
                      , rep(NA,2)
                      , round(summary(f)$ coefficients[c(3,12)],3)
                      , rep(NA,8)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

f <- lm(y ~ x2 + x3, data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,10)],3)
                      , rep(NA,2)
                      , round(summary(f)$ coefficients[c(2,11,3,12)],3)
                      , rep(NA,8)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))


f <- lm(y ~ x1 + x2 + x3, data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,13,2,14,3,15,4,16)],3)
                      , rep(NA,8)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

#   1      2     3     4     5     6     7     8      9       10
# "Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2",
#    11      12      13      14      15         16       17
# "Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"
f <- lm(y ~ x1 + x2 + I(x1*x2), data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,13,2,14,3,15)],3)
                      , rep(NA,2)
                      , round(summary(f)$ coefficients[c(9,10)],3)
                      , rep(NA,6)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

f <- lm(y ~ x1 + x3 + I(x1*x3), data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,13,2,14)],3)
                      , rep(NA,2)
                      , round(summary(f)$ coefficients[c(3,15)],3)
                      , rep(NA,2)
                      , round(summary(f)$ coefficients[c(4,16)],3)
                      , rep(NA,4)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

#   1      2     3     4     5     6     7     8      9       10
# "Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2",
#    11      12      13      14      15         16       17
# "Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"

f <- lm(y ~ x2 + x3 + I(x2*x3), data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,13)],3)
                      , rep(NA,2)
                      , round(summary(f)$ coefficients[c(2,14,3,15)],3)
                      , rep(NA,4)
                      , round(summary(f)$ coefficients[c(4,16)],3)
                      , rep(NA,2)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

f <- lm(y ~ x1 + x2 + x3 + I(x1*x2*x3), data=d); summary(f)
tmp <- data.frame(t(c(round(summary(f)$ coefficients[c(1,16,2,17,3,18,4,19)],3)
                      , rep(NA,6)
                      , round(summary(f)$ coefficients[c(5,20)],3)
                      , round(summary(f)$ adj.r.squared,3))))
names(tmp) <- c("Bx0", "px0","Bx1","px1","Bx2","px2","Bx3","px3","Bx1x2","px1x2"
                ,"Bx1x3","px1x3","Bx2x3","px2x3","Bx1x2x3","px1x2x3","AdjR2"); tmp
(results <-  rbind(results,tmp))

return(results)

}