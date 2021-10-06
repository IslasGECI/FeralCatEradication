library("tidyverse")
###########################################################################################
## Type III functional response (reduction in capture efficiency with decreasing density)
max.eff <- 1 # max efficiency
min.eff <- 0 # min efficiency
max.pN <- 1 # max population proportion
min.pN <- 0 # min population proportion
infl.eff <- 0.5

pN.vec <- c(min.pN, 0.2, 0.4, 0.5, 0.7, 0.8, max.pN)
eff.vec <- c(min.eff, 0.05, 0.3, infl.eff, 0.85, 0.95, max.eff)
plot(pN.vec, eff.vec, type = "b", pch = 19)
eff.dat <- data.frame(pN.vec, eff.vec)
colnames(eff.dat) <- c("pN", "eff")

# a/(1 + b*e^(-cx)) (logistic)
param.init <- c(1, 85, 8.9)
fit.eff <- nls(eff ~ a / (1 + (b * exp(-c * pN))),
  data = eff.dat,
  algorithm = "port",
  start = c(a = param.init[1], b = param.init[2], c = param.init[3]),
  trace = TRUE,
  nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1 / 1024)
)
fit.eff.summ <- summary(fit.eff)
plot(pN.vec, eff.vec, pch = 19, xlab = "pN", ylab = "efficiency")
pN.vec.cont <- seq(0, 1, 0.01)
pred.eff.fx <- coef(fit.eff)[1] / (1 + (coef(fit.eff)[2] * exp(-coef(fit.eff)[3] * pN.vec.cont)))
lines(pN.vec.cont, pred.eff.fx, lty = 2, lwd = 3, col = "red")

a.eff <- coef(fit.eff)[1]
b.eff <- coef(fit.eff)[2]
c.eff <- coef(fit.eff)[3]

###########################################################################################
