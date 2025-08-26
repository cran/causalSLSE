## ----include=FALSE------------------------------------------------------------
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  paste0("\n \\", "footnotesize","\n\n", x, "\n\n \\normalsize")
})
knitr::opts_chunk$set(comment="")

## ----echo=FALSE, fig.align='center', out.width='50%', fig.width=10, fig.height=6----
knots <- c(2,5,7.5)
maxX <- 11.5
minX <- -2
plot(NULL, xlim=c(minX,maxX+1), ylim=c(minX,knots[2]), axes=FALSE, ylab="u(x)", xlab="x")
axis(1, c("min(X)", expression(k[1]), expression(k[2]), expression(k[3]),
          "max(X)"), at=c(minX, knots, maxX))
axis(2, c("min(X)", 0), at=c(minX, 0))
curve(x*(x<=knots[1])+knots[1]*(x>knots[1]), minX, maxX, add=TRUE,lwd=2)
curve((x-knots[3])*(x>knots[3]), minX, maxX, add=TRUE, col=4, lty=4,lwd=2)
curve((x-knots[1])*(knots[1]<=x)*(x<=knots[2])+
      (knots[2]-knots[1])*(x>knots[2]), minX,
      maxX, add=TRUE, col=2, lty=2,lwd=2)
curve((x-knots[2])*(knots[2]<=x)*(x<=knots[3])+
      (knots[3]-knots[2])*(x>knots[3]), minX,
      maxX, add=TRUE, col=3, lty=3,lwd=2)
legend("topleft", c(expression(u[1](x)), expression(u[2](x)),
                    expression(u[3](x)), expression(u[4](x))),
       col=1:4, lty=1:4, bty='n', lwd=2)
box()

## -----------------------------------------------------------------------------
library(causalSLSE)
data(nsw)

## -----------------------------------------------------------------------------
k <- slseKnots(form = ~ age + re75 + ed + married, data = nsw)

## -----------------------------------------------------------------------------
print(k)

## -----------------------------------------------------------------------------
p <- seq(0,1,len=9)[c(-1,-9)] # these are the probabilities with 7 knots
quantile(nsw[,'re75'], p, type=1)
quantile(nsw[,'ed'], p, type=1)

## -----------------------------------------------------------------------------
k$age

## -----------------------------------------------------------------------------
k$married

## -----------------------------------------------------------------------------
mod1 <- slseModel(form = re78 ~ age + re75 + ed + married, data = nsw)

## -----------------------------------------------------------------------------
print(mod1)

## -----------------------------------------------------------------------------
mod1$knots

## -----------------------------------------------------------------------------
data(simDat4)
mod2 <- slseModel(Y ~ X1 + X2 + X3 + X4, data = simDat4)
print(mod2, which="selKnots")

## -----------------------------------------------------------------------------
slseModel(re78 ~ age + re75 + ed + married, data = nsw, knots = NULL)

## -----------------------------------------------------------------------------
selK <- list(NA, c(1000,5000,10000), NULL, NA)
mod <- slseModel(re78 ~ age + re75 + ed + married, data = nsw,
                 knots = selK)
print(mod, which = "selKnots")

## -----------------------------------------------------------------------------
selK <- list(married = NA, ed = NULL, 'age:ed' = NULL, re75 = c(1000,5000,10000), age = NA)
model <- slseModel(re78 ~ age * ed + re75 + married, data = nsw, knots = selK)
print(model, which="selKnots")

## -----------------------------------------------------------------------------
selK <- list(ed = NULL, 'age:ed' = NULL, re75 = c(1000,5000,10000))
model <- slseModel(re78 ~ age * ed + re75 + married, data = nsw, knots = selK)
print(model, which="selKnots")

## -----------------------------------------------------------------------------
selK <- list('age:ed' = NULL, 'ed:re75' = NULL, 'ed:married' = NULL)
model <- slseModel(re78 ~ age * ed + re75 * ed + married * ed,
                   data = nsw, knots = selK)

## -----------------------------------------------------------------------------
model

## -----------------------------------------------------------------------------
model <- slseModel(re78 ~ age + ed * black, data = nsw)       
model$knots[["ed:black"]]

## -----------------------------------------------------------------------------
mod2 <- slseModel(form = re78 ~ ed + married, data = nsw)
fit2 <- estSLSE(mod2)
summary(fit2)

## ----fig.align='center', out.width='50%', fig.height=5------------------------
plot(fit2, "ed", interval="confidence", level=0.95)

## -----------------------------------------------------------------------------
model1 <- cslseModel(re78 ~ treat | ~ age + re75 + ed + married, data = nsw)

## -----------------------------------------------------------------------------
model1

## -----------------------------------------------------------------------------
attr(model1, "treatedVar")
attr(model1, "groupInd")

## -----------------------------------------------------------------------------
model1$treated$knots

## -----------------------------------------------------------------------------
print(model1, which="selKnots")

## -----------------------------------------------------------------------------
model2 <- cslseModel(Y ~ treat | ~ X1 + X2 + X3 + X4, data = simDat4,
                     groupInd = c(treated = "treat", nontreated = "notreat"))
model2

## -----------------------------------------------------------------------------
selK <- list(treated=NULL)
cslseModel(re78 ~ treat | ~ age + re75 + ed + married, data = nsw,
           knots = selK)

## -----------------------------------------------------------------------------
selK <- list(nontreated=list(NA, c(1000,5000,10000), NULL, NA))
model <- cslseModel(re78 ~ treat | ~ age + re75 + ed + married, data = nsw,
                    knots = selK)
print(model, which = "selKnots")

## -----------------------------------------------------------------------------
model <- cslseModel(re78 ~ treat | ~ age + married, data = nsw,
                    nbasis = function(n) 2)

## -----------------------------------------------------------------------------
estSLSE(model$treated)

## -----------------------------------------------------------------------------
fit <- estSLSE(model)
fit

## -----------------------------------------------------------------------------
attr(fit, "treatedVar")
attr(fit, "groupInd")

## -----------------------------------------------------------------------------
fit$treated

## -----------------------------------------------------------------------------
s <- summary(fit, type="HC0")

## -----------------------------------------------------------------------------
s$nontreated

## -----------------------------------------------------------------------------
model$nontreated$knots$age

## ----results='asis', message=FALSE--------------------------------------------
library(texreg)
texreg(as.list(fit), table=FALSE)

## -----------------------------------------------------------------------------
predict(fit, newdata = data.frame(treat = c(1,1,0,0),age = 20:23, married = 1))

## -----------------------------------------------------------------------------
predict(fit,  newdata = data.frame(treat = c(1,1,0,0),age = 20:23, married = 1),
        interval = "confidence")

## -----------------------------------------------------------------------------
predict(fit, newdata = data.frame(treat = c(1,1,0,0),age = 20:23, married = 1),
        se.fit = TRUE)

## -----------------------------------------------------------------------------
causalSLSE:::.initParCSLSE()$treated$points

## -----------------------------------------------------------------------------
graphPar <- list(treated = list(lines = list(lty=5, col=4)),
                 nontreated = list(points = list(pch=25, col=3)),
                 common = list(xlab = "MyNewLab", main="My New Title"),
                 legend = list(x = "top"))

## -----------------------------------------------------------------------------
model1 <- cslseModel(re78 ~ treat | ~ age + re75 + ed + married, data = nsw)
fit1 <- estSLSE(model1)

## ----fig.show='hold', out.width='50%'-----------------------------------------
library(sandwich)
arg1 <- list(treated = list(lines = list(col = "darkred", lty = 4)),
             nontreated = list(lines = list(col = "darkgreen", lty = 2)),
             legend = list(x = "topleft"))
arg2 <- list(legend = list(x = "top", cex=0.7))
plot(fit1, "ed", vcov. = vcov, graphPar=arg1, interval = 'confidence')
plot(fit1, "age", interval = 'confidence', level = 0.9, type = "HC1", graphPar=arg2)

## ----fig.show='hold', out.width='50%'-----------------------------------------
plot(fit1, "age", FUN = median)
plot(fit1, "age", FUN = function(x) quantile(x, 0.20))

## ----fig.show='hold', out.width='50%'-----------------------------------------
arg2 <- list(legend = list(cex = 0.8), common=list(ylim=c(4000,9000)))
plot(fit1, "age", fixedCov = list(married = 1, re75 = 10000),
     addToLegend = "married", graphPar = arg2)
plot(fit1, "age", fixedCov = list(married = 0, re75 = 10000),
     addToLegend = "non-married", graphPar = arg2)

## ----fig.align='center', out.width='70%'--------------------------------------
arg3 <- list(legend = list(cex = 0.7),
             common = list(ylim = c(3000, 10000)))
plot(fit1, "age", fixedCov = list(married = 1, re75 = 10000),
     addToLegend = "married", graphPar = arg3)
arg4 <- list(treated = list(lines = list(col = "darkred", lty = 5)),
             nontreated = list(lines = list(col = "darkgreen", lty = 4)),
             legend = list(x = "topleft", cex = 0.7))
plot(fit1, "age", fixedCov = list(married = 0, re75 = 10000),
     addToLegend = "non-married", add = TRUE, graphPar = arg4)

## ----fig.show='hold', out.width='50%'-----------------------------------------
arg5 <- list(treated = list(lines = list(col = "darkred", lty = 4)),
             nontreated = list(lines = list(col = "darkgreen", lty = 2)),
             legend = list(x = "topleft"))
plot(fit1, "ed", addPoints = TRUE, graphPar = arg5)
plot(fit1, "re75", addPoints = TRUE)

## -----------------------------------------------------------------------------
data(simDat4)
mod <- cslseModel(Y ~ Z | ~ X1 + X2 + X4, data = simDat4)
mod

## -----------------------------------------------------------------------------
data(simDat4)
mod <- cslseModel(Y ~ Z | ~ X1 + X2 * X4, data = simDat4)
mod

## ----eval=FALSE---------------------------------------------------------------
# fit <- estSLSE(mod)
# plot(fit, "X1", fixedCov = list(X2 = "first"))

## -----------------------------------------------------------------------------
model1 <- cslseModel(re78 ~ treat | ~ age + re75 + ed + married, data = nsw)
model1
model2 <- selSLSE(model1)
model2

## -----------------------------------------------------------------------------
print(model2, which="Pvalues")

## -----------------------------------------------------------------------------
model3 <- selSLSE(model1, selType = "BLSE", selCrit = "BIC")
model3

## -----------------------------------------------------------------------------
estSLSE(selSLSE(model1, selType = "FLSE", selCrit = "BIC"))

## -----------------------------------------------------------------------------
model1 <- cslseModel(re78 ~ treat | ~ age + re75 + ed + married, data = nsw)
model2 <- selSLSE(model1, selType="BLSE", selCrit="AIC")
model2

## -----------------------------------------------------------------------------
model3 <- model1
model3$treated <- selSLSE(model3$treated, selType="BLSE", selCrit="AIC")
model3$nontreated <- selSLSE(model3$nontreated, selType="BLSE", selCrit="AIC")
model3

## -----------------------------------------------------------------------------
model <- cslseModel(re78 ~ treat | ~ age + re75 + ed + married, data = nsw)
model

## -----------------------------------------------------------------------------
model <- selSLSE(model, selType="BLSE", selCrit="AIC")

## -----------------------------------------------------------------------------
names(model$treated$selections)
names(model$treated$selections)

## -----------------------------------------------------------------------------
model <- selSLSE(model, selType="FLSE", selCrit="AIC")
names(model$treated$selections)
names(model$treated$selections)

## -----------------------------------------------------------------------------
names(model$treated$selections$BLSE)

## -----------------------------------------------------------------------------
model$treated <- selSLSE(model$treated, "BLSE", "AIC")
model

## -----------------------------------------------------------------------------
names(model$treated$selections$BLSE)

## -----------------------------------------------------------------------------
attr(model$nontreated$knots, "curSel")

## -----------------------------------------------------------------------------
model$treated$selections$BLSE$JAIC

## -----------------------------------------------------------------------------
estSLSE(model$treated, model$treated$selections$BLSE$JAIC)

## -----------------------------------------------------------------------------
model

## -----------------------------------------------------------------------------
model <- update(model, selType="FLSE", selCrit="AIC")
model

## -----------------------------------------------------------------------------
update(model, selType="None")

## -----------------------------------------------------------------------------
model$treated

## -----------------------------------------------------------------------------
update(model$treated, "BLSE", "AIC")

## -----------------------------------------------------------------------------
model1 <- cslseModel(re78 ~ treat | ~ age + re75 + ed + married, data=nsw)
fit1 <- estSLSE(model1)
ce <- causalSLSE(fit1)
ce

## -----------------------------------------------------------------------------
ce$ACE

## -----------------------------------------------------------------------------
sce <- summary(ce)
sce

## ----fig.align='center', out.width='65%'--------------------------------------
plot(ce, "re75")

## ----eval=FALSE---------------------------------------------------------------
# library(texreg)
# c1 <- causalSLSE(fit1)
# fit2 <- estSLSE(selSLSE(model1, selType="BLSE"))
# fit3 <- estSLSE(selSLSE(model1, selType="FLSE"))
# c2 <- causalSLSE(fit2)
# c3 <- causalSLSE(fit3)
# texreg(list(SLSEC=c1, BLSE_AIC=c2, FLSE_AIC=c3), table=FALSE, digits=4)

## ----results='asis', message=FALSE, echo=FALSE--------------------------------
library(texreg)
c1 <- causalSLSE(fit1)
fit2 <- estSLSE(selSLSE(model1, selType="BLSE"))
fit3 <- estSLSE(selSLSE(model1, selType="FLSE"))
c2 <- causalSLSE(fit2)
c3 <- causalSLSE(fit3)
texreg(list(SLSE=c1, BLSE=c2, FLSE=c3), table=FALSE, digits=4)

## ----eval=FALSE---------------------------------------------------------------
# texreg(list(SLSE=c1, BLSE=c2, FLSE=c3), table=FALSE, digits=3,
#        which="ACE-ACT", include.adjrs=FALSE, separated.rsquared=TRUE)

## ----results='asis', message=FALSE, echo=FALSE--------------------------------
texreg(list(SLSE=c1, BLSE=c2, FLSE=c3), table=FALSE, digits=3, 
       which="ACE-ACT", include.adjrs=FALSE, separated.rsquared=TRUE)

## ----eval=FALSE---------------------------------------------------------------
# c1 <- causalSLSE(model1, selType="SLSE")
# c2 <- causalSLSE(model1, selType="BLSE")
# c3 <- causalSLSE(model1, selType="FLSE")
# texreg(list(SLSE=c1, BLSE=c2, FLSE=c3), table=FALSE, digits=4)

## ----results='asis', message=FALSE, echo=FALSE--------------------------------
c1 <- causalSLSE(model1, selType="SLSE")
c2 <- causalSLSE(model1, selType="BLSE")
c3 <- causalSLSE(model1, selType="FLSE")
texreg(list(SLSE=c1, BLSE=c2, FLSE=c3), table=FALSE, digits=4)

## ----eval=FALSE---------------------------------------------------------------
# c1 <- causalSLSE(model1, selType="FLSE", selCrit="AIC")
# c2 <- causalSLSE(model1, selType="FLSE", selCrit="BIC")
# c3 <- causalSLSE(model1, selType="FLSE", selCrit="PVT")

## -----------------------------------------------------------------------------
model1 <- selSLSE(model1, selType="FLSE")
c1 <- causalSLSE(model1, selType="FLSE", selCrit="AIC")
c2 <- causalSLSE(model1, selType="FLSE", selCrit="BIC")
c3 <- causalSLSE(model1, selType="FLSE", selCrit="PVT")

## ----eval=FALSE---------------------------------------------------------------
# c1 <- causalSLSE(re78 ~ treat | ~ age + re75 + ed + married, data=nsw,
#                  selType="SLSE")
# c2 <- causalSLSE(re78 ~ treat | ~ age + re75 + ed + married, data=nsw,
#                  selType="BLSE")
# c3 <- causalSLSE(re78 ~ treat | ~ age + re75 + ed + married, data=nsw,
#                  selType="FLSE")
# texreg(list(SLSE=c1, BLSE=c2, FLSE=c3), table=FALSE, digits=4)

## ----results='asis', message=FALSE, echo=FALSE--------------------------------
c1 <- causalSLSE(model1, selType = "SLSE")
c2 <- causalSLSE(model1, selType = "BLSE")
c3 <- causalSLSE(model1, selType = "FLSE")
texreg(list(SLSE=c1, BLSE=c2, FLSE=c3), table = FALSE, digits = 4)

## -----------------------------------------------------------------------------
data(simDat1)
mod <- cslseModel(Y ~ Z | ~ X, data = simDat1)
mod <- selSLSE(mod, "BLSE") ## Let's save them all first
mod <- selSLSE(mod, "FLSE")

## ----eval=FALSE---------------------------------------------------------------
# c1 <- causalSLSE(mod, selType = "SLSE")
# c2 <- causalSLSE(mod, selType = "BLSE", selCrit = "BIC")
# c3 <- causalSLSE(mod, selType = "FLSE", selCrit = "BIC")
# texreg(list(SLSE = c1, BLSE = c2, FLSE = c3), table = FALSE, digits = 4)

## ----results='asis', message=FALSE, echo=FALSE--------------------------------
c1 <- causalSLSE(mod, selType = "SLSE")
c2 <- causalSLSE(mod, selType = "BLSE", selCrit = "BIC")
c3 <- causalSLSE(mod, selType = "FLSE", selCrit = "BIC")
texreg(list(SLSE = c1, BLSE = c2, FLSE = c3), table = FALSE, digits = 4)

## ----out.width='50%', fig.show='hold', fig.height=4---------------------------
list(common = list(main = "Y vs X using BLSE-BIC"))
plot(c1, "X")
curve(1 - 2 * x, -3, 3, col = "darkgreen", lty = 4, lwd = 3, add = TRUE)
curve(1 + x + x^2, -3, 3, col = "darkorange", lty = 4, lwd = 3, add = TRUE)
legend("bottomleft", c("True-treated", "True-nontreated"),
       col=c("darkgreen", "darkorange"), lty = 4, lwd = 3, bty = 'n')
plot(c2, "X", graphPar = list(common = list(main = "Y vs X using BLSE-BIC")))
curve(1 - 2 * x, -3, 3, col="darkgreen", lty = 4, lwd = 3, add = TRUE)
curve(1 + x + x^2, -3, 3, col = "darkorange", lty = 4, lwd = 3, add = TRUE)
legend("bottomleft", c("True-treated", "True-nontreated"),
       col = c("darkgreen", "darkorange"), lty = 4, lwd = 3, bty = 'n')
plot(c3, "X", graphPar = list(common = list(main = "Y vs X using FLSE-BIC")))
curve(1 - 2 * x, -3, 3, col="darkgreen", lty = 4, lwd = 3, add = TRUE)
curve(1 + x + x^2, -3, 3, col = "darkorange", lty = 4, lwd = 3, add = TRUE)
legend("bottomleft", c("True-treated", "True-nontreated"),
       col = c("darkgreen", "darkorange"), lty = 4, lwd = 3, bty = 'n')

## ----out.width='60%', fig.align='center'--------------------------------------
plot(c1, "X", addPoints=TRUE)

## -----------------------------------------------------------------------------
data(simDat2)
mod <- cslseModel(Y~Z | ~X, data=simDat2)
mod <- selSLSE(mod, "BLSE") ## We just add BLSE because we do not use FLSE

## ----eval=FALSE---------------------------------------------------------------
# c1 <- causalSLSE(mod, selType = "SLSE")
# c2 <- causalSLSE(mod, selType = "BLSE", selCrit = "BIC")
# c3 <- causalSLSE(mod, selType = "BLSE", selCrit = "AIC")
# texreg(list(SLSE = c1, BLSE.BIC = c2, BLSE.AIC = c3), table = FALSE, digits = 4)

## ----results='asis', message=FALSE, echo=FALSE--------------------------------
c1 <- causalSLSE(mod, selType = "SLSE")
c2 <- causalSLSE(mod, selType = "BLSE", selCrit = "BIC")
c3 <- causalSLSE(mod, selType = "BLSE", selCrit = "AIC")
texreg(list(SLSE = c1, BLSE.BIC = c2, BLSE.AIC = c3), table = FALSE, digits = 4)

## ----out.width='50%', fig.show='hold'-----------------------------------------
arg <-  list(common = list(main = "Y vs X using BLSE-AIC"),
             legend = list(x = "right", cex = 0.8))
plot(c2, "X", graphPar = arg)
curve((1 -2 * x) * (x <= 0) + (1 + 2 * x) * (x > 0), -3, 3,
      col = "darkgreen", lty = 3, lwd = 3, add = TRUE)
curve((1 + x) * (x <= -1) + (-1 - x) * (x > -1),
      -3, 3, col = "darkorange", lty = 3, lwd = 3, add = TRUE)
legend("left", c("True-treated", "True-nontreated"),
       col = c("darkgreen", "darkorange"), lty = 3, lwd = 3, bty = 'n', cex = .8)
arg$legend$x <- "topleft"
plot(c2, "X", addPoints = TRUE, graphPar = arg)

## -----------------------------------------------------------------------------
data(simDat3)
mod <- cslseModel(Y ~ Z | ~ X1 + X2, data = simDat3)
mod <- selSLSE(mod, "FLSE") ## We just add FLSE because we do not use BLSE

## ----eval=FALSE---------------------------------------------------------------
# c1 <- causalSLSE(mod, selType = "SLSE")
# c2 <- causalSLSE(mod, selType = "FLSE", selCrit = "BIC")
# c3 <- causalSLSE(mod, selType = "FLSE", selCrit = "AIC")
# texreg(list(SLSE = c1, FLSE.BIC = c2, FLSE.AIC = c3), table = FALSE, digits = 4)

## ----results='asis', message=FALSE, echo=FALSE--------------------------------
c1 <- causalSLSE(mod, selType = "SLSE")
c2 <- causalSLSE(mod, selType = "FLSE", selCrit = "BIC")
c3 <- causalSLSE(mod, selType = "FLSE", selCrit = "AIC")
texreg(list(SLSE = c1, FLSE.BIC = c2, FLSE.AIC = c3), table = FALSE, digits = 4)

## ----out.width='50%', fig.show='hold'-----------------------------------------
arg <-  list(common = list(main = "Y vs X1 using FLSE-AIC"),
             legend = list(x = "right", cex = 0.8))
plot(c2, "X1", graphPar = arg)
curve(x + x^2, -3, 3, col = "darkgreen", lty = 3, lwd = 3, add = TRUE)
curve(2 - 2 * x, -3, 3, col = "darkorange", lty = 3, lwd = 3, add = TRUE)
legend("topleft", c("True-treated", "True-nontreated"),
       col = c("darkgreen", "darkorange"), lty = 3, lwd = 3, bty = 'n', cex = .8)
arg$legend$x <- "topleft"
plot(c2, "X1", addPoints = TRUE, graphPar = arg)

## ----out.width='50%', fig.show='hold'-----------------------------------------
arg <-  list(common = list(main = "Y vs X2 using FLSE-AIC"),
             legend = list(x = "right", cex = 0.8))
plot(c2, "X2", graphPar = arg)
curve(1 + (1 - 2 * x) * (x <= 0) + (1 + 2 * x) * (x > 0), -3, 3,
      col = "darkgreen", lty = 3, lwd = 3, add = TRUE)
curve(1 + (1 + x) * (x <= -1) + (-1 - x) * (x > -1),
      -3, 3, col = "darkorange", lty = 3, lwd = 3, add = TRUE)
legend("left", c("True-treated", "True-nontreated"),
       col = c("darkgreen", "darkorange"), lty = 3, lwd = 3, bty = 'n', cex = .8)
arg$legend$x <- "topleft"
plot(c2, "X2", addPoints = TRUE, graphPar = arg)

## ----echo=FALSE---------------------------------------------------------------
sim_model <- function(n)
{
	X1 <- rnorm(n)
    X2 <- rnorm(n)
	probX <- plogis(1 + X1 + X2 + X1*X2) 
	Z <- rbinom(n, 1, probX)
	er0 <- rnorm(n)
	er1 <- rnorm(n)
	Y0 <-  (1 + X1 + X1^2) + (1 + X2)  * (X2 <= -1) + 
		(-1 - X2) * (X2 > -1) + (1+X1*X2 + X1^2*X2^2) + er0
	Y1 <- (1 - 2*X1) + (1 - 2*X2)*(X2 <= 0) + 
		(1 + 2*X2) * (X2 > 0) + (1-2*X1*X2) + er1
	Y <- Y0 * (1 - Z) + Y1 * Z
	data.frame(Y=Y, X1=X1, X2=X2, Z=Z, Y1=Y1, Y0=Y0)
}

## -----------------------------------------------------------------------------
data(simDat5)
mod <- cslseModel(Y ~ Z | ~ X1 * X2, data = simDat5)
mod <- selSLSE(mod, "FLSE") ## We just add FLSE because we do not use BLSE

## ----eval=FALSE---------------------------------------------------------------
# c1 <- causalSLSE(mod, selType = "SLSE")
# c2 <- causalSLSE(mod, selType = "FLSE", selCrit = "BIC")
# c3 <- causalSLSE(mod, selType = "FLSE", selCrit = "AIC")
# texreg(list(SLSE = c1, FLSE.BIC = c2, FLSE.AIC = c3), table = FALSE, digits = 4)

## ----results='asis', message=FALSE, echo=FALSE--------------------------------
c1 <- causalSLSE(mod, selType="SLSE")
c2 <- causalSLSE(mod, selType="FLSE", selCrit="BIC")
c3 <- causalSLSE(mod, selType="FLSE", selCrit="AIC")
texreg(list(SLSE=c1, FLSE.BIC=c2, FLSE.AIC=c3), table=FALSE, digits=4)

## ----out.width='50%', fig.show='hold'-----------------------------------------
x20 <- mean(subset(simDat5, Z == 0)$X2)
x21 <- mean(subset(simDat5, Z == 1)$X2)
arg <-  list(common = list(main = "Y vs X1 (X2 = sample mean for each group)"),
             legend = list(x = "right", cex = 0.8))
plot(c2, "X1", fixedCov = list(nontreated = list(X2 = x20), treated = list(X2 = x21)),
     graphPar = arg)
curve(1.3698 + 0.6302 * x + 1.1368 * x^2, -3, 3,
      col = "darkgreen", lty = 3, lwd = 3, add = TRUE)
curve(3.3964 - 2.3964 * x, -3, 3, col = "darkorange", lty = 3, lwd = 3, add = TRUE)
legend("top", c("True-treated", "True-nontreated"),
       col=c("darkorange", "darkgreen"), lty = 3, lwd = 3, bty = 'n', cex = .8)
arg <-  list(common = list(main = "Y vS X1 (X2 = 1 for each group)"),
             legend = list(x = "right", cex = 0.8))
plot(c2, "X1", fixedCov = list(X2 = 1), graphPar = arg)
curve(2 * x + 2 * x^2, -3, 3, col = "darkgreen", lty = 3, lwd = 3, add = TRUE)
curve(5 - 4 * x, -3, 3, col = "darkorange", lty = 3, lwd = 3, add = TRUE)
legend("top", c("True-treated", "True-nontreated"),
       col = c("darkgreen", "darkorange"), lty = 3, lwd = 3, bty = 'n', cex = .8)

## ----out.width='50%', fig.show='hold'-----------------------------------------
x10 <- mean(subset(simDat5, Z == 0)$X1)
x11 <- mean(subset(simDat5, Z == 1)$X1)
arg <-  list(common = list(main = "Y vs X2 (X1 = sample mean for each group)"),
             legend = list(x = "right", cex = 0.8))
plot(c2, "X2", fixedCov = list(nontreated = list(X1 = x10), treated = list(X1 = x11)),
     graphPar = arg)
curve(1.603900 - .3964 * x + (1 - 2 * x) * (x <= 0) + (1 + 2 * x) * (x > 0), -3, 3,
      col = "darkgreen", lty = 3, lwd = 3, add = TRUE)
curve(1.767 - 0.3698 * x + 0.1368 * x^2 + (1 + x) * (x <= -1) + (-1 - x) * (x > -1),
      -3, 3, col = "darkorange", lty = 3, lwd = 3, add = TRUE)
legend("top", c("True-treated", "True-nontreated"),
       col = c("darkorange", "darkgreen"), lty = 3, lwd = 3, bty = 'n', cex = .8)
arg$common$main <- "Y vS X2 (X1 = 1 for each group)"
plot(c2, "X2", fixedCov = list(X1 = 1), graphPar = arg)
curve(-2 * x + (1 - 2 * x) * (x <= 0) + (1 + 2 * x) * (x > 0), -3, 3,
      col = "darkgreen", lty = 3, lwd = 3, add = TRUE)
curve(4 + (1 + x) * (x <= -1) + (-1 - x) * (x > -1) + x + x^2,
      -3, 3, col = "darkorange", lty = 3, lwd = 3, add = TRUE)
legend("top", c("True-treated", "True-nontreated"),
       col = c("darkgreen", "darkorange"), lty = 3, lwd = 3, bty = 'n', cex = .8)

## -----------------------------------------------------------------------------
data(simDat5)
mod <- cslseModel(Y ~ Z | ~ X1 * X2, data = simDat5)
getAlt <- function(which=c("ipw","matching"), ...)
{
    which <- match.arg(which)
    met <- c("ACE","ACT","ACN")
    l <- lapply(met, function(mi)
    {
        arg <- list(...)
        arg$type <- mi
        res <- do.call(which, arg)
    })
    if (length(l[[1]]$estim)==2)
    {
        l <- lapply(1:2, function(i) {
            obj <- lapply(l, function(li) {
                li$estim <- li$estim[i]
                li$se <- li$se[i]
                li})
            names(obj) <- met
            class(obj) <- "allAlt"
            obj})
        names(l) <- c("Matching", "BC_Matching")
    } else {
        names(l) <- met
        class(l) <- "allAlt"
    }
    l
}

setMethod("extract", signature = className("allAlt", package='causalSLSE'),
          definition = function (model, include.nobs = TRUE, ...) 
          {
              se <- sapply(model, function(li) li$se)
              co <- sapply(model, function(li) li$estim)
              pval <- 2*pnorm(-abs(co/se))
              names(co) <- names(se) <- names(pval) <- names(model)
              gof <- numeric()
              gof.names <- character()
              gof.decimal <- logical()
              if (isTRUE(include.nobs)) {
                  n <- nrow(model[[1]]$data)
                  gof <- c(gof, n)
                  gof.names <- c(gof.names, "Num. obs.")
                  gof.decimal <- c(gof.decimal, FALSE)
              }
              tr <- createTexreg(coef.names = names(co), coef = co, se = se, 
                                 pvalues = pval, gof.names = gof.names,
                                 gof = gof, gof.decimal = gof.decimal)
              return(tr)
          })

res <- getAlt("ipw", form=Y~Z, psForm=Z~X1*X2, data=simDat5, normalized=TRUE)
res2 <- getAlt("matching", form=Y~Z, psForm=Z~X1*X2, data=simDat5, bcForm=~X1*X2,
               balm=~X1*X2)
texreg(list(IPW=res, Matching=res2[[1]], BC.Matching=res2[[2]]))

