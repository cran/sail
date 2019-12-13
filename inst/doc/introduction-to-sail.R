## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("pacman")
#  pacman::p_load_gh('sahirbhatnagar/sail')

## ------------------------------------------------------------------------
library(sail)
data("sailsim")
names(sailsim)

## ------------------------------------------------------------------------
library(splines)
f.basis <- function(x) splines::bs(x, degree = 3)

## ------------------------------------------------------------------------
fit <- sail(x = sailsim$x, y = sailsim$y, e = sailsim$e, basis = f.basis)

## ------------------------------------------------------------------------
fit

## ------------------------------------------------------------------------
plot(fit)

## ------------------------------------------------------------------------
coef(fit)[1:6,50:55]

## ------------------------------------------------------------------------
predict(fit)[1:5,50:55]

## ------------------------------------------------------------------------
predict(fit, s = 0.8)

## ------------------------------------------------------------------------
predict(fit, s = c(0.8, 0.2))

## ------------------------------------------------------------------------
fit[["active"]]

## ------------------------------------------------------------------------
set.seed(432) # to reproduce results (randomness due to CV folds)
library(doParallel) 
registerDoParallel(cores = 2) 
cvfit <- cv.sail(x = sailsim$x, y = sailsim$y, e = sailsim$e, basis = f.basis,
                 dfmax = 10, # to speed up vignette build time
                 nfolds = 5, parallel = TRUE, nlambda = 50)

## ------------------------------------------------------------------------
plot(cvfit)

## ------------------------------------------------------------------------
cvfit[["lambda.min"]]
cvfit[["lambda.1se"]]

## ------------------------------------------------------------------------
cbind(coef(cvfit, s="lambda.1se"), # lambda.1se is the default
coef(cvfit, s = "lambda.min"))

## ------------------------------------------------------------------------
predict(cvfit, type = "nonzero")

## ------------------------------------------------------------------------
plotMain(cvfit$sail.fit, x = sailsim$x, xvar = "X3",
         legend.position = "topright",
         s = cvfit$lambda.min, f.truth = sailsim$f3)

## ---- fig.height=7, fig.width=8------------------------------------------
plotInter(cvfit$sail.fit, x = sailsim$x, xvar = "X4",
          f.truth = sailsim$f4.inter,
          s = cvfit$lambda.min,
          title_z = "Estimated")

## ------------------------------------------------------------------------
f.identity <- function(i) i

## ------------------------------------------------------------------------
cvfit_linear <- cv.sail(x = sailsim$x, y = sailsim$y, e = sailsim$e,
                        basis = f.identity, nfolds = 5, parallel = TRUE,
                 dfmax = 10, # to speed up vignette build time
                        nlambda = 50)

## ------------------------------------------------------------------------
plot(cvfit_linear)

## ------------------------------------------------------------------------
coef(cvfit_linear, s = "lambda.min")

## ------------------------------------------------------------------------
# the weights correspond to E, X1, X2, X3, ... X_p, X1:E, X2:E, ... X_p:E
p.fac <- c(0, 1, 0.4, 0.6, 0.7, rep(1, 2*ncol(sailsim$x) - 4))

## ------------------------------------------------------------------------
fit_pf <- sail(x = sailsim$x, y = sailsim$y, e = sailsim$e, basis = f.basis,
               nlambda = 50,
               dfmax = 10, # to speed up vignette build time
               penalty.factor = p.fac)

## ------------------------------------------------------------------------
plot(fit_pf)

