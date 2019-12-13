## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
set.seed(1234)
library(sail)
x_df <- as.data.frame(sailsim$x)
x_df$race <- factor(sample(1:5, nrow(x_df), replace = TRUE))
table(x_df$race)

## ------------------------------------------------------------------------
library(splines)
x <- stats::model.matrix(~ 0 +  bs(X1, degree = 5) + bs(X2, degree = 3) + ns(X3, df = 8) + 
                           bs(X4, degree = 6) + X5 + poly(X6,2) + race, data = x_df)
head(x)

## ------------------------------------------------------------------------
attr(x, "assign")

## ------------------------------------------------------------------------
fit_design <- sail(x = x, y = sailsim$y, e = sailsim$e, 
                   expand = FALSE,
                   group = attr(x, "assign"), verbose = 0)

## ------------------------------------------------------------------------
plot(fit_design)

## ------------------------------------------------------------------------
library(doParallel)
registerDoParallel(cores = 2)
cvfit_design <- cv.sail(x = x, y = sailsim$y, e = sailsim$e, 
                        expand = FALSE,
                        dfmax = 10, # to speed up vignette build time
                        group = attr(x, "assign"), verbose = 0,
                        nfolds = 5, parallel = TRUE, nlambda = 50)

## ------------------------------------------------------------------------
plot(cvfit_design)

## ------------------------------------------------------------------------
cbind(coef(cvfit_design, s="lambda.1se"), # lambda.1se is the default
coef(cvfit_design, s = "lambda.min"))

## ------------------------------------------------------------------------
predict(cvfit_design, type = "nonzero")

