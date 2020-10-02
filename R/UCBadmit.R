#' ---
#' title: UCBadmit.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.26"
#' purpose:
#' ---

# This is the UCBadmit dataset from the rethinking package
remotes::install_github("rmcelreath/rethinking")

library(rethinking)
data(UCBadmit)

model <- admit ~ 1 + applicant.gender
fit <- lm(model, data=UCBadmit)
summary(fit)

model <- admit ~ 1 + applicant.gender + dept
fit <- lm(model, data=UCBadmit)
summary(fit)

model <- admit ~ 1 + applicant.gender * dept
fit <- lm(model, data=UCBadmit)
summary(fit)
