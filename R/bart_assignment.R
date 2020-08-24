#' ---
#' title: bart_assignment.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.24"
#' purpose: 
#' ---

library(magrittr)
library(tidyverse)
# columns: `gender`, `balloon_color`, `probability`, `press_count` and `GAD7`.
N <- 201
p_female <- 0.40
balloon_color <- rep(c("red", "green", "blue"), N/3)
probability <- rep(c(1/32, 1/64, 1/128), N/3)

data <- tibble(participant_id = 1:N,
               gender = runif(N) < p_female,
               balloon_color = balloon_color,
               probability = probability,
               press_count = 1,
               gad7 = 1)

data %<>%
  mutate(gender = ifelse(gender, "female", "male"),
         gender = factor(gender, levels = c("female", "male")),
         balloon_color = factor(balloon_color, levels = c("red", "green", "blue")))

X <- select(data, press_count, gender, balloon_color)
X <- model.matrix(press_count ~ 1 + gender*balloon_color, data=X)

# beta's for gad7
#  (Intercept), gendermale, balloon_colorgreen, balloon_colorblue, 
#   gendermale:balloon_colorgreen, gendermale:balloon_colorblue
b <- c(8, 3, 0, 0, 0, 0) 
yhat <- X %*% b
data$gad7 <- round(rnorm(N, yhat, 3))

# sanity check looks good
summary(lm(gad7 ~ 1 + gender*balloon_color, data=data))

# beta's for press_count (depends on gad7)
X <- select(data, press_count, gender, balloon_color, gad7)
X <- mutate(data, gad7 = scale(gad7))
X <- model.matrix(press_count ~ 1 + gender*balloon_color + gad7, data=X)
# (Intercept), gendermale, balloon_colorgreen,
# balloon_colorblue, gad7, gendermale:balloon_colorgreen
# gendermale:balloon_colorblue
b <- c(24, 6, 30, 100, -2, -2, -6) 
yhat <- -1
while (any(yhat < 0)) { yhat <- X %*% b }

data$press_count <- round(rnorm(N, yhat, 5))

# sanity check looks good
summary(lm(press_count ~ 1 + gender*balloon_color + gad7, data=data))

data %>% write_csv("data/bart_assignment.csv")
