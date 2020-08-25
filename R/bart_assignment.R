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


# beta's for gad7
#  (Intercept), gendermale, balloon_colorgreen, balloon_colorblue,
X <- select(data, press_count, gender, balloon_color)
X <- model.matrix(press_count ~ 1 + gender + balloon_color, data=X)
b <- c(8, 8, 0, 0)
yhat <- X %*% b
y <- -1
while (any(y < 0 | y > 21)) {y <- round(rnorm(N, yhat, 3))}
data$gad7 <- y

# sanity check looks good
summary(lm(gad7 ~ 1 + gender + balloon_color, data=data))

# beta's for press_count (depends on gad7)
X <- select(data, press_count, gender, balloon_color, gad7)
X <- mutate(data, gad7 = scale(gad7))
X <- model.matrix(press_count ~ 1 + gender + balloon_color * gad7, data=X)
# (Intercept), gendermale, balloon_colorgreen, balloon_colorblue,
# gad7, balloon_colorgreen:gad7, balloon_colorblue:gad7
b <- c(24, 15, 30, 100, -2, -3, -4)
yhat <- X %*% b
y <- -1
while (any(y < 0)) {y <- round(rnorm(N, yhat, 3))}
data$press_count <- y

ggplot(data) +
  geom_point(
    aes(x = gad7,
        y = press_count,
        color = gender)
  )

# sanity check looks good
summary(lm(press_count ~ 1 + probability, data=data))
summary(lm(press_count ~ 1 + gad7, data=data))
summary(lm(press_count ~ 1 + balloon_color + gad7, data=data))
summary(lm(press_count ~ 1 + gender + balloon_color * gad7, data=data))
summary(lm(press_count ~ 1 + balloon_color * gad7, data=data))
summary(lm(press_count ~ 1 + balloon_color  + gender* gad7, data=data))

ggplot(data) +
  geom_point(
    aes(x = gad7,
        y = press_count,
        color = balloon_color)
  ) +
  geom_smooth(
    method = "lm",
    aes(x = gad7,
        y = press_count,
        group = balloon_color)
  )

ggplot(data) +
  geom_point(
    aes(x = gad7,
        y = press_count,
        color = gender)
  ) +
  geom_smooth(
    method = "lm",
    aes(x = gad7,
        y = press_count,
        group = gender)
  )

data %>% write_csv("data/bart_assignment.csv")

data <- read_csv("data/bart_assignment.csv")

summary(lm(press_count ~ 1 + gad7, data=data))
