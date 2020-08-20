#' ---
#' title: create_schools_example.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.20"
#' purpose:
#' ---

library(tidyverse)
schools <- c("Rutgers", "Montclair", "Princeton", "Stockton")
years <- c("first-year", "sophomore", "junior", "senior")

X <- list(school = schools, year = years) %>%
  cross_df()

for (i in 1:6) {
  X <- bind_rows(X, X)
}

data <- mutate(X, PHQ9 = 1)

X <- model.matrix(PHQ9 ~ school + year + school:year,
             data = data)

b <- rep(0, dim(X)[2])
b[1:4] <- c(18, 2, -3, -4) # Schools, (Intercept), Princeton, Rutgers, Stockton
b[5:7] <- c(-2, 3, 1) # junior, senior, sophomore
b[13] <- 6 # Stockton, senior year

y <- -1
while (min(y) < 9 | max(y) > 27) {
  y <- as.numeric(X %*% b)
  y <- rnorm(length(y), y, 1.5)
  y <- round(y)
}

data$PHQ9 <- y

# shuffle the data
data <- sample_frac(data, 1L)


model <- formula(PHQ9 ~ 1 + school)
fit <- lm(model, data)
summary(fit)

model <- formula(PHQ9 ~ 1 + year)
fit <- lm(model, data)
summary(fit)

model <- formula(PHQ9 ~ 1 + school + year)
fit <- lm(model, data)
summary(fit)

model <- formula(PHQ9 ~ 1 + school + year + school:year)
fit <- lm(model, data)
summary(fit)

data %>% write_csv("data/schools_depression.csv")
