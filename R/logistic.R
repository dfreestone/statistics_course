#' ---
#' title: logistic.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.10.13"
#' purpose:
#' ---

logistic <- function(x) {return(1 / (1 + exp(-x)))}
data <- tibble(x = seq(-10, 10, length.out = 20),
               y_identity = 0 + 0.2*x,
               y_logistic = logistic(y_identity))

data %<>%
  pivot_longer(-x, names_to = "model", values_to = "y") %>%
  mutate(time = ifelse(model == "y_identity", 1, 2)) %>%
  arrange(time, x)

library(gganimate)
g <- ggplot(data,
       aes(x = x,
           y = y)) +
  geom_point(size = 3) +
  theme_bw() +
  scale_x_continuous(name = "x") +
  scale_y_continuous(name = "y") +
  transition_states(time,
                    transition_length = 1,
                    state_length = 0.5)

magick::image_write(animate(g), path="R/output/logistic.gif")

data <- tibble()
for (i in 1:60) {
  df <-  tibble() %>%
    tidyr::expand(gender = c("male", "female"),
                  ses = c("low", "medium", 'high'),
                  sex_orientation = c("straight", "gay", "transgender"))
  data <- bind_rows(data, df)
}

data %<>%
  mutate(family_support = rnorm(nrow(.), 0, 1),
         friend_support = rnorm(nrow(.), 0, 1),
         suicidal_thoughts = 1)

data %<>%
  mutate(gender = factor(gender, levels = c("male", "female")),
         ses = factor(ses, levels = c("medium", "low", "high")),
         sex_orientation = factor(sex_orientation, levels = c("straight", "gay", "transgender")))

X <- model.matrix(suicidal_thoughts ~ 1 + gender + ses + sex_orientation +
                    family_support + friend_support +
                    family_support:sex_orientation + friend_support:sex_orientation,
                  data = data)

coefficients <- tibble(name="", b=NA) %>%
  filter(!is.na(b)) %>%
  add_row(name = "(Intercept)",  b = -3) %>%
  add_row(name = "genderfemale", b = 0.35) %>%
  add_row(name = "seslow",       b = 0.75) %>%
  add_row(name = "seshigh",      b = 0) %>%
  add_row(name = "sex_orientationgay", b = 1.5) %>%
  add_row(name = "sex_orientationtransgender", b = 2.0) %>%
  add_row(name = "family_support", b = -1.0) %>%
  add_row(name = "friend_support", b = -0.5) %>%
  add_row(name = "sex_orientationgay:family_support",         b = -0.15) %>%
  add_row(name = "sex_orientationtransgender:family_support", b = -0.2) %>%
  add_row(name = "sex_orientationgay:friend_support",         b = -0.1) %>%
  add_row(name = "sex_orientationtransgender:friend_support", b = -0.1)


logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob2logit <- function(prob) {
  return(log(prob / (1-prob)))
}

data %<>%
  mutate(logitp = as.numeric(X %*% coefficients$b),
         p = logit2prob(logitp)) %>%
  rowwise() %>%
  mutate(suicidal_thoughts = rbinom(1, 1, p))

data %>%
  select(suicidal_thoughts, gender, ses, sex_orientation, family_support, friend_support) %>%
  write_csv("data/logistic.csv")

fit <- glm(suicidal_thoughts ~ 1 + gender + ses +
             sex_orientation*(family_support + friend_support),
           family = "binomial", data=data)

fit <- brms::brm(suicidal_thoughts ~ 1 + gender + ses +
              sex_orientation*(family_support + friend_support),
            family = "bernoulli",
            data = data)

s <- slice(data, 1:18)
s <- bind_cols(s, predict(fit, s, type = "link", se.fit = TRUE))
ggplot(s) +
  geom_histogram(aes(fit - logitp))
ggplot(s) +
  geom_point(aes(x=logitp, y=fit)) +
  geom_abline(intercept = 0, slope = 1)
