#' ---
#' title: logistic.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.10.13"
#' purpose:
#' ---

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob2logit <- function(prob) {
  return(log(prob / (1-prob)))
}
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

data <- read_delim("~/Desktop/wells.dat", delim=" ") %>%
  select(-N) %>%
  write_csv("data/wells.csv")
