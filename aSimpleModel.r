library(tidyverse)
library(modelr)
#plot model
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "#2DC6D6")

#generating a few and overlaying them on the data
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()

#turn our model family into an R function
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)
## [1]  8.5  8.5  8.5 10.0 10.0 10.0 11.5 11.5
##[9] 11.5 13.0 13.0 13.0 14.5 14.5 14.5 16.0
##[17] 16.0 16.0 17.5 17.5 17.5 19.0 19.0 19.0
##[25] 20.5 20.5 20.5 22.0 22.0 22.0

#compute the difference between actual and predicted, square them, average them, and the take the square root
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)
##[1] 2.665212

#use purrr to compute the distance for all the models defined above
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models
## # A tibble: 250 × 3
##a1     a2  dist
##<dbl>  <dbl> <dbl>
##  1  30.4    2.92   31.2
##2  27.2   -0.114  12.9
##3  27.2    2.06   23.2
##4  -3.00  -1.89   31.1
##5  18.1   -1.39   11.3
##6   0.551 -4.55   44.3
##7  -3.21  -0.464  22.6
##8  27.4   -0.701  11.5
##9 -18.5    4.62   11.5
##10  26.7    3.33   29.8
## # … with 240 more rows

#overlay the 10 best models on to the data. I’ve coloured the models by -dist#
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  ) +
  scale_color_continuous(low = "#2097A3", high = "#95E1EA")

#visualising with a scatterplot of ..., again coloured by -dist.
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) +
  scale_color_continuous(low = "#2097A3", high = "#95E1EA")

#generate an evenly spaced grid of points (this is called a grid search)
grid <- expand_grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

#overlay the best 10 models back on the original data,
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

#to tackle that problem: a numerical minimization tool called Newton-Raphson search. The intuition of Newton-Raphson is pretty simple: you pick a starting point and look around for the steepest slope. You then ski down that slope a little way, and then repeat again and again, until you can’t go any lower. In R, we can do that with optim():
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
## [1] 4.22 2.05

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

#lm() will translate to a function like .. We can fit the model and look at the output:
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)
## (Intercept)           x 
##        4.22        2.05

