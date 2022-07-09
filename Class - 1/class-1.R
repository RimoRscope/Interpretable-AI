library(MASS)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(leaps)
library(mgcv)
library(glmnet)
library(boot)


#loading data
data(Boston)
dim(Boston)


head(Boston)

corrplot(cor(Boston), method = "number", type = "upper", diag = FALSE)

Boston %>%
  gather(key, val, -medv) %>%
  ggplot(aes(x = val, y = medv)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, col = "blue") +
  facet_wrap(~key, scales = "free") +
  theme_gray() +
  ggtitle("Scatter plot of dependent variables vs Median Value (medv)") 


data("BostonHousing", package = "mlbench")
BostonHousing <- transform(BostonHousing,
                           chas = factor(chas, levels = 0:1, labels = c("no", "yes")),
                           rad = factor(rad, ordered = TRUE))



library(ggparty)
## linear model tree
bh_tree <- lmtree(medv ~ log(lstat) + I(rm^2) | zn +
                    indus + chas + nox + age + dis + rad + tax + crim + b + ptratio,
                  data = BostonHousing, minsize = 40)


bh_plot <- ggparty(bh_tree, terminal_space = 0.5) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  # plot first row
  geom_node_plot(gglist = list(
    geom_point(aes(y = medv, x = `log(lstat)`, col = chas),
               alpha = 0.6)),
    # halving the height shrinks plots towards the top
    height = 0.5) +
  # plot second row
  geom_node_plot(gglist = list(
    geom_point(aes(y = medv, x = `I(rm^2)`, col = chas),
               alpha = 0.6)),
    height = 0.5,
    # move -0.25 y to use the bottom half of the terminal space
    nudge_y = -0.25)

bh_plot+theme_grey()







