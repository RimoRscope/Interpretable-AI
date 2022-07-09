library(tidyverse)

df <- tibble(x = 1:6,
             y = c("Sam","Alice","Bob","Allen", "John", "Doe"))

df

df <- c(1,2,3,4)

df

df <- as.data.frame(df)

library(tibble)

data("CO2")

dff <- as_tibble(CO2)

dff

library(dplyr)

sData <- select(dff, 1, 4)

sData

fdata <- filter(sData, conc > 700)

fdata


#data wrangling/data manipulation

gData <- group_by(dff, uptake)

gData

MathData <- summarize(gData, m_conc = mean(uptake))

MathData

#mutation 

?sd

y <- sd(gData$uptake)

dff

sorted <- arrange(dff, uptake)

dff %>% arrange(desc(uptake))

df <- c(10,20,30,40,50)

df

df %>% mean()

library(MASS)

data("Boston")

glimpse(Boston)

library(dplyr)

DBoston <- Boston %>%
  filter(indus > 3) %>%
  group_by(zn) %>%
  summarize(meanX = mean(zn)) %>%
  mutate(ZZZ = (meanX/100)*60) %>%
  arrange(zn)


plot <- ggplot(Boston, aes(x = zn, y= indus))+
  geom_point()

plot + theme_gray()


plot <- ggplot(Boston, aes(x = zn, y= indus))

plot +
  geom_line()

library(tidymodels)
set.seed(123)

tx_split <- initial_split(Boston, prop = 0.75)

Dtrain <- training(tx_split)

Dtest <- testing(tx_split)

model_lm <- linear_reg() %>% set_engine("lm")

linear_fit <-
  model_lm %>%
  fit(medv ~ ., data = Dtrain)


predict(linear_fit, new_data = Dtest)

var <- Dtest$medv

Dtest %>%
  #select(var) %>%
  bind_cols(predict(linear_fit, Dtest))
  #ind_cols(predict(linear_fit, Dtest), type = "pred_int")

