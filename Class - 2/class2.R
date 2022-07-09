library(modeldata)
library(tidyverse)
library(naniar)
library(tidymodels)
library(ggplot2)


data("ames")

ggplot(ames, aes(x = Sale_Price))+
  geom_histogram(bins = 50, col = "white")


#holdout method
#split two parts based on percentage

set.seed(123)

df_tr <- initial_split(ames, prop = 0.8)
#80/100 = 0.8

train <- training(df_tr)
test <- testing(df_tr)


model_lm <- linear_reg() %>% set_engine("lm")

linear_reg()%>%set_engine("lm")%>%translate()
  
lmf <-
  model_lm %>%
  fit_xy(
    x = train %>% select(First_Flr_SF, Second_Flr_SF),
    y = train %>% select(Sale_Price)
  )

lmf

#RMSE, R-Sq, MAPE, MAE, EVS, RMSLE, SMAPE

glimpse(df)

library(corrplot)

mycor <- cor(df)

corrplot(mycor)


df %>%
ggplot(aes(x = Appliances))+
  geom_histogram(bins = 30, col = "white")


model_lm <- linear_reg(penalty = 1) %>% set_engine("glmnet")

lmf <-
  model_lm %>%
  fit_xy(
    x = df %>% select(lights, Windspeed, Visibility),
    y = df %>% pull(Appliances)
  )
