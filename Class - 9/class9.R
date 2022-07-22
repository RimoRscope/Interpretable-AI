library(readr)
df <- read.csv("winequality-red")

library(tidyverse)
library(skimr)

glimpse(df)

df <- df%>%
  mutate(quality = as.factor(ifelse(quality < 6, "bad", "good")))

glimpse(df)

colnames(df) <- gsub(" ", "_", colnames(df))

View(df)

################################EDA#########################

plot <- df %>%
  ggplot(aes(x = quality, fill = quality))+
  geom_bar(alpha = 0.8) + 
  scale_fill_tableau()+
  guides(scale = "none")

view(df)

library(caret)
library(gridExtra)
library(ggridges)
library(ggthemes)
theme_set(theme_grey())

plot2 <- df%>%
  gather(x, y, fixed_acidity:alcohol) %>%
  ggplot(aes(x = y, y = quality, fill = quality))+
  geom_density_ridges()


###Facetting

plot2 <- df%>%
  gather(x, y, fixed_acidity:alcohol) %>%
  ggplot(aes(x = y, y = quality, color=quality, fill = quality))+
  facet_wrap(~ x, scale = "free", ncol = 3)+
  scale_fill_tableau()+
  scale_color_tableau()+
  geom_density_ridges()


library(patchwork)

plot + plot2

grid.arrange(plot, plot2, ncol = 2, widths = c(0.4,0.8))



#########################MODELLING##########################



set.seed(42)

index <- createDataPartition(df$quality,
                             p = 0.8,
                             list = FALSE,
                             times = 1)

train <- df[index,]
class(train)


test <- df[-index,]



dim(train)

dim(test)


ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2)

library(doParallel)
makeCluster(detectCores())


set.seed(123)

model_rf <- train(quality ~ .,
                  data =  train,
                  method = "rf",
                  preProcess = c("scale" ,"center"),
                  trcontrol = ctrl,
                  verbose = FALSE)

model_pred <- predict(model_rf, test)

confusionMatrix(model_pred, as.factor(test$quality))

vimp_rf <- varImp(model_rf)

plot(vimp_rf)

class(vimp_rf$importance)

plot3 <- vimp_rf$importance %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  ggplot(aes(x = reorder(rowname, Overall), y = Overall))+
  geom_bar(stat = "identity", fill = "#1F77B4", alpha = 0.8)+
  coord_flip()
  

imp_roc <- filterVarImp(x = df[, -ncol(train)], y = train$quality)


plot4 <- imp_roc %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  ggplot(aes(x = reorder(rowname, good), y = good))+
  geom_bar(stat = "identity", fill = "#1F77B4", alpha = 0.8)+
  coord_flip()



plot3 + plot4

##########OR###############

grid.arrange(plot3, plot4, ncol = 2, widths = c(0.5,0.5))



newVar <- train %>%
  select(-quality) %>%
  as.data.frame()

View(newVar)


install.packages("iml")

library("iml")


newModel <- Predictor$new(model_rf, data = newVar, y = newVar$quality)


str(newModel)

pdp_plot <- Partial$new(newModel, feature = "alcohol")
pdp_plot$center(min(train$alcohol))

glimpse(pdp_plot$results)


pdp_plot$plot()
