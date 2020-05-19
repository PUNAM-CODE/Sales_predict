############ LIBRARIES #####

library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(e1071)
library(gbm)
library(data.table)
library(tictoc)

test <- read_csv("C:/Users/admin/Desktop/data science/project/predicfeturesaIes/test.csv")

sales_train <- read_csv("C:/Users/admin/Desktop/data science/project/predicfeturesaIes/sales_train.csv")

items <- read_csv("C:/Users/admin/Desktop/data science/project/predicfeturesaIes/items.csv")

dim(test)
dim(sales_train)
dim(items)

sum(is.na(items))

sum(is.na(test))

sum(is.na(sales_train))

summary(sales_train)

summary(test)

summary(items)

glimpse(sales_train)
glimpse(items)
glimpse(test)


sales_data = merge(sales_train, items[,c("item_id", "item_category_id")], by = "item_id", all.x = T)
sales_data$date = as.Date(sales_data$date, "%d.%m.%Y")

View(sales_data)

dim(sales_data)

reg1<-lm(item_cnt_day~.,data=sales_data)
summary(reg1)
predict<-predict(reg1,test[,c("shop_id","item_id")])

reg1$residuals
sum(reg1$residuals)

mean(reg1$residuals)
sqrt(sum(reg1$residuals^2)/nrow(sales_data))  #RMSE

sqrt(mean(reg1$residuals^2))

confint(reg1,level=0.95)
predict(reg1,interval="predict")

linear_model = lm(formula = item_cnt_day ~ shop_id + item_id,
                  data = sales_data) 
linear_model
summary(linear_model)

result = predict(linear_model, test[,c("shop_id","item_id")]) 

submission =  data.frame(ID = test$ID,
                         item_cnt_month = result)
head(submission)
write.csv(submission, "submission1.csv", row.names = F)

# GBM Model
library(tictoc)
tic("Time Taken to Run GBM Model ")
gbm_model  =  gbm(item_cnt_day ~ shop_id + item_id,
                  data = sales_data,
                  shrinkage = 0.01,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 5, 
                  bag.fraction = 0.5,
                  train.fraction = 0.8,
                  # cv.folds = 5,
                  n.cores = -1,
                  verbose = T)

toc()

summary(gbm_model)
result2 = predict(gbm_model,newdata = test[c("shop_id","item_id")], n.trees = 1000)
summary(result2)
str(result2)

sub2 = data.frame(ID = test$ID, 
                  item_cnt_month =  result2)
ggplot(data = items_in_shop,
       mapping = aes(x = reorder(shop_id,item_id),
                     y = item_id,
                     fill = factor(shop_id)))+
  geom_histogram(stat = "identity", color = "yellow") +
  xlab(" Shop ID")+ ylab(" Items in shop")+
  ggtitle("Most Items in Shops") +
  coord_flip()+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
    # Change plot and panel background
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    # legend.position = c(0.6, 0.07),
    # legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # align title to top center, top ledt is by default.
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    # axis ticks to bold black
    axis.text=element_text(colour = "yellow",face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  )

write.csv(sub2, "sub2.csv", row.names = F)


install.packages("tictoc", type = "source")
install.packages("gbm")


