install.packages("readxl")
DAY <- readxl::read_excel("C:/Users/sucha/Downloads/1657875746_day.xlsx")
DAY <- read.csv(file.choose())

View(DAY)
str(DAY)

sum(is.na(DAY))






install.packages("lubridate")
library(lubridate)
library(ggplot2)



DAY$dteday <- ymd(DAY$dteday)


qplot(dteday,cnt,data = DAY,
      main =  'NO. OF BIKES RENTED MONTHLY')









set.seed(1234)
train_ind <- sample(1:nrow(DAY),round(0.7*nrow(DAY)))

train_DY <- DAY[train_ind, ]
test_DY <- DAY[-train_ind, ]
train_DY
test_DY









dt <- rpart(cnt ~., data = test_DY)

dt
View(test_DY)

table(test_DY$cnt)
 
table(test_DY$cnt)/nrow(test_DY)
  
library(rpart.plot)
rpart.plot(dt)











ctrl <- rpart.control(minsplit = 2,
                      minbucket = 1 , cp = 0.00001)


dt <- rpart(cnt ~., data = test_DY, control = ctrl)
dt
rpart.plot(dt)

219*3/100
dt$cptable

cptable_1 <- as.data.frame(dt$cptable)
plot(cptable_1$CP,cptable_1$xerror )

plotcp(dt)



dt_pruned <- rpart(cnt ~., data = test_DY,
                   control = rpart.control(minbucket = 7,
                                           minsplit = 7*3,
                                           cp = 0.026))


dt_pruned
rpart.plot(dt_pruned)

library(caret)

pred_test_dt <- factor(predict(dt, test_DY,type = 'vector'))
pred_test_pr <- factor(predict(dt_pruned, test_DY,type = 'vector'))
test_DY <-  as.factor(test_DY$cnt)

caret::confusionMatrix( pred_test_dt,test_DY)
caret::confusionMatrix(pred_test_pr,test_DY)
caret::confusionMatrix(data =pred_test_dt,reference = as.factor(test_DY$cnt) )
levels(pred_test_pr)





table(train_DY[,1])
table(test_DY[,1])

library(randomForest)
library(dplyr)
sapply(DAY, class)





set.seed(1234)
train_ind <- sample(1:nrow(DAY),round(0.7*nrow(DAY)))

train_DY <- droplevels(DAY[train_ind, ])
test_DY <- DAY[-train_ind, ]
train_DY
test_DY






rf <-  randomForest(cnt ~., data = train_DY,
                    ntree = 1000, 
                    mtry = 3, 
                    nodesize = 10, 
                    importance = TRUE)
rf



plot(rf)

















