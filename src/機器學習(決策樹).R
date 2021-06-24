###以下為機器學習的部分

library(ggplot2)
library(dplyr)

View(jud_df)


###只要有缺失值，就把該row刪掉

tidytest <- jud_df %>%
  filter(is.na(court_place) == FALSE & 
           is.na(date) == FALSE &
           is.na(result) == FALSE &
           is.na(result_level) == FALSE &
           is.na(alcohol) == FALSE &
           is.na(alcohol_level) == FALSE &
           is.na(diploma) == FALSE &
           is.na(economy) == FALSE &
           is.na(again) == FALSE &
           is.na(accident) == FALSE &
           is.na(attitude) == FALSE) 
View(tidytest)




#將tidytest隨機排序

suffle_index<-sample(x = 1 : nrow(tidytest))
head(suffle_index)

tidytest <- tidytest[suffle_index,]
head(tidytest)    

#刪除不必要變數，jud_id、court_place、date、alcohol_level、result
tidyenter <- tidytest %>% select(-c(jud_id, court_place, date,
                                    alcohol_level, result))
glimpse(tidyenter)
summary(tidyenter)


#select data to traning ratio 7:3
#將刑度低中高分成三dataframe
inputones <- tidyenter %>% filter(result_level == "short")
inputwo <- tidyenter %>% filter(result_level == "medium")
inputhree <- tidyenter %>% filter(result_level == "long")


#將三dataframe 各取七成的row出來訓練
#並確保每次執行之隨機數一樣
set.seed(6666) 
inputones_training_row <- sample(1:nrow(inputones), 0.7*nrow(inputones))
inputwo_traing_row <- sample(1:nrow(inputwo),0.7*nrow(inputwo))
inputhree_traing_row <- sample(1:nrow(inputhree),0.7*nrow(inputhree))

training_ones <- inputones[inputones_training_row,]
training_two <- inputwo[inputwo_traing_row,]
training_three <- inputhree[inputhree_traing_row,]



#將這三個dataframe各取出的七成值(訓練模型用)，組成在 trainingdata
trainingdata <- rbind(training_ones,training_two,training_three)

trainingdata %>%
  filter(result_level == "long")


#testing data
##將這三個dataframe各取出的三成值(測試準確率用)，組成在 testdata
testones <- inputones[-inputones_training_row,]
testwo <- inputwo[-inputwo_traing_row,]
testhree <- inputhree[-inputhree_traing_row,]
testdata <- rbind(testones,testwo,testhree)


dim(trainingdata)
dim(testdata)


  

#check if it is random
#table() 為算出各元素出現的次數
#prop.table()為算出各次數的機率(百分比) 
#兩組相差小於1%
prop.table(table(trainingdata$result_level))
prop.table(table(testdata$result_level))


###以下開始訓練模型
library(rpart)
library(rpart.plot)
library(partykit)

# 想要預測刑期區間，並以alcohol、diploma、economy、
# again、accident、attitude變數作為影響項目
# class=用於分類變數
#建立決策樹
fit <- rpart(formula = result_level~alcohol+diploma+
               economy+again+accident,
             data = trainingdata,
             method = 'class')


#畫出決策樹圖
rpart.plot(fit,
           extra = 106,
           faclen = 0,
           shadow.col = "gray",
           box.palette = "auto")

##計算模型準確率

predicted <- predict(object = fit,
                     newdata = testdata,
                     type = 'class')


tb1<-table(predicted, testdata$result_level)
tb1

accuracy <- sum(diag(tb1) / sum(tb1))
accuracy











