###本檔案為資料整理(擷取)
library(stringr)
library(jiebaR)
library(quanteda)
library(tidytext)
library(dplyr)
library(tibble)
library(readr)




jud_vec <- unlist(year_jud_list)

###以下為透過呼叫各函數，做資料擷取，取出我們要的值

court_list <- list()
date_list <- list()
result_list <- list()
result_level_list <- list()
alcohol_list <- list()
alcohol_level_list <- list()
diploma_list <- list()
economy_list <- list()
again_list <- list()
accident_list <- list()
attitude_list <- list()




#1.依法院地切割後，將其傳入Court_f判斷
seg <- worker(user = "segment/法院地.txt",
              qmax = 1000)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Court_f(segged)
  court_list <- append(court_list, ans)
  
}



#2.依日期切割後，將其傳入Date_f判斷
seg <- worker(user = "segment\\日期.txt",
              qmax = 1000)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Date_f(segged)
  date_list <- append(date_list, ans)
}


#3.依刑度切割後，將其傳入Result_f判斷其刑度值
seg <- worker(user = "segment\\刑度.txt",
              qmax = 1000)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Result_f(segged)
  result_list <- append(result_list, ans)
}

#4.依刑度切割後，將其傳入Result_level_f判斷其刑度區間
seg <- worker(user = "segment\\刑度.txt",
              qmax = 1000)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Result_level_f(segged)
  result_level_list <- append(result_level_list, ans)
  
}




#5.依酒精值切割後，將其傳入Alcohol_f判斷值
seg <- worker(user = "segment/酒精值.txt",
              qmax = 1000)
x <- read_csv("segment/酒精值.txt",
                 col_names = FALSE,
                 locale = locale(encoding = "UTF-8"))

#將讀進來的x(tibble)轉成向量以便切割
x <- deframe(x)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Alcohol_f(segged)
  alcohol_list <- append(alcohol_list, ans)
  
}



#6.依酒精值切割後，將其傳入Alcohol_level_f判斷值的區間
seg <- worker(user = "segment/酒精值.txt",
              qmax = 1000)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Alcohol_level_f(segged)
  alcohol_level_list <- append(alcohol_level_list, ans)
  
}




#7.依智識切割後，將其傳入Diploma_f判斷
seg <- worker(user = "segment/智識.txt",
              qmax = 1000)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Diploma_f(segged)
  diploma_list <- append(diploma_list, ans)
  
}




#8.依經濟切割後，將其傳入Economy_f判斷
seg <- worker(user = "segment/經濟.txt",
              qmax = 1000)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Economy_f(segged)
  economy_list <- append(economy_list, ans)
  
}



#9.依是否累犯切割後，將其傳入Again_f判斷
seg <- worker(user = "segment\\累犯.txt",
              qmax = 1000)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Again_f(segged)
  again_list <- append(again_list, ans)
}





#10.依肇事切割後，將其傳入Accident_f判斷
seg <- worker(user = "segment/肇事.txt",
              qmax = 1000)

#因未肇事的詞語較多，未節省程式運算時間，將詞彙在迴圈外讀入
posi <- read_csv("segment/肇事.txt",
                 col_names = FALSE,
                 locale = locale(encoding = "UTF-8"))

posi <- deframe(posi)
for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Accident_f(segged)
  accident_list <- append(accident_list, ans)
  
}



#11.依犯後態度切割後，將其傳入Attitude_f判斷
seg <- worker(user = "segment\\態度.txt",
              qmax = 1000)

for (i in seq_along(jud_vec)) {
  segged <- segment(jud_vec[i], seg)
  ans <- Attitude_f(segged)
  attitude_list <- append(attitude_list, ans)
}




###以下為將這些值存進一tibble(row 為判決，column 為上述取的值)

court_vec <- unlist(court_list)
date_vec <- unlist(date_list)
result_vec <- unlist(result_list)
result_level_vec <- unlist(result_level_list)
alcohol_vec <- unlist(alcohol_list)
alcohol_level_vec <- unlist(alcohol_level_list)
diploma_vec <- unlist(diploma_list)
economy_vec <- unlist(economy_list)
again_vec <- unlist(again_list)
accident_vec <- unlist(accident_list)
attitude_vec <- unlist(attitude_list)



jud_df <- tibble::tibble(
  
  jud_id = seq_along(court_vec),
  court_place = court_vec,
  date = date_vec,
  result = result_vec,
  result_level = result_level_vec,
  alcohol = alcohol_vec,
  alcohol_level = alcohol_level_vec,
  diploma = diploma_vec,
  economy = economy_vec,
  again = again_vec,
  accident = accident_vec,
  attitude = attitude_vec
  
)

View(jud_df)


