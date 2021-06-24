
###以下為專案所用到的自訂函數


###讀檔所用函數
write <- function(path_list, seg){
  ans_jud_list <- list()
  
  for (i in seq_along(path_list)){
    x2 <- ""
    x1 <- read_delim(path_list[[i]], 
                     "\t", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
    x1 <- deframe(x1)
    #以下為剔除不需要的文句(法官...以下)
    segged <- segment(x1, seg)
    l <- grep("法官", segged)
    for (j in 1:l){
      x2 <- paste0(x2, segged[j])
    }
    ans_jud_list <- append(ans_jud_list, x2)
    
  }
  return(ans_jud_list)
}




###資料整理所用的函數(6/12 15:00 更新)


###1.判斷法院地
Court_f <- function(input){
  loc <- grep("地方法院",input)[1]
  
  place <- input[loc - 1]
  
  if(place == "基隆"){
    ans <- "基隆地方法院"
  }
  else if (place == "臺北"){
    ans <- "臺北地方法院"
  }
  else if (place == "新北"){
    ans <- "新北地方法院"
  }
  else if (place == "桃園"){
    ans <- "桃園地方法院"
  }
  else if (place == "新竹"){
    ans <- "新竹地方法院"
  }
  else if (place == "苗栗"){
    ans <- "苗栗地方法院"
  }
  else if (place == "臺中"){
    ans <- "臺中地方法院"
  }
  else if (place == "彰化"){
    ans <- "彰化地方法院"
  }
  else if (place == "雲林"){
    ans <- "雲林地方法院"
  }
  else if (place == "嘉義"){
    ans <- "嘉義地方法院"
  }
  else if (place == "臺南"){
    ans <- "臺南地方法院"
  }
  else if (place == "高雄"){
    ans <- "高雄地方法院"
  }
  else if (place == "屏東"){
    ans <- "屏東地方法院"
  }
  else if (place == "臺東"){
    ans <- "臺東地方法院"
  }
  else if (place == "花蓮"){
    ans <- "花蓮地方法院"
  }
  else if (place == "宜蘭"){
    ans <- "宜蘭地方法院"
  }
  else if (place == "南投"){
    ans <- "南投地方法院"
  }
  else if (place == "澎湖"){
    ans <- "澎湖地方法院"
  }
  else if (place == "橋頭"){
    ans <- "橋頭地方法院"
  }
  else if (place == "金門"){
    ans <- "金門地方法院"
  }
  else if (place == "連江"){
    ans <- "連江地方法院"
  }
  else if (place == "士林"){
    ans <- "士林地方法院"
  }
  
  else{
    ans <- NA
  }
  
  return(ans)
}



###2.判斷日期
Date_f <- function(input){
  loc1 <- grep("裁判日期民國", input)
  loc2 <- grep("裁判案由", input)
  
  l1 <- as.integer(loc1) + 1
  l2 <- as.integer(loc2) - 1
  
  ans <- paste0(input[l1:l2], collapse = "")
  
  return(ans)
}







###3.判斷刑度的值(用國字表示)

Result_f <- function(input){
  resu <- 0
  loc <- grep("處有期徒刑",input)[1]
  ans <- substr(input[loc], 6, 7)
  #「叁」顯示不出來，無法抓
  return(ans)
  
}



###4.判斷刑度的區間

Result_level_f <- function(input){
  resu <- 0
  loc <- grep("處有期徒刑",input)[1]
  mon <- substr(input[loc], 6, 7)
  #「叁」顯示不出來，無法抓
  
  #輕案
  if (mon %in% c("壹月", "貳月")){
    ans <- "short"
  }
  
  #中案  
  else if (mon %in% c("參月", "肆月", "伍月","陸月")){
    ans <-"medium"
  }
  
  #重案
  else if (mon %in% c("柒月", "捌月", "玖月","拾月",
                      "甘月", "卅月", "壹年", "貳年",
                      "參年")){
    ans <- "long"
  }
  else{
    ans <- NA
  }
  
  return(ans)
  
}


###5.判斷酒精值數值
Alcohol_f <- function(input){
  loc <- grep("毫克", input)
  if (length(loc) == 0){
    ans <- NA
  }

  else{
    loc <- loc[1]
    if (input[loc - 1] %in% x){
      d <- as.numeric(input[loc - 1])
      ans <- d
    }
   else{
     ans <- NA
   }
  }
  
  return(ans)
}




###6.判斷酒精值區間
Alcohol_level_f <- function(input){
  d <- Alcohol_f(input)
  
  #判斷不到酒精值
  if (is.na(d)){
    ans <- NA
  }
  else{
    posi <- seq(from = 0.26, to = 0.55, by = 0.01)
    med <- seq(from = 0.55, to = 0.85, by = 0.01)
    neg <- seq(from = 0.86, to = 1.25, by = 0.01)
    if(d > 0.25 & d < 0.55){
      ans <- "low"
    }
    else if(d >= 0.55 & d < 0.85){
      ans <- "medium"
    }
    else if (d >= 0.85){
      ans <- "high"
    }
    else{ #不在以上範圍
      ans <- NA
    }
  }
  
  return(ans)
}



###7.判斷學歷情況
Diploma_f <- function(input){
  
  posi <- c("國小畢業", "國中畢業", 
            "國小肄業","國中肄業","不識字",
            "輕度身心障礙","身心障礙")
  
  med <- c("高中畢業", "高職畢業", "專科畢業",
           "高中肄業", "高職肄業","專科肄業")
  
  neg<-c("大學畢業","碩士畢業","研究所",
         "高學歷", "博士畢業","碩士肄業",
         "博士肄業","研究所學歷","大學肄業")
  
  if(any(posi %in% input)){
    ans <- "low"
  }
  else if(any(med %in% input)){
    ans <- "medium"
  }
  else if (any(neg %in% input)){
    ans <- "high"
  }
  else{ #沒提到
    ans <- NA
  }
  return(ans)
}



###8.判斷經濟情況
Economy_f <- function(input){
  
  posi <- c("貧困", "清寒", "貧窮",
            "貧寒","低收", "中低收",
            "移工", "經濟較為弱勢",
            "經濟弱勢")
  med <- c("勉持", "小康")
  
  neg<-c("富裕")
  
  if(any(posi %in% input)){
    ans <- "poor"
  }
  else if(any(med %in% input)){
    ans <- "medium"
  }
  else if (any(neg %in% input)){
    ans <- "rich"
  }
  else{ #沒提到
    ans <- NA
  }
  return(ans)
}


###9.判斷是否累犯
Again_f <- function(input){
  if ("累犯" %in% input){
    ans <- "Yes"
  }
  else{
    ans <- "No"
  }
  return(ans)
}




###10.判斷是否肇事

Accident_f <- function(input){

  acc <- NA   #未提及
  
  ##未肇事的詞彙於丟進函數前已先建立
  
  neg <- c("致人受傷")
  if(any(posi %in% input)){
    acc <- "No"
  }
  else if(any(neg %in% input)){
    acc <- "Yes"
  }
  return(acc)
}




###11.判斷犯後態度
Attitude_f <- function(input){
  posi <- c("態度尚佳","態度尚可","態度良好",
            "坦承不諱", "坦承犯行", "坦認不諱")

  
  neg<-c("態度惡劣", "詎不認錯","態度強硬")
  
  if(any(posi %in% input)){
    ans <- "good"
  }
  else if (any(neg %in% input)){
    ans <- "bad"
  }
  else{ #沒提到
    ans <- NA
  }
  return(ans)
}


  
  





















