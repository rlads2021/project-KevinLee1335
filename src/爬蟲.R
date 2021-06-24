
#####此檔案為爬蟲存檔到讀檔

library(stringr)
library(RSelenium)
library(readr)
library(tidyverse)
library(tibble)
library(jiebaR)

#開啟chrome

remDr <- remoteDriver(browserName ="chrome")
remDr$open() 


###抓取108、109，5~12月判決文(各500篇，共8000篇)
year_url <- list("https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=d1adc3c43679830e8e347a5353e227e1&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=74762e3fb20690f1f29946c98277de48&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=6cbbd127326d1b0b067fd9c5bcad55a6&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=26ec22f95b877a7aa00aff90bfc95aab&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=1e0b5d026492f96a78ff72e77cebaa46&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=376bc7cf729206144b23f0b1ed20d148&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=d5f048132cc1b8134ec010cb62609d07&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=51e715cd62bb7e59f51ee1a36d4e9efc&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=3172f734816c421bf0e2efa30f68b643&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=db2953aeaee3aa29fe10c9a3b12bea89&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=c682ee6e49992198769fc2a28eb81f5f&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=12b58df2bfb8a4fca1cae32a58bf26c6&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=b4ee44ad8165bf47e83abe7d4463972f&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=10658fc25fe932ec5c9c8a6eb6f46430&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=21ef91e91e5198df68950d31a6409867&sort=DS&ot=in",
                 "https://law.judicial.gov.tw/FJUD/data.aspx?ro=0&q=ae9b8482c818893729249f45ecb29301&sort=DS&ot=in")



#108、109，5~12月判決文(各500篇，共8000篇)存至year_jud_list
year_jud_list <- list()
for (i in seq_along(year_url)){
  remDr$navigate(year_url[[i]])
  
  for (j in 1:500){
    #抓到判決文標籤
    jud <- remDr$findElement(using = 'css selector',
                             value = '#jud')
    #將處理好後之判決文存入year_jud_list
    judtext <- jud$getElementText() %>%
      str_replace_all("\n", "") %>%
      str_replace_all(" ", "") %>%
      str_replace_all("　", "")
    
    year_jud_list <- append(year_jud_list, judtext)
    
    #進入下一篇判決
    next_page <- remDr$findElement(using = 'css selector',
                                   value = '#hlNext')
    
    next_page$clickElement()

  }
}

###將year_jud_list內的判決分別存入桌面資料夾(並標檔名)

path <- "C:\\Users\\user\\Desktop\\judgement_108_109_8000\\"
txt <- ".txt" 

year <- "108"
a <- 0

for(i in seq_along(year_jud_list)){
  if (a == 4000){
    year <- as.character(as.numeric(year) + 1)
    a <- 0
  }

  name <- paste0(year, "_", as.character(i))
  
  filename <- paste0(path, name, txt)
  write.table(year_jud_list[i],
              file = filename,
              sep = " ",
              quote = FALSE,
              na = "NA",
              col.names = FALSE,
              fileEncoding = "UTF-8")
  
  a <- a + 1
  
  
}






###讀檔，讀進year_jud_list
year_jud_list <- list()
#取出各檔案完整路徑

path_list <- list.files("judgement_108_109_8000",
                        full.names = TRUE)
                    

seg <- worker(user = "segment\\如主文.txt",
              qmax = 1000)

#依次讀進year_jud_list



year_jud_list <- write(path_list, seg)


