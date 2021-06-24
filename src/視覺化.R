###以下為製圖


library(ggplot2)
library(dplyr)


#real number
dipplot2 <- jud_df %>% 
  filter(is.na(diploma) == FALSE ,is.na(result_level)==FALSE)

ggplot(data = dipplot2) + 
  geom_bar(mapping = aes(x = diploma, fill = result_level))+
  labs(title = "diploma vs result_level (total=4485)")+



#achochol

alcoplot2 <- jud_df %>%
  filter(is.na(alcohol) == FALSE,
         is.na(result) == FALSE,
         is.na(result_level) == FALSE,
         alcohol != 0,
         alcohol > 0.25)

ggplot(data = alcoplot2, na.rm = TRUE)+
  geom_point(aes(x = result_level, y = alcohol, color = factor(result)))+
  labs(title = "alchohol vs result (total=7081)")



alcoplot <- jud_df %>% 
  filter(is.na(alcohol) == FALSE,
         is.na(result) == FALSE,
         alcohol != 0)

ggplot(data = alcoplot)+
  geom_point(aes(x = result, y = alcohol, color = factor(judge)))+
  labs(title = "alchohol vs month (total=7661)")


#累犯
againplot <- jud_df %>%
  filter(is.na(again) == FALSE,
         is.na(result) == FALSE,
         is.na(result_level) == FALSE)

ggplot(data = againplot)+
  geom_bar(aes(x = again, fill = result_level), stat = "count")+
  labs(title = "again vs month (total=7679)")


#place court

courtplot <- jud_df %>%
  filter(is.na(court_place) == FALSE,
         is.na(result) == FALSE,
         is.na(result_level) == FALSE)

ggplot(data = courtplot)+
  geom_bar(aes(x = court_place, fill = result_level), stat = "count")+
  labs(title = "court vs month (total=7678)")


#economic
ecoplot <- jud_df %>%
  filter(is.na(economy) == FALSE,
         is.na(result) == FALSE,
         is.na(result_level) == FALSE)

ggplot(data = ecoplot)+
  geom_bar(aes(x = economy, fill = result_level), stat = "count")+
  labs(title = "economy vs month (total=3667)")


#accident
acciplot <- jud_df %>%
  filter(is.na(accident) == FALSE,
         is.na(result) == FALSE,
         is.na(result_level)==FALSE)

#僅抓到未肇事的
ggplot(data = acciplot)+
  geom_bar(aes(x = accident, fill = result_level), stat = "count")+
  labs(title = "accident vs month (total=3208)")



