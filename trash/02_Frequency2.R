#install.packages("rvest")
library(rvest)
library(data.table)
library(stringr)
library(KoNLP)
library(dplyr)
setwd("C:/var/06.인천광역시_서구/Data/db_data")
df_temp <- data.table(read.csv("tb_api_news_202004232136.csv", stringsAsFactors = F))

### html -  tag, calss 는 html_nodes()
### html -  id 는 html_node()

#####################################################################################################################################
### 네이버
news_list <- list()
df_temp2 <- data.frame(df_temp[link %like% "naver.com"])
for (i in 1:nrow(df_temp2)) {

  url <- df_temp2[i,3]
  df_html <- read_html(url, encoding = "ko")
  temp <- data.frame(df_html %>% html_nodes("#articleBodyContents") %>% html_text(), stringsAsFactors = F)
  news_list[[i]]  <- temp
  
}
news_1 <- rbindlist(news_list)
colnames(news_1) <- "contents"


# incheontoday
news_list <- list()
df_temp2 <- data.frame(df_temp[link %like% "incheontoday"])
for (i in 1:nrow(df_temp2)) {
  url <- df_temp2[i,3]
  df_html <- read_html(url, encoding = "ko")
  temp <- data.frame(df_html %>% html_nodes("#article-view-content-div") %>% html_text(), stringsAsFactors = F)
  news_list[[i]]  <- temp
}
news_2 <- rbindlist(news_list)
colnames(news_2) <- "contents"


# incheonin
news_list <- list()
df_temp2 <- data.frame(df_temp[link %like% "incheonin"])
for (i in 1:nrow(df_temp2)) {
  url <- df_temp2[i,3]
  df_html <- read_html(url, encoding = "ko")
  temp <- data.frame(df_html %>% html_nodes("#article-view-content-div") %>% html_text(), stringsAsFactors = F)
  news_list[[i]]  <- temp
}
news_3 <- rbindlist(news_list)
colnames(news_3) <- "contents"


# kyeongin
news_list <- list()
df_temp2 <- data.frame(df_temp[link %like% "kyeongin"])
for (i in 1:nrow(df_temp2)) {
  url <- df_temp2[i,3]
  df_html <- read_html(url, encoding = "ko")
  temp <- data.frame(df_html %>% html_node("#font") %>% html_text(), stringsAsFactors = F)
  news_list[[i]]  <- temp
}
news_4 <- rbindlist(news_list)
colnames(news_4) <- "contents"


# incheonnews
news_list <- list()
df_temp2 <- data.frame(df_temp[link %like% "incheonnews"])
for (i in 1:nrow(df_temp2)) {
  url <- df_temp2[i,3]
  df_html <- read_html(url, encoding = "ko")
  temp <- data.frame(df_html %>% html_node("#articleBody") %>% html_text(), stringsAsFactors = F)
  news_list[[i]]  <- temp
}
news_5 <- rbindlist(news_list)
colnames(news_5) <- "contents"


# incheonilbo
news_list <- list()
df_temp2 <- data.frame(df_temp[link %like% "incheonilbo"])
for (i in 1:nrow(df_temp2)) {
  url <- df_temp2[i,3]
  df_html <- read_html(url, encoding = "ko")
  temp <- df_html %>% html_nodes("p") %>% html_text()
  temp <- data.frame(temp[1], stringsAsFactors = F)
  news_list[[i]]  <- temp
}
news_6 <- rbindlist(news_list)
colnames(news_6) <- "contents"

###################################################################################################################################


news <- rbind(news_1,news_2,news_3,news_4,news_5,news_6)

news$contents <- news$contents %>% 
  str_remove_all("flash 오류를 우회하기 위한 함수 추가") %>% # 문자열 제거
  str_remove_all("function _flash_removeCallback") %>% 
  str_remove_all("[a-zA-Z]") %>% # 정규표현식 영어제거
  str_remove_all("\\d") %>% 
  str_remove_all("무단 전재 및 재배포 금지") %>% 
  str_remove_all("내용 없음") %>% 
  str_replace_all("\\W" , " ") %>%  # 정규표현식 : 문자가 아닌 것을 공백으로 변경
  str_replace_all("코로나" , "코로나일구바이러스")



View(news)

#news$contents <- gsub("코로나","코로나바이러스 ", news$contents)
setwd("C:/var/06.인천광역시_서구/Script/kim_script")
source('processing.R', encoding = "UTF-8")

dics <- c("woorimalsam","sejong","insighter")
user_d <- data.frame(term = readLines("Noun.txt"), tag = "ncn")#,stringsAsFactors = F)
buildDictionary(ext_dic = dics, 
                #category_dic_nms = "",
                user_dic = user_d 
                #replace_usr_dic = F, 
                #verbose = F
)
#useNIADic()
#buildDictionary(ext_dic = c("woorimalsam","sejong","insighter"), category_dic_nms = "",
#                user_dic = data.frame("행정안전부","ncn"), replace_usr_dic = F, verbose = F)

## 임시 데이터프레임 생성
df <- news




#extractNoun("코로나일구바이러스")

## 절깎
df$contents <- stemming_words2(text = df$contents, pattern = pattern_list, replacement = replacement_list)
df$contents <- remove_stopwords2(text = df$contents, stopword = stopwords_list)
df$contents <- stemming_words(df$contents)


tmp_list <- list()
for (i in 1:nrow(df)) {
  tmp <- words(df[i,1])  
  tmp_list[[i]] <- tmp  
}

freq <- rbindlist(tmp_list)
freq <- freq[, .N, by = "word"]
freq <- freq[nchar(word) >= 2 & N != 1,]

write.csv(freq, file="C:/var/06.인천광역시_서구/Script/result/keyword.csv", row.names = F)


# NC 보통명사
noun <- sapply(df$contents, extractNoun, USE.NAMES = FALSE)
tmp_noun <- lapply(noun, function(x) x[nchar(x)>1])
tmp_noun <- sapply(tmp_noun, paste, collapse = " ")



corp <- Corpus(VectorSource(tmp_noun))
inspect(corp)


tdm <- TermDocumentMatrix(corp)


## 단어간의 상관성 파악
#findAssocs(tdm, "수도권",0.2)

word.count = as.array(rollup(tdm,2))   #매트릭스 행별 합계구하기
word.order = order(word.count, decreasing = T)[1:100] #많이 쓰인 단어 순서정리(단어번호)
freq.word = word.order[1:100]  #상위 1000개 단어만 재할당(단어번호)
row.names(tdm[freq.word,])      #해당단어번호 단어 확인

## DTM으로 변환
dtm = as.DocumentTermMatrix(tdm[freq.word,])  

## make SNA
SNA_dtm(dtm)

