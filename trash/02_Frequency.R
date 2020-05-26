
library(dplyr)
library(RColorBrewer)
library(KoNLP)
library(data.table)
library(wordcloud2)
library(tm)
library(RPostgreSQL)
library(stringr)

## 인코딩 설정
set_utf8 <- function(x){
  for (i in 1:ncol(x)){
    if (is.character(x[, i])) Encoding(x[, i]) <- "UTF-8"
  }
  for (name in colnames(x)){
    Encoding(name) <- "UTF-8"
  }
  x
}

## DB 연결
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "ich_sg",
                 port = "5432",
                 host = "192.168.1.169",
                 user = "postgres",
                 password = "socsoft")

## 세종시 국민신문고
r_sql <- c("select * from tb_api_news limit 500;")
tb_api_news <- data.table(set_utf8(dbGetQuery(con,r_sql)))


dics <- c("woorimalsam","sejong","insighter")
user_d <- data.frame(term = readLines("Noun.txt"), tag = "ncn")#,stringsAsFactors = F)
buildDictionary(ext_dic = dics, 
                #category_dic_nms = "",
                user_dic = user_d 
                #replace_usr_dic = F, 
                #verbose = F
                )
useNIADic()
#buildDictionary(ext_dic = c("woorimalsam","sejong","insighter"), category_dic_nms = "",
#                user_dic = data.frame("행정안전부","ncn"), replace_usr_dic = F, verbose = F)

## 임시 데이터프레임 생성
df <- tb_api_news

setwd("X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script")

source('X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/processing.R', encoding = "UTF-8")


## 절깎
df$contents <- stemming_words2(text = df$contents, pattern = pattern_list, replacement = replacement_list)
df$contents <- remove_stopwords2(text = df$contents, stopword = stopwords_list)



tmp_list <- list()
for (i in 1:nrow(df)) {
   tmp <- words(df[i,2])  
   tmp_list[[i]] <- tmp  
}

freq <- rbindlist(tmp_list)
freq <- freq[, .N, by = "word"]
freq <- freq[nchar(word) >= 2 & N != 1,]

write.csv(freq, file="keyword.csv", row.names = F)

#######################################################################
# NC 보통명사
noun <- sapply(tb_api_news$contents, SimplePos22, USE.NAMES = FALSE)
tmp_noun <- lapply(noun, function(x) grep("/NC", x, value=T))
tmp_noun <- lapply(tmp_noun, function(x) str_subset(x, "/NC$"))
tmp_noun <- lapply(tmp_noun, function(x) str_replace_all(x, "/NC", ""))
tmp_noun <- sapply(tmp_noun, paste, collapse = " ")



corp <- Corpus(VectorSource(tmp_noun))
inspect(corp)
View(data.frame(tmp_noun))


tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)))


tdm
as.matrix(tdm)
findAssocs(tdm, "신도심",0.1)



##################################################### sample #####################################################
corp2 <- tm_map(corp, removePunctuation)
corp2 <- tm_map(corp2, removeNumbers)
corp2 <- tm_map(corp2, tolower)
corp2 <- tm_map(corp2, stripWhitespace)
book_stopwords <- c(stopwords("english"),"이후", "없", "되", "하면", "것", "그것", "음", "수", "들이", "한", "할", "있", "년", "하기", "하지", "하")
corp2 <-tm_map(corp2, removeWords, book_stopwords)
corp2 <-tm_map(corp2, PlainTextDocument)
##################################################### sample #####################################################


wordcloud2(freq, size = 1, color = "random-light", backgroundColor = "black")
