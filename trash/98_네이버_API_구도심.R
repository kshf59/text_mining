#############################################################################################################################################
# 제목 - 네이버 API 호출
# 작성자 - 김상훈
# 작성일 - 2020-04-21 
############################################################################################################################################



#library(xml2)
library(dplyr)
library(data.table)
library(XML)
library(plyr)
library(httr)
library(stringr)
library(RPostgreSQL)
######################################################################################
##네이버 API 호출
######################################################################################

setwd("X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script")

source('X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/processing.R', encoding = "UTF-8")


api_url = "https://openapi.naver.com/v1/search/news.xml"
query = URLencode(iconv("인천 서구 구도심", to="UTF-8"))
query = str_c("?query=", query)
display = "&display=100"
start = "&start=1000"
sort = "&sort=sim"


client_id <- "id"
client_secret <- "secret"

result = GET(str_c(api_url, query, display, start, sort), 
             add_headers("X-Naver-Client-Id" = client_id, "X-Naver-Client-Secret" = client_secret))

par_result <- xmlParse(result)
xL1 <- xmlToList(par_result)
## 뉴스 제목
tit <- xpathSApply(par_result,"/rss/channel/item/title", xmlValue)
## 뉴스 내용
des <- xpathSApply(par_result,"/rss/channel/item/description", xmlValue)
## 해당 뉴스 링크
link <- xpathSApply(par_result,"/rss/channel/item/originallink", xmlValue)
## 날짜
date <- xpathSApply(par_result,"/rss/channel/item/pubDate", xmlValue)
View(link)

## 데이터 프레임 생성
tb_api_news <- data.frame(matrix(ncol = 4))
colnames(tb_api_news) <- c("title","contents", "link", "date")
for (i in 1:length(tit)) {tb_api_news[i,1] <- i}
## title, description 데이터 넣기
for (i in 1:length(tit)) {
  tb_api_news[i,1] <- tit[i]
  tb_api_news[i,2] <- des[i]
  tb_api_news[i,3] <- link[i]
  tb_api_news[i,4] <- date[i]
}

## title 제거
tb_api_news$title <-remove_stopwords(tb_api_news$title)
tb_api_news$title <-stemming_words(tb_api_news$title)


## contents 제거
tb_api_news$contents <-remove_stopwords(tb_api_news$contents)
tb_api_news$contents <-stemming_words(tb_api_news$contents)

tb_api_news_oldnew <- tb_api_news

## DB 연결
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "ich_sg",
                 port = "5432",
                 host = "192.168.1.169",
                 user = "postgres",
                 password = "socsoft")

## DB에 데이터 넣기
#dbClearResult(dbSendQuery(con,enc2utf8("drop table if exists tb_api_news_oldnew;")))
#dbClearResult(dbSendQuery(con,enc2utf8("create table tb_api_news_oldnew (
#                                              title text,
#                                              contents text,
#                                              link text,
#                                              date varchar(100)
#                                     );")))

#dbClearResult(dbSendQuery(con,enc2utf8("COMMENT ON COLUMN tb_api_news_oldnew.title IS '뉴스제목';")))
#dbClearResult(dbSendQuery(con,enc2utf8("COMMENT ON COLUMN tb_api_news_oldnew.contents IS '뉴스내용';")))
#dbClearResult(dbSendQuery(con,enc2utf8("COMMENT ON COLUMN tb_api_news_oldnew.link IS '링크';")))
#dbClearResult(dbSendQuery(con,enc2utf8("COMMENT ON COLUMN tb_api_news_oldnew.link IS '날짜';")))
dbWriteTable(conn = con,
             name = "tb_api_news_oldnew",
             value = tb_api_news_oldnew,
             append = T, row.names = F)

## DB 연결 해제
dbDisconnect(con)

