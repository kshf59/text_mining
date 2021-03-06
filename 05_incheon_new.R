#####################################################################################################
# Script id   : 02 Frequency.R
# Script Name : 패키지 로드
# Author      : 김상훈
# Date        : 2020.04.26
#####################################################################################################
## 패키지 로드
#####################################################################################################
setwd("x:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/")
source("01_load_package.R", encoding = "UTF-8")
setwd("X:/2020년 프로젝트/06.인천광역시_서구/Data/문화재단_보도자료/문화재단_보도자료요청_excel")

dir_list <- dir(pattern = "trans_")
df_list <- list()
for (i in 1:length(dir_list)) {
   df <- read.csv(dir_list[i], stringsAsFactors = F)
   df$month <- gsub(".csv","",str_split_fixed(dir_list[i], "_", n = 2)[,2])
   df_list[[i]] <- df
}
df_news <- rbindlist(df_list)



setwd("x:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/")
source('processing.R', encoding = "UTF-8")



## builddictionary
buildDictionary(ext_dic = c("woorimalsam","sejong","insighter"), 
                #category_dic_nms = "",
                user_dic = data.frame(term = readLines("Noun.txt"), tag = "ncn")
                #replace_usr_dic = F, 
                #verbose = F
)
#useNIADic()

df_news$contents <- stemming_words(df_news$contents)

## 오래걸린다 원본 보존
df <- df_news

df_count <- df %>% select(month) %>% group_by(month) %>% count()
library(writexl)
write_xlsx(df_count, "월별_뉴스건수.xlsx")

## 절깎
df$contents <- stemming_words2(text = df$contents, pattern = pattern_list, replacement = replacement_list)
df$contents <- remove_stopwords2(text = df$contents, stopword = stopwords_list)
df <- filter(df, nchar(contents) > 0)


# NC 보통명사
noun <- sapply(df$contents, extractNoun, USE.NAMES = FALSE)
tmp_noun <- lapply(noun, function(x) x[nchar(x)>2])
tmp_noun <- sapply(tmp_noun, paste, collapse = " ")

corp <- Corpus(VectorSource(tmp_noun))
#inspect(corp)
tdm <- TermDocumentMatrix(corp)


word.count = as.array(rollup(tdm,2))   #매트릭스 행별 합계구하기
word.order = order(word.count, decreasing = T)[1:30] #많이 쓰인 단어 순서정리(단어번호)
freq.word = word.order[1:30]  #상위 1000개 단어만 재할당(단어번호)
row.names(tdm[freq.word,])      #해당단어번호 단어 확인


## DTM으로 변환
dtm = as.DocumentTermMatrix(tdm[freq.word,])  


## 단어 frequency
freq <- data.frame(word.count)
freq$name <- row.names(freq)
freq <- freq %>% select(name, X1) %>% arrange(desc(X1)) %>% head(30)

imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/인천뉴스/", substr(Sys.Date(), 1, 4), sep="")
imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
imagedir <- paste(imagedir, "wordcloud", sep="_")
imagedir <- paste(imagedir, "종합", sep="_")
write.csv(freq, 
          file = paste0(imagedir,".csv"), 
          row.names = F)

# Wordcloud를 그립니다. (확인용)
display.brewer.pal(n = 8, name = 'Set2')
pal <- brewer.pal(n = 8, name = 'Set2')
wordcloud2(
  data = freq[1:30,],
  size = 1,
  fontFamily = 'NanumGothic',
  color = pal,
  backgroundColor = 'white',
  minRotation = -pi / 4,
  maxRotation = pi / 4,
  shuffle = TRUE,
  rotateRatio = 0.25,
  shape = 'circle',
  ellipticity = 0.6)



mon <- unique(df$month)
for (i in 1:length(mon)) {
  
  temp <- df[df$month == mon[i],]
  # NC 보통명사
  noun <- sapply(temp$contents, extractNoun, USE.NAMES = FALSE)
  tmp_noun <- lapply(noun, function(x) x[nchar(x)>2])
  tmp_noun <- sapply(tmp_noun, paste, collapse = " ")
  corp <- Corpus(VectorSource(tmp_noun))
  tdm <- TermDocumentMatrix(corp)
  word.count = as.array(rollup(tdm,2))
  ## 단어 frequency
  freq <- data.frame(word.count)
  freq$name <- row.names(freq)
  freq <- freq %>% select(name, X1) %>% arrange(desc(X1)) %>% head(30)
  assign(paste0("freq_",gsub("월","",mon[i])),freq)
  imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/인천뉴스/", substr(Sys.Date(), 1, 4), sep="")
  imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
  imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
  imagedir <- paste(imagedir, "wordcloud", sep="_")
  imagedir <- paste(imagedir, gsub("월","",mon[i]), sep="_")
  write.csv(freq, 
            file = paste0(imagedir,".csv"), 
            row.names = F)

}


## SNA 결과 파악(매개중심성 위주)
SNA_dtm(dtm = dtm)

sna_result <- c("미세먼지", "문화재단", "쓰레기")
for (i in 1:length(sna_result)) {
  
   temp <- filter(df, contents %like% sna_result[i])
   
   # NC 보통명사
   noun <- sapply(temp$contents, extractNoun, USE.NAMES = FALSE)
   tmp_noun <- lapply(noun, function(x) x[nchar(x)>2])
   tmp_noun <- sapply(tmp_noun, paste, collapse = " ")
   
   corp <- Corpus(VectorSource(tmp_noun))
   #inspect(corp)
   tdm <- TermDocumentMatrix(corp)
   
   
   word.count = as.array(rollup(tdm,2))   #매트릭스 행별 합계구하기
   word.order = order(word.count, decreasing = T)[1:30] #많이 쓰인 단어 순서정리(단어번호)
   freq.word = word.order[1:30]  #상위 1000개 단어만 재할당(단어번호)
   row.names(tdm[freq.word,])      #해당단어번호 단어 확인
   
   ## DTM으로 변환
   dtm = as.DocumentTermMatrix(tdm[freq.word,])  
   
   assign(paste0("temp_",i),temp)
   assign(paste0("tdm_",i),tdm)
   assign(paste0("dtm_",i),dtm)
      
}
# 미세먼지, 문화재단, 쓰레기
sna_result[1]; sna_result[2]; sna_result[3]


## 상관계수
findAssocs(tdm_1, "미세먼지",0.3)
findAssocs(tdm_2, "문화재단",0.3)
findAssocs(tdm_3, "쓰레기",0.3)


# 상관계수 높은 단어 그래프
source('상관분석.R', encoding = "UTF-8")
checkAssocs(dtm = tdm, keyword = "미세먼지")
checkAssocs(dtm = tdm, keyword = "문화재단")
checkAssocs(dtm = tdm, keyword = "쓰레기")


## 네트워크 맵 생성
source('네트워크.R', encoding = "UTF-8")
df$contents <- gsub("립소년소녀합창단","소년소녀합창단", df$contents)
dt4Networkmap1(workGb = '미세먼지', df = df, sparse = 0.90, corr = 0.30, link = 2, cex = 3)
dt4Networkmap1(workGb = '문화재단', df = df, sparse = 0.90, corr = 0.30, link = 2, cex = 3)
dt4Networkmap1(workGb = '쓰레기', df = df, sparse = 0.90, corr = 0.30, link = 2, cex = 3)


