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

## 오래걸린다 원본 보존
df <- df_news

## 절깎
df$contents <- stemming_words2(text = df$contents, pattern = pattern_list, replacement = replacement_list)
df$contents <- remove_stopwords2(text = df$contents, stopword = stopwords_list)
df$contents <- stemming_words(df$contents)
df <- filter(df, nchar(contents) > 0)


# NC 보통명사
noun <- sapply(df$contents, extractNoun, USE.NAMES = FALSE)
tmp_noun <- lapply(noun, function(x) x[nchar(x)>2])
tmp_noun <- sapply(tmp_noun, paste, collapse = " ")

corp <- Corpus(VectorSource(tmp_noun))
#inspect(corp)
tdm <- TermDocumentMatrix(corp)


word.count = as.array(rollup(tdm,2))   #매트릭스 행별 합계구하기
word.order = order(word.count, decreasing = T)[1:50] #많이 쓰인 단어 순서정리(단어번호)
freq.word = word.order[1:50]  #상위 1000개 단어만 재할당(단어번호)
row.names(tdm[freq.word,])      #해당단어번호 단어 확인


## DTM으로 변환
dtm = as.DocumentTermMatrix(tdm[freq.word,])  


## 단어 frequency
freq <- data.frame(word.count)
freq$name <- row.names(freq)
freq <- freq %>% select(name, X1) %>% arrange(desc(X1))
display.brewer.pal(n = 8, name = 'Set2')
pal <- brewer.pal(n = 8, name = 'Set2')
# Wordcloud를 그립니다. 
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


SNA_dtm(dtm = dtm, keyword = "어르신")


## 단어간의 상관성 파악
findAssocs(tdm, "대학교",0.3)
findAssocs(tdm, "아파트",0.3)
findAssocs(tdm, "초등학교",0.3)

# 상관계수 높은 단어들을 확인합니다. 
checkAssocs(dtm = tdm, keyword = "친환경")
checkAssocs(dtm = tdm, keyword = "야생화")
checkAssocs(dtm = tdm, keyword = "주거환경")


#서구 지역 원도심 TOP 30 SNA
## 워드클라우드 및 SNA 결과 생성
for(i in 1:length(kword)){sh_func(data = df, word = kword[i])}

## 네트워크 맵 생성
source('네트워크.R', encoding = "UTF-8")
df2 <- data.table(df)
keyword <- "매립지"
df_tmp <- df2[key %like% keyword]
dt4Networkmap1(workGb = '주거환경', df = df_tmp, sparse = 0.90, corr = 0.30, link = 2, cex = 3)
dt4Networkmap1(workGb = '야생화', df = df_tmp, sparse = 0.90, corr = 0.30, link = 2, cex = 3)
dt4Networkmap1(workGb = '친환경', df = df_tmp, sparse = 0.90, corr = 0.30, link = 2, cex = 3)

df2 <- data.table(df)
keyword <- "원도심"
df_tmp <- df2[key %like% keyword]
dt4Networkmap1(workGb = '소상공인', df = df_tmp, sparse = 0.90, corr = 0.30, link = 2, cex = 3)
dt4Networkmap1(workGb = '터미널', df = df_tmp, sparse = 0.90, corr = 0.30, link = 2, cex = 3)
dt4Networkmap1(workGb = '협의회', df = df_tmp, sparse = 0.90, corr = 0.30, link = 2, cex = 3)

df2 <- data.table(df)
keyword <- "코로나"
df_tmp <- df2[key %like% keyword]
dt4Networkmap1(workGb = '소상공인', df = df_tmp, sparse = 0.90, corr = 0.30, link = 2, cex = 3)
dt4Networkmap1(workGb = '방호복', df = df_tmp, sparse = 0.90, corr = 0.30, link = 2, cex = 3)
dt4Networkmap1(workGb = '보건소', df = df_tmp, sparse = 0.90, corr = 0.30, link = 2, cex = 3)
