#####################################################################################################
# Script id   : 02 Frequency.R
# Script Name : 패키지 로드
# Author      : 김상훈
# Date        : 2020.04.26
#####################################################################################################
## 패키지 로드
#####################################################################################################



setwd("x:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/")
source('processing.R', encoding = "UTF-8")

kword <- c("매립지","코로나","원도심")
nstart <- c("100","200","300","400","500","600","700","800","900","1000")

## 매립지, 코로나, 원도심 관련 뉴스 가져오기
#result <- naver_api_news(kword = kword, nstart = nstart)

##저장
#write.csv(result, "result.csv" ,row.names = F)


maerib <- read.csv("maerib.csv", stringsAsFactors = F)
covid <- read.csv("covid.csv", stringsAsFactors = F)
gcity <- read.csv("gcity.csv", stringsAsFactors = F)


## 매립지, 코로나, 원도심 중 6개 일보에서 데이터 가져오기
#result_list <- naver_api_keyword(kword = kword)
#maerib <- result_list[[1]]
maerib$key <- kword[1]
#covid <- result_list[[2]]
covid$key <- kword[2]
#gcity <- result_list[[3]]
gcity$key <- kword[3]



title_list <- c("maerib","covid","gcity")
for (i in 1:length(title_list)) {

  temp <- get(title_list[i])
  temp$contents <- temp$contents %>% 
    str_remove_all("flash 오류를 우회하기 위한 함수 추가") %>% # 문자열 제거
    str_remove_all("function _flash_removeCallback") %>% 
    str_remove_all("[a-zA-Z]") %>% # 정규표현식 영어제거
    str_remove_all("\\d") %>% 
    str_remove_all("무단 전재 및 재배포 금지") %>% 
    str_remove_all("내용 없음") %>% 
    str_replace_all("\\W" , " ") %>%  # 정규표현식 : 문자가 아닌 것을 공백으로 변경
    str_replace_all("코로나" , "코로나일구바이러스")
  
  ## 저장
  #write.csv(temp, paste0(title_list[i],".csv"), row.names = F)
  assign(title_list[i],temp)
    
}



## builddictionary
buildDictionary(ext_dic = c("woorimalsam","sejong","insighter"), 
                #category_dic_nms = "",
                user_dic = data.frame(term = readLines("Noun.txt"), tag = "ncn")
                #replace_usr_dic = F, 
                #verbose = F
)
#useNIADic()








## 임시 데이터프레임 생성
df <- rbind(maerib,covid,gcity)
df$contents <- trimws(df$contents, which = "left")


df$year <- substr(df$date,13,16)
df$month <- substr(df$date,9,11)
df$day <- substr(df$date,6,7)


df$month <- ifelse(df$month == "Jan", "01",
              ifelse(df$month == "Feb", "02",
              ifelse(df$month == "Mar", "03",
              ifelse(df$month == "Apr", "04",
              ifelse(df$month == "May", "05",
              ifelse(df$month == "Jun", "06",
              ifelse(df$month == "Jul", "07",
              ifelse(df$month == "Aug", "08",
              ifelse(df$month == "Sep", "09",
              ifelse(df$month == "Oct", "10",
              ifelse(df$month == "Nov", "11",
              ifelse(df$month == "Dec", "12",df$month))))))))))))




for (i in 1:nrow(df)) {df$yymmdd[i] <- paste0(df$year[i], "-",df$month[i], "-",df$day[i])}
for (i in 1:nrow(df)) {df$yymm[i] <- paste0(df$year[i], "-",df$month[i])}
df <- df %>% filter(yymm >= "2019-04" & yymm <= "2020-04")
df$contents <- gsub("코로나일구바이러스일구바이러스","코로나", df$contents)

## 절깎
df$contents <- stemming_words2(text = df$contents, pattern = pattern_list, replacement = replacement_list)
df$contents <- remove_stopwords2(text = df$contents, stopword = stopwords_list)
df$contents <- stemming_words(df$contents)
df <- filter(df, nchar(contents) > 0)
#df$contents <- gsub("코로나일구바이러스바이러스","코로나", df$contents)


tmp <- data.table(df)
temp <- tmp[key %like% "매립지"]
date <- temp %>% select(yymm) %>% filter(yymm >= "2019-04" & yymm <= "2020-04") %>% group_by(yymm) %>% count()
write_xlsx(date, "X:/2020년 프로젝트/06.인천광역시_서구/Script/result/시각화를위한파일/02_매립지_월별_뉴스.xlsx")


temp <- tmp[key %like% "원도심"]
date <- temp %>% select(yymm) %>% filter(yymm >= "2019-04" & yymm <= "2020-04") %>% group_by(yymm) %>% count()
write_xlsx(date, "X:/2020년 프로젝트/06.인천광역시_서구/Script/result/시각화를위한파일/02_원도심_월별_뉴스.xlsx")


temp <- tmp[key %like% "코로나"]
date <- temp %>% select(yymm) %>% filter(yymm >= "2019-04" & yymm <= "2020-04") %>% group_by(yymm) %>% count()
write_xlsx(date, "X:/2020년 프로젝트/06.인천광역시_서구/Script/result/시각화를위한파일/02_코로나_월별_뉴스.xlsx")




df2 <- data.table(df)
keyword <- "매립지"
df_tmp <- df2[key %like% keyword]

# NC 보통명사
noun <- sapply(df_tmp$contents, extractNoun, USE.NAMES = FALSE)
tmp_noun <- lapply(noun, function(x) x[nchar(x)>2])
tmp_noun <- sapply(tmp_noun, paste, collapse = " ")

corp <- Corpus(VectorSource(tmp_noun))
#inspect(corp)
tdm <- TermDocumentMatrix(corp)


word.count = as.array(rollup(tdm,2))   #매트릭스 행별 합계구하기
word.order = order(word.count, decreasing = T)[1:100] #많이 쓰인 단어 순서정리(단어번호)
freq.word = word.order[1:100]  #상위 1000개 단어만 재할당(단어번호)
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




## 단어간의 상관성 파악
findAssocs(tdm, "마이스",0.3)


# 상관계수 높은 단어들을 확인합니다. 
checkAssocs(dtm = tdm, keyword = "친환경")
checkAssocs(dtm = tdm, keyword = "야생화")
checkAssocs(dtm = tdm, keyword = "주거환경")


# 상관계수 높은 단어들을 확인합니다. 
checkAssocs(dtm = tdm, keyword = "소상공인")
checkAssocs(dtm = tdm, keyword = "터미널")
checkAssocs(dtm = tdm, keyword = "협의회")

# 상관계수 높은 단어들을 확인합니다. 
checkAssocs(dtm = tdm, keyword = "소상공인")
checkAssocs(dtm = tdm, keyword = "방호복")
checkAssocs(dtm = tdm, keyword = "보건소")

checkAssocs(dtm = tdm, keyword = "마스크")

서구 지역 원도심 TOP 30 SNA
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
