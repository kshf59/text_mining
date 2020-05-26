## 패키지 불러오기
setwd("X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script")
source("01_load_package.R", encoding = "UTF-8")

readxl::excel_sheets("X:/2020년 프로젝트/06.인천광역시_서구/Data/문화재단_라운드테이블/2회차 라운드테이블 자료.xlsx")
#po_ne <- read.csv("sentiword.csv", header = F, stringsAsFactors =F)

## 데이터 불러오기
excel_list <- list()
for (i in 1:5) {
  temp <- read_excel("X:/2020년 프로젝트/06.인천광역시_서구/Data/문화재단_라운드테이블/2회차 라운드테이블 자료.xlsx", sheet = i)
  excel_list[[i]] <- temp[,1:3]
}

## 검암경서 청라, 가정석남, 검단, 가좌
df_gg <- excel_list[[1]]; df_chr <- excel_list[[2]]; df_gs <- excel_list[[3]]; df_gd <- excel_list[[4]]; df_gj <- excel_list[[5]]
df_gg$지역 <- "검암경서"; df_chr$지역 <- "청라"; df_gs$지역 <- "가정석남"; df_gd$지역 <- "검단"; df_gj$지역 <- "가좌"

df_tot <- rbind(df_gg, df_chr, df_gs, df_gd, df_gj)
df_tot$발언자 <- ifelse(df_tot$발언자 %like% "재단", df_tot$발언자, "시민")
## 시민들의 의견을 찾기위한 시민의견 데이터 추출
df_tot_mind <- filter(df_tot, 발언자 == "시민")

#pos_neg <- sentimental(df_tot_mind$토의내용, positive, negative)
#df <- filter(pos_neg, score >= 1)
#df$text <- as.character(df$text)


names(df_tot_mind)
colnames(df_tot_mind) <- c("no","speak","contents","region")

## builddictionary
buildDictionary(ext_dic = c("woorimalsam","sejong","insighter"), 
                #category_dic_nms = "",
                user_dic = data.frame(term = readLines("Noun.txt"), tag = "ncn"),
                replace_usr_dic = T
                #verbose = F
)




## 데이터 정제
source("processing.R", encoding = "UTF-8")
df_tot_mind$contents <- rtable_stemming(text = df_tot_mind$contents, pattern = pattern_list, replacement = replacement_list)
df_tot_mind$contents <- rtable_stopword(text = df_tot_mind$contents, stopword = stopwords_list)
df_tot_mind$contents <- stemming_words(df_tot_mind$contents)


#df <- filter(df_tot_mind, 지역 == "검암경서")
df <- df_tot_mind
# NC 보통명사
noun <- sapply(df$contents, extractNoun, USE.NAMES = FALSE)
tmp_noun <- lapply(noun, function(x) x[nchar(x)>1])
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
freq <- freq %>% select(name, X1) %>% arrange(desc(X1))
#display.brewer.pal(n = 8, name = 'Set2')
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


## 워드 클라우드 
imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/라운드테이블2/", substr(Sys.Date(), 1, 4), sep="")
imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
imagedir <- paste(imagedir, "wordcloud", sep="_")
imagedir <- paste(imagedir, "종합", sep="_")
imagename <- paste(imagedir, ".jpg", sep="")
library(webshot)
#webshot::install_phantomjs()
# Make the graph


display.brewer.pal(n = 8, name = 'Set2')
pal <- brewer.pal(n = 8, name = 'Set2')
# Wordcloud를 그립니다. 
my_graph =   wordcloud2(
  data = freq[1:50,],
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

# save it in html
library("htmlwidgets")
saveWidget(my_graph,"tmp.html",selfcontained = F)
# and in png
webshot("tmp.html", imagename, delay =5, vwidth = 480, vheight=300) # changed to png. 

write.csv(freq, 
          file = paste0(imagedir,".csv"), 
          row.names = F)
