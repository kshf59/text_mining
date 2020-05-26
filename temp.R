i =6
reg <- unique(df_tot_mind$region)
for (i in 1:length(reg)) {
  
  df <- filter(df_tot_mind, region == reg[i])

## 데이터 정제
source("processing.R", encoding = "UTF-8")
df$contents <- rtable_stemming(text = df$contents, pattern = pattern_list, replacement = replacement_list)
df$contents <- rtable_stopword(text = df$contents, stopword = stopwords_list)
df$contents <- stemming_words(df$contents)

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
reg[i]
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
imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/라운드테이블3/", substr(Sys.Date(), 1, 4), sep="")
imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
imagedir <- paste(imagedir, "wordcloud", sep="_")
imagedir <- paste(imagedir, reg[i], sep="_")
imagename <- paste(imagedir, ".jpg", sep="")
library(webshot)
#webshot::install_phantomjs()
# Make the graph


display.brewer.pal(n = 8, name = 'Set2')
pal <- brewer.pal(n = 8, name = 'Set2')
# Wordcloud를 그립니다. 
my_graph =   wordcloud2(
  data = freq[1:30,],
  size = 1,
  fontFamily = 'NanumGothic',
  color = pal,
  backgroundColor = 'white',
  minRotation = -pi / 4,
  maxRotation = pi / 4,
  shuffle = TRUE,
  rotateRatio = 0.25,
  #shape = 'circle',
  ellipticity = 0.6)

# save it in html
library("htmlwidgets")
saveWidget(my_graph,"tmp.html",selfcontained = F)
# and in png
webshot("tmp.html", imagename, delay =5, vwidth = 480, vheight=400) # changed to png. 

write.csv(freq[1:30,], 
          file = paste0(imagedir,".csv"), 
          row.names = F)

}



#doc_word <- as.character(doc_all$word)
positive <- readLines("positive.txt", encoding = "cp949")
positive=positive[-1]
positive <- data.frame(positive, stringsAsFactors = F)
colnames(positive) <- "word"

negative <- readLines("negative.txt", encoding = "cp949")
negative=negative[-1]
negative <- data.frame(negative, stringsAsFactors = F)
colnames(negative) <- "word"



reg <- unique(df_tot_mind$region)
for (i in 1:length(reg)) {
  
  df <- filter(df_tot_mind, region == reg[i])


doc <- as.character(df$contents)
doc2 <- paste(SimplePos22(doc))
## 명사
doc_pv <- str_match(doc2, "([가-힣]+)/PV")
doc_pv <- doc_pv[,2]
doc_pv <- data.frame(word=doc_pv[!is.na(doc_pv)], stringsAsFactors = F)
## 보조영언
doc_px <- str_match(doc2, "([가-힣]+)/PX")
doc_px <- doc_px[,2]
doc_px <- data.frame(word=doc_px[!is.na(doc_px)], stringsAsFactors = F)
## 형용사
doc_pa <- str_match(doc2, "([가-힣]+)/PA")
doc_pa <- doc_pa[,2]
doc_pa <- data.frame(word=doc_pa[!is.na(doc_pa)], stringsAsFactors = F)

doc_all <- data.table(rbind(doc_pv,doc_px,doc_pa))
doc_all$word <- paste0(doc_all$word,"다")
doc_all$word <- gsub("다다","다",doc_all$word)
cnt <- doc_all %>% group_by(word) %>% count()



pos <- inner_join(cnt, positive, by = c("word"))
pos <- arrange(pos, desc(freq))
neg <- inner_join(cnt, negative, by = c("word"))
neg <- arrange(neg, desc(freq))
imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/라운드테이블3/", substr(Sys.Date(), 1, 4), sep="")
imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
imagedir <- paste(imagedir, reg[i], sep="_")
imagename <- paste(imagedir, "_positive.csv", sep="")
imagename2 <- paste(imagedir, "_negative.csv", sep="")

write.csv(pos, imagename, row.names = F)
write.csv(neg, imagename2, row.names = F)


}




doc <- as.character(df_tot_mind$contents)
doc2 <- paste(SimplePos22(doc))
## 명사
doc_pv <- str_match(doc2, "([가-힣]+)/PV")
doc_pv <- doc_pv[,2]
doc_pv <- data.frame(word=doc_pv[!is.na(doc_pv)], stringsAsFactors = F)
## 보조영언
doc_px <- str_match(doc2, "([가-힣]+)/PX")
doc_px <- doc_px[,2]
doc_px <- data.frame(word=doc_px[!is.na(doc_px)], stringsAsFactors = F)
## 형용사
doc_pa <- str_match(doc2, "([가-힣]+)/PA")
doc_pa <- doc_pa[,2]
doc_pa <- data.frame(word=doc_pa[!is.na(doc_pa)], stringsAsFactors = F)
## 명사
doc_nc <- str_match(doc2, "([가-힣]+)/NC")
doc_nc <- doc_nc[,2]
doc_nc <- data.frame(word=doc_nc[!is.na(doc_nc)], stringsAsFactors = F)
## mag?
doc_mag <- str_match(doc2, "([가-힣]+)/mag")
doc_mag <- doc_mag[,2]
doc_mag <- data.frame(word=doc_mag[!is.na(doc_mag)], stringsAsFactors = F)




doc_all <- data.table(rbind(doc_pv,doc_px,doc_pa))
doc_all$word <- paste0(doc_all$word,"다")
doc_all$word <- gsub("다다","다",doc_all$word)
cnt <- doc_all %>% group_by(word) %>% count()

ttt <- inner_join(doc_nc, positive, by = c("word"))
ttt <- inner_join(doc_nc, negative, by = c("word"))


pos <- inner_join(cnt, positive, by = c("word"))
pos <- arrange(pos, desc(freq))
neg <- inner_join(cnt, negative, by = c("word"))
neg <- arrange(neg, desc(freq))

imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/라운드테이블3/", substr(Sys.Date(), 1, 4), sep="")
imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
imagedir <- paste(imagedir, "종합", sep="_")
imagename <- paste(imagedir, "_positive.csv", sep="")
imagename2 <- paste(imagedir, "_negative.csv", sep="")


write.csv(pos, imagename, row.names = F)
write.csv(neg, imagename2, row.names = F)

View(doc2)


SimplePos22("반가운")


ttt <- str_match(doc2, "반가")
View(doc2)
