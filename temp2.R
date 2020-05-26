
reg <- unique(df_tot_mind$region)

for (i in 1:length(reg)) {
  
df <- filter(df_tot_mind,region == reg[i])
#df <- df_tot_mind
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




dtmSub <- dtm[rownames(x = dtm), ]

# 모든 값이 0인 열을 삭제합니다. 
dtmSub <- dtmSub[, as.matrix(x = dtmSub) %>% colSums() >= 1]



# 상관계수 행렬의 크기를 조정합니다.
corTerms <- dtmSub %>% as.matrix() %>% cor()
corTerms[corTerms <= 0.3] <- 0

# 네트워크 객체를 생성합니다.
netTerms <- network(x = corTerms, directed = FALSE)

# 매개 중심성을 계산합니다.
btnTerms <- sna::betweenness(dat = netTerms)
netTerms %v% 'mode' <-
  ifelse(
    test = btnTerms >= quantile(x = btnTerms, probs = 0.1, na.rm = TRUE), 
    yes = 'Top', 
    no = 'Rest')

# 노드 컬러를 지정합니다. 
# 상위 10%는 금색, 나머지 90%는 흰색으로 설정합니다. 
#nodeColors <- c('Top' = 'steelblue', 'Rest' = 'white')

# 엣지 크기를 지정합니다. 이번 예제에서는 상관계수의 1.2배로 합니다.
set.edge.value(x = netTerms, attrname = 'edgeSize', value = corTerms * 2)


ggnet2(net = netTerms,
       layout.par = list(cell.jitter = 0.001),
       #label = TRUE, 
       #label.size = 4,
       #shape.legend = NA,
       #color = "phono",
       palette = "Set3",
       #node.size = sna::degree(dat = netTerms),
       edge.color = "grey50",
       edge.size = 'edgeSize')+
  geom_point(aes(color = color), size = 12, color = "white") +
  geom_point(aes(color = color), size = 12, alpha = 0.5) +
  geom_point(aes(color = color), size = 9) +
  geom_text(aes(label =label), color = "black", fontface = "bold", size = 5) +
  guides(color = FALSE)

}



