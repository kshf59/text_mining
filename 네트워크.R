# 네트워크 맵을 그리는 사용자 정의 함수를 생성합니다. 
drawNetworkmap <- function(dtmObj, title, sparse, corr, prob, link, cex) {
  
  # 상관계수 행렬의 크기를 조정합니다.
  corTerms <- dtmObj %>% as.matrix() %>% cor()
  corTerms[corTerms <= corr] <- 0
  
  # 네트워크 객체를 생성합니다.
  netTerms <- network(x = corTerms, directed = FALSE)
  
  # 매개 중심성을 계산합니다.
  btnTerms <- sna::betweenness(dat = netTerms)
  netTerms %v% 'mode' <-
    ifelse(
      test = btnTerms >= quantile(x = btnTerms, probs = prob, na.rm = TRUE), 
      yes = 'Top', 
      no = 'Rest')
  
  # 노드 컬러를 지정합니다. 
  # 상위 10%는 금색, 나머지 90%는 흰색으로 설정합니다. 
  nodeColors <- c('Top' = 'gold', 'Rest' = 'white')
  
  # 엣지 크기를 지정합니다. 이번 예제에서는 상관계수의 1.2배로 합니다.
  set.edge.value(x = netTerms, attrname = 'edgeSize', value = corTerms * 2)
  
  # 네트워크 지도를 그립니다. 
  ggnet2(
    net = netTerms,
    mode = 'fruchtermanreingold',
    layout.par = list(cell.jitter = 0.001),
    size.min = link,
    label = TRUE,
    label.size = cex,
    node.color = 'mode',
    palette = nodeColors,
    node.size = sna::degree(dat = netTerms),
    edge.size = 'edgeSize',
    legend.position = 'None',) + 
    labs(title = title) + 
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
}



dt4Networkmap1 <- function(workGb, df, sparse, corr, prob = 0.95, link = 4, cex = 4) {
  
  temp <- df
  # 입력조건에 따라 dtm을 선택합니다.
  checks <- filter(temp, contents %like% workGb)
  #dtmSub <- dtm[rownames(x = dtm) %in% temp$contents[checks], ]
  # NC 보통명사
  noun <- sapply(checks$contents, extractNoun, USE.NAMES = FALSE)
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
  
  
  dtmSub <- dtm[rownames(x = dtm), ]
  
  # 모든 값이 0인 열을 삭제합니다. 
  dtmSub <- dtmSub[, as.matrix(x = dtmSub) %>% colSums() >= 1]
  
  # 그래프 제목을 설정합니다. 
  title <- str_c('[', workGb, '] 네트워크맵', sep = ' ')
  
  # 네트워크 맵을 그립니다. 
  drawNetworkmap(dtmObj = dtmSub, title, sparse = 0.9, prob = 1,corr = 0.1, link = 2, cex =3)
}



