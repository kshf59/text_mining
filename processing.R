#############################################################################################################################################
# 제목 - 네이버 API 호출
# 작성자 - 김상훈
# 작성일 - 2020-04-21 
############################################################################################################################################
########################################################################################################################################


#########################################################################################################################################
naver_api_news <- function(kword = word, nstart = nstart){
  
  temp_list <- list()
  api_list <- list()
  for (i in 1:length(kword)) {
    api_url = "https://openapi.naver.com/v1/search/news.xml"
    query = URLencode(iconv(paste("인천 서구",kword[i]), to="UTF-8"))
    query = str_c("?query=", query)
    display = "&display=100"
    sort = "&sort=sim"
    client_id <- "_rPkaKMJgEctxGJTQXsg"
    client_secret <- "YZbQjL3v8h"
    
    for (j in 1:length(nstart)) {
      
      
      result = GET(str_c(api_url, query, display, paste0("&start=",nstart[j]), sort), 
                   add_headers("X-Naver-Client-Id" = client_id, "X-Naver-Client-Secret" = client_secret))
      
      par_result <- xmlParse(result)
      xL1 <- xmlToList(par_result)
      ## 뉴스 제목
      tit <- xpathSApply(par_result,"/rss/channel/item/title", xmlValue)
      ## 뉴스 내용
      des <- xpathSApply(par_result,"/rss/channel/item/description", xmlValue)
      ## 해당 뉴스 링크
      link <- xpathSApply(par_result,"/rss/channel/item/link", xmlValue)
      ## 날짜
      date <- xpathSApply(par_result,"/rss/channel/item/pubDate", xmlValue)
      
      
      ## 데이터 프레임 생성
      tb_api_news <- data.frame(matrix(ncol = 5))
      colnames(tb_api_news) <- c("title","contents", "link", "date","key")
      for (d in 1:length(tit)) {tb_api_news[d,1] <- d}
      ## title, description 데이터 넣기
      for (k in 1:length(tit)) {
        tb_api_news[k,1] <- tit[k]
        tb_api_news[k,2] <- des[k]
        tb_api_news[k,3] <- link[k]
        tb_api_news[k,4] <- date[k]
      }
      
      tb_api_news$key <- kword[i]
      temp_list[[j]] <- tb_api_news
      
    }
    
    result <- unique(as.data.frame(rbindlist(temp_list), stringsAsFactors = F))
    api_list[[i]] <- result
    
  }
  return(rbindlist(api_list))
}
##################################################################################################################################


naver_api_keyword <- function(kword = kword) {
  
  
  result_list <- list()
  for(t in 1:length(kword)) {
    
    df_temp <- result[key == kword[t]]  
    ### 네이버
    news_list <- list()
    df_temp2 <- data.frame(df_temp[link %like% "naver.com"])
    for (i in 1:nrow(df_temp2)) {
      
      url <- df_temp2[i,3]
      df_html <- read_html(url, encoding = "ko")
      temp <- data.frame(df_html %>% html_nodes("#articleBodyContents") %>% html_text(), stringsAsFactors = F)
      if(nrow(temp) != 0) {
      
      temp <- cbind(df_temp2[i,1], temp, df_temp2[i,3], df_temp2[i,4])
      news_list[[i]]  <- temp
      
      }
    }
    news_1 <- rbindlist(news_list)
    colnames(news_1) <- c("title","contents","link","date")
    news_1$name <- "네이버"
    
    # incheontoday
    news_list <- list()
    df_temp2 <- data.frame(df_temp[link %like% "incheontoday"])
    for (i in 1:nrow(df_temp2)) {
      url <- df_temp2[i,3]
      df_html <- read_html(url, encoding = "ko")
      temp <- data.frame(df_html %>% html_nodes("#article-view-content-div") %>% html_text(), stringsAsFactors = F)
      temp <- cbind(df_temp2[i,1],temp, df_temp2[i,3], df_temp2[i,4])
      news_list[[i]]  <- temp
    }
    news_2 <- rbindlist(news_list)
    colnames(news_2) <- c("title","contents","link","date")
    news_2$name <- "인천투데이"
    
    # incheonin
    news_list <- list()
    df_temp2 <- data.frame(df_temp[link %like% "incheonin"])
    for (i in 1:nrow(df_temp2)) {
      url <- df_temp2[i,3]
      df_html <- read_html(url, encoding = "ko")
      temp <- data.frame(df_html %>% html_nodes("#article-view-content-div") %>% html_text(), stringsAsFactors = F)
      temp <- cbind(df_temp2[i,1],temp, df_temp2[i,3], df_temp2[i,4])
      news_list[[i]]  <- temp
    }
    news_3 <- rbindlist(news_list)
    colnames(news_3) <- c("title","contents","link","date")
    news_3$name <- "인천인"
    
    # kyeongin
    news_list <- list()
    df_temp2 <- data.frame(df_temp[link %like% "kyeongin"])
    for (i in 1:nrow(df_temp2)) {
      url <- df_temp2[i,3]
      df_html <- read_html(url, encoding = "ko")
      temp <- data.frame(df_html %>% html_node("#font") %>% html_text(), stringsAsFactors = F)
      temp <- cbind(df_temp2[i,1],temp, df_temp2[i,3], df_temp2[i,4])
      news_list[[i]]  <- temp
    }
    news_4 <- rbindlist(news_list)
    colnames(news_4) <- c("title","contents","link","date")
    news_4$name <- "경인"
    
    # incheonnews
    news_list <- list()
    df_temp2 <- data.frame(df_temp[link %like% "incheonnews"])
    for (i in 1:nrow(df_temp2)) {
      url <- df_temp2[i,3]
      df_html <- read_html(url, encoding = "ko")
      temp <- data.frame(df_html %>% html_node("#articleBody") %>% html_text(), stringsAsFactors = F)
      temp <- cbind(df_temp2[i,1],temp, df_temp2[i,3], df_temp2[i,4])
      news_list[[i]]  <- temp
    }
    news_5 <- rbindlist(news_list)
    colnames(news_5) <- c("title","contents","link","date")
    news_5$name <- "인천뉴스"
    
    # incheonilbo
    news_list <- list()
    df_temp2 <- data.frame(df_temp[link %like% "incheonilbo"])
    for (i in 1:nrow(df_temp2)) {
      url <- df_temp2[i,3]
      df_html <- read_html(url, encoding = "ko")
      temp <- df_html %>% html_nodes("p") %>% html_text()
      temp <- data.frame(temp[1], stringsAsFactors = F)
      temp <- cbind(df_temp2[i,1],temp, df_temp2[i,3], df_temp2[i,4])
      news_list[[i]]  <- temp
    }
    news_6 <- rbindlist(news_list)
    colnames(news_6) <- c("title","contents","link","date")
    news_6$name <- "인천일보"
    
    
    temp <- as.data.frame(rbind(news_1,news_2,news_3,news_4,news_5,news_6), stringsAsFactors = F)
    result_list[[t]] <- temp
    
    
  }
  
  return(result_list)
}


###################################################################################################################################

## html tag 제거 및 불용어 제거
remove_stopwords <- function(text = text, stopword = stopword_list){
  stopwords_list <- read.csv("X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/stopwords.csv", header=T, stringsAsFactors = F)
  stopwords_list <- as.character(stopwords_list$pattern)
  for(i in 1:length(stopwords_list)){
    text <- gsub(stopwords_list[i], "", text)
  }
  return(text)
}
########################################################################################################################################

stemming_words2 <- function(text = text, pattern = pattern_list, replacement = replacement_list){
  stemming_list <- read.csv("X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/stemming2.csv", header=T, stringsAsFactors = F)
  pattern_list <- as.character(stemming_list$pattern)
  replacement_list <- as.character(stemming_list$replacement)
  for (i in 1:length(pattern_list)) {
    text <- gsub(pattern_list[i], replacement_list[i], text)
  }
  return(text)
}
########################################################################################################################################

remove_stopwords2 <- function(text = text, stopword = stopword_list){
  stopwords_list <- read.csv("X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/stopword2.csv", header=T, stringsAsFactors = F)
  stopwords_list <- as.character(stopwords_list$pattern)
  for(i in 1:length(stopwords_list)){
    text <- gsub(stopwords_list[i], "", text)
  }
  return(text)
}


########################################################################################################################################



## html tag 제거 및 불용어 제거
rtable_stopword <- function(text = text, stopword = stopword_list){
  stopwords_list <- read.csv("X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/rtable_stopword.csv", header=T, stringsAsFactors = F)
  stopwords_list <- as.character(stopwords_list$pattern)
  for(i in 1:length(stopwords_list)){
    text <- gsub(stopwords_list[i], "", text)
  }
  return(text)
}
########################################################################################################################################

rtable_stemming <- function(text = text, pattern = pattern_list, replacement = replacement_list){
  stemming_list <- read.csv("X:/2020년 프로젝트/06.인천광역시_서구/Script/kim_script/rtable_stemming.csv", header=T, stringsAsFactors = F)
  pattern_list <- as.character(stemming_list$pattern)
  replacement_list <- as.character(stemming_list$replacement)
  for (i in 1:length(pattern_list)) {
    text <- gsub(pattern_list[i], replacement_list[i], text)
  }
  return(text)
}


###################################################################################################################################




stemming_words <- function(text = text){
  
  text <- gsub(" *\\[.*?\\] *"," ", text)
  text <- gsub(" *\\(.*?\\) *"," ", text)
  text <- gsub("[\\*]+.*","", text)
  text <- gsub("[[:punct:]]", " ", text)
  text <- gsub("[ㄱ-ㅎㅏ-ㅣ]", " ", text)
  text <- gsub("○", "", text)
  text <- gsub("▲", "", text)
  text <- gsub("☆", "", text)
  
  text <- gsub("☆", "", text)
  text <- gsub("○", "", text)
  text <- gsub("●", "", text)
  text <- gsub("◇", "", text)
  
  text <- gsub("◆", "", text)
  text <- gsub("□", "", text)
  text <- gsub("■", "", text)
  text <- gsub("△", "", text)
  text <- gsub("ㅁ", "", text)
  text <- gsub("☆", "", text)
  text <- gsub("┃", "", text)
  text <- gsub("[[:digit:]]", " ", text)
  return(text)
}


########################################################################################################################################

## 명사 추출 
words <- function(doc){
  doc <- as.character(doc)
  doc2 <- paste(SimplePos22(doc))
  doc3 <- str_match(doc2, "([가-힣]+)/NC")
  doc4 <- doc3[,2]
  doc5 <- data.frame(word=doc4[!is.na(doc4)], stringsAsFactors = F)
  
  return(doc5)
}
########################################################################################################################################

SNA_dtm <- function(dtm = dtm, keyword = keyword){
  library(igraph)
  dtmx <- as.matrix(dtm)
  # change it to a Boolean matrix
  dtmx[dtmx >= 1] <- 1
  # transform into a term-term adjacency matrix
  dtmx2 <- t(dtmx) %*% dtmx
  # build a graph from the above matrix
  g <- graph.adjacency(dtmx2, weighted=T, mode = "undirected")
  # remove loops
  g <- simplify(g)
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  V(g)$color <- rgb(135, 206, 250, max = 255)
  # plot the graph in layout1
  layout1 <- layout.fruchterman.reingold(g)
  par(mar=c(1, 1, 1, 1))
  plot(g, 
       layout=layout.fruchterman.reingold,
       vertex.label.font = 2,
       #vertex.size = 2, 
       #vertex.shape = "none", 
       edge.width=1 + E(g)$weight/100, 
       edge.arrow.width = E(g)$weight/100 
  )
  # output data
  # 연결중심성 : 한 노드에 직접적으로 연결된 링크들의 합으로 얻어진 중심성
  # 근접중심성 : 각 노드 간의 거리를 근거로 중심성을 측정하는 방법으로, 연결 정도 중심성과는 달리 직접적으로 연결된 노드뿐만 아니라 간접적으로 연결된 모든 노드 간의 거리를 합산해 중심성을 측정
  # 매개중심성 : 네트워크 내에서 한 노드가 담당하는 매개자 역할의 정도로서 중심성을 측정하는 방법
  # 위세중심성 : 연결된 노드의 중요성에 가중치를 둬 노드의 중심성을 측정하는 방법
  aa <- data.table('키워드' = V(g)$label, 
                   '연결중심성' = as.numeric(degree(g)), 
                   '근접중심성' = as.numeric(closeness(g)),
                   '매개중심성' = as.numeric(betweenness(g)),
                   '위세중심성' = as.numeric(evcent(g)$vector)
  )
  setorder(aa, -'연결중심성')
  # save plot and output data
  # save plot and output data
  imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/", substr(Sys.Date(), 1, 4), sep="")
  imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
  imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
  imagedir <- paste(imagedir, "SNA", sep="_")
  imagedir <- paste(imagedir, keyword, sep="_")
  imagename <- paste(imagedir, ".jpeg", sep="")
  dev.copy(jpeg, filename = imagename)
  dev.off()
  dev.off()
  write.csv(aa, file = paste(imagedir, ".csv", sep=""))
  
}



########################################################################################################################################


keyword_aruls <- function(dtm = dtm,supp = supp, conf = conf, minlen = minlen){
  require(arules)
  require(arulesViz)
  dtmx <- as.matrix(dtm)
  tmp <- as(dtmx, 'transactions')
  ## support: 얼마나 자주 등장하나
  ## confidence
  ## minlen : rule 의 길이
  rules_tmp <- apriori(tmp, parameter = list(supp = supp, conf = conf, minlen = minlen))
  rules_sorted <- sort(rules_tmp, by = 'lift')
  par(mar=c(1, 1, 1, 1))
  plot(rules_sorted, method = 'graph', control = list(type = 'items'))
  imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/", substr(Sys.Date(), 1, 4), sep="")
  imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
  imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
  imagename <- paste(imagedir, ".jpeg", sep="")
  dev.copy(jpeg, filename = imagename)
  dev.off()
  dev.off()
  write.csv(inspect(rules_sorted), file = paste(imagedir, ".csv", sep=""))
}



#######################################################################################################################################


sh_func <- function(data = data, word = word){
  
  df <- data.table(data)
  keyword <- word
  
  df_tmp <- df[key %like% keyword]

  # NC 보통명사
  noun <- sapply(df_tmp$contents, extractNoun, USE.NAMES = FALSE)
  tmp_noun <- lapply(noun, function(x) x[nchar(x)>2])
  tmp_noun <- sapply(tmp_noun, paste, collapse = " ")
  
  corp <- Corpus(VectorSource(tmp_noun))
  #inspect(corp)
  tdm <- TermDocumentMatrix(corp)
  
  ## 단어간의 상관성 파악
  findAssocs(tdm, "수도권",0.2)
  word.count = as.array(rollup(tdm,2))   #매트릭스 행별 합계구하기
  word.order = order(word.count, decreasing = T)[1:30] #많이 쓰인 단어 순서정리(단어번호)
  freq.word = word.order[1:30]  #상위 1000개 단어만 재할당(단어번호)
  row.names(tdm[freq.word,])      #해당단어번호 단어 확인
  
  ## 단어 frequency
  freq <- data.frame(word.count)
  freq$name <- row.names(freq)
  freq <- freq %>% select(name, X1) %>% arrange(desc(X1))
  
  ## 워드 클라우드 
  imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/", substr(Sys.Date(), 1, 4), sep="")
  imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
  imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
  imagedir <- paste(imagedir, "wordcloud", sep="_")
  imagedir <- paste(imagedir, keyword, sep="_")
  imagename <- paste(imagedir, ".jpeg", sep="")
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
    shape = 'circle',
    ellipticity = 0.6)
  
  # save it in html
  library("htmlwidgets")
  saveWidget(my_graph,"tmp.html",selfcontained = F)
  # and in png
  webshot("tmp.html", imagename, delay =5, vwidth = 480, vheight=300) # changed to png. 
  
  write.csv(freq, 
            file= paste0(imagedir,".csv"), 
            row.names = F)
  
  ## DTM으로 변환
  dtm = as.DocumentTermMatrix(tdm[freq.word,])  
  
  library(igraph)
  dtmx <- as.matrix(dtm)
  # change it to a Boolean matrix
  dtmx[dtmx >= 1] <- 1
  # transform into a term-term adjacency matrix
  dtmx2 <- t(dtmx) %*% dtmx
  # build a graph from the above matrix
  g <- graph.adjacency(dtmx2, weighted=T, mode = "undirected")
  # remove loops
  g <- simplify(g)
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  V(g)$color <- rgb(135, 206, 250, max = 255)
  # plot the graph in layout1
  layout1 <- layout.fruchterman.reingold(g)
  par(mar=c(1, 1, 1, 1))
  plot(g, 
       layout=layout.fruchterman.reingold,
       vertex.label.font = 2,
       #vertex.size = 2, 
       #vertex.shape = "none", 
       edge.width=1 + E(g)$weight/100, 
       edge.arrow.width = E(g)$weight/100 
  )
  # output data
  # 연결중심성 : 한 노드에 직접적으로 연결된 링크들의 합으로 얻어진 중심성
  # 근접중심성 : 각 노드 간의 거리를 근거로 중심성을 측정하는 방법으로, 연결 정도 중심성과는 달리 직접적으로 연결된 노드뿐만 아니라 간접적으로 연결된 모든 노드 간의 거리를 합산해 중심성을 측정
  # 매개중심성 : 네트워크 내에서 한 노드가 담당하는 매개자 역할의 정도로서 중심성을 측정하는 방법
  # 위세중심성 : 연결된 노드의 중요성에 가중치를 둬 노드의 중심성을 측정하는 방법
  aa <- data.table('키워드' = V(g)$label, 
                   '연결중심성' = as.numeric(degree(g)), 
                   '근접중심성' = as.numeric(closeness(g)),
                   '매개중심성' = as.numeric(betweenness(g)),
                   '위세중심성' = as.numeric(evcent(g)$vector)
  )
  setorder(aa, -'연결중심성')
  # save plot and output data
  imagedir <- paste("X:/2020년 프로젝트/06.인천광역시_서구/Script/result/", substr(Sys.Date(), 1, 4), sep="")
  imagedir <- paste(imagedir, substr(Sys.Date(), 6, 7), sep="_")
  imagedir <- paste(imagedir, substr(Sys.Date(), 9, 10), sep="_")
  imagedir <- paste(imagedir, "SNA", sep="_")
  imagedir <- paste(imagedir, keyword, sep="_")
  imagename <- paste(imagedir, ".jpeg", sep="")
  dev.copy(jpeg, filename = imagename)
  dev.off()
  dev.off()
  write.csv(aa, file = paste(imagedir, ".csv", sep=""))
  
  
  
  
  ## make SNA
  #SNA_dtm(dtm)
  
  
}

#########################################################################################################################


sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '', sentence)        # 숫자 제거
    
    word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
  
}




