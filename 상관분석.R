#install.packages("magrittr")
library(magrittr)

mytheme <- theme(
  plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
  axis.title.x = element_text(color = 'blue', size = 13, face = 'bold'),
  axis.title.y = element_text(color = '#993333', size = 13, face = 'bold'),
  axis.text.x = element_text(family = '맑은고딕', size = 13, face = 'bold'),
  axis.text.y = element_blank(), 
  axis.ticks.length = unit(0, 'cm'),
  panel.background = element_blank(),
  panel.grid = element_blank() )


# tm 패키지의 findAssocs() 함수 이용하여 상관계수 높은 단어를 확인
# 상관계수가 기준 이상인 단어들만 추출
checkAssocs <- function(dtm, keyword, corr = 0.01) {
  
  # 재직상태별 상관계수를 생성하고 데이터 프레임에 저장
  createDtmObj <- function(dtm, workGb, n = 10) {
    
    # 전직원/현직원별로 dtm을 나눕
    #dtmSmp <- dtm[rownames(x = dtm) %in% texts$id[texts$재직상태 == workGb], ]
    
    # 상관계수가 높은 단어만 저장
    assocs <- findAssocs(x = dtm, terms = keyword, corlimit = corr)
    
    # 재직상태별 상관계수로 데이터 프레임을 생성
    dtmObj <- eval(expr = parse(text = str_c('assocs', keyword, sep = '$'))) %>% 
      `[`(1:n) %>% 
      as.data.frame() %>% 
      set_colnames('corr') 
    
    # 행이름으로 word 컬럼을 생성
    dtmObj$word <- rownames(x = dtmObj) 
    
    # 행이름을 삭제합니다. 
    rownames(x = dtmObj) <- NULL 
    
    # workGb 컬럼을 생성
    dtmObj$workGb <- workGb
    
    # 결과를 반환합니다. 
    return(dtmObj)
  }
  
  # 행 기준으로 붙여서 dtmObj를 생성
  dtmObj <- rbind(createDtmObj(dtm = dtm, workGb = '매립지'))
  
  # 막대그래프 리스트를 생성합니다. 
  plots <- lapply(X = split(x = dtmObj, f = dtmObj$workGb), FUN = function(x) {
    
    # 단어의 순서를 상관계수 역순으로 재조정
    x$word <- factor(x = x$word, levels = x$word[order(x$corr, decreasing = TRUE)])
    
    # 막대그래프를 설정합니다. 
    ggplot(data = x, 
           mapping = aes(
             x = word, 
             y = corr, 
             width = 0.8)) +
      geom_col(fill = 'gray50') +
      geom_text(mapping = aes(label = corr), 
                size = 4, 
                vjust = -0.5) + 
      scale_y_continuous(limits = c(0, max(x$corr)*1.1 )) + 
      labs(x = '', 
           y = '상관계수',
           title = str_c('[', unique(x$workGb), ']', 
                         keyword, 
                         '관련 상관성 높은 단어', 
                         sep = ' ')) + 
      theme(legend.position = 'none') + mytheme 
  })
  
  # 2행으로 그래프
  do.call(what = gridExtra::grid.arrange, args = c(plots, nrow = 1))
}

