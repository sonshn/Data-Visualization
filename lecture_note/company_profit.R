# Week13_0531_R시각화_그래프
my <- data.frame(
  company = c('Walmart','BP','Amazon','BASF','IBM','PG','Airbus',
              'Apple','Nestle','Tesla'),
  profit = c(-97,24,68,257,35,41,59,216,-83, 270)
)

my

libray(ggplot2)

# ggplot 기본 구조
ggplot(data=my, aes(x=company,y=profit))
# 기본 막대그래프
ggplot(data=my, aes(x=company,y=profit)) + geom_bar(stat = 'identity')
# 막대 폭 조절
ggplot(data=my, aes(x=company,y=profit)) + geom_bar(stat = 'identity', width = 0.3)

# 막대그래프 정렬하기(profit이 높은 것부터 왼쪽에 배치)
ggplot(data=my, aes(x=reorder(company,-profit),y=profit)) + geom_bar(stat = 'identity', width = 0.3)
# 막대그래프 제목 및 레이블 설정
ggplot(data=my, aes(x=reorder(company,-profit),y=profit)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익')

# 막대그래프 테두리 색상설정
ggplot(data=my, aes(x=reorder(company,-profit),y=profit)) + geom_bar(stat = 'identity', width = 0.3, color='green') + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익')
# 막대그래프 막대색상 설정
ggplot(data=my, aes(x=reorder(company,-profit),y=profit)) + geom_bar(stat = 'identity', width = 0.3, color='green', fill='red') + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익')
# 막대그래프 막대색상 채도조정
ggplot(data=my, aes(x=reorder(company,-profit),y=profit)) + geom_bar(stat = 'identity', width = 0.3, color='green',fill='red', alpha = 0.5) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익')

# ggplot 라이브러리의 fill 옵션을 통해 막대에 색을 자동반영 할 수 있음
# 막대그래프 항목별 색상설정
ggplot(data=my, aes(x=reorder(company,-profit),y=profit, fill = company)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익')

# 막대그래프의 범례(legend) 삭제
ggplot(data=my, aes(x=reorder(company,-profit),y=profit, fill = company)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + theme(legend.position = "none")

# 막대그래프의 특정항목을 시각적으로 색상을 강조
library(dplyr)
library(magrittr)

# 특정기업 강조 표시 변수 생성
# my에 flag라는 강조 표시 변수를 추가
my %<>% mutate(flag = ifelse(company == "Apple",T,F))
my

# 막대그래프 항목별 색상 자동설정 (Apple 막대만 색깔이 다름(강조))
ggplot(data=my, aes(x=reorder(company,-profit),y=profit, fill = flag)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익')

# 막대그래프 항목 레이블 출력각도 조절
ggplot(data=my, aes(x=reorder(company,-profit),y=profit, fill = flag)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + theme(axis.text.x = element_text(angle=45,hjust=1))

# 막대그래프 항목별 색상 수동설정 (Apple 막대만 색깔이 다름(주황색))
ggplot(data=my, aes(x=reorder(company,-profit),y=profit, fill = flag)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + scale_fill_manual(values = c('grey','orange')) + theme_bw() + theme(legend.position = "none")

# 막대그래프 가로방향 출력 (낮은 수치가 위에 배치)
ggplot(data=my, aes(x=reorder(company,-profit),y=profit, fill = flag)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + scale_fill_manual(values = c('grey','orange')) + theme_bw() + theme(legend.position = "none") + coord_flip()

# 막대그래프 가로방향 출력 (높은 수치가 위에 배치)
ggplot(data=my, aes(x=reorder(company,profit),y=profit, fill = flag)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + scale_fill_manual(values = c('grey','orange')) + theme_bw() + theme(legend.position = "none") + coord_flip()

# 특정조건에 해당하는 항목을 강조

# profit이 0이하인 기업을 강조표시 변수 생성
my %<>% mutate(minus = ifelse(profit <0, T, F))
my
# 'fill = minus'
ggplot(data=my, aes(x=reorder(company,profit),y=profit, fill = minus)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + scale_fill_manual(values = c('grey80','brown4')) + theme_bw() + theme(legend.position = "none") + coord_flip()
# profit이 100이상인 기업을 강조표시 변수 생성
my %<>% mutate(over100 = ifelse(profit >=0, T, F))
my

# 한 변수에 여러 profit 조건을 적용하는 변수 생성
my %<>% mutate(result = ifelse(profit >= 100, 'Good', 
                               ifelse(profit >= 0 & profit < 100, 'Normal', 'Bad')))
my

# 범주형 변수 빈도분석
table(my$result)

# 막대그래프 항목색상 설정 시 범주변수 기본순서에 따름
ggplot(data=my, aes(x=reorder(company,profit),y=profit, fill = result)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + scale_fill_manual(values = c('steelblue4','brown4','grey80')) + theme_bw() + theme(legend.position = "none") + coord_flip()

# 범주형 변수를 팩터형으로 전환해 순서지정
my$result <- factor(my$result, levels = c('Good','Normal','Bad'))
table(my$result)

# 막대그래프 항목색상 설정 시 팩터변수 우선순서에 따름
ggplot(data=my, aes(x=reorder(company,profit),y=profit, fill = result)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + scale_fill_manual(values = c('steelblue4','brown4','grey80')) + theme_bw() + theme(legend.position = "none") + coord_flip()

# R predefined color names
colors() # length: 657

# R color pallet
library(RColorBrewer)
display.brewer.all()
brewer.pal.info
brewer.pal(n=3, name= 'Greens')

# 색상 파레트 이용하여 막대 그래프 색상 설정
ggplot(data=my, aes(x=reorder(company,profit),y=profit, fill = result)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + scale_fill_brewer(palette = "Greens") + theme_bw() + theme(legend.position = "none") + coord_flip()

# 단일 색상 파레트의 색상 '순서'를 변경하고 싶을 때
ggplot(data=my, aes(x=reorder(company,profit),y=profit, fill = result)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + scale_fill_brewer(palette = "Greens", direction = -1) + theme_bw() + theme(legend.position = "none") + coord_flip()

# 다중 색상파레트 사용
ggplot(data=my, aes(x=reorder(company,profit),y=profit, fill = result)) + geom_bar(stat = 'identity', width = 0.3) + ggtitle('기업별 이익비교') + xlab('기업') + ylab('이익') + scale_fill_brewer(palette = "RdBu") + theme_bw() + theme(legend.position = "none") + coord_flip()

# 막대항목 패턴 라이브러리 활용
install.packages("remotes")
remotes::install_github("coolbutuseless/ggpattern")
