#1. 분석대상 데이터준비 
setwd("/Users/dany/workshop/seoul_stripmall_ analyze/seoul_YYYYmm")
library(ggmap)
library(readxl)
library(ggplot2)

# 한글 폰트 설정
par(family="AppleGothic")
theme_set(theme_gray(base_family="AppleGothic"))

files <- c("201512","201606","201612","201706","201712")
columns <- c( "상가업소번호", "상호명", "상권업종대분류명", "상권업종중분류명", "상권업종소분류명", "시군구명", "행정동명", "경도", "위도")
ds.total <- NULL

for (i in 1:length(files)) {
filename <- paste("seoul_", files[i], ".xlsx", sep="") # 문자열 결합
cat("read ", filename, "...\n") # 읽을 파일 이름 출력
ds <- read_excel(filename) # 엑셀 파일 읽기
ds <- data.frame(ds) # 데이터프레임으로 변환
ds <- ds[,columns] # 분석에 필요한 변수만 추출
ds$수집연월 <- rep(i, nrow(ds)) # 데이터 수집 시점
ds.total <- rbind(ds.total,ds) # 데이터 통합
}
head(ds.total)



#2. 데이터 탐색 

##1. 데이터 기본 정보 확인 및 분석 대상 데이터 추출
str(ds.total) # 요약
unique(ds.total$수집연월) # 수집연월
unique(ds.total$상권업종대분류명) # 상권업종 대분류
unique(ds.total$상권업종중분류명) # 상권업종 중분류
unique(ds.total$상권업종소분류명) # 상권업종 소분류

##2.업종별 점포수(대분류)
# 2017년 12월 데이터 추출 (수집연월 = 5)
ds.201712 <- ds.total[ds.total$수집연월 == 5, ]

store.level_1 <- aggregate(ds.201712[,1],
by=list(대분류=ds.201712$상권업종대분류명),
FUN=length)
store.level_1

names(store.level_1)[2] <- c("count")
ggplot(store.level_1, aes(x=대분류, y=count)) +
geom_bar(stat="identity", width=0.7, fill="steelblue") +
ggtitle("업종별 점포수") +
theme(plot.title = element_text(color="black", size=14, face="bold", family="AppleGothic"),
      text = element_text(family="AppleGothic"),
      axis.text = element_text(family="AppleGothic"))

##3. 구별 점포수 그래프의 작성
store.region <- aggregate(ds.201712[,1],
by=list(구이름=ds.201712$시군구명),
FUN=length)
store.region

names(store.region)[2] = c("count")
ggplot(store.region, aes(x=구이름, y=count)) +
geom_bar(stat="identity", width=0.7, fill="steelblue") +
ggtitle("구별 점포수") +
theme(plot.title = element_text(color="black", size=14, face="bold", family="AppleGothic"),
      text = element_text(family="AppleGothic"),
      axis.text = element_text(family="AppleGothic"),
      axis.text.x = element_text(angle = 45, family="AppleGothic"))

##4. 점포수가 많은 상위 10개 동 확인
# 점포수가 많은 상위 10개 동 확인
store.dong <- aggregate(ds.201712[,1],
by=list(동이름=ds.201712$행정동명),
FUN=length)
store.dong

names(store.dong)[2] = c("count")
store.dong <- store.dong[order(store.dong$count,decreasing=T),]
dong.top10 <- store.dong[1:10,]
dong.top10

ggplot(dong.top10, aes(x=reorder(동이름, -count), y=count)) +
geom_bar(stat="identity", width=0.7, fill="steelblue") +
ggtitle("점포수 많은 상위 10개동") +
theme(plot.title = element_text(color="black", size=14, face="bold", family="AppleGothic"),
      text = element_text(family="AppleGothic"),
      axis.text = element_text(family="AppleGothic"),
      axis.text.x = element_text(angle = 45, family="AppleGothic"))