#1. 분석대상 데이터준비 
setwd("/Users/dany/workshop/seoul_stripmall_ analyze/seoul_YYYYmm")
library(ggmap)
library(readxl)
library(ggplot2)

# 맥용 한글 폰트 설정
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



#3.기간별 분석

##1. 업종별 점포수의 변화

# 업종별 점포수의 변화
store.change <- aggregate(ds.total[,1],
by=list(연월=ds.total$수집연월,
업종대분류=ds.total$상권업종대분류명),
FUN=length)
head(store.change)

ggplot(store.change, aes(x=연월, y=x, colour=업종대분류, group=업종대분류)) +
    geom_line() +
    geom_point(size=6, shape=19, alpha=0.5) +
    ggtitle("업종별 점포수 변화 (대분류)") +
    ylab("점포수") +
    scale_x_continuous(breaks=1:5, labels=files) +
    theme(plot.title = element_text(color="black", size=14, face="bold"))


##2. 점포수 변화가 큰 상위 10개 업종
#ds.total 데이터프레임에서 업종별로 수집연월에 따른 데이터 개수(count)를 계산하여 store.tmp에 저장
store.tmp <- aggregate(ds.total[,1],
by=list(연월=ds.total$수집연월,
업종소분류=ds.total$상권업종소분류명),
FUN=length)
# store.tmp 데이터프레임의 세 번째 열 이름을 "count"로 변경
names(store.tmp)[3] <- c("count")
# 연월이 1인 데이터(2015년 12월 데이터)를 store.201512에 저장
store.201512 <- store.tmp[store.tmp$연월==1,]
# 확인
str(store.201512)
# store.201512 데이터프레임의 세 번째 열 이름을 "cnt_2015"로 변경
names(store.201512)[3] <- c("cnt_2015")
# 확인
str(store.201512)

# 연월이 5인 데이터(2017년 12월 데이터)를 store.201712에 저장
store.201712 <- store.tmp[store.tmp$연월==5,]
# store.201712 데이터프레임의 세 번째 열 이름을 "cnt_2017"로 변경
names(store.201712)[3] <- c("cnt_2017")
# 확인
str(store.201712)
#기본적으로 첫 번째 열을 기준으로 merge()를 수행
# store.201512와 store.201712를 업종소분류를 기준으로 병합
store.diff <- merge(store.201512[,2:3], store.201712[,2:3])
# 2015년과 2017년 데이터 개수 차이를 계산하여 diff 열에 저장, 
store.diff$diff <- abs(store.diff$cnt_2015 - store.diff$cnt_2017)
# diff 값을 기준으로 내림차순 정렬, 새로운 열로서 추가
store.diff <- store.diff[order(store.diff$diff, decreasing=T),]
# 상위 10개의 업종소분류명을 추출하여 top10에 저장
top10 <- store.diff[1:10,1]

#2. 점포수 변화가 큰 상위 10개 업종
store.change <- subset(store.tmp, store.tmp$업종소분류 %in% top10)
ggplot(store.change, aes(x=연월, y=count, colour=업종소분류,
group=업종소분류)) +
geom_line() +
geom_point(size=6, shape=19, alpha=0.5) +
ggtitle("점포수 변화 Top 10 업종(소분류)") +
ylab("점포수") +
scale_x_continuous(breaks=1:5,
labels=files) +
theme(plot.title = element_text(color="black", size=14, face="bold"))

#3. 구별 점포수의 변화
store.gu <- aggregate(ds.total[,1],
by=list(연월=ds.total$수집연월,
구이름=ds.total$시군구명),
FUN=length)
names(store.gu)[3] <- c("count")
ggplot(store.gu, aes(x=연월, y=count, colour=구이름, group=구이름)) +
geom_line( ) +
geom_point(size=6, shape=19, alpha=0.5) +
ggtitle("구별 점포수 변화 (대분류)") +
ylab("점포수") +
scale_x_continuous(breaks=1:5,
labels=files) +
theme(plot.title = element_text(color="black", size=14, face="bold"))

# ds.total 데이터프레임에서 동이름과 수집연월에 따른 데이터 개수(count)를 계산하여 store.tmp에 저장
store.tmp <- aggregate(ds.total[,1],
by=list(연월=ds.total$수집연월,
동이름=ds.total$행정동명),
FUN=length)
# store.tmp 데이터프레임의 세 번째 열 이름을 "count"로 변경
names(store.tmp)[3] <- c("count")
# 연월이 1인 데이터(2015년 12월 데이터)를 store.dong.201512에 저장
store.dong.201512 <- store.tmp[store.tmp$연월==1,]
# store.dong.201512 데이터프레임의 세 번째 열 이름을 "cnt_2015"로 변경
names(store.dong.201512)[3] <- c("cnt_2015")
# 연월이 5인 데이터(2017년 12월 데이터)를 store.dong.201712에 저장
store.dong.201712 <- store.tmp[store.tmp$연월==5,]
# store.dong.201712 데이터프레임의 세 번째 열 이름을 "cnt_2017"로 변경
names(store.dong.201712)[3] <- c("cnt_2017")
# store.dong.201512와 store.dong.201712를 동이름을 기준으로 병합
store.diff <- merge(store.dong.201512[,2:3], store.dong.201712[,2:3])
# 2015년과 2017년 데이터 개수 차이를 계산하여 diff 열에 저장
store.diff$diff <- abs(store.diff$cnt_2015 - store.diff$cnt_2017)
# diff 값을 기준으로 내림차순 정렬
store.diff <- store.diff[order(by=store.diff$diff, decreasing=T),]
# 상위 10개의 동이름을 추출하여 top10에 저장
top10 <- store.diff[1:10,1]
top10

#점포수 변화가 큰 상위 10개 동 그래프
store.change <- subset(store.tmp, store.tmp$동이름 %in% top10)
ggplot(store.change, aes(x=연월, y=count, colour=동이름, group=동이름)) +  # group=동이름 수정
geom_line() +
geom_point(size=6, shape=19, alpha=0.5) +
ggtitle("점포수 변화 Top 10 동") +
ylab("점포수") +
scale_x_continuous(breaks=1:5, labels=files) +
theme(plot.title = element_text(color="black", size=14, face="bold"))
