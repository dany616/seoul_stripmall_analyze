setwd("/Users/dany/workshop/seoul_stripmall_ analyze/seoul_YYYYmm")
library(ggmap)
library(readxl)

files <- c("201512","201606","201612","201706","201712")

# 먼저 첫 번째 파일의 열 이름들을 확인
filename <- paste("seoul_", files[1], ".xlsx", sep="")
cat("첫 번째 파일의 열 이름 확인:", filename, "\n")
ds_sample <- read_excel(filename)
print("실제 열 이름들:")
print(colnames(ds_sample))

# 실제 열 이름에 맞게 수정 (일단 주석처리)
# columns <- c("상가업소번호", "상호명", "상권업종대분류명", "상권업종중분류명", "상권업종소분류명", "시군구명", "행정동명", "경도", "위도")

ds.total <- NULL
for (i in 1:length(files)) {
filename <- paste("seoul_", files[i], ".xlsx", sep="") # 문자열 결합
cat("read ", filename, "...\n") # 읽을 파일 이름 출력
ds <- read_excel(filename) # 엑셀 파일 읽기
ds <- data.frame(ds) # 데이터프레임으로 변환
# ds <- ds[,columns] # 일단 주석처리하여 모든 열을 유지
ds$수집연월 <- rep(i, nrow(ds)) # 데이터 수집 시점
ds.total <- rbind(ds.total,ds) # 데이터 통합
}
head(ds.total)