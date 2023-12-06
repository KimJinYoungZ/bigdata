setwd("C:/Users/chech/Desktop/death/death")

# 초기화
from2008_to_2022 = list()

# 2004년부터 2022년까지 반복
for(year in 2008:2022) {
  filelist = choose.files()
  datalist = lapply(filelist, function(x) read.table(x, sep=",", fileEncoding="cp949"))
  from2008_to_2022[[as.character(year)]] = do.call("rbind", datalist)
}


#install.packages('gtools', repos='http://cran.us.r-project.org')

# plyr 패키지 로드
library(gtools)

# smartbind를 사용하여 모든 데이터 프레임을 하나로 합치기
death = do.call(smartbind, from2008_to_2022)

# 열 이름 지정
names(death) = c("신고_년", "신고_월", "신고_일", "주소지", "사망연월일", "사망시간", "사망장소", "사망자직업", "혼인상태", "교육정도", "성별", "사망연령", "사망원인1", "사망원인2")

# 결과 확인
head(death)

# '사망연월일' 열을 년도, 월, 일로 분리
death$년도 <- substr(death$사망연월일, 1, 4)
death$월 <- substr(death$사망연월일, 5, 6)
death$일 <- substr(death$사망연월일, 7, 8)

# '사망연월일' 열 제거
death$사망연월일 <- NULL

# 열 이름 다시 지정
names(death) <- c("신고_년", "신고_월", "신고_일", "주소지", "사망시간", "사망장소", "사망자직업", "혼인상태", "교육정도", "성별", "사망연령", "사망원인1", "사망원인2", "사망_년", "사망_월", "사망_일")

# 결과 확인
head(death)

#사망원인중 자살 추출하여 데이터셋 생성
num <- 600:849
num <- paste0("X", num)

# 벡터화된 조건을 사용하여 자살 사례 추출
suicide <- death[death$사망원인2 %in% num, ]

str(death)

summary(death)

# 총 데이터 개수(사망 건수)
Dcount <- nrow(death)
Dcount


str(suicide)


summary(suicide)


#자살 데이터 개수(자살 건수)
Scount <- nrow(suicide)
Scount

pop <- read.csv("연앙인구.csv")
age = c("10대 미만", "10대", "20대", "30대", "40대", "50대", "60대", "70대", "80대","90대")
rownames(pop) <- age[1:11]
pop
