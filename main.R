setwd("C:/Users/김진영/Desktop/death/death")

# 초기화
from2008_to_2021 = list()

# 2004년부터 2022년까지 반복
for(year in 2008:2021) {
  filelist = choose.files()
  datalist = lapply(filelist, function(x) read.table(x, sep=",", fileEncoding="cp949"))
  from2008_to_2021[[as.character(year)]] = do.call("rbind", datalist)
}


#install.packages('gtools', repos='http://cran.us.r-project.org')

# gtools 패키지 로드
library(gtools)

# smartbind를 사용하여 모든 데이터 프레임을 하나로 합치기
death = do.call(smartbind, from2008_to_2021)

# 열 이름 지정
names(death) = c("신고_년", "신고_월", "신고_일", "주소지", "사망연월일", "사망시간", "사망장소", "사망자직업", "혼인상태", "교육정도", "성별", "사망연령", "사망원인1", "사망원인2")

# 결과 확인
head(death)
tail(death)
# '사망연월일' 열을 년도, 월, 일로 분리
death$년도 <- substr(death$사망연월일, 1, 4)
death$월 <- substr(death$사망연월일, 5, 6)
death$일 <- substr(death$사망연월일, 7, 8)

# '사망연월일' 열 제거
death$사망연월일 <- NULL

# 열 이름 다시 지정
names(death) <- c("신고_년", "신고_월", "신고_일", "주소지", "사망시간", "사망장소", "사망자직업", "혼인상태", "교육정도", "성별", "사망연령", "사망원인1", "사망원인2", "사망_년", "사망_월", "사망_일")


#사망원인중 자살 추출하여 데이터셋 생성
num <- 600:849
num <- paste0("X", num)

# 벡터화된 조건을 사용하여 자살 사례 추출
suicide <- death[death$사망원인2 %in% num, ]

rownames(suicide) <- 1:nrow(suicide)
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

pop <- read.csv("연앙인구.csv", stringsAsFactor=FALSE)

# 데이터프레임을 행렬로 변환
pop <- as.matrix(pop)

# 행렬 내의 모든 값을 숫자로 변환
pop <- apply(pop, 2, as.numeric)

colnames(pop) <- c(2008:2021)
age = c("10대 미만", "10대", "20대", "30대", "40대", "50대", "60대", "70대", "80대 이상","나이 미상")
rownames(pop) <- age[1:9]

# 결과 확인
pop

suicide$사망연령 <- as.numeric(suicide$사망연령)

# '사망연령'의 빈도수 계산
age_table <- table(suicide$사망연령)

# 결과 출력
print(age_table)

# 연령대별로 구분
suicide$group <- cut(suicide$사망연령, 
                     breaks = c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, 109, Inf),
                     labels = age,
                     right = TRUE)





year = 0
k<-matrix(nrow=10, ncol = 14)

for(i in 1:14){
  year = subset(suicide, suicide$사망_년==2007+i, drop=F)
  print(year)
  year = table(year$group)
  for(j in 1:10)
    k[j,i] <- as.numeric(year[[j]])
}



year_age = k
year_age[9,] = year_age[9,] + year_age[10,]
year_age = year_age[-10,]
colnames(year_age)<-c(2008:2021)
rownames(year_age)<-age[1:9]
for (i in 1:9){
  year_age[i,] = year_age[i,]*100000/pop[i, ] 
}

k
year_sum = colSums(k)
pop2 <- t(pop[,c(-2,-3)])

year_sum
pop_sum= colSums(pop)

year_death_per = as.data.frame(year_sum*100000/pop_sum)
years = c(2008:2021)
year_death_per = cbind(years, year_death_per)
colnames(year_death_per) = c("year", "rate")


str(year_death_per)
#install.packages("ggplot2")

library(ggplot2)
ggplot(year_death_per, aes(x=year,y=rate))+
  geom_line()+
  geom_point(size=2) +
  ggtitle("자살률") +labs(list(x="연도", y="자살률"))+
  geom_text(aes(label=round(rate,1)), size=5, vjust=-0.8)




#install.packages('RColorBrewer', repos='http://cran.us.r-project.org')

library(RColorBrewer)

color = brewer.pal(10, "Paired")

plot.new()
par(mfrow=c(1,1),mar=c(1,4.1,0,4.1))
plot.new()
legend("top", ncol=5,inset=.05, legend=rownames(year_age),pch=16, col = color)
par(new=T)
par(mfrow=c(1,1),mar=c(5.1,4.1,5.1,2.1))
matplot(colnames(year_age),t(year_age),lwd=3 , xlim = c(2008,2022),ylim = c(0,100),xlab='Years', ylab='rate',col =color, type = "l",lty = 1 )

suicide$사망원인2 = droplevels(suicide$사망원인2)
suicide$사망원인2 = substr(suicide$사망원인2, 1, 3)
suicide$사망원인2 = as.factor(suicide$사망원인2)

levels(suicide$사망원인2) = list("기타 약물"="X60", "수면제"="X61", "마약 및 환각제"="X62", "기타 약물"="X63", "기타 약물"="X64",
                             "알콜"="X65", "휘발물질"="X66", "가스"="X67", "농약"="X68", "기타 독성물질"="X69",
                             "목맴"="X70", "익사"="X71", "권총발사"="X72", "기타 화기발사"="X73", "기타 화기발사"="X74", 
                             "폭발성 물질"="X75", "불"="X76", "증기"="X77", "예리한 물체"="X78", "둔한 물체"="X79",
                             "추락"="X80", "충돌"="X81", "충돌"="X82", "기타"="X83", "상세불명"="X84")


#install.packages("forcats")
#install.packages("dplyr")
library(forcats)
library(dplyr)

# 상위 10개 자살 원인의 빈도 계산
top_causes <- sort(table(suicide$사망원인2), decreasing=TRUE)[1:10]

# 상위 10개 자살 원인 설명
sui <- c("목맴", "농약", "추락", "가스", "기타 독성물질", "익사", "상세불명", "예리한 물체", "불", "수면제")

# 상위 10개 원인과 그 빈도를 데이터 프레임으로 생성
top_causes_df <- data.frame(원인 = names(top_causes), 빈도 = as.numeric(top_causes))

# 상위 10개 원인의 설명을 매핑
top_causes_df$원인설명 <- sui[match(top_causes_df$원인, names(top_causes))]

# ggplot2를 사용한 시각화
library(ggplot2)
ggplot(top_causes_df, aes(x=reorder(원인설명, -빈도), y=빈도)) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("자살 원인") +
  ylab("빈도") +
  ggtitle("상위 10개 자살 원인") +
  theme_minimal() +
  coord_flip()

Stime = subset(suicide10, suicide10$사망시간!=99)
Stimefreq = table(Stime$사망시간)
ggplot(Stime, aes(x=사망시간)) +
  geom_line(stat='count') +
  labs(list(title="시간대별 자살 빈도", x="시간", y="빈도"))+
  geom_vline(xintercept=c(9.5,16.5), linetype='dotted', color='red', size=2)


table(death$사망장소)
table(suicide$사망장소)
death$사망장소 <- replace(death$사망장소, death$사망장소 ==99, 11)
suicide$사망장소 <- replace(suicide$사망장소, suicide$사망장소 ==99, 11)

Splace_freq <-table(suicide$사망장소)
Splace_per <-round(Splace_freq/sum(Splace_freq)*100,1)
place<-c("주택","의료기관","사회복지시설","공공시설","도로","상업,서비스시설","산업장","농장","병원이송중","기타","미상")

Splace_lbs<-paste0(Splace_per,"%")

pie(Splace_freq,labels=Splace_lbs,cex.main=3,main="장소",init.angle=90, radius=1,col=rainbow(11))
legend(x="bottom", legend=place, fill=rainbow(11),cex=1, ncol=2)

s00 = subset(suicide, group=="10대 미만")
s10 = subset(suicide, group=="10대")
s20 = subset(suicide, group=="20대")
s30 = subset(suicide, group=="30대")
s40 = subset(suicide, group=="40대")
s50 = subset(suicide, group=="50대")
s60 = subset(suicide, group=="60대")
s70 = subset(suicide, group=="70대")
s80 = subset(suicide, group=="80대 이상")
sNA = subset(suicide, group=="Na")

sexM <- c(nrow(s00[s00$성별==1,]), nrow(s10[s10$성별==1,]),
          nrow(s20[s20$성별==1,]), nrow(s30[s30$성별==1,]),
          nrow(s40[s40$성별==1,]), nrow(s50[s50$성별==1,]),
          nrow(s60[s60$성별==1,]), nrow(s70[s70$성별==1,]),
          nrow(s80[s80$성별==1,]), nrow(sNA[sNA$성별==1,]))

sexF <- c(nrow(s00[s00$성별==2,]), nrow(s10[s10$성별==2,]),
          nrow(s20[s20$성별==2,]), nrow(s30[s30$성별==2,]),
          nrow(s40[s40$성별==2,]), nrow(s50[s50$성별==2,]),
          nrow(s60[s60$성별==2,]), nrow(s70[s70$성별==2,]),
          nrow(s80[s80$성별==2,]), nrow(sNA[sNA$성별==2,]))


#install.packages('pyramid', repos='http://cran.us.r-project.org')

#install.packages('readxl', repos='http://cran.us.r-project.org')

library(pyramid)
library(readxl)

Ssex<- data.frame(sexM,sexF,age)
pyramid::pyramid(Ssex,main="2008~2022년 성별 및 연령대 자살 현황",Laxis=seq(0,30000,len=5),Cstep=1,Cgap = .4)

suicide$성별 = as.factor(suicide$성별)
levels(suicide$성별) = list("남자"=1, "여자"=2)

ggplot(data=subset(suicide10, !is.na(사망원인2)), aes(fct_infreq(사망원인2), fill=factor(성별), group=성별)) + 
  geom_bar() + ggtitle("성별 자살방법") +labs(list(x="자살 방법", y="빈도", fill="성별"))+
  scale_fill_manual(values = c( "#99CCCC", "#FF9999"))




# 사망_월에 따른 자살 빈도 계산
suicide_monthly <- table(suicide$사망_월)

# 데이터 프레임으로 변환
suicide_monthly_df <- data.frame(월 = names(suicide_monthly), 빈도 = as.numeric(suicide_monthly))

# ggplot2를 사용한 선 그래프 시각화
ggplot(suicide_monthly_df, aes(x = 월, y = 빈도, group = 1)) +
  geom_line() +  # 선 그래프 추가
  geom_point() + # 포인트 추가
  theme_minimal() +
  ggtitle("월별 자살 빈도") +
  xlab("월") +
  ylab("자살 빈도수")


s00_freq<-table(s00$사망원인2)
s00_per<-round(s00_freq*100/sum(s00_freq),1)
s00_lbs<- paste(s00_per, "%", sep="")
s10_freq<-table(s10$사망원인2)
s10_per<-round(s10_freq*100/sum(s10_freq),1)
s10_lbs<- paste(s10_per, "%", sep="")
s20_freq<-table(s20$사망원인2)
s20_per<-round(s20_freq*100/sum(s20_freq),1)
s20_lbs<- paste(s20_per, "%", sep="")
s30_freq<-table(s30$사망원인2)
s30_per<-round(s30_freq*100/sum(s30_freq),1)
s30_lbs<- paste(s30_per, "%", sep="")
s40_freq<-table(s40$사망원인2)
s40_per<-round(s40_freq*100/sum(s40_freq),1)
s40_lbs<- paste(s40_per, "%", sep="")
s50_freq<-table(s50$사망원인2)
s50_per<-round(s50_freq*100/sum(s50_freq),1)
s50_lbs<- paste(s50_per, "%", sep="")
s60_freq<-table(s60$사망원인2)
s60_per<-round(s60_freq*100/sum(s60_freq),1)
s60_lbs<- paste(s60_per, "%", sep="")
s70_freq<-table(s70$사망원인2)
s70_per<-round(s70_freq*100/sum(s70_freq),1)
s70_lbs<- paste(s70_per, "%", sep="")
s80_freq<-table(s80$사망원인2)
s80_per<-round(s80_freq*100/sum(s80_freq),1)
s80_lbs<- paste(s80_per, "%", sep="")
#install.packages("randomcoloR")

library(randomcoloR)
a<-randomColor(21)
par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1),oma=c(0,0,0,0))
plot.new()

par(mfrow=c(3,3),mar=c(5.1, 4.1, 4.1, 2.1),oma=c(0,0,0,15))

freq00 = table(s00$사망원인2)
pie(freq00, labels=s00_lbs, main="10대 미만", radius=1, init.angle=90,col=a)
freq10 = table(s10$사망원인2)
pie(freq10, labels=s10_lbs, main="10대", radius=1, init.angle=90,col=a)
freq20 = table(s20$사망원인2)
pie(freq20, labels=s20_lbs, main="20대", radius=1, init.angle=90,col=a)
freq30 = table(s30$사망원인2)
pie(freq30, labels=s30_lbs, main="30대", radius=1, init.angle=90,col=a)
freq40 = table(s40$사망원인2)
pie(freq40, labels=s40_lbs, main="40대", radius=1, init.angle=90,col=a)
freq50 = table(s50$사망원인2)
pie(freq50, labels=s50_lbs, main="50대", radius=1, init.angle=90,col=a)
freq60 = table(s60$사망원인2)
pie(freq60, labels=s60_lbs, main="60대", radius=1, init.angle=90,col=a)
freq70 = table(s70$사망원인2)
pie(freq70, labels=s70_lbs, main="70대", radius=1, init.angle=90,col=a)
freq80 = table(s80$사망원인2)
pie(freq80, labels=s80_lbs, main="80대 이상", radius=1, init.angle=90,col=a)


par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 0.2),oma=c(0,0,0,0))
par(new=T)
lbsu<-dimnames(freq00)[[1]]
legend("right",lbsu,fill=a)
