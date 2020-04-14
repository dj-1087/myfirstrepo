##평균, 중앙값, 절단평균
avg <- read.csv("./평균,중앙값,절단평균/average.csv", header = T, na.strings = NA); str(avg)
mean(avg$x, na.rm = T)
median(avg$x)
summary(avg)
mean(avg$x, trim = 0.1)
boxplot(avg)
avg <- avg[!(avg$x==1685),]
boxplot(avg)
summary(avg)
library(psych)
describe(avg)
##기하평균, 조화평균
#기하평균
corona <- read.csv("./기하평균,조화평균/corona.csv",header = T, na.strings = NA); str(corona)
summary(corona)
#식
(max(corona$confirmed_case)/min(corona$confirmed_case))^(1/(length(corona$confirmed_case)-1))-1

#페키지
library(psych)
cagr <- vector(length = length(corona$confirmed_case)-1)
for (i in 1:length(corona$confirmed_case)-1) {
  cagr[i] <- corona$confirmed_case[i+1]/corona$confirmed_case[i]
}; str(cagr)
geometric.mean(cagr)-1

#조화평균
#1번) A는 학교에 아침에는 등교하는데 7km/h로 가고 저녁에 하교하는데 5km/h로 돌아갑니다. 이 때의 조화평균을 구하세요.
km_h <- c(7,5)
harmonic.mean(km_h)
#2번) B는 미국으로 여행을 가는데 갈 때는 900km/h의 비행기를 사용하고, 귀국할 때는 200km/h의 여객선을 사용합니다. 이 때의 조화평균을 구하세요.
airplane <- c(900,200)
harmonic.mean(airplane)

##가중평균
wt_avg <- read.csv("./가중평균/weighted average.csv",header = T,na.strings = NA)
weighted.mean(wt_avg$y1,wt_avg$y2)

##분포
dmb <- read.csv("./분포/DMB재난경보방송발령현황(2020년).csv",header = T, na.strings = NA); str(dmb)
local <- summary(dmb$지역); local
describe(local)
local <- local[-100]; local
describe(local)
summary(local)
local.z <- scale(local); local.z
local.t <- cbind(local, local.z); local.t

##다변량
health <- read.csv("./다변량/건강검진정보.csv",header = T, na.strings = NA); str(health)
#전처리
library(dplyr)
attach(health)
health <- health%>%select(-치아우식증유무,-결손치유무,-치아마모증유무,-제3대구치.사랑니.이상); str(health)
health$성별코드 <- factor(성별코드, levels = c(1,2), labels = c("남","여")); str(health$성별코드)
detach(health)

summary(health)
describe(health)
