library(plyr)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("./data")

risk_job2 <- read_delim("risk_job2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)


ggplot(risk_job2,aes(x=risk))+geom_density(bins=50)


RES$RiskGroup2=0
RES$RiskGroup2=cut(RES$RISK, breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2,0.225,0.25,0.275,0.3,0.325,0.35,0.375,0.4,0.425,0.45,0.475,0.5,0.525,0.55,0.575,0.6,0.625,0.65,0.675,0.7,0.725,0.75,0.775,0.8,0.825,0.85,0.875,0.9,0.925,0.95,0.975,1), labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40), right = FALSE)


ggplot(RES,aes(x=RISK, y=V44))+geom_density(fill = "yellow", colour=NA, alpha=.5)
ggplot(RES,aes(x=RiskGroup2, y=V44))+geom_


p+geom_polygon(fill="white",color="black") + coord_map()

hist(risk_job2$, main="PGA Players Heights", bin=100, ylim=c(0, 100), xlab="cm",
     col="#80fd3d")


ggplot(risk_job2, aes(x=확률)) + geom_density(fill = "yellow", colour=NA, alpha=.5)
  
ggplot(risk_job2, mapping = aes(x = risk) + geom_point()
       
       
출처: http://kkokkilkon.tistory.com/17 [꼬낄콘의 분석일지]

출처: http://rfriend.tistory.com/67 [R, Python 분석과 프로그래밍 (by R Friend)]