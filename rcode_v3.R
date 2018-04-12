library(plyr)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("./data")

RES_2017B <- read_csv("RES_2017B.csv")
RISK <- read_csv("risk_by_job.csv")

#VLOOKUP 구현
RES_RISK <- join(RES_2017B, RISK, by='V21')

#필요한 변수만 선택하여 RES 데이터프레임 구축
#학력(V5, 0~7), 교육컨버전(1~3), 산업소분류, 직업소분류, 평균임금(V26), 경제활동구분(V42), 시도전국가중치, 대체확률) 
RES <- select(RES_RISK, V5, V9, V20, V21, V22, V26, V42, V44, RISK)
write.csv(RES, file="./result/RES.csv")

remove(RES_RISK)
remove(RES_2017B)

RES$V20_M = 0


# 산업별 그룹핑(V20)
RES$IndustryGroup=0
RES$IndustryGroup=cut(RES$V20,breaks=c(0,50,100,350,370,410,450,490,550,580,640,680,700,740,840,850,860,900,940,970,990,1000),
                       labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21), right = FALSE)
# table(res1$IndustryGroup)


# 직업별 그룹핑(V21)
RES$JobGroup=0
RES$JobGroup=cut(RES$V21,breaks=c(100,200,300,400,500,600,700,800,900,1000),
                  labels=c(1,2,3,4,5,6,7,8,9), right = FALSE)

# 컴퓨터화 위험별 그룹핑(RISK)
RES$RiskGroup=0
RES$RiskGroup=cut(RES$RISK, breaks=c(0, 0.3, 0.7,1), labels=c(1,2,3), right = FALSE)


# 컴퓨터화 위험별 그룹핑(RISK) - 세분화
#RES$RiskGroup2=0
#RES$RiskGroup2=cut(RES$RISK, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels=c(1,2,3,4,5,6,7,8,9,10), right = FALSE)

#RES$RiskGroup2=0
#RES$RiskGroup2=cut(RES$RISK, breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2,0.225,0.25,0.275,0.3,0.325,0.35,0.375,0.4,0.425,0.45,0.475,0.5,0.525,0.55,0.575,0.6,0.625,0.65,0.675,0.7,0.725,0.75,0.775,0.8,0.825,0.85,0.875,0.9,0.925,0.95,0.975,1), labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40), right = FALSE)



#직업별로 취업자 수 정렬
resultJob <- RES %>% 
  group_by(JobGroup, RiskGroup2) %>% 
  summarize(sum_Employment=sum(V44))
write.csv(resultJob, file="./result/resultJob.csv")


ggplot(resultJob, aes(RiskGroup2)) + geom_histogram()



# 직업*산업 정렬
resultJob_Frey <- RES %>% 
  group_by(JobGroup, RiskGroup2) %>% 
  summarize(sum_Employment=sum(V44))
write.csv(resultJob_Frey, file="./result/resultJob_Frey.csv")


# 학력별 그룹핑
# res1[res1$V9 == 1]
#RES$EduGroup = RES$V9
RES$EduGroup = RES$V5

# 임금 수준별(분위별 계산)
#quantile(RES$V26, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
RES$WageGroup=0
RES$WageGroup=cut(RES$V26, breaks=c(0, 120, 176, 230, 320, 10000), labels=c(1,2,3,4,5), right = FALSE)





#(분포)
# 직업*산업 정렬
resultJob_Indu <- RES %>% 
  group_by(JobGroup, IndustryGroup) %>% 
  summarize(sum_Employment=sum(V44))
write.csv(resultJob_Indu, file="./result/resultJob_Indu.csv")

# 직업*산업 정렬 II
resultJob_Indu <- RES %>% 
  group_by(JobGroup, IndustryGroup) %>% 
  summarize(sum_Employment=sum(V44))
write.csv(resultJob_Indu, file="./result/resultJob_Indu.csv")


#<확률>
# 직업별 확률
resultRISK_Job <- RES %>% 
  group_by(JobGroup) %>% 
  summarize(mean_RISK=mean(RISK))
write.csv(resultRISK_Job, file="./result/resultRISK_Job.csv")


# 직업별 확률 (가중평균)
resultRISKw_Job <- RES %>% 
  group_by(JobGroup) %>% 
  summarize(mean_RISKw=weighted.mean(RISK, V44))
write.csv(resultRISKw_Job, file="./result/resultRISKw_Job.csv")

# 직업별 확률 (가중평균 TOTAL)
resultRISKwa_Job <- RES %>% 
  group_by(V42) %>% 
  summarize(mean_RISKwa=weighted.mean(RISK, V44))
write.csv(resultRISKwa_Job, file="./result/resultRISKwa_Job.csv")

resultRISKa_Job <- RES %>% 
  group_by(V42) %>% 
  summarize(mean_RISKa=mean(RISK))





wm =0
wm < - weighted.mean(RES$RISK, RES$V44)


# 산업별 확률
resultRISK_Indu <- RES %>% 
  group_by(IndustryGroup) %>% 
  summarize(mean_RISK=weighted.mean(RISK, V44))
write.csv(resultRISK_Indu, file="./result/resultRISK_Indu.csv")

# 학력별 확률 
resultRISK_Edu <- RES %>% 
  group_by(EduGroup, V42) %>% 
  summarize(mean_RISK=weighted.mean(RISK, V44))
write.csv(resultRISK_Edu, file="./result/resultRISK_Edu.csv")

# 임금수준별 확률 
resultRISK_Wage <- RES %>% 
  group_by(WageGroup, V42) %>% 
  summarize(mean_RISK=weighted.mean(RISK, V44))
write.csv(resultRISK_Wage, file="./result/resultRISK_Wage.csv")




#remove(resultRisk_Indu)

#직업별로 취업자 수 정렬
resultJob <- RES %>% 
  group_by(JobGroup, RiskGroup) %>% 
  summarize(sum_Employment=sum(V44))
write.csv(resultJob, file="./result/resultJob.csv")


#산업별 취업자 수 정렬(대분류)
resultIndustry <- RES %>% 
  group_by(IndustryGroup, RiskGroup) %>% 
  summarize(sum_Employment=sum(V44))
write.csv(resultIndustry, file="./result/resultIndustry.csv")



#학력별 취업자 수 정렬
resultEdu <- RES %>% 
  group_by(EduGroup, RiskGroup) %>% 
  summarize(sum_Employment=sum(V44))
write.csv(resultEdu, file="./result/resultEdu.csv")



#임금별 취업자 수 정렬
resultWage <- RES %>% 
  group_by(WageGroup, RiskGroup) %>% 
  summarize(sum_Employment=sum(V44))
write.csv(resultWage, file="./result/resultWage.csv")



