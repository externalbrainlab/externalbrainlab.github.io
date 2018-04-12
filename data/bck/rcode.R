# Dataframe Manipulation을 위한 dplyr 라이브러리 로딩
# select() filter() group_by() summarize() mutate() 
library(dplyr)

# 워킹 디렉토리 지정 및 확인
setwd("d:/rwork/report1")
getwd()

# 데이터 로딩
library(readr)
RES_2017 <- read_csv("D:/rwork/report1/RES_2017.csv")
View(RES_2017)

res <- select(RES_2017, V2, V4, V9, V20, V21, V22, V26, V42, V43, V44, V45)
