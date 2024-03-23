ls()
getwd()
rm(list=ls())
# setwd("C:/Users/user/Desktop/상품검증 교육자료/서인교주임_교육자료")
# install.packages("devtools")
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("fmsb")
# install.packages("factorial2x2")
# install.packages("insTest")
# devtools::install_github("inkyoseo/insuranceTest")

library(data.table)
library(dplyr)
library(reshape2)
library(fmsb)
library(factorial2x2) # roundDown()
library(insTest)

options(repr.matrix.max.cols=50, repr.matrix.max.rows=100)

ST <- fread(file = "343I_S_230614.inf", header = F, stringsAsFactors = T, sep = "\n")
VT <- fread(file = "343I_V_230614.inf", header = F, stringsAsFactors = T, sep = "\n")
VS_layout <- fread("VS_layout.csv", header = F, sep = ",", encoding = "UTF-8")
VT
ST <- stringr::str_split_fixed(ST$V1, "[:space:]+", 112) %>% data.frame()
VT <- stringr::str_split_fixed(VT$V1, "[:space:]+", 112) %>% data.frame()
VS_layout
ST %>% head()
VT %>% head()

ncol(ST) # 112
ncol(VT) # 112
length(VS_layout[['V1']]) # 127

colnames(ST) <- t(VS_layout[1:112,])
colnames(VT) <- t(VS_layout[1:112,])

RT <- fread(file = "최신소방관위험률ver3.csv", header = T, encoding = "UTF-8")

RT <- RT %>% select(-cnt_qi, -Qi, -diff_no_qi, -q15)

PT <- fread(file = "343I_P_230614.inf", header = F)
P_layout <- fread(file = "P_layout.csv", header = T, stringsAsFactors = T, sep = ",", encoding = "UTF-8")
colnames(PT) <- t(P_layout)
RT %>% head(1)
PT %>% head(1)

#cov <- c(10703, 10704, 10705, 10706, 10707, 11296, 30188, 30190, 30040)
#cov <- c(11296, 30188, 30190, 30040)

#cov <- c(20250) # 소방관 셈플 
cov <- RT %>% select(코드) %>% unique() %>% .[[1]]
cov %>% t

PT <- PT %>% data.frame() %>% filter(코드 %in% cov)
VT <- VT %>% data.frame() %>% filter(코드 %in% cov)
ST <- ST %>% data.frame() %>% filter(코드 %in% cov)
RT <- RT %>% data.frame() %>% filter(코드 %in% cov)

col_base  <- colnames(PT %>% select("코드":"보험가입금액"))
col_base

RT2 <- RT %>% rename(성별구분 = 성별, 차량용도 = 용도, 보험가입금액 = 가입금액) 
RT2 %>% head(1)
cat("기존레이아웃(참고) : '코드''Q''유형''성별구분''급수''차량용도''보상한도''기타코드''나이''위험률'")

col_risk  <- RT2 %>% select(코드:보험가입금액) %>% colnames()
col_risk

ncol(PT) # 33
nrow(P_layout) # 33

PT %>% head(1)
ST %>% head(1)
VT %>% head(1)


### ◇ 유형별(가, 나, 다, ...)별 기수산식

기수산식 <- plyr::rbind.fill(cbind(Q = c(0), data.frame(유형 = c("보통약관", "가"), fo.lx = "(1-q0)", fo.llx = "(1-q0)", fo.Cx = "q0*lx")),
                         cbind(Q = c(0), data.frame(유형 = "나", fo.lx = "1", fo.llx = "1", fo.Cx = "q0*lx")),
                         cbind(Q = c(1:14), data.frame(유형 = "다", fo.lx = "1", fo.llx = "1", fo.Cx = "(q1+0.6*q2+0.4*q3+0.2*q4+0.1*(q5+q6)+0.05*q7+0.03*(q8+q9)+0.02*q10+0.01*(q11+q12+q13+q14))*lx")),
                         #cbind(Q = c(1:8), data.frame(유형 = "라", fo.lx = "1", fo.llx = "1", fo.Cx = "(q1+q2+q3+q4+q5+q6+q7+q8)*lx")),
                         cbind(Q = c(0), data.frame(유형 = "라", fo.lx = "1", fo.llx = "1", fo.Cx = "(q0)*lx")),
                         cbind(Q = c(0), data.frame(유형 = "마", fo.lx = "(1-q0)", fo.llx = "(1-q0)", fo.Cx = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.75, 1)*q0*lx")),
                         cbind(Q = c(0), data.frame(유형 = "바", fo.lx = "(1-q0)", fo.llx = "(1-q0)", fo.Cx = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.75*0.5, 1)*q0*lx")),
                         cbind(Q = c(1,2,3,4), data.frame(유형 = "사", fo.lx = "1", fo.llx = "1", fo.l1x = "(1-q1)", fo.l2x = "(1-q2)", fo.l3x = "(1-q3)", fo.l4x = "(1-q4)", fo.C1x = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.5, 1)*q1*l1x", fo.C2x = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.5, 1)*q2*l2x", fo.C3x = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.5, 1)*q3*l3x", fo.C4x = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.5, 1)*q4*l4x")), 
                         cbind(Q = c(1,2,3,4,5), data.frame(유형 = "아", fo.lx = "1", fo.llx = "1", fo.Cx = "(ifelse(t==0 & 기타코드 %in% c(0, 1), 0.75, 1)*q1*lx+0.2*(q2+q3+q4+q5)*lx)")),       
                         cbind(Q = c(1,2,3,4,5), data.frame(유형 = "자", fo.lx = "1", fo.llx = "1", fo.Cx = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.75*0.5, 1)*q1*lx+ifelse(t==0 & 기타코드 %in% c(0, 1), 0.5, 1)*0.2*(q2+q3+q4+q5)*lx")), 
                         cbind(Q = c(1,2,3), data.frame(유형 = "차", fo.lx = "(1-q1-q2)", fo.llx = "(1-q1-q2)", fo.l3x = "(1-q1-q2-q3)", fo.Cx = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.75*0.5, 1)*q1*lx + ifelse(t==0 & 기타코드 %in% c(0, 1), 0.5, 1)*q2*lx + ifelse(t==0 & 기타코드 %in% c(0, 1), 0.5, 1)*0.2*q3*l3x")),            
                         cbind(Q = c(0), data.frame(유형 = "카", fo.lx = "(1-q0)", fo.llx = "(1-q0)", fo.Cx = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.5, 1)*q0*lx")),
                         
                         cbind(Q = c(0), data.frame(유형 = "타", fo.lx = "1", fo.llx = "1", fo.Cx = "ifelse(t==0 & 기타코드 %in% c(0, 1), 0.5, 1)*q0*lx")),
                         
                         cbind(Q = c(1,2,3), data.frame(유형 = "파", fo.lx = "1", fo.llx = "1", fo.Cx = "(q1+q2+q3)*lx")))  


코드2유형 <- fread("담보코드2유형.csv", header = T, sep = ",", encoding = "UTF-8") 
기수산식2 <- 코드2유형 %>% left_join(기수산식, by = "유형")
#a %>% fwrite("담보위험률map.csv", bom = T)


기수산식3 <- 기수산식2 %>% select(-Q) %>% unique 

RT4 <- 기수산식3 %>% left_join(RT2) %>% unique()
RT4 %>% nrow()
RT4

납면위험률 <- fread(file = "납면위험률_공통.csv", header = T, stringsAsFactors = T, sep = ",", encoding = "UTF-8") %>% rename(나이 = 연령구분)
납면위험률 %>% head(1)

# 팔요한 위험률 가져오기 Q1000 : 정기사망률
납면위험률2  <-  납면위험률 %>% filter(Q==1000) %>% select(Q,나이, 성별구분, 차량용도, risk_rate)
납면위험률2 %>% head(1)
rm(납면위험률)


RT5 <- RT4 %>% left_join(납면위험률2) %>% unique %>% mutate(Q = paste0("Q", Q)) %>% dcast( ...~ Q, value.var = "risk_rate")


col_base  <- colnames(PT %>% select(코드:보험가입금액))


base <- PT %>% 
  select(all_of(col_base)) %>%
  filter(코드 %in% cov) %>%
  mutate(seq = row_number(), .before = 코드)

base %>% head

cov %>% length


paste0("시작시간 : ", Sys.time())
시간  <- Sys.time()

col_base  <- colnames(PT %>% select(코드:보험가입금액))
base3 <- NULL

for (x in cov) {
  base2 <- base %>% filter(코드 == x) %>% left_join(RT5) %>% unique()
  
  tmp <- base2 %>% colnames()
  col_base2 <- c(col_base, tmp[!tmp %in% col_base]) 
  
  tmp <- base2[, col_base2] %>% 
    arrange(seq, 연령구분, 나이) %>% 
    group_by(seq) %>%
    filter(나이 >= 연령구분, 나이 <= 연령구분 + 보험기간)
  
  base3 <- rbind(base3, tmp)
  rm(tmp, base2)
}

paste0("완료시간 : ", Sys.time())
paste0("소요시간 : ", Sys.time()-시간)


exp <- PT %>% filter(코드 %in% cov) %>% mutate(seq = row_number()) %>% select(seq, contains("exp_")) %>% rename_with(~stringr::str_remove(., "exp_"))
exp %>% head(2)
기수표 %>% head(2)



gisu <- base3 %>% insTest::base2gisu(0.0275)
gisu %>% fwrite("예시_기수표.csv")

prem <- gisu %>% insTest::gisu2prem(exp)
gisu <- gisu %>% select(-담보명)
prem <- gisu %>% insTest::gisu2prem(exp)
gisu <- gisu %>% select(-유형)
prem <- gisu %>% insTest::gisu2prem(exp)
prem %>% fwrite("예시_보험료.csv")

rsv <- gisu %>% insTest::gisu2reserve(exp)
rsv %>% fwrite("예시_준비금vr.csv")




