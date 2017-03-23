# initial setting
source("./fds/library.R")
source("./fds/configuration.R", encoding='UTF-8')


# data loading
rm(list=ls())
load("./fds/fds_raw.Rdata")
load("./fds/derived_variables.Rdata")

# 데이터 마트 with 기본변수 13개. 
# 단, 5개 변수 : RESI_COST, OCCP_GRP_2, WEDD_YN, CHLD_CNT, LTBN_CHLD_AGE는
# knn imputation한 후 merge를 위해 제외


mart <- cust[, c("CUST_ID", "DIVIDED_SET",     # 형변환 불필요
                 "SIU_CUST_YN", "SEX", "AGE", "FP_CAREER",
                 "RESI_COST", "OCCP_GRP_2", "WEDD_YN", "CHLD_CNT", "LTBN_CHLD_AGE",       # imputation target
                 "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM")]  # temp variables

# type conversion -----
mart$SIU_CUST_YN        <- as.factor(mart$SIU_CUST_YN)
mart$SEX                <- as.factor(mart$SEX)
levels(mart$SEX)        <- c("m", "f")                  # caret에서 숫자형 factor 오류발생 가능성
mart$AGE                <- as.numeric(mart$AGE)
mart$FP_CAREER          <- as.factor(mart$FP_CAREER)

mart$RESI_COST          <- as.numeric(mart$RESI_COST)             # imputation target
mart$OCCP_GRP_2         <- as.factor(mart$OCCP_GRP_2)             # imputation target
mart$WEDD_YN            <- as.factor(mart$WEDD_YN)                # imputation target
mart$CHLD_CNT           <- as.numeric(mart$CHLD_CNT)              # imputation target
mart$LTBN_CHLD_AGE      <- as.numeric(mart$LTBN_CHLD_AGE)         # imputation target

mart$RESI_TYPE_CODE     <- as.factor(mart$RESI_TYPE_CODE)         # used only for knn
mart$CUST_INCM          <- as.numeric(mart$CUST_INCM)             # used only for knn
mart$RCBASE_HSHD_INCM   <- as.numeric(mart$RCBASE_HSHD_INCM)      # used only for knn
mart$JPBASE_HSHD_INCM   <- as.numeric(mart$JPBASE_HSHD_INCM)      # used only for knn
# -----


# join derived variables  -----

# 파생변수 리스트 
# DV1_TOT_MON_PREM      : 총월납보험료 
# DV2_NCLAIM_AFT1408    : 2년내 청구건수
# DV3_TOT_PAYM_AFT1408  : 2년내 수령금액 합산
# DV4_PERI_FRS_CLAIM    : 최초청구까지 시간
# DV5_W2Y_HSPT_MAX      : 병원별 2년내 보험청구건 중 최대치
# DV6_W2Y_DCTR_MAX      : 의사별 2년내 보험청구건 중 최대치
# DV7_FMLY_HSPT_SUM     : 본인 및 가족 공통 청구 병원 중 최대건수
# DV8_FMLY_DCTR_SUM     : 본인 및 가족 공통 청구 의사 중 최대건수
# DV9_HEED_HOSP_N       : 금감원 유의 대상 병원 청구 횟수

mart <- merge(mart, DV1_TOT_MON_PREM, by="CUST_ID", all.x=T)
mart <- merge(mart, DV2_NCLAIM_AFT1308, by="CUST_ID", all.x=T)
mart <- merge(mart, DV3_TOT_PAYM_AFT1308, by="CUST_ID", all.x=T)
mart <- merge(mart, DV4_PERI_FRS_CLAIM, by="CUST_ID", all.x=T)
mart <- merge(mart, DV5_W3Y_HSPT_MAX, by="CUST_ID", all.x=T)
mart <- merge(mart, DV6_W3Y_DCTR_MAX, by="CUST_ID", all.x=T)
mart <- merge(mart, DV7_FMLY_HSPT_SUM, by="CUST_ID", all.x=T)
mart <- merge(mart, DV8_FMLY_DCTR_SUM, by="CUST_ID", all.x=T)
mart <- merge(mart, DV9_HEED_HOSP_N, by="CUST_ID", all.x=T)
# -----


# basic summary -----
str(mart)
summary.formula(DIVIDED_SET~. , data=mart, method="reverse")
describe(mart)
# -----


# 변수 의미상 파생변수 DV2,3,7,8,9 NA를 0으로 처리해야 함 -----

# 기존 NA 개수 : 10911 / 10874 / 22024 / 22024 / 21224
# 기존 0값 개수 : 0 / 19 / 127 / 161 / 0 
# DV3의 경우, PAYM_AMT가 0으로 기록되어 있는 건이 있음.
# DV7, 8의 경우, 본인과 가족의 공통 방문병원이 없으면 0이 되도록 처리해 놓음
nrow(mart %>% filter(DV2_NCLAIM_AFT1308==0))
nrow(mart %>% filter(DV3_TOT_PAYM_AFT1308==0))
nrow(mart %>% filter(DV7_FMLY_HSPT_SUM==0))
nrow(mart %>% filter(DV8_FMLY_DCTR_SUM==0))
nrow(mart %>% filter(DV9_HEED_HOSP_N==0))

# subtitute NA for 0
mart[is.na(mart$DV2_NCLAIM_AFT1308),"DV2_NCLAIM_AFT1308"] <- 0
mart[is.na(mart$DV3_TOT_PAYM_AFT1308),"DV3_TOT_PAYM_AFT1308"] <- 0
mart[is.na(mart$DV7_FMLY_HSPT_SUM),"DV7_FMLY_HSPT_SUM"] <- 0
mart[is.na(mart$DV8_FMLY_DCTR_SUM),"DV8_FMLY_DCTR_SUM"] <- 0
mart[is.na(mart$DV9_HEED_HOSP_N),"DV9_HEED_HOSP_N"] <- 0
# -----


# partitioning -----
train <- 
  mart %>% 
  filter(DIVIDED_SET=="1")

test <- 
  mart %>% 
  filter(DIVIDED_SET=="2")

# -----


# ===================================================================================
# 1. OCCP_GRP_2 595건, WEDD_YN, CHLD_CNT, LTBN_CHLD_AGE 473건 imputation 필요함.
# 2. TOTALPREM, MATE_OCCP_GRP_2는 제외 결정.
# ===================================================================================
# asymmetry, near-zero variance 처리는 순서에 영향을 미치지 않으므로 preprocessing 제외
# 보통 decision tree에서 normalization이나 scaling이 성능에 영향을 미치지는 않으므로 하지 않음
# (이 작업을 수행하는 다른 모델과 비교할 때는 해볼 수 있으나, 여기선 RF만 할 것임.)
# Input Data for Decision Trees 논문에 따르면, 결정트리에서 다중공선성 문제는
# 고려하지 않아도 됨. 다중공선성 제거 시도가 오히려 성능을 낮춤.
# ===================================================================================

# 참고 : WEDD_YN, CHLD_CNT, LTBN_CHLD_AGE 탐색  -----

table(cust$WEDD_YN)
cust %>% 
  filter(WEDD_YN=="") %>% 
  summarise(n=n())  # ""로 된 값 473건 확인.

cust %>% 
  filter(WEDD_YN=="", is.na(CHLD_CNT), is.na(LTBN_CHLD_AGE)) %>% 
  select(CUST_ID, SEX, AGE, WEDD_YN, CHLD_CNT, LTBN_CHLD_AGE) %>% 
  nrow() # 총 473건 확인
# 대부분 고연령대. 물어보기 어려운 연령대인듯.

# 세 변수가 동일한 행에서 NA 발견 -----


# knn imputation of RESI_COST, OCCP_GRP_2, WEDD_YN, CHLD_CNT, LTBN_CHLD_AGE  ------


# test셋의 imputatiton에 사용하기 위해서
# 소득추정을 위한 4개 임시 변수를 포함한 셋 knn으로 보존
knn <- train


# RESI_COST, '0:추정불가' 이므로 0을 NA로 바꾸고 imputation
nrow(knn[knn$RESI_COST==0 & !is.na(knn$RESI_COST==0),])    # 0인 개수 1317개
knn[knn$RESI_COST==0, "RESI_COST"] <- NA
sum(is.na(knn$RESI_COST))    # NA인 개수 1317개


# RESI_COST imputation
system.time(
knn <- VIM::kNN(knn, variable = "RESI_COST",
                dist_var = c("SEX", "AGE","FP_CAREER", "OCCP_GRP_2", "WEDD_YN", "CHLD_CNT", "LTBN_CHLD_AGE",
                             "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
           k=144, imp_var=F) # sqrt(knn데이터수)=143.55
)   # 50초.


# OCCP_GRP_2 "" -> NA처리
knn %>% 
  filter(OCCP_GRP_2=="") %>% 
  nrow()  # 547개

knn <- 
  knn %>% 
    mutate(OCCP_GRP_2=replace(OCCP_GRP_2, OCCP_GRP_2=="", NA))

sum(is.na(knn$OCCP_GRP_2))

# OCCP_GRP_2 imputation
system.time(
  knn <- VIM::kNN(knn, variable = "OCCP_GRP_2",
                  dist_var = c("SEX", "AGE","RESI_COST","FP_CAREER", 
                               "WEDD_YN", "CHLD_CNT", "LTBN_CHLD_AGE",
                               "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
                  k=144, imp_var=F)
) # 18초

# WEDD_YN "" -> NA처리
knn %>% 
  filter(WEDD_YN=="") %>% 
  nrow()  # 473개

knn <- 
  knn %>% 
    mutate(WEDD_YN=replace(WEDD_YN, WEDD_YN=="", NA))

sum(is.na(knn$WEDD_YN))

# WEDD_YN imputation
system.time(
  knn <- VIM::kNN(knn, variable = "WEDD_YN",
                  dist_var = c("SEX", "AGE","RESI_COST","FP_CAREER", 
                               "OCCP_GRP_2", "CHLD_CNT", "LTBN_CHLD_AGE",
                               "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
                  k=144, imp_var=F)
) # 15초



# CHLD_CNT NA 개수
sum(is.na(knn$CHLD_CNT))  # 434ㄱ


# CHLD_CNT imputation
system.time(
  knn <- VIM::kNN(knn, variable = "CHLD_CNT",
                  dist_var = c("SEX", "AGE", "RESI_COST", "FP_CAREER",
                               "OCCP_GRP_2","WEDD_YN", "LTBN_CHLD_AGE",
                               "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
                  k=144, imp_var=F)
) #14초


# LTBN_CHLD_AGE NA 개수
sum(is.na(knn$LTBN_CHLD_AGE))   # 434개


# LTBN_CHLD_AGE imputation
system.time(
  knn <- VIM::kNN(knn, variable = "LTBN_CHLD_AGE",
                  dist_var = c("SEX", "AGE","RESI_COST","FP_CAREER", 
                               "OCCP_GRP_2", "WEDD_YN", "CHLD_CNT",
                               "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
                  k=144, imp_var=F)
) # 14초

train <- knn

# -----



# imputation을 위해 추가한 temp variables 제거 -----
train <- 
  train %>% 
    select(-RESI_TYPE_CODE, -CUST_INCM, -RCBASE_HSHD_INCM, -JPBASE_HSHD_INCM)

# test set은 imputation 이후에 제거
temptest <- test

# -----


# save -----
save(train, temptest, knn, file="./fds/final_data.Rdata")
rm(list=ls())
# -----

