# initial setting
source("./fds/library.R")
source("./fds/configuration.R", encoding='UTF-8')


# data loading
rm(list=ls())
load("./fds/fds_raw.Rdata")


# --------------------------------------------------------------------------------------------------------
# start EDA
# --------------------------------------------------------------------------------------------------------

# entire exploration for cust set -----------------------------------------
describe(cust)
# CTPR : 도레벨로 범위가 너무 큼/제외
# OCCP_GRP_1, MATE_OCCP_GRP_1 : 직업코드2와 겹치고, 2가 더 자세하여 제외
# CUST_RGST : 계약 가입년월이 활용 가능성이 더 높고 연관성 없을 것으로 판단하여 제외.
# MAX_PAYM_YM, MAX_PRM : 보험료 관련 정보는 현시점 데이터 활용이 나아 제
# MINCRDT,MAXCRDT도 현시점 데이터가 아니고, 결측치가 9476(42%)로 많아 제외 -----


# class imbalance check --------------------------------------------------
cust %>% 
  filter(cust$DIVIDED_SET==1) %>% 
  group_by(SIU_CUST_YN) %>% 
  summarise(n=n())            # training set N:Y = 18801:1806 = 10:1
table(cust$SIU_CUST_YN)       # test set :1793건
# 10:1 비율. sampling에 고려 -----


# CUST_INCM : 연령, 직업, 보험료 수준 등 기반 고객의 개인소득 추정 -----------------------------
sum(is.na(cust$CUST_INCM))         # 결측   : 5263건
nrow(filter(cust, CUST_INCM==0))   # 0      : 6181건
(5263+6181)/22400                  # 결측+0 : 약 51% 
# 제외, RESI_COST imputation에만 활용 -----


# RCBASE_HSHD_INCM : 주택가격 기반 가구소득 추정금액 ------------------------------------
sum(is.na(cust$RCBASE_HSHD_INCM))         # 결측   : 0건
nrow(filter(cust, RCBASE_HSHD_INCM==0))   # 0      : 3816건
3816/22400                                # 결측+0 : 약 17%
# 제외, RESI_COST imputation에만 활용 -----


# RESI_TYPE_CODE : 거주타입 결측치 체크 --------------------------------------------
sum(is.na(cust$RESI_TYPE_CODE))       # 결측 : 1254건.
1254/22400                            # 결측 : 약 5.6%
table(cust$RESI_TYPE_CODE)            # 코드성으로 10여개의 분류로 정밀하지 않음.
# 결측은 적으나, 본 목적인 소득 추정 데이터로 활용하기에는 RESI_COST를 사용하는
# 것이 더 정확할 것으로 판단하여, RESI_COST imputation에만 활용
# 제외, RESI_COST imputation에만 활용 -----


# RESI_COST : 거주지 주택가격 추정 -------------------------------------------------
sum(is.na(cust$RESI_COST))                # 결측    : 0건
nrow(filter(cust, RESI_COST==0))          # 0       : 1445건(추정불가로 정의되어 사실상 결측)
1445/22400                                # 결측+0  : 6.5%
length(table(cust$RESI_COST))             # 값의 종류 6663개로 거주타입보다 더 정밀함.
# 사용 -----
# CUST_INCM, RCBASE_HSHD_INCM, JPBASE_HSHD_INCM, RESI_TYPE_CODE -----
# 추가하여 knn imputation에만 사용 -----


# JPBASE_HSHD_INCM : 직업 및 납입보험료 기반 추정소득 -----------------------------------
sum(is.na(cust$JPBASE_HSHD_INCM))         # 결측   : 680건
nrow(filter(cust, JPBASE_HSHD_INCM==0))   # 0      : 1376건
(680+1376)/22400                          # 결측+0 : 약 9%
# 사용 고려 대상이나, RESI_COST 사용으로 제외. RESI_COST imputation에만 활용 -----


# TOTALPREM, MATE_OCCP_GRP_2
prop.table(table(cust$MATE_OCCP_GRP_2))   # ""값이 약 53%
sum(is.na(cust$TOTALPREM))/nrow(cust)     # NA값이 약 26%
# missing이 지나치게 많아 제외 -----


# --------------------------------------------------------------------------------------------------------
# 파생변수 이해, 사용 가능성, 실효성 체크. 파생변수 생성 전 후 검증
# --------------------------------------------------------------------------------------------------------

# dv1 : 총월납보험료 DV1_TOT_MON_PREM 생성 ----------------------------------------

# CNTT$SUM_ORIG_PREM의 결측 82건.
# mart생성 이전, 즉 변수 생성시 cntt테이블에서 imputation하는게
# 논리적으로 더 적절함.
# knn-imputation
cntt0 <- cntt
str(cntt0)

# kNN 메서드를 위한 형변
cntt0$SUM_ORIG_PREM <- as.numeric(cntt0$SUM_ORIG_PREM)
cntt0$IRKD_CODE_DTAL <- as.factor(cntt0$IRKD_CODE_DTAL)
cntt0$IRKD_CODE_ITEM <- as.factor(cntt0$IRKD_CODE_ITEM)
cntt0$GOOD_CLSF_CDNM <- as.factor(cntt0$GOOD_CLSF_CDNM)
cntt0$CNTT_YM <- as.numeric(cntt0$CNTT_YM)
cntt0$REAL_PAYM_TERM <- as.numeric(cntt0$REAL_PAYM_TERM)
cntt0$SALE_CHNL_CODE <- as.factor(cntt0$SALE_CHNL_CODE)

# SUM_ORIG_PREM과 동반 NA인 변수 8개를 제외한 변수 중,
# imputation에 적절한 변수들 사용하여 계산.
system.time(
cntt0 <- kNN(cntt0, variable = "SUM_ORIG_PREM", 
             dist_var=c("IRKD_CODE_DTAL", "IRKD_CODE_ITEM", "GOOD_CLSF_CDNM",
                        "CNTT_YM", "REAL_PAYM_TERM", "SALE_CHNL_CODE"), 
             k=50, imp_var=F)
)
sum(is.na(cntt0$SUM_ORIG_PREM))


DV1_TOT_MON_PREM <- 
  cntt0 %>% 
  group_by(CUST_ID) %>% 
  summarise(DV1_TOT_MON_PREM=sum(SUM_ORIG_PREM))
DV1_TOT_MON_PREM

rm(cntt0)
# 생성완료-----


# dv1 관련 탐색 -------------------------------------------------------
nrow(cust)                      # 고객 수 총 22400명
nrow(distinct(cntt, CUST_ID))   # 계약 테이블에서 고객 수가 22400임을 확인

sum(is.na(cntt$SUM_ORIG_PREM))  # 결측치 82건  

cntt0 <- cntt
describe(cntt0)   # 결측이 82 또는 84인 변수가 상당수 존재, imp 전에 확인 필요
cntt0 %>% 
  filter(is.na(SUM_ORIG_PREM)) %>% 
  select(MNTH_INCM_AMT, CNTT_RECP, RECP_PUBL, MAIN_INSR_AMT, PAYM_CYCL_CODE,
         EXPR_YM, CNTT_STAT_CODE, CLLT_FP_PRNO)
# 다함께 NA임. 이 8개의 변수들은 knn의 distance 계산 변수에서 제외.


# imputation하여 파생변수 생성
sum(is.na(DV1_TOT_MON_PREM))    # 결측치 0건

rm(cntt0)
# 이상없음 -----



# dv2 : 3년내 청구건수 DV2_NCLAIM_AFT1308 ------------------------------------------
claim$RECP_DATE <- ymd(claim$RECP_DATE)
period <- interval(ymd("20130801"), ymd("20160731"))

DV2_NCLAIM_AFT1308 <- 
  claim %>% 
  select(CUST_ID, POLY_NO, RECP_DATE) %>% 
  filter(RECP_DATE %within% period) %>% 
  group_by(CUST_ID) %>% 
  summarise(DV2_NCLAIM_AFT1308=n())

rm(period)
# 생성 완료 -----


# dv2 관련 탐색 ----------------------------------------------------------

# claim 테이블에서 날짜 변수들의 모든 마지막 날짜 체크하기
str(claim)
claim[,"ORIG_RESN_DATE"]  <- ymd(claim[,"ORIG_RESN_DATE"])
claim[,"RESN_DATE"]       <- ymd(claim[,"RESN_DATE"])
claim[,"HOSP_OTPA_STDT"]  <- ymd(claim[,"HOSP_OTPA_STDT"])
claim[,"HOSP_OTPA_ENDT"]  <- ymd(claim[,"HOSP_OTPA_ENDT"])
claim[,"PAYM_DATE"]       <- ymd(claim[,"PAYM_DATE"])
dateCols <- c("RECP_DATE", "ORIG_RESN_DATE", "RESN_DATE", 
              "HOSP_OTPA_STDT", "HOSP_OTPA_ENDT", "PAYM_DATE")
sapply(claim[dateCols], max, na.rm=T, simplify=F)    # simplify=F 안하면 벡터로 변환하면서 integer로 바뀜.

# 2년 내라는 조건이 붙었으므로, 8683명에 대한 건수만 나옴.
nrow(DV2_NCLAIM_AFT1308)

# 결측치 체크 = 0
sum(is.na(DV2_NCLAIM_AFT1308))  

rm(dateCols)
# 날짜를 보면 이 데이터의 기간이 대략 2016년 7월 정도까지의 데이터임을 알 수 있다.
# 특히 dv2 의미에 맞는 RECP_DATE 변수를 살펴봐도 가장 마지막 날짜가 2016-07-01이므로
# 최근 2년이라는 기간을 20140801~20160731로 잡는다.
# 탐색 결과 이상 없음 -----
# 탐색 결과로, 20130801~20160731로 기간 설정 -----



# dv3 : 3년 내 수령금액 합산 DV3_TOT_PAYM_AFT1308 -------------------------------------
claim$PAYM_DATE <- ymd(claim$PAYM_DATE)
period2         <- interval(ymd("20130801"), ymd("20160731"))
DV3_TOT_PAYM_AFT1308 <- 
  claim %>% 
  select(CUST_ID, CRNT_PROG_DVSN, PAYM_AMT, PAYM_DATE) %>% 
  filter(PAYM_DATE %within% period2, !is.na(PAYM_DATE)) %>% 
  group_by(CUST_ID) %>% 
  summarise(DV3_TOT_PAYM_AFT1308=sum(PAYM_AMT))
DV3_TOT_PAYM_AFT1308

rm(period2)
# 생성완료 -----


# dv3 관련 탐색 ---------------------------------------------------------------

# 진행상태 코드에 따른 수령금액 체크
# 접수(11), 심사배정(21), 심사(22), 심사결재(23), 조사(32), 조사결재(33)
# 이 중, 심사는 청구서류 및 현재까지 고객 데이터를 기반으로
# 보험금을 즉시 지급할지 추가적인 조사가 필요할지 결정하는 과정.

# 확인해보면, 접수(11), 심사배정(21), 심사(22), 심사결재(23), 조사(32), 조사결재(33)
# 중에서, 접수 단계부터 PAYM_AMT(실지급금액)값이 붙어 있는 등, 
# 데이터에 대한 확인 필요

testClaim <- 
  claim %>% 
    select(CUST_ID, POLY_NO, CNTT_RECP_SQNO, RECP_DATE, ORIG_RESN_DATE, RESN_DATE,
           CRNT_PROG_DVSN, PAYM_DATE, DMND_AMT, PAYM_AMT)


# 지급금액 결측치 확인 : 없음
sum(is.na(testClaim$PAYM_AMT))


# 지급일자 na개수 : 13개.
sum(is.na(testClaim$PAYM_DATE))
# 지급일자가 na인 것의 진행단계별 분류
testClaim[is.na(testClaim$PAYM_DATE),] %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(n=n())
# 13건은 이상치로 제거해야 할 듯.


# 진행코드별 건수 확인
(testClaim %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(n=n()))
# 11:22290 / 21:1 / 22:3 / 23:87023 / 32:2 / 33:9701


# 지급금액이 0인 건 진행코드별 확인
(testClaim %>% 
  filter(PAYM_AMT==0) %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(n_PAYM_AMT=n()))
# 11:627 / 21:1 / 22:3 / 23:2653 / 32:2 / 33:283
# 21, 22, 32는 모두 지급금액이 0으로 나타남.


# 심사배정, 심사, 조사 단계의 청구금액 확인
(testClaim %>% 
  filter(CRNT_PROG_DVSN %in% c(21, 22, 32)))
# 6건의 위 3개 단계에서 청구금액이 0인 건은 없음


# 진행코드별 청구금액!=0, 지급금액==0인 건
(testClaim %>% 
  filter(PAYM_AMT==0, DMND_AMT!=0) %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(DMND_NOT0_PAYM_0=n()))


# 진행코드별 청구==0, 지급금액!=0
(testClaim %>% 
  filter(PAYM_AMT!=0, DMND_AMT==0) %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(DMND_0_PAYM_NOT0=n()))
# 조사결재만 6건
# 한 진행코드에만 집중되어 있지 매우 적은 건수여서 이상치인지 아닌지 판단하기 어려움


# 청구금액이 0인 건 진행코드별 확인
(testClaim %>% 
  filter(DMND_AMT==0) %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(n_DMND_AMT0=n()))


# 진행코드별 청구==0, 지급금액==0
(testClaim %>% 
  filter(PAYM_AMT==0, DMND_AMT==0) %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(DMND_PAYM_SAME0=n()))
# 0으로 청구하고 0으로 지급하는 건이 상당수 있음
# 이상치라고 보기 어려운, 비즈니스상의 어떤 규칙에 의한 것일 듯함.
# 청구=0이나 지급이 0이상인 조사결재의 6건을 제외하면, 청구 0인 경우 지급 0으로 결론.


# 청구>지급
(testClaim %>% 
  filter(DMND_AMT > PAYM_AMT)) %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(n_UNDER_PAYM=n())
# 모든 단계에 걸쳐, 상당수가 분포하고 있음.


# 청구==지급
(testClaim %>% 
  filter(PAYM_AMT==DMND_AMT) %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(n=n()))
# 11:22253, 23:82268, 33:5763


# 청구<지급
(testClaim %>% 
  filter(PAYM_AMT>DMND_AMT) %>% 
  group_by(CRNT_PROG_DVSN) %>% 
  summarise(n=n()))
# 23:2, 33:9

rm(testClaim)
# 결재단계에만 분포하는 것으로 봐서, 극히 소수이긴 하지만 정상적인 것으로 예상.
# 접수(11), 심사결재(23), 조사결재(33) 세 단계에서는 지급이 이루어진 것으로 봐도 될 듯.-----
# 청구금액이 0인 건은 무엇인지 몰라도, 보험사에서 서류접수받아 입력하는 청구금액이므로 특정 경우가 있는듯 -----
# 지급일자가 결측치인 13건 제외 -----
# 이상없음 -----



# discard1 동일사유 청구누적횟수 최대값 N_SAME_MAX_CAUS ---------------------------------
# N_SAME_MAX_CAUS <-
#   claim %>%
#   select(CUST_ID, DMND_RSCD_SQNO) %>%
#   group_by(CUST_ID) %>%
#   summarise(N_SAME_MAX_CAUS=max(DMND_RSCD_SQNO))
# N_SAME_MAX_CAUS
# 사용불가 -----


# discard1 관련 탐색 --------------------------------------------------------------

tempClaim <- claim %>%
  arrange(POLY_NO, DMND_RESN_CODE) %>%
  select(CUST_ID, CNTT_RECP_SQNO, DMND_RSCD_SQNO, POLY_NO, DMND_RESN_CODE, RESN_DATE, RECP_DATE, ORIG_RESN_DATE)
head(tempClaim, 20)

# DMND_RSCD_SQNO  변수설명에 맞는 값이 나오지 않음.
# (동일증번, 동일청구사지만 사유날짜 다른 경우 변수값 1씩 증가. 제대로 증가 안하고 있음)
# 사용불가 -----



# dv4 최초청구까지의 기간 DV4_PERI_FRS_CLAIM ----------------------------------------

# 원사유일자, 사유일자, 사고접수일자를 사용할 수 있음.
# 일단 원사유일자(ORIG_RESN_DATE)로 사용.
tempClaim <- 
  claim %>% 
    select(POLY_NO, CUST_ID, RECP_DATE, ORIG_RESN_DATE, RESN_DATE) %>% 
    group_by(POLY_NO) %>% 
    summarise(FRS_CLAIM_DATE = min(ORIG_RESN_DATE))
tempClaim
tempClaim$FRS_CLAIM_DATE <- ymd(tempClaim$FRS_CLAIM_DATE)

str(cntt)
cntt$CNTT_YM <- ymd(parse_date_time(cntt$CNTT_YM, orders="ym"))
tempCntt <- 
  cntt %>% 
    select(POLY_NO, CUST_ID, CNTT_YM) 
str(tempCntt)
tempMERGE <- merge(tempClaim, tempCntt, by='POLY_NO', all=T)
str(tempMERGE)
tbl_df(tempMERGE)

tempMERGE <- 
  tempMERGE %>% 
    transmute(POLY_NO = POLY_NO, 
              CUST_ID = CUST_ID,
              FRS_PERI = (FRS_CLAIM_DATE - CNTT_YM))
tempMERGE$FRS_PERI <- as.integer(tempMERGE$FRS_PERI)
tbl_df(tempMERGE)
nrow(distinct(tempMERGE, CUST_ID))

DV4_PERI_FRS_CLAIM <- 
  tempMERGE %>% 
    group_by(CUST_ID) %>% 
    summarise(DV4_PERI_FRS_CLAIM = min(FRS_PERI, na.rm=T))
tbl_df(DV4_PERI_FRS_CLAIM)

rm(tempMERGE, tempCntt, tempClaim)
# 생성완료 -----


# dv4 관련 탐색 ------------------------------------------------------------------

# 1번 고객의 원사유일자 최소값 확인 : 20111219 맞음.
claim %>% 
  select(POLY_NO, CUST_ID, RECP_DATE, ORIG_RESN_DATE, RESN_DATE) %>% 
  filter(POLY_NO==1)


# 22400명 모두 값을 가지고 있음.
sum(!is.na(INTV_FRS_CLAIM$INTV_DAYS))

# CUST_ID 수 22400과 일치함.
nrow(distinct(claim, CUST_ID))

# 이상 없음 -----



# dv5 병원별 3년내 보험청구건 중 최대치 DV5_W2Y_HSPT_MAX ------------------------
period <- interval(ymd("20130801"), ymd("20160731"))
claim$RECP_DATE <- ymd(claim$RECP_DATE)
tempHosp <- 
  claim %>% 
    filter(RECP_DATE %within% period) %>%
    group_by(HOSP_CODE) %>% 
    summarise(n=n())
tbl_df(tempHosp)

tempClaim <- 
  claim %>% 
    select(CUST_ID, HOSP_CODE)

tempMERGE <- merge(tempClaim, tempHosp, by='HOSP_CODE', all=T)
sum(is.na(tempMERGE$n)) # 15917건이 2년간 청구없던 병원에 의한 건수.
tempMERGE[is.na(tempMERGE$n),"n"] <- 1  # 2년내 첫 청구이므로 1
tbl_df(tempMERGE)

DV5_W3Y_HSPT_MAX <- 
  tempMERGE %>% 
    group_by(CUST_ID) %>% 
    summarise(DV5_W3Y_HSPT_MAX=max(n))
DV5_W3Y_HSPT_MAX

rm(period, tempHosp, tempClaim, tempMERGE)
# 생성완료 -----


# dv5 관련 탐색 -----------------------------------------------------------

# 고객 수 22400만큼 결과 나옴.
nrow(DV5_W3Y_HSPT_MAX)

# 결측치 없음.
sum(is.na(DV5_W3Y_HSPT_MAX[2]))

# 임시, 동일 데이터셋 생성
period <- interval(ymd("20130801"), ymd("20160731"))
tempHosp <- 
  claim %>% 
    filter(RECP_DATE %within% period) %>%
    group_by(HOSP_CODE) %>% 
    summarise(n=n())
tbl_df(tempHosp)

tempClaim <- 
  claim %>% 
  select(CUST_ID, HOSP_CODE)

tempMERGE <- merge(tempClaim, tempHosp, by='HOSP_CODE', all=T)

# 결측으로 나온 코드가, 2년간 없는 병원코드인지 확인.
str(tempMERGE)
codeCheck <- 
  tempMERGE %>% 
    filter(is.na(n)) %>% 
    distinct(HOSP_CODE)
sum(tempHosp$HOSP_CODE %in% codeCheck[[1]])

rm(codeCheck, period, tempMERGE, tempHosp, tempClaim)
# 0으로 맞음.

# 이상 없음 -----



# dv6 의사별 3년내 보험청구건 중 최대치 : DV6_W3Y_DCTR_MAX -----------------------
period <- interval(ymd("20130801"), ymd("20160731"))
claim$RECP_DATE <- ymd(claim$RECP_DATE)

tempDoct <- 
  claim %>% 
    filter(RECP_DATE %within% period) %>% 
    group_by(CHME_LICE_NO) %>% 
    summarise(n=n())
tbl_df(tempDoct)

tempClaim <- 
  claim %>% 
    select(CUST_ID, CHME_LICE_NO)
tbl_df(tempClaim)

tempMERGE <- merge(tempDoct, tempClaim, by='CHME_LICE_NO', all=T)
tbl_df(tempMERGE)
tempMERGE[is.na(tempMERGE$n), "n"] <- 1

DV6_W3Y_DCTR_MAX <- 
  tempMERGE %>% 
    group_by(CUST_ID) %>% 
    summarise(DV6_W3Y_DCTR_MAX=max(n))
tbl_df(DV6_W3Y_DCTR_MAX)

rm(period, tempDoct, tempClaim, tempMERGE)

# 생성완료 -----
  
  
# dv6 관련 탐색 ------------------------------------------------------------
nrow(DV6_W3Y_DCTR_MAX)

# 확인을 위한 임시 셋 생성
period <- interval(ymd("20130801"), ymd("20160731"))
tempDoct <- 
  claim %>% 
  filter(RECP_DATE %within% period) %>% 
  group_by(CHME_LICE_NO) %>% 
  summarise(n=n())
tbl_df(tempDoct)

tempClaim <- 
  claim %>% 
  select(CUST_ID, CHME_LICE_NO)
tbl_df(tempClaim)

tempMERGE <- merge(tempDoct, tempClaim, by='CHME_LICE_NO', all=T)


# 2년내 청구건 없는 의사에 대한 NA확인
codeCheck <- 
  tempMERGE %>% 
    filter(is.na(n)) %>% 
    distinct(CHME_LICE_NO)
codeCheck

sum(tempDoct[[1]] %in% codeCheck[[1]])

rm(codeCheck, tempClaim, tempDoct, tempMERGE, period)

# 이상없음 -----



# dv7 본인 및 가족 공통 청구병원 건수 총합 : DV7_FMLY_HSPT_SUM ------------------------
str(fmly)

# 고객별 병원별 청구건수 생성
hospByCust <- 
  claim %>% 
    select(CUST_ID, HOSP_CODE) %>% 
    group_by(CUST_ID, HOSP_CODE) %>% 
    summarise(n=n())
tbl_df(hospByCust)

# 가족이 등록된 고객리스트
tempList <- distinct(fmly, CUST_ID)


# 최종데이터 1열 계산하는 함수
create_dv8_Row <- function(i){
  
  # 고객별로 가족 리스트
  tempFmly <- 
    fmly %>% 
      filter(CUST_ID==tempList[i,])
  tempFmly <- c(tempList[i,], tempFmly[[2]])
  
  # 본인이 청구한 병원 코드 리스트
  hospList <- 
    (hospByCust %>% 
       filter(CUST_ID==tempFmly[1]))[[2]]
  hospList
  
  # 가족이 청구한 병원 코드 리스트
  hospList2 <- 
    (hospByCust %>% 
       filter(CUST_ID %in% tempFmly[-1]))[[2]]
  hospList2
  
  # 공통 청구한 병원 코드
  hospInter <- intersect(hospList, hospList2)
  hospInter

    
  # 가족 모두에 대해서 hospByCust에서 claim기록 추출해서
  # 공통 청구한 병원별 방문횟수 추출
  tempValue <- 
    hospByCust %>% 
      filter(CUST_ID %in% tempFmly, HOSP_CODE %in% hospInter)
  
  # hospInter가 없을 경우, 값을 0으로 고정.
  if(dim(tempValue)[1]==0){
    tempValue <- data.frame(0, 0, 0)
  }
  
  # 공통 청구한 병원 건수 합산
  tempRow <- c(tempList[i,], sum(tempValue[[3]]))
  
  return(tempRow)

}

system.time(
  DV7_FMLY_HSPT_SUM <- 
  foreach(i=1:nrow(tempList), .combine=rbind, .packages="dplyr") %dopar% {create_dv8_Row(i)}
)

colnames(DV7_FMLY_HSPT_SUM) <- c("CUST_ID", "DV7_FMLY_HSPT_SUM")
DV7_FMLY_HSPT_SUM <- as.data.frame(DV7_FMLY_HSPT_SUM)

rm(hospByCust, tempList)
# for문과 foreach병렬처리의 시간차이 약 2배
# 총 계산시간 (376 loops) : 636초 걸림. 8core로 지정해도 633초.

# 생성완료 -----


# dv7 관련 탐색 ---------------------------------------------------------------

claim %>% 
  filter(HOSP_CODE==3187, CUST_ID==4)
str(claim)

# CUST_ID가 2는 fmly에 없고 claim에는 있다. 
fmly %>% 
  filter(CUST_ID==2)
claim %>%
  select(CUST_ID, HOSP_CODE) %>% 
  filter(CUST_ID==2)


# 확인을 위한 임시 데이터 셋 생성
hospByCust <- 
  claim %>% 
  select(CUST_ID, HOSP_CODE) %>% 
  group_by(CUST_ID, HOSP_CODE) %>% 
  summarise(n=n())
tbl_df(hospByCust)

tempList <- distinct(fmly, CUST_ID)
  
# tempList의 4번째(CUST_ID==19239)의 경우 가족과 겹치는 병원 없음
# 정상작동 확인.
k=tempList[9,]
# cust별 HOSP_CODE, n 계산
tempFmly <- 
  fmly %>% 
  filter(CUST_ID==k)
tempFmly
tempFmly <- c(k, tempFmly[[2]])
tempFmly

# 본인이 청구한 병원 코드 리스트
hospList <- 
  (hospByCust %>% 
     filter(CUST_ID==tempFmly[1]))[[2]]
hospList

# 가족이 청구한 병원 코드 리스트
hospList2 <- 
  (hospByCust %>% 
     filter(CUST_ID %in% tempFmly[-1]))[[2]]
hospList2

hospInter <- intersect(hospList, hospList2)
hospInter

# 가족 중 나와 중복 청구한 병원들 횟수
tempValue <- 
  hospByCust %>% 
    filter(CUST_ID %in% tempFmly, HOSP_CODE %in% hospInter)
tempValue

# hospInter가 없을 경우, 값을 0으로 고정.
if(dim(tempValue)[1]==0){
  tempValue <- data.frame(0, 0, 0)
}
tempValue

tempRow <- c(k, sum(tempValue[[3]]))
tempRow  

rm(tempRow, tempValue, hospList, hospList2, hospInter, 
   hospByCust, tempList, tempFmly, k)
# 이상없음 -----



# dv8 본인 및 가족 공통 청구의사 합산 건수 : DV8_FMLY_DCTR_SUM ------------------

# 고객별 의사별 청구건수 생성
dctrByCust <- 
  claim %>% 
  select(CUST_ID, CHME_LICE_NO) %>% 
  group_by(CUST_ID, CHME_LICE_NO) %>% 
  summarise(n=n())
tbl_df(dctrByCust)

# 가족이 등록된 고객리스트
tempList <- distinct(fmly, CUST_ID)
tempList


# 최종데이터 1열 계산하는 함수
create_dv9_Row <- function(i){
  
  # 고객별로 가족 리스트
  tempFmly <- 
    fmly %>% 
    filter(CUST_ID==tempList[i,])
  tempFmly <- c(tempList[i,], tempFmly[[2]])
  
  # 본인이 청구한 의사 코드 리스트
  dctrList <- 
    (dctrByCust %>% 
       filter(CUST_ID==tempFmly[1]))[[2]]
  dctrList
  
  # 가족이 청구한 의사 코드 리스트
  dctrList2 <- 
    (dctrByCust %>% 
       filter(CUST_ID %in% tempFmly[-1]))[[2]]
  dctrList2
  
  # 공통 청구한 의사 코드
  dctrInter <- intersect(dctrList, dctrList2)
  dctrInter
  
  
  # 가족 모두에 대해서 dctrByCust에서 claim기록 추출해서
  # 의사별 청구건수 추출
  tempValue <- 
    dctrByCust %>% 
    filter(CUST_ID %in% tempFmly, CHME_LICE_NO %in% dctrInter)
  
  # dctrInter가 없을 경우, 값을 0으로 고정.
  if(dim(tempValue)[1]==0){
    tempValue <- data.frame(0, 0, 0)
  }
  
  # 공통 의사 청구건수 합산.
  tempRow <- c(tempList[i,], sum(tempValue[[3]]))
  
  return(tempRow)
  
}

system.time(
  DV8_FMLY_DCTR_SUM <- 
    foreach(i=1:nrow(tempList), .combine=rbind, .packages="dplyr") %dopar% {create_dv9_Row(i)}
)

colnames(DV8_FMLY_DCTR_SUM) <- c("CUST_ID", "DV8_FMLY_DCTR_SUM")

DV8_FMLY_DCTR_SUM <- as.data.frame(DV8_FMLY_DCTR_SUM)

rm(dctrByCust, tempList)
# 640초 소요.
# 생성완료 -----


# dv8 관련 확인 ---------------------------------------------------------------

# 확인을 위한 임시 데이터셋 생성
# 고객별 의사별 청구건수 생성
dctrByCust <- 
  claim %>% 
  select(CUST_ID, CHME_LICE_NO) %>% 
  group_by(CUST_ID, CHME_LICE_NO) %>% 
  summarise(n=n())
tbl_df(dctrByCust)

# 가족이 등록된 고객리스트
tempList <- distinct(fmly, CUST_ID)
tempList


# 고객 선ㅌ
j = tempList[3, ]
j

# 고객별로 가족 리스트
tempFmly <- 
  fmly %>% 
  filter(CUST_ID==j)
tempFmly <- c(j, tempFmly[[2]])
tempFmly

# 본인이 청구한 의사 코드 리스트
dctrList <- 
  (dctrByCust %>% 
     filter(CUST_ID==tempFmly[1]))[[2]]
dctrList

# 가족이 청구한 의사 코드 리스트
dctrList2 <- 
  (dctrByCust %>% 
     filter(CUST_ID %in% tempFmly[-1]))[[2]]
dctrList2

# 공통청구한 의사 코드
dctrInter <- intersect(dctrList, dctrList2)
dctrInter


# 가족 모두에 대해서 dctrByCust에서 claim기록 추출해서
# 공통 청구한 의사 방문횟수 추출
tempValue <- 
  dctrByCust %>% 
  filter(CUST_ID %in% tempFmly, CHME_LICE_NO %in% dctrInter)
tempValue

# dctrInter가 없을 경우, 값을 0으로 고정.
if(dim(tempValue)[1]==0){
  tempValue <- data.frame(0, 0, 0)
}

# n중 최대 값 사용.
tempRow <- c(j, sum(tempValue[[3]]))
tempRow

rm(dctrByCust, tempList, tempFmly, dctrList, dctrList2, 
   dctrInter, tempValue, tempRow, j)
# 이상 없음-----



# dv9 금감원 유의 대상 병원 청구 횟수 : DV9_HEED_HOSP_N -----------------------
DV9_HEED_HOSP_N <- 
  claim %>% 
    select(CUST_ID, HEED_HOSP_YN) %>% 
    filter(HEED_HOSP_YN=="Y") %>% 
    group_by(CUST_ID) %>% 
    summarise(DV9_HEED_HOSP_N=n())
tbl_df(DV9_HEED_HOSP_N)

# 생성완료 -----



# dv9 관련 확인 --------------------------------------------------------------

# CUST_ID==39 인 고객의 3건 확인
claim %>% 
  filter(CUST_ID==39) %>% 
  select(CUST_ID, HOSP_CODE, HEED_HOSP_YN)

# 확인 완료 -----



# 파생변수 저장
save(DV1_TOT_MON_PREM, DV2_NCLAIM_AFT1308, DV3_TOT_PAYM_AFT1308,
     DV4_PERI_FRS_CLAIM, DV5_W3Y_HSPT_MAX, DV6_W3Y_DCTR_MAX,
     DV7_FMLY_HSPT_SUM, DV8_FMLY_DCTR_SUM, DV9_HEED_HOSP_N,
     file="./fds/derived_variables.Rdata")

rm(list=ls())
