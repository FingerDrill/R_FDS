
# initial setting
source("./fds/library.R")
source("./fds/configuration.R", encoding='UTF-8')


# data loading
rm(list=ls())
load("./fds/final_data.Rdata")
load("./fds/final_test.Rdata")
load("./fds/models.Rdata")
load("./fds/evaluations.Rdata")


# 정답지
answer <- read.xlsx("./fds/data/answer.xlsx", 
                    sheetName="answer",
                    as.data.frame = T,
                    header = T)
answer <- as.factor(answer[[3]])



# test 전처리 -----
describe(test)

test <- temptest

system.time(
test <- 
  foreach(i=1:nrow(test), .combine=rbind, .packages=c("VIM", "dplyr")) %dopar% {
  
  tempRow <- test[i,]
  
  # RESI_COST, '0:추정불가' 이므로 0을 NA로 바꾸고 imputation
  # OCCP_GRP_2 "" -> NA처리
  # WEDD_YN "" -> NA처리
  tempRow <- 
    tempRow %>% 
      mutate(RESI_COST=replace(RESI_COST, RESI_COST==0, NA),
             OCCP_GRP_2=replace(OCCP_GRP_2, OCCP_GRP_2=="", NA),
             WEDD_YN=replace(WEDD_YN, WEDD_YN=="", NA))
  
  # test set의 row를 한개 붙인 뒤 imputation. 
  # test set의 데이터가 imputation에 영향 안주기 위해서.
  knn <- rbind(knn, tempRow)
  
  # RESI_COST imputation
  system.time(
    knn <- VIM::kNN(knn, variable = "RESI_COST",
                    dist_var = c("SEX", "AGE", "FP_CAREER", 
                                 "OCCP_GRP_2", "WEDD_YN", "CHLD_CNT", "LTBN_CHLD_AGE",
                                 "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
                    k=144, imp_var=F)
  )
  
  
  
  # OCCP_GRP_2 imputation
  system.time(
    knn <- VIM::kNN(knn, variable = "OCCP_GRP_2",
                    dist_var = c("SEX", "AGE","RESI_COST",
                                 "FP_CAREER", "WEDD_YN", "CHLD_CNT", "LTBN_CHLD_AGE",
                                 "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
                    k=144, imp_var=F)
  )
  
  # WEDD_YN imputation
  system.time(
    knn <- VIM::kNN(knn, variable = "WEDD_YN",
                    dist_var = c("SEX", "AGE","RESI_COST",
                                 "FP_CAREER", "OCCP_GRP_2", "CHLD_CNT", "LTBN_CHLD_AGE",
                                 "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
                    k=144, imp_var=F)
  )
  
  
  # CHLD_CNT imputation
  system.time(
    knn <- VIM::kNN(knn, variable = "CHLD_CNT",
                    dist_var = c("SEX", "AGE","RESI_COST",
                                 "FP_CAREER", "OCCP_GRP_2", "WEDD_YN", "LTBN_CHLD_AGE",
                                 "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
                    k=144, imp_var=F)
  )
  
  # LTBN_CHLD_AGE imputation
  system.time(
    knn <- VIM::kNN(knn, variable = "LTBN_CHLD_AGE",
                    dist_var = c("SEX", "AGE","RESI_COST",
                                 "FP_CAREER", "OCCP_GRP_2", "WEDD_YN", "CHLD_CNT",
                                 "RESI_TYPE_CODE", "CUST_INCM", "RCBASE_HSHD_INCM", "JPBASE_HSHD_INCM"),
                    k=144, imp_var=F)
  )
  
  imputed_test_row <- knn[20608, ]
  knn <- knn[-20608, ]
    
  return(imputed_test_row)
  
}
) # 1119초

# imputation 여부 확인
test %>% 
  filter(is.na(RESI_COST) | is.na(LTBN_CHLD_AGE) | is.na(CHLD_CNT) | OCCP_GRP_2=="" | WEDD_YN=="") %>% 
  nrow()
nrow(test)
describe(test)


# 사용하지 않는 변수 제거
test <- 
  test %>% 
    select(-RESI_TYPE_CODE, -CUST_INCM, -RCBASE_HSHD_INCM, -JPBASE_HSHD_INCM)

# CUST_ID(identifier)와 train/test 셋 구분을 위한 DIVIDED_SET 제외
test <- 
  test %>% 
    select(-CUST_ID, -DIVIDED_SET)

# imputation 이전에 형 변환으로 인해 생겼던 empty factor 제거
levels(test$SIU_CUST_YN)[1] <- NA
levels(test$OCCP_GRP_2)[1]  <- NA
levels(test$WEDD_YN)[1]     <- NA

save(test, file="./fds/final_test.Rdata")
# -----


# prediction : 1. rf only -----
predicted_m1 <- predict(m1, test)

cMat_m1 <- confusionMatrix(predicted_m1, answer, positive="Y")

(f1_m1 <- cMat_m1$byClass[7]) # 0.2580645161

# -----


# prediction : 2. rf with up-sampling -----
predicted_m2 <- predict(m2, test)

cMat_m2 <- confusionMatrix(predicted_m2, answer, positive="Y")

(f1_m2 <- cMat_m2$byClass[7]) # 0.3619047619

# -----


# prediction : 3. rf with down-sampling -----
predicted_m3 <- predict(m3, test)

cMat_m3 <- confusionMatrix(predicted_m3, answer, positive="Y")

(f1_m3 <- cMat_m3$byClass[7]) # 0.3439306358

# -----


# prediction : 4. rf with SMOTE -----
predicted_m4 <- predict(m4, test)

cMat_m4 <- confusionMatrix(predicted_m4, answer, positive="Y")

(f1_m4 <- cMat_m4$byClass[7]) # 0.4697508897

# -----


# prediction : 6. rf with 10/3 CV, parameter searching -----
evaluations %>% 
  group_by(ntree, mtry, classwt, sampsize_L, sampsize_R) %>% 
  summarise(mean_f1=mean(f1)) %>% 
  arrange(desc(mean_f1))
# 평균적으로 ntree=500, mtry=4, classwt=100,
# sampsize_L=1500, sampsize_R=1500 인 경우 mean_f1값 0.3444784850
# -----


# prediction : 7. rf with optimal parameter, no cv, smote -----
predicted_m5 <- predict(m5, test)

cMat_m5 <- confusionMatrix(predicted_m5, answer, positive="Y")

(f1_m5 <- cMat_m5$byClass[7]) # 0.4183673469

# -----

