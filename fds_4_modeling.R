
# initial setting
source("./fds/library.R")
source("./fds/configuration.R", encoding='UTF-8')


# data loading
rm(list=ls())
load("./fds/final_data.Rdata")


# classification을 다루는데, class-imbalance problem이 있으므로, 
# resampling이나 weighting을 사용해야 한다.
# 1. rf only
# 2. rf with up-sampling 
# 3. rf with down-sampling
# 4. rf with SMOTE
# 5. rf with 10/3 CV
# 6. rf with 10/3 CV, parameter searching
# 7. rf with optimal parameter, no cv, smote



# 0. setting --------------------------------------------------------------
describe(train)
str(train)

# CUST_ID(identifier)와 train/test 셋 구분을 위한 DIVIDED_SET 제외
train <- 
  train %>% 
  select(-CUST_ID, -DIVIDED_SET)

# imputation 이전에 형 변환으로 인해 생겼던 empty factor 제거
train$SIU_CUST_YN <- droplevels(train$SIU_CUST_YN)
train$OCCP_GRP_2  <- droplevels(train$OCCP_GRP_2)
train$WEDD_YN     <- droplevels(train$WEDD_YN)

# -----


# 1. random forest only ---------------------------------------------------

# randomforest는 missing value를 다룰 수 없다.
set.seed(421)
system.time(
m1 <- randomForest(SIU_CUST_YN ~. , 
                   data=train, 
                   importance=T)
) # 103초

# -----



# 2. upsampling -----
set.seed(421)
up_train <- upSample(x=train[-1], y=train[[1]])
str(up_train)

system.time(
  m2 <- randomForest(Class ~. , 
                     data=up_train, 
                     importance=T)
) # 240초

# -----



# 3. downsampling -----
set.seed(421)
down_train <- downSample(x=train[-1], y=train[[1]])
str(down_train)

system.time(
  m3 <- randomForest(Class ~. , 
                     data=down_train, 
                     importance=T)
) # 16초

# -----


# 4. SMOTE -----
set.seed(421)
system.time(
  smote_train <- SMOTE(SIU_CUST_YN ~. , train,
                       perc.over=200, perc.under=300, k=144)
)                   

table(smote_train$SIU_CUST_YN)

system.time(
  m4 <- randomForest(SIU_CUST_YN ~. , 
                     data=smote_train, 
                     importance=T)
) # 103초

# -----

save(m1, m2, m3, m4, file="./fds/models.Rdata")


# smote_train일 때 가장 성능이 좋게 나옴.
# 데이터셋은 smote_train으로 확정 


# 5. 10/3 CV : 6을 위한 코드 테스트 -----

K <- 10
R <- 3
# oversampling을 할 때의 올바른 cv는 
# train셋에서 validation을 먼저 분리한 후 oversampling을 진행한다.
# 순서가 바뀌면, validation set에 training set의 정보가 유입된다.
set.seed(421)
cv <- cvFolds(nrow(train), K=K, R=R, type="random")

system.time(
evaluations <- 
  foreach(r=1:R, .combine=rbind) %:%
    foreach(k=1:K, .combine=rbind, .packages=c("DMwR", "randomForest","caret")) %dopar% {
      validation_idx <- cv$subsets[which(cv$which==k), r]
      cv_train <- train[-validation_idx, ]
      smote_train <- SMOTE(SIU_CUST_YN ~. , cv_train,
                           perc.over=200, perc.under=300, k=7)
      validation <- train[validation_idx, ]
      
      #modeling
      model <- randomForest(SIU_CUST_YN ~. , 
                            data=smote_train, 
                            importance=T)
      
      # 평가
      cMat  <- confusionMatrix( predict(model, validation), 
                                validation$SIU_CUST_YN, positive="Y")
      f1    <- cMat$byClass[7]
      
      rm(cv_train, smote_train, validation)
      gc(reset=T)
      return( c(r, k, f1) )
    }
)

# 1268초 = 20분
# -----



# 6. rf with 10/3 cv, parameter searching -------------------------------------------

# 108 models.
grid <- expand.grid(ntree=c(200,500,1000),
                    mtry=c(4,7,10),
                    classwt=c(20,50,100),
                    sampsize_L=c(800,1500),
                    sampsize_R=c(800,1500)
)

# make cv indices
K <- 5
R <- 2
set.seed(421)
cv <- cvFolds(nrow(train), K=K, R=R, type="random")

evaluations <- 
  foreach(g=1:nrow(grid), .combine=rbind) %:%
    foreach(r=1:R, .combine=rbind) %:%
      foreach(k=1:K, .combine=rbind, .packages=c("DMwR", "randomForest","caret")) %dopar% {
        validation_idx <- cv$subsets[which(cv$which==k), r]
        cv_train <- train[-validation_idx, ]
        smote_train <- SMOTE(SIU_CUST_YN ~. , cv_train,
                             perc.over=200, perc.under=300, k=7)
        validation <- train[validation_idx, ]
        
        # modeling
        model <- randomForest(SIU_CUST_YN ~. , 
                              data=smote_train, 
                              importance=T,
                              ntree=grid[g, "ntree"],
                              mtry=grid[g, "mtry"],
                              classwt=c(1, grid[g, "classwt"]),
                              sampsize=c(grid[g, "sampsize_L"], grid[g, "sampsize_R"])
                              )
        
        # 평가
        cMat  <- confusionMatrix( predict(model, validation), 
                                  validation$SIU_CUST_YN, positive="Y")
        f1    <- cMat$byClass[7]
        
        # 메모리 확보
        rm(cv_train, smote_train, validation)
        gc(reset=T)
        
        return( data.frame(r=r, k=k, ntree=grid[g, "ntree"], 
                           mtry=grid[g, "mtry"],  classwt=grid[g, "classwt"],
                           sampsize_L=grid[g, "sampsize_L"], 
                           sampsize_R=grid[g, "sampsize_R"], f1) )
      }

save(evaluations, file="./fds/evaluations.Rdata")

# -----


# 7. rf with optimal parameter, no cv, smote -------------------------------------

set.seed(421)
system.time(
  m5 <- randomForest(SIU_CUST_YN ~. , 
                     data=smote_train, 
                     importance=T, ntree=500, mtry=4, classwt=c(1,100),
                     sampsize=c(1500, 1500))
) # 103초

# -----
