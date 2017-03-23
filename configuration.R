# scientific notation 사용 안하고 그냥 일반적 숫자로 출력
options(scipen=100)  

# 정수+소수 자릿수. 정수부는 무조건 다 출력됨.
options(digits=10) 

# 데이터 읽을 때 character를 자동 factor로 읽지 않게
options(stringsAsFactors=F)   

# p값 *로 표시 안되고 제대로 나오도록
options(show.coef.Pvalues=T)

# 에러메시지 영어로 출력
options(LANG="en_US.UTF-8")

# 병렬처리 코어수
registerDoParallel(cores=detectCores())
