# initial setting
source("./fds/library.R")
source("./fds/configuration.R", encoding='UTF-8')

# =====================================================================================
# Data import
# =====================================================================================
# raw data(excel) to  Rdata

rm(list=ls())
(raw_file_list     <- list.files("./fds/data/"))


file_name <- c()
for(i in 1:length(raw_file_list)){
  file_name <- c(file_name, raw_file_list[i])
}
file_name


# "BGCON_CLAIM_DATA.csv"  "BGCON_CNTT_DATA.csv"   "BGCON_CUST_DATA.csv"  
# "BGCON_FMLY_DATA.csv"   "BGCON_FPINFO_DATA.csv"
claim       <- read.csv( paste0("./fds/data/", file_name[2]), 
                         fileEncoding = "UTF-16", header=T, sep=",")
cntt        <- read.csv( paste0("./fds/data/", file_name[3]), 
                         fileEncoding = "UTF-16", header=T, sep=",")
cust        <- read.csv( paste0("./fds/data/", file_name[4]), 
                         fileEncoding = "UTF-16", header=T, sep=",")
fmly        <- read.csv( paste0("./fds/data/", file_name[5]), 
                         fileEncoding = "UTF-16", header=T, sep=",")
fpinfo      <- read.csv( paste0("./fds/data/", file_name[6]), 
                         fileEncoding = "UTF-16", header=T, sep=",")

# save to Rdata
save(claim, cntt, cust, fmly, fpinfo, file="./fds/fds_raw.Rdata")

rm(list=ls())
