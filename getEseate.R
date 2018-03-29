# ======================================================
# 作者:陳東笙
# 日期:2018/03/25
# 內容:不動產實價登錄解析
# ======================================================
library(stringr)
library(ggmap)
url <- "http://plvr.land.moi.gov.tw//Download?type=zip&fileName=lvr_landcsv.zip"
zipFile <- "lvr_landcsv.zip"
download.file(url, zipFile, mode = "wb")
subDir <- "lvr_landcsv"
path <- paste(getwd(), subDir, sep = "/")
# 檢查目錄是否存在，不存在則Create ----
if(!file.exists(subDir)){
  dir.create(subDir)
}
# 將檔案解壓縮至指定目錄 ----
unzip(zipfile = zipFile, exdir = path)
# 將目錄:lvr_landcsv下的檔案名稱讀進來 ----
files <- dir(path, pattern="\\.CSV", recursive=TRUE, full.names=T)
# 利用正則表示式過濾檔名，只取不動產買賣檔 ----
files <- files[grepl("[A-Z]_LVR_LAND_A.CSV", files)]
# 讀第一個檔案的第一筆資料, 作為column name ----
col.Name <- strsplit(readLines(files[1], encoding = "BIG-5", n=1), ",")
# 第一個欄位補上"縣市" ----
col.Name <- c("縣市",col.Name[[1]][1:26])
# 讀入地區對照檔:A->臺北市, B->臺中市 .... ----
citys <- read.table("C:/RDATA/地區對照1.txt", header = T, stringsAsFactors = F, sep = ",")
# 讀入第一個檔案, 取前1~26個Column ----
tables <- read.csv(files[1], header = FALSE, skip = 2, fileEncoding = "BIG-5",
                   stringsAsFactors = F,
                   colClasses = c(rep(NA,26), rep("NULL", 10)))
# 第27欄補上縣市 ----
tables$V27 <- citys[1,2]
# 利用迴圈自第2個檔案讀起, 利用rbind指令, 合併為data frame ----
for(idx in 2:length(files)){
  data <- readLines(files[idx])
  if(length(data)>2){
    table<- read.csv(files[idx], header = FALSE, skip = 2, fileEncoding = "BIG-5",
                     stringsAsFactors = F,
                     colClasses = c(rep(NA,26), rep("NULL", 10)))
    table$V27 <- citys[idx,2]
    tables <- rbind(tables, table)
  }
}
View(tables)
# 重新整理, 將V27(縣市)搬到第一欄 ----
tables <- cbind(tables[,27], tables[,1:26])
# 設定data frame的column name ----
colnames(tables) <- col.Name
# 移除【交易標的】為土地、車位 ----
tables <- tables[tables$交易標的!="土地",]
tables <- tables[tables$交易標的!="車位",]
# 日期轉換
CNV_DATE <- function(x){
  if(is.null(x)){
    return(NA)
  }else if(is.na(x)){
    return(NA)
  }else if(x == ""){
    return(NA)
  }else if(nchar(x) == 7){
    return(paste(as.integer(substr(x, 1, 3))+1911,substr(x, 4, 5),substr(x, 6, 7), sep = "-"))
  }else if(nchar(x) == 6){
    return(paste(as.integer(substr(x, 1, 2))+1911,substr(x, 3, 4),substr(x, 5, 6), sep = "-"))
  }
}
# 將交易日期/建築完成日期, 轉為西元年月日
tables$西元交易年月日 <- sapply(tables$交易年月日, CNV_DATE)
tables$西元建築完成年月 <- sapply(tables$建築完成年月, CNV_DATE)  # List
# 計算屋齡(交易日-建築完成日)/365, 換算為年, 取至小數2位
x <- tables$西元交易年月日
ymdList <- tables$西元建築完成年月
ymd <- c(rep(NA, length(ymdList)))
HouseAge <- c(rep(NA, length(x)))
for(idx in 1:length(ymdList)){
  if(is.null(ymdList[[idx]])){
    ymd[idx] <- NA
    HouseAge[idx] <- NA
  }else if(is.na(ymdList[[idx]])){
    ymd[idx] <- NA
    HouseAge[idx] <- NA
  }else{
    ymd[idx] <- ymdList[[idx]]
    HouseAge[idx] <- round(as.double((as.Date(x[idx])-as.Date(ymd[idx]))/365),2)
  }
}
tables$西元建築完成年月 <- ymd
tables$屋齡 <- HouseAge