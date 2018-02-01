# 抓取PTT
library(XML)
library(RCurl)


# only for windows, MAC or Linux預設採OpenSSL
# 因為網址為https, 要取用OpenSSL的SSL憑證檔(C:/R/R-3.4.1/library/RCurl/CurlSSL/cacert.pem)
signatures <- system.file("CurlSSL", cainfo="cacert.pem", package="RCurl")

#### Get the last page Number
# 原來用"//div[@class='btn-group pull-right']/a", 但PTT改了
# lastpage <- unlist(xpathSApply(htmlParse( getURL(paste0("https://www.ptt.cc/bbs/CVS/index.html"), cainfo = signatures)),  "//div[@class='btn-group pull-right']/a",xmlGetAttr, "href"))[[2]]
# 變更為"//div[@class='btn-group btn-group-paging']/a"
lastpage <- unlist(xpathSApply(htmlParse( getURL(paste0("https://www.ptt.cc/bbs/CVS/index.html"), cainfo = signatures)),  "//div[@class='btn-group btn-group-paging']/a",xmlGetAttr, "href"))[[2]]
lastpage <- gsub(".*index", "", lastpage)
lastpage <- as.numeric(gsub("[.]*html", "", lastpage))+1
lastpage  # 最新一筆的序號(2018/01/28為1732)

#### 組出要抓取的網址, 這裏選用最新的100筆網址
link.CVS <- NULL
for( i in (lastpage-99):lastpage){
  url <- paste0("https://www.ptt.cc/bbs/CVS/index", i, ".html")
  html <- htmlParse(getURL(url, cainfo = signatures))
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  link.CVS <- c(link.CVS, paste('https://www.ptt.cc', url.list, sep=''))
  print(paste("Get url from the billboard's(CVS) page :", i))
}
link.CVS[1:3]

#### Write a function to save documents
getdoc <- function(link, path){
  doc <- xpathSApply(htmlParse(getURL(link, cainfo = signatures), encoding="UTF-8"), "//div[@id='main-content']", xmlValue)
  name <- strsplit(link, '/')[[1]][6]
  write(doc, file=file.path(path,gsub('html', 'txt', name)))
}

#### Set the path where you want to save documents
system.time(sapply(1:50, function(i) getdoc(link.CVS[i], path="C:/RDATA/PTT/CVSdocument/")))
