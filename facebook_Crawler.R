# =======================================
# 作者:陳東笙
# 日期:2018/02/20
# 目的:爬蟲for Facebook
# =======================================
# install.packages("Rfacebook")
library(Rfacebook)
# token <- "由FB申請來的TOKEN"
token <- "貼入你的Token"
# 顯示我的FB訊息
me <- getUsers("me", token=token,private_info = TRUE)
myFB <- c(me$id, me$name)
myFB
# 抓社團資訊:
# group_id=753114451505938 (BigDataSpark 大數據論壇)
group <- getGroup(group_id = "753114451505938", token = token, since = "2017/01/01", until = "2017/12/31", n = 5000)
str(group)
summary(group)
# 誰最常貼文
library(wordcloud2)
library(wordcloud)
group$post <- 1
postList <- aggregate(post~from_name, group, sum)
postCount <- data.frame(word = postList[,1], freq = postList[,2])
postCount <- postCount[order(postCount$freq, decreasing = TRUE),]
head(postCount)
# 畫文字雲
wordcloud(postCount$word, postCount$freq,random.order = F, ordered.colors = F,
          colors = rainbow(postCount$freq))
wordcloud2(postCount)
top5 <- table(group$from_name)
top5 <- sort(top5, decreasing = TRUE)[1:5]
top5
barplot(top5, ylim = c(0,200), main = "貼文前5名")
#
## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
# aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(group[[paste0(metric, "_count")]], list(month = group$month),
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}
group$datetime <- format.facebook.date(group$created_time)
group$month <- format(group$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)
# visualize evolution in metric
library(ggplot2)
library(scales)

ggplot(df, aes(x = month, y = x, group = metric)) +
  geom_line(aes(color = metric)) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  labs(title="2017變化", x="Month", y="Average count per post")

top.post <- group[which.max(group$likes_count), ]
top.post



# 再往下挖, 將每一則貼文的細部資訊爬出來
nrow(group)
list1 <- list()
for (i in 1:nrow(group)) {
  list1[[i]] <- getPost(post = group$id[i], token = token, n = 5000)
  Sys.sleep(abs(rnorm(1, 1, 0.5)))
}
# 誰最常來按讚
list1[[1]]$likes
likes <- data.frame()
for(i in 1:length(list1)){
  likes <- rbind(likes,list1[[i]]$likes)
}
likes$post <- 1
likesList <- aggregate(post~from_name, likes, sum)
likesCount <- data.frame(word=likesList[,1], freq = likesList[,2])
likesCount <- likesCount[order(likesCount$freq, decreasing = TRUE),]
head(likesCount)
wordcloud(likesCount$word, likesCount$freq,random.order = F, ordered.colors = F,
          colors = rainbow(likesCount$freq))
wordcloud2(likesCount)
#
# 爬花媽的粉絲團
# Page_ID = kikuChen (花媽粉絲團)
kikuChen_Page <- getPage(page = "kikuChen", token = token, feed = TRUE)
View(kikuChen_Page)
# 爬柯P的粉絲團
# Page_ID = DoctorKoWJ (柯P粉絲團)
DoctorKoWJ_Page <- getPage(page = "DoctorKoWJ", token = token, feed = TRUE)
View(DoctorKoWJ_Page)
