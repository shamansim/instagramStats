# library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
# library(EBImage) # :(
# library(RCurl)

# download all source code of pages from your instagram account on that website http://www.pictaram.com/user/shamansim/1945339775
# copy-paste it into the same text file
# inspire from http://statistics.berkeley.edu/computing/r-reading-webpages

thepage <- readLines('201703241823_instagramSourceCode.html')

# caption
caption <- '<p class="content">' %>%
  grep(thepage, value=TRUE) %>%
  gsub('<[^>]*>', "", .) %>%
  gsub('&nbsp;', "", .) %>%
  gsub('  ', "", .)

caption <- lapply(strsplit(caption, ' '), function(w) grep('#', w, value=TRUE))

# # 4 of my pictures do not have captions
# # function to insert characters/numbers into a vector
# insert.at <- function(a, pos, ...){
#   dots <- list(...)
#   stopifnot(length(dots)==length(pos))
#   result <- vector("list", 2*length(pos)+1)
#   result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
#   result[c(FALSE,TRUE)] <- dots
#   unlist(result)
# }
# pos <- c(6, 12, 21, 44) # fill by counting on your instagram
# caption <- insert.at(caption, pos, "No caption", "No caption", "No caption", "No caption")

# time
timedate <- '<i class="fa fa-clock-o"></i>([^<]*)</span>' %>%
  grep(thepage, value=TRUE) %>%
  gsub('<[^>]*>', "", .) %>%
  gsub('  ', "", .) %>%
  strptime("%I:%M%p %m/%d/%Y")

# comments
comments <- '<i class="fa fa-comments-o"></i>' %>%
  grep(thepage, value=TRUE) %>%
  gsub('<[^>]*>', "", .) %>%
  gsub(' ', "", .) %>%
  as.integer

# likes
likes <- '<i class="fa fa-heart"></i>' %>%
  grep(thepage, value=TRUE) %>%
  gsub('<[^>]*>', "", .) %>%
  gsub(' ', "", .) %>%
  as.integer
  
# location
locations <- '<i class="fa fa-map-marker"></i>' %>%
  grep(thepage, value=TRUE) %>%
  gsub('<[^>]*>', "", .) %>%
  gsub('&#039;', "", .) %>%
  gsub('  ', "", .) %>%
  as.factor

# 17 of my pictures do not have locations

# photoLink
# image shouldn't be http://scontent.cdninstagram.com/t51.2885-19/s150x150/12139891_506761289492626_942895162_a.jpg
myPhotoLinkPattern <- 'http://scontent.cdninstagram.com/t51.2885-19/s150x150/12139891_506761289492626_942895162_a.jpg'
photoLink <- '<img src="' %>%
  grep(thepage, value=TRUE) %>%
  extract(grep(myPhotoLinkPattern, ., invert = T)) %>% # reverse grep
  sub('.*"(.*)".*', "\\1", .)

# extract hashtag words
######

# basic statistics
(nbPosts <- length(timedate))
(averageLikes <- mean(likes))
(averageComments <- mean(comments))
(uniqueLocation <- length(levels(locations)))

# grather everything
DataClean <- data.frame(time = timedate, likes = likes, comments = comments) %>%
  arrange(time)

# graphs
library(scales)

subtitle <- paste("Number of posts:", nbPosts, ";", "Average nb of likes:", round(averageLikes, 1), ";", "Average nb of comments:", round(averageComments, 1))

ggplot(DataClean, aes(time, likes)) +
  geom_smooth(method = "loess") +
  geom_point(aes(size = comments)) +
  scale_size_continuous(range = c(min(DataClean$comments)+1, max(DataClean$comments))) +
  theme_minimal() +
  labs(x = "", y = "Number of likes") +
  ggtitle(bquote(atop(
    "Instagram @shamansim progression (24th March, 2017)",
    atop(italic(.(subtitle)))
    ))) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(breaks = date_breaks("1 month"), labels = date_format("%m/%Y"))

# ggsave(filename = "201703241823_Progression.png", width = 29, height = 20, units = "cm")

# analysing captions

# caption usage
captionTable <- caption %>%
  unlist %>%
  table %>%
  sort(decreasing = T) %>%
  as.data.frame %>%
  set_colnames(c("hashtag", "nb"))

ggplot(head(captionTable, 100), aes(hashtag, nb)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = "", y = "Frequency") +
  ggtitle(bquote(atop(
    "Instagram @shamansim hashtag usage TOP 100 (24th March, 2017)",
    atop(italic(.(subtitle)))
  ))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(breaks = seq(from = 0, to = max(captionTable$nb)+5, by = 5))

# ggsave(filename = "201703241823_HashtagBarplot.png", width = 29, height = 20, units = "cm")

# nb of hashtags and relation with likes
nbHashtags <- sapply(caption, function(x) length(x)) %>% as.vector
captionLikes <- data.frame("hashtag" = unlist(caption), "nblikes" = rep(likes, nbHashtags), "nbcomments" = rep(comments, nbHashtags)) %>%
  group_by(hashtag) %>%
  summarise_each(funs(sum)) %>%
  arrange(desc(nblikes)) %>%
  as.data.frame

captionLikes <- captionLikes[order(captionLikes$nblikes, decreasing = T), ]

ggplot(head(captionLikes, 100), aes(hashtag, nbcomments, fill = nblikes)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = "", y = "Cumulated nb of likes") +
  ggtitle(bquote(atop(
    "Instagram @shamansim hashtag likes comments TOP 100 (24th March, 2017)",
    atop(italic(.(subtitle)))
  ))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))# +
  # scale_y_continuous(breaks = seq(from = 0, to = max(captionTable$nb)+5, by = 5))

# ggsave(filename = "201703241823_HashtagLikesCommentsBarplot.png", width = 29, height = 20, units = "cm")
