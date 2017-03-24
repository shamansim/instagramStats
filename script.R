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

ggsave(filename = "201703241823_Progression.png", width = 29, height = 20, units = "cm")
