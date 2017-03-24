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

grep("<p class=\"content\">", thepage) # putting keywords around the caption for instance
thepage[190]

thepage[grep("<p class=\"content\">", thepage)]

# continue explore you recognition patterns
# properly gather and write them
captionPattern <- '<p class="content">'
timePattern <- '<i class="fa fa-clock-o"></i>([^<]*)</span>'
commentsPattern <- '<i class="fa fa-comments-o"></i>'
likesPattern <- '<i class="fa fa-heart"></i>'
imageLinkPattern <- '<img src="'
locationPattern <- '<i class="fa fa-map-marker"></i>'

# grabthe values
captionData <- grep(captionPattern, thepage, value=TRUE) # /!\ careful apparently some images do not have captions
timeData <- grep(timePattern, thepage, value=TRUE)
commentsData <- grep(commentsPattern, thepage, value=TRUE)
likesData <- grep(likesPattern, thepage, value=TRUE)
imageLinkData <- grep(imageLinkPattern, thepage, value=TRUE)
locationData <- grep(locationPattern, thepage, value=TRUE) # /!\ careful apparently some images do not have captions

# parse imageLinkData
# image shouldn't be http://scontent.cdninstagram.com/t51.2885-19/s150x150/12139891_506761289492626_942895162_a.jpg
myPhotoLinkPattern <- 'http://scontent.cdninstagram.com/t51.2885-19/s150x150/12139891_506761289492626_942895162_a.jpg'
imageLinkData <- imageLinkData[grep(myPhotoLinkPattern, imageLinkData, invert = T)] # reverse grep

# remove html tags and others
captionData2 <- gsub('<[^>]*>', "", captionData)
captionData3 <- gsub('&nbsp;', "", captionData2)
captionDataClean <- gsub('  ', "", captionData3)

commentsData2 <- gsub('<[^>]*>', "", commentsData)
commentsDataClean <- as.integer(gsub(' ', "", commentsData2))

imageLinkDataClean <- sub('.*"(.*)".*', "\\1", imageLinkData)

likesData2 <- gsub('<[^>]*>', "", likesData)
likesDataClean <- as.integer(gsub(' ', "", likesData2))

timeData2 <- gsub('<[^>]*>', "", timeData)
timeData3 <- gsub('  ', "", timeData2)
timeDataClean <- strptime(timeData3, "%I:%M%p %m/%d/%Y")

locationData2 <- gsub('<[^>]*>', "", locationData)
locationData3 <- gsub('&#039;', "", locationData2)
locationDataClean <- as.factor(gsub('  ', "", locationData3))

# extract hashtag words
######

# basic statistics
(nbPosts <- length(timeDataClean))
(averageLikes <- mean(likesDataClean))
(averageComments <- mean(commentsDataClean))
(uniqueLocation <- length(levels(locationDataClean)))

# grather everything
DataClean <- data.frame(time = timeDataClean, likes = likesDataClean, comments = commentsDataClean) %>%
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
