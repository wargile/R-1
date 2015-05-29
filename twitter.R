# Twitter.R - using the twitterR library

# http://thinktostart.wordpress.com/?s=Twitter&submit=Search
# http://www.symplur.com/healthcare-hashtags/diseases/
# http://bommaritollc.com/2011/02/tracking-the-frequency-of-twitter-hashtags-with-r/
# http://gettinggeneticsdone.blogspot.no/2012/07/plotting-frequency-of-twitter-hashtag.html
# http://decisionsandr.blogspot.no/2013/11/using-r-to-find-obamas-most-frequent.html
# https://github.com/tavishsrivastava/Sentiment-analysis-FIFA/blob/master/fifa_pred.R (sentiment analysis on twitter feeds)

# install.packages(c("devtools", "rjson", "bit64", "httr"))
# Restart R
# library(devtools)
# install_github("twitteR", username="geoffjentry")

oldpar <- par()
SetStandardOptions()

#package.install("twitteR")
library(twitteR)
library(ggmap)
library(sp)
#package.install("rworldmap")
library(rworldmap)
library(stringr)
#package.install("wordcloud")
library(wordcloud)

api_key = "hBuoyPq722GLoNpqcGyfdsIRb"
api_secret = "TuHjNnWfdUtBSrbErFJb1JkUXIwEDjPByoahTcLJ8lCVI2clmQ"
access_token_key = "2599813796-cv0nhAmZ1170iHMUNUdN6fOcb13q4Q649Wlk9xi"
access_token_secret = "mMmrynulIXH7QkUJK8AC8IbOa0eCVaYIlX47pB2Co21ZN"

setup_twitter_oauth(api_key, api_secret, access_token_key, access_token_secret)

tweets1 = searchTwitter('@Apple', n=150)
tweets1 = searchTwitter('@Potus', n=350) # Barack Obama
tweets1 = searchTwitter('#flatgate', n=1000, lang="en")
tweets1 = searchTwitter('#AppleWatch', n=150, lang="en")
# @PolitiVestfold, @JBVpresse, @vgnett, @reuterspictures, @dhh, @BarackObama
head(tweets1)
head(tweets1)
tweets1[[1]]$getScreenName()
tweets1[[1]]$getText()

# Convert the returned list to a data frame:
df <- twListToDF(tweets1)
head(df)
names(df)
df[!is.na(df$longitude) & df$longitude != 0, c(11,15,16)] # screenName, longitude, latitude
# Get rid of NA:
df <- df[which(!is.na(df$longitude)), c("latitude", "longitude", "screenName", "created", "text")]
# Get rid of 0:
df <- df[which(df$longitude != "0"), c("latitude", "longitude", "screenName", "created", "text")]
rownames(df)

# http://stackoverflow.com/questions/14334970/convert-latitude-and-longitude-coordinates-to-country-name-in-r?lq=1
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

# TODO: UTF-8 encode non-english stuff?
# http://stackoverflow.com/questions/16574980/problems-with-non-utf-8-and-ascii-characters-twitter-package-in-r
index <- 2
lon <- as.numeric(df$longitude[index])
lat <- as.numeric(df$latitude[index])
longitudes <- df$longitude[!is.na(df$longitude)]
longitudes
if (nrow(df) > 0) {
  zoom.level <- 8 # Lowest level is 3. Highest is 20
  country <- coords2country(data.frame(lon, lat))
  country
} else {
  cat("Can't find lat/lon in dataset!")
}

if (nrow(df) > 0) {
  Encoding(df$text[index]) <- 'utf-8'
  
  if (length(df$text[index] > 50)) {
    text <- paste0(substr(df$text[index], 1, 50), "\n", substr(df$text[index], 51, 130))
  } else {
    text <- paste0(substr(df$text[index], 1, 50))
  }
  
  al1 <- get_map(location=c(lon=as.numeric(df$longitude[index]), lat=as.numeric(df$latitude[index])),
                 zoom=zoom.level, maptype='roadmap')
  al1MAP <- ggmap(al1)
  data <- data.frame(Longitude=as.numeric(df$longitude[index]),
                     Latitude=as.numeric(df$latitude[index]))
  al1MAP + geom_point(data=data, aes(x=Longitude, y=Latitude), color="black", size=8) +
    geom_point(data=data, aes(x=Longitude, y=Latitude), color="red", size=4) +
    xlab("Latitude") + ylab("Longitude") +
    theme(plot.title=element_text(size=12, color="steelblue4")) +
    ggtitle(paste0(df$screenName[index], " in ", country, " said: ", text))
}

# Get hashtag frequencies in tweets from a person:
# http://www.r-bloggers.com/using-r-to-find-obamas-most-frequent-twitter-hashtags/
tw <- userTimeline("BarackObama", n=3200)
tw <- twListToDF(tw)
head(tw)
View(tw$text)
vec1 <- tw$text

# Create a wordcloud from a specific user/users:
# https://sites.google.com/site/miningtwitter/questions/user-tweets/contain-hashtags

person <- c("KimKardashian", "StephenFry", "BarackObama")
person1_tweets = userTimeline(person[1], n=500)
person2_tweets = userTimeline(person[2], n=500)
person3_tweets = userTimeline(person[3], n=500)
person1_df = twListToDF(person1_tweets)
person2_df = twListToDF(person2_tweets)
person3_df = twListToDF(person3_tweets)
person1_hashtags = str_extract_all(person1_df$text, "#\\w+")
person2_hashtags = str_extract_all(person2_df$text, "#\\w+")
person3_hashtags = str_extract_all(person3_df$text, "#\\w+")
# put tags in vector
person1_hashtags = unlist(person1_hashtags)
person1_hashtags <- iconv(person1_hashtags, "latin1", "UTF-8")
person2_hashtags = unlist(person2_hashtags)
person2_hashtags <- iconv(person2_hashtags, "latin1", "UTF-8")
person3_hashtags = unlist(person3_hashtags)
person3_hashtags <- iconv(person3_hashtags, "latin1", "UTF-8")
# calculate hashtag frequencies
person1_tags_freq = table(person1_hashtags)
person2_tags_freq = table(person2_hashtags)
person3_tags_freq = table(person3_hashtags)
# put all tags in a single vector
all_tags = c(person1_tags_freq, person2_tags_freq, person3_tags_freq)
# vector of colors
colors = c("#1B9E77", "#7570B3", "#D95F02")
cols = c(
  rep(colors[1], length(person1_tags_freq)),
  rep(colors[2], length(person2_tags_freq)),
  rep(colors[3], length(person3_tags_freq))
)
# create the wordcloud
oldmar <- par()$mar
par(mar=c(1,1,4,1))
wordcloud(names(all_tags), all_tags, random.order=FALSE, min.freq=2, 
          colors=cols, ordered.colors=T)
mtext(c(paste0("@", person[1]), paste0("@", person[2]), paste0("@", person[3])), side=3,
      line=2, at=c(0.10, 0.52, 0.92), col=colors,
      family="serif", font=2, cex=1.5)
par(mar=oldmar)

# ----------------------------------------------------------------------------------------------------------
# TODO: Use the code from the edX The Anaytics Edge week 5 Twitter resitation to process real tweets
