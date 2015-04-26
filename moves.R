# moves.R - get data from Moves app
# https://www.moves-app.com/

library(ggplot2)

data <- read.csv("c:/coding/R/testdata/moves_activities.csv", header=T, sep=",")
head(data)
sapply(data, class)
#View(data)
summary(data)
str(data)

data <- data[data$Activity == "walking", ]

op <- par()
par(mar=c(5,4.5,2,1))
plot(data$Steps ~ data$Activity, col="orange", main="Moves - Activities",
     cex.main=.8, cex.lab=.7, cex.axis=.7, pch=21, bg="orange")

data.agg <- aggregate(Steps ~ Date, data=data, sum)
sapply(data.agg, class)
#data.agg$Date <- gsub("\\.", "/", as.character(data.agg$Date))
#data.agg$Date <- as.Date(data.agg$Date)
data.agg$Date <- paste0(substring(data.agg$Date, 7,8), "/",
                         substring(data.agg$Date, 4,5), "/",
                         substring(data.agg$Date, 1,2))
data.agg <- data.agg[order(data.agg$Date),]
plot(data.agg$Steps, type="o", pch=21, bg="cyan", col="blue", xaxt="n",
     main="Moves - Activities", ylim=c(0, max(data.agg$Steps)),
     cex.main=.8, cex.lab=.7, cex.axis=.7, xlab="Date", ylab="Steps")
axis(side=1, at=1:nrow(data.agg), labels=data.agg$Date, cex.axis=.7, las=2)

ggplot(data=data.agg, aes(x=Date, y=Steps, fill=Steps)) +
  #geom_bar(stat="Identity", aes(order=Date2)) +
  geom_ribbon(aes(x=Date, ymin=min(Steps), ymax=max(Steps))) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_area(aes(y=Steps)) +
  geom_bar(stat="Identity") +
  #geom_line() +
  #scale_colour_gradient(low="red") +
  ggtitle("Moves - Dates") + xlab("Date") + ylab("Steps")

par <- op