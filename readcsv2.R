# Get some more datasets....

GetUSHouseholdIncome <- function() {
  data <- read.csv("C:/coding/R/TestData/US_CensusBureau_HouseholdIncome.csv", sep=";", header=T)
  data.melted <- melt(data)
  
  # Fix the names to use for nice breaks in x axis labels
  my.labels <- gsub("X", "", names(data))
  my.labels <- gsub("to", " to $", my.labels)
  my.labels <- gsub("^", "$", my.labels)
  my.labels <- gsub("^\\$Under", "Under $", my.labels)
  my.labels <- gsub("andover", " and over", my.labels)
  cat(my.labels)
  
  p1 <- ggplot(data.melted, aes(x=variable, y=value, fill=value)) + geom_bar(stat="identity") +
    ggtitle("US Household income") + xlab("Income in dollars") + ylab("Population") +
    theme(plot.title=element_text(size=18, colour="steelblue4")) +
    theme(axis.text.x=element_text(angle=90, hjust=1))
    #coord_flip()
  
  p1 <- p1 + scale_x_discrete(labels=c(my.labels))
  p1
}

GetImpactStudiesData <- function() {
  impact <- read.table("c:/coding/R/TestData/ImpactStudies.txt", header = T)
  
  concussed <- subset(impact, impact$condition == "concussed")
  control <- subset(impact, impact[, 2] == "control")
  
  p1 <- ggplot(impact) +
    geom_line(aes(x=subject, y=verbal_memory_retest, color=condition, group=condition))
  
  p2 <- ggplot(impact) +
    geom_line(aes(x=subject, y=visual_memory_retest, color=condition, group=condition))

  p3 <- ggplot(impact) +
    geom_line(aes(x=subject, y=visual.motor_speed_retest, color=condition, group=condition))
  
  p4 <- ggplot(impact) +
    geom_line(aes(x=subject, y=impulse_control_retest, color=condition, group=condition))
  
  p5 <- ggplot(impact) +
    geom_line(aes(x=subject, y=reaction_time_retest, color=condition, group=condition))

  p6 <- ggplot(impact) +
    geom_line(aes(x=subject, y=total_symptom_retest, color=condition, group=condition))

  grid.arrange(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
}

ReadFittedTemperatureData <- function() {
  tdata <- read.csv("c:/coding/R/testdata/FittedTemperatureData.txt", sep="", header=T)
  
  ggplot(tdata, aes(x=Temperature, y=Pressure)) + geom_point(col="blue") +
    ggtitle("Temperature Data") + #geom_line(aes(x=Temperature, y=FittedPressure), color="red", alpha=.3) +
    geom_smooth(method="lm", color="green", fill="steelblue2", alpha=.3)
}

GetQuandlData1 <- function() {
  Quandl.auth("xUUDpCpTD3p7HwyrgZjB")
  
  mydata <- Quandl("NSE/OIL")
  head(mydata)
  ggplot(mydata, aes(x=Date, y=Close)) + geom_line(col="blue") + ggtitle("NSE Oil prices")

  mydata <- Quandl("UKONS/LMS_LF24_M") # UK Employment rate
  head(mydata)
  p1 <- ggplot(mydata, aes(x=Year, y=Value)) + geom_line(col="seagreen4") + ylab("Employment rate %") +
    ggtitle("LFS: Employment rate: UK: All: Aged 16-64 (%): SA (Monthly)")
  
  mydata <- Quandl("UKONS/LMS_MGSX_M") # UK Unemployment rate
  head(mydata)
  p2 <- ggplot(mydata, aes(x=Year, y=Value)) + geom_line(col="red") + ylab("Unemployment rate %") +
  ggtitle("LFS: Unemployment rate: UK: All: Aged 16 and over: %: SA (Monthly)")
  
  grid.arrange(p1, p2) # UK Employment and unemployment rate in two graphs
  
  mydata <- Quandl("UKONS/LMS_YBUV_M") # UK Hours worked, weekly average
  head(mydata)
  ggplot(mydata, aes(x=Year, y=Value)) + geom_line(col="blue") + ylab("Weekly average") +
  ggtitle("LFS: Avg actual weekly hours of work: UK: All workers in main & 2nd job: SA (Monthly)")
  
  mydata <- Quandl("FRED/UNRATE")
  # US Civilian Unemployment Rate (http://www.quandl.com/FRED-Federal-Reserve-Economic-Data/UNRATE-Civilian-Unemployment-Rate)
  head(mydata)
  ggplot(mydata, aes(x=Date, y=Value)) + geom_line(col="blue") + ylab("Monthly average") +
    ggtitle("US Civilian Unemployment Rate")
  
  mydata <- Quandl("ZILLOW/MSTATE_PCTTRANSACTIONSTHATAREPREVIOUSLYFORECLOSUREDHOMES_ALLHOMES_PENNSYLVANIA")
  # Zillow Metrics: Foreclosure Resales (%) - Pennsylvania
  # (http://www.quandl.com/ZILLOW-Zillow-Real-Estate-Research/
  #  MSTATE_PCTTRANSACTIONSTHATAREPREVIOUSLYFORECLOSUREDHOMES_ALLHOMES_PENNSYLVANIA-Zillow-Metrics-Foreclosure-Resales-Pennsylvania)
  head(mydata)
  ggplot(mydata, aes(x=Date, y=Value)) + geom_line(col="blue") + ylab("Year") +
    ggtitle("Foreclosure Resales (%) - Pennsylvania")
    #geom_vline(aes(x=sort(mydata[substr(mydata$Date, 6, 7) == "04", 1])))
  
  # Reorder the data:
  mydata2 <- mydata[order(mydata$Date, decreasing=F),]
  mydata.new <- data.frame(Date=mydata2$Date, Foreclosures=mydata2$Value)
  mydata.ts <- ts(mydata.new$Foreclosures, start=c(1998,1), end=c(2014,11), freq=12)
  plot(mydata.ts)
  plot(forecast(mydata.ts), main="Forecast") # Nice!
}

GetUSForeignTradeInGoodsAndServices <- function() {
  # Source: http://www.census.gov/foreign-trade/statistics/historical/
  data <- read.csv("C:/coding/R/TestData/US_ForeignTradeInGoodsAndServices.csv", sep=";", header=T)
  head(data, n=3)
  # Fix spaces in numbers and convert to numeric for continuous y-axis
  data$Export.Services <- as.numeric(gsub(pattern=' ', x=data$Export.Services, replacement=''))
  data$Export.Goods <- as.numeric(gsub(pattern=' ', x=data$Export.Goods, replacement=''))
  data$Import.Services <- as.numeric(gsub(pattern=' ', x=data$Import.Services, replacement=''))
  data$Import.Goods <- as.numeric(gsub(pattern=' ', x=data$Import.Goods, replacement=''))
  head(data, n=3)
  
  year.min <- min(data$Year)
  year.max <- max(data$Year)
  the.title <- paste0("US Foreign trade in goods and services ", year.min, "-", year.max)
  
  data.melted <- melt(data, id="Year", na.rm=T, variable_name="goods.and.services")
  
  ggplot(data=data.melted, aes(x=Year, y=value, color=goods.and.services, group=goods.and.services)) +
    geom_vline(xintercept = 2009, colour="blue", linetype = "longdash") + # insert vertical line at bad year...
    geom_line() + ggtitle(the.title) + ylab("Trade in million dollars") + theme_bw()
    #theme(element_text())  
}

ReadTorontoTrafficLightData <- function() {
  # http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=965b868b5535b210VgnVCM1000003dd60f89RCRD&vgnextchannel=7807e03bb8d1e310VgnVCM10000071d60f89RCRD
  # http://www.r-bloggers.com/heatmap-of-toronto-traffic-signals-using-rgooglemaps/
  data <- read.csv("C:/coding/R/TestData/Toronto_traffic_signals_with_aps.csv", header=T, stringsAsFactors=F) # skip=1 to skip blank top line
  head(data)
  # Transform latitude and longitude:
  rawdata <- data.frame(as.numeric(data$Longitude), as.numeric(data$Latitude))
  names(rawdata) <- c("lon", "lat")
  data <- as.matrix(rawdata)
  theta = pi/15.0
  m = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
  data <- as.matrix(data) %*% m  
}

ScottishIndependenceReferendum <- function() {
  # http://www.theguardian.com/politics/ng-interactive/2014/sep/18/
  #   -sp-scottish-independence-referendum-results-in-full
  # http://rud.is/b/2014/09/20/chartingmapping-the-scottish-vote-with-r-rvestdplyrtidyrtopojsonggplot/
  # https://github.com/hrbrmstr/secede-2014
  
  df <- read.csv("C:/coding/R/TestData/ScottishReferendum2014Results.txt", header=T, sep="\t")
  head(df)
  sapply(df, class)
  df$vote.percentage.yes <- (df$vote.yes / (df$vote.no + df$vote.yes)) * 100
  df$vote.percentage.no <- (df$vote.no / (df$vote.no + df$vote.yes)) * 100
  
  vote.yes <- round(sum(df$vote.yes) / (sum(df$vote.yes) + sum(df$vote.no)) * 100, 1)
  vote.no <- 100 - vote.yes
  
  #df <- df[order(as.character(df$council.area), decreasing=T), ]
  #rownames(df) <- 1:nrow(df)
  
  # NOTE: Re-order the factor levels so ggplot sorts the y-axis A->Z
  df$council.area = with(df, factor(council.area, levels = rev(levels(council.area))))
  
  # TODO: value.name not working here?
  df.melt <- melt(cbind(df[, c("council.area","vote.percentage.yes","vote.percentage.no")],
                        ind=rownames(df)), is.vars=c('ind'), variable_name="Vote")
  
  # TODO: Add percent no/yes and voter numbers texts on bars
  ggplot(data=df.melt, aes(x=council.area, y=value, group=Vote, fill=Vote)) +
    geom_bar(stat="identity") + coord_flip() + geom_hline(yintercept=50, colour="white") +
    scale_fill_manual(values=c("steelblue4","red3"),
                      labels=c(paste0("Yes: ", vote.yes, "%"), paste0("No: ", vote.no, "%")), name="Cecede?") +
    ylab("") + xlab("Council area") +
    #geom_text(aes(label=value), y=0.08, color="white", size=3) +
    #geom_text(aes(label=value), y=0.92, color="white", size=3) +
    scale_y_discrete(limits = c(0, 100)) + # To avoid gray axis "margins"
    theme(panel.background=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + # Remove box/grid
    ggtitle("Scottish independence referendum")  
}

NorwayInternetUse <- function() {
  data <- read.csv("C:/coding/R/TestData/InternetUseNorway.csv", header=T, sep=",")
  head(data)
  data.melted <- melt(data, id="Year")
  names(data.melted) <- c("Year","AgeGroup","value")
  
  ggplot(data=data.melted, aes(x=Year, y=value, colour=AgeGroup)) + geom_line(size=1.2) +
    scale_color_manual(values=c("Blue","Red","Green4"), labels=c("16-44", "45-66", "67-79"), name="Age group") +
    ggtitle("Internet use in Norway by age group") + ylab("Age groups") + theme_bw()
}
