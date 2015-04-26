# Good statistics threads, sites, books, etc:
# ----------------------------------
# Create your own R packages: https://github.com/jtleek/rpackages

# http://fr.slideshare.net/DataRobot/final-10-r-xc-36610234
# http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/
# http://minimaxir.com/2015/02/ggplot-tutorial/
# https://www.teamsciencetoolkit.cancer.gov/Public/TSResourceTool.aspx?tid=1&rid=1959 (NodeXL)
# https://github.com/JohnLangford/vowpal_wabbit/wiki/Tutorial
# http://www.r-bloggers.com/fast-track-publishing-using-knitr-table-mania-part-iv/
# http://www.stattrek.com/tutorials/ap-statistics-tutorial.aspx
# http://www.amazon.com/Statistics-Plain-English-Third-Edition/dp/041587291X/
# https://class.coursera.org/stats1-002/forum/thread?thread_id=2117&sort=newest
# http://datendesign-r.de
# http://r-bloggers.com
# http://www.statistics2013.org/
# http://gallery.r-enthusiasts.com
# http://www.rss.org.uk/site/cms/contentChapterView.asp?chapter=1 (UK Royal Statistical Society)
# http://spark.rstudio.com/fsmart/OLS-App/
# http://scholarwiki.indiana.edu/wiki/index.php?title=A_systematic_analysis_of_performance_measures_for_classification_tasks
# http://www.statisticsblog.com/
# http://www.youtube.com/watch?v=fZuDwiM1XBQ (Hilary Mason)
# http://stats.stackexchange.com/questions/29781/when-should-you-center-your-data-when-should-you-standardize
# http://www.onliveresearch.no/
# http://www.datatau.com/news
# http://www.kdnuggets.com/
# http://101.datascience.community/
# http://jmlr.org/
# http://freakonometrics.hypotheses.org/category/statistics
# https://onlinecourses.science.psu.edu/stat557/
# http://stats.stackexchange.com/questions/29781/when-should-you-center-your-data-when-should-you-standardize
# http://stats.stackexchange.com/questions/7112/when-and-how-to-use-standardized-explanatory-variables-in-linear-regression
# http://stats.stackexchange.com/questions/19216/variables-are-often-adjusted-e-g-standardised-before-making-a-model-when-is

# Free Datasets and other stuff:
# ------------------------------
# http://ec.europa.eu/eurostat/web/main/home (Eurostats, EU Statistics Office)
# http://www.google.com/trends/
# http://data.brreg.no/oppslag/enhetsregisteret/enheter.xhtml
# http://webscope.sandbox.yahoo.com/catalog.php
# http://storage.googleapis.com/books/ngrams/books/datasetsv2.html  (N-grams from Google book scanning)
# http://data.gov
# http://www.bea.gov/index.htm
# http://www.bls.gov/data/
# http://www.asdfree.com/ (Various survey data)
# http://data.worldbank.org/
# http://data.worldbank.org/indicator/SI.POV.GINI (GINI index)
# http://www.kdnuggets.com/datasets/
# http://www.quandl.com
# http://www.inside-r.org/howto/finding-data-internet
# http://www.infochimps.com/collections/statistical-abstract-of-the-united-states
# http://www.census.gov/compendia/statab/
# http://www.census.gov/2010census
# http://factfinder2.census.gov/faces/nav/jsf/pages/index.xhtml
# http://hopey.netfonds.no/paperdump.php?paper=EVRY
# http://www.ncbi.nlm.nih.gov/guide/all/#downloads_
# http://datamarket.com/topic/list/countries/
# http://datamarket.com/data/ (user: tbak8@hotmail.com, pw: XYZxyz234)
# http://www.radicalcartography.net/
# http://data.un.org
# http://infochimps.com/marketplace
# http://www.ungdata.no/id/22414
# http://www.sdss.org (The Sloan Digital Sky Survey)
# http://archive.ics.uci.edu/ml/datasets.html (UCI Machine Learning Repository)
# http://www.gapminder.org/data/
# http://opengovermentdata.org/data/catalogues
# http://wiki.civiccommons.org/initiatives
# http://www.asdfree.com (R scripts for accessing US data)
# http://www.kaggle.com
# http://nedwww.ipac.caltech.edu/ (NASA NED extragalactic database)
# datahub.io
# datahub.io/dataset/movielens
# http://qiao.github.io/PathFinding.js/visual/ (Veeeery cool shortest search part visualization!)
# http://www.theguardian.com/news/datablog

# Free Datasets (no URL):
# -----------------------
# Hilary Mason's Research Data
# Stanford Large Network Data
# CMU Statlib
# Gene expression omnibus
# ArXiv Data
# twitter API, twitterR package
# figshare and rfigshare
# PLoS amd rplos (Public Library of Science)
# rOpenSci (R packages for scientific data)
# https://share.coursera.org/wiki/index.php/Dataanalysis:Main (links for these above)


# Read a dataset and plot with names ----
GraphPISAScoreVersusPovertyRate <- function() {
  my.data <- read.csv("C:/coding/R/TestData/Poverty_PISAscore_by_Country2.csv", header=TRUE, sep=";")
  lp <- ggplot(data=my.data, aes(x=Poverty.Rate, y=PISA.Score, group=1), colour="Blue") # + geom_point(size=2)
  lp <- lp + geom_smooth(method="lm", colour="blue", fill="steelblue2", alpha=.3)
  lp <- lp + xlim(min(my.data$Poverty.Rate), max(my.data$Poverty.Rate) + 3.5) # Avoid text clipping by extending xlim
  lp <- lp + xlab("Percent poverty") + ylab("PISA score") + ggtitle("Poverty versus PISA score")
  lp <- lp + geom_text(label=my.data$Country, size=4, se=FALSE,
                       formula=my.data$PISA.Score ~ my.data$Poverty.Rate, hjust=-0.12, colour="black") +
    geom_point(size=2, se=FALSE,
              formula=my.data$PISA.Score ~ my.data$Poverty.Rate, colour="red")
  
  xaxis.min <- ggplot_build(lp)$panel$ranges[[1]]$x.range[1]
  xaxis.max <- ggplot_build(lp)$panel$ranges[[1]]$x.range[2]
  
  yaxis.min <- ggplot_build(lp)$panel$ranges[[1]]$y.range[1]
  yaxis.max <- ggplot_build(lp)$panel$ranges[[1]]$y.range[2]
  
  #lp <- lp + xlim(xaxis.min, xaxis.max + 1)
  lp
}

# Graph the OECD employment rate for ages 15-64 ----
GraphOECDEmploymentRate <- function(annotate=FALSE) {
  my.data <- read.csv("C:/coding/R/TestData/OECD_EmploymentRate_age15_age64.csv", header=TRUE, sep=";")
  my.data.melted <- melt(my.data, variable="quarter", na.rm=TRUE)
  #levels(my.data.melted$quarter) <- quarter.abb ## Abbreviate x labels if necessary (and if possible. e.g. month names)
  
  lp <- ggplot(data=my.data.melted, aes(quarter, value, group=Country, colour=Country)) +
    #geom_freqpoly(aes(quarter, value, group=Country, colour=Country))
    geom_line() + geom_point(size=3) +
    theme(axis.text.x=element_text(angle=90, hjust=1))
    
  lp <- lp + xlab("Quarter") + ylab("Country") + ggtitle("OECD employment rate, ages 15-64")
  
  if (annotate) {
    lp <- lp + geom_text(label=my.data.melted$Country, size=4, se=TRUE, hjust=-0.1, colour="black", stat = "identity")
    
    #for (counter in 1:length(my.data$Country)) 
    #{  
    #  lp <- lp + geom_text(y=my.data$X2010.Q3[counter] + 0.3, x=1, size=4, colour="black",
    #                       label=my.data$Country[counter])
    #}
  }
  
  lp
}

GraphEuroStatUnemploymentRate <- function(annotate=F) {
  my.data <- read.csv("C:/coding/R/TestData/EuroStat_UnemploymentData_2005_2014.csv", header=TRUE, sep=";",
                      colClasses=c("factor",rep("numeric", 10),"factor","factor"))
  my.data <- subset(my.data, (Sex %in% c("All","F","M")) &
                      (Country %in% c("Norway","Sweden","Denmark","United States","Spain","Greece")))[,1:12]
  my.data.melted <- melt(my.data, id=c("Country","Sex"), na.rm=TRUE)
  head(my.data.melted)
  #levels(my.data.melted$quarter) <- quarter.abb ## Abbreviate x labels if necessary (and if possible. e.g. month names)
  
  lp <- ggplot(data=my.data.melted, aes(substr(variable, 5, 9), value, group=Country, colour=Country)) +
    geom_line(size=.8) + geom_point(size=3, shape=15) +
    ggtitle("EuroStat unemployment rate 2005-2014, by sex") +
    ylab("Unemployment rate") + xlab("Year") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=0, hjust=1)) +
    facet_grid(Sex ~ .)
  
  if (annotate) {
    lp <- lp + geom_text(label=my.data.melted$Country, size=4, se=TRUE, hjust=-0.1, colour="black", stat="identity")
    
    #for (counter in 1:length(my.data$Country)) 
    #{  
    #  lp <- lp + geom_text(y=my.data$X2010.Q3[counter] + 0.3, x=1, size=4, colour="black",
    #                       label=my.data$Country[counter])
    #}
  }
  
  lp
}

# Generic boxplot example with mtcars dataframe ----
GraphBoxPlotData <- function(annotate=FALSE) {
  attach(mtcars)  

  lp <- ggplot(data=mtcars, aes(factor(cyl), mpg, fill=cyl)) + geom_boxplot()
  lp <- lp + xlab("Cylinders") + ylab("Miles per gallon") + ggtitle("Just testing boxplot... MPG by Cylinders") +
    scale_fill_continuous(name="Number of\ncylinders") # NOTE: For custom title on legend
  
  lp
}

GraphBoxPlotData2 <- function() {
  df = read.table(text = "
    Year Month Max.Temp Min.Temp Frost  Rain
    1950   1Jan      3.7      2.8     7  20.1
    1950   2Feb     10.3        4     4 127.0
    1950   3Mar     13.0      4.5     2  39.4
    1950   4Apr     13.1      4.7     0  62.0
    1950   5May     14.9      7.8     1  32.2
    1951   1Jan     12.7      3.8     6  20.1
    1951   2Feb     12.3        3     5 101.0
    1951   3Mar     15.0      3.5     2  29.4
    1951   4Apr     16.6      3.2     0  51.0
    1951   5May     21.3      3.8     0  54.2
    1952   1Jan      4.7      3.1     3  15.1
    1952   2Feb      7.6      2.2     4  88.0
    1952   3Mar     11.9      4.2     2  22.3
    1952   4Apr     16.2      6.5     0  41.2
    1952   5May     18.1      9.4     1  22.8",
    header = TRUE, sep = "")
                  
  p1 <- ggplot(df, aes(Month, Max.Temp, fill=Month)) +
    geom_boxplot(outlier.shape=16, outlier.size=3, outlier.color="blue", stat="boxplot", alpha=.5) +
    geom_jitter() + 
    ylab("Maximum Temperature") + ggtitle("Temperature at Terje's Weather Station (1950-1952)") + 
    theme(plot.title=element_text(size=18, color="steelblue4"))
  
  # Using facets to split plot by year + smoother + b/w theme
  p2 <- ggplot(df, aes(x=Month, y=Max.Temp, fill=Month)) +
    geom_point(aes(color=Month), alpha=.8, size=3) +
    #stat_summary(fun.y=sum, geom="line", color="seagreen2", alpha=.6, size=1.4) +
    ylab("Maximum Temperature") + ggtitle("Temperature at Terje's Weather Station (1950-1952)") + 
    theme(plot.title=element_text(size=18, color="steelblue4")) + facet_grid(. ~ Year) + 
    geom_smooth(method="lm", aes(group=1), color="seagreen2", alpha=.2, se=F) + 
    theme_bw()
  
  # TIP: To include outliers in plot when using ylim:
  # + coord_cartesian(ylim=c(-3, 3))
  
  grid.arrange(p1, p2)
}

# Get stock data from NetFonds
GraphStocksData <- function(ticker="EVRY", ticker.company="EVRY", getweek=TRUE, numweeks=1, saveToPDF=F) {
  stocks <- read.csv(url(paste("http://hopey.netfonds.no/paperhistory.php?paper=", ticker, ".OSE&csv_format=csv")))
  
  numrows <- nrow(stocks)
  
  #URL <- "http://ichart.finance.yahoo.com/table.csv?s=EVRY"
  #stocks <- read.csv(URL)
  
  #cat(colnames(stocks))
  str(stocks) # Get some info about the returned data set
  #print(stocks)
  
  if (grepl("DOCTYPE", colnames(stocks)[1])) # Just HTML header col returned
    return(warning("Unable to get stock data for ticker ", ticker.company, "!"))

  if (NCOL(stocks) > 1) { # If just 1 col, it's just HTML header etc. returned, ticker not found
    # TODO: Get a subset for more line data?
    #stocks.subset <- subset(stocks, select=c(close, open, high))
    
    # TODO: Needed for xaxis?? Sort the dataframe on quote_date ASC, since it is received in DESC order
    #stocks <- stocks[order(stocks$quote_date),]
    
    # Get sample of rows:
    # quote_date_sample <- stocks[seq(1,nrow(stocks),ceiling(nrow(stocks)/20)), ]
    
    if (getweek) {
      # if (weekdays(today) == "Saturday" || weekdays(today) == "Sunday") { ... }
      today <- Sys.Date() - (7 * numweeks) # Get data for <numweeks> weeks
      today.formatted <- format(today, "%Y%m%d")
      #cat("Today:", today.formatted)
      stocks <- stocks[stocks$quote_date >= today.formatted,]
      #cat("\n\nStock rows returned:\n")
      #cat(stocks$close, "\n")
      #cat(stocks$quote_date, "\n")
    } else {
      # Just get some rows with regular intervals
      #cat(stocks$quote_date, "\n")
      #take <- 1
      #skip <- ceiling(numrows / 100.0)
      #total <- nrow(stocks)
      #reps <- total %/% (skip + take)
      #index <- rep(0:reps, each=take) * (skip + take)
      #index[1] <- 1 # change from 0
      #stocks <- stocks[index, ]
      
      # TEST, use this instead to get a sample with regular intervals:
      stocks <- stocks[seq(1,nrow(stocks),ceiling(nrow(stocks)/30)), ]
    }

    # stocks$quote_date <- factor(stocks$quote_date, levels=stocks$quote_date[!duplicated(stocks$quote_date)])
    
    # Fix the dates for xlabs
    stocks <- transform(stocks, quote_date = paste(substring(as.character(quote_date), 1, 4), "/",
                                                   substring(as.character(quote_date), 5, 6), "/",
                                                   substring(as.character(quote_date), 7, 8), sep=""))
    
    #stocks$quote_date <- as.Date(as.character(c(stocks$quote_date)))
    
    #stocks <- sample(stocks, 10) # TODO: sample better??
    #print(stocks)
    #par(xaxt="n")
    #cat("Length:", length(stocks$close), "\n")
    
    value.at.close <- stocks$close[1]
    
    lp <- ggplot(data=stocks, aes(x=quote_date, y=close, fill=paper, group=1)) +
      #ymax=max(stocks$close), ymin=min(stocks$close))
      geom_point(color="blue", size=2) + stat_summary(fun.y=sum, geom="line", color="seagreen2", alpha=.6, size=1.4)
    
    if (getweek) {
      weeks <- paste("- last", ceiling(length(stocks$quote_date)/5),
                     ifelse(floor(length(stocks$quote_date)/5) > 1, "weeks", "week"))
    } else {
      weeks <- "- total period available"
    }
                     
    lp <- lp + xlab(paste("Quote date for", ticker, weeks)) +
      ylab("Value at close") + ggtitle(paste("Ticker:", ticker.company, "-", value.at.close)) +
      theme(plot.title=element_text(size=16, color="steelblue4", vjust=1)) +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      theme(axis.title.x = element_text(vjust = -0.2)) +
      theme(axis.title.y = element_text(vjust = 1)) +
      guides(fill=F) # No legend
      #theme(panel.grid.minor=element_line(linetype = "2925"))
    
    if (length(stocks$close) > 30)
      lp <- lp + scale_x_discrete(breaks=c(min(stocks$quote_date),
                                           stocks$quote_date[ceiling(length(stocks$quote_date) / 2.0)],
                                           max(stocks$quote_date)))
    
    # Save the generated plot automatically to PDF
    if (saveToPDF == T) {
      SaveToPDF(paste(getwd(), "/Plots/", "TerjesStocksFeed.pdf", sep=""), lp)
    }
    
    lp
  } else warning(paste("Unable to get stock data for", ticker, "!"))
}

# Get stocks data, calls GRaphStockData() ----
GetStocksData <- function(getweek=TRUE, numweeks=1) {
  #df <- read.csv("c:/coding/R/testdata/NorskeTickers.csv", header=T, sep=";", stringsAsFactor=F)
  df <- read.csv("http://norma.netfonds.no/kurs.php?layout=horizontal&exchange=OSE&sec_types=&ticks=&table=tab&sort=alphabetic",
                     header=T, sep="\t", stringsAsFactor=F)
  
  tickers <- as.list(df$paper)
  tickers.company <- df$name
  savetopdf <- F
  
  manipulate(GraphStocksData(ticker, tickers.company[which(unlist(tickers) == ticker)], getweek, numweeks, savetopdf),
             ticker=picker(tickers, label="Select ticker"),
             getweek=checkbox(getweek, label="Get <n> weeks only"),
             numweeks=slider(1, 52, 4, numweeks, label="Number of weeks to get"),
             savetopdf=checkbox(savetopdf, label="Save graph to PDF"))
}

# Save plot to PDF ----
SaveToPDF <- function(plotname, plot) {
  # width and height are in inches
  pdf(plotname, width=10, height=7)
  print(plot)
  dev.off()
}

# Histogram example ----
CreateHistogram <- function() {
  my.numbers <- c(3,3,3,1,1,1,2,3,3,4,4,4,4,4,4,4,1,1,2,2,5,5,5,5,5,6,7,7,9,10,10,10,10,12)
  my.frame1 <- data.frame(my.data=table(my.numbers)) # Convert to table to avoid gaps in histogram
  
  my.temperatures <- as.numeric(c(37.9, 37.8, 37.7, 37.0, 36.9, 36.8, 36.7, 37.1, 37.2, 37.3, 37.4, 37.5, 37.6))
  my.frequencies <- as.numeric(c(2, 3, 4, 14, 8, 6, 4, 14, 12, 11, 8, 6, 4))
  my.frame2 <- data.frame(my.temperatures, my.frequencies)

  # Get the mode (from tools.R function MyMode) to show in xlab
  my.rep.temperatures <- rep(my.temperatures, my.frequencies)
  my.mode <- MyMode(my.rep.temperatures)
  
  # Histogram shows frequency of elements on y-axis
  
  # Non-density plot
  #p2 <- ggplot(my.frame, aes(x=my.data.my.numbers, y=my.data.Freq)) +
  #  geom_histogram(color="blue", stat="identity", fill="steelblue3")
  p2 <- ggplot(my.frame2, aes(x=my.temperatures, y=my.frequencies)) +
    geom_histogram(color="blue", stat="identity", fill="steelblue3", alpha=.6)
  p2 <- p2 + ylab("Frequency") + xlab(paste("Temperature (mode=", paste(my.mode, collapse=" "), ")", sep="")) +
    ggtitle("Terje's Temperature Histogram") +
    theme(plot.title=element_text(size = 18, colour="steelblue4", face="bold.italic"))
  
  # TODO: Density plot
  p1 <- qplot(data=my.frame1, x=my.data.my.numbers, weight=my.data.Freq)
  p1 <- p1 + geom_histogram(aes(y=..density..)) + geom_density(fill="steelblue3", colour="red")
  
  grid.arrange(p2, p1)
}

# Nice stacked and annotated barplot example from the web ----
CreateBarPlot <- function() {
  # Read in data
  df = read.table(text = "
    activity                         yes    no  dontknow
    Social.events                     27    3   3
    Academic.skills.workshops         23    5   8
    Summer.research                   22    7   7
    Research.fellowship               20    6   9
    Travel.grants                     18    8   7
    Resume.preparation                17    4   12
    RAs                               14    11  8
    Faculty.preparation               13    8   11
    Job.interview.skills              11    9   12
    Preparation.of.manuscripts        10    8   14
    Courses.in.other.campuses          5    11  15
    Teaching.fellowships               4    14  16
    TAs                                3    15  15
    Access.to.labs.in.other.campuses   3    11  18
    Interdisciplinary.research         2    11  18
    Interdepartmental.projects         1    12  19",
    header = TRUE, sep = "")

  # Melt the data frame
  dfm = melt(df, id.vars=c("activity"), measure.vars=c("yes","no","dontknow"),
    variable_name="haveused", value.name="responses") # NOTE: value.name (or value_name?) param no longer working!

  # Reorder the levels of activity
  dfm$activity = factor(dfm$activity, levels=df$activity)
  
  # Get the positions of the labels
  library(plyr)
  dfm = ddply(dfm, .(activity), transform, pos=cumsum(value) - 0.5*value)
  
  # Draw the plot
  ggplot(dfm, aes(x=activity, y=value)) + 
    geom_bar(aes(fill=haveused), stat="identity") +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    guides(fill=guide_legend(reverse=TRUE)) +
    geom_text(aes(label=value, y=pos), size=3)  # Labels inside the bar segments  
}

# Using qplot example ----
CreateBarPlot2 <- function() {
  my.data <- read.table(text="
    Company            Month    Sales
    'My Company'       1        10
    'Your Company'     1        22
    'My Company'       2        15
    'Your Company'     2        11
    'My Company'       3        26
    'Your Company'     3        14
    'My Company'       4        19
    'Your Company'     4        13
    'My Company'       5        15
    'Your Company'     5        16
    'My Company'       6        12
    'Your Company'     6        22
    'My Company'       7        25
    'Your Company'     7        15",
    header=TRUE, sep="", nrows=15)
  # Setting nrows makes R run faster on big datasets if you know beforehand. Slight overestimation is OK.
  
  # Position="dodge": Side-by-side bars, otherwise stacked
  #qplot(data=my.data, x=factor(Month), y=Sales, geom="bar", fill=Company,
  #      group=Company,
  #      position="dodge",
  #      stat="Identity", main="Company Sales", xlab="Month", ylab="Sales") + coord_flip()
  
  k <- ggplot(data=my.data, aes(x=factor(Month), y=Sales, fill=Company))
  k <- k + ggtitle("Company Sales") + theme(plot.title=element_text(size=18, colour="blue")) +
    xlab("Month") + ylab("Sales pr. month")
  k + geom_bar(stat="identity", color="Blue", alpha=.6, position="dodge") + scale_fill_brewer(palette=5)
}

CreateBarPlot3 <- function() {
  # http://factfinder2.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_12_1YR_DP02&prodType=table
  
  df <- read.csv("C:/coding/R/TestData/AmericanCommunitySurvey2012_factfinder2.census.gov.csv")
  #df <- transform(df, rev(Nationality)) # Reverse y-scale for coord_flip
  
  ggplot(data=df) + geom_bar(aes(x=Nationality, y=Population, fill=Nationality), stat="identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    ggtitle("US Ancestry 2012") + theme(plot.title=element_text(size=18, colour="blue")) #+ coord_flip()
}

# Find frequencies in numeric data, plot histogram
FindFrequencies <- function() {
  my.data <- c(2,2,1,5,5,5,5,5,6,6,7,7,8,5,4,3,2,1,11)
  df <- as.data.frame(table(my.data))
  
  k <- ggplot(data=df) + geom_histogram(aes(x=my.data, y=Freq), stat="identity", color="Blue", fill="Cyan", alpha=.2)
  k <- k + ggtitle("Find frequencies in numeric data") + theme(plot.title=element_text(size=18, colour="steelblue4")) +
    xlab("Numbers") + ylab("Frequency")
  k
}

WinTheLotto <- function(totalNumbers=34, correctNumbers=7) {
  factorial(totalNumbers) / (factorial(correctNumbers) * factorial(totalNumbers - correctNumbers))
}

GraphTimeSeries <- function() {
  my.ts <- ts(seq(1, 41, 2), frequency = 12, start = c(1959, 2))
  df <- data.frame(my.ts, x=seq(1:length(my.ts)))
  
  ggplot(data=df) + geom_area(aes(x=x, y=as.numeric(my.ts)^2), alpha=.2, color="Blue", fill="Blue") + 
    geom_line(aes(x=x, y=as.numeric(my.ts)^2.05), alpha=.8, color="Green", size=1.2) + 
    geom_point(aes(x=x, y=as.numeric(my.ts)^2.05), alpha=.8, color="Blue", size=3)
}

GetCensusData1 <- function() {
  # http://www.infochimps.com/collections/statistical-abstract-of-the-united-states
  # http://www.census.gov/compendia/statab/
  # http://www2.census.gov/census_2010/03-Demographic_Profile/Pennsylvania/
  
  df <- read.csv("C:/coding/R/TestData/statab2008_0002_Population1900To2006-csv/data.csv", header=TRUE)
  head(df)
  year.subset <- subset(df$Year, df$Year %% 5 == 0)
  
  p1 <- ggplot(data=df, aes(x=Year, y=ResidentPopulationIn1000)) + geom_line(color="blue", size=.2) +
    geom_area(alpha=.2, fill="seagreen4") + 
    ggtitle("US Resident Population 1900-2006") + theme(plot.title = element_text(size = 14, colour="blue")) +
    xlab("Year") + ylab("Resident Population in 1000") + scale_x_continuous(breaks=year.subset) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1))
  
  dfPeriod <- subset(df, (df$Year >= 1935 & df$Year <= 1955))
  
  p2 <- ggplot(data=dfPeriod, aes(x=Year, y=ResidentPopulationIn1000)) + geom_line(color="blue", size=.2) +
    geom_area(alpha=.2, fill="seagreen4") +
    ggtitle("US Resident Population 1935-1955") + theme(plot.title = element_text(size = 14, colour="blue")) + 
    xlab("Year") + ylab("Resident Population in 1000") + scale_x_continuous(breaks=year.subset) + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
  grid.arrange(p1, p2, ncol=2, nrow=1)
}

GetCensusData2 <- function() {
  # US Population By Age And Sex 2000 2010
  # DataSet: http://www.census.gov/prod/cen2010/briefs/c2010br-03.pdf
  df <- read.csv("C:/coding/R/TestData/US_PopulationByAgeAndSex_2000_2010.csv", header=T)
  
  # TIPS: rbind, cbind of female/male? Factor? Group?
  # TIPS: geom_bar(stat="summary", fun.y="mean") 
  # TIPS: position="stack"
  # TIPS: http://stackoverflow.com/questions/4559229/drawing-pyramid-plot-using-r-and-ggplot2
  # TIPS: http://learnr.wordpress.com/2009/09/24/ggplot2-back-to-back-bar-charts/
  
  #my.matrix = matrix(c(df$Male2010, df$Female2010), nrow=length(df$Age), ncol=2, dimnames=list(df$Age, c("Male","Female")))
  #my.df <- t(my.matrix) # Transpose the matrix
  #barplot(t(my.matrix), beside=TRUE)
  
  p1 <- qplot(data=df, x=factor(Age), y=Male2010, geom="bar", fill=Age, group=Age,
    position="dodge", stat="Identity", main="Age 2010, Male", xlab="Age group", ylab="Population") + coord_flip()

  p2 <- qplot(data=df, x=factor(Age), y=Female2010, geom="bar", fill=Age, group=Age,
              position="dodge", stat="Identity", main="Age 2010, Female", xlab="Age group", ylab="Population") + coord_flip()
  
  grid.arrange(p1, p2, ncol=2, nrow=1)
}

SalkPolioVaccineTrials <- function() {
  # http://wps.aw.com/wps/media/objects/14/15269/projects/ch12_salk/index.html
  # http://datamarket.com/data/set/22u4/monthly-us-polio-cases#!display=line&ds=22u4&e=3nw
  df <- read.csv("C:/coding/R/TestData/US_SalkPolioVaccineTrials.csv", header=T)
  
  ggplot(data=df, aes(x=Year, y=PolioCases)) + geom_line() + 
    geom_area(alpha=.3, fill="seagreen4") +
    ggtitle("Salk 1954 Vaccine Field Trial, US Polio Cases 1930-1955") +
    theme(plot.title = element_text(size = 18, colour="steelblue4")) + 
    xlab("Year") + ylab("Polio Cases") + scale_x_continuous(breaks=c(df$Year)) +
    scale_y_continuous(breaks=seq(0, max(df$PolioCases), max(df$PolioCases) / 10.0)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
}

GetDataMarketData1 <- function() {
  # install.packages("rdatamarket")
  # library(rdatamarket)
  
  # Norway population
  dminit("0577d88e3d4743d5bcbb1f278415b593")
  a <- dmlist("1cfl!r3d=2b")
  a
  
  ggplot(data=a, aes(x=Year, y=Value)) + geom_area(fill="blue", alpha=.2) +
    geom_line(size=.8, color="Blue") + ylab("Population") + ggtitle("Norway Population")
}

GetDataMarketData2 <- function() {
  #Norway total population, both sexes combined
  dminit("0577d88e3d4743d5bcbb1f278415b593")
  a <- dmlist("12rb!e4s=2c:e4t=2.3.5")
  a.melted <- melt(a, id=c("Country.or.Area", "Variant", "Year"))
  
  #ggplot(data=a, aes(x=Year, y=Value)) + geom_area(fill="blue", alpha=.2) +
  #  geom_line(size=.8, color="Blue") + ylab("Population") + ggtitle("Norway Population")
  
  ggplot(data=a.melted, aes(x=Year, y=value, color=Variant)) + geom_line(size=1) +
    ylab("Population") + ggtitle("Norway Population, both sexes combined")
  
}

GetDataMarketData3 <- function() {
  # Norway population by level of education
  dminit("0577d88e3d4743d5bcbb1f278415b593")
  a <- dmlist("1ctp!wtk=1q:wtm=3.1:wtl=6.5.4")
  #a.melted <- melt(a, id=c("Country", "Sex", "Level.of.education", "Year"))
  
  # Stacked barplot
  #ggplot(data=a.melted, aes(x=Level.of.education, y=value, category=Level.of.education, fill=Sex)) +
  #  geom_bar(stat="identity")
  
  # Side-by-side bars with position="dodge", otherwise stacked bars
  #ggplot(data=a.melted, aes(x=Level.of.education, y=value, category=Level.of.education, fill=Sex)) +
  ggplot(data=a, aes(x=Level.of.education, y=Value, category=Level.of.education, fill=Sex)) +
    geom_bar(stat="identity", position="dodge") + coord_flip() + ylab("Population") + xlab("Level of education") +
    ggtitle("Norway's population by level of education") + theme(plot.title = element_text(size = 18, colour="blue"))
}

GetDataMarketData4 <- function() {
  # Norway population, median age (years)
  dminit("0577d88e3d4743d5bcbb1f278415b593")
  a <- dmlist("12rs!e5q=23:e5r")

  a.melted <- melt(a, id=c("Country.or.Area", "Variant", "Year"))
  
  ggplot(data=a.melted, aes(x=Year, y=value, color=Variant)) + geom_line(size=1) +
    ylab("Population") + ggtitle("Norway population, median age (years)")  
}

GetDataMarketData5 <- function() {
  # US, Harmonised unemployment rates (%) - monthly data
  a <- read.csv("C:/coding/R/TestData/US_Unemployment_NotSeasonallyAdjusted.txt", header=T, sep=",", dec=".")
  
  a.melted <- melt(a, id="Month")
  # View(a.melted) # View data in RStudio table
  
  # NOTE: Use code below to split a time series into nice chunks for axis ticks/labels
  my.breaks <- c(seq(1, length(a.melted$Month), ceiling(length(a.melted$Month) / 13)),
                 length(a.melted$Month))
  
  p <- ggplot(data=a.melted, aes(x=Month, y=value, color=variable, group=variable)) + geom_line(size=.8) +
    ylab("Unemployment rate") + ggtitle("US, ILO harmonised unemployment rates (%) - monthly data") +
    scale_x_discrete(breaks=a.melted$Month[my.breaks]) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
  # No effect below, changes legends?? scale_fill_manual??
  p <- p +
    scale_fill_manual(name="Unemployment rates", values=c("#999999", "#E69F00", "#56B4E9"),
                         breaks=c("Unemployment.ILO.Def.Over.25.Total",
                                  "Unemployment.ILO.Def.Total",
                                  "Unemployment.ILO.Def.Under.25.Total"),
                         labels=c("Over 25 total", "Total", "Under 25 total"))
  
  p
  # IMPORTANT with group=variable here! Note also "breaks" syntax for row selection above
}

GetDataMarketData6 <- function() {
# Average monthly temperatures across the world (1701-2011)
  dminit("0577d88e3d4743d5bcbb1f278415b593")
  a <- dmlist("1loo!1n6s=lt.25m.38k.3wl")
  
  #Lots of data, so get some rows
  #numrows <- nrow(a)
  #take <- 1
  #skip <- ceiling(numrows / 50.0)
  #total <- nrow(a)
  #reps <- total %/% (skip + take)
  #index <- rep(0:reps, each=take) * (skip + take)
  #index[1] <- 1 # change from 0
  #a <- a[index, ]
  
  # Get a particular month
  #gnp <- cycle(a$Month)
  #subset(a, cycle(gnp) == 7)
  # Convert to time series
  #head(ts(a))
  
  # Just get the month and year substrings, and subset
  my.months <- as.numeric(substring(a$Month, first=6, last=7))
  my.years <- as.numeric(substring(a$Month, first=1, last=4))
  my.subset <- subset(a, (my.months %in% c(1, 7)) & (my.years %% 10 == 0))
   
  ggplot(data=my.subset, aes(x=Month, y=Value, group=Weather.station, color=Weather.station)) + geom_line(size=1) +
    ylab("Temperature celsius degrees") + ggtitle("Average monthly temperatures across the world (1701-2011)") +
    theme(axis.text.x=element_text(angle=90, hjust=1))
}

GetSuperBowlData <- function() {
  superbowl <- read.table(
    "http://www.portfolioprobe.com/R/blog/dowjones_super.csv", 
    sep=",", header=TRUE)
  #dim(superbowl)
  
  plot(100 * DowJonesSimpleReturn ~ Winner, cex.lab=.8, cex.axis=.8, main="Superbowl Winners",
       data=superbowl, col=as.integer(unique(superbowl$Winner)) + 2, ylab="Return (%)")
}

UseDataTable <- function() {
  # library(data.table)
  # TODO: Find better data with rows to group/sum/avg. etc.!
  my.data <- fread("C:/coding/R/TestData/Poverty_PISAscore_by_Country2.csv", header=TRUE, sep=";")
  head(my.data)
  sapply(my.data, class)
  setnames(my.data, c("Country", "Poverty.Rate", "PISA.Score"))
  my.data[, Score:=sum(PISA.Score), by=Country] # Bad example....
  head(my.data)  
}

UseLandsatImages <- function() {
  # http://www.jstatsoft.org/v43/i04/paper
  # http://landsat.usgs.gov/consumer.php
  # http://glovis.usgs.gov/
  # http://earthexplorer.usgs.gov/
  package.install("oce")
  library(oce)
  
  # START Code from: http://www.r-bloggers.com/landsat-thermal-imaging/
  l <- read.landsat("LC80080292014065LGN00", band="tirs1")
  tirs1 <- l[["tirs1"]]
  # @ = slots: http://stat.ethz.ch/R-manual/R-devel/library/methods/html/slot.html
  ML <- l@metadata$header$radiance_mult_band_10
  AL <- l@metadata$header$radiance_add_band_10
  K1 <- l@metadata$header$k1_constant_band_10
  K2 <- l@metadata$header$k2_constant_band_10
  d <- tirs1 * (2^16 - 1)            # convert from range 0 to 1
  Llambda <- ML * d + AL
  dd <- K2 / log(K1 / Llambda + 1)
  SST <- dd - 273.15                 # convert Kelvin to Celcius
  l@data$SST <- SST
  plot(l, band="SST", col=oceColorsJet)
  mtext(l[["time"]])
  # END Code from: http://www.r-bloggers.com/landsat-thermal-imaging/
}

CreateMovingAverage <- function(data) {
  plot(data, col="blue", type="l", main="Moving Average example", cex.axis=.7, cex.lab=.7, cex.main=.8,
       ylim=c(min(data)-.1, max(data)+.4))

  ma.period <- 6
  data.norm <- sapply(ma.period:length(data), function(x) (1/ma.period) * sum(data[(x-ma.period):x]))
  lines(ma.period:length(data), data.norm, col="gray")
  ma.period <- 4
  data.norm <- sapply(ma.period:length(data), function(x) (1/ma.period) * sum(data[(x-ma.period):x]))
  lines(ma.period:length(data), data.norm, col="green")
  ma.period <- 2
  data.norm <- sapply(ma.period:length(data), function(x) (1/ma.period) * sum(data[(x-ma.period):x]))
  lines(ma.period:length(data), data.norm, col="red")
}

ScrapeWebTable <- function(url) {
  # See: http://statofmind.wordpress.com/2014/05/27/using-sentiment-analysis-to-predict-ratings-of-popular-tv-series/comment-page-1/#comment-39
  # https://github.com/tlfvincent/StatOfMind/tree/master/Sentiment_Analysis_of_TV_shows
  # Call with:
  # url <- 'http://www.imdb.com/title/tt0903747/epdate'
  # series.ratings <- ScrapeWebTable(url)
  require(XML)
  # Get HTML of url
  doc <- htmlParse(url)
    
  # Find all tables in webpage
  tables <- readHTMLTable(doc)
    
  # Find largest table and return as dataframe
  nrows <- unlist(lapply(tables, function(t) dim(t)[1]))
  df <- tables[[which.max(nrows)]]
    
  return(df)  
}

UsingSqlDf <- function() {
  # library(sqldf)
  df1 <- data.frame(x=c(1,2,3,4), y=c('En','To','Tre','Fire'))
  df2 <- data.frame(x=c(1,2,3,5), y=c('En','To','Tre','Fem'))
  
  result <- sqldf('SELECT * FROM df1 UNION SELECT * FROM df2')
  result <- sqldf('SELECT * FROM df1 WHERE x > 2')
  result <- sqldf('SELECT * FROM df1 WHERE x IN (1,3)')
  result <- sqldf('SELECT * FROM df1 WHERE x IN (SELECT x FROM df2)')
  result <- sqldf('SELECT y FROM df1 EXCEPT SELECT y FROM df2') # Get records in df1 not found in df2, removing duplicates
  result <- sqldf('SELECT x,y FROM df1 INTERSECT SELECT x,y FROM df2') # Get records present in both tables

  # Similar for Python: https://class.coursera.org/datasci-002/forum/thread?thread_id=898
  db <- dbConnect(SQLite(),
                  dbname="C:/coding/git/coursera/introductiontodatascience/datasci_course_materials/assignment2/reuters.db")
  result <- dbGetQuery(db, "SELECT * FROM Frequency WHERE count=19")
  result
}

GetHYGStarsDatabase <- function() {
  hygstars <- read.csv("https://github.com/astronexus/HYG-Database/blob/master/hygfull.csv?raw=true",
                       header=T, sep=",", stringsAsFactor=F)
  head(hygstars)

  require(graphics)
  data <- hygstars[hygstars$Mag < 1.8, ]
  
  #palette(terrain.colors(length(data$Mag), alpha=1)[magnitudes])
  hmcols<-colorRampPalette(c("red","white","blue"))(256)
  ramp <- colorRamp(c("red", "white", "blue"))
  cols=rgb( ramp(seq(0, 1, length=37)), max=255)
  mags <- round(Normalize(min(data$Mag), max(data$Mag), 1, 37, data$Mag))

  oldbg <- par()$bg
  par(bg="gray")
  oldmar <- par()$mar
  par(mar=c(7,4,3,1))
  data$ProperName[data$ProperName == ""] <- data$BayerFlamsteed[data$ProperName == ""]
  data$ProperName[data$ProperName == ""] <- data$Gliese[data$ProperName == ""]
  data$ColorIndex[which(is.na(data$ColorIndex))] <- mean(data$ColorIndex, na.rm=T)
  plot(data$Mag, col=cols[mags], pch=19, xaxt="n", main="HYG Catalog - Bright Stars",
       xlab="", ylab="Magnitude", cex.axis=.7, cex.lab=.7, cex=(data$Mag * -1) + 2.4, cex.main=1,
       ylim=c(min(data$Mag) - .55, max(data$Mag)), las=1)
  axis(1, at=1:length(data$ProperName), labels=data$ProperName, las=2, cex.axis=.6)
  #grid (NULL, NULL, lty=3, col = "cornsilk3")
  abline(v=seq(1,length(data$ProperName)), lty=3, col="cornsilk2")
  points(data$Mag, col=round(abs(data$ColorIndex * 3) + 2), pch=19, cex=(data$Mag * -1) + 2.4)
  par(mar=oldmar)
  par(bg=oldbg)
  
  # Using sqldf:
  # result <- sqldf("SELECT * FROM hygstars WHERE BayerFlamsteed LIKE '%Ori%'")
  result <- sqldf('SELECT Mag, BayerFlamsteed, ProperName FROM hygstars WHERE BayerFlamsteed LIKE \'%Ori%\'')

  starnames <- paste(result$BayerFlamsteed, result$ProperName)
  plot(result$Mag, xaxt="n", xlab="", pch=19, col="blue", main="Stars in Orion", cex.axis=.6)
  axis(1, at=1:length(result$Mag), labels=starnames, las=2, cex.axis=.6)
}

UsingGoogleVisCharts <- function() {
  # http://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
  package.install("googleVis")
  library(googleVis)
  #demo(googleVis)
  
  require(datasets)
  states <- data.frame(state.name, state.x77)
  GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                            options=list(region="US", 
                                         displayMode="regions", 
                                         resolution="provinces",
                                         width=600, height=400))
  plot(GeoStates) # NOTE: Is loaded in the default browser
  
  PopTable <- gvisTable(Population, 
                        formats=list(Population="#,###",
                                     '% of World Population'='#.#%'),
                        options=list(width='automatic', 
                                     height=300, 
                                     page='enable'))
  plot(PopTable)
}
