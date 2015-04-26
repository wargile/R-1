# General statistics stuff from "Statistikk Kort og Godt" book, etc.

fridges.sold <- c(0,1,2,3,4,5,6,7,8)
days.sold <- c(1,3,4,8,14,10,6,3,1)
max.sales.days <- sum(days.sold)
df <- data.frame(fridges.sold, days.sold)
df$p <- apply(df, 1, function(row) row[2]/max.sales.days)
df
sum(df$p)

bp <- ggplot(data=df, aes(factor(x=fridges.sold), y=days.sold, fill=days.sold)) + geom_histogram(stat="identity")
bp <- bp + xlab("Antall kjoleskap solgt i lopet av en dag") + ylab("Antall dager") +
  ggtitle("Statistikk Kort og Godt") +
  theme(plot.title=element_text(size = 18, colour="steelblue4", face="bold.italic")) +
  scale_x_discrete(breaks=MyQuantile(fridges.sold), labels=c("min", "lower", "median", "upper", "max"))
bp

##################

daily.oil.production <- c(515, 520, 518, 521, 524, 519, 519, 513, 516, 522,
                          517, 520, 520, 514, 521, 523, 515, 517, 519, 522,
                          518, 517, 520, 521, 518, 519, 518, 520, 519, 520,
                          521, 519, 521, 523, 522, 512, 517, 520, 516, 518,
                          514, 523, 515, 519, 522, 520, 520, 518, 521, 520)

df <- data.frame(table(daily.oil.production))
my.breaks <- MyQuantile(daily.oil.production)

bp <- ggplot(data=df, aes(factor(x=daily.oil.production), y=Freq, fill=Freq)) + geom_histogram(stat="identity")
bp <- bp + xlab("Daglig oljeproduksjon, antall fat") + ylab("Antall dager") +
  ggtitle("Statistikk Kort og Godt") +
  theme(plot.title=element_text(size = 18, colour="steelblue4", face="bold.italic")) +
  scale_x_discrete(breaks=my.breaks, labels=c(paste("min:", my.breaks[1]), paste("lower:", my.breaks[2]),
                                              paste("median:", my.breaks[3]), paste("upper:", my.breaks[4]),
                                              paste("max:", my.breaks[5])))
bp

######################
# Poisson distribution

MyDPoisson <- function(data) {
  # Customers in a bank, per hour
  bank.customers <- as.integer(sort(data)) # Must be sorted??
  #bank.customers <- as.integer(c(5,4,2,1,6,3,2,8,3,1,2,5,0,5,3,4)) # Must be sorted??

  #my.mean <- format(MyMean(bank.customers), digits=2)
  #my.variance <- format(MyVariance(bank.customers), digits=2)
  my.mean <- MyMean(bank.customers)
  my.variance <- MyVariance(bank.customers)

  cat("Mean:", my.mean, "\n")
  cat("Variance:", my.variance, "\n")
  # If mean = variance, the sample is probably poisson distributed

  r <- as.numeric(my.mean) # Important if my.mean is not numeric
  e <- exp(1) # Get Euler's constant: exp(1)
  a <- numeric(length(bank.customers))
  b <- numeric(length(bank.customers))

  #for (counter in 1:length(bank.customers)) {
  #  x <- as.integer(bank.customers[counter])
  #  a[counter] <- ((poisson^x) / factorial(bank.customers[counter])) * (e^(-poisson))
  #}

  #a

  pos = 1
  #p(k) = r*k / (k!)(e*r) # r = rate, k = number to check probability for
  
  for (counter in 1:length(bank.customers)) {
    k <- as.integer(bank.customers[counter])
    temp <- (r * k) / (factorial(k) * (e * r))
    
    if (!is.nan(temp)) {
      b[pos] <- temp
      pos = pos + 1
    }
  }

  cat(rev(sort(b)))
  barplot(rev(sort(b)), col=heat.colors(length(data)))
  pos = 1
  
  # http://en.wikipedia.org/wiki/Poisson_distribution
  for (counter in 1:length(bank.customers)) {
    k <- as.integer(bank.customers[counter])
    temp <- (r^k * e^(-r)) / factorial(k)
    
    if (!is.nan(temp)) {
      b[pos] <- temp
      pos = pos + 1
    }
  }  

  #cat(rev(sort(b)))
  #barplot(rev(sort(b)), col=heat.colors(length(b)))

}

# Create a normal distribution bell curve
MyNormDist <- function(min.dist=-pi, max.dist=pi) {
  e <- exp(1)
  x <- seq(min.dist, max.dist, abs(max.dist - min.dist) / 100)
  #x <- seq(-100, 0, 1)
  
  norm.dist <- (1/sqrt(2*pi))*(e^(-x^2/2))
  my.mean <- round(mean(norm.dist), 5)
  my.sd <- round(sd(norm.dist), 2)
  title <- bquote(paste("Normal distribution from " -pi, " to ", pi, " (", bar(X) == .(my.mean), " ", sigma == .(my.sd), ")"))
  
  plot(norm.dist, col="blue",
       #main=expression("Normal distribution from " * -pi * " to " * pi),
       main=title,
       xlab="x", ylab="y")
  lines(norm.dist, col="red")
  grid()
}

# Prove that sum of probabilities is always 1
MyProbability <- function(data) {
  data.table <- table(data)
  totprob <- 0
  
  for (i in 1:length(data.table)) {
    totprob <- totprob + (data.table[[i]] / length(data))
  }
  
  return(totprob)
}

# Show correlation in non-linear (quadratic in this case) relationship
CorrelationInNonLinearRelationship <- function() {
  par(mar=c(4.5,4,2,1))
  temperature.us <- c(1, 5, 9, 15, 20, 25, 30)
  energy.in.kW.us <- c(110, 80, 60, 40, 60, 70, 90) # Goes up again (AC usage increases) as temperature increases
  
  temperature.nor <- c(1, 5, 9, 15, 20, 25, 30)
  energy.in.kW.nor <- c(110, 80, 60, 40, 20, 10, 1) # Goes down (no AC!) as temperature increases

  par(mfrow=c(2,1))
  corr <- round(cor(energy.in.kW.us, temperature.us), 2)
  title <- bquote(paste("US - Energy use versus temperature (Cor: ", .(corr), ")"))
  plot(energy.in.kW.us ~ temperature.us, col="red", main=title, pch=19)
  abline(lm(energy.in.kW.us ~ temperature.us), col="blue")
  #grid()
  
  corr <- round(cor(energy.in.kW.nor, temperature.nor), 2)
  title <- bquote(paste("Norway - Energy use versus temperature (Cor: ", .(corr), ")"))
  plot(energy.in.kW.nor ~ temperature.nor, col="red", main=title, pch=19)
  abline(lm(energy.in.kW.nor ~ temperature.nor), col="blue")
  #grid()  
  
  par(mfrow=c(1,1))
}
