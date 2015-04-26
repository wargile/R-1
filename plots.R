par(mfrow=c(1, 1))

x <- rnorm(100)
y <- x + rnorm(100)
g <- gl(2, 50, labels=c("Male", "Female"))

plot(x, y, type="n") # Blank plot

points(x[g == "Male"], y[g == "Male"], col="blue", pch=18) # Add points
points(x[g == "Female"], y[g == "Female"], col="Red", pch=19)

par(bg="cornsilk")

# Lattice plots
package ? lattice
library(help = lattice)

x <- rnorm(20)
y <- x + rnorm(20, sd=0.5)
f <- gl(4, 5, labels=c("Group 1", "Group 2", "Group 3", "Group 4"))

#xyplot(y ~ x | f, col=c("Red", "Green"))
xyplot(y ~ x | f,
       panel=function(x, y, ...) {
         panel.xyplot(x, y, col="blue", pch=19, width=3, ...) # Always call xyplot default first
         panel.abline(h=median(y), lty=2, col="green")
         panel.lmline(x, y, col=5) # Regression line
       })

# Plot the built-in environmental dataset
data(environmental)
summary(environmental)

temp.cut <- equal.count(environmental$temperature, 4)
wind.cut <- equal.count(environmental$wind, 4)

xyplot(ozone ~ radiation | temp.cut * wind.cut, data=environmental,
       layout=c(4, 4), as.table=F, # as.table: Switch bottom/top 
       panel=function(x, y, ...) {
         panel.xyplot(x, y, col="blue", pch=19, width=2, ...) # Default
         panel.loess(x, y)
         fit <- lm(y ~ x)
         panel.abline(fit, col="cyan")
       },
       xlab="Radiation", ylab="Ozone", main="Ozone vs. Solar Radiation")

splom(~ environmental)
histogram(~ temperature | temp.cut * wind.cut, data=environmental)

# qplot
qplot(displ, hwy, data=mpg, color=drv # mpg dataset that comes with ggplot2
qplot(displ, hwy, data=mpg, geom=c("point", "smooth"), method="loess") # mpg dataset that comes with ggplot2
qplot(hwy, data=mpg, geom="density", color=drv) # mpg dataset that comes with ggplot2
qplot(hwy, data=mpg, fill=drv)
qplot(hwy, data=mpg, fill=drv, facets=drv~., binwidth=2) # drv~. : . = no col indication, 3 drv rows
qplot(displ, hwy, data=mpg, color=drv, facets=.~drv) # .~drv : . = no row indication, 3 drv cols

# Math annotation
?plotmath
      
# Create a scatterplot with correlation between two measurements, to check Reliability:
par(bg="white")
#par(mfrow=c(2,1))
temperaturesS1 <- c(37, 37.1, 37.3, 38.1, 37.1, 39.3)
temperaturesS2 <- c(37.1, 37.2, 37.2, 38.2, 37.2, 39.1)
plot(temperaturesS1, temperaturesS2, col="blue",
     main=paste0("Temperatures (cor=", round(cor(temperaturesS1, temperaturesS2), 2), ")"))
fit <- lm(temperaturesS2 ~ temperaturesS1)
abline(fit, col="green")
#loess(temperaturesS1, temperaturesS2)
grid()


# Testing some plots on the VH AlertCache data....
op <- par()

col.classes <- c("factor","factor","factor","character","factor","factor","factor","factor",
                 "integer","character","factor","factor")
df <- read.csv("c:/coding/R/TestData/VH_AlertCacheHistory.csv", sep=";", header=T, colClasses=col.classes)
head(df)
sapply(df, class)
names(df)
#df <- df[, c(1,3,5,7,9)]
ggplot(data=df, aes(x=LocationNumber, y=PackageCount, fill=formtypename)) +
  geom_bar(stat="identity") + ggtitle("")
# ggplot handles df directly identically to aggregate below
parcels.by.location = aggregate(PackageCount ~ formtypename + LocationNumber, df, sum)

ggplot(data=parcels.by.location, aes(x=LocationNumber, y=PackageCount, fill=formtypename)) +
  geom_bar(stat="identity") + ggtitle("Parcel count by location and formtype")
ggplot(data=parcels.by.location, aes(x=LocationNumber, y=PackageCount, fill=formtypename)) +
  geom_bar(position="dodge", stat="identity") + scale_fill_brewer() +
  ggtitle("Parcel count by location and formtype") + theme_bw()

par <- op
