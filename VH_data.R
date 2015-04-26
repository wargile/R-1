# Playing with some VH data
col.classes <- c("factor","factor","factor","character","factor","factor","factor","character",
                "integer","character","character","character")
ach <- read.csv("C:/coding/R/TestData/VH_AlertCacheHistory.csv", sep=";", header=T,
                               colClasses=col.classes)

ach$LocationNumber <- as.character(ach$LocationNumber)
ach$LocationNumber[which(nchar(ach$LocationNumber) == 5)] <-
  paste0("0", ach$LocationNumber[which(nchar(ach$LocationNumber) == 5)])
ach$LocationNumber <- as.factor(ach$LocationNumber)

head(ach)
sapply(ach, class)

par(mar=c(5,5,3,1))
ggplot(data=ach, aes(y=PackageCount, x=formtypename, fill=LocationNumber)) +
  ylab("Package count") + xlab("Formtype name") +
  geom_bar(stat="identity") + theme_bw() + ggtitle("AlertCacheHistory") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

df <- aggregate(PackageCount ~ formtypename, data=ach, sum)
df
plot(df$PackageCount, col=2:(nrow(df)+1), type="h", lwd=5)
boxplot(ach$PackageCount ~ ach$formtypename, col=ach$formtypename)
