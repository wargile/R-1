# Impute with mice

datafolder <- "C:/coding/Kaggle/TheAnalyticsEdge_2015/Data/"
submissionfolder <- "C:/coding/Kaggle/TheAnalyticsEdge_2015/Submissions/"

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train2 <- read.csv(paste0(datafolder, "train.csv"), header=T, stringsAsFactors=F)
  test2 <- read.csv(paste0(datafolder, "test.csv"), header=T, stringsAsFactors=F)
}

data <- data.frame(x=c("Balle",NA,"Skalle","Kalle",NA,NA,"Falle"), y=c("Raga","Paga","Paga","Faga","Paga","Faga","Kaga"))
sapply(data, class)
complete(mice(data))
data

train2 <- rbind(train2[,c(-9,-10)], test2[,-9])

train2$NewsDesk[train2$NewsDesk == ""] <- NA
train2$SectionName[train2$SectionName == ""] <- "Unknown"
train2$SubsectionName[train2$SubsectionName == ""] <- "Unknown"

data2 <- data.frame(NewsDesk=train2$NewsDesk[1:500], SectionName=train2$SectionName[1:500],
                    SubsectionName=train2$SubsectionName[1:500])
data2 <- data.frame(NewsDesk=train2$NewsDesk, SectionName=train2$SectionName,
                    SubsectionName=train2$SubsectionName)
sapply(data2, class)
result <- complete(mice(data2))

result[1:20,]
train2[1:20,1:3]

row <- sample(1:(nrow(train2)-20), 1)
result[row:(row+20),]
train2[row:(row+20),1:3]

table(result$NewsDesk)
table(train2$NewsDesk)

ImputeWithMICE <- function() {
  if (file.exists(paste0(datafolder, "train.rda")) == F) {
    train2 <- read.csv(paste0(datafolder, "train.csv"), header=T, stringsAsFactors=F)
    test2 <- read.csv(paste0(datafolder, "test.csv"), header=T, stringsAsFactors=F)
  }

  train2 <- rbind(train2[,c(-9,-10)], test2[,-9])
  train2$NewsDesk[train2$NewsDesk == ""] <- NA
  train2$SectionName[train2$SectionName == ""] <- "Unknown"
  train2$SubsectionName[train2$SubsectionName == ""] <- "Unknown"
  data2 <- data.frame(NewsDesk=train2$NewsDesk, SectionName=train2$SectionName, SubsectionName=train2$SubsectionName)
  result <- complete(mice(data2))
  return (result)  
}

imputed <- ImputeWithMICE()
train.imputed <- head(imputed, nrow(train2))
test.imputed <- tail(imputed, nrow(test2))
dim(train.imputed)
dim(train2)
dim(test.imputed)
dim(test2)
