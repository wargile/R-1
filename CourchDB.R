# Using R to access CourchDB JSON info...

library(RJSONIO)
library(RCurl)

#dbpath <- "http://127.0.0.1:5984/terje"
dbpath <- "http://127.0.0.1:5984/terje2"

httpPUT(dbpath)  # Creates the terje database

# Example: Add a document
fromJSON(getURL(dbpath, customrequest='POST',
                httpheader=c('Content-Type'='application/json'), postfields=toJSON(list(day="Monday", dinner="Pasta"))))

view.results <- fromJSON(httpGET(paste0(dbpath, "/_design/cars/_view/all")), simplifyWithNames=T)
# TIP (not working): do.call(rbind.data.frame, view.results)
df <- data.frame(matrix(unlist(view.results$rows), nrow=length(view.results$rows), byrow=T))
colnames(df)[5:8] <- c("Make", "Model", "Year", "Price")
df[, c("Make", "Model", "Year", "Price")]

sapply(view.results$rows, "[[", 3)[5, 1:4]
a <- unlist(view.results)
view.results.df <- data.frame(model=a[which(names(a) == "rows.value.model")],
                              make=a[which(names(a) == "rows.value.make")],
                              year=a[which(names(a) == "rows.value.year")],
                              price=a[which(names(a) == "rows.value.price")])
view.results.df$price <- as.numeric(as.character(view.results.df$price))
view.results.df
plot(price ~ make, data=view.results.df, col="orange", main="Price by make",
     ylab="Price", xlab="Make", cex.axis=.8, cex.lab=.8, cex.main=1)

# Get total sum
view.results <- fromJSON(httpGET(paste0(dbpath, "/_design/cars/_view/total_price")))
total.sum=view.results$rows[[1]]$value
total.sum
# Get sum by make
view.results <- fromJSON(httpGET(paste0(dbpath, "/_design/cars/_view/total_price?group=true")))
view.results.df <- do.call(rbind.data.frame, view.results$rows)
colnames(view.results.df) <- c("make", "sum")
view.results.df
# Get by make, return for make=Toyota
view.results <- fromJSON(httpGET(paste0(dbpath, "/_design/cars/_view/by_make?key=\"Toyota\"")))
length(view.results)
a <- unlist(view.results)
view.results.df <- data.frame(model=a[which(names(a) == "rows.value.model")],
                              year=a[which(names(a) == "rows.value.year")],
                              price=a[which(names(a) == "rows.value.price")])
view.results.df

view.results <- fromJSON(httpGET(paste0(dbpath, "/_design/cars/_view/by_make?keys=[\"Toyota\",\"Ford\"]")))
a <- unlist(view.results)
view.results.df <- data.frame(model=a[which(names(a) == "rows.value.model")],
                              year=a[which(names(a) == "rows.value.year")],
                              price=a[which(names(a) == "rows.value.price")])
view.results.df

# Using jsonlite library to do the same:
package.install("jsonlite")
library(jsonlite)
#mydata <- fromJSON("http://projects.propublica.org/forensics/geos.json")
#View(mydata$geo)
view.results <- fromJSON(httpGET(paste0(dbpath, "/_design/cars/_view/by_make?key=\"Toyota\"")))
names(view.results$rows$value)
view.results$rows$value$make
