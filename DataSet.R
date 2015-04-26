# Create DataSet XML

CreateDataSet <- function(foldername, filename, filename.xml, filename.defvalues, id.fields,
                          parent.level, genericColNames=F) {
  data <- read.csv(paste0(foldername, filename), header=T, sep="\t", stringsAsFactor=F)
  head(data)
  dim(data)
  sapply(data, class)
  # Get level 1:
  data[data$Level == 3, ]
  
  data.defvalues <- read.csv(paste0(foldername, filename.defvalues), header=F, sep=";", stringsAsFactor=F)
  head(data.defvalues)
  standard.values <- strsplit(data.defvalues$V2[1], "|", fixed=T)
  standard.values <- strsplit(data.defvalues$V2[1], "\\|") # Escape special char, because regexp here
  standard.values[[1]][1]
  
  # Create XML
  library(XML)
  # See: xmlTree, xmlOutputBuffer, xmlOutputDOM
  
  ########################################################################################
  
  # Standardize data types:
  data$Type[data$Type == "Int"] <- "int"
  data$Type[data$Type == "Smallint"] <- "int"
  data$Type[data$Type == "Char"] <- "string"
  data$Type[data$Type == "Date"] <- "string"
  data$Type[data$Type == "Time"] <- "string"
  data$Type[data$Type == "Varchar"] <- "string"
  
  result.xml.total <- ""
  levels <- length(id.fields)
  multiplier <- 4
  id.values <- matrix(nrow=levels, ncol=(2^(levels + 1)))
  
  node.dataset <- newXMLNode("dataset", attrs=c(levels=levels))
  
  for (level.counter in 1:levels) {
    node.recordset <- newXMLNode("recordset", parent=node.dataset, attrs=c(level=level.counter))
  
    for (recordset.counter in 1:(2^(level.counter + 1))) {
      node.row <- newXMLNode("row", parent=node.recordset)
      
      fielddata <- data[data$Level == level.counter, ]
    
      for (counter in 1:nrow(fielddata)) {
          lookup <- data.defvalues[data.defvalues$V1 == fielddata$Name[counter], ]
          
          if (nrow(lookup) > 0) {
            standard.values <- strsplit(lookup$V2, "|", fixed=T)
            index <- sample(length(standard.values), 1)
            lookup <- standard.values[[1]][index]
            
            if (genericColNames == F) {
              newXMLNode("col", lookup, parent=node.row,
                         attrs=c(name=toupper(fielddata$Name[counter]),
                                 length=fielddata$Length[counter],
                                 datatype=fielddata$Type[counter]))
            } else {
              newXMLNode("col", lookup, parent=node.row,
                         attrs=c(name=paste0("Column", counter),
                                 length=fielddata$Length[counter],
                                 datatype=fielddata$Type[counter]))              
            }
            
          } else {
            field.value <- paste0(fielddata$Name[counter], recordset.counter)
  
            if (level.counter > 1)
              if (fielddata$Name[counter] == id.fields[level.counter])
                field.value <- id.values[parent.level[level.counter],
                                         floor((recordset.counter+1)/2)] # TODO: always correct split here?
  
            if (fielddata$Name[counter] == id.fields[level.counter]) {
              id.values[level.counter, recordset.counter] <- field.value
            }
            
            
            if (genericColNames == F) {
              newXMLNode("col", field.value, parent=node.row,
                         attrs=c(name=toupper(fielddata$Name[counter]),
                                 length=fielddata$Length[counter],
                                 datatype=fielddata$Type[counter]))
            } else {
              newXMLNode("col", field.value, parent=node.row,
                         attrs=c(name=paste0("Column", counter),
                                 length=fielddata$Length[counter],
                                 datatype=fielddata$Type[counter]))              
            }
            
          }
      }
    }
  }
  
  result.xml.total <- saveXML(node.dataset)
  
  # Write XML to output file:
  # result <- paste0("<!--Test...-->\n", result.xml.total)
  result <- result.xml.total
  outfile <- file(paste0(foldername, filename.xml), encoding="UTF-8")
  write(x=result, file=outfile)
  close(outfile)
}

#####################################################################################################

foldername <- "c:/coding/ruby/"
filename.defvalues <- "LAJA0250_defvalues.txt"

# Create XML for LAJA0250:
filename <- "LAJA0250.txt"
filename.xml <- "LAJA0250_out_R.xml"
id.fields <- c("Sendingnr", "Sendingnr", "Sendingnr")
parent.level <- c(0,1,2) # The parent level for current recordset level
CreateDataSet(foldername, filename, filename.xml, filename.defvalues, id.fields, parent.level)

# Create XML for LAJA0251:
filename <- "LAJA0251.txt"
filename.xml <- "LAJA0251_out_R.xml"
id.fields <- c("Sendingnr", "Sendingnr", "Sendingnr", "Sendingnr")
parent.level <- c(0,1,1,2) # The parent level for current recordset level
CreateDataSet(foldername, filename, filename.xml, filename.defvalues, id.fields, parent.level)

# Create XML for LAJA0253:
filename <- "LAJA0253.txt"
filename.xml <- "LAJA0253_out_R.xml"
id.fields <- c("Sendingnr", "Sendingnr", "Sendingnr", "Sendingnr")
parent.level <- c(0,1,1,2) # The parent level for current recordset level
CreateDataSet(foldername, filename, filename.xml, filename.defvalues, id.fields, parent.level)

# Create XML for LAJA0257:
filename <- "LAJA0257.txt"
filename.xml <- "LAJA0257_out_R.xml"
id.fields <- c("Sendingnr", "Sendingnr", "Sendingnr")
parent.level <- c(0,1,2) # The parent level for current recordset level
CreateDataSet(foldername, filename, filename.xml, filename.defvalues, id.fields, parent.level, T) # Generic col names
