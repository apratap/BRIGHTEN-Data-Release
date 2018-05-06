require(knitr) #command line option in compiling notebook
require(tools)
require(xlsx) #read xmls files
require(data.table) 
require(reticulate) #Python integration into R studio
require(stringr) #parse strings

`%!in%` <- Negate('%in%')

#PRE string identifying volume label content, and synapse file description object (can be list)
#POST data frame describing the variables in each file named in the synapse object.
simple.content.listing <- function(VOL, INFO) {
  output <- data.frame(volume = character(),
                       synID = character(),
                       path = character(),
                       fileName = character(),
                       variables = character(),
                       stringsAsFactors = FALSE)
  #IN 
  `%!in%` <- Negate('%in%')
  for(i in 1:(length(INFO))) {
    temp <- find.data(INFO[[i]]$path)
    if(is.na(temp)) {
      temp <- "file read error"
    }
    
    output[i,] <- c(VOL,
                    INFO[[i]]$properties$id,
                    INFO[[i]]$path,
                    INFO[[i]]$files,
                    paste(names(temp), collapse = ", "))
  }
  return(output)
}


#PRE a path to a file of either tsv csv or xls
#post a data frame containin the contents of that file, or NA if not found
find.data <- function(path) {
  extension <- tools::file_ext(path)
  if(extension == "tsv"| extension == "txt") {
    temp <-as.data.frame(fread(path, stringsAsFactors = TRUE, check.names = FALSE)) #read.table(INFO[[i]]$path, sep = "\t",  header = TRUE)
  }
  if(extension == "csv") {
    temp <- read.csv(path, header = TRUE, stringsAsFactors = TRUE, check.names = FALSE)
  }
  if(extension == "xls" | extension == "xlsx") {
    temp <- xlsx::read.xlsx(path, sheetIndex = 1 , check.names = FALSE)
  }  
  if(!exists("temp")) {
    temp <- FALSE
  }
  return(temp)
}


#PRE: path to a file
#POST: returns a data frame with decsriptions of each variable in the file
build.wiki.data.description <- function(path) {

  synoData <- find.data(path)
  
  #generate descriptions of each variable's data type
  classes <- sapply(synoData,class)
  for (j in 1:length(classes)) {
    classes[j] <- classes[[j]][1]
    #Change "POSIXct" to "date" in data type listing
    #if (classes[j] == "POSIXct") {
    #  classes[j] <- "Date"
    #}
  }
  
  #create  data frame with each variable's descriptors
  wiki <- data.frame("#" = 1:length(synoData),
                     "Variable Name" = names(synoData),
                     "Data Type" = unlist(classes),
                     check.names = FALSE)
  
  wiki$Range <- NA
  for( wikiRows in 1:nrow(wiki)) {
    #print(paste("stepB",wikiRows, sep = " "))
    if (!all(is.na(synoData[,wikiRows]))) {
      if (wiki[wikiRows,]$"Data Type" == "factor") {
        if (length(levels(synoData[,wikiRows])) > 5 ) {
          x <- length(levels(synoData[,wikiRows]))
          wiki[wikiRows,]$"Range" <- paste(as.character(levels(synoData[,wikiRows])[c(1,2,median(1:x),x-1,x)]), collapse = ", ") 
        }
        if (length(levels(synoData[,wikiRows])) <= 5 ) {
          wiki[wikiRows,]$"Range" <- paste(as.character(levels(synoData[,wikiRows])), collapse = ", ") 
        }
      }
      if (wiki[wikiRows,]$"Data Type" != "factor") {
        wiki[wikiRows,]$"Range" <- paste(as.character(range(synoData[,wikiRows], na.rm = TRUE)),sep =  " - ", collapse = " - ")
      }
    }
    if (all(is.na(synoData[,wikiRows]))) { wiki[wikiRows,]$Range <- "NA" }
  }
  
  wiki$Description <- NA
  return(wiki)
    
}

add.match <- function(MasterList, V1SynID = NA, V2SynID = NA, V1FileName = NA, V1Path = NA, V2Path = NA, V2FileName = NA, V1Variables = NA, V2Variables = NA){
  if(is.na(V1SynID) & !is.na(V1FileName)) {
    V1SynID <- MasterList[MasterList$fileName == V1FileName,]$synID
  }
  if(is.na(V2SynID) & !is.na(V2FileName)) {
    V2SynID <- MasterList[MasterList$fileName == V2FileName,]$synID  
  }
  if(is.na(V1FileName) & !is.na(V1SynID)) {
    V1FileName <- MasterList[MasterList$synID == V1SynID,]$fileName
  }
  if(is.na(V2FileName) & !is.na(V2SynID)) {
    V2FileName <- MasterList[MasterList$synID == V2SynID,]$fileName
  }
  if(!is.na(V1SynID)) {
    V1Variables <- MasterList[MasterList$synID == V1SynID,]$variables
    V1Path <- MasterList[MasterList$synID == V1SynID,]$path
  }
  if(!is.na(V2SynID)) {
    V2Variables <- MasterList[MasterList$synID == V2SynID,]$variables
    V2Path <- MasterList[MasterList$synID == V2SynID,]$path
  }
  return(c(V1SynID, V1FileName, V1Path, V1Variables, V2SynID, V2FileName, V2Path, V2Variables ))
}

