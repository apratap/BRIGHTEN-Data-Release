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
    temp <-as.data.frame(fread(path, stringsAsFactors = FALSE)) #read.table(INFO[[i]]$path, sep = "\t",  header = TRUE)
  }
  if(extension == "csv") {
    temp <- read.csv(path, header = TRUE, check.names = FALSE, stringsAsFactors = TRUE)
  }
  if(extension == "xls" | extension == "xlsx") {
    temp <- xlsx::read.xlsx(path, sheetIndex = 1, )
  }  
  if(!exists("temp")) {
    temp <- FALSE
  }
  return(temp)
}

