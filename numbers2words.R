numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  if(x==0){
    print( "Zero")
  } else{
    helper <- function(x){
      
      digits <- rev(strsplit(as.character(x), "")[[1]])
      nDigits <- length(digits)
      if (nDigits == 1) as.vector(ones[digits])
      else if (nDigits == 2)
        if (x <= 19) as.vector(teens[digits[1]])
      else trim(paste0(tens[digits[2]],"-",
                       ones2[digits[1]]))
                    #  Recall(as.numeric(digits[1]))))
      else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                        Recall(makeNumber(digits[2:1]))))
      else {
        nSuffix <- ((nDigits + 2) %/% 3) - 1
        if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
        trim(paste(Recall(makeNumber(digits[
          nDigits:(3*nSuffix + 1)])),
          suffixes[nSuffix],"," ,
          Recall(makeNumber(digits[(3*nSuffix):1]))))
      }
    }
    trim <- function(text){
      #Tidy leading/trailing whitespace, space before comma
      text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
      #Clear any trailing " and"
      text=gsub(" and$","",text)
      #Clear any trailing comma
      gsub("\ *,$","",text)
    }  
    makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
    #Disable scientific notation
    opts <- options(scipen=100) 
    on.exit(options(opts)) 
    ones <- c("", "One", "Two", "Three", "Four", "Five", "Six", "Seven",
              "Eight", "Nine") 
    names(ones) <- 0:9 
    teens <- c("Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen",
               "Sixteen", " Seventeen", "Eighteen", "Nineteen")
    names(teens) <- 0:9 
    tens <- c("Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty",
              "Ninety") 
    names(tens) <- 2:9 
    ones2 <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    names(ones2) <- 1:9
    x <- round(x)
    suffixes <- c("thousand", "million", "billion", "trillion")     
    if (length(x) > 1) return(trim(sapply(x, helper)))
    helper(x)    
  }
  
}