#' @title Audit of a dataframe
#'
#' @description
#' This \code{audit} function is used to assess a data frame. It will return the percentage of values that are missing, where the
#' percentage is dependent on user input. It also returns information on the outlier of the categorcial and nuemric data.
#'
#' @details Once given a dataframe it will return an audit that describes the data frame.
#'
#' @import dplyr
#'
#' @param data a dataframe
#' @param catCut minimum percentage of values in a category in order to not be an outlier
#' @param levels_numeric minimum number of unique values that a numeric variable should have
#' @param levels_categorical maximum number of categories that a categorical variable should have
#' @param row_percent the upper bound percent value for missing values in a row
#' @param col_percent the upper bound percent value for missing values in a column
#'
#' @return a message in the console that list an analysis on the missing values of the data, the categorical and numeric variables and the outliers
#' based on these datatypes
#'
#' @examples
#' audit(mtcars)
#' audit(airquality)
#'
#'
#'
#' @export
#'

audit <- function(data, catCut = 0.05, levels_numeric = 4, levels_categorical = 5,
                  row_percent = 35, col_percent = 20) {


 if(!is.numeric(row_percent)){
  stop("The row_percent should be a numeric value")
 }

 if(!is.numeric(col_percent)){
  stop("The col_percent should be a numeric value")
 }

 if(!is.data.frame(data)){
  stop("Your data input should be a data frame")
 }

 #Descriptive stats
 cat("Descripttion of Dataframe\n")

 cat("\nNumber of Observations:", nrow(dplyr::distinct(data)),"\n")

 cat("\nVariable Names:", colnames(data), "\n")


 #Column name with too many missing values
 col_length = nrow(data)
 missValCountCol = colSums(is.na(data))
 miss_percentCol = as.data.frame((missValCountCol/col_length)*100)
 excess_missCol0 = row.names(miss_percentCol)[which(miss_percentCol$`(missValCountCol/col_length)` >= col_percent)]
 #as.list(excess_missCol)
 #print(excess_missCol)
 excess_missCol<- paste(as.list(excess_missCol0), collapse= " ")
 # print(length(excess_missCol))

 #Row name with too many missing values
 row_length = ncol(data)
 missValCountRow = rowSums(is.na(data))
 miss_percentRow = as.data.frame((missValCountRow/row_length)*100)
 excess_missRow0 = as.numeric(row.names(miss_percentRow)[which(miss_percentRow$`(missValCountRow/row_length)` >= row_percent)])
 excess_missRow<- paste(as.list(excess_missRow0), collapse= " ")


 #a <- data.frame(data)


 if(length(excess_missCol)!= 0){
  text1<- paste0("There are more than, ", col_percent, "% missing values in the column/s: ", excess_missCol, ". ")
 }

 if(length(excess_missRow != 0)){
  text2<- paste0("There are more than, ", row_percent,"% missing values in the row/s: ", excess_missRow,". ")
 }
 cat("\nRunning Missing Values Analysis...\n")


 if((excess_missCol != "") & (excess_missRow != "")){
  missValtext = cat(text1, text2,"\n")
 }
 else if((excess_missCol != "") & (excess_missRow == "")){
  missValtext = cat(text1,"\n")
 }
 else if((excess_missCol == "") & (excess_missRow != "")){
  missValtext = cat(text2,"\n")
 }
 else missValtext = cat("\nMissing Values Analysis passed with no Warnings\n")


  #Typecheck

 cat("\nRunning Type Check Analysis...\n")

 library(dplyr)
 cols <- colnames(data)
 errors <- FALSE
 for(i in cols){
  if(is.numeric(data[[i]])){
   if (n_distinct(data[[i]]) < levels_numeric){
    print(paste("The variable", i, "has very few unique values. We suggest turing this variable into", n_distinct(data[[i]]) ,"factor/s."))
    errors <- TRUE
   }
  }
  if(!is.numeric(data[[i]])){
   if (n_distinct(data[[i]]) > levels_categorical){
    print(paste("The variable", i, "has more than", levels_categorical, "levels. You may want to consider combining categories or subsetting."))
    errors <- TRUE
   }
  }
 }
 if (errors == FALSE){
  print("Type Check Analysis passed with no Warnings")
 }


 #Outliers
 cat("\nRunning Outliers Analysis...\n")
 numStr <- NULL
 catStr <- NULL
 vars <- ls(data)
 for (var in vars){
  vals <- data[[var]]
  newStr <- ""
  # should integer be included?
  if (class(vals) == "numeric"){
   sd = sd(vals)
   mean = mean(vals)
   count = 0
   for (v in vals){
    if (v < mean - 3*sd | v > mean + 3*sd){
     count = count + 1
    }
   }
   if (count != 0){
    if (count > 1){
     newStr <- paste(count,"outliers in",var, sep = " ")
    } else {
     newStr <- paste(count,"outlier in", var, sep = " ")
    }
    numStr <- paste(numStr,newStr, sep = "\n")
   }

  } else if(class(vals) %in% c("factor","character")){
   propTable <- prop.table(table(vals))
   for (level in unique(vals)){
    if (propTable[level] < catCut){
     newStr <- paste(level, "in",var, sep = " ")
     catStr <- paste(catStr, newStr, sep = "\n")
    }
   }
  }
 }

 cat("Numeric variables that have outliers:",
     numStr,
     "\nCategorical variables that have outliers:",
     catStr,
     "\nExamine these variables and decide whether they should be excluded in further analysis."
 )


}



