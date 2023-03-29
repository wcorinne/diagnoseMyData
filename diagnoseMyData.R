library(tidyverse)
library(glue)


checkEmpty <- function(vector, threshold, label) {
  ### Check for empty vector
  empty_val=sum(vector=="", na.rm=TRUE)/length(vector)
  is_empty=empty_val>1-threshold
  if(is_empty) {
    print(glue("-- {label} is {(empty_val)*100}% missing data"))
  }
}

checkNA <- function(vector, threshold, label) {
  ### Check for NA vector
  NA_val=sum(is.na(vector))/length(vector)
  is_NA=NA_val>1-threshold
  if(is_NA) {
    print(glue("-- {label} is {(NA_val)*100}% missing data"))
  }
}

checkZero <- function(vector, threshold, label) {
  ### Check for zero vectors
  zero_val=sum(vector==0, na.rm=TRUE)/length(vector)
  is_zero=zero_val>1-threshold
  if(is_zero) {
    print(glue("-- {label} is {(zero_val)*100}% zeroes"))
  }
}

checkSpecialChara <- function(vector, label, check_nonstandard, check_regex=TRUE, check_punctuation=TRUE, check_delimiter=TRUE) {
  
  if (check_nonstandard) {
    special_characters="[^A-Za-z0-9]"
  } else if (check_regex) {
    special_characters="[\\.\\?\\[\\+\\*\\^\\$\\{\\}\\(\\)\\|]|\\]"
  } else if (check_delimiter) {
    special_characters="[,;\t]"
  }
  # else assume the person has passed in a pattern to match
  
  ### Check for special characters
  has_sc = sum(grep(special_characters, vector), na.rm=TRUE)>0
  
  if(has_sc) {
    entry = grep(special_characters, vector)
    value = grep(special_characters, vector, value=TRUE)
    print(glue("-- {label} has special characters in entry {entry}: \"{value}\""))
  }
}

### Check for custom condition
checkCustom <- function(vector, custom_function, label) {
  ### Check custom function condition
  # check that custom_function is actually a function
  if(typeof(custom_function)!="closure") { # if it isn't a function
    tmp=custom_function
    custom_function=tryCatch(match.fun(custom_function), #check if they've passed a base-R function
                             error = function(e) {
                               message(print(glue("** The custom_function parameter passed, {toString(custom_function)}, is not a function! To use checkCustom, please provide a function that takes a VECTOR INPUT, and produces a single LOGICAL (TRUE/FALSE) OUTPUT")))
                               return()
                             })
    if(is.null(custom_function)) {return(message())}
  }
  
  # check if our vector satisfies the custom_function condition
  is_customCondition = tryCatch(custom_function(vector),
                                error = function(e) {
                                  message(glue("** Custom function failed on {label}:\n"))
                                  message(e)
                                  return()
                                })
  if(is.null(is_customCondition)) {return(message())}
  
  # check that custom_function outputted a single output
  if (length(is_customCondition)>1) {
    return(print(glue("** Custom function failed on {label}: Function output was {toString(is_customCondition)}. Please provide a function that produces a single output, when applied to a vector.")))
  } else
  # check that custom_function outputted a logical output
    if(!is.logical(is_customCondition)) {
    return(print(glue("** Custom function failed on {label}: Function output was {is_customCondition}. Please provide a function that produces a logical (TRUE/FALSE) output, when applied to a vector")))
      } 
    
  # print whether the custom_condition was met
  if(is_customCondition) {
    print(glue("-- {label} meets custom-condition."))
  }
}



### pass dataset OR pass file-path

### Sample Annotation -- capitalization / string issues 


diagnoseMyData <- function(data, save_output=TRUE, save_output_label="diagnoseMyData", print_output=TRUE, ### CURRENTLY false does not work
                          threshold=0.1, exclude_data=NULL, #excude data entirely, as opposed to skipping for row/column
                          check_cols=TRUE, skip_cols=c(),
                          check_rows=TRUE, skip_rows=c(),
                          gct_check_rdesc=TRUE, gct_check_cdesc=TRUE, gct_check_matrix=TRUE,
                          check_NA=TRUE, check_empty=TRUE, check_zero=TRUE, check_specialchara=TRUE, check_custom=NULL) {
  
  ### Check if check_custom is a valid function
  if (!is.null(check_custom)) {print("trying check_custom, hope you provided a good funciton!")}
  
  datasets=list(data) # initialize datasets with data
  ### Check if we've been passed a file-path instead of a file
  check_file = tryCatch(if(is.character(data)) {file.exists(data)})
  if(!is.null(check_file)) {
    if(check_file) {
      extension=str_extract(data, "\\.[A-z]+?$")
      if (extension==".csv") {
        print(glue("Detected {extension} file."))
        data=read.csv(data)
        datasets=list(data)
      } else if (extension==".tsv") {
        print(glue("Detected {extension} file."))
        data=read.delim(data, sep="\t")
        datasets=list(data)
      } else if (extension==".gct") {
        print(glue("Detected {extension} file."))
        library(cmapR)
        data=parse_gctx(data)
        datasets=list(data@rdesc, data@cdesc, data@mat)
      } else {
        stop(glue("File extension {extension} not compatible for auto-loading. Ask C. Williams to fix this, or load in the dataset yourself."))
      }
    } else {
      stop(glue("The inputted string, {data}, does not appear to be a file that exists. Upload a valid filepath, or upload a dataset object instead."))
    }
  }
  
  ### open output file connection (if save_output is on)
  if (save_output) {
    file=glue("{save_output_label}_{format(Sys.time(),'%Y%m%d_%H%M')}.log")
    sink(file(file), append=TRUE)
    # sink(con, append=TRUE, type="message")
  }
  
  
  ### Check class of data
  print(paste0("This dataset is a ", class(data), "."))
  
  for (dataset in datasets) {
    ### Check dimensions
    print(paste("It has", dim(dataset)[1], "rows and", dim(dataset)[2], "columns."))
    
    print("Problems will be flagged with --")
    
    ### Columns
    if (check_cols) for (i in setdiff(1:dim(dataset)[2], skip_cols)) {
      column=dataset[,i] #pull column as a vector
      n=names(dataset)[i] #check if it has a name
      t=typeof(dataset[,i])
      label = if (!is.null(n)) { glue("Column {i}, {n},") } else { glue("Column {i}") }
      
      # datatype
      print(glue("{label} contains {t} data."))
      
      
      if (check_NA) checkNA(column, threshold, label)
      if (check_empty) checkEmpty(column, threshold, label)
      if (check_zero) checkZero(column, threshold, label)
      if (t=="character" && check_specialchara) checkSpecialChara(column, label) #only check special chara if datatype is character
      if (!is.null(check_custom)) checkCustom(column, check_custom, label)
    }
    ### Rows
    if (check_rows) for (j in setdiff(1:dim(dataset)[1], skip_rows)) {
      row=dataset[j,] #pull row as a vector
      n=rownames(dataset)[j] #check if it has a name
      label = if (!is.null(n)) { glue("Row {j}, {n},") } else { glue("Row {i}") }
      
      if (check_NA) checkNA(row, threshold, label)
      if (check_empty) checkEmpty(row, threshold, label)
      if (check_zero) checkZero(row, threshold, label)
      if (check_specialchara) checkSpecialChara(row, label)
      if (!is.null(check_custom)) checkCustom(column, check_custom, label)
    }
    
    
    ### Missing data in EVERY row
  }
  
  
  if (save_output) {
    for(i in seq_len(sink.number())){ # close all sink connections
      sink(NULL)
    }
    if (print_output) {
      cat(readLines(file), sep="\n") #print file
    }
  }
}

x=matrix(c(1,2,45,6), 2,2)
dataset = data.frame(c(1,2,3), c("5","7","2"))
dataset = data.frame(c(1,2,3), c("5","7","2"))
dataset = data.frame(c(0,0,0), c("5","7","2"))
dataset = data.frame(c(0,0,0,0), c(1,5,7, 0), c(1, NA, NA, 0))
# dataset = data.frame(c(0,0,0), c("5","7","2"))
dataset = data.frame(c(0,0,0), c("5","7","2"))
dataset = data.frame(A=c(18,4,6,2,7), B=c("fd","SHBf256", ",.,./", "%&&!", "stuf; thingshgs"), C=c(NA,NA,NA,NA,NA))

data="~/Downloads/acetylome-normalized_table-output.gct"
data=parse_gctx(data)

### Close all connections
for(i in seq_len(sink.number())){
  sink(NULL)
}


diagnoseMyData(dataset)
diagnoseMyData(data)
diagnoseMyData(data@mat, check_rows=FALSE)

