library(dplyr); 
library(stringi); 
library(stringr)


loadDataset <- function(data_file_path)
{
  
  # Read data in binary mode
  conn <- file(data_file_path, open="rb")
  dataset <- readLines(conn, encoding="UTF-8"); 
  close(conn)
  rm(conn)
  return (dataset)
  
}



getLastNWords <- function(x, n=1)
{
  x_list <- stri_split_charclass(x, "\\p{WHITE_SPACE}")
  
  x_vector <- x_list[[1]]
  
  if(length(x_vector) >= n)
    return (tail(x_vector,n))
  else
  {
    print("There are less number of words than requested. Returning only last word")
    return (tail(x_vector,1))
  }
}

getSubNgram <- function(x, n=1)
{
  
  x_list <- stri_split_charclass(tolower(x), "\\p{WHITE_SPACE}")
  
  x_vector <- x_list[[1]]
  
  ngram = c("")
  
  if(length(x_vector) >= n)
  {
    y_vector <- tail(x_vector,n)
    #ngram <- lapply(y_vector,function(x) paste0(ngram,y_vector, collapse = " "))
    ngram <- paste0(ngram,y_vector, collapse = " ")
    # foreach(i=1:n)
    #   paste0(ngram,y_vector[i], collapse = " ")
  }
  else
  {
    warning("There are less number of words than requested. Returning only the last word")
    return(tail(x_vector,1))
  }
  
  return(ngram)
}

getNumberOfWords <- function(x)
{
  x <- gsub("^\\s+|\\s+$", "", x)
  
  x_list <- stri_split_charclass(x, "\\p{WHITE_SPACE}")
  
  x_vector <- x_list[[1]]
  
  return(length(x_vector))
}

# sub_ngram <- getSubNgram("the rest of the time I have been", 3)
# sub_ngram
# print(getNumberOfWords(sub_ngram))
