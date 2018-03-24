### Data Science Capstone : Course Project
### server.R file for the Shiny app
### Github repo : https://github.com/mountain-lion/data-science-capstone

suppressWarnings(library(stringr))
suppressWarnings(library(shiny))


mesg <<- ""

source("init.R")

predictNextWord <- function(key)
{
  #print(key)
  #mesg <<- getNumberOfWords(key)
  #message(getNumberOfWords(key))
  ngram <- ft.Data[[getNumberOfWords(key)+1]]
  matched_ng <- ngram[substring(ngram$Words, 1, nchar(key)) == key,]
  #View(matched_ng)
  matching_ngram <- filter(matched_ng, Frequency == max(Frequency))
  #matching_ngram <- head(filter(matched_ng, Frequency == max(Frequency)), 1)
  ng.bag <- as.character(matching_ngram[1]$Words)
  #print(ng.bag)
  if(!is.null(ng.bag) & length(ng.bag) >=1)
    return(word(ng.bag, -1))
  else
    return(NULL)
  
}

#x <- predictNextWord("the rest of")


getNextWord <- function(input_string)
{
  # Katz Back Off Algorithm
  # Predict the next term of the user input sentence
  # 1. For prediction of the next word, Quadgram is first used (first three words of Quadgram are the last three words of the user provided sentence).
  # 2. If no Quadgram is found, back off to Trigram (first two words of Trigram are the last two words of the sentence).
  # 3. If no Trigram is found, back off to Bigram (first word of Bigram is the last word of the sentence)
  # 4. If no Bigram is found, back off to the most common word with highest frequency 'the' is returned.
  
  predicted_word <- NULL
  #print(input_string)
  
  if (getNumberOfWords(input_string)>= 3) {
    sub_ngram <<- getSubNgram(input_string, 3)
    #print("sub_ngram")
    if(is.null(predicted_word <- predictNextWord(sub_ngram)))
      predicted_word <- getNextWord(getSubNgram(input_string, 2))
    else
      mesg <<- "Next word is predicted using 4-gram."
  }
  else if(getNumberOfWords(input_string) == 2)
  {
    sub_ngram <- getSubNgram(input_string, 2)
    #print(sub_ngram)
    if(is.null(predicted_word <- predictNextWord(sub_ngram)))
      predicted_word <- getNextWord(getSubNgram(input_string, 1))
    else
      mesg <<- "Next word is predicted using 3-gram."
  }
  else if (getNumberOfWords(input_string) == 1){
    sub_ngram <- getSubNgram(input_string)
    #print(sub_ngram)
    if(is.null(predicted_word <- predictNextWord(sub_ngram)))
    {
      {mesg<<-"No match found. Most common word 'the' is returned."; head("the",1)}
      #warning(mesg)
      predicted_word <- "the"
    }
  }
  
  return(predicted_word[1])
}




textPrediction <- function(x) {
  
  getNextWord(x)

}


shinyServer(function(input, output) {
  output$prediction <- renderPrint({
    #remove all white spaces at the begining and at the end
    x <- gsub("^\\s+|\\s+$", "", input$inputString)
    result <- textPrediction(x)
    output$text2 <- renderText({mesg})
    result
  });
  
  output$text1 <- renderText({
    input$inputString});
}
)