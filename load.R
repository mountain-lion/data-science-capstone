DATA_SOURCE <- "../data/final/en_US/"

en_blogs_path <- paste0(DATA_SOURCE,"en_US.blogs.txt")
en_news_path <- paste0(DATA_SOURCE,"en_US.news.txt") 
en_twitter_path <- paste0(DATA_SOURCE,"en_US.twitter.txt") 

if(!exists("blogs"))
  blogs <<- loadDataset(en_blogs_path)
if(!exists("news"))
  news <<- loadDataset(en_news_path)
if(!exists("twitter"))
  twitter <<- loadDataset(en_twitter_path)


if(!exists("sample_data"))
{
  message("sample_data does not exist. It needs to be created")
  # Set random seed for reproducibility and sample the data
  set.seed(1234)
  sample_size <- 0.01 # Subsample to 5%
  
  sample_blogs <- blogs[sample(1:length(blogs), sample_size*length(blogs), replace=FALSE)]
  sample_news <- news[sample(1:length(news), sample_size*length(news), replace=FALSE)]
  sample_twitter <- twitter[sample(1:length(twitter), sample_size*length(twitter), replace=FALSE)]
  
  # Remove unconvention/funny characters for sampled Blogs/News/Twitter
  sample_blogs <- iconv(sample_blogs, "UTF-8", "ASCII", sub="")
  sample_news <- iconv(sample_news, "UTF-8", "ASCII", sub="")
  sample_twitter <- iconv(sample_twitter, "UTF-8", "ASCII", sub="")
  
  sample_data <<- c(sample_blogs,sample_news,sample_twitter)
}

cleanLargeTemps <- function(){
  # Remove temporary variables
  rm(blogs, news, twitter)
}

cleanLargeTemps()
