
folder <- "final"

### quizz question 2
countLinesFolder <- function() {
  library(R.utils)
  pathName <- paste(folder, "\\en_US\\en_US.twitter.txt", sep="")
  res <- countLines(pathName)
  return (res)
}

### quizz question 3
maxLen <- function(){
  len <- c(
    maxLenFile("en_US.blogs.txt"), #40835
    maxLenFile("en_US.twitter.txt"),#213
    maxLenFile("en_US.news.txt") #5760
  );
  return (len)
}

maxLenFile <- function(fileName){
  src <- paste(folder, "\\en_US\\", fileName, sep="")
  maxLen <- 0
  conn=file(src, open="r")
  linn=readLines(conn)
  for (i in 1:length(linn)){
    maxLen <- max(maxLen, nchar(linn[i]))
  }
  close(conn)
  return (maxLen)
}

### quizz question 4
getLinesWithWordDiv <- function(){
  love <- getLinesWithWord("love")
  hate <- getLinesWithWord("hate")
  return (love/hate)
}

getLinesWithWord <- function(word){
  src <- paste(folder, "\\en_US\\", "en_US.twitter.txt", sep="")
  # (^|\s)love($|\s)
  regex <- paste("(^|\\s)", word, "($|\\s)", sep="")
  conn=file(src, open="r")
  linn=readLines(conn)
  count <- 0
  for (i in 1:length(linn)){
    if (grepl(regex, linn[i]))
      count <- count + 1
  }
  close(conn)
  return (count)
}

### quizz question 5
findBiostats <- function(){
  src <- paste(folder, "\\en_US\\", "en_US.twitter.txt", sep="")
  word <- "biostats"
  regex <- paste("(^|\\s)", word, "($|\\s)", sep="")
  conn=file(src, open="r")
  linn=readLines(conn)
  x <- ""
  for (i in 1:length(linn)){
    if (grepl(regex, linn[i]))
      x <- linn[i]
  }
  close(conn)
  return (x)
}

### quizz question 6
findExactLine <- function(){
  line <- "A computer once beat me at chess, but it was no match for me at kickboxing"
  src <- paste(folder, "\\en_US\\", "en_US.twitter.txt", sep="")
  conn=file(src, open="r")
  linn=readLines(conn)
  count <- 0
  for (i in 1:length(linn)){
    if (linn[i]==line)
      count <- count + 1
  }
  close(conn)
  return (count)
}
