#FOLDER <- "short"
#FOLDER <- "final"
FOLDER <- "random10"

library(NLP)
library(tm)
library(R.oo)
library(R.methodsS3)
library(R.utils)
library(hash)


## example of corpus created from word vectors
# text1 <- c("apple" , "love", "crazy", "peaches", "cool", "coke", "batman", "joker")
# text2 <- c("omg", "#rstats" , "crazy", "cool", "bananas", "functions", "apple")
# text3 <- c("Playing", "rstats", "football", "data", "coke", "caffeine", "peaches", "cool")
# tdm1 <- TermDocumentMatrix(Corpus(VectorSource(text1)))
# tdm2 <- TermDocumentMatrix(Corpus(VectorSource(text2)))
# tdm3 <- TermDocumentMatrix(Corpus(VectorSource(text3)))

fileNameSK <- function(locale, fileType, folder=FOLDER){
  return (paste(folder, "\\", locale, "\\", locale, ".", fileType, ".txt", sep=""))
}

fileSizeSK <- function(locale, folder=FOLDER){
  return (list(
    blogs=file.size(fileNameSK(locale, "blogs", folder)),
    news=file.size(fileNameSK(locale, "news", folder)),
    twitter=file.size(fileNameSK(locale, "twitter", folder))
  ))
}

countLinesSK <- function(locale, folder=FOLDER) {
  countLinesSK_ <- function(locale, fileType) {
    pathName <- fileNameSK(locale, fileType, folder)
    res <- countLines(pathName)
    return (res)
  }
  return (list( 
    blogs=countLinesSK_(locale, "blogs"),
    news=countLinesSK_(locale, "news"),
    twitter=countLinesSK_(locale, "twitter")
  ))
}

avgLineSize <- function(locale, folder=FOLDER){
  avgLineSize_ <- function(locale, fileType) {
    pathName <- fileNameSK(locale, fileType, folder)
    conn=file(pathName, open="r")
    lines=readLines(conn, encoding="UTF-8")
    close(conn)
    len <- nchar(lines)
    return (sum(len)/length(len))
  }
  return (list( 
    blogs=avgLineSize_(locale, "blogs"),
    news=avgLineSize_(locale, "news"),
    twitter=avgLineSize_(locale, "twitter")
  ))
}

countWordsSimple <- function(locale, folder=FOLDER) {
  countWordsSimple_ <- function(locale, fileType) {
    pathName <- fileNameSK(locale, fileType, folder)
    wordCount <- 0;
    conn=file(pathName, open="r")
    linn=readLines(conn)
    spl <- strsplit(linn, "\\s")
    for (i in 1:length(spl)){
      wordCount <- wordCount + length(spl[[i]])
    }
    close(conn)
    return (wordCount)
  }
  return (list( 
    blogs=countWordsSimple_(locale, "blogs"),
    news=countWordsSimple_(locale, "news"),
    twitter=countWordsSimple_(locale, "twitter")
  ))
} 

wordMap <- function(locale, folder=FOLDER){
  wordMap_ <- function(locale, fileType){
    pathName <- fileNameSK(locale, fileType, folder)
    h <- hash()
    wordCount <- 0;
    conn=file(pathName, open="r")
    linn=readLines(conn)
    close(conn)
    spl <- strsplit(linn, "\\s")
    for (i in 1:length(spl)){
      wordCount <- wordCount + length(spl[[i]])
      for (j in 1:length(spl[[i]])){
        word <- spl[[i]][j]
        if (nchar(word)>0){
          #print(paste("treating word", word))
          current <- h[[word]]
          if (is.null(current)){
            h[[word]] <- 1
          }else{
            h[[word]] <- current+1
          }
        }
      }
    }
    
    return (list(count=wordCount, hash=h))
  }
  return (list( 
    blogs=wordMap_(locale, "blogs"),
    news=wordMap_(locale, "news"),
    twitter=wordMap_(locale, "twitter")
  ))
}

hashMerge <- function(wordMapResult){
  merge_ <- function(src, target){
    keys <- keys(src)
    lapply(keys, function(x){
      if (has.key(x, target)){
        target[[x]] <- target[[x]] + src[[x]]
      }else{
        target[[x]] <- src[[x]]
      }
    })
    return (target)
  }
  print("copying blogs")
  result<- copy(wordMapResult$blogs$hash)
  print("merging with news")
  merge_(wordMapResult$news$hash, result)
  print("merging with twitter")
  merge_(wordMapResult$twitter$hash, result)
  return (result)
}

wordOccursMost <- function(hash){
  result <- list(maxWord="", maxFrequency=0)
  keys <- keys(hash)
  for (i in 1:length(keys)){
    x <- keys[[i]]
    freq <- hash[[x]]
    if (freq>result$maxFrequency){
      print(paste("overridding frequency to", freq))
      result$maxWord <- x
      result$maxFrequency <- freq
    }
  }
  return (result)
}

countLowFreq <- function(hash){
  count <- rep(0, 10)
  keys <- keys(hash)
  thresh <- length(count)+1
  for (i in 1:length(keys)){
    x <- keys[[i]]
    freq <- hash[[x]]
    if (freq<thresh){
      count[[freq]] <- count[[freq]] + 1
    }
  }
  return (count)
}

preprocessAll <- function(folder=FOLDER, filters=list(), verbose=TRUE){
  en <- preprocess(folder, "en_US", filters, verbose)
  de <- preprocess(folder, "de_DE", filters, verbose)
  fi <- preprocess(folder, "fi_FI", filters, verbose)
  ru <- preprocess(folder, "ru_RU", filters, verbose)
  return (list(en=en, de=de, fi=fi, ru=ru))
}

### remove some additional special characters from a line during pre-processing
removeQuotes <- function(line){
  rClass <- "['´`\"\u2018\u2019\u201c\u201d]";
  regex <- paste(" ", rClass, "|", rClass, " |", rClass, "$|^", rClass, "|\u2026", sep="")
  result <- gsub(regex, " ", line)
  return (result)
}

### removes all words that contain digits inside
removeMyNumbers <- function(line){
  return (gsub("\\w*\\d+\\w*", " ", line))
}

### provides the list of words to remove from the result set
getStopWords <- function(locale){
  fileName <- paste("profanity\\", locale, ".txt", sep="")
  x <- scan(fileName, what="", sep="\n")
  return (x)
}


### do the pre-processing of a text data set
### folders can be 'short' or 'final'
### filters is a list of booleans: list(
###           puncRemove,       => removes all punctuation
###           puncWhite,        => replaces punctuation by white characters, doesn't work if puncRemove is TRUE
###           hyphenationCount  => counts the number of hyphens at the end of lines
###           numbersRemove     => removes all words with numbers inside
###           profanity         => removes all profanity from words list
###           )
preprocessCorpus <- function(locale, filters=list(), verbose=TRUE, folder=FOLDER){
  # use PCorpus to store in DB
  src <- paste(folder, "\\", locale, sep="")
  if (verbose) print(paste("reading", src))
  #   enc <- ""
  #   if (locale == "ru_RU")
  #     enc <- "UTF-8"
  enc <- "UTF-8"
  if(verbose) print(paste("encoding:", enc))
  corpus <- Corpus(DirSource(src, encoding=enc),
                   readerControl=list(reader=readPlain, 
                                      language=locale))
  
  # invalid characters filtering 
  # http://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
  # http://tm.r-forge.r-project.org/faq.html
  #tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
  #tm_map(corpus, content_transformer(function(x) iconv(x, to="ASCII", sub = "")))
  #t<-iconv(t, to="ASCII", sub = "")
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, from="UTF-8", to="ASCII", sub = "")))
  
  
  # puncRemove filtering
  if ((!is.null(filters$puncRemove)) && filters$puncRemove){
    if (verbose) print(" filtering puncRemove")
    corpus <- tm_map(corpus, FUN=removePunctuation)
    # this doesn't remove some special quotes => do it afterwards
    corpus <- tm_map(corpus, FUN=removeQuotes)
  }
  
  # puncWhite filtering
  # -> not very good filtering. More characters must be removed
  # besides, different locales might require different rules => leave it to standard filters
  if ((!is.null(filters$puncWhite)) && filters$puncWhite){
    if (verbose) print(" filtering puncWhite")
    corpus <- tm_map(corpus, FUN=function(x){
      result <- gsub("[\\.,\\/#!$%\\^&\\*;:{}=\\-_'´`~()]", " ", x)
      return (result)
    })
  }
  
  # hyphenation count
  hyphenationCount <- -1
  if ((!is.null(filters$hyphenationCount)) && filters$hyphenationCount){
    hyphenationCount <- 0
    if (verbose) print(" hyphenation count")
    corpus <- tm_map(corpus, FUN=function(x){
      if (x[length(x)]=='-')
        hyphenationCount <- hyphenationCount + 1
      return (x)
    })
    if (verbose) print(paste("     => ", hyphenationCount))
  }
  
  # numbers remove
  if ((!is.null(filters$numbersRemove)) && filters$numbersRemove){
    if (verbose) print(" filtering numbersRemove")
    corpus <- tm_map(corpus, FUN=removeMyNumbers)
  }
  
  # profanity
  if ((!is.null(filters$profanity)) && filters$profanity){
    if (verbose) print(" filtering profanity")
    corpus <- tm_map(corpus, removeWords, getStopWords(locale))
  }
  if (verbose) print(" final cleanup")
  # final cleaning: making sure the transformation returns PlainTextDocuments in the end
  corpus <- tm_map(corpus, PlainTextDocument)
  if (verbose) print(" corpus ready for processing")
  return (list(corpus=corpus, hyphenationCount=hyphenationCount))
}

preprocessWords_beep <- function(locale, filters=list(), verbose=TRUE, folder=FOLDER){
  return( tryCatch({preprocessWords(locale, filters, verbose, folder)}, finally={alarm()}) )
}

preprocessWords <- function(locale, filters=list(), verbose=TRUE, folder=FOLDER){
  
  # preprocessing
  corpusPreprocess <- preprocessCorpus(locale, filters, verbose, folder)
  corpus <- corpusPreprocess$corpus
  hyphenationCount <- corpusPreprocess$hyphenationCount
  
  # basic counting
  if (verbose) print(" building document term matrix (1-gram)")
  tdm <- TermDocumentMatrix(corpus, control=list(stopwords=FALSE))
  if (verbose) print(paste(" terms size:", ncol(tdm)))
  
  alarm()
  return (list(
    corpus=corpus,
    tdm=tdm,
    freq=ncol(tdm),
    hyphenationCount=hyphenationCount
  ))
}

### get all words whose frequency is between minFreq and maxFreq
frequentWords <- function(result, minFreq=2, maxFreq=Inf){
  return (findFreqTerms(result$tdm, minFreq, maxFreq))
}

### find the frequencies of all words in the preprocessing result. It's in a named vector. type answer["word"] to get the frequency
findFreq <- function(tdm){
  return (rowSums(inspect(tdm)))
}

preprocessNgrams_beep <- function(locale, filters=list(), n=2, verbose=TRUE, folder=FOLDER){
  return( tryCatch({preprocessNgrams(locale, filters, n, verbose, folder)}, finally={alarm()}) )
}

### find all n-grams for a specific locale. n is the parameter of n-grams.
preprocessNgrams <- function(locale, filters=list(), n=2, verbose=TRUE, folder=FOLDER){
  
  # preprocessing
  corpusPreprocess <- preprocessCorpus(locale, filters, verbose, folder)
  corpus <- corpusPreprocess$corpus
  hyphenationCount <- corpusPreprocess$hyphenationCount
  
  # n-grams
  if (verbose) print(" building document term matrix (n-grams)")
  BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
  if (verbose) print(paste(" terms size:", nrow(tdm)))
  
  alarm()
  return (list(
    corpus=corpus,
    tdm=tdm,
    freq=ncol(tdm),
    hyphenationCount=hyphenationCount
  ))
}

### builds a lookup table from a preprocess2grams call. It keeps 3 results
build2grams <- function(result){
  freqList <- findFreq(result)
  h <- hash()
  firstWord <- ''
  bestWords <- rep('', 3)
  bestFreqs <- rep(0, 3)
  for (i in 1:length(freqList)){
    # if ((i%%10000)==0) print(paste("treated: ", i))
    freq <- freqList[i]  # frequency
    pair <- names(freq)
    #print(paste("treating pair ", pair))
    spl <- strsplit(pair, " ")
    #print(paste("spl = ", spl))
    first <- spl[[1]][[1]]   # first word
    second <- spl[[1]][[2]]  # second word
    
    DEBUG <- FALSE
    if (first=="the" || firstWord=="the") DEBUG <- TRUE
    
    if (first==firstWord){
      # go on in the same group (it's in alphabetical order)
      lowestFreq = min(bestFreqs)
      if (freq>lowestFreq){
        # we found a best word
        index <- match(lowestFreq, bestFreqs) # index of lowest frequency
        bestFreqs[index] <- freq
        bestWords[index] <- second
        if (DEBUG) print (paste("replaced: pair ", pair, "of frequency", freq, "at", index))
      }else {
        if (DEBUG) print (paste("skipped: pair ", pair, "of frequency", freq, "(best frequencies are [", paste(bestFreqs, collapse=","), "] for [", paste(bestWords, collapse=","),"])"))
      }
    }else{
      # close last group if exists
      valid <- TRUE
      if (bestWords[2]=='' || bestWords[3]=='')
        valid <- FALSE
      if (DEBUG) print(paste("valid =", valid))
      if (valid){  # only store if there is at least 3 suggestions
        if (DEBUG) print(paste("storing key '", firstWord, "' with value: [", paste(bestWords, collapse=","), "]"))
        h[[firstWord]] <- bestWords
        if (DEBUG) print(paste("lookup for '", firstWord, "' gives [", paste(h[[firstWord]], collapse=","), "]"))
      }
      # start a new group
      firstWord <- first
      bestWords <- rep('', 3)
      bestFreqs <- rep(0, 3)
      bestFreqs[1] <- freq
      bestWords[1] <- second
      if (DEBUG) print(paste("start with pair '", pair, "' and frequency ", freq, sep=""))
    }
  }
  return (h)
}

### finds all terms that start with 'begining'. 'begining' should not end with a space. 
findStarting <- function(tdm, begining) {
  firstWord <- paste(begining, " ", sep="")
  freqList <- findFreq(tdm)
  len <- nchar(firstWord)
  for (i in 1:length(freqList)){
    x <- freqList[i]
    if (nchar(names(x))>=len) {
      if (substring(names(x), 1, len)==firstWord)
        print(paste(names(x), " of frequency ", x))
    }
  }
}

