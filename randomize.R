source("swiftkey.R")

randomize <- function(ratio, targetFolder){
  randomizeFile <- function(src, target){
    print(paste("starting", target))
    #len <- countLinesSK("en_US", "final")
    conn=file(src, open="r")
    lines=readLines(conn)
    close(conn)
    print(paste("len=", length(lines)))
    print(paste("ratio=", ratio))
    s <- sample(lines, length(lines)*ratio, replace=FALSE)
    conn = file(target, open="w")
    writeLines(s, con=conn)
    close(conn)
  }
  src <- "final\\en_US\\en_US.blogs.txt"
  target <- paste(targetFolder, "\\en_US\\en_US.blogs.txt", sep="")
  randomizeFile(src, target)
  src <- "final\\en_US\\en_US.news.txt"
  target <- paste(targetFolder, "\\en_US\\en_US.news.txt", sep="")
  randomizeFile(src, target)
  src <- "final\\en_US\\en_US.twitter.txt"
  target <- paste(targetFolder, "\\en_US\\en_US.twitter.txt", sep="")
  randomizeFile(src, target)
  
}