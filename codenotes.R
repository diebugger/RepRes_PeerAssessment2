# includes
library("knitr")

# load dataset
conn <- bzfile("repdata-data-StormData.csv.bz2", "rt")
StormData <- readLines(conn)
close(conn)
unlink("repdata-data-StormData.csv.bz2")

# preprocess dataset
