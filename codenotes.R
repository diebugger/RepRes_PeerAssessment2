# includes
library("knitr")
library("lubridate")
library("dplyr")
library("ggplot2")

# load dataset
StormData <- read.csv(bzfile("repdata-data-StormData.csv.bz2", "rt"))

# preprocess dataset
StormData$BGN_DATE <- as.Date.character(StormData$BGN_DATE, "%m/%d/%Y %H:%M:%S")
StormData$END_DATE <- as.Date.character(StormData$END_DATE, "%m/%d/%Y %H:%M:%S")
# BGN_TIME and BGN_END are not homogeneus and cannot be successfully converted

# add sum of population damages and group by event type
StormData <- StormData %>% mutate(POP_HEALTH_DMG = sum(INJURIES, FATALITIES)) %>% group_by(EVTYPE) 

# subset for ease of use
PopDmg <- StormData[, c(8,38)]
PopDmgByEvType <- PopDmg[!duplicated(PopDmg),]
PopDmgByEvType <- PopDmgByEvType[order(PopDmgByEvType$POP_HEALTH_DMG, na.last = T, decreasing = T),]

# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
par(las = 2) # make label text perpendicular to axis
par(mar = c(12,5,5,2))
barplot(
  PopDmgByEvType$POP_HEALTH_DMG[1:10], 
  main = "Top 10 Casualties by Event Type", 
  names.arg = PopDmgByEvType$EVTYPE [1:10], 
  col = "skyblue"
)
