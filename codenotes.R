# includes
library("knitr")
library("lubridate")
library("dplyr")
library("ggplot2")

# load dataset
StormData <- read.csv(bzfile("repdata-data-StormData.csv.bz2", "rt"))

# preprocess dataset (not used)
#StormData$BGN_DATE <- as.Date.character(StormData$BGN_DATE, "%m/%d/%Y %H:%M:%S")
#StormData$END_DATE <- as.Date.character(StormData$END_DATE, "%m/%d/%Y %H:%M:%S")
# BGN_TIME and BGN_END are not homogeneus and cannot be successfully converted

# add sum of population damages and group by event type
StormData <- StormData %>% group_by(EVTYPE) %>% mutate(POP_HEALTH_DMG = sum(INJURIES, FATALITIES))

# subset for ease of use
PopDmg <- StormData[, c(8,38)]
PopDmgByEvType <- PopDmg[!duplicated(PopDmg),]
PopDmgByEvType <- PopDmgByEvType[order(PopDmgByEvType$POP_HEALTH_DMG, na.last = T, decreasing = T),]

# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
par(las = 2) # make label text perpendicular to axis
par(mar = c(12,5,5,2))
barplot(
  PopDmgByEvType$POP_HEALTH_DMG[1:10], 
  main = "Top 10 Casualties by Event", 
  names.arg = PopDmgByEvType$EVTYPE[1:10], 
  col = "skyblue", 
  log = "y"
)

# 2. Across the United States, which types of events have the greatest economic consequences?
# subset data for ease of use
PropertyDmg <- StormData[, c(8, 25:28)]
PropertyDmg <- PropertyDmg[
  (PropertyDmg$PROPDMGEXP %in% c("K", "k", "B", "b", "M", "m") 
   & PropertyDmg$CROPDMGEXP %in% c("K", "k", "B", "b", "M", "m"))
  ,]

# calculate the correct damage amount value based on marked magnitude (K, M, B)
PropertyDmg$PROPDMG <- GetCorrectAmount(PropertyDmg$PROPDMG, PropertyDmg$PROPDMGEXP)
PropertyDmg$CROPDMG <- GetCorrectAmount(PropertyDmg$CROPDMG, PropertyDmg$CROPDMGEXP)

# drop unused variables (3,5)
PropertyDmg <- PropertyDmg[, c(1, 2, 4)]

# adjust significant digits
#options(digits = 4)

# make sum of damage by event type and order by sum desc
PropertyDmg <- PropertyDmg %>% mutate(DMGSUM = sum(PROPDMG, CROPDMG)) %>% group_by(EVTYPE)
PropertyDmg <- PropertyDmg[order(PropertyDmg$DMGSUM, na.last = T, decreasing = T), ]

# shrink dataset and remove duplicates
PropertyDmgByEvType <- PropertyDmg[, c(1, 4)]
PropertyDmgByEvType <- PropertyDmgByEvType[!duplicated(PropertyDmgByEvType), ]

# question answer plot
barplot(
  PropertyDmgByEvType$DMGSUM[1:10], 
  main = "Top 20 Damage by Event (in USD)", 
  names.arg = PropertyDmgByEvType$EVTYPE[1:10], 
  col = "purple", 
  log = "y"
)
