# 1950 - 2011 Storm Data Top Casualties and Damages
diebugger  
Saturday, April 25, 2015  

# Abstract
Here there should be a 10-sentences summary of this analysis.

# Data Processing
There should be a section titled Data Processing which describes (in words and code) how the data were loaded into R and processed for analysis. In particular, your analysis must start from the raw CSV file containing the data. You cannot do any preprocessing outside the document. If preprocessing is time-consuming you may consider using the cache = TRUE option for certain code chunks.


```r
# includes
library("knitr")
library("lubridate")
library("dplyr")
library("ggplot2")
```


```r
# load dataset
StormData <- read.csv(bzfile("repdata-data-StormData.csv.bz2", "rt"))
```


```r
# add sum of population damages and group by event type
StormData <- StormData %>% group_by(EVTYPE) %>% mutate(POP_HEALTH_DMG = sum(INJURIES, FATALITIES)) 

# subset for ease of use
PopDmg <- StormData[, c(8,38)]
PopDmgByEvType <- PopDmg[!duplicated(PopDmg),]
PopDmgByEvType <- PopDmgByEvType[order(PopDmgByEvType$POP_HEALTH_DMG, na.last = T, decreasing = T),]
```



```r
# subset data for ease of use
PropertyDmg <- StormData[, c(8, 25:28)]
PropertyDmg <- PropertyDmg[
  (PropertyDmg$PROPDMGEXP %in% c("K", "k", "B", "b", "M", "m") 
   & PropertyDmg$CROPDMGEXP %in% c("K", "k", "B", "b", "M", "m"))
  ,]
```


```r
GetDamageInBillions <- function (val, mag) {
  for (i in 1:length(mag)) {
    if (mag[i] == "K" | mag[i] == "k")
      val[i] <- val[i] / 1000000
    if (mag[i] == "M" | mag[i] == "m")
      val[i] <- val[i] / 1000
  }
  return(val)
}
```


```r
# calculate the correct damage amount value based on marked magnitude (K, M, B)
PropertyDmg$PROPDMG <- GetDamageInBillions(PropertyDmg$PROPDMG, PropertyDmg$PROPDMGEXP)
PropertyDmg$CROPDMG <- GetDamageInBillions(PropertyDmg$CROPDMG, PropertyDmg$CROPDMGEXP)
```


```r
# drop unused variables (3,5)
PropertyDmg <- PropertyDmg[, c(1, 2, 4)]

# make sum of damage by event type and order by sum desc
PropertyDmg <- PropertyDmg %>% mutate(DMGSUM = sum(PROPDMG, CROPDMG)) %>% group_by(EVTYPE)
PropertyDmg <- PropertyDmg[order(PropertyDmg$DMGSUM, na.last = T, decreasing = T), ]

# shrink dataset and remove duplicates
PropertyDmgByEvType <- PropertyDmg[, c(1, 4)]
PropertyDmgByEvType <- PropertyDmgByEvType[!duplicated(PropertyDmgByEvType), ]
```

# Results
Barplots answering the two questions:


```r
# adjust labels and margins
par(las = 2)
par(mar = c(12,5,5,2))
# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
barplot(
  PopDmgByEvType$POP_HEALTH_DMG[1:10], 
  main = "Top 10 Casualties by Event", 
  names.arg = PopDmgByEvType$EVTYPE[1:10], 
  col = "skyblue", 
  log = "y"
)
```

![](StormData_files/figure-html/question1-1.png) 


```r
# 2. Across the United States, which types of events have the greatest economic consequences?
barplot(
  PropertyDmgByEvType$DMGSUM[1:10], 
  main = "Top 10 Damage by Event (in Billions of USD)", 
  names.arg = PropertyDmgByEvType$EVTYPE[1:10], 
  col = "purple", 
  log = "y"
)
```

![](StormData_files/figure-html/question2-1.png) 