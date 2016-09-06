library("ggplot2")
library("stringr")
library("repmis")
library("ggthemes")
library("reshape")
library("xts")
#library("quantmod")
#library("foreach")
library("scales")
#library("reshape2")
#library("lubridate")
library("shiny")
library("leaflet")
library("plyr")

AddCumulativeCol <- function(df, columnIndex) 
{
  newColumnName <-str_c(columnIndex, "_", "Cumulative")
  column <- df[,columnIndex]
  df[1,newColumnName] <- column[1]
  for(i in 2:length(column))
  {
    df[i,newColumnName] <- df[i-1,newColumnName]+column[i]
  }
  return(df)
}

GetLatLong <- function(df, cityState)
{
  city <- unlist(strsplit(cityState, ","))[1]
  state <- unlist(strsplit(cityState, ","))[2]
  lat <- df[which(df[,"City"] == city & df[,"State"] == state), ][1,"Lat"]
  long <- df[which(df[,"City"] == city & df[,"State"] == state), ][1,"Long"]
  latLong <- paste(lat,long,sep = ",")
}
GetLatLong2 <- function(df, city, state)
{
  lat <- df[which(df[,"City"] == city & df[,"State"] == state), ][1,"Lat"]
  long <- df[which(df[,"City"] == city & df[,"State"] == state), ][1,"Long"]
  latLong <- paste(lat,long,sep = ",")
}

SplitByInterval <- function(df,dateColumn,valueColumn, interval)
{
  df_new = data.frame(DATE = df[,dateColumn])
  df_new[,"VALUE"] = df[,valueColumn]
  df_new <- as.xts(df_new[,"VALUE"], order.by = as.Date(df_new[,"DATE"]))
  if(interval == "daily")
  {
    df_new_inter <- apply.daily(df_new,sum)
  }
  if(interval == "weekly")
  {
    df_new_inter <- apply.weekly(df_new,sum)
  }
  if(interval =="monthly")
  {
    df_new_inter <- apply.monthly(df_new,sum)
  }
  if(interval == "quarterly")
  {
    df_new_inter <- apply.quarterly(df_new,sum)
  }
  if(interval == "yearly")
  {
    df_new_inter <- apply.yearly(df_new,sum)
  }
  df_new = data.frame(dateColumn = index(df_new_inter))
  df_new[, valueColumn] = df_new_inter[,1]
  df_new = data.frame(df_new)
  
  return(df_new)
}

MonthlyTable <- function(df1,df1Name,dateColumn1,valueColumn1,df2,df2Name,dateColumn2,valueColumn2, startDate,endDate)
{
  numMonths <- 0
  endDateCounter <- endDate
  while(endDateCounter>startDate)
  {
    endDateCounter <- as.Date(endDateCounter) - months(1)
    numMonths <- numMonths+1
  }
  month <- NULL
  value1 <- NULL
  value2 <- NULL
  
  for(i in 1:numMonths)
  {
    subtractMonths = numMonths-i
    end2 <- as.Date(endDate)-months(subtractMonths)
    start2 <- as.Date(end2)-months(1)
    df_Monthly1 <- data.frame(date=df1[,dateColumn1], Value = df1[,valueColumn1])
    df_Monthly1 <- df_Monthly1[df_Monthly1[,1]>=startDate & df_Monthly1[,1]<end2,]
    df_Monthly1 <- df_Monthly1[order(df_Monthly1[,1]),]
    df_Monthly1 <- AddCumulativeCol(df_Monthly1,2)
    
    df_Monthly2 <- data.frame(date=df2[,dateColumn2], Value = df2[,valueColumn2])
    df_Monthly2 <- df_Monthly2[df_Monthly2[,1]>=startDate & df_Monthly2[,1]<end2,]
    df_Monthly2 <- df_Monthly2[order(df_Monthly2[,1]),]
    df_Monthly2 <- AddCumulativeCol(df_Monthly2,2)
    
    
    month = c(month,format(start2,"%b %Y"))
    value1 = c(value1,df_Monthly1[nrow(df_Monthly1),3])
    value2 = c(value2,df_Monthly2[nrow(df_Monthly2),3])
    
  }
  percent <-  (value1/value2)*100
  df_MonthlyTable <- data.frame(Month=month)
  df_MonthlyTable[,df1Name] = value1
  df_MonthlyTable[,df2Name] = value2
  df_MonthlyTable[,"PERCENT"] = percent
  
  return(df_MonthlyTable)
}

MonthlyTable2 <- function(df1,df1Name,dateColumn1,valueColumn1,df2,df2Name,dateColumn2,valueColumn2, startDate,endDate)
{
  numMonths <- 0
  endDateCounter <- endDate
  while(endDateCounter>startDate)
  {
    endDateCounter <- as.Date(endDateCounter) - months(1)
    numMonths <- numMonths+1
  }
  month <- NULL
  value1 <- NULL
  value2 <- NULL
  
  for(i in 1:numMonths)
  {
    subtractMonths = numMonths-i
    end2 <- as.Date(endDate)-months(subtractMonths)
    start2 <- as.Date(end2)-months(1)
    df_Monthly1 <- data.frame(date=df1[,dateColumn1], Value = df1[,valueColumn1])
    df_Monthly1 <- df_Monthly1[df_Monthly1[,1]>=start2 & df_Monthly1[,1]<end2,]
    df_Monthly1 <- df_Monthly1[order(df_Monthly1[,1]),]
    df_Monthly1 <- AddCumulativeCol(df_Monthly1,2)
    
    df_Monthly2 <- data.frame(date=df2[,dateColumn2], Value = df2[,valueColumn2])
    df_Monthly2 <- df_Monthly2[df_Monthly2[,1]>=start2 & df_Monthly2[,1]<end2,]
    df_Monthly2 <- df_Monthly2[order(df_Monthly2[,1]),]
    df_Monthly2 <- AddCumulativeCol(df_Monthly2,2)
    
    
    month = c(month,format(start2,"%b %Y"))
    value1 = c(value1,df_Monthly1[nrow(df_Monthly1),3])
    value2 = c(value2,df_Monthly2[nrow(df_Monthly2),3])
    
  }
  percent <-  (value1/value2)*100
  df_MonthlyTable <- data.frame(Month=month)
  df_MonthlyTable[,df1Name] = value1
  df_MonthlyTable[,df2Name] = value2
  df_MonthlyTable[,"PERCENT"] = percent
  
  return(df_MonthlyTable)
}




###############################################################################################
#PREPARE DATA#

#DOWNLOAD DATA
#df_fallon_released <- source_DropboxData(file="FALLON_RELEASED.csv", key = "eg4yqi3780vqkja", header = TRUE, cache = TRUE)
#df_fallon_sold_all <- source_DropboxData(file="FALLON_SOLD.csv", key = "3oe8vv9kb3p16y4", header = TRUE, cache = TRUE)
#df_fallon_quoted <- source_DropboxData(file="FALLON_QUOTED.csv", key="fe7g5p34ufpmnvr", header=TRUE, cache = TRUE)
#df_fallon_deck_quoted_all <- source_DropboxData(file = "FALLON_DECK QUOTED.csv", key ="j1yzoq33l20bm5a", header = TRUE, cache = TRUE)
#df_fallon_deck_sold <- source_DropboxData(file = "FALLON_DECK SOLD.csv", key = "hygxu9d0l4q8cxz", cache = TRUE)

###df_fallon_released <- source_DropboxData(file="FALLON_RELEASED.csv", key = "eg4yqi3780vqkja", header = TRUE)
###df_fallon_sold_all <- source_DropboxData(file="FALLON_SOLD.csv", key = "3oe8vv9kb3p16y4", header = TRUE)
###df_fallon_quoted <- source_DropboxData(file="FALLON_QUOTED.csv", key="fe7g5p34ufpmnvr", header=TRUE)
###df_fallon_deck_quoted_all <- source_DropboxData(file = "FALLON_DECK QUOTED.csv", key ="j1yzoq33l20bm5a", header = TRUE)
###df_fallon_deck_sold <- source_DropboxData(file = "FALLON_DECK SOLD.csv", key = "hygxu9d0l4q8cxz")

df_fallon_released <- read.csv("./data/FALLON_RELEASED.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
df_fallon_sold_all <- read.csv("./data/FALLON_SOLD.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
df_fallon_quoted <- read.csv("./data/FALLON_QUOTED.csv", header=TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
df_fallon_deck_quoted_all <- read.csv("./data/FALLON_DECK QUOTED.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
df_fallon_deck_sold <- read.csv("./data/FALLON_DECK SOLD.csv", header = TRUE,check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)

df_cityData <- read.csv("./data/citydata.csv", header = TRUE, check.names =FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
df_colors <- read.csv("./data/colors2.csv", header = FALSE, check.names =FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
#CONVERT DATE COLUMN TO DATE FORMAT

df_fallon_released[,1] <- as.Date(df_fallon_released[,1], "%m/%d/%Y")
df_fallon_sold_all[,1] <- as.Date(df_fallon_sold_all[,1], "%m/%d/%Y")
df_fallon_quoted[,1] <- as.Date(df_fallon_quoted[,1], "%m/%d/%Y")
df_fallon_deck_quoted_all[,1] <- as.Date(df_fallon_deck_quoted_all[,1], "%m/%d/%Y")
df_fallon_deck_sold[,1] <- as.Date(df_fallon_deck_sold[,1], "%m/%d/%Y")

#Sort Columns By Date

df_fallon_released <- df_fallon_released[order(df_fallon_released[,1]),]
df_fallon_sold_all <- df_fallon_sold_all[order(df_fallon_sold_all[,1]),]
df_fallon_quoted <- df_fallon_quoted[order(df_fallon_quoted[,1]),]
df_fallon_deck_quoted_all <- df_fallon_deck_quoted_all[order(df_fallon_deck_quoted_all[,1]),]
df_fallon_deck_sold <- df_fallon_deck_sold[order(df_fallon_deck_sold[,1]),]

# GET UPDATE DATE (ASSUMES EVERTHING IS UPDATED AT THE SAME TIME)
df_fallon_released_date <- df_fallon_released[nrow(df_fallon_released),1]
df_fallon_sold_all_date <- df_fallon_sold_all[nrow(df_fallon_sold_all),1]
df_fallon_quoted_date <- df_fallon_quoted[nrow(df_fallon_quoted),1]
df_fallon_deck_quoted_all_date <- df_fallon_deck_quoted_all[nrow(df_fallon_deck_quoted_all),1]
df_fallon_deck_sold_date <- df_fallon_deck_sold[nrow(df_fallon_deck_sold),1]
lastDate <- max(c(df_fallon_released_date,df_fallon_sold_all_date,df_fallon_quoted_date,df_fallon_deck_quoted_all_date,df_fallon_deck_sold_date))
#Add Cumulative Ton Column

df_fallon_released <- AddCumulativeCol(df_fallon_released, "Total Tons")
df_fallon_sold_all <- AddCumulativeCol(df_fallon_sold_all, "Total Tons")
df_fallon_quoted <- AddCumulativeCol(df_fallon_quoted, "Total Tons")
df_fallon_deck_quoted_all <- AddCumulativeCol(df_fallon_deck_quoted_all, "Tons")
df_fallon_deck_sold <- AddCumulativeCol(df_fallon_deck_sold, "Tons")

#Calculate Time Between SOld Date and Average Released Date and Store it with Sold Data (FOR JOBS BUILT IN FALLON ONLY)
df_fallon_sold_fallon <- df_fallon_sold_all
for(i in 1:length(df_fallon_sold_fallon[,1]))
{
  regexString <- paste("^",df_fallon_sold_fallon[i,"Job Number"],"$", sep="")
  df_temp <- df_fallon_released[grep(regexString, df_fallon_released[,"Job Number"]),]
  timeBetween <- mean(df_temp[,1])-df_fallon_sold_fallon[i,1]
  if(grepl("A",df_fallon_sold_fallon[i,"Job Number"])==FALSE && grepl("R",df_fallon_sold_fallon[i,"Job Number"])==FALSE)
  {
    df_fallon_sold_fallon[i,"Days Untill Release"] <- timeBetween
  }
}
rm(i)
rm(regexString)
rm(timeBetween)
rm(df_temp)

# Get Rid of all jobs that calculated out to NA for date between sold and released; these are jobs that were sold in fallon, yet have not been released
df_fallon_sold_fallon <- df_fallon_sold_fallon[complete.cases(df_fallon_sold_fallon),]



#############
 # df_test <- df_fallon_quoted[,]
 # df_test[,"Location"] <- lapply(data.frame(df_test[,"Location"]), function(x) {gsub("  ", " ", x)})
 # df_test <- merge(df_cityData,df_test, all.y = TRUE, by = c("Location", "State"))
 # rm(df_cityData)
 # 
 # latLong <- as.list(df_test[,"LatLong"])
 # latLongApplied <- data.frame(unlist(lapply(latLong, function(x) {GetLatLong(df=df_cityData, x)})))
 # df_test[,"LatLong"] <- latLongApplied
 # rm(latLong)
 # rm(latLongApplied)
 # df_test[,"LatLong"] <- data.frame(lapply(unlist(as.list(df_test[,"LatLong"])), function(x) {GetLatLong(df = df_cityData, cityState = as.character(x))}))
 
# 
# 
# 
# for (i in c(1:nrow(df_test)))
# {
#   df_test[i,"LAT"] <- df_cityData[which(df_cityData$City == df_test[i,"Location"] & df_cityData$State == df_test[i,"State"]), ][1,"Lat"]
#   df_test[i,"LONG"] <- df_cityData[which(df_cityData$City == df_test[i,"Location"] & df_cityData$State == df_test[i,"State"]), ][1,"Long"]
# }
# 
# df_test <- df_test[complete.cases(df_test),]
# write.csv(df_test,"./data/df_quoted_map.csv")
df_joist_quoted_map <- read.csv("./data/df_quoted_map.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
listUniqueTakeoffPerson <- data.frame(unique(df_joist_quoted_map$TakeoffPerson))
df_test2 <- listUniqueTakeoffPerson
colnames(df_test2) <- "TakeoffPerson"
for(i in c(1:nrow(listUniqueTakeoffPerson)))
{
  df_test2[i,"Color"] <- df_colors[i,1]
}

df_joist_quoted_map <- merge(df_test2, df_joist_quoted_map, all.y=TRUE, by ="TakeoffPerson")
df_joist_sold_map <- read.csv("./data/df_test.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)