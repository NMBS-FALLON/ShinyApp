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

#source("./scripts/dataframes.r")
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

CreateIntervalColumn <- function(df,dateColumn, interval)
{
  if (interval == "day")
  {
    df[,"Interval"] <-  df[,dateColumn]
  }
  else if (interval == "week")
  {
    df[,"Interval"] <-  as.Date(cut(df[, dateColumn], breaks = "week", start.on.monday = TRUE))
  }
  else
  {
    df[,"Interval"] <-  as.Date(cut(df[,dateColumn], breaks = interval))
  }
  return(df)
}

# MonthlyTable <- function(df1,df1Name,dateColumn1,valueColumn1,df2,df2Name,dateColumn2,valueColumn2, startDate,endDate)
# {
#   numMonths <- 0
#   endDateCounter <- endDate
#   while(endDateCounter>startDate)
#   {
#     endDateCounter <- as.Date(endDateCounter) - months(1)
#     numMonths <- numMonths+1
#   }
#   month <- NULL
#   value1 <- NULL
#   value2 <- NULL
#   
#   for(i in 1:numMonths)
#   {
#     subtractMonths = numMonths-i
#     end2 <- as.Date(endDate)-months(subtractMonths)
#     start2 <- as.Date(end2)-months(1)
#     df_Monthly1 <- data.frame(date=df1[,dateColumn1], Value = df1[,valueColumn1])
#     df_Monthly1 <- df_Monthly1[df_Monthly1[,1]>=startDate & df_Monthly1[,1]<end2,]
#     df_Monthly1 <- df_Monthly1[order(df_Monthly1[,1]),]
#     df_Monthly1 <- AddCumulativeCol(df_Monthly1,2)
#     
#     df_Monthly2 <- data.frame(date=df2[,dateColumn2], Value = df2[,valueColumn2])
#     df_Monthly2 <- df_Monthly2[df_Monthly2[,1]>=startDate & df_Monthly2[,1]<end2,]
#     df_Monthly2 <- df_Monthly2[order(df_Monthly2[,1]),]
#     df_Monthly2 <- AddCumulativeCol(df_Monthly2,2)
#     
#     
#     month = c(month,format(start2,"%b %Y"))
#     value1 = c(value1,df_Monthly1[nrow(df_Monthly1),3])
#     value2 = c(value2,df_Monthly2[nrow(df_Monthly2),3])
#     
#   }
#   percent <-  (value1/value2)*100
#   df_MonthlyTable <- data.frame(Month=month)
#   df_MonthlyTable[,df1Name] = value1
#   df_MonthlyTable[,df2Name] = value2
#   df_MonthlyTable[,"PERCENT"] = percent
#   
#   return(df_MonthlyTable)
# }
# 
# MonthlyTable2 <- function(df1,df1Name,dateColumn1,valueColumn1,df2,df2Name,dateColumn2,valueColumn2, startDate,endDate)
# {
#   numMonths <- 0
#   endDateCounter <- endDate
#   while(endDateCounter>startDate)
#   {
#     endDateCounter <- as.Date(endDateCounter) - months(1)
#     numMonths <- numMonths+1
#   }
#   month <- NULL
#   value1 <- NULL
#   value2 <- NULL
#   
#   for(i in 1:numMonths)
#   {
#     subtractMonths = numMonths-i
#     end2 <- as.Date(endDate)-months(subtractMonths)
#     start2 <- as.Date(end2)-months(1)
#     df_Monthly1 <- data.frame(date=df1[,dateColumn1], Value = df1[,valueColumn1])
#     df_Monthly1 <- df_Monthly1[df_Monthly1[,1]>=start2 & df_Monthly1[,1]<end2,]
#     df_Monthly1 <- df_Monthly1[order(df_Monthly1[,1]),]
#     df_Monthly1 <- AddCumulativeCol(df_Monthly1,2)
#     
#     df_Monthly2 <- data.frame(date=df2[,dateColumn2], Value = df2[,valueColumn2])
#     df_Monthly2 <- df_Monthly2[df_Monthly2[,1]>=start2 & df_Monthly2[,1]<end2,]
#     df_Monthly2 <- df_Monthly2[order(df_Monthly2[,1]),]
#     df_Monthly2 <- AddCumulativeCol(df_Monthly2,2)
#     
#     
#     month = c(month,format(start2,"%b %Y"))
#     value1 = c(value1,df_Monthly1[nrow(df_Monthly1),3])
#     value2 = c(value2,df_Monthly2[nrow(df_Monthly2),3])
#     
#   }
#   percent <-  (value1/value2)*100
#   df_MonthlyTable <- data.frame(Month=month)
#   df_MonthlyTable[,df1Name] = value1
#   df_MonthlyTable[,df2Name] = value2
#   df_MonthlyTable[,"PERCENT"] = percent
#   
#   return(df_MonthlyTable)
# }




###############################################################################################
#PREPARE DATA#

#DOWNLOAD DATA
#df_fallon_released <- source_DropboxData(file="FALLON_RELEASED.csv", key = "eg4yqi3780vqkja", header = TRUE, cache = TRUE)
#df_fallon_sold_all <- source_DropboxData(file="FALLON_SOLD.csv", key = "3oe8vv9kb3p16y4", header = TRUE, cache = TRUE)
#df_fallon_quoted <- source_DropboxData(file="FALLON_QUOTED.csv", key="fe7g5p34ufpmnvr", header=TRUE, cache = TRUE)
#df_fallon_deck_quoted_all <- source_DropboxData(file = "FALLON_DECK QUOTED.csv", key ="j1yzoq33l20bm5a", header = TRUE, cache = TRUE)
#df_fallon_deck_sold <- source_DropboxData(file = "FALLON_DECK SOLD.csv", key = "hygxu9d0l4q8cxz", cache = TRUE)

df_fallon_released <- source_DropboxData(file="FALLON_RELEASED.csv", key = "eg4yqi3780vqkja", header = TRUE)
df_fallon_sold_all <- source_DropboxData(file="FALLON_SOLD.csv", key = "3oe8vv9kb3p16y4", header = TRUE)
df_fallon_quoted <- source_DropboxData(file="FALLON_QUOTED.csv", key="fe7g5p34ufpmnvr", header=TRUE)
df_fallon_deck_quoted_all <- source_DropboxData(file = "FALLON_DECK QUOTED.csv", key ="j1yzoq33l20bm5a", header = TRUE)
df_fallon_deck_sold <- source_DropboxData(file = "FALLON_DECK SOLD.csv", key = "hygxu9d0l4q8cxz")
df_colors <- source_DropboxData(file = "colors.csv", key = "7am08m32umo3lxh")
df_joist_quoted_map <- source_DropboxData(file = "df_quoted_map.csv", key = "lccwbr6g13qvt9h" )
df_joist_sold_map  <- source_DropboxData(file ="df_sold_map.csv", key =  "21n2dwta3kkon41")
  
# df_fallon_released <- read.csv("./data/FALLON_RELEASED.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
# df_fallon_sold_all <- read.csv("./data/FALLON_SOLD.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
# df_fallon_quoted <- read.csv("./data/FALLON_QUOTED.csv", header=TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
# df_fallon_deck_quoted_all <- read.csv("./data/FALLON_DECK QUOTED.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
# df_fallon_deck_sold <- read.csv("./data/FALLON_DECK SOLD.csv", header = TRUE,check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)

#df_cityData <- read.csv("./data/citydata.csv", header = TRUE, check.names =FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
#df_colors <- read.csv("./data/colors2.csv", header = FALSE, check.names =FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
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
# for(i in 1:length(df_fallon_sold_fallon[,1]))
# {
#   regexString <- paste("^",df_fallon_sold_fallon[i,"Job Number"],"$", sep="")
#   df_temp <- df_fallon_released[grep(regexString, df_fallon_released[,"Job Number"]),]
#   timeBetween <- mean(df_temp[,1])-df_fallon_sold_fallon[i,1]
#   if(grepl("A",df_fallon_sold_fallon[i,"Job Number"])==FALSE && grepl("R",df_fallon_sold_fallon[i,"Job Number"])==FALSE)
#   {
#     df_fallon_sold_fallon[i,"Days Untill Release"] <- timeBetween
#   }
# }
# rm(i)
# rm(regexString)
# rm(timeBetween)
# rm(df_temp)

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
#df_joist_quoted_map <- read.csv("./data/df_quoted_map.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
df_listUniqueTakeoffPerson <- data.frame(unique(df_joist_quoted_map$TakeoffPerson))
colnames(df_listUniqueTakeoffPerson) <- "TakeoffPerson"
df_listUniqueTakeoffPerson[,"Color"] <- df_colors[1:nrow(df_listUniqueTakeoffPerson),1]

df_joist_quoted_map[,"Date"] <- as.Date(df_joist_quoted_map[,"Date"], "%m/%d/%Y")
#df_joist_sold_map <- read.csv("./data/df_test.csv", header = TRUE, check.names=FALSE, stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
df_joist_sold_map[,"Date"] <- as.Date(df_joist_sold_map[,"Date"], "%m/%d/%Y")
colnames(df_joist_sold_map)[colnames(df_joist_sold_map)=="takeoffperson"] <- "TakeoffPerson"
#df_joist_quoted_map <- merge(df_listUniqueTakeoffPerson, df_joist_quoted_map, all.y=TRUE, by ="TakeoffPerson")
#df_joist_sold_map <- merge(df_listUniqueTakeoffPerson, df_joist_sold_map, all.y = TRUE, by = "TakeoffPerson")
# Define server logic required to draw a histogram







shinyServer(function(input, output, session) {
  
  output$myPlot <- renderPlot({


        start <- input$startDate
        end <- input$endDate
        start <- as.Date("2016-01-01")
        end <- as.Date("2017-01-01")
        
        soldJoist <- df_fallon_sold_all
        soldJoist <- soldJoist[soldJoist[,"Date"]>=start & soldJoist[,"Date"]<end,]
        soldJoist[,"Total Tons_Cumulative"] <- soldJoist[,"Total Tons_Cumulative"]-
          soldJoist[1,"Total Tons_Cumulative"]

        released <- df_fallon_released
        released <- released[released[,"Date"]>=start & released[,"Date"]<end,]
        released[,"Total Tons_Cumulative"] <- released[,"Total Tons_Cumulative"]-
          released[1,"Total Tons_Cumulative"]

        soldDeck <- df_fallon_deck_sold
        soldDeck <- soldDeck[soldDeck[,"Date"]>=start & soldDeck[,"Date"]<end,]
        soldDeck[,"Tons_Cumulative"] <- soldDeck[,"Tons_Cumulative"]-
          soldDeck[1,"Tons_Cumulative"]

        quotedJoist <- df_fallon_quoted
        quotedJoist <- quotedJoist[quotedJoist[,"Date"]>=start & quotedJoist[,"Date"]<end,]
        quotedJoist[,"Total Tons_Cumulative"] <- quotedJoist[,"Total Tons_Cumulative"]-
          quotedJoist[1,"Total Tons_Cumulative"]

        quotedDeck <- df_fallon_deck_quoted_all
        quotedDeck <- quotedDeck[quotedDeck[,"Date"]>=start & quotedDeck[,"Date"]<end,]
        quotedDeck[,"Tons_Cumulative"] <- quotedDeck[,"Tons_Cumulative"]-
          quotedDeck[1,"Tons_Cumulative"]


    releasedVsSold <- ggplot()+
      guides(color=guide_legend(title=NULL))+
      labs(title="Cumulative Tons") +
      xlab("Date") + ylab("Tons") +
      ggthemes::theme_gdocs()


    graphs <- input$graphs

    if('Joist Released' %in% graphs == TRUE)
    {
      releasedVsSold = releasedVsSold + geom_line(data=released, aes(released[,"Date"], released[,"Total Tons_Cumulative"], color="JOIST RELEASED"), size=1)
    }

    if('Joist Sold' %in% graphs == TRUE)
    {
      releasedVsSold = releasedVsSold + geom_line(data=soldJoist, aes(soldJoist[,"Date"], soldJoist[,"Total Tons_Cumulative"], color="JOIST SOLD"),size=1)
    }
    if('Joist Quoted' %in% graphs == TRUE)
    {
      releasedVsSold = releasedVsSold + geom_line(data=quotedJoist, aes(quotedJoist[,"Date"], quotedJoist[,"Total Tons_Cumulative"], color="JOIST QUOTED"), size=1)
    }
    if('Deck Sold' %in% graphs == TRUE)
    {
      releasedVsSold = releasedVsSold + geom_line(data=soldDeck, aes(soldDeck[,"Date"], soldDeck[,"Tons_Cumulative"], color="DECK SOLD"), size=1)
    }
    if('Deck Quoted' %in% graphs == TRUE)
    {
      releasedVsSold = releasedVsSold + geom_line(data=soldDeck, aes(soldDeck[,"Date"], soldDeck[,"Tons_Cumulative"], color="DECK SOLD"), size=1)
    }

    releasedVsSold


  })
  
  output$myBarPlot <- renderPlot({
    
    
    start <- input$startDate
    end <- input$endDate
    interval <- input$interval
    
    
    soldJoist <- df_fallon_sold_all
    soldJoist <- soldJoist[soldJoist[,"Date"]>=start & soldJoist[,"Date"]<=end,]
    soldJoist[,"Total Tons_Cumulative"] <- soldJoist[,"Total Tons_Cumulative"]-
      soldJoist[1,"Total Tons_Cumulative"]
    #soldJoist_inter <- SplitByInterval(soldJoist,dateColumn="Date", valueColumn = "Total Tons", interval = interval)
    #soldJoist_inter[,"TYPE"] <- "soldJoist"
    #colnames(soldJoist_inter) <- c("DATE", "TONS")
    
    released <- df_fallon_released
    released <- released[released[,"Date"]>=start & released[,"Date"]<=end,]
    released[,"Total Tons_Cumulative"] <- released[,"Total Tons_Cumulative"]-
      released[1,"Total Tons_Cumulative"]
    #released_inter <- SplitByInterval(released,dateColumn="Date", valueColumn = "Total Tons", interval = interval)
    #released_inter[,"TYPE"] <- "released"
    #colnames(released_inter) <- c("DATE", "TONS")
    
    soldDeck <- df_fallon_deck_sold
    soldDeck <- soldDeck[soldDeck[,"Date"]>=start & soldDeck[,"Date"]<=end,]
    soldDeck[,"Tons_Cumulative"] <- soldDeck[,"Tons_Cumulative"]-
      soldDeck[1,"Tons_Cumulative"]
    #soldDeck_inter <- SplitByInterval(soldDeck,dateColumn="Date", valueColumn = "Tons", interval = interval)
    #soldDeck_inter[,"TYPE"] <- "soldDeck"
    #colnames(soldDeck_inter) <- c("DATE", "TONS")
    
    quotedJoist <- df_fallon_quoted
    quotedJoist <- quotedJoist[quotedJoist[,"Date"]>=start & quotedJoist[,"Date"]<=end,]
    quotedJoist[,"Total Tons_Cumulative"] <- quotedJoist[,"Total Tons_Cumulative"]-
      quotedJoist[1,"Total Tons_Cumulative"]
    #quotedJoist_inter <- SplitByInterval(quotedJoist,dateColumn="Date", valueColumn = "Total Tons", interval = interval)
    #quotedJoist_inter[,"TYPE"] <-  "quotedJoist"
    #colnames(quotedJoist_inter) <- c("DATE", "TONS")
    
    quotedDeck <- df_fallon_deck_quoted_all
    quotedDeck <- quotedDeck[quotedDeck[,"Date"]>=start & quotedDeck[,"Date"]<=end,]
    quotedDeck[,"Tons_Cumulative"] <- quotedDeck[,"Tons_Cumulative"]-
      quotedDeck[1,"Tons_Cumulative"]
    #quotedDeck_inter <- SplitByInterval(quotedDeck,dateColumn="Date", valueColumn = "Tons", interval = interval)
    #quotedDeck_inter[,"TYPE"] <- "quotedDeck"
    #colnames(quotedDeck_inter) <- c("DATE", "TONS")
    
    
    

    
    barPlot <- ggplot()+
      guides(color=guide_legend(title=NULL))+
      xlab("Date") + ylab("Tons") +
      ggthemes::theme_gdocs()
    
    
    graphs <- input$graphs
    
    plotTitle <- ""
    if (input$interval=="daily"){plotTitle <- "Daily Tons"}
    if (input$interval=="weekly"){plotTitle <- "Weekly Tons"}
    if (input$interval=="monthly"){plotTitle <- "Monthly Tons"}
    if (input$interval=="quarterly"){plotTitle <- "Quarterly Tons"}
    if (input$interval=="yearly"){plotTitle <- "Yearly Tons"}
    
    if('Joist Released' %in% graphs == TRUE)
    {
      released <- df_fallon_released
      released <- released[released[,"Date"]>=start & released[,"Date"]<=end,]
      released <- CreateIntervalColumn(released,"Date", input$interval)
      barPlot = barPlot +geom_bar(data=released, aes(released_inter[,"Interval"], released_inter[,"Total Tons"], color="JOIST RELEASED"), size = 1,stat = "identity", position = "dodge")
    }
    
    if('Joist Sold' %in% graphs == TRUE)
    {
      barPlot = barPlot + stat_summary()
        geom_bar(data=soldJoist_inter, aes(soldJoist_inter[,1], soldJoist_inter[,2], color="JOIST SOLD"), size = 1, stat = "identity", position = "dodge")
    }
    if('Joist Quoted' %in% graphs == TRUE)
    {
      barPlot = barPlot +geom_bar(data=quotedJoist_inter, aes(quotedJoist_inter[,1], quotedJoist_inter[,2], color="JOIST QUOTED"), size = 1, stat = "identity", position = "dodge")
    }
    if('Deck Sold' %in% graphs == TRUE)
    {
      barPlot = barPlot +geom_bar(data=soldDeck_inter, aes(soldDeck_inter[,1], soldDeck_inter[,2], color="DECK SOLD"), size = 1, stat = "identity", position = "dodge")
    }
    if('Deck Quoted' %in% graphs == TRUE)
    {
      barPlot = barPlot +geom_bar(data=quotedDeck_inter, aes(quotedDeck_inter[,1], quotedDeck_inter[,2], color="DECK QUOTED"), size = 1, stat = "identity", position = "dodge")
    }
    barPlot <-  barPlot +
      labs(title=plotTitle)
    
    barPlot
    
  })
  
  output$map <- renderLeaflet({

    if(input$whatToMap == "joistQuoted")
    {
      df_test <- df_joist_quoted_map
    }
    if(input$whatToMap == "joistSold")
    {
      df_test <- df_joist_sold_map
    }
    start_map <- input$startDate_Map
    end_map <- input$endDate_Map
    df_test <- df_test[df_test[,"Date"]>=start_map & df_test[,"Date"]<end_map,]
    df_listUniqueTakeoffPerson <- data.frame(unique(df_test$TakeoffPerson))
    colnames(df_listUniqueTakeoffPerson) <- "TakeoffPerson"
    df_listUniqueTakeoffPerson[,"Color"] <- df_colors[1:nrow(df_listUniqueTakeoffPerson),1]
    df_test <- merge(df_listUniqueTakeoffPerson, df_test, all.y=TRUE, by ="TakeoffPerson")
    m <- leaflet(data = df_test[,]) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      #addMarkers(~LONG, ~LAT, popup = as.character(paste(df_test$`Job Name`,df_test$`Total Tons`, sep="\n")) )
      addCircles(lng = df_test$LONG, lat = df_test$LAT,color=~df_test$`Color`, radius = ~160*df_test$`Total Tons`^.7, popup = as.character(paste(df_test$`Job Name`,df_test$`Total Tons`, sep="\n")) )%>%
      addLegend(position = "bottomright", labels=~df_listUniqueTakeoffPerson$`TakeoffPerson`, colors = ~df_listUniqueTakeoffPerson$`Color`) %>%
      addProviderTiles("CartoDB.Positron")
    m
  })

})

