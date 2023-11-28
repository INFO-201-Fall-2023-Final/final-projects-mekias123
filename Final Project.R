#Mekias Kebede
#11/27/2023
#INFO201 Final Project - Data Wrangling
#I worked on this project all on my own. 

library("stringr")
library(testthat)
library(dplyr)
library(ggplot2)
library(readxl)

#Reading in Data (4 data sets used in my analysis)
setwd("/Users/mekiaskebede/Desktop/INFO201/Data")
microsoft <- na.omit(read.csv("MSFT.csv"))
google <- na.omit(read.csv("Google.csv"))
apple <- na.omit(read.csv("AAPL.csv"))
poverty <- na.omit(read_excel("hstpov13.xlsx"))

#Data Cleaning & Augmentation
yr1 <- vector()
for (i in 1:nrow(microsoft)) {
  str_date_microsoft <- microsoft[i,1]
  for (k in 1986:2022) {
    if (str_detect(str_date_microsoft, toString(k)) == TRUE){
      yr1 <- append(yr1, k)
    } 
  }
}
yr2 <- vector()
for (i in 1:nrow(google)) {
  str_date_google <- google[i,1]
  for (k in 2004:2022) {
    if (str_detect(str_date_google, toString(k)) == TRUE){
      yr2 <- append(yr2, k)
    } 
  }
}
yr3 <- vector()
for (i in 1:nrow(apple)) {
  str_date_apple <- apple[i,1]
  for (k in 1980:2022) {
    if (str_detect(str_date_apple, toString(k)) == TRUE){
      yr3 <- append(yr3, k)
    } 
  }
}
#Created a unified/joined data set (df2 - 4,431 rows)
microsoft <- mutate(microsoft, yr = yr1)
google <- mutate(google, yr = yr2)
apple <- mutate(apple, yr = yr3)
df <- merge(x = google, y = microsoft, by = "Date")
df2 <- merge(x = df, y = apple, by = "Date")
df2 <- df2[,c("Date", "yr", "High.x", "Volume.x", "High.y", "Volume.y", "High", "Volume")]

# Renaming column names for clarity
names(df2)[3] <- "High.google"
names(df2)[4] <- "Volume.google"
names(df2)[5] <- "High.microsoft"
names(df2)[6] <- "Volume.microsoft"
names(df2)[7] <- "High.apple"
names(df2)[8] <- "Volume.apple"

#Dollar volume liquidity (new continuous/numerical variable)
DVL.google <- df2$High.google * df2$Volume.google
DVL.microsoft <- df2$High.microsoft * df2$Volume.microsoft
DVL.apple <- df2$High.apple * df2$Volume.apple
df2$DVL.google <- DVL.google
df2$DVL.microsoft <- DVL.microsoft
df2$DVL.apple <- DVL.apple

# DVL category (new categorical variable)
DVL.google.category <- vector()
for (i in 1:length(df2$DVL.google)) {
  if(df2$DVL.google[i] >= 1000000000){
    DVL.google.category <- append(DVL.google.category, "High DVL") 
  } else if(df2$DVL.google[i] < 1000000000) {
    DVL.google.category <- append(DVL.google.category, "Low DVL")  
  }
}
DVL.microsoft.category <- vector()
for (i in 1:length(df2$DVL.microsoft)) {
  if(df2$DVL.microsoft[i] >= 1000000000){
    DVL.microsoft.category <- append(DVL.microsoft.category, "High DVL") 
  } else if(df2$DVL.microsoft[i] < 1000000000) {
    DVL.microsoft.category <- append(DVL.microsoft.category, "Low DVL")  
  }
}
DVL.apple.category <- vector()
for (i in 1:length(df2$DVL.apple)) {
  if(df2$DVL.apple[i] >= 1000000000){
    DVL.apple.category <- append(DVL.apple.category, "High DVL") 
  } else if(df2$DVL.apple[i] < 1000000000) {
    DVL.apple.category <- append(DVL.apple.category, "Low DVL")  
  }
}
df2 <- mutate(df2, DVL.google.cat = DVL.google.category)
df2 <- mutate(df2, DVL.microsoft.cat = DVL.microsoft.category)
df2 <- mutate(df2, DVL.apple.cat = DVL.apple.category)

# summarize data frame (average stock price high and volume)
grouped <- group_by(df2, yr)
df2_high <- summarize(grouped, avg_High.google = mean(High.google), avg_High.microsoft = mean(High.microsoft), avg_High.apple = mean(High.apple))
df2_vol <- summarize(grouped, avg_vol.google = mean(Volume.google), avg_vol.microsoft = mean(Volume.microsoft), avg_vol.apple = mean(Volume.apple))

# homeless/poverty data in relation to tech company stock data
names(poverty)[1] <- "yr"
poverty$yr[3] <- "2020"
poverty$yr[6] <- "2017"
poverty$yr[11] <- "2013"
poverty$yr[12] <- "2013"
poverty$yr[15] <- "2010"
poverty$yr[21] <- "2004"
poverty <- poverty[1:21,]
poverty$yr <- as.numeric(poverty$yr)
names(poverty)[2] <- "total_poverty (thousands)"
poverty <- poverty[,c("yr", "total_poverty (thousands)")]
poverty <- poverty[-c(7, 11),]

# Merging poverty data into summarized data frame
df3_pov <- merge(x = df2_high, y = poverty, by = "yr")