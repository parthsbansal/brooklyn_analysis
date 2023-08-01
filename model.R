# MSCA 31007. Statistical Analysis. Autumn 2022
# Parth Bansal 
# Final Assignment. Part 01.

# Note: Many lines of code were used for EDA and develop a better understanding of the dataset. However, some of them are commented out for clarity while working on the project. 

# install.packages("stringr")
# install.packages ("Hmisc")
# install.packages("dplyr")
# install.packages ("magrittr")

library(dplyr)
library(ggplot2)
library(stringr)
library(ggspatial)
library(ggthemes)
library(janitor)
library(reshape2)
library(tidyr)
library(data.table)
library(Hmisc)
library(zoo)

## Importing data into R
cnames <- c("borough", "neighborhood", "bldclasscat", "taxclasscurr", "block", "lot", "easement", "bldclasscurr", "address", "aptnum", "zip", "resunits", "comunits", "totunits", "landsqft", "grosssqft", "yrbuilt", "taxclasssale", "bldclasssale", "price", "date")

df_2016 <- read.csv("/Users/parthbansal/Downloads/2016_brooklyn.csv", skip = 4, col.names = cnames)
df_2017 <- read.csv("/Users/parthbansal/Downloads/2017_brooklyn.csv", skip = 4, col.names = cnames)
df_2018 <- read.csv("/Users/parthbansal/Downloads/2018_brooklyn.csv", skip = 4, col.names = cnames)
df_2019 <- read.csv("/Users/parthbansal/Downloads/2019_brooklyn.csv", skip = 4, col.names = cnames)
df_2020 <- read.csv("/Users/parthbansal/Downloads/2020_brooklyn.csv", skip = 6, col.names = cnames)

# Changing date to correct format
df_2016 <- data.table(df_2016)
df_2016 <- df_2016[ , date := as.Date(date,format = "%m/%d/%Y")]
df_2017 <- data.table(df_2017)
df_2017 <- df_2017[ , date := as.Date(date,format = "%m/%d/%y")]
df_2018 <- data.table(df_2018)
df_2018 <- df_2018[ , date := as.Date(date,format = "%m/%d/%y")]
df_2019 <- data.table(df_2019)
df_2019 <- df_2019[ , date := as.Date(date,format = "%m/%d/%y")]
df_2020 <- data.table(df_2020)
df_2020 <- df_2020[ , date := as.Date(date,format = "%m/%d/%y")]

# Changing year from "0017" to "2017"
# df_2017$year <- "2017"
# df_2018$year <- "2018"
# df_2019$year <- "2019"
# df_2020$year <- "2020"

# Removing "$" character in price in 2018
df_2018$price = (gsub("\\$", "", df_2018$price))

# Binding (combining) all 5 csv files
df <- rbind (df_2016, df_2017, df_2018, df_2019, df_2020)

head(df, 10)
summary(df)
sapply(df,class)

# ------------------------------------------------------------------------------

# Removin comas in these data columns 
df$landsqft <- gsub(",", "", df$landsqft)
df$grosssqft <- gsub(",", "", df$grosssqft)
df$price <- gsub(",", "", df$price)

# Trimming white space 
df$neighborhood <- gsub("\\s+", "", df$neighborhood)
df$bldclasscat <- gsub("\\s+", "", df$bldclasscat)
df$taxclasscurr <- gsub("\\s+", "", df$taxclasscurr)
df$taxclasssale <- gsub("\\s+", "", df$taxclasssale)
df$bldclasscurr <- gsub("\\s+", "", df$bldclasscurr)
df$bldclasssale <- gsub("\\s+", "", df$bldclasssale)
df$address <- gsub("\\s+", "", df$address)

# Adding year, quarter, month, day(date) columns for further analysis 
df$year <- format(as.Date(df$date, format="%Y-%m-%d"),"%Y")
df$month <- format(as.Date(df$date, format="%Y-%m-%d"),"%m")
df$day <- format(as.Date(df$date, format="%Y-%m-%d"),"%d")
df$quarter <- as.yearqtr(df$date, format = "%Y-%m-%d")
df$quarter
unique(df$quarter)

sapply(df, class)

# Converting various types to numeric/ double
df$price <- as.numeric(df$price)
df$landsqft <- as.numeric(df$landsqft)
df$grosssqft <- as.numeric(df$grosssqft)
df$resunits <- as.numeric(df$resunits)
df$comunits <- as.numeric(df$comunits)
df$totunits <- as.numeric(df$totunits)
df$taxclasscurr <- as.numeric(df$taxclasscurr)
df$taxclasssale <- as.numeric(df$taxclasssale)
df$year <- as.numeric(df$year)
df$month <- as.numeric(df$month)
df$day <- as.numeric(df$day)

# Exporting combined csv
# write.csv(df, "/Users/parthbansal/Desktop/a.csv", row.names=FALSE)

# Cleaning data based on project expectations
remove_empty(df, "rows")
df <- subset(df, grepl("^A|^R", df$bldclasssale))
df <- df[(df$totunits == 1) & (df$resunits == 1)]
df <- filter(df, grosssqft > 0)
df <- filter(df, !is.na(price))
df <- filter(df, !(zip == 0))
count(df)

# At the moment, the row count is 19,639

# Commuting year since the properety was built
df$since <- as.numeric(df$year) - df$yrbuilt

# Setting values where df$since == df$yrbuilt to 0 
df$since[df$since == 2016] <- 0
df$since[df$since == 2017] <- 0
df$since[df$since == 2018] <- 0
df$since[df$since == 2019] <- 0
df$since[df$since == 2020] <- 0

#Dropping columns that won't be needed for further analysis or our model. Two of these columns are empty and the remaining ones have the same data or don't quantifiable data.
df <- subset(df, select = -c(borough, bldclasscat,  resunits, comunits, totunits, address, easement, aptnum, bldclasssale))
sapply(df, class)
count(df)

# write.csv(df, "/Users/parthbansal/Desktop/combined.csv", row.names=FALSE)

# ------------------------------------------------------------------------------

# Performing price EDA to gain better understanding of price distribution
df <- filter(df, !(price == 0))
# count(df)
temp_price <- sort(df$price, decreasing = FALSE, na.last = TRUE)

# Commuting log price to normalize and get better insight of price points
df$logp <- log(df$price)

# Visualizing price and log price
ggplot(df, aes(x = price)) + geom_histogram(aes( y = ..density..), binwidth = .75, colour ="blue", fill = "blue") + geom_density(alpha=.5, fill="#3361FF") 
ggplot(df, aes(x = logp)) + geom_histogram(aes( y = ..density..), binwidth = .5, colour ="blue", fill = "white") + geom_density(alpha=.5, fill="#3361FF") 

# Plots to eye relations between price and listed factors 
plot(df$price, df$year)
plot(df$price, df$landsqft) 
plot(df$price, df$grosssqft)
plot(df$price, df$zip)    
plot(df$price, df$yrbuilt)

# summary(temp_price)
#df <- subset(df, select = -c(logp))

# Filtering out prices below $15,000 (based on primary research) as those prices are very unlikely market prices.
# I wanted to further cut down prices from $50,000. However, I didn't wish to reduce the no. of rows any further
df <- subset(df, df$price >= 15000)
count(df)
sapply(df, class)

# Visualizing various 'factor' relations with price. No longer needed
# ggplot(df, aes(as.factor(neighborhood),price)) + geom_point()
# ggplot(df, aes(as.factor(taxclasscurr),price)) + geom_point()
# ggplot(df, aes(as.factor(taxclasssale),price)) + geom_point()
# ggplot(df, aes(as.factor(bldclasscurr),price)) + geom_point()

# At the moment, the row count is 13,568 

# ------------------------------------------------------------------------------

# Finding the number of unique neighborhoods in our df and sorting it alphabetically 
temp_n <- unique(df$neighborhood)
temp_n <- sort(temp_n, decreasing = FALSE)
temp_n

# All unique neighborhoods in our dataset
# [1] "BATHBEACH"               "BAYRIDGE"                "BEDFORDSTUYVESANT"       "BENSONHURST"             "BERGENBEACH"             "BOERUMHILL"              "BOROUGHPARK"            
# [8] "BRIGHTONBEACH"           "BROOKLYNHEIGHTS"         "BROWNSVILLE"             "BUSHTERMINAL"            "BUSHWICK"                "CANARSIE"                "CARROLLGARDENS"         
# [15] "CLINTONHILL"             "COBBLEHILL"              "COBBLEHILL-WEST"         "CONEYISLAND"             "CROWNHEIGHTS"            "CYPRESSHILLS"            "DOWNTOWN-FULTONFERRY"   
# [22] "DOWNTOWN-FULTONMALL"     "DOWNTOWN-METROTECH"      "DYKERHEIGHTS"            "EASTNEWYORK"             "FLATBUSH-CENTRAL"        "FLATBUSH-EAST"           "FLATBUSH-LEFFERTSGARDEN"
# [29] "FLATBUSH-NORTH"          "FLATLANDS"               "FORTGREENE"              "GERRITSENBEACH"          "GOWANUS"                 "GRAVESEND"               "GREENPOINT"             
# [36] "KENSINGTON"              "MADISON"                 "MANHATTANBEACH"          "MARINEPARK"              "MIDWOOD"                 "MILLBASIN"               "NAVYYARD"               
# [43] "OCEANHILL"               "OCEANPARKWAY-NORTH"      "OCEANPARKWAY-SOUTH"      "OLDMILLBASIN"            "PARKSLOPE"               "PARKSLOPESOUTH"          "PROSPECTHEIGHTS"        
# [50] "REDHOOK"                 "SEAGATE"                 "SHEEPSHEADBAY"           "SPRINGCREEK"             "SUNSETPARK"              "WILLIAMSBURG-CENTRAL"    "WILLIAMSBURG-EAST"      
# [57] "WILLIAMSBURG-NORTH"      "WILLIAMSBURG-SOUTH"      "WINDSORTERRACE"          "WYCKOFFHEIGHTS"      
# 60 unique neighborhoods

# Segmenting neighborhoods based on geographical location 
n1 = c("GREENPOINT")
n2 = c("BUSHWICK")
n3 = c("BEDFORDSTUYVESANT", "OCEANHILL")
n4 = c("BROOKLYN HEIGHTS", "DUMBO", "VINEGARHILL", "DOWNTOWN-FULTONMALL", "BOERUMHILL", "FORTGREENE")
n5 = c("EASTNEWYORK", "CYPRESSHILLS")
n6 = c("REDHOOK", "CARROLLGARDENS", "PARKSLOPE", "GOWANUS")
n7 = c("COBBLEHILL", "COBBLEHILL-WEST", "CARROLLGARDENS")
n8 = c("SUNSETPARK", "WINDSORTERRACE")
n9 = c("CROWNHEIGHTS", "PROSPECTHEIGHTS")
n10 = c("BAYRIDGE", "DYKERHEIGHTS")
n11 = c("BATHBEACH", "BENSONHURST", "GRAVESEND")
n12 = c("BOROUGHPARK", "KENSINGTON", "MIDWOOD")
n13 = c("OCEANPARKWAY-NORTH", "OCEANPARKWAY-SOUTH")
n14 = c("CONEYISLAND", "BRIGHTONBEACH", "SEAGATE" )
n15 = c("FLATBUSH-CENTRAL", "FLATBUSH-EAST", "FLATBUSH-LEFFERTSGARDEN", "FLATBUSH-NORTH")
n16 = c("SHEEPSHEADBAY", "MANHATTANBEACH", "GERRITSENBEACH", "MADISON")
n17 = c("BROWNSVILLE")
n18 = c("CANARSIE", "BERGENBEACH", "MILLBASIN", "FLATLANDS", "MARINEPARK", "SPRINGCREEK" )
n19 = c("WILLIAMSBURG-CENTRAL", "WILLIAMSBURG-EAST", "WILLIAMSBURG-NORTH", "WILLIAMSBURG-SOUTH", "WYCKOFFHEIGHTS")
n20 = c("NAVYYARD", "DOWNTOWN-FULTONFERRY", "DOWNTOWN-METROTECH", "CLINTONHILL")

# Setting bins
df_model <- df %>% mutate(neighborhood_factors = case_when(neighborhood %in% n1 ~ "n1", neighborhood %in% n2 ~ "n2", neighborhood %in% n3 ~ "n3", neighborhood %in% n4 ~ "n4", neighborhood %in% n5 ~ "n5", neighborhood %in% n6 ~ "n6", neighborhood %in% n7 ~ "n7", neighborhood %in% n8 ~ "n8", neighborhood %in% n9 ~ "n9", neighborhood %in% n10 ~ "n10", neighborhood %in% n11 ~ "n11", neighborhood %in% n12 ~ "n12", neighborhood %in% n13 ~ "n13", neighborhood %in% n14 ~ "n14", neighborhood %in% n15 ~ "n15", neighborhood %in% n16 ~ "n16", neighborhood %in% n17 ~ "n17", neighborhood %in% n18 ~ "n18", neighborhood %in% n19 ~ "n19", neighborhood %in% 20 ~ "n20", TRUE ~ "NA"))
sapply(df, class)

# Calculating difference in year between csv year and year built 
df_model$difference <- as.numeric(df_model$year) - df_model$yrbuilt

# Best model attempts (R < 0.6)
# m1 = lm(formula = log(price) ~ factor(neighborhood_factors) + sqrt(grosssqft) +  zip + difference + taxclasssale + landsqft + I(difference^2), data = df_model)
# summary(best_model)
# m2 = lm(formula = sqrt(price) ~ factor(neighborhood_factors) + sqrt(grosssqft) + block + zip  + taxclasssale + landsqft + yrbuilt + lot, data = df_model)
# summary(best_model)

# Best running model after multiple attempts 
best_model = lm(formula = sqrt(price) ~ factor(neighborhood_factors) + sqrt(grosssqft) + block + zip + difference + taxclasssale + landsqft + yrbuilt + I(difference^2) + lot, data = df_model)
summary(best_model)      

# Adjusted R-squared:  0.6017,  DFs 28, Row Count 13,539

#Communting RMSE
RSS <- c(crossprod((best_model$residuals)^2))
MSE <- RSS / length(best_model$residuals)
RMSE <- sqrt(MSE)
RMSE

# RMSE ~ 144,478

# Model test 
require(lmtest)
hist(best_model$residuals)
ks.test(best_model$residuals / summary(best_model)$sigma, pnorm)

plot(best_model$fitted.values, best_model$residuals)
bptest(best_model)

# End of Part 1

# ------------------------------------------------------------------------------

# MSCA 31007. Statistical Analysis. Autumn 2022
# Parth Bansal 
# Final Assignment. Part 02.

# Note: Many lines of code were used for EDA and develop a better understanding of the dataset. However, some of them are commented out for clarity while working on the project.

# Reinspecting dataframe from part 1
head(df_model, 10)

# Commuted quarter in Part 1 beginning and stored under column best_model$quarter
unique(df_model$quarter)

# df_model$quarter variable is type yearqtr. Parsing to a factor 
sapply(df_model, class)
df_model$quarter <- as.factor(df_model$quarter)
class(df_model$quarter) #factor


# The levels of a factor quarter are re-ordered so that the level "2020 Q3" is first 
df_model$quarter <- relevel(df_model$quarter, ref = "2020 Q3")

# Rerunning my model with quarter as a factor and removing interaction term 
best_model_2 = lm(formula = sqrt(price) ~ factor(neighborhood_factors) + sqrt(grosssqft) + block + zip + difference + taxclasssale + landsqft + quarter + lot, data = df_model)
summary(best_model_2)

# Adjusted R-squared:  0.6026

# Graphing residuals
plot(best_model_2)

write.csv(df_model, "/Users/parthbansal/Desktop/a.csv", row.names=FALSE)



