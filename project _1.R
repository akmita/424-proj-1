

# 1. load and concatenate data into single frame
d1 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_1.tsv", sep = "\t", header = TRUE)
d2 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_2.tsv", sep = "\t", header = TRUE)
d3 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_3.tsv", sep = "\t", header = TRUE)
d4 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_4.tsv", sep = "\t", header = TRUE)
d5 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_5.tsv", sep = "\t", header = TRUE)
d6 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_6.tsv", sep = "\t", header = TRUE)
d7 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_7.tsv", sep = "\t", header = TRUE)
d8 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_8.tsv", sep = "\t", header = TRUE)
d9 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_9.tsv", sep = "\t", header = TRUE)
d10 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_10.tsv", sep = "\t", header = TRUE)
d11 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_11.tsv", sep = "\t", header = TRUE)
				
# 2. fix messed up coluymn names
names(d1)<-c("station_id","stationname", "date", "daytype", "rides")

# 3. concatenate all sets
D <- rbind(D, d11)

# 4. convert dates
library(lubridate)
D_date_formatted <- D
D_date_formatted$date <- as.Date(D$date, '%m-%d-%y')
D_date_formatted$date <- mdy(D$date)

# 5. get only UIC data
D_UIC <- subset(D_date_formatted, station_id == 40350)

# 6. put years into separate column should be 7643 rows
D_UIC$year <- format(D_UIC$date, format="%Y")
D_UIC$month <- format(D_UIC$date, format="%m")
D_UIC$day <- format(D_UIC$date, format="%d")

# 7. aggregate and count
# library(sqldf)
# test <- sqldf("SELECT COUNT(*) FROM D_UIC AS count GROUP BY years")
UIC_agg <- aggregate(rides~year,D_UIC,sum)



# 
# 8. start graphin'
# 
library(ggplot2)
ggplot(data=UIC_agg, aes(x=year, y=rides)) + geom_bar(stat="identity") 



save.image("~/FALL 2022 offline/CS 424 offline/project 1/proj_1_workspace.RData")


# 
# playing around
# remove years from UIC dates, then use years as aggegates below
# 
D_u_aggr <- aggregate(D_UIC, by=list(D_UIC$year), FUN=length)

D_fac <- factor(D_UIC)


options(max.print = 1000)
getOption("max.print")
print(UIC_agg)
print(D_UIC)
typeof(D_UIC$years)

data("midwest", package = "ggplot2")
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()


