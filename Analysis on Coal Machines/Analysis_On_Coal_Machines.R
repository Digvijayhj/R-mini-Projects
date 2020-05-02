# List in R

# Deliverable - a list with the folloewing components:

#• Character: Machine name 
#• Vector: (min, mean, max) utilisation for the month (excluding unknown hours)
#• Logical: Has utilisation ever fallen below 90%? TRUE/ FALSE 
#• Vector: All hours where utilisation is unknown (NA's)
#• Dataframe: For this machine 
#• Plot: For all machines


getwd()

util <- read.csv("P3-Machine-Utilization.csv")
backup <- util

head(util, 20)
str(util)
summary(util)


#------------------------------------------------------

#Adding new column 
#Derive uilization column

util$Utilization = 1 - util$Percent.Idle
head(util,12)


#---------------------------------------------------------

# Handling Date and time
# Use ?POSIXct to get std format of dates
# tail(util) hard to understand which format the date is!

util$PosixTime <- as.POSIXct(util$Timestamp, format = "%d/%m/%Y %H:%M")

# removing the column from data frame
util$Timestamp <- NULL

# rearranging the dataframe columns
util <- util[,c(4,1,2,3)]
head(util,12)

#------------------------------------------------------------------

# What is a list? ANSWER: List can contain any type of data type elements unlike vectors

summary(util)
str(util)

# checking on one machine
RL1 <- util[util$Machine == "RL1",]
summary(RL1)

RL1$Machine <- factor(RL1$Machine)
summary(RL1)

# Construct List:
#• Character: Machine name 
#• Vector: (min, mean, max) utilisation for the month (excluding unknown hours)
#• Logical: Has utilisation ever fallen below 90%? TRUE/ FALSE 

# creating vector
util_stats_rl1 <- c(min(RL1$Utilization, na.rm = T),
                    mean(RL1$Utilization, na.rm = T),
                    max(RL1$Utilization, na.rm = T))
util_stats_rl1

# creating flag (TRUE/FALSE)

util_under_90_flag <- length(which(RL1$Utilization < 0.90)) > 0
util_under_90_flag

# Creating List
list_rl1 <- list("RL1", util_stats_rl1, util_under_90_flag) 
list_rl1

#----------------------------------------------------------------------

# Naming component of a list

list_rl1

names(list_rl1)
names(list_rl1) <- c("Machines", "Stats", "LowThreshold")
list_rl1

# Another way of naming the component of a list

rm(list_rl1)
list_rl1

lsit_rl1 <- list(Machine="RL1", Stats=util_stats_rl1, LowThreshold=util_under_90_flag)
list_rl1

#---------------------------------------------------------------------------

# Extract components of a list
# 3 ways
# 1 [] - will always return a list
# 2 [[]] - will return actual object
# 3 $ - same as [[]] BUT prettier

list_rl1[1]
list_rl1[[2]][3]
list_rl1$Stats[3]
lsit_rl1$Machine
list_rl1$Stats

#---------------------------------------------------------------------------

# adding component to list and deleting the component
lsit_rl1


list_rl1$UnknownHours <- RL1[is.na(RL1$Utilization), "PosixTime"]
list_rl1

# adding data frame

list_rl1$Data <- RL1
list_rl1

summary(lsit_rl1)
str(list_rl1)

#----------------------------------------------------------------------------

# Subsetting a list

list_rl1[1:3]
list_rl1[c(1,4)]

sublist_rl1 <- list_rl1[c("Machines","Stats")]
sublist_rl1
sublist_rl1[[2]][2]

# Note: Double square bracket is NOT for Subsetting [ ex: sublist_rl1[[1:3]] ]

#----------------------------------------------------------------------------

#Building a Timeseries plot

library(ggplot2)

p <- ggplot(data=util)

myplot <- p + 
          geom_line( aes(x=PosixTime, y=Utilization, colour=Machine), size=1.2)  + 
          facet_grid(Machine~.) + 
          geom_hline(yintercept = 0.90, colour="Gray", size=1.2, linetype=3)

list_rl1$Plot <- myplot
list_rl1

summary(list_rl1)
str(list_rl1)
