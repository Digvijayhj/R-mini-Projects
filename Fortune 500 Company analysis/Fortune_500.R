getwd()
#------------------------------------------------------------
#read the csv file

fin <- read.csv("P3-Future-500-The-Dataset.csv")
fin

# make a backup file
fin_backup <- fin


#replace empty files with NA
fin <- read.csv("P3-Future-500-The-Dataset.csv",na.strings = c(""))
head(fin,24)

#remove special and and unwanted informations

fin$Expenses <- gsub("Dollars","",fin$Expenses)
fin$Expenses <- gsub(",","",fin$Expenses)
fin$Revenue <- gsub(",","",fin$Revenue)
fin$Revenue <- gsub("\\$","",fin$Revenue)
fin$Growth <- gsub("\\%","",fin$Growth)
head(fin)
str(fin)
fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Profit <- as.numeric(fin$Profit)
fin$Growth <- as.numeric(fin$Growth)
str(fin)

fin[is.na(fin$Industry),] 

# removing NA records of industries
fin <- fin[!is.na(fin$Industry),]
fin

# Reordering the indexs
rownames(fin) <- NULL
head(fin,20)

#---------------------------------------------------

# replacing the NA

fin[!complete.cases(fin),]
fin[is.na(fin$State),]

fin[is.na(fin$State) & fin$City == "New York", "State"] <- "NY"
fin[is.na(fin$State) & fin$City == "San Francisco", "State"] <- "CA"

fin[which(fin$City == "San Francisco"),]
fin[which(fin$City == "New York"),]
fin[c(11,377),]
fin[c(82,265),]

#-----------------------------------------------------

# Median Imputation for (Employees)

fin[!complete.cases(fin),]

fin[is.na(fin$Employees),]

m_Retail <- median(fin[fin$Industry == "Retail", "Employees"], na.rm = TRUE)
m_Services <- median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = TRUE)


fin[is.na(fin$Employees) & fin$Industry == "Retail", "Employees"] <- m_Retail
fin[is.na(fin$Employees) & fin$Industry == "Financial Services", "Employees"] <- m_Services
fin[c(3,330),]

# median imputation for Growth , Revenue and Expenses

fin[is.na(fin$Growth),]

m_Growth <- median(fin[fin$Industry == "Construction","Growth"], na.rm = TRUE)
m_Growth

fin[is.na(fin$Growth) & fin$Industry == "Construction", "Growth"] <- m_Growth
fin[8,]

# median imputation for Revenue

fin[!complete.cases(fin),]

m_Revenue <- median(fin[fin$Industry == "Construction","Revenue"],na.rm = TRUE)
fin[is.na(fin$Revenue) & fin$Industry == "Construction", "Revenue"] <- m_Revenue
fin[c(8,42),]

# Medianimputation for expenses
m_Expenses <- median(fin[fin$Industry == "Construction", "Expenses"], na.rm = TRUE)
fin[is.na(fin$Expenses) & fin$Industry == "Construction", "Expenses"] <- m_Expenses
fin[c(8,42),]

#-----------------------------------------------------------------------------------------------

# Replacing the missing data: deriving values
# Revenue - Expenses = Profit
# Expenses = Revenue - Profit

fin[!complete.cases(fin),]
fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]
fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"]
fin[c(8,15,42),]

#---------------------------------------------------------------------------------------------------------------------------------------
# Visualization

#-------------- A scatterplot classified by industry showing revenue, Expenses, Profit------------------------------
install.packages("ggplot2")

library(ggplot2)

p <- ggplot(data=fin)
p
p + geom_point(aes(x=Revenue, y=Expenses, colour = Industry, size = Profit))

#-------------- A scatterplot that includes industry trends for the expenses~revenue relationship-------------------


d <- ggplot(data=fin, aes(x=Revenue, y=Expenses, colour=Industry)) 

d + geom_point() + geom_smooth(fill=NA, size=1.2)


#-------------- BoxPlots showing growth by industry----------------------------------------------

 f <- ggplot(data = fin, aes(x=Industry, y=Growth, colour=Industry))
 f + geom_jitter() + geom_boxplot(size = 1 , alpha=0.5, outlier.color = NA)

