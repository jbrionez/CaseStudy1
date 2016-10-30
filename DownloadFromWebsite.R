##Installed Packages
library(ggplot2)
library(extrafont)
library(repmis)
library(RCurl)

#Downloading URL and clean up prep for Merge
site <-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
GDP <- source_data(site, sep=",", header=FALSE)
GDP <- GDP[-c(3,6,7,8,9,10)] #removing empty columns
names(GDP) <- c("CountryCode","Ranking","Economy","MUSD")  #Names for Columns
GDPFinal <- subset(x=GDP, MUSD != "..") #Removes .. from MUSD column
GDPFinal <- subset(x=GDPFinal, Ranking != "Gross domestic product 2012")
GDPFinal <- subset(x=GDPFinal, Ranking != "Ranking")
GDPFinal <- subset(x=GDPFinal, Ranking != "Note: Rankings include only those economies with confirmed GDP estimates. Figures in italics are for 2011 or 2010.")
GDPFinal <- subset(x=GDPFinal, Ranking != "a. Includes Former Spanish Sahara.  b. Excludes South Sudan  c. Covers mainland Tanzania only. d. Data are for the area")
GDPFinal <- subset(x=GDPFinal, Ranking != "controlled by the government of the Republic of Cyprus.   e. Excludes Abkhazia and South Ossetia.  f. Excludes Transnistria.")
GDPFinal <- subset(x=GDPFinal, Ranking >0) #Removes Outliers Items with no MUSD or that are not countries


urladdress <-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
EDU <- source_data(urladdress, sep=",", header=TRUE)

#Merge of Datasets GDPFinal and EDU
GDPEDUdata <- merge(x=GDPFinal, y=EDU, by = "CountryCode", all=TRUE) #Merge of data by CountryCode
EDU <- source_data(urladdress, sep=",", header=TRUE)

##Question 1
#Merge the data based on the country shortcode. How many of the IDs match?
#Based on the Merge of the Country Education Data and the Gross Domestic Product Data, there were 190 matches
#for countries that had GDP data available and that had Education Data entered.

GDPEDUdataRank <- subset(x=GDPEDUdata, Ranking > 0) #Remove non matched data
GDPEDUdataRank <- arrange(GDPEDUdataRank, Ranking)
GDPEDUdataRank[c(1:2)]

##Question 2
#Sort the data frame in ascending order by GDP (so United States is last). What is the 13th
#country in the resulting data frame?

GDPEDUdataRank$MUSD<-as.numeric(gsub("\\,","",GDPEDUdataRank$MUSD)) #remove commas and change to numeric
GDPEDUdataRank$MUSD<-as.numeric(gsub("\\$","",GDPEDUdataRank$MUSD)) #remove Dollar signs and change to numeric
GDPEDUdataRank$Ranking<-as.numeric(GDPEDUdataRank$Ranking) #convert Rankings to Numeric
GDPEDUdataRank <- dplyr::arrange(GDPEDUdataRank, MUSD) #
GDPEDUdataRank[c(1:2)]


#KNA is the Thirteenth country on the list.

##Question 3
#What are the average GDP rankings for the "High income: OECD" and "High income:nonOECD" groups?
HighIncomeOECD <- subset(GDPEDUdataRank, `Income Group` == "High income: OECD")
HighIncomeOECD$Ranking<-as.numeric(HighIncomeOECD$Ranking) #convert Rankings to Numeric
HighIncomeOECD[c(1,6)] #CheckValues in Income Group Column
summary(HighIncomeOECD)

#Average Ranking is 32.97

HighIncomenonOECD <- subset(GDPEDUdataRank, `Income Group` == "High income: nonOECD")
HighIncomenonOECD$Ranking<-as.numeric(HighIncomenonOECD$Ranking) #convert Rankings to Numeric
HighIncomenonOECD[c(1,6)] #CheckValues in Income Group Column
summary(HighIncomenonOECD)

#Average Ranking is 91.91

##Question 4
#Plot the GDP for all of the countries. Use ggplot2 to color your plot by Income Group.


g<-g+ggtitle("GDP")
g+theme(plot.title = element_text(size=20, face="bold", vjust=1, lineheight=0.6))
g+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))
g<-ggplot(GDPEDUdataRank, aes(Ranking, MUSD, color=factor(`Income Group`)))+geom_point()
g


##Question 5
#Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group.
#How many countries are Lower middle income but among the 38 nations with highest
#GDP?

##Additional Info - Exploration
#quantile(GDPEDUdataRank$MUSD)
#brks <- with(GDPEDUdataRank, quantile(MUSD, probs = c(0, 0.20, 0.40, 0.60, 0.8, 1)))
#GDPEDUdataRankQant <- within(GDPEDUdataRank, quantile <- cut(MUSD, breaks = brks, labels = 1:5, include.lowest = TRUE))

brks2 <- with(GDPEDUdataRank, quantile(Ranking, probs = c(0, 0.20, 0.40, 0.60, 0.8, 1)))
GDPEDUdataRankQantRnk <- within(GDPEDUdataRank, quantile <- cut(Ranking, breaks = brks2, labels = 1:5, include.lowest = TRUE))

#plot
g<-g+ggtitle("GDP")
g+theme(plot.title = element_text(size=20, face="bold", vjust=1, lineheight=0.6))
g+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))
g<-ggplot(GDPEDUdataRankQant, aes(Ranking, MUSD, color=factor(`Income Group`)))+geom_point()
g


#Separating out the Lower middle income group for comparison to ranking
LowerMiddleIncome <- subset(GDPEDUdataRankQant, `Income Group` == "Lower middle income") #create subset for Lower Middle Income
LowerMiddleIncomeTop38GDP <- subset(LowerMiddleIncome, Ranking <= 38) #create additional subset with top 38 GDPs
LowerMiddleIncomeTop38GDP[c(1:2)] #list of GDPs with both that are both in the lower Middle income group and in top 38 GDPs

# Five countries are in the lower middle income group that are in the top 38 GDP.