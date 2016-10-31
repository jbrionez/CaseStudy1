#Downloads
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