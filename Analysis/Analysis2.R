#Analysis 2
#Sort the data frame in ascending order by GDP (so United States is last). 
#What is the 13th country in the resulting data frame?

GDPEDUdataRank$MUSD<-as.numeric(gsub("\\,","",GDPEDUdataRank$MUSD)) #remove commas and change to numeric
GDPEDUdataRank$MUSD<-as.numeric(gsub("\\$","",GDPEDUdataRank$MUSD)) #remove Dollar signs and change to numeric
GDPEDUdataRank$Ranking<-as.numeric(GDPEDUdataRank$Ranking) #convert Rankings to Numeric
GDPEDUdataRank <- dplyr::arrange(GDPEDUdataRank, MUSD) #
#GDPEDUdataRank[c(1:2)]