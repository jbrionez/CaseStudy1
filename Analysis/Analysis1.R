#Analysis 1
#Merge the data based on the country shortcode. 
#How many of the IDs match?
#Based on the Merge of the Country Education Data and the Gross Domestic Product Data, there were 190 matches
#for countries that had GDP data available and that had Education Data entered.

GDPEDUdataRank <- subset(x=GDPEDUdata, Ranking > 0) #Remove non matched data
GDPEDUdataRank <- dplyr::arrange(GDPEDUdataRank, Ranking)