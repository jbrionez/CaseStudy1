#Analysis5
#Cut the GDP ranking into 5 separate quantile groups. 

brks2 <- with(GDPEDUdataRank, quantile(Ranking, probs = c(0, 0.20, 0.40, 0.60, 0.8, 1)))
GDPEDUdataRankQantRnk <- within(GDPEDUdataRank, quantile <- cut(Ranking, breaks = brks2, labels = 1:5, include.lowest = TRUE))


#Make a table versus Income.Group.

table(GDPEDUdataRankQantRnk$quantile, GDPEDUdataRankQantRnk$`Income Group`, exclude = NULL)

#How many countries are Lower middle income but among the 38 nations with highest GDP?

LowerMiddleIncome <- subset(GDPEDUdataRankQantRnk, `Income Group` == "Lower middle income") #create subset for Lower Middle Income
LowerMiddleIncomeTop38GDP <- subset(LowerMiddleIncome, Ranking <= 38) #create additional subset with top 38 GDPs
LowerMiddleIncomeTop38GDP[c(1:2)] #list of GDPs with both that are both in the lower Middle income group and in top 38 GDPs

#Additional Info - Exploration
#quantile(GDPEDUdataRank$MUSD)
#brks <- with(GDPEDUdataRank, quantile(MUSD, probs = c(0, 0.20, 0.40, 0.60, 0.8, 1)))
#GDPEDUdataRankQant <- within(GDPEDUdataRank, quantile <- cut(MUSD, breaks = brks, labels = 1:5, include.lowest = TRUE))



