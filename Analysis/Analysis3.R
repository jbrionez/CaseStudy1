#Analysis 3
#What are the average GDP rankings for the "High income: OECD" and "High income:nonOECD" groups?

HighIncomeOECD <- subset(GDPEDUdataRank, `Income Group` == "High income: OECD")
HighIncomeOECD$Ranking<-as.numeric(HighIncomeOECD$Ranking) #convert Rankings to Numeric
HighIncomeOECD[c(1,6)] #CheckValues in Income Group Column
summary(HighIncomeOECD)


HighIncomenonOECD <- subset(GDPEDUdataRank, `Income Group` == "High income: nonOECD")
HighIncomenonOECD$Ranking<-as.numeric(HighIncomenonOECD$Ranking) #convert Rankings to Numeric
HighIncomenonOECD[c(1,6)] #CheckValues in Income Group Column
summary(HighIncomenonOECD)