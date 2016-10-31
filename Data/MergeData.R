#Merge
GDPEDUdata <- merge(x=GDPFinal, y=EDU, by = "CountryCode", all=TRUE) #Merge of data by CountryCode