#Analysis4
#Plot the GDP for all of the countries. Use ggplot2 to color your plot by Income Group.
#g<-g+ggtitle("GDP")
#g+theme(plot.title = element_text(size=20, face="bold", vjust=1, lineheight=0.6))
#g+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))

g<-ggplot(GDPEDUdataRank, aes(Ranking, MUSD, color=factor(`Income Group`)))+geom_point()
g
