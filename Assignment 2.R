########## Libraries ##########
library("reshape2")
library("ggplot2")

########## Data Import and Cleaning ##########
mydata = read.csv("D:/Data Science Course/DSCT-EDA-And-Visualisation-master/GCI_dataset.csv", sep=";", header=TRUE, row.names=NULL)

attach(mydata)
mydata = mydata[Series.code==1.03 | 
                  Series.code==1.05 | 
                  Series.code==1.16 | 
                  Series.code==3.03 | 
                  Series.code==8.06 | 
                  Series.code==4.05 | 
                  Series.code==5.03 | 
                  Series.code==4.1 | Series.code==5.01 | Series.code==5.02 | 
                  Series.code==0.01 | 
                  Series.code==0.03 | 
                  Series.code==10.01 | Series.code==10.02, ]

rankdata = mydata[mydata$Attribute=="Rank", ]
valuedata = mydata[mydata$Attribute=="Value", ]

rankdata = melt(rankdata, id=c("Edition", "Series.code", "Series", "Attribute"))
valuedata = melt(valuedata, id=c("Edition", "Series.code", "Series", "Attribute"))
colnames(rankdata)[colnames(rankdata) == 'variable'] = 'Countries'
colnames(rankdata)[colnames(rankdata) == 'value'] = 'Rank'
colnames(valuedata)[colnames(valuedata) == 'variable'] = 'Countries'
colnames(valuedata)[colnames(valuedata) == 'value'] = 'Value'

rankdata$Rank = as.numeric(rankdata$Rank)
valuedata$Value = as.numeric(valuedata$Value)



########## Question 1. ########## 
HIVranks = rankdata[rankdata$Edition=="2014-2015" & rankdata$Series.code==4.05,]

qn1plot = ggplot(HIVranks,aes(x=reorder(HIVranks$Countries, as.numeric(HIVranks$Rank)) ,y=as.numeric(HIVranks$Rank)))+ 
  geom_bar(stat ="identity")+ 
  geom_text(aes(label=HIVranks$Rank), vjust=-1) + 
  xlab("Countries") +
  ylab("Rank") +
  ggtitle("Ranking of Countries for HIV Prevalence")

qn1plot

# Phillipines and Singapore both have Rank 1, Indonesia, Malaysia, and Phillipines have Rank 75, and Cambodia and Thailand have Ranks 104 and 110 respectively.
# Therefore, PHL and SGP have the lowest and KHM and THA have the highest HIV Prevalence.



########## Question 2. ########## 
attach(valuedata)
# irregpaydata = valuedata[Series.code==1.05 & Countries=="KHM",]
# policesvcdata = valuedata[Series.code==1.16 & Countries=="KHM",]
qn2data = valuedata[(Series.code==1.05 | Series.code==1.16) & Countries=="KHM",]

qn2plot = ggplot(qn2data,aes(x=qn2data$Edition, y=qn2data$Value, group=qn2data$Series.code, colour=factor(qn2data$Series.code))) + 
  geom_line() +
  geom_point() + 
  scale_color_discrete(name="Series", labels=c("Irregular payments and bribes", "Reliability of Police Services")) + 
  xlab("Years") +
  ylab("Rank") + 
  ggtitle("Relationship between Irregular payments and bribes & Reliability of Police Services")

qn2plot

# As the Irregular payments and bribes increases, the Reliability of Police Services increases and vice versa.



########## Question 3. ##########
attach(valuedata)
domesticforeigndata = valuedata[Series.code==10.01 | Series.code==10.02,]
domesticdata = valuedata[Series.code==10.01,]
foreigndata = valuedata[Series.code==10.02,]
totalvaluedata = data.frame(domesticdata$Edition, domesticdata$Countries, domesticdata$Value, foreigndata$Value, (domesticdata$Value+foreigndata$Value))
colnames(totalvaluedata) = c("Edition", "Countries", "Domestic Value", "Foreign Value", "Total Value")

domforeignplot = ggplot(domesticforeigndata,aes(x=domesticforeigndata$Edition, y=domesticforeigndata$Value, group=interaction(domesticforeigndata$Series.code, domesticforeigndata$Countries), colour=domesticforeigndata$Countries, shape=factor(domesticforeigndata$Series.code))) + 
  geom_line() +
  geom_point() + 
  scale_shape_discrete(name="Market Type", labels=c("Domestic Market Size Index", "Foreign Market Size Index")) + 
  scale_color_discrete(name="Countries", labels=c("KHM", "IDN", "MYS", "PHL", "SGP", "THA", "VNM")) + 
  xlab("Years") +
  ylab("Value") + 
  ggtitle("Relationship between Domestic and Foreign Market Size Index across all Countries")

domesticplot = ggplot(domesticdata,aes(x=domesticdata$Edition, y=domesticdata$Value, group=domesticdata$Countries, colour=domesticdata$Countries)) + 
  geom_line() +
  geom_point() + 
  scale_color_discrete(name="Countries", labels=c("KHM", "IDN", "MYS", "PHL", "SGP", "THA", "VNM")) + 
  xlab("Years") +
  ylab("Value") + 
  ggtitle("Trend of Domestic Market Size Index over all Countries")

foreignplot = ggplot(foreigndata,aes(x=foreigndata$Edition, y=foreigndata$Value, group=foreigndata$Countries, colour=foreigndata$Countries)) + 
  geom_line() +
  geom_point() + 
  scale_color_discrete(name="Countries", labels=c("KHM", "IDN", "MYS", "PHL", "SGP", "THA", "VNM")) + 
  xlab("Years") +
  ylab("Value") + 
  ggtitle("Trend of Foreign Market Size Index over all Countries")

totalmarketplot = ggplot(totalvaluedata,aes(x=totalvaluedata$Edition, y=totalvaluedata$`Total Value`, group=totalvaluedata$Countries, colour=totalvaluedata$Countries)) + 
  geom_line() +
  geom_point() + 
  scale_color_discrete(name="Countries", labels=c("KHM", "IDN", "MYS", "PHL", "SGP", "THA", "VNM")) + 
  xlab("Years") +
  ylab("Total Value") + 
  ggtitle("Trend of Total Market Size Index over all Countries")

domforeignplot
domesticplot
foreignplot
totalmarketplot

# The domestic market size for all countries dropped from 2006 to 2008. After which, all countries generally have an upward trend, an increase in somestic market size.
# The foreign market is generally on an upwards trend through the years with the exception of a fall across all countries from 2009 to 2010.
# The total market size has a general upwards trend from 2010 to 2014. However prior to that there has differing trends across countries.
# This could be due to the overall increase and decrease of the domestic and foreign markets for each of the countries.



########## Question 4. ##########
attach(valuedata)
qn4data = valuedata[(Series.code==5.03 | Series.code==0.01) & (Countries=="IDN" | Countries=="THA"), ]
qn4data2 = qn4data[qn4data$Edition!="2014-2015" & qn4data$Edition!="2007-2008" & qn4data$Edition!="2006-2007" & qn4data$Edition!="2008-2009" & qn4data$Edition!="2009-2010", ]
EDUCdata = qn4data[qn4data$Series.code==5.03, ]
GDPdata = qn4data[qn4data$Series.code==0.01, ]
GDPdata$ValueSmall = GDPdata$Value / 100

# ggplot(qn4data,aes(x=qn4data$Edition, y=qn4data$Value, group=interaction(qn4data$Series.code, qn4data$Countries), colour=qn4data$Countries, shape=factor(qn4data$Series.code))) + 
#   geom_line() + 
#   geom_point() + 
#   scale_shape_discrete(name="Series", labels=c("Quality of Education System", "GDP (US$ Billions)")) + 
#   scale_color_discrete(name="Countries", labels=c("IDN", "THA")) + 
#   xlab("Years") + 
#   ylab("Value") + 
#   ggtitle("Relationship between Quality of the Education System and GDP for IDN and THA")
# 
# ggplot(GDPdata,aes(x=GDPdata$Edition, y=GDPdata$ValueSmall, group=GDPdata$Countries, colour=GDPdata$Countries)) + 
#   geom_line() + 
#   geom_point() + 
#   geom_bar(data=EDUCdata, stat="identity", position="dodge", alpha=0.5, aes(x=EDUCdata$Edition, y=EDUCdata$Value, group=EDUCdata$Countries, colour=EDUCdata$Countries, fill=EDUCdata$Countries)) + 
#   scale_color_discrete(name="Countries GDP", labels=c("IDN", "THA")) + 
#   scale_fill_brewer(name="Countries Education", labels=c("IDN", "THA")) + 
#   xlab("Years") + 
#   ylab("Value") + 
#   ggtitle("Relationship between Quality of the Education System and GDP for IDN and THA")

GDPplot = ggplot(GDPdata,aes(x=GDPdata$Edition, y=GDPdata$Value, group=GDPdata$Countries, colour=GDPdata$Countries)) + 
  geom_line() + 
  geom_point() + 
  scale_color_discrete(name="Countries GDP", labels=c("IDN", "THA")) + 
  xlab("Years") + 
  ylab("Value") + 
  ggtitle("Relationship between IDN and THA for GDP")

educationplot = ggplot(EDUCdata, aes(x=EDUCdata$Edition, y=EDUCdata$Value, group=EDUCdata$Countries, colour=EDUCdata$Countries)) + 
  geom_line() + 
  geom_point() + 
  scale_color_discrete(name="Countries GDP", labels=c("IDN", "THA")) + 
  xlab("Years") + 
  ylab("Value") + 
  ggtitle("Relationship between IDN and THA for Education")

GDPplot
educationplot

# For GDP, both countries follow a similar trend. Both exhibit and increase, followed by a huge fall from 2009 to 2010, followed by an increasing trend after.
# For Education both countries increase and decrease at the same time up till 2012. After which, IDN increased while THA increased a little and dropped. 



########## Question 5. ##########
attach(valuedata)
inflationdata = valuedata[Series.code==3.03, ]
bankdata = valuedata[Series.code==8.06, ]
gdpcapitadata = valuedata[Series.code==0.03, ]
gdpcapitadata$ValueSmall = gdpcapitadata$Value / 1000

inflationplot = ggplot(inflationdata,aes(x=inflationdata$Edition, y=inflationdata$Value, group=inflationdata$Countries, colour=inflationdata$Countries)) +
  geom_line() +
  geom_point() +
  #geom_smooth(method='lm', se = FALSE) + 
  scale_color_discrete(name="Countries", labels=c("KHM", "IDN", "MYS", "PHL", "SGP", "THA", "VNM")) +
  xlab("Years") +
  ylab("Value") +
  ggtitle("Tend of Inflation across all Countries")

bankplot = ggplot(bankdata,aes(x=bankdata$Edition, y=bankdata$Value, group=bankdata$Countries, colour=bankdata$Countries)) +
  #geom_line() +
  geom_point() + 
  geom_smooth(method='lm', se = FALSE) + 
  scale_color_discrete(name="Countries", labels=c("KHM", "IDN", "MYS", "PHL", "SGP", "THA", "VNM")) +
  xlab("Years") +
  ylab("Value") +
  ggtitle("Tend of Soundness of Banks across all Countries")

GDPcapitaplot = ggplot(gdpcapitadata,aes(x=gdpcapitadata$Edition, y=gdpcapitadata$Value, group=gdpcapitadata$Countries, colour=gdpcapitadata$Countries)) +
  #geom_line() +
  geom_point() + 
  geom_smooth(method='lm', se = FALSE) + 
  scale_color_discrete(name="Countries", labels=c("KHM", "IDN", "MYS", "PHL", "SGP", "THA", "VNM")) +
  xlab("Years") +
  ylab("Value") +
  ggtitle("Tend of GDP per Capita across all Countries")

# ggplot(gdpcapitadata) + 
#   geom_line(aes(x=gdpcapitadata$Edition, y=gdpcapitadata$ValueSmall, group=gdpcapitadata$Countries, colour=gdpcapitadata$Countries, shape="Circle")) +
#   geom_line(data=inflationdata,aes(x=inflationdata$Edition, y=inflationdata$ValueSmall, group=inflationdata$Countries, colour=inflationdata$Countries, shape="Triangle")) + 
#   geom_point() + 
#   scale_color_discrete(name="Countries", labels=c("KHM", "IDN", "MYS", "PHL", "SGP", "THA", "VNM")) + 
#   xlab("Years") +
#   ylab("Value") + 
#   ggtitle("Relationship between Inflation and GDP per Capita")

inflationplot
GDPcapitaplot
bankplot

# Inflation and GDP per Capita
# From the trends from all countries, Inflation does not seem to affect GDP as although inflation goes up and down, GDP is generally on an upwards trend.
# As such, from Inflation and GDP per Capita alone how GDP per Capita is affected cannot be determined.
# Soundness of Banks and  GDP per Capita
# For all countries except MYS and VNM it can be said that as the Soundness of Banks increases, the GDP per capita increases.
# However, the increase in GDP per capita is small in these countries as compared to SGP although the increase in the Soundness of Banks is greater than that of SGP.
# As such, from Soundness of Banks and GDP per Capita alone how GDP per Capita is affected cannot be determined.


