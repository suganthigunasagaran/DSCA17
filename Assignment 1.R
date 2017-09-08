########## Question 1. ########## 
mydata = read.csv("D:/Data Science Course/DSCT-Intro-To-R-master/35478-0001-Data.csv")[ ,c('AGE', 'DEGREE', 'EDUC', 'HAPPY', 'HOMPOP', 'INCOME', 'MARITAL', 'RACE', 'RINCOME', 'SEX')]

# 4820 Rows



########## Question 2. ##########
# Age:
# 98 Don't know
# 99 No answer
# 
# Degree:
# 7 Inapplicable
# 8 Don't know
# 9 No answer
# 
# Education:
# 97 Inapplicable
# 98 Don't know
# 99 No answer
# 
# Happy:
# 0 Inapplicable
# 8 Don't know
# 9 No answer
# 
# Home Population:
# 98 Don't know
# 99 No answer
# 
# Income:
# 0 Inapplicable
# 13 Refused
# 98 Don't know
# 99 No answer
# 
# Marital Status:
# 9 No answer
# 
# Race:
# 0 Inapplicable
# 
# Resspondernt's Income:
# 0 Inapplicable
# 13 Refused
# 98 Don't know
# 99 No answer



########## Question 3. ########## 
attach(mydata)
mydata = mydata[AGE!=98 & AGE!=99 & 
                     DEGREE!=7 & DEGREE!=8 & DEGREE!=9 & 
                     EDUC!=97 & EDUC!=98 & EDUC!=99 & 
                     HAPPY!=0 & HAPPY!=8 & HAPPY!=9 & 
                     HOMPOP!=98 & HOMPOP!=99 & 
                     INCOME!=0 & INCOME!=13 & INCOME!=98 & INCOME!=99 & 
                     MARITAL!=9 & 
                     RACE!=0 & 
                     RINCOME!=0 & RINCOME!=13 & RINCOME!=98 & RINCOME!=99, ]

# submydata2 = mydata[AGE != c(98,99),]
# submydata2 = submydata2[submydata2$RINCOME!=0,]

# 2749 rows



########## Question 4. ########## 
incomekey = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
incomevalue = c(0, 1000, 3000, 4000, 5000, 6000, 7000, 8000, 10000, 15000, 20000, 25000)
incomedata = data.frame(incomekey, incomevalue)
mydata$totfamincome = incomedata$incomevalue[match(mydata$INCOME, incomedata$incomekey)]
mydata$PERCAPITA = mydata$totfamincome/mydata$HOMPOP



########## Question 5. ##########
summary(mydata)
# AGE           DEGREE           EDUC           HAPPY           HOMPOP           INCOME         MARITAL     
# Min.   :18.0   Min.   :0.000   Min.   : 0.00   Min.   :1.000   Min.   : 1.000   Min.   : 1.00   Min.   :1.000  
# 1st Qu.:33.0   1st Qu.:1.000   1st Qu.:12.00   1st Qu.:1.000   1st Qu.: 2.000   1st Qu.:12.00   1st Qu.:1.000  
# Median :44.0   Median :1.000   Median :14.00   Median :2.000   Median : 2.000   Median :12.00   Median :1.000  
# Mean   :44.8   Mean   :1.834   Mean   :14.12   Mean   :1.809   Mean   : 2.744   Mean   :11.42   Mean   :2.549  
# 3rd Qu.:55.0   3rd Qu.:3.000   3rd Qu.:16.00   3rd Qu.:2.000   3rd Qu.: 4.000   3rd Qu.:12.00   3rd Qu.:5.000  
# Max.   :89.0   Max.   :4.000   Max.   :20.00   Max.   :3.000   Max.   :10.000   Max.   :12.00   Max.   :5.000  
# RACE          RINCOME           SEX         totfamincome     PERCAPITA    
# Min.   :1.000   Min.   : 1.00   Min.   :1.000   Min.   :    0   Min.   :    0  
# 1st Qu.:1.000   1st Qu.:10.00   1st Qu.:1.000   1st Qu.:25000   1st Qu.: 6250  
# Median :1.000   Median :12.00   Median :2.000   Median :25000   Median : 8333  
# Mean   :1.322   Mean   :10.31   Mean   :1.516   Mean   :22711   Mean   :10964  
# 3rd Qu.:1.000   3rd Qu.:12.00   3rd Qu.:2.000   3rd Qu.:25000   3rd Qu.:12500  
# Max.   :3.000   Max.   :12.00   Max.   :2.000   Max.   :25000   Max.   :25000  

summary(mydata$PERCAPITA)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    6250    8333   10964   12500   25000
# The Minimum value is 0.
# The Median is 8333.
# The Mean is 10964.
# The Maximum value is 25000.
# The 25th percentile and 75th percentile are 6250 and 12500 respectively.



########## Question 6. ##########
library( dplyr )
totalrows = nrow(mydata)
bachelordegree = dplyr::filter(mydata, DEGREE>=3)
totalbachelordegreerows = nrow(bachelordegree)
proportion = (totalbachelordegreerows / totalrows) * 100
# Proportion of respondents with a Bachelor Degree or higher is 35.29%.



########## Question 7. ##########
attach(mydata)
mydata$AGE[AGE>17 & AGE < 36] = "Young"
mydata$AGE[AGE>35 & AGE < 56] = "Middle"
mydata$AGE[AGE>55] = "Old"



########## Question 8. ##########
agedata = mydata$AGE
numericagedata = as.numeric(factor(agedata , levels = c("Young","Middle","Old")))
agehappytable = table(mydata$HAPPY, numericagedata)
colnames(agehappytable) = c("Young","Middle","Old")
prop.table(agehappytable)

# No.
# The proportion of Young who are happy is more than that of the Old people. 
# However it is lower than that of the Middle people.
# Therefore, the proportion of young respondents who are happy is not greater than that of the other age groups.



########## Question 9. ##########
#prop.table(ftable(mydata$EDUC, mydata$RINCOME))
           
plot(jitter(mydata$EDUC),
     mydata$RINCOME,
     main="Scatter Plot of Respondents Income against Years of Education",
     xlab="Years of Education",
     ylab="Respondent's Income")
abline(lm(mydata$RINCOME ~ mydata$EDUC, data=mydata))

boxplot(mydata$RINCOME ~ mydata$EDUC, main="Boxplot of Respondent's Income against Years of Education")

# It can be seen that there is a general increase in the income as the years of education increases.



########## Question 10. ##########
boxplot(mydata$RINCOME ~ mydata$RACE, main="Boxplot of Respondent Income against Race")

# Whites.
# The Whites have a higher minimum income and a higher lower quartile income as compared to the other Races.

