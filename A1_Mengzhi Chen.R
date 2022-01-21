#ECON613 A1 Mengzhi Chen
#==================================================

#Exercise1 Basic Statistics
#==================================================

#Set working directory
setwd("/Users/halcyonchan/Desktop/Econ613/A1/Data")
install.packages("data.table")
library(data.table)
#==================================================

#Number of households surveyed in 2007
#Read the file
dathh2007 = fread("dathh2007.csv")
print(dathh2007)
#The number of X represents the number of household surveyed
max(dathh2007$V1) #10498
#==================================================

#Number of households with a marital status “Couple with kids” in 2005
dathh2005 = fread("dathh2005.csv")
print(dathh2005)
#Filter the households that are Couple, with Kids
dathh2005_Couple = dathh2005[which(dathh2005$mstatus=="Couple, with Kids"),]
#Count the number
length(dathh2005_Couple$idmen) #3374
#==================================================

#Number of individuals surveyed in 2008
datind2008 = fread("datind2008.csv")
print(datind2008)
#The number of X represents the number of individual surveyed
length(datind2008$idind) #25510
#==================================================

#Number of individuals aged between 25 and 35 in 2016
datind2016 = fread("datind2016.csv")
print(datind2016)
#Which() shows the condition and Length() compute the number that satisfy the condition
length(which(datind2016$age<=35 & datind2016$age>=25)) #2765
#==================================================

#Cross-table gender/profession in 2009
datind2009 = fread("datind2009.csv")
print(datind2009)
CrossTable = table(datind2009$gender,datind2009$profession)
CrossTable
#==================================================

#Distribution of wages in 2005 and 2019. Report the mean, the standard deviation, 
#the inter-decile ratio D9/D1 and the Gini coefficient
datind2005 = fread("datind2005.csv")
datind2019 = fread("datind2019.csv")
#Dist of wage
install.packages("ggplot2")
library(ggplot2)
ggplot(datind2005,aes(x=wage),na.rm=TRUE)+geom_density(color="black")
ggplot(datind2019,aes(x=wage),na.rm=TRUE)+geom_density(color="black")

#Report statistical indicators
#First, write the function of Gini
install.packages("tidyverse")
library(tidyverse)
wage2005 = datind2005%>%drop_na(wage)
w = sum(wage2005$wage/18767)
x = wage2005$wage
y = t(wage2005$wage)
Gini2005 = 1/(2*w*18767^2)*sum(abs(outer(x,y,FUN="-")))

wage2019 = datind2019%>%drop_na(wage)
v = sum(wage2019$wage/21421)
m = wage2019$wage
n = t(wage2019$wage)
Gini2019 = 1/(2*v*21421^2)*sum(abs(outer(m,n,FUN="-")))

dsummary = function(datind) {
  mean = summary(datind)[[4]] 
  sd = sd(datind,na.rm=TRUE)
  dec1 = quantile(datind,prob=c(0.1,0.9),na.rm=TRUE)[[1]]
  dec9 = quantile(datind,prob=c(0.1,0.9),na.rm=TRUE)[[2]]
  D9D1 = dec9/dec1
  return(c(mean,sd,D9D1))
}
dsummary(datind2005$wage)
mean #11992.26
sd #17318.56
D9D1 #inf
Gini2005 #0.6671654
dsummary(datind2019$wage)
mean #15350.47
sd #23207.18
D9D1 #inf
Gini2019 #0.6655301
#==================================================

#Distribution of age in 2010. Plot an histogram. Is there any difference between men and women?
datind2010 = fread("datind2010.csv")
print(datind2010)
hist(datind2010$age)
hist(datind2010[which(datind2010$gender=="Male"),age])
hist(datind2010[which(datind2010$gender=="Female"),age])
#==================================================

#Number of individuals in Paris in 2011
dathh2011 = fread("dathh2011.csv")
datind2011 = fread("datind2011.csv")
dat2011 = merge(datind2011,dathh2011,by =c("idmen"))
ind_paris = dat2011[which(dat2011$location == "Paris"),]
length(unique(ind_paris$idind)) #3514
#==================================================

#Exercise2 Merge Datasets
#==================================================

#Read all individual datasets from 2004 to 2019. Append all these datasets
datind2004 = read.csv("datind2004.csv")
datind2005 = read.csv("datind2005.csv")
datind2006 = read.csv("datind2006.csv")
datind2007 = read.csv("datind2007.csv")
datind2008 = read.csv("datind2008.csv")
datind2009 = read.csv("datind2009.csv")
datind2010 = read.csv("datind2010.csv")
datind2011 = read.csv("datind2011.csv")
datind2012 = read.csv("datind2012.csv")
datind2013 = read.csv("datind2013.csv")
datind2014 = read.csv("datind2014.csv")
datind2015 = read.csv("datind2015.csv")
datind2016 = read.csv("datind2016.csv")
datind2017 = read.csv("datind2017.csv")
datind2018 = read.csv("datind2018.csv")
datind2019 = read.csv("datind2019.csv")
# Use rbind() to append all these datasets
datind=rbind(datind2004,datind2005,datind2006,datind2007,datind2008,datind2009,datind2010,datind2011,datind2012,datind2013,datind2014,datind2015,datind2016,datind2017,datind2018,datind2019)
#==================================================

#Read all household datasets from 2004 to 2019. Append all these datasets
dathh2004 = read.csv("dathh2004.csv")
dathh2005 = read.csv("dathh2005.csv")
dathh2006 = read.csv("dathh2006.csv")
dathh2007 = read.csv("dathh2007.csv")
dathh2008 = read.csv("dathh2008.csv")
dathh2009 = read.csv("dathh2009.csv")
dathh2010 = read.csv("dathh2010.csv")
dathh2011 = read.csv("dathh2011.csv")
dathh2012 = read.csv("dathh2012.csv")
dathh2013 = read.csv("dathh2013.csv")
dathh2014 = read.csv("dathh2014.csv")
dathh2015 = read.csv("dathh2015.csv")
dathh2016 = read.csv("dathh2016.csv")
dathh2017 = read.csv("dathh2017.csv")
dathh2018 = read.csv("dathh2018.csv")
dathh2019 = read.csv("dathh2019.csv")
# Use rbind() to append all these datasets
dathh=rbind(dathh2004,dathh2005,dathh2006,dathh2007,dathh2008,dathh2009,dathh2010,dathh2011,dathh2012,dathh2013,dathh2014,dathh2015,dathh2016,dathh2017,dathh2018,dathh2019)
#==================================================

#List the variables that are simultaneously present in the individual and household datasets
ls(dathh)
ls(datind)
#"idmen","X","year"
#==================================================

#Merge the appended individual and household datasets
#group by idmen and year
dat = merge(datind,dathh,by =c("idmen","year"))
#==================================================

#In the second part, we use the newly created dataset from the previous to answer the following questions: 
#Number of households in which there are more than four family members
count = matrix(rep(1,413501))
#Add a column of 1’s to dat
dat_count = cbind(dat,count)
#Sum up the number of individuals with same year, idmen
#And find out which household has more than four members
dat_count1 = aggregate(x=dat_count[c('count')],by=list(dat$year,dat$idmen),FUN=sum)
count2004 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2004"),] 
count2005 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2005"),]
count2006 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2006"),] 
count2007 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2007"),]
count2008 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2008"),]
count2009 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2009"),]
count2010 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2010"),]
count2011 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2011"),]
count2012 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2012"),]
count2013 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2013"),]
count2014 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2014"),]
count2015 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2015"),] 
count2016 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2016"),]
count2017 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2017"),]
count2018 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2018"),]
count2019 = dat_count[which(dat_count1$count>4 & dat_count1$Group.1=="2019"),]
#combine the data of each year
count_all = rbind(count2004,count2005,count2006,count2007,count2008,count2009,count2010,count2011,count2012,count2013,count2014,count2015,count2016,count2017,count2018,count2019)
#remove the repeat data and count the number
length(unique(count_all$idmen)) #3734
#==================================================

#Number of households in which at least one member is unemployed
#Sum up the number of individuals with same year, idmen and empstat
dat_count2 = aggregate(x=dat_count[c('count')],by=list(dat_count$year,dat_count$idmen,dat_count$empstat),FUN=sum)
#And then count the number of households in which at least one member is unemployed
unemp2004 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2004"),]
unemp2005 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2005"),]
unemp2006 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2006"),]
unemp2007 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2007"),]
unemp2008 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2008"),]
unemp2009 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2009"),]
unemp2010 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2010"),]
unemp2011 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2011"),]
unemp2012 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2012"),]
unemp2013 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2013"),]
unemp2014 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2014"),]
unemp2015 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2015"),]
unemp2016 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2016"),]
unemp2017 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2017"),]
unemp2018 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2018"),]
unemp2019 = dat_count2[which(dat_count2$Group.3=="Unemployed" & dat_count2$Group.1=="2019"),]
unemp_all = rbind(unemp2004,unemp2005,unemp2006,unemp2007,unemp2008,unemp2009,unemp2010,unemp2011,unemp2012,unemp2013,unemp2014,unemp2015,unemp2016,unemp2017,unemp2018,unemp2019)
length(unique(unemp_all$Group.2)) #8161
#==================================================

#Number of households in which at least two members are of the same profession
#Sum up the number of individuals with same year, idmen and profession
dat_count3 = aggregate(x=dat_count[c('count')],by=list(dat_count$year,dat_count$idmen,dat_count$profession),FUN=sum)
#And then count the number of households that in which at least two members are of the same profession
prof2004 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2004"),]
prof2005 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2005"),]
prof2006 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2006"),]
prof2007 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2007"),]
prof2008 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2008"),]
prof2009 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2009"),]
prof2010 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2010"),]
prof2011 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2011"),]
prof2012 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2012"),]
prof2013 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2013"),]
prof2014 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2014"),]
prof2015 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2015"),]
prof2016 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2016"),]
prof2017 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2017"),]
prof2018 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2018"),]
prof2019 = dat_count3[which(dat_count3$count>=2 & dat_count3$Group.1=="2019"),]
prof_all = rbind(prof2004,prof2005,prof2006,prof2007,prof2008,prof2009,prof2010,prof2011,prof2012,prof2013,prof2014,prof2015,prof2016,prof2017,prof2018,prof2019)
length(unique(prof_all$Group.2)) #8752
#==================================================

#Number of individuals in the panel that are from household-Couple with kids
#Sum up the number of individuals with same year, idind and mstatus
dat_count4 = aggregate(x=dat_count[c('count')],by=list(dat_count$year,dat_count$idind,dat_count$mstatus),FUN=sum)
#And then count the number of individuals that are from household-Couple with kids in each year
Couple2004 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2004"),]
Couple2005 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2005"),]
Couple2006 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2006"),]
Couple2007 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2007"),]
Couple2008 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2008"),]
Couple2009 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2009"),]
Couple2010 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2010"),]
Couple2011 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2011"),]
Couple2012 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2012"),]
Couple2013 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2013"),]
Couple2014 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2014"),]
Couple2015 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2015"),]
Couple2016 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2016"),]
Couple2017 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2017"),]
Couple2018 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2018"),]
Couple2019 = dat_count4[which(dat_count4$Group.3=="Couple, with Kids" & dat_count4$Group.1=="2019"),]
Couple_all = rbind(Couple2004,Couple2005,Couple2006,Couple2007,Couple2008,Couple2009,Couple2010,Couple2011,Couple2012,Couple2013,Couple2014,Couple2015,Couple2016,Couple2017,Couple2018,Couple2019)
length(unique(Couple_all$Group.2)) #15567
#==================================================

#Number of individuals in the panel that are from Paris
#Sum up the number of individuals with same year, idind and locaiton
dat_count5 = aggregate(x=dat_count[c('count')],by=list(dat_count$year,dat_count$idind,dat_count$location),FUN=sum)
#And then count the number of individuals that are from Paris in each year
Paris2004 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2004"),]
Paris2005 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2005"),]
Paris2006 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2006"),]
Paris2007 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2007"),]
Paris2008 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2008"),]
Paris2009 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2009"),]
Paris2010 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2010"),]
Paris2011 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2011"),]
Paris2012 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2012"),]
Paris2013 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2013"),]
Paris2014 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2014"),]
Paris2015 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2015"),]
Paris2016 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2016"),]
Paris2017 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2017"),]
Paris2018 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2018"),]
Paris2019 = dat_count5[which(dat_count5$Group.3=="Paris" & dat_count5$Group.1=="2019"),]
Paris_all = rbind(Paris2004,Paris2005,Paris2006,Paris2007,Paris2008,Paris2009,Paris2010,Paris2011,Paris2012,Paris2013,Paris2014,Paris2015,Paris2016,Paris2017,Paris2018,Paris2019)
length(unique(Paris_all$Group.2)) #6177
#==================================================

#Find the household with the most number of family members. Report its idmen
#Find the maximum number of family numbers
max(dat_count1$count) #14
#Find the according idmen use the condition of "14" family members
dat_count1[which(dat_count1$count=="14"),2] 
#Report idmen: 2207811124040100 2510263102990100

#Number of households present in 2010 and 2011
dathh_num = rbind(dathh2010,dathh2011)
#Use Unique() to remove the repeat idmen
length(unique(dathh_num$idmen)) #13424
#==================================================

#Exercise3 Migration
#==================================================

#Find out the year each household enters and exit the panel.
#First, find out the year enters and exit separately
dat_min = aggregate(x=dat_count[c('year')],by=list(dat_count$idmen),FUN=min)
dat_max = aggregate(x=dat_count[c('year')],by=list(dat_count$idmen),FUN=max)
#Merge them into one dataset
dat_length = merge(dat_min,dat_max,by="Group.1")
#Report the length of years each household stay in the panel.
#The 4th column in dat_length shows the length of years each household stay in the panel 
dat_length[,4]=dat_length[,3]-dat_length[,2]
#rename the columns
colnames(dat_length)[1]="idmen"
colnames(dat_length)[2]="enter_year"
colnames(dat_length)[3]="exit_year"
colnames(dat_length)[4]="length_year"
#==================================================

#Base on datent,identify whether or not household moved into its current dwelling at the year of survey. 
#Report the first 10 rows of your result and plot the share of individuals in that situation across years.
dat[17] = dat[2]==dat[12] #V17 identify whether or not household moved into its current dwelling at the year of survey
dat[1:10,] #Report the first 10 rows
#Convert the logical varibles into 0-1
dat[18] = ifelse(dat[17] == TRUE,1,0)
#Compute the share of each year
dat_share = dat %>% group_by(year) %>% mutate(share = length(which(V18 == 1))/length(which(V18 >= 0)))
#Plot the share of individuals in that situation across years
ggplot(select(dat_share,year,share),aes(x=year,y=share))+geom_line()
#==================================================

#Base on myear and move, identify whether or not household migrated at the year of survey. 
#Report the first 10 rows of your result and plot the share of individuals in that situation across year
dat_share[20] = ifelse(dat_share$myear == dat_share$year,1,0)
dat_share[21] = ifelse(dat_share$move == 2,1,0)
dat_share[20] = replace(dat_share[,20],is.na(dat_share[,20]),0)
dat_share[21] = replace(dat_share[,21],is.na(dat_share[,21]),0)
#The column migration shows whether or not household migrated at the year of survey
#where 0 reprent not and 1 reprent migration at the year of survey
dat_share[,"migration"] = dat_share[,20]+dat_share[,21]
dat_share[1:10,c(1,2,20,21,22)] #Report the first 10 rows
#Compute the share of each year
dat_share = dat_share %>% group_by(year) %>% 
  mutate(share2 = length(which(migration == 1))/length(which(migration >= 0)))
#Plot the share of individuals in that situation across years
ggplot(select(dat_share,year,share2),aes(x=year,y=share2))+geom_line()
#==================================================

#Mix the two plots you created above in one graph, clearly label the graph. 
ggplot(select(dat_share,year,share,share2),aes(x=year,y=share2))+
  geom_line(mapping=aes(x=year,y=share,color="share"))+
  geom_line(mapping=aes(x=year,y=share2,color="share2"))
#Do you prefer one method over the other? Justify.
#I prefer the first method, because the graph shows less volatility. 
#Since we use two different variables "myear" and "move" to compute in the second method,
#there might be more biases according to the changing of statistical caliber.
#Thus, I think the first method of using "datent" is better.
#==================================================

#For households who migrate, find out how many households had at least one family member 
#changed his/her profession or employment status.
install.packages("tidyverse")
library(tidyverse)
#Remove NA from datent, and the rest households all have migrated.
dat %>% drop_na(datent)
#Add column lagprof and lagemp
dat = dat %>% mutate(lagprof = lag(profession,1,order_by=year),lagemp=lag(empstat,1,order_by=year))
#Identify whether they change their profession and empstat
dat = dat %>% mutate(change = ifelse(lagprof != profession | lagemp != empstat,1,0))
#Filter the data that change equals to 1, which means there exits changes between years
change2 = dat[which(dat["change"]==1),]
#Count the number of households who migrate had at least one family member changed his/her profession or employment status
#Unique() remove the repeat data.
length(unique(change2$idind)) #39849
#==================================================

#Exercise4 Attrition
#==================================================

#Compute the attrition across each year, where attrition is defined as the reduction 
#in the number of individuals staying in the data panel. Report your final result as a table in proportions.
i=0
z=0
vec=1:15
for (y in 2004:2018) {
  dat1 = dat[which(dat["year"]==y),]
  dat2 = dat[which(dat["year"]==y+1),]
  #find the individuals in both panel
  attrition = Reduce(intersect,list(dat1$idind,dat2$idind))
  i=i+1
  #the number of people exit year y = length(dat1$idind)-length(unique(attrition))
  #the number of people enter year y+1 = length(dat2$idind)-length(unique(attrition)
  #define proportion = (people who leave the panel of this year/people who enter the next year)
  vec[i]=(length(dat1$idind)-length(unique(attrition)))/(length(dat2$idind)-length(unique(attrition)))
}
table = cbind(c(2004:2018),vec)
colnames(table)[1]="year"
colnames(table)[2]="proportion"
table