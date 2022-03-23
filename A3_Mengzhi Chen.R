#ECON613 A3 Mengzhi Chen
#==================================================

#Set working directory
setwd("/Users/halcyonchan/Desktop/Econ613/A3/Data")
library(data.table)
library(tidyverse)

#Exercise1============================================
#Calculate the following statistics.
#Read the file
datstu = fread("datstu_v2.csv")
datjss = fread("datjss.csv")
datsss = fread("datsss.csv")

#1.1==================================================
#Number of students, schools, programs
#Calculate the number of students
stu_num = length(datstu$V1)
stu_num #340823

#Combine (school,program) into a long matrix
choice1 = cbind(datstu$schoolcode1,datstu$choicepgm1)
choice2 = cbind(datstu$schoolcode2,datstu$choicepgm2)
choice3 = cbind(datstu$schoolcode3,datstu$choicepgm3)
choice4 = cbind(datstu$schoolcode4,datstu$choicepgm4)
choice5 = cbind(datstu$schoolcode5,datstu$choicepgm5)
choice6 = cbind(datstu$schoolcode6,datstu$choicepgm6)
choice = rbind(choice1,choice2,choice3,choice4,choice5,choice6)
#Calculate the number of schools that students apply using dataset datstu
school_num1 = length(unique(choice[,1]))
school_num1 #641

#Calculate the total number of schools using dataset datsss
school2 = unique(datsss[,3])
school_num2 = nrow(school2)
school_num2 #898

#Calculate the number of programs
program_num = length(unique(choice[,2]))
program_num #33

#1.2==================================================
#Number of choices (school, program)
#Hint: Create a matrix of school, programs. Convert data from Wide to Long.
choice1 = cbind(datstu$schoolcode1,datstu$choicepgm1)
choice2 = cbind(datstu$schoolcode2,datstu$choicepgm2)
choice3 = cbind(datstu$schoolcode3,datstu$choicepgm3)
choice4 = cbind(datstu$schoolcode4,datstu$choicepgm4)
choice5 = cbind(datstu$schoolcode5,datstu$choicepgm5)
choice6 = cbind(datstu$schoolcode6,datstu$choicepgm6)
choice = unique(rbind(choice1,choice2,choice3,choice4,choice5,choice6))
colnames(choice)=c('schoolcode','choicepgm')
choice_num = nrow(choice)
choice_num #3086

#1.3==================================================
#Number of students applying to at least one senior high schools in the same district to home 
#(Suppose students live in the same district to their junior high schools)
#Choose the longest schoolname in dataset datsss
datsss = datsss %>%
  group_by(schoolcode) %>%
  filter(nchar(schoolname) == max(nchar(schoolname))) 
datsss1 = unique(datsss[,c('schoolcode','sssdistrict')])

#match each schoolcode to its corresponding district
colnames(datsss1)=c('schoolcode1','sssdistrict1')
merge1 = merge(datstu,datsss1,by = c('schoolcode1'),allow.cartesian=TRUE)
colnames(datsss1)=c('schoolcode2','sssdistrict2')
merge2 = merge(merge1,datsss1,by = c('schoolcode2'),allow.cartesian=TRUE)
colnames(datsss1)=c('schoolcode3','sssdistrict3')
merge3 = merge(merge2,datsss1,by = c('schoolcode3'),allow.cartesian=TRUE)
colnames(datsss1)=c('schoolcode4','sssdistrict4')
merge4 = merge(merge3,datsss1,by = c('schoolcode4'),allow.cartesian=TRUE)
colnames(datsss1)=c('schoolcode5','sssdistrict5')
merge5 = merge(merge4,datsss1,by = c('schoolcode5'),allow.cartesian=TRUE)
colnames(datsss1)=c('schoolcode6','sssdistrict6')
merge6 = merge(merge5,datsss1,by = c('schoolcode6'),allow.cartesian=TRUE)

#If the student apply to at least one senior high schools in the same district to home, set "same_dist" as 1
#If there is no senior high schools in the same district to home, set "same_dist" as 0
merge6[which(merge6[,'jssdistrict'] == merge6[,'sssdistrict1'] | merge6[,'jssdistrict'] == merge6[,'sssdistrict2'] | 
               merge6[,'jssdistrict'] == merge6[,'sssdistrict3'] | merge6[,'jssdistrict'] == merge6[,'sssdistrict4'] | 
               merge6[,'jssdistrict'] == merge6[,'sssdistrict5'] | merge6[,'jssdistrict'] == merge6[,'sssdistrict6']),'same_dist'] = 1
#Sum the column of "same_dist"
sum(merge6[,'same_dist'],na.rm=TRUE) #250826

#1.4==================================================
#Number of students each senior high school admitted
#First, find the schools that each student admitted to
datstu[which(datstu$rankplace == "1"),'admitted'] = datstu[which(datstu$rankplace == "1"),5]
datstu[which(datstu$rankplace == "2"),'admitted'] = datstu[which(datstu$rankplace == "2"),6]
datstu[which(datstu$rankplace == "3"),'admitted'] = datstu[which(datstu$rankplace == "3"),7]
datstu[which(datstu$rankplace == "4"),'admitted'] = datstu[which(datstu$rankplace == "4"),8]
datstu[which(datstu$rankplace == "5"),'admitted'] = datstu[which(datstu$rankplace == "5"),9]
datstu[which(datstu$rankplace == "6"),'admitted'] = datstu[which(datstu$rankplace == "6"),10]
datstu = datstu %>% drop_na(admitted)
#Add one column of 1's for further counting
count = matrix(rep(1,nrow(datstu)))
admitstu = cbind(datstu,count)
#Count the number of students admitted of each school use function aggregate and by
admitstu_count = aggregate(x=admitstu[,20],by=list(admitstu$admitted),FUN=sum)
#Rename the columns
colnames(admitstu_count)=c('schoolcode','num_stu')
view(admitstu_count)

#1.5==================================================
#The cutoff of senior high schools (the lowest score to be admitted)
admitstu_cutoff = aggregate(x=admitstu[,2],by=list(admitstu$admitted),FUN=min)
colnames(admitstu_cutoff)=c('schoolcode','cutoff')
view(admitstu_cutoff)

#1.6==================================================
#The quality of senior high schools (the average score of students admitted)
admitstu_quality = aggregate(x=admitstu[,2],by=list(admitstu$admitted),FUN=mean)
colnames(admitstu_quality)=c('schoolcode','quality')
view(admitstu_quality)


#Exercise2============================================
#Create a school level dataset, where each row corresponds to a (school,program) with the following variables:
#1.the district where the school is located
#2.the latitude and longtitude of the district
#3.cutoff (the lowest score to be admitted)
#4.quality (the average score of the students admitted)
#5.size (number of students admitted)

#drop na in datsss and merge it with choice(school,program)
datsss1 = datsss[,c('schoolcode','sssdistrict','ssslong','ssslat')]
datsss1 = datsss1 %>% drop_na(ssslong)
datschool = merge(choice,datsss1,by = c('schoolcode'),all.x=TRUE)

#Add a column "admitpgm" (admitted program) in dataset datstu
datstu[which(datstu$rankplace == "1"),'admitpgm'] = datstu[which(datstu$rankplace == "1"),11]
datstu[which(datstu$rankplace == "2"),'admitpgm'] = datstu[which(datstu$rankplace == "2"),12]
datstu[which(datstu$rankplace == "3"),'admitpgm'] = datstu[which(datstu$rankplace == "3"),13]
datstu[which(datstu$rankplace == "4"),'admitpgm'] = datstu[which(datstu$rankplace == "4"),14]
datstu[which(datstu$rankplace == "5"),'admitpgm'] = datstu[which(datstu$rankplace == "5"),15]
datstu[which(datstu$rankplace == "6"),'admitpgm'] = datstu[which(datstu$rankplace == "6"),16]
#Add one column of 1's for further counting
count = matrix(rep(1,nrow(datstu)))
admitstu = cbind(datstu,count)
#Compute the cutoff of each choice
datschool_cutoff = aggregate(x=admitstu[,2],by=list(admitstu$admitted,admitstu$admitpgm),FUN=min)
colnames(datschool_cutoff) = c('schoolcode','choicepgm','score')
#Compute the quality of each choice
datschool_quality = aggregate(x=admitstu[,2],by=list(admitstu$admitted,admitstu$admitpgm),FUN=mean)
colnames(datschool_quality) = c('schoolcode','choicepgm','score')
#Compute the size of each choice
datschool_num = aggregate(x=admitstu[,21],by=list(admitstu$admitted,admitstu$admitpgm),FUN=sum)
colnames(datschool_num) = c('schoolcode','choicepgm','size')

#merge these columns
datschool = merge(datschool,datschool_cutoff,by = c('schoolcode','choicepgm'),all.x=TRUE)
datschool = merge(datschool,datschool_quality,by = c('schoolcode','choicepgm'),all.x=TRUE)
datschool = merge(datschool,datschool_num,by = c('schoolcode','choicepgm'),all.x=TRUE)
#Drop the repeated rows using unique function
datschool = unique(datschool)
#Rename the columns
colnames(datschool) = c('schoolcode','choicepgm','district','longtitude','latitude','cutoff','quality','size')
view(datschool)


#Exercise3============================================
#Using the formula
#dist(sss, jss) = sqrt(69.172 ∗ (ssslong − jsslong) ∗ cos(jsslat/57.3))^2 + (69.172 ∗ (ssslat − jsslat))^2)
#where ssslong and ssslat are the coordinates of the district of the school (students apply to), 
#while jsslong and jsslat are the coordinates of the junior high school, 
#calculate the distance between junior high school, and senior high school. 
#You should generate a value of distance for each of students’ choices.

datstu = fread("datstu_v2.csv")
datjss1 = datjss[,c(2,3,4)]
datsss1 = datsss[,c(3,4,5,6)]
#For schoolcode1 for each student
choice_jss1 = cbind(datstu$V1,datstu$schoolcode1,datstu$choicepgm1,datstu$jssdistrict)
colnames(choice_jss1) = c('student','schoolcode','choicepgm','jssdistrict')
#merge the choices with jsslong and jsslat according to 'jssdistrict', as well as sssdistrict and their longtitude and latitude
choice_dist1 = merge(choice_jss1,datjss1,by = c('jssdistrict'),all.x=TRUE)
choice_dist1 = unique(merge(choice_dist1,datsss1,by = c('schoolcode'),all.x=TRUE))
#Calculate dist using the formula
choice_dist1[,'dist'] = sqrt((69.172*(choice_dist1[,'ssslong']-choice_dist1[,'point_x'])
                             *cos(choice_dist1[,'point_y']/57.3))^2+(69.172*(choice_dist1[,'ssslat']-choice_dist1[,'point_y']))^2)
view(choice_dist1)

#For schoolcode2 for each student
choice_jss2 = cbind(datstu$V1,datstu$schoolcode2,datstu$choicepgm2,datstu$jssdistrict)
colnames(choice_jss2) = c('student','schoolcode','choicepgm','jssdistrict')
choice_dist2 = merge(choice_jss2,datjss1,by = c('jssdistrict'),all.x=TRUE)
choice_dist2 = unique(merge(choice_dist2,datsss1,by = c('schoolcode'),all.x=TRUE))
choice_dist2[,'dist'] = sqrt((69.172*(choice_dist2[,'ssslong']-choice_dist2[,'point_x'])
                              *cos(choice_dist2[,'point_y']/57.3))^2+(69.172*(choice_dist2[,'ssslat']-choice_dist2[,'point_y']))^2)
view(choice_dist2)

#For schoolcode3 for each student
choice_jss3 = cbind(datstu$V1,datstu$schoolcode3,datstu$choicepgm3,datstu$jssdistrict)
colnames(choice_jss3) = c('student','schoolcode','choicepgm','jssdistrict')
choice_dist3 = merge(choice_jss3,datjss1,by = c('jssdistrict'),all.x=TRUE)
choice_dist3 = unique(merge(choice_dist3,datsss1,by = c('schoolcode'),all.x=TRUE))
choice_dist3[,'dist'] = sqrt((69.172*(choice_dist3[,'ssslong']-choice_dist3[,'point_x'])
                              *cos(choice_dist3[,'point_y']/57.3))^2+(69.172*(choice_dist3[,'ssslat']-choice_dist3[,'point_y']))^2)
view(choice_dist3)

#For schoolcode4 for each student
choice_jss4 = cbind(datstu$V1,datstu$schoolcode4,datstu$choicepgm4,datstu$jssdistrict)
colnames(choice_jss4) = c('student','schoolcode','choicepgm','jssdistrict')
choice_dist4 = merge(choice_jss4,datjss1,by = c('jssdistrict'),all.x=TRUE)
choice_dist4 = unique(merge(choice_dist4,datsss1,by = c('schoolcode'),all.x=TRUE))
choice_dist4[,'dist'] = sqrt((69.172*(choice_dist4[,'ssslong']-choice_dist4[,'point_x'])
                              *cos(choice_dist4[,'point_y']/57.3))^2+(69.172*(choice_dist4[,'ssslat']-choice_dist4[,'point_y']))^2)
view(choice_dist4)

#For schoolcode5 for each student
choice_jss5 = cbind(datstu$V1,datstu$schoolcode5,datstu$choicepgm5,datstu$jssdistrict)
colnames(choice_jss5) = c('student','schoolcode','choicepgm','jssdistrict')
choice_dist5 = merge(choice_jss5,datjss1,by = c('jssdistrict'),all.x=TRUE)
choice_dist5 = unique(merge(choice_dist5,datsss1,by = c('schoolcode'),all.x=TRUE))
choice_dist5[,'dist'] = sqrt((69.172*(choice_dist5[,'ssslong']-choice_dist5[,'point_x'])
                              *cos(choice_dist5[,'point_y']/57.3))^2+(69.172*(choice_dist5[,'ssslat']-choice_dist5[,'point_y']))^2)
view(choice_dist5)

#For schoolcode6 for each student
choice_jss6 = cbind(datstu$V1,datstu$schoolcode6,datstu$choicepgm6,datstu$jssdistrict)
colnames(choice_jss6) = c('student','schoolcode','choicepgm','jssdistrict')
choice_dist6 = merge(choice_jss6,datjss1,by = c('jssdistrict'),all.x=TRUE)
choice_dist6 = unique(merge(choice_dist6,datsss1,by = c('schoolcode'),all.x=TRUE))
choice_dist6[,'dist'] = sqrt((69.172*(choice_dist6[,'ssslong']-choice_dist6[,'point_x'])
                              *cos(choice_dist6[,'point_y']/57.3))^2+(69.172*(choice_dist6[,'ssslat']-choice_dist6[,'point_y']))^2)
view(choice_dist6)


#Exercise4============================================
#4.1==================================================
#Recode the schoolcode into its first three digits (substr). Call this new variable scode_rev.
scode_rev = substring(choice[,1],1,3)
choice_code = cbind(choice,scode_rev)
choice_code = data.frame(choice_code)
view(choice_code)

#4.2==================================================
#Recode the program variable into 4 categories: 
#arts (general arts and visual arts), economics (business and home economics), 
#science (general science) and others. Call this new variable pgm_rev.
choice_code[which(choice_code[,2] == 'General Arts'),'pgm_rev'] = 'arts'
choice_code[which(choice_code[,2] == 'Visual Arts'),'pgm_rev'] = 'arts'
choice_code[which(choice_code[,2] == 'Business'),'pgm_rev'] = 'economics'
choice_code[which(choice_code[,2] == 'Home Economics'),'pgm_rev'] = 'economics'
choice_code[which(choice_code[,2] == 'General Science'),'pgm_rev'] = 'science'
choice_code[which(is.na(choice_code[,4])==TRUE),'pgm_rev'] = 'others'
view(choice_code)

#4.3==================================================
#Create a new choice variable choice_rev.
choice_code[,'choice_rev'] = paste(choice_code[,'scode_rev'],choice_code[,'pgm_rev'])
view(choice_code)

#4.4==================================================
#Recalculate the cutoff and the quality for each recoded choice.
admitstu1 = admitstu[,c('score','admitted','admitpgm')]
admitstu1 = data.frame(admitstu1)
#Recode the schoolcode in dataset admitstu1
scode_rev = substring(admitstu1[,2],1,3)
admitstu1 = cbind(admitstu1,scode_rev)
#Recode the program in dataset admitstu1
admitstu1[which(admitstu1[,3] == 'General Arts'),'pgm_rev'] = 'arts'
admitstu1[which(admitstu1[,3] == 'Visual Arts'),'pgm_rev'] = 'arts'
admitstu1[which(admitstu1[,3] == 'Business'),'pgm_rev'] = 'economics'
admitstu1[which(admitstu1[,3] == 'Home Economics'),'pgm_rev'] = 'economics'
admitstu1[which(admitstu1[,3] == 'General Science'),'pgm_rev'] = 'science'
admitstu1[which(is.na(admitstu1[,5])==TRUE),'pgm_rev'] = 'others'
admitstu1[,'choice_rev'] = paste(admitstu1[,'scode_rev'],admitstu1[,'pgm_rev'])
admitstu2 = admitstu1[,c('score','choice_rev')]
#Use aggregate to calculate the cutoff and quality of each choice_rev
choice_rev_cutoff = aggregate(x=admitstu2[,'score'],by=list(admitstu2$choice_rev),FUN=min)
colnames(choice_rev_cutoff)=c('choice_rev','cutoff')
view(choice_rev_cutoff)
choice_rev_quality = aggregate(x=admitstu2[,'score'],by=list(admitstu2$choice_rev),FUN=mean)
colnames(choice_rev_quality)=c('choice_rev','quality')
view(choice_rev_quality)
choice_rev = merge(admitstu2,choice_rev_cutoff,by='choice_rev',all.x=TRUE)
choice_rev = merge(choice_rev,choice_rev_quality,by='choice_rev',all.x=TRUE)
view(choice_rev)

#4.5==================================================
#Consider the 20,000 highest score students.
#The rest of the assignment uses the recoded choices and the 20,000 highest score students.
admitstu = admitstu[order(admitstu$score,decreasing = TRUE),]
admitstu = admitstu[1:20000,]
#Process the data of the first choice of each student
admitstu1 = admitstu[,c('score','schoolcode1','choicepgm1')]
admitstu1 = data.frame(admitstu1)
#Recode the schoolcode in dataset admitstu1
scode_rev = substring(admitstu1[,2],1,3)
admitstu1 = cbind(admitstu1,scode_rev)
#Recode the program in dataset admitstu1
admitstu1[which(admitstu1[,3] == 'General Arts'),'pgm_rev'] = 'arts'
admitstu1[which(admitstu1[,3] == 'Visual Arts'),'pgm_rev'] = 'arts'
admitstu1[which(admitstu1[,3] == 'Business'),'pgm_rev'] = 'economics'
admitstu1[which(admitstu1[,3] == 'Home Economics'),'pgm_rev'] = 'economics'
admitstu1[which(admitstu1[,3] == 'General Science'),'pgm_rev'] = 'science'
admitstu1[which(is.na(admitstu1[,5])==TRUE),'pgm_rev'] = 'others'
admitstu1[,'choice_rev'] = paste(admitstu1[,'scode_rev'],admitstu1[,'pgm_rev'])
admitstu2 = admitstu1[,c('score','choice_rev')]
#Use aggregate to calculate the cutoff and quality of each choice_rev
choice_rev_cutoff = aggregate(x=admitstu2[,'score'],by=list(admitstu2$choice_rev),FUN=min)
colnames(choice_rev_cutoff)=c('choice_rev','cutoff')
choice_rev_quality = aggregate(x=admitstu2[,'score'],by=list(admitstu2$choice_rev),FUN=mean)
colnames(choice_rev_quality)=c('choice_rev','quality')
choice_rev = merge(admitstu2,choice_rev_cutoff,by='choice_rev',all.x=TRUE)
choice_rev = merge(choice_rev,choice_rev_quality,by='choice_rev',all.x=TRUE)
view(choice_rev)


#Exercise5============================================
#Using the new data with recoded choices, we want to understand the effect of the student test score on his first choice.
#5.1==================================================
#Propose a model specification. Write the Likelihood function.

choice_rev[,'choice'] = match(choice_rev$choice_rev,sort(unique(choice_rev$choice_rev)))
choice_rev1 = choice_rev[,c('choice','score')]

install.packages("nnet")
library(nnet)
reg1 = multinom(choice ~ score, data=choice_rev1)
coef1 = coef(reg1)

#Write the likelihood function
like1 = function(param,dat)
{
  score = dat$score
  ch = dat$choice 
  ni = nrow(dat) 
  nj = length(unique(dat$choice))
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1 = param[1:nj]
  pn2 = param[(nj+1):(2*nj)]
  for (j in 1:nj)
  {
    ut[,j] = score*pn1[j] + pn2[j]
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
    {
    probc[i] = prob[i,ch[i]]
    }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = log(probc)
  return(-sum(like))
}
nj = length(unique(choice_rev1$choice))
start1 = runif(2*nj,min=-0.0001,max=0.0001)
opt1 = optim(start1,fn=like1,method="BFGS",control=list(trace=5,REPORT=1,maxit=100),dat=choice_rev1,hessian=FALSE)

#5.2==================================================
#Estimate parameters and compute the marginal effect of the proposed model.
opt1$par #estimate parameters

#Compute the marginal effect
beta1 = mat.or.vec(2,nj)
beta1[1,2:246] = coef1[1:245,1]
beta1[2,2:246] = coef1[1:245,2]
X1 = cbind(rep(1,nrow(choice_rev1)),choice_rev1$score)
Y_hat1 = X1 %*% beta1
pr1 = mat.or.vec(20000,246)
for (i in 1:246) {
  pr1[,i] = dnorm(Y_hat1[,i])
}
ME1 = as.data.frame(apply(pr1,2,mean))
ME1 = cbind(unique(choice_rev$choice_rev),ME1)
colnames(ME1) = c('choice','ME')
view(ME1)

#Exercise6============================================
#Using the new data with recoded choices, we want to understand the effect of the school quality on the first choice.
#6.1==================================================
#Propose a model specification. Write the Likelihood function.
choice_rev2 = choice_rev[,c('choice','quality')]
reg2 = multinom(choice ~ quality, data=choice_rev2)
coef2 = coef(reg2)

#Write the likelihood function
like2 = function(param,dat)
{
  quality = dat$quality
  ch = dat$choice
  ni = nrow(dat)
  nj = length(unique(dat$choice))
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1 = param[1:nj]
  pn2 = param[(nj+1):(2*nj)]
  for (j in 1:nj)
  {
    ut[,j] = quality*pn1[j] + pn2[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}
start2 = runif(2*nj,min=-0.0001,max=0.0001)
opt2 = optim(start2,fn=like2,method="BFGS",control=list(trace=5,REPORT=1,maxit=100),dat=choice_rev2,hessian=FALSE)

#6.2==================================================
#Estimate parameters and compute marginal effect of the proposed model.
opt2$par

#Compute the marginal effect
beta2 = mat.or.vec(2,nj)
beta2[1,2:246] = coef2[1:245,1]
beta2[2,2:246] = coef2[1:245,2]
X2 = cbind(rep(1,nrow(choice_rev2)),choice_rev2$quality)
Y_hat2 = X2 %*% beta2
pr2 = mat.or.vec(20000,246)
for (i in 1:246) {
  pr2[,i] = dnorm(Y_hat2[,i])
}
ME2 = as.data.frame(apply(pr2,2,mean))
ME2 = cbind(unique(choice_rev$choice_rev),ME2)
colnames(ME2) = c('choice','ME')
view(ME2)

#Exercise7============================================
#In this exercise, we are interested in the effect of excluding choices where the program is “Others”.
#7.1==================================================
#Explain and justify, which model (first or second model) you think is appropriate to conduct this exercise.
#Delete the rows whose program is "others"
admitstu3 = admitstu1[-which(admitstu1$pgm_rev == 'others'),]
admitstu3 = admitstu3[,c('score','choice_rev')]
#Use aggregate to calculate the cutoff and quality of each choice_rev
choice_rev_cutoff1 = aggregate(x=admitstu3[,'score'],by=list(admitstu3$choice_rev),FUN=min)
colnames(choice_rev_cutoff1)=c('choice_rev','cutoff')
choice_rev_quality1 = aggregate(x=admitstu3[,'score'],by=list(admitstu3$choice_rev),FUN=mean)
colnames(choice_rev_quality1)=c('choice_rev','quality')
choice_rev_delete = merge(admitstu3,choice_rev_cutoff1,by='choice_rev',all.x=TRUE)
choice_rev_delete = merge(choice_rev_delete,choice_rev_quality1,by='choice_rev',all.x=TRUE)
view(choice_rev_delete)

#Run the first model: choice~score
choice_rev_delete[,'choice'] = match(choice_rev_delete$choice_rev,sort(unique(choice_rev_del$choice_rev)))
choice_rev3 = choice_rev_delete[,c('choice','score')]
nj_3 = length(unique(choice_rev3$choice))
start3 = runif(2*nj_3,min=-0.0001,max=0.0001)
opt1_delete = optim(start3,fn=like1,method="BFGS",control=list(trace=5,REPORT=1,maxit=100),dat=choice_rev3,hessian=FALSE)

#Run the second model: choice~quality
choice_rev4 = choice_rev_delete[,c('choice','quality')]
nj_4 = length(unique(choice_rev4$choice))
star4 = runif(2*nj_4,min=-0.0001,max=0.0001)
opt2_delete = optim(start4,fn=like2,method="BFGS",control=list(trace=5,REPORT=1,maxit=100),dat=choice_rev4,hessian=FALSE)

#The second model is better, because the return likelihood is smaller.

#7.2==================================================
#Calculate choice probabilities under the appropriate model.
reg4 = multinom(choice ~ quality, data=choice_rev4)
coef4 = coef(reg4)
beta4 = mat.or.vec(2,nj_4)
beta4[1,2:195] = coef4[1:194,1]
beta4[2,2:195] = coef4[1:194,2]
X4 = cbind(rep(1,nrow(choice_rev4)),choice_rev4$quality)
Y_hat4 = X4 %*% beta4
pr4 = mat.or.vec(nrow(Y_hat4),195)
for (i in 1:195) {
  pr4[,i] = dnorm(Y_hat4[,i])
}
view(pr4)


