
###Data Analysis for Master's Thesis of Annie Waldman
###Upload Date: March 23, 2014
###Contact: annie.waldman@gmail.com

#Please feel free to the review the code and play around with it. 

#I rename my scraped data first (for organizational purposes). This data represents all
#of the conviction information for the people who went to the parole board between December 2011
#and December 2013.
conviction <- parole

#Next, I read in the second data set that I am going to merge with my first data set. This 
#data represents all the people who went to the parole board between December 2011 and 
#December 2013. 
board <- read.csv("parole.csv")
View(board)
#There are 24290 total rows (representing interview dates) in the board data.
dim(board)
View(conviction)
#However, of this total number of interview dates, only 23,660 have conviction information.
dim(conviction)

#Merge the two datasets by NYSID number, make a master data set, and then eventually we
#will slim it down.
all <- merge(board, conviction, by="NYSID")
View(all)

#As you can see, this new data set that has the merged parole board interview data with the
#conviction data has created many duplicates in the data set.
dim(all)

#Before we slim out the data, let's take out some of the redundant columns (here I'm selecting
#only the columns I want to keep) and calling it med.all
med.all <- all[,c(1, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36) ]
#Now there are only 33 columns, instead of 36.
dim(med.all)
length(med.all)


####Now let's rename some of the variables to make them more manageable. 
#Renaming SEX to gender.
names(med.all)[2] <- "gender"

#Renaming BIRTH.DATE to birthdate
names(med.all)[3] <- "birthdate"

#Remaning RACE...ETHINICITY to race:
names(med.all)[4] <- "race"

#Renaming HOUSING.OR.INTERVIEW.FACILI to interview_loc:
names(med.all)[5] <- "interview_loc"

#Renaming PAROLE BOARD INTERVIEW DATE to interview_date:
names(med.all)[6] <- "interview_date"

#Renaming PAROLE BOARD INTERVIEW TYPE to interview_type:
names(med.all)[7] <- "interview_type"

#Renaming PAROLE BOARD INTERVIEW TYPE to interview_decision:
names(med.all)[8] <- "interview_decision"

#Renaming AGGREGATED MIN SENTENCE to min_sentence:
names(med.all)[11] <- "min_sentence"

#Renaming AGGREGATED MAX SENTENCE to max_sentence:
names(med.all)[12] <- "max_sentence"

#Renaming Parole Eligibility Date to prl_elif_date:
names(med.all)[16] <- "prl_elig_date"

#Renaming Conditional Release Date to "cond_rls_date":
names(med.all)[17] <- "cond_rls_date"

#Renaming Maximum Expiration Date to "max_expiration":
names(med.all)[18] <- "max_expiration"



#Now that the columns are renamed, let's start digging into the data by looking
#at some of the descriptors of the text:

#Gender
summary(med.all$gender)
#Results: FEMALE 2462   MALE 36352  UNK  86 
#Mainly men in the data set, and 86 unknown values, should figure out what these mean. 
#Issue that has come up, there are many many duplicates, need to figure out how to take them out.

#Birthdate/Age
summary(med.all$birthdate)
#Now, I need to get age, which will be more useful for looking at the data.
#First, convert the birthdate to R format:
med.all$newbdate <- as.Date(med.all$birthdate, "%m/%d/%Y")
#Convert interview date to format:
med.all$newintvdate <- as.Date(med.all$interview_date, "%m/%d/%Y")
View(med.all)
#Now try to get age
age_years <- function(med.all$newbdate, med.all$newintvdate) 
{ 
  lt <- data.frame(med.all$newbdate, med.all$newintvdate) 
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y")) 
  first <- as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")) 
  age[which(med.all$newbdate > lt[,2])] <- age[which(med.all$newbdate > lt[,2])] - 1 
  age 
}
View(age)

#add the age on:
med.all$age <- age
View(med.all)
#yay! Now there is a column for age. The summary for age:
#Min.    1st Qu.  Median    Mean    3rd Qu.    Max.    
#17.00   31.00    41.00     40.99   50.00     90.00    

###Race
summary(med.all$race)
#AMER IND/ALSK ASIAN/PACIFIC         BLACK      HISPANIC         OTHER 
#398           282         16859          7606           389 
#UNKNOWN         WHITE 
#781         12585 

#Interview location:
summary(med.all$interview_loc)

#New Interview Date:
summary(med.all$newintvdate)

#Interview type
summary(med.all$interview_type)
#DEPORT      ECPDO    INITIAL    MEDICAL MERIT TIME        PIE   PV REAPP 
#3        224      19190         34       6097       1533        440 
#REAPPEAR RESCISSION  SP CONSDR SUPP MERIT 
#10536        300        511         32 

###Interview decision:
summary(med.all$interview_decision)
#*  **********      DENIED     GRANTED  NOT GRANTD   OPEN DATE 
#431         547       18023         237        3890        6096 
#OR EARLIER     PAROLED  RCND&HOLD; RCND&RELSE;   REINSTATE 
#9416         114          53          71          22 

###Min Sentence
summary(med.all$min_sentence)

#Now in order to better understand my data and analyze it, I have to convert it to a factor.
med.all$min_sentence <- as.factor(med.all$min_sentence)
summary(med.all$min_sentence)
View(med.all)
dim(med.all)
###Max Sentence
med.all$max_sentence <- as.factor(med.all$max_sentence)
summary(med.all$max_sentence)

###Facility
med.all$Facility <- as.factor(med.all$Facility)
summary(med.all$Facility)

###Crime 1
med.all$Crime1 <- as.factor(med.all$Crime1)
summary(med.all$Crime1)

###Crime 2
med.all$Crime2 <- as.factor(med.all$Crime2)
summary(med.all$Crime2)

###Crime 3
med.all$Crime3 <- as.factor(med.all$Crime3)
summary(med.all$Crime3)

###Crime 4:
med.all$Crime4 <- as.factor(med.all$Crime4)
summary(med.all$Crime4)

###Age:
summary(med.all$age)
plot(med.all$age)

#But before I can really understand the data, I have to take out the duplicates:
#First need to collapse all the strings into characters, name this b.
#Find the duplicates. The length is the same which is great.
b = apply(med.all,1,paste,collapse=" ")
length(b)

#Find the sume of the duplicated:
sum(duplicated(b))
#15240 duplicates.
View(b)

#Take away the duplicated ones from b in med.all
med.all = med.all[!duplicated(b),]
dim(med.all)
#My number of observations went from 38900 to 23660 (the number of people with convinctions
#at the parole board). Happy this matches up.

#Reorder the interview data:
reordered_med.all = med.all[order(med.all$newintvdate),]
View(reordered_med.all)

#Now creating a new category for year.
x <- med.all$interview_date
year = gsub("(.*)/(.*)/(.*)", "\\3", x)
med.all$year <- year
View(med.all)
summary(med.all$year)
med.all$year <- as.factor(med.all$year)
summary(med.all)
# *   :  575  
#2011: 1534  
#2012:11229  
#2013:10322

#Now ordering my interview date and creating tables for each year.
med.all2 = med.all[order(med.all$newintvdate),]
reordered_2011 <- med.all[reordered_med.all$year == "2011",]
reordered_2012 <- med.all[reordered_med.all$year == "2012",]
reordered_2013 <- med.all[reordered_med.all$year == "2013",]
reordered_star <- med.all[reordered_med.all$year == "*",]
View(reordered_2011)
View(reordered_2012)
View(reordered_2013)
View(reordered_star)

sum(duplicated(reordered_2011$NYSID))
sum(duplicated(reordered_2012$NYSID))
sum(duplicated(reordered_2013$NYSID))
sum(duplicated(reordered_star$NYSID))

reordered_2011 = reordered_2011[order(reordered_2011$newintvdate,decreasing=TRUE),]
reordered_2012 = reordered_2012[order(reordered_2012$newintvdate,decreasing=TRUE),]
reordered_2013 = reordered_2013[order(reordered_2013$newintvdate,decreasing=TRUE),]
reordered_star = reordered_star[order(reordered_star$newintvdate,decreasing=TRUE),]

reordered_2011 = reordered_2011[!duplicated(reordered_2011$NYSID),]
reordered_2012 = reordered_2012[!duplicated(reordered_2012$NYSID),]
reordered_2013 = reordered_2013[!duplicated(reordered_2013$NYSID),]
reordered_star = reordered_star[!duplicated(reordered_star$NYSID),]

summary(reordered_2011)
summary(reordered_2012)
summary(reordered_2013)


#Check the head of the data:
head(med.all2)

#Look at how many are duplicated using the NYSID:
sum(duplicated(med.all2$NYSID))
sum(duplicated(reordered_med.all$NYSID))

#Now order it based on the most recent dates:
med.all2 = med.all[order(med.all$newintvdate,decreasing=TRUE),]


#Now take away all the duplicates after the first instance.
med.all2 = med.all2[!duplicated(med.all2$NYSID),]
View(med.all2)
summary(med.all2)

#Now look at the summaries again and see what has changed:
#First up, gender:
summary(med.all2$gender)
gend = table(med.all2$gender)
hist(gend)
require("RColorBrewer")
barplot(gend,
        col = brewer.pal(3, "Set3"),
        names = c("Female",
                  "Male",
                  "Unknown"),
        boxwex = 0.5, #width of box
        whisklty = 1, #whisker line type is 1 = solid line
        staplelty = 0, #staple type, 0 = none
        main = "Parole Interviews 2012-2013: Gender",
        xlab = "Gender",
        ylab = "Number of Prisoners")

#Now look up age:
summary(med.all2$age)
#The min is 17 years old, the media is 41, the mean is 41.01, and the max is 90. 293 people
#do not have their ages labeled. That is interesting.
agetab <- table(med.all2$age)
View(agetab)
barplot(agetab,
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Frequency of Age",
        ylab = "Frequency",
        xlab = "Age in years at parole board interview",
        ylim = c(0, 650)
        )

menage <- table(med.all2$age[med.all2$gender == "MALE"])
summary(menage)
barplot(menage)
View(agetab)
barplot(agetab,
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Frequency of Age for Men",
        ylab = "Frequency",
        xlab = "Age in years at parole board interview",
        ylim = c(0, 650)
)

#So interesting because of the peak in middle age.

#Now let's look at race:
summary(med.all2$race)
#AMER IND/ALSK ASIAN/PACIFIC         BLACK      HISPANIC         OTHER       UNKNOWN         WHITE 
#165            78          7793          3363           152           316          5641 

#Now let's look at barplot of race
racetab <- table(med.all2$race)
View(racetab)
barplot(racetab,
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Frequency of Race",
        ylab = "Frequency",
        xlab = "Race of parole interviewee",
       ylim = c(0, 8000)
)

#Now let's look at interview loc:
intvtab <- med.all2[order(med.all2$interview_loc),]
intvtab2 <- sort(table(med.all2$interview_loc))
View(intvtab2)
barplot(intvtab[order(intvtab, decreasing = T)],
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Frequency of Interview Location",
        ylab = "Frequency",
        xlab = "Interview location of parole interviewee",
        ylim = c(0, 1000),
        cex.axis=.8,
        cex.names=0.5
)

#Now let's look at the interview date and see if there is a time in the month that is most popular:
x = med.all2$interview_date
month = gsub("(.*)/(.*)/(.*)", "\\1", x)
View(month)
med.all2$month <- month
View(med.all2)
med.all2$month <- as.numeric(med.all2$month)
monthtab = table(med.all2$month)
View(monthtab)
barplot(monthtab,
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Frequency of Interview Month",
        ylab = "Frequency",
        xlab = "Interview month for parole board",
        ylim = c(0, 2000)
)

#Now looking at interview decision:
interviewtype <- table(med.all2$interview_type)
View(interviewtype)
barplot(interviewtype[order(interviewtype, decreasing = T)],
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Frequency of Interview Type",
        ylab = "Frequency",
        xlab = "Interview type of parole interviewee",
        ylim = c(1, 10000)
)

#Now looking at interview decision:
interviewdec <- table(med.all2$interview_decision)
View(interviewdec)
barplot(interviewdec[order(interviewdec, decreasing = T)],
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Frequency of Interview Decision",
        ylab = "Frequency",
        xlab = "Interview type of parole decision",
        ylim = c(1,13500)
)

#Now looking at release type:
med.all2$Release_Type <- as.factor(med.all2$Release_Type)
summary(med.all2$Release_Type)
#BOARD    CR    JS    PR 
#9289  3944  4258     3    14 

#Now looking at facility:
summary(med.all2$Facility)
facilitytab <- table(med.all2$Facility)
View(facilitytab)
barplot(facilitytab[order(facilitytab, decreasing = T)],
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Location of Inmates",
        ylab = "Frequency",
        xlab = "Housing Location of Inmates Seeking Parole",
        ylim = c(0, 1000)
)

View(med.all2)

#Looking at some of the crimes:
summary(med.all2$Crime1)
crime1tab <- table(med.all2$Crime1)
View(crime1tab)
barplot(crime1tab[order(crime1tab, decreasing = T)],
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Location of Inmates",
        ylab = "Frequency",
        xlab = "Housing Location of Inmates Seeking Parole",
        ylim = c(0, 2250)
)

#A lot of crimes. Murder is the most common. 
#Now looking at crime2
#Looking at some of the crimes:
summary(med.all2$Crime2)
crime2tab <- table(med.all2$Crime2)
View(crime2tab)
barplot(crime2tab[order(crime2tab, decreasing = T)],
        col = "steelblue",
        border = NULL,
        main = "Parole Board Interviews 2012-2013: Location of Inmates",
        ylab = "Frequency",
        xlab = "Housing Location of Inmates Seeking Parole",
        ylim = c(0, 1000)
)
##And so forth.

##Now let's dig into the data on the lifers:
sum(med.all2$max_expiration == "LIFE SENTENCE")
#Total are 3325.
##Create new table of just the lifers:
lifers <- c(med.all2$max_expiration == "LIFE SENTENCE")


#Create a new variable for min_sentence
b =  med.all2$min_sentence
b
#Make this variable into a character
b = as.character(b)
b
#Create a decimal number of the minimum year. 
med.all2$min_sent_yr = as.numeric(substr(b,1,2))+as.numeric(substr(b,4,5))/12

#Now do the same for maximum sentence (to make into a decimal):
b =  med.all2$max_sentence
b = as.character(b)
med.all2$max_sent_yr = as.numeric(substr(b,1,2))+as.numeric(substr(b,4,5))/12

#Now what to do about the "LIFE" sentences:
b[is.na(med.all2$max_sent_yr)]
med.all2$max_sent_yr[is.na(med.all2$max_sent_yr)]= Inf
View(med.all2)

#Only issue is when I take the mean of the max sentence, it will be Inf because
#of the life sentences.
mean(med.all2$max_sent_yr)

#One way to solve this is by saying bring me back all but Inf for the mean.
mean(med.all2$max_sent_yr[med.all2$max_sent_yr!=Inf])

#Now this shows a box plot that shows age and crime1 together, but way too much data.
boxplot(split(med.all2$age,med.all2$Crime1))

#This assigns it to bys so we can look at it as a table and sort crimes per age.
bys = split(med.all2$age,med.all2$Crime1)

#Check out the head of the data table.
head(bys)

#Look at the length (or number of frequencies) of all the variables in bys
sapply(bys,length)

#Now sort the data as seen above.
sort(sapply(bys,length))

#Now only take crimes that have over 200 people committing them as crime1.
bys2 = bys[sapply(bys,length)>200]

#Check out the boxplot
boxplot(bys2)

#Add names to the boxplot.
boxplot(bys2,las=2)

#Make the names smaller:
boxplot(bys2,las=2,cex.axis=0.5)

#Now order the data and look at the median ages across (with outliers of course):
bys2 = bys2[order(sapply(bys2,median,na.rm=TRUE))]
boxplot(bys2,las=2,cex.axis=0.5,horizontal=TRUE)

#Now look at race and crime1
b = table(med.all2$Crime1,med.all2$race)
b
#Look at sums of race and crime.
apply(b,2,sum)
apply(b,1,sum)

#Look at the dimensions of the mosiac plot (22 crimes, 7 races)
dim(b)

#Only look at crimes with over 200 people:
b = b[apply(b,1,sum)>200,]

#Make a mosiac plot:
mosaicplot(b)

#Turn the text vertical
mosaicplot(b,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(b,las=2,col=cc)

#Change the color to something more contrasting:
cc = brewer.pal(7,"Blues")
mosaicplot(b,las=2,col=cc)
b

#Combine the race groups that are less than 10%. 
b2 = b
apply(b[,-c(3,4,7)],2,sum)
b2 = cbind(b[,3:4],b[,7],apply(b[,-c(3,4,7)],1,sum))
cc = brewer.pal(4,"Blues")

#Amazing mosaic plot:
mosaicplot(b2,las=2,col=cc)

#Now let's create a new table, only looking at the parole grantees and denied (taking
#away the ones with their dates rescheduled)

#Summary of interview decision:
summary(med.all2$interview_decision)

#From this we can see that 269 have not happened yet, 11803 were denied, 181 were granted
#parole, 589 were not granted. 3987 have an open date. 554 have Or Earlier. 86 were paroled.
#5 were RCND&HOLD, 25 were RCND&RELSE, and 8 were REINSTATE.

#Some help with the data (https://www.parole.ny.gov/calendardatadefinitions.html)
#RCND&RELSE: RESCIND ORIGINAL RELEASE DATE/NEW DATE -- this means that the board says ok
#and then new information comes in before your release that nullifies your parole. This is
#in fact a denial. 
#REINSTATE: REINSTATE ORIGINAL RELEASE DATE -- this is when parole was granted, rescinded, 
#and then granted again. These inmates are free to go.
#RCND&HOLD: RESCIND ORIGINAL RELEASE DATE & HOLD -- this means that the board says ok
#and then new information comes in before your release that nullifies your parole. This is
#in fact a denial. In this case, there will likely be a trial, and then it will be decided
#on again.
#GRANTED: CPDO GRANTED (or merit time?) -- this should be double checked, but this is conditional parole for
#deportation only. These individuals are immigrants in the prison system who will be deported
#back to their countries. But this category could also mean that the person has been let out
#on merit time, meaning they had an indeterminate sentence with a minimum of six years, the Merit Time 
#allowance would equal one year. This does not qualify for A-1 felons (other than A-1 drug offense),
#A Penal Law sec. 70.02 violent felony offense; Manslaughter 2nd degree; Vehicular 
#manslaughter 1st degree; Criminally negligent homicide; Penal Law Articles 130 and 263 
#offenses; or Incest. Also the inmate must also have successfully participated in a work 
#or treatment program assigned pursuant to Correction Law and have accomplished one of 
#the following: Earned a GED; Acquired an ASAT Certificate; Earned a Vocational Trade 
#Certificate after six months of vocational programming; or Performed 400 hours of 
#community service on a work crew.
#NOT GRANTED: CPDO/MERIT TIME NOT GRANTED: not out on merit time, this is counted as DENIAL.
#PAROLED: PAROLED - STRAIGHT DATE: This means that you are paroled, and you already have
#approved residence and employment prior to the parole board interview.
#OPEN DT: PAROLED - OPEN DATE - open date case, the responsibility for finding a job, 
#a place to live, and other elements of the parole program are shared by the inmate and 
#the designated Parole field office to which the inmate will be released. The inmate will 
#reappear before the Parole Board if you have not developed an approved parole 
#plan in the community within six months of being granted an open date status.
#OR EARLIER: OR EARLIER/POSTPONEMENT: postponed, treating this as not granted parole.
#DENIED: DENIED -- obvious.
summary(med.all2$race)

#Note: before breaking down each category by race, it is critical to look at the parolees as a percentage of 
#who is going to parole board per crime. This will be broken down at the bottom.
p1 <- med.all2[med.all2$interview_decision == "OPEN DATE",]
p2 <- med.all2[med.all2$interview_decision == "PAROLED",]
p <- rbind(p1, p2)
View(p)
summary(p$interview_decision)
summary(p$race)
summary(np$race)
np <- med.all2[med.all2$interview_decision !="PAROLED" & med.all2$interview_decision !="OPEN DATE",]
summary(np$interview_decision)
dim(p)
dim(np)
summary(table(p$interview_date))




#Now I have a data frame of all the paroled individuals (p) and the 
#nonparoled individuals (np).

#Let's look at some of the breakdown.....

#First up, age.

par_age = table(p$age)
barplot(par_age)
npar_age = table(np$age)
barplot(npar_age)

#Now let's look at race.
par_race = table(p$race)
barplot(par_race)
npar_race = table(np$race)
barplot(npar_race)

#Now let's look at crime.
par_crime1 = table(p$Crime1)
View(par_crime1)
cp = brewer.pal(9,"Blues")
barplot(sort(par_crime1[par_crime1>100]),
        col = cp,
        las=2,
        ylim = c(0, 600),
        cex.axis=0.8,
        main = "Most common crimes of inmates who receive parole",
        ylab = "Frequency",
        xlab = "Crimes",
        cex.names=0.5)
#        horiz=TRUE)
?barplot
npar_crime1 = table(np$Crime1)
cp = brewer.pal(9,"Blues")
barplot(sort(npar_crime1[npar_crime1>300]),
        col = cp,
        las=2,
        main = "Most common crimes of inmates who did not receive parole",
        ylab = "Frequency",
        ylim = c(0, 2000),
        cex.axis=0.8,
        cex.names=0.5)

#Now let's look at crime overall.
all_crime = table(med.all3$Crime1)
View(all_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(all_crime[all_crime>400]),
        col = cp,
        las=2,
        ylim = c(0, 2000),
        cex.axis=0.8,
        main = "Most common crimes of inmates facing the parole board",
        ylab = "Frequency",
        cex.names=0.5)
#        horiz=TRUE)

#Now lets look at race and crime1 with parole and not parole:
race_crime_parole = table(p$Crime1,p$race)
race_crime_parole = race_crime_parole[apply(race_crime_parole,1,sum)>100,]
mosaicplot(race_crime_parole)
#Turn the text vertical
mosaicplot(race_crime_parole,las=2,col = cp)

#Use color "Blues":
cc<-brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(race_crime_parole,las=2,col=cc)

#Combine the race groups that are less than 10%. 
race_crime_parole2 = race_crime_parole
apply(race_crime_parole[,-c(3,4,7)],2,sum)
race_crime_parole2 = cbind(race_crime_parole[,3:4],race_crime_parole[,7],apply(race_crime_parole[,-c(3,4,7)],1,sum))
colnames(race_crime_parole2)[3] <- "WHITE"
colnames(race_crime_parole2)[4] <- "OTHER"
cc = brewer.pal(4,"Blues")

#Amazing mosaic plot:
cp = brewer.pal(5,"Blues")
mosaicplot(race_crime_parole2,las=2,col=cp, main = "Race and crime of inmates who received parole")


#Now lets look at race and crime1 with not parole:
race_crime_notparole = table(np$Crime1,np$race)
race_crime_notparole = race_crime_notparole[apply(race_crime_notparole,1,sum)>300,]
mosaicplot(race_crime_notparole)
#Turn the text vertical
mosaicplot(race_crime_notparole,las=2)

#Use color "Blues":
cc = brewer.pal(9,"Blues")

#Make a mosaic plot:
mosaicplot(race_crime_notparole,las=2,col=cp, main = "Race and crime of inmates who received parole")

#Combine the race groups that are less than 10%. 
race_crime_notparole2 = race_crime_notparole
apply(race_crime_notparole[,-c(3,4,7)],2,sum)
race_crime_notparole2 = cbind(race_crime_notparole[,3:4],race_crime_notparole[,7],apply(race_crime_notparole[,-c(3,4,7)],1,sum))
colnames(race_crime_notparole2)[3] <- "WHITE"
colnames(race_crime_notparole2)[4] <- "OTHER"
cp = brewer.pal(4,"Blues")

#Amazing mosaic plot:
mosaicplot(race_crime_notparole2,las=2,col=cp, main = "Race and crime of inmates who did not receive parole")
?prop.table

#Now comes the issue that these are not a percentage of the whole. This must be done for
#further clarity. And then also I have to look at the specific crimes and see if for each crime
#whose percentage of the whole pop is getting parole more.

##Let's just look at a table of the murder2 inmates and see what percentage they are
#paroled based on race:
murder2_table <- med.all2[med.all2$Crime1 == "MURDER-2",]
View(murder2_table)
class(murder2_table)
murder2_table$race <- as.factor(murder2_table$race)
View(murder2_table)
summary(murder2_table$race)
#Now of the murder-2 inmates, lets look at just the race of parole.
murder2p1 <- murder2_table[murder2_table$interview_decision == "OPEN DATE",]
murder2p2 <- murder2_table[murder2_table$interview_decision == "PAROLED",]
murder2p <- rbind(murder2p1, murder2p2)
View(murder2p)
summary(murder2p$interview_decision)
summary(murder2p$race)
murder2np <- murder2_table[murder2_table$interview_decision !="PAROLED" & murder2_table$interview_decision !="OPEN DATE",]
summary(murder2np$interview_decision)
summary(murder2np$race)
#Now I can calculate what total of those paroled for each race are a percentage of the total going to the board for
#each crime. 

# Total white inmate parole success = 77
# Total white inmate going to board = 435
# Total release rate for white parolees going to board with murder charge as crime 1: 77/435 = .1770115 or 18% release rate

# Total black inmate parole success = 263
# Total black inmate going to board = 760
# Total release rate for black parolees going to board with murder charge as crime 1: 263/1028 = .257087 or 26% release rate

# Total hispanic inmate parole success = 103
# Total hispanic inmate going to board = 382
# Total release rate for hispanic parolees going to board with murder charge as crime 1: 103/382 = .2696335 or 27% release rate

#Now it's key to look at murder as crime 2 to see if the release rates differ.
murderCrime2_table <- med.all2[med.all2$Crime2 == "MURDER-2",]
class(murderCrime2_table)
murderCrime2_table$race <- as.factor(murderCrime2_table$race)
View(murderCrime2_table)
summary(murderCrime2_table$race)
#Now of the murder-2 second crime inmates, lets look at just the race of parole.
murderCrime2p1 <- murderCrime2_table[murderCrime2_table$interview_decision == "OPEN DATE",]
murderCrime2p2 <- murderCrime2_table[murderCrime2_table$interview_decision == "PAROLED",]
murderCrime2parole <- rbind(murderCrime2p1, murderCrime2p2)
View(murderCrime2parole)
summary(murderCrime2parole$interview_decision)
summary(murderCrime2parole$race)
murderCrime2np <- murderCrime2_table[murderCrime2_table$interview_decision !="PAROLED" & murderCrime2_table$interview_decision !="OPEN DATE",]
summary(murderCrime2np$interview_decision)
summary(murderCrime2np$race)

#Now I can calculate what total of those paroled for each race are a percentage of the total going to the board for
#each crime. 

# Total white inmate parole success = 5
# Total white inmate going to board = 27
# Total release rate for white parolees going to board with murder charge as crime 2: 5/32 = 0.15625 or 16% release rate

# Total black inmate parole success = 14
# Total black inmate going to board = 70
# Total release rate for black parolees going to board with murder charge as crime 2: 14/70 = .2 or 20% release rate

# Total hispanic inmate parole success = 4
# Total hispanic inmate going to board = 20
# Total release rate for hispanic parolees going to board with murder charge as crime 2: 4/24 = .1666667 or 17% release rate

#Let's try it now for another crime.
#Let's start with burglary.
burglaryParole_table <- med.all2[med.all2$Crime1 == "BURGLARY-3",]
View(burglaryParole_table)
class(burglaryParole_table)
burglaryParole_table$race <- as.factor(burglaryParole_table$race)
View(burglaryParole_table)
summary(burglaryParole_table$race)
#Now of the Burglary-3 inmates, lets look at just the race of parole.
burglaryParole2p1 <- burglaryParole_table[burglaryParole_table$interview_decision == "OPEN DATE",]
burglaryParole2p2 <- burglaryParole_table[burglaryParole_table$interview_decision == "PAROLED",]
burglaryParole2p <- rbind(burglaryParole2p1, burglaryParole2p2)
View(burglaryParole2p)
summary(burglaryParole2p$interview_decision)
summary(burglaryParole2p$race)
burglaryParole2np <- burglaryParole_table[burglaryParole_table$interview_decision !="PAROLED" & burglaryParole_table$interview_decision !="OPEN DATE",]
summary(burglaryParole2np$interview_decision)
summary(burglaryParole2np$race)

#Now I can calculate what total of those paroled for each race are a percentage of the total going to the board for
#each crime. 

# Total white inmate parole success = 226
# Total white inmate going to board = 614
# Total release rate for white parolees going to board with burglary 3 charge as crime 1: 226/614 = 0.3680782 or 36.8% release rate

# Total black inmate parole success = 129
# Total black inmate going to board = 537
# Total release rate for black parolees going to board with burglary 3  charge as crime 1: 129/537 = 0.2402235 or 24.0% release rate

# Total hispanic inmate parole success = 61
# Total hispanic inmate going to board = 248
# Total release rate for hispanic parolees going to board with burglary 3 charge as crime 1: 61/248 = 0.2459677 or 24.0% release rate

#Let's try it now for another crime.
#Let's start with robbery
robberyParole_table <- med.all2[med.all2$Crime1 == "ROBBERY-3",]
View(robberyParole_table)
class(robberyParole_table)
robberyParole_table$race <- as.factor(robberyParole_table$race)
View(robberyParole_table)
summary(robberyParole_table$race)
#Now of the robbery 3 inmates, lets look at just the race of parole.
robberyParole2p1 <- robberyParole_table[robberyParole_table$interview_decision == "OPEN DATE",]
robberyParole2p2 <- robberyParole_table[robberyParole_table$interview_decision == "PAROLED",]
robberyParole2p <- rbind(robberyParole2p1, robberyParole2p2)
View(robberyParole2p)
summary(robberyParole2p$interview_decision)
summary(robberyParole2p$race)
robberyParole2np <- robberyParole_table[robberyParole_table$interview_decision !="PAROLED" & robberyParole_table$interview_decision !="OPEN DATE",]
summary(robberyParole2np$interview_decision)
summary(robberyParole2np$race)

#Now I can calculate what total of those paroled for each race are a percentage of the total going to the board for
#each crime. 

# Total white inmate parole success = 52
# Total white inmate going to board = 171
# Total release rate for white parolees going to board with robbery 3 charge as crime 1: 52/171 = 0.3040936 or 30.4% release rate

# Total black inmate parole success = 80
# Total black inmate going to board = 480
# Total release rate for black parolees going to board with robbery 3 charge as crime 1: 80/480 = 0.1666667 or 16.7% release rate

# Total hispanic inmate parole success = 22
# Total hispanic inmate going to board = 177
# Total release rate for hispanic parolees going to board with robbery 3 charge as crime 1: 22/177 = 0.1242938 or 12.4% release rate

#Let's try it now for another crime.
#Let's start with grand larceny 4
grandLarceny4Parole_table <- med.all2[med.all2$Crime1 == "GRAND LARCENY-4",]
View(grandLarceny4Parole_table)
class(grandLarceny4Parole_table)
grandLarceny4Parole_table$race <- as.factor(grandLarceny4Parole_table$race)
View(grandLarceny4Parole_table)
summary(grandLarceny4Parole_table$race)
#Now of the grand larceny 4 inmates, lets look at just the race of parole.
GL42p1 <- grandLarceny4Parole_table[grandLarceny4Parole_table$interview_decision == "OPEN DATE",]
GL42p2 <- grandLarceny4Parole_table[grandLarceny4Parole_table$interview_decision == "PAROLED",]
GL42p <- rbind(GL42p1, GL42p2)
View(GL42p)
summary(GL42p$interview_decision)
summary(GL42p$race)
GL42np <- grandLarceny4Parole_table[grandLarceny4Parole_table$interview_decision !="PAROLED" & grandLarceny4Parole_table$interview_decision !="OPEN DATE",]
View(GL42np)
summary(GL42np$interview_decision)
summary(GL42np$race)

#Now I can calculate what total of those paroled for each race are a percentage of the total going to the board for
#each crime. 

# Total white inmate parole success = 118
# Total white inmate going to board = 266
# Total release rate for white parolees going to board with grand larceny 4 charge as crime 1: 118/266 = 0.443609 or 44.4% release rate

# Total black inmate parole success = 68
# Total black inmate going to board = 297
# Total release rate for black parolees going to board with grand larceny 4 charge as crime 1: 68/297 = 0.2289562 or 22.9% release rate

# Total hispanic inmate parole success = 32
# Total hispanic inmate going to board = 116
# Total release rate for hispanic parolees going to board with grand larceny 4 charge as crime 1: 32/116 = 0.2758621 or 27.6% release rate

#Let's try it now for another crome.
####DWI 2nd Offense
DWIParole_table <- med.all2[med.all2$Crime1 == "DWI 2ND OFF",]
View(DWIParole_table)
class(DWIParole_table)
DWIParole_table$race <- as.factor(DWIParole_table$race)
View(DWIParole_table)
summary(DWIParole_table$race)
#Now of the DWI 2nd Offense inmates, lets look at just the race of parole.
DWI2p1 <- DWIParole_table[DWIParole_table$interview_decision == "OPEN DATE",]
DWI2p2 <- DWIParole_table[DWIParole_table$interview_decision == "PAROLED",]
DWI2p <- rbind(DWI2p1, DWI2p2)
View(DWI2p)
summary(DWI2p$interview_decision)
summary(DWI2p$race)
DWI2np <- DWIParole_table[DWIParole_table$interview_decision !="PAROLED" & DWIParole_table$interview_decision !="OPEN DATE",]
summary(DWI2np$interview_decision)
summary(DWI2np$race)

#Now I can calculate what total of those paroled for each race are a percentage of the total going to the board for
#each crime. 

# Total white inmate parole success = 139
# Total white inmate going to board = 410
# Total release rate for white parolees going to board with DWI 2nd Offense charge as crime 1: 139/410 = 0.3390244 or 33.9% release rate

# Total black inmate parole success = 21
# Total black inmate going to board = 65
# Total release rate for black parolees going to board with DWI 2nd Offense charge as crime 1: 21/65 = 0.3230769 or 32.3% release rate

# Total hispanic inmate parole success = 16
# Total hispanic inmate going to board = 45
# Total release rate for hispanic parolees going to board with DWI 2nd Offense charge as crime 1: 16/45 = 0.3555556 or 35.6% release rate


#What about based on interview location?
#First lets look at the locations with most parole interview:
#59  COLLINS	488
#60	OTISVILLE	489
#61	MOHAWK	545
#62	GROVELAND	578
#63	WYOMING	581
#64	BARE HILL	593
#65	GREENE	636
#66	MIDSTATE	655
#67	FRANKLIN	734
#68	FISHKILL	805
#69	GOWANDA	883

#Let's start with Gowanda
GowandaParole_table <- med.all2[med.all2$Facility == "GOWANDA",]
View(GowandaParole_table)
class(GowandaParole_table)
GowandaParole_table$race <- as.factor(GowandaParole_table$race)
View(GowandaParole_table)
summary(GowandaParole_table$race)
#Now of the Gowanda inmates, lets look at just the race of parole.
GowandaP1 <- GowandaParole_table[GowandaParole_table$interview_decision == "OPEN DATE",]
GowandaP2 <- GowandaParole_table[GowandaParole_table$interview_decision == "PAROLED",]
GowandaP <- rbind(GowandaP1, GowandaP2)
View(GowandaP)
summary(GowandaP$interview_decision)
summary(GowandaP$race)
GowandaNP <- GowandaParole_table[GowandaParole_table$interview_decision !="PAROLED" & GowandaParole_table$interview_decision !="OPEN DATE",]
summary(GowandaNP$interview_decision)
summary(GowandaNP$race)

#Now I can calculate what total of those paroled for each race are a percentage of the total going to the board for
#Gowanda 

# Total white inmate parole success = 132
# Total white inmate going to board = 446
# Total release rate for white parolees going to board: 132/446 = 0.2959641 or 29.6% release rate

# Total black inmate parole success = 34
# Total black inmate going to board = 193
# Total release rate for black parolees going to board: 34/193 = 0.1761658 or 17.6% release rate

# Total hispanic inmate parole success = 11
# Total hispanic inmate going to board = 89
# Total release rate for hispanic parolees going to board: 11/89 = 0.1235955 or 12.4% release rate

#Let's go with Fishkill
FishkillParole_table <- med.all2[med.all2$Facility == "FISHKILL",]
View(FishkillParole_table)
class(FishkillParole_table)
FishkillParole_table$race <- as.factor(FishkillParole_table$race)
View(FishkillParole_table)
summary(FishkillParole_table$race)
#Now of the Fishkill inmates, lets look at just the race of parole.
FishkillP1 <- FishkillParole_table[FishkillParole_table$interview_decision == "OPEN DATE",]
FishkillP2 <- FishkillParole_table[FishkillParole_table$interview_decision == "PAROLED",]
FishkillP <- rbind(FishkillP1, FishkillP2)
View(FishkillP)
summary(FishkillP$interview_decision)
summary(FishkillP$race)
FishkillNP <- FishkillParole_table[FishkillParole_table$interview_decision !="PAROLED" & FishkillParole_table$interview_decision !="OPEN DATE",]
summary(FishkillNP$interview_decision)
summary(FishkillNP$race)

#Now I can calculate what total of those paroled for each race are a percentage of the total going to the board for
#at Fishkill

# Total white inmate parole success = 50
# Total white inmate going to board = 186
# Total release rate for white parolees going to board: 50/186 = 0.2688172 or 26.9% release rate

# Total black inmate parole success = 130
# Total black inmate going to board = 441
# Total release rate for black parolees going to board: 130/441 = 0.2947846 or 29.4% release rate

# Total hispanic inmate parole success = 32
# Total hispanic inmate going to board = 154
# Total release rate for hispanic parolees going to board: 32/154 = 0.2077922 or 20.8% release rate



#Let's go with Franklin
FranklinParole_table <- med.all2[med.all2$Facility == "FRANKLIN",]
View(FranklinParole_table)
class(FranklinParole_table)
FranklinParole_table$race <- as.factor(FranklinParole_table$race)
View(FranklinParole_table)
summary(FranklinParole_table$race)
#Now of the Franklin inmates, lets look at just the race of parole.
FranklinP1 <- FranklinParole_table[FranklinParole_table$interview_decision == "OPEN DATE",]
FranklinP2 <- FranklinParole_table[FranklinParole_table$interview_decision == "PAROLED",]
FranklinP <- rbind(FranklinP1, FranklinP2)
View(FranklinP)
summary(FranklinP$interview_decision)
summary(FranklinP$race)
FranklinNP <- FranklinParole_table[FranklinParole_table$interview_decision !="PAROLED" & FranklinParole_table$interview_decision !="OPEN DATE",]
summary(FranklinNP$interview_decision)
summary(FranklinNP$race)

#Now I can calculate what total of those paroled for each race are a percentage of the total going to the board for
#at Franklin:

# Total white inmate parole success = 43
# Total white inmate going to board = 216
# Total release rate for white parolees going to board: 43/216 = 0.1990741 or 19.9% release rate

# Total black inmate parole success = 38
# Total black inmate going to board = 243
# Total release rate for black parolees going to board: 38/243 = 0.1563786 or 15.6% release rate

# Total hispanic inmate parole success = 15
# Total hispanic inmate going to board = 91
# Total release rate for hispanic parolees going to board: 15/91 = 0.1648352 or 16.4% release rate

#This is getting crazy. I have to create a new column that is if someone is paroled or not. 
med.all2$Parole_Status <- ifelse(med.all2$interview_decision !="PAROLED" & med.all2$interview_decision !="OPEN DATE", "NP", "P")
View(med.all2)
med.all2$Parole_Status <- as.factor(med.all2$Parole_Status)

###################################################################################

#Fisher Test:
fisher.test(table(subset(med.all2,Crime1=="MURDER-2")[,c("Parole_Status","max_sent_yr")]),simulate.p.value=TRUE)

#Fisher's Exact Test for Count Data with simulated p-value (based on 2000 replicates)
#data:  
#p-value = 0.0004998
#alternative hypothesis: two.sided

parole_facility = aggregate(order(Parole_Status~Facility,data=med.all2,table))
View(parole_facility)
aggregate(Parole_Status~Facility,data=med.all2,table)
##########Facility Parole_Status.NP Parole_Status.P
#1       ADIRONDACK              136              53
#2           ALBION               13               2
#3    ALBION-FEMALE              258             172
#4        ALBION-WR                5              16
#5           ALTONA              132              38
#6           ATTICA              310              29  --------> 0.08554572
#7           AUBURN              226              21  --------> 0.08502024
#8        BARE HILL              362             109
#9          BAYVIEW                9              27
#10   BEACON-FEMALE                5              45
#11   BEDFORD HILLS              125              64
#12   BUTLER-ASACTC               41              14
#13    CAPE VINCENT              239              55
#14          CAYUGA              290              80
#15         CLINTON              386              63
#16         COLLINS              327             102
#17       COXSACKIE              136              44
#18       DOWNSTATE              117              42
#19         EASTERN               33              12
#20       EDGECOMBE                4               7
#21          ELMIRA              210              35 ---------> 0.1428571
#22        FISHKILL              602             222
#23     FIVE POINTS              192              27
#24        FRANKLIN              475              99
#25      GOUVERNEUR              249              59
#26         GOWANDA              575             183
#27     GOWANDA SOP              154              13 ---------->  0.07784431
#28    GREAT MEADOW              314              30 ---------->  0.0872093
#29     GREEN HAVEN              204              28 ---------->  0.1206897
#30          GREENE              402             128
#31       GROVELAND              446             134
#32 HALE CREEK-ASAC               28               9
#33          HUDSON              189              76
#34        LAKEVIEW               40               6
#35 LAKEVIEW-FEMALE                4               1
#36  LAKEVIEW-SHOCK               30               3
#37         LINCOLN               27              47
#38      LIVINGSTON              251              61
#39           MARCY              392              91
#40    MARCY-ASACTC                0               1
#41        MIDSTATE              480             135
#42          MOHAWK              395             116
#43        MONTEREY               12               0
#44          MORIAH                6               0
#45    MT. MCGREGOR               72              40
#46      OGDENSBURG               99              58
#47    ORL TRANSPRG                2               0
#48         ORLEANS              500             203
#49    OTHER AGENCY               11               0
#50       OTISVILLE              360             130
#51      QUEENSBORO              453             227
#52       RIVERVIEW              205              55
#53       ROCHESTER               25              40
#54      SHAWANGUNK               97              10
#55       SING SING              215              23
#56       SOUTHPORT              193              14
#57        SULLIVAN               85              14
#58  TACONIC-ASACTC                2               1
#59  TACONIC-FEMALE              124             109
#60          ULSTER              201              84
#61         UNKNOWN                6               0
#62         UPSTATE              218              10 -----------> 0.04385965
#63        WALLKILL              255              68
#64  WALSH MED CNTR               42              22
#65      WASHINGTON              219              63
#66       WATERTOWN              150              54
#67           WENDE              301              95
#68      WILLARD CF                2               0
#69     WILLARD-DTC                0               1
#70      WOODBOURNE              309             114
#71         WYOMING              458             139
############################################################################
#Ok trying to find the difference between initial and reappearance:

Initial_table <- med.all2[med.all2$interview_type == "INITIAL",]
View(Initial_table)
class(Initial_table)
Initial_table$race <- as.factor(Initial_table$race)
summary(Initial_table$race)
summary(Initial_table$age)
summary(Initial_table$Parole_Status)
1627/(7759+1627)
#.17334 percent
Initial_table2p1 <- Initial_table[Initial_table$interview_decision == "OPEN DATE",]
Initial_table2p2 <- Initial_table[Initial_table$interview_decision == "PAROLED",]
Initial_table2p <- rbind(Initial_table2p1, Initial_table2p2)
View(Initial_table2p)
summary(Initial_table2p$interview_decision)
summary(Initial_table2p$race)
Initial_table2np <- Initial_table[Initial_table$interview_decision !="PAROLED" & Initial_table$interview_decision !="OPEN DATE",]
summary(Initial_table2np$interview_decision)
summary(Initial_table2np$race)
############################################################################
#Finding the most populous prisons.
library(plyr)

#Now I will look at the prisons with the most people going to parole (five min and five max):
#Maxes: Attica, Auburn, Clinton, Great Meadow, Wende
#Mins: Fishkill, Gowanda, Orleans, Wyoming, Midstate

#First up, Attica (which has 339 people going to the parole board).
#First a create a table just for Attica:
Attica_table <- med.all2[med.all2$Facility == "ATTICA",]
summary(Attica_table$Parole_Status)
#NP   P 
#310  29
#Parole rate overall --> .08554572 parole success rate

#Now let's break down the race at Attica
Attica_race = table(Attica_table$race)
barplot(Attica_race)

#Now let's break down the crime at Attica
Attica_crime = table(Attica_table$Crime1)
barplot(Attica_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(Attica_crime[Attica_crime>15]),
        col = cp,
        las=2,
        ylim = c(0, 70),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Attica (ac)
ac = table(Attica_table$Crime1,Attica_table$race)
ac
#Look at sums of race and crime.
apply(ac,2,sum)
apply(ac,1,sum)

#Look at the dimensions of the mosiac plot
dim(ac)

#Only look at crimes with over 200 people:
ac = ac[apply(ac,1,sum)>5,]

#Make a mosiac plot:
mosaicplot(ac)

#Turn the text vertical
mosaicplot(b,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(ac,las=2,col=cc)

#Change the color to something more contrasting:
cc = brewer.pal(7,"Blues")
mosaicplot(ac,las=2,col=cc)
b

#Combine the race groups that are less than 10%. 
b2 = ac
apply(ac[,-c(3,4,7)],2,sum)
b2 = cbind(ac[,3:4],ac[,7],apply(ac[,-c(3,4,7)],1,sum))
cc = brewer.pal(4,"Blues")

#Amazing mosaic plot for attica:
mosaicplot(b2,las=2,col=cc)

#Attica_people who got out on parole list:
Attica_p <- Attica_table[Attica_table$Parole_Status == "P",]
View(Attica_p)

#Look at median age**********:
summary(Attica_p)
#Age minimum is 28 years old.
#Age median is 47 years old.
#Age maximum is 70 years old.

#The most common crime facing the board is Burglary (freq of 5). Only three people of the 29 are in for murder.
#8 people have life sentences.

#Auburn (which has 247 people going to the parole board).
#First a create a table just for Auburn:
Auburn_table <- med.all2[med.all2$Facility == "AUBURN",]
View(Auburn_table)
summary(Auburn_table$Parole_Status)
#NP   P 
#226  21
#Parole rate overall --> 0.08502024 parole success rate (this is almost EXACTLY the same as Attica)

#Now let's break down the race at Auburn
Auburn_race = table(Auburn_table$race)
barplot(Auburn_race)

#Now let's break down the crime at Attica
Auburn_crime = table(Auburn_table$Crime1)
barplot(Auburn_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(Auburn_crime[Auburn_crime>6]),
        col = cp,
        las=2,
        ylim = c(0, 70),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Auburn (abc)
abc = table(Auburn_table$Crime1,Auburn_table$race)
abc
#Look at sums of race and crime.
apply(abc,2,sum)
apply(abc,1,sum)

#Look at the dimensions of the mosiac plot
dim(abc)

#Only look at crimes with over 200 people:
abc = abc[apply(abc,1,sum)>5,]

#Make a mosiac plot:
mosaicplot(abc)

#Turn the text vertical
mosaicplot(abc,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(abc,las=2,col=cc)

#Change the color to something more contrasting:
cc = brewer.pal(7,"Set1")
mosaicplot(abc,las=2,col=cc)
abc

#Combine the race groups that are less than 10%. 
ab2 = abc
apply(abc[,-c(3,4,7)],2,sum)
ab2 = cbind(abc[,3:4],abc[,7],apply(abc[,-c(3,4,7)],1,sum))

#Amazing mosaic plot for Auburn:
mosaicplot(ab2,las=2,col=cc)

#Auburn people who got out on parole list:
Auburn_p <- Auburn_table[Auburn_table$Parole_Status == "P",]
View(Auburn_p)

#Look at median age**********:
summary(Auburn_p)
#Age minimum is 25 years old.
#Age median is 45 years old.
#Age maximum is 66 years old.

#The most common crime facing the board is Murder-2 (freq of 7). And 3 people are in for burglary in the 3 degree.
#8 people have life as a max sentence

#
#
#

#Next up Clinton (which has 449 people going to the parole board).
#First a create a table just for Clinton:
Clinton_table <- med.all2[med.all2$Facility == "CLINTON",]
View(Clinton_table)
summary(Clinton_table$Parole_Status)
#NP   P 
#386  63
#Parole rate overall --> 0.1403118 parole success rate (this is almost DOUBLE Attica and Auburn)

#Now let's break down the race at Clinton
Clinton_race = table(Clinton_table$race)
barplot(Clinton_race)

#Now let's break down the crime at Clinton
Clinton_crime = table(Clinton_table$Crime1)
barplot(Clinton_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(Clinton_crime[Clinton_crime>15]),
        col = cp,
        las=2,
        ylim = c(0, 70),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Clinton (ac)
clc = table(Clinton_table$Crime1,Clinton_table$race)
clc
#Look at sums of race and crime.
apply(clc,2,sum)
apply(clc,1,sum)

#Look at the dimensions of the mosiac plot
dim(clc)

#Only look at crimes with over 200 people:
clc = clc[apply(clc,1,sum)>5,]

#Make a mosiac plot:
mosaicplot(clc)

#Turn the text vertical
mosaicplot(clc,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(clc,las=2,col=cc)

#Change the color to something more contrasting:
cc = brewer.pal(7,"Set1")
mosaicplot(clc,las=2,col=cc)
abc

#Combine the race groups that are less than 10%. 
clc2 = clc
apply(clc[,-c(3,4,7)],2,sum)
clc2 = cbind(clc[,3:4],clc[,7],apply(clc[,-c(3,4,7)],1,sum))

#Amazing mosaic plot for Clinton:
mosaicplot(clc2,las=2,col=cc)

#Clinton people who got out on parole list:
Clinton_p <- Clinton_table[Clinton_table$Parole_Status == "P",]
View(Clinton_p)

#Look at median age**********:
summary(Clinton_p)
#Age minimum is 21 years old.
#Age median is 42 years old.
#Age maximum is 77 years old.

#The most common crime facing the board is Murder-2 (freq of 17). And 4 people are in for burglary in the 3 degree.
#29 people have life as a max sentence

#
#
#

#Next up Great Meadow (which has 344 people going to the parole board).
#First a create a table just for great meadow:
GM_table <- med.all2[med.all2$Facility == "GREAT MEADOW",]
View(GM_table)
summary(GM_table$Parole_Status)
#NP   P 
#314  30
#Parole rate overall --> 0.0872093 parole success rate (this is the same of Attica and Auburn)

#Now let's break down the race at Great Meadow
GM_race = table(GM_table$race)
barplot(GM_race)

#Now let's break down the crime at Great Meadow
GM_crime = table(GM_table$Crime1)
barplot(GM_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(GM_crime[GM_crime>10]),
        col = cp,
        las=2,
        ylim = c(0, 70),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Great Meadow (gmc)
gmc = table(GM_table$Crime1,GM_table$race)
gmc
#Look at sums of race and crime.
apply(gmc,2,sum)
apply(gmc,1,sum)

#Look at the dimensions of the mosiac plot
dim(gmc)

#Only look at crimes with over 200 people:
gmc = gmc[apply(gmc,1,sum)>5,]

#Make a mosiac plot:
mosaicplot(gmc)

#Turn the text vertical
mosaicplot(gmc,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(gmc,las=2,col=cc)

#Combine the race groups that are less than 10%. 
gmc2 = gmc
apply(gmc[,-c(3,4,7)],2,sum)
gmc2 = cbind(gmc[,3:4],gmc[,7],apply(gmc[,-c(3,4,7)],1,sum))

#Amazing mosaic plot for Great Meadow:
mosaicplot(gmc2,las=2,col=cc)

#Great Meadow people who got out on parole list:
GM_p <- GM_table[GM_table$Parole_Status == "P",]
View(GM_p)

#Look at stats**********:
summary(GM_p)
#Age minimum is 27 years old.
#Age median is 47.5 years old.
#Age maximum is 71 years old.

#The most common crime facing the board is Murder-2 (freq of 9). And 4 people are in for CSCS-3 >79.
#17 people have life as a max sentence
#
#
#

#Next up Wende (which has 396 people going to the parole board).
#First a create a table just for Wende:
Wende_table <- med.all2[med.all2$Facility == "WENDE",]
View(Wende_table)
summary(Wende_table$Parole_Status)
#NP   P 
#301  95
#Parole rate overall --> 0.239899 parole success rate (this is higher)

#Now let's break down the race at Wende
Wende_race = table(Wende_table$race)
barplot(Wende_race)

#Now let's break down the crime at Wende
Wende_crime = table(Wende_table$Crime1)
barplot(Wende_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(Wende_crime[Wende_crime>10]),
        col = cp,
        las=2,
        ylim = c(0, 70),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Wende
wc = table(Wende_table$Crime1,Wende_table$race)
wc
#Look at sums of race and crime.
apply(wc,2,sum)
apply(wc,1,sum)

#Look at the dimensions of the mosiac plot
dim(wc)

#Only look at crimes with over 200 people:
wc = wc[apply(wc,1,sum)>5,]

#Make a mosiac plot:
mosaicplot(wc)

#Turn the text vertical
mosaicplot(wc,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(wc,las=2,col=cc)

#Combine the race groups that are less than 10%. 
wc2 = wc
apply(wc[,-c(3,4,7)],2,sum)
wc2 = cbind(wc[,3:4],wc[,7],apply(wc[,-c(3,4,7)],1,sum))

#Amazing mosaic plot for Wende:
mosaicplot(wc2,las=2,col=cc)

#Wende who got out on parole list:
Wende_p <- Wende_table[Wende_table$Parole_Status == "P",]
View(Wende_p)

#Look at stats**********:
summary(Wende_p)
#Age minimum is 19 years old.
#Age median is 42 years old.
#Age maximum is 83 years old.

#The most common crime facing the board is Murder-2 (freq of 23). And 6 people are in for Burglary in the third degree.
#34 people have life as a max sentence

#
#
#
#
#Now on the med security
#
#
#
#
#Next up Fishkill (which has 824 people going to the parole board).
#First a create a table just for Wende:
Fishkill_table <- med.all2[med.all2$Facility == "FISHKILL",]
View(Fishkill_table)
summary(Fishkill_table$Parole_Status)
#NP   P 
#602  222
#Parole rate overall --> 0.2694175 parole success rate (this is higher)

#Now let's break down the race at Fishkill
Fishkill_race = table(Fishkill_table$race)
barplot(Fishkill_race)

#Now let's break down the crime at Fishkill
Fishkill_crime = table(Fishkill_table$Crime1)
barplot(Fishkill_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(Fishkill_crime[Fishkill_crime>15]),
        col = cp,
        las=2,
        ylim = c(0, 70),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Fishkill (fkc)
fkc = table(Fishkill_table$Crime1,Fishkill_table$race)
fkc
#Look at sums of race and crime.
apply(fkc,2,sum)
apply(fkc,1,sum)

#Look at the dimensions of the mosiac plot
dim(fkc)

#Only look at crimes with over 15 people:
fkc = fkc[apply(fkc,1,sum)>15,]

#Make a mosiac plot:
mosaicplot(fkc)

#Turn the text vertical
mosaicplot(fkc,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(fkc,las=2,col=cc)

#Combine the race groups that are less than 10%. 
fkc2 = fkc
apply(fkc[,-c(3,4,7)],2,sum)
fkc2 = cbind(fkc2[,3:4],fkc2[,7],apply(fkc2[,-c(3,4,7)],1,sum))

#Amazing mosaic plot for attica:
mosaicplot(fkc2,las=2,col=cc)

#Attica_people who got out on parole list:
Fishkill_p <- Fishkill_table[Fishkill_table$Parole_Status == "P",]
View(Fishkill_p)

#Look at stats**********:
summary(Fishkill_p)
#Age minimum is 21 years old.
#Age median is 48 years old.
#Age maximum is 89 years old.

#The most common crime facing the board is Murder-2 (freq of 68). And 12 people are in for Burglary in the third degree.
#105 people have life as a max sentence.

#
#
#
#Next up Gowanda (which has 758 people going to the parole board).
#First a create a table just for Gowanda:
Gowanda_table <- med.all2[med.all2$Facility == "GOWANDA",]
View(Gowanda_table)
summary(Gowanda_table$Parole_Status)
#NP   P 
#575  183
#Parole rate overall --> 0.2414248 parole success rate (this is higher)

#Now let's break down the race at Gowanda
Gowanda_race = table(Gowanda_table$race)
barplot(Gowanda_race)

#Now let's break down the crime at Gowanda
Gowanda_crime = table(Gowanda_table$Crime1)
barplot(Gowanda_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(Gowanda_crime[Gowanda_crime>20]),
        col = cp,
        las=2,
        ylim = c(0, 250),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Gowanda (gwc)
gwc = table(Gowanda_table$Crime1,Gowanda_table$race)
gwc
#Look at sums of race and crime.
apply(gwc,2,sum)
apply(gwc,1,sum)

#Look at the dimensions of the mosiac plot
dim(gwc)

#Only look at crimes with over 15 people:
gwc = gwc[apply(gwc,1,sum)>15,]

#Make a mosiac plot:
mosaicplot(gwc)

#Turn the text vertical
mosaicplot(gwc,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(gwc,las=2,col=cc)

#Combine the race groups that are less than 10%. 
gwc2 = gwc
apply(gwc2[,-c(3,4,7)],2,sum)
gwc2 = cbind(gwc2[,3:4],gwc2[,7],apply(gwc2[,-c(3,4,7)],1,sum))

#Amazing mosaic plot for Gowanda:
mosaicplot(gwc2,las=2,col=cc)

#Gowanda_people who got out on parole list:
Gowanda_p <- Gowanda_table[Gowanda_table$Parole_Status == "P",]
View(Gowanda_p)

#Look at stats**********:
summary(Gowanda_p)
#Age minimum is 22 years old.
#Age median is 43 years old.
#Age maximum is 66 years old.

#The most common crime facing the board is DWI Second Offense (freq of 71). And 28 people are in for DWI 3RD/SUBSQNT.
#8 people have life as a max sentence. Most have 3 years as max sentence (105).
#
#
#
#Next up Orleans (which has 703 people going to the parole board).
#First a create a table just for Orleans:
Orleans_table <- med.all2[med.all2$Facility == "ORLEANS",]
View(Orleans_table)
summary(Orleans_table$Parole_Status)
#NP   P 
#500  203
#Parole rate overall --> 0.2887624 parole success rate (this is higher)

#Now let's break down the race at Orleans
Orleans_race = table(Orleans_table$race)
barplot(Orleans_race)

#Now let's break down the crime at Orleans
Orleans_crime = table(Orleans_table$Crime1)
barplot(Orleans_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(Orleans_crime[Orleans_crime>20]),
        col = cp,
        las=2,
        ylim = c(0, 70),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Orleans 
orlc = table(Orleans_table$Crime1,Orleans_table$race)
orlc
#Look at sums of race and crime.
apply(orlc,2,sum)
apply(orlc,1,sum)

#Look at the dimensions of the mosiac plot
dim(orlc)

#Only look at crimes with over 15 people:
orlc = orlc[apply(orlc,1,sum)>15,]

#Make a mosiac plot:
mosaicplot(orlc)

#Turn the text vertical
mosaicplot(orlc,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(orlc,las=2,col=cc)

#Combine the race groups that are less than 10%. 
orlc2 = orlc
apply(orlc[,-c(3,4,7)],2,sum)
orlc2 = cbind(orlc2[,3:4],orlc2[,7],apply(orlc2[,-c(3,4,7)],1,sum))

#Amazing mosaic plot for Orleans:
mosaicplot(orlc2,las=2,col=cc)

#Orleans people who got out on parole list:
Orleans_p <- Orleans_table[Orleans_table$Parole_Status == "P",]
View(Orleans_p)

#Look at stats**********:
summary(Orleans_p)
#Age minimum is 18 years old.
#Age median is 39 years old.
#Age maximum is 67 years old.

#The most common crime facing the board is Murder-2 (freq of 34). And 16 people are in for Burglary in the third degree.
#49 people have life as a max sentence. But the most common is 3 years with 52.
#
#
#
#Next up Wyoming (which has 597 people going to the parole board).
#First a create a table just for Wyoming:
Wyoming_table <- med.all2[med.all2$Facility == "WYOMING",]
View(Wyoming_table)
summary(Wyoming_table$Parole_Status)
#NP   P 
#458  139
#Parole rate overall --> 0.2328308 parole success rate.

#Now let's break down the race at Wyoming
Wyoming_race = table(Wyoming_table$race)
barplot(Wyoming_race)

#Now let's break down the crime at Wyoming
Wyoming_crime = table(Wyoming_table$Crime1)
barplot(Wyoming_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(Wyoming_crime[Wyoming_crime>20]),
        col = cp,
        las=2,
        ylim = c(0, 70),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Wyoming (wyc)
wyc = table(Wyoming_table$Crime1,Wyoming_table$race)
wyc
#Look at sums of race and crime.
apply(wyc,2,sum)
apply(wyc,1,sum)

#Look at the dimensions of the mosiac plot
dim(wyc)

#Only look at crimes with over 15 people:
wyc = wyc[apply(wyc,1,sum)>15,]

#Make a mosiac plot:
mosaicplot(wyc)

#Turn the text vertical
mosaicplot(wyc,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(wyc,las=2,col=cc)

#Combine the race groups that are less than 10%. 
wyc2 = wyc
apply(wyc[,-c(3,4,7)],2,sum)
wyc2 = cbind(wyc2[,3:4],wyc2[,7],apply(wyc2[,-c(3,4,7)],1,sum))

#Amazing mosaic plot for Wyoming:
mosaicplot(wyc2,las=2,col=cc)

#Wyoming people who got out on parole list:
Wyoming_p <- Wyoming_table[Wyoming_table$Parole_Status == "P",]
View(Wyoming_p)

#Look at stats**********:
summary(Wyoming_p)
#Age minimum is 19 years old.
#Age median is 38 years old.
#Age maximum is 72 years old.

#The most common crime facing the board is Murder-2 (freq of 13). And 11 people are in for Burglary in the third degree.
#20 people have life as a max sentence. But the most common is 3 years with 41.
#
#
#
#Next up Midstate (which has 615 people going to the parole board).
#First a create a table just for Midstate:
Midstate_table <- med.all2[med.all2$Facility == "MIDSTATE",]
View(Midstate_table)
summary(Midstate_table$Parole_Status)
#NP   P 
#480  135
#Parole rate overall --> 0.2195122 parole success rate.

#Now let's break down the race at Midstate
Midstate_race = table(Midstate_table$race)
barplot(Midstate_race)

#Now let's break down the crime at Midstate
Midstate_crime = table(Midstate_table$Crime1)
barplot(Midstate_crime)
cp = brewer.pal(9,"Blues")
barplot(sort(Midstate_crime[Midstate_crime>20]),
        col = cp,
        las=2,
        ylim = c(0, 70),
        cex.axis=0.8,
        cex.names=0.5)
#        horiz=TRUE)

#Now amazing mosaic plot by race and crime for Midstate (msc)
msc = table(Midstate_table$Crime1,Midstate_table$race)
msc
#Look at sums of race and crime.
apply(msc,2,sum)
apply(msc,1,sum)

#Look at the dimensions of the mosiac plot
dim(msc)

#Only look at crimes with over 15 people:
msc = msc[apply(msc,1,sum)>15,]

#Make a mosiac plot:
mosaicplot(msc)

#Turn the text vertical
mosaicplot(msc,las=2)

#Load colorbrewer package:
library(RColorBrewer)
require("RColorBrewer")

#Use color "Blues":
cc = brewer.pal(7,"Blues")

#Make a mosaic plot:
mosaicplot(msc,las=2,col=cc)

#Combine the race groups that are less than 10%. 
msc2 = msc
apply(msc[,-c(3,4,7)],2,sum)
msc2 = cbind(msc2[,3:4],msc2[,7],apply(msc2[,-c(3,4,7)],1,sum))

#Amazing mosaic plot for Midstate:
mosaicplot(msc2,las=2,col=cc)

#Midstate people who got out on parole list:
Midstate_p <- Midstate_table[Midstate_table$Parole_Status == "P",]
View(Midstate_p)

#Look at stats**********:
summary(Midstate_p)
#Age minimum is 21 years old.
#Age median is 43 years old.
#Age maximum is 75 years old.

#The most common crime facing the board is Burglary-3 (freq of 23). And 10 people are in for Grand Larceny in the fourth degree.
#13 people have life as a max sentence. But the most common is 3 years with 44.
#
#
#
#Now I should create a new column in medall2 for years since people were eligible for parole. 
#First, convert the parole eligibility date to R format:
View(med.all2)
med.all2$newParoleEligibleDate <- as.Date(med.all2$prl_elig_date, "%m/%d/%Y")
#Now try to get years between parole eligible date and interview date.
parole_years <- function(med.all2$newParoleEligibleDate, med.all2$newintvdate) 
{ 
  lt <- data.frame(med.all2$newParoleEligibleDate, med.all2$newintvdate) 
  newParoleYears <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y")) 
  first <- as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")) 
  newParoleYears[which(med.all2$newParoleEligibleDate > lt[,2])] <- newParoleYears[which(med.all2$newParoleEligibleDate > lt[,2])] - 1 
  newParoleYears 
}
View(newParoleYears)

#add the years on:
med.all2$newParoleYears <- newParoleYears
View(med.all2)
#yay!

#Now look at the median:
newParole_table <- med.all2[med.all2$newParoleYears>20,]
summary(newParole_table)
View(newParole_table)
##################################

# Age visualization:
hist(med.all3$age, breaks=50, col="lightblue")

#Organizing the data by parole facility.
x = aggregate(Parole_Status~Facility,data=med.all2,table)
View(x)
View(parole_facility)
parole_facility = aggregate(order(Parole_Status~Facility,data=med.all2,table))
summary(med.all2$Facility)

#reading in the prison types csv to merge to my data set.
prison_types <- read.csv("~/Data II/Parole/Parole/prison_types.csv")
View(prison_types)
prison_types
names(med.all2)
prison_types$Prison_Facility %in% med.all2$Facility
all(prison_types$Prison_Facility %in% med.all2$Facility)
all(med.all2$Facility %in% prison_types$Prison_Facility)
(med.all2$Facility %in% prison_types$Prison_Facility)
med.all2$Facility[!(med.all2$Facility %in% prison_types$Prison_Facility)]
unique(med.all2$Facility[!(med.all2$Facility %in% prison_types$Prison_Facility)])
class(prison_types$Prison_Facility)
levels(prison_types$Prison_Facility)
prison_types_s = prison_types
levels(prison_types$Prison_Facility)[23]
levels(prison_types$Prison_Facility)[23] = "FIVE POINTS"
levels(prison_types$Prison_Facility)
unique(med.all2$Facility[!(med.all2$Facility %in% prison_types$Prison_Facility)])
View(prison_types)
View(prison_types)
?merge

#Here comes the merge:
tmp = merge(med.all2,prison_types,by.x="Facility",by.y="Prison_Facility")
dim(tmp)
dim(med.all2)
table(med.all2$Facility)
table(tmp$Prison_Type,tmp$Facility)
med.all3 = tmp
rm(tmp)
names(med.all3)
med.all3$race
table(med.all3$race)
View(med.all3)
table(med.all3$Crime1)
sort(table(med.all3$Crime1))
sort(table(med.all3$Crime1[grepl("MURDER",med.all3$Crime)]))
sort(table(as.character(med.all3$Crime1)[grepl("MURDER",med.all3$Crime)]))
sort(table(as.character(med.all3$Crime1)[grepl("MURDER",med.all3$Crime1)]))
(table(as.character(med.all3$Crime1)))
table(med.all3$Parole_Status,med.all3$Prison_Type)
mosaicplot(table(med.all3$Parole_Status,med.all3$Prison_Type))
mosaicplot(table(med.all3$Prison_Type,med.all3$Parole_Status))
mosaicplot(table(med.all3$Prison_Type,med.all3$Parole_Status,med.all3$Crime1))
sort(table(med.all3$Crime1))
table(med.all3$Crime1,med.all3$Parole_Status)
table(med.all3$Crime1,med.all3$Parole_Status,med.all3$Prison_Type)

#Trying to do the tree again (and its not working):
library(rpart)
fit = rpart(Parole_Status~Crime1+race+Prison_Type,data=med.all3)
plot(fit)
text(fit,use.n=T)
par(xpd=T)
text(fit,use.n=T)
fit
table(med.all3$Crime1,med.all3$Prison_Type)
b = table(med.all3$Crime1,med.all3$Prison_Type)
b[order(apply(b,1,sum)),]
b[order(apply(b,1,sum)),c(1,5,4,3,2)]
b = table(med.all3$Crime1,med.all3$Parole_Status,med.all3$Prison_Type)
b[,,1]
b = b[,,c(1,5,4,3,2)]
b[,,1]
b[,,5]
apply(b,3,function(x) x[,2]/(x[,1]+x[,2]))
apply(b,3,function(x) ifelse(x[,1]+x[,2] == 0, NA,x[,2]/(x[,1]+x[,2]))
)
apply(b,3,function(x) x[,1]+x[,2])
cbind(apply(b,3,function(x) x[,1]+x[,2]),
      apply(b,3,function(x) ifelse(x[,1]+x[,2] == 0, NA,x[,2]/(x[,1]+x[,2])))
)
tt = strptime(as.character(med.all3$prl_elig_date),"%m/%d/%Y")
tt
tt = strptime("12/15/2013","%m/%d/%Y"),strptime(as.character(med.all3$prl_elig_date),"%m/%d/%Y")
tt = strptime("12/15/2013","%m/%d/%Y")-strptime(as.character(med.all3$prl_elig_date),"%m/%d/%Y")
tt
range(tt)
range(tt,na.rm=T)
med.all3$prl_elig_date[tt<0]
tt = strptime("12/15/2013","%m/%d/%Y")-strptime(as.character(med.all3$prl_elig_date),"%m/%d/%Y")
med.all3$Crime1[tt<0]
head(med.all3$Crime1[order(tt)])
head(med.all3$prl_elig_date[order(tt)])
med.all3$Class1
table(med.all3$Class10
)
table(med.all3$Class1)
table(med.all3$Class1,med.all3$Parole_Status,med.all3$Prison_Type)

#Mosaic plot with the prison type, parole and crime type.
mosaicplot(table(med.all3$Class1,med.all3$Parole_Status,med.all3$Prison_Type))
mosaicplot(table(med.all3$Prison_Type,med.all3$Class1,med.all3$Parole_Status))
table(med.all3$Prison_Type,med.all3$Class1,med.all3$Parole_Status)
mosaicplot(table(med.all3$Prison_Type,med.all3$Class1,med.all3$Parole_Status)[,c(1,5,4,3,2),])
mosaicplot(table(med.all3$Prison_Type,med.all3$Class1,med.all3$Parole_Status)[,,c(1,5,4,3,2)])
mosaicplot(table(med.all3$Prison_Type,med.all3$Class1,med.all3$Parole_Status)[c(1,5,4,3,2),,])
mosaicplot(table(med.all3$Prison_Type,med.all3$Class1,med.all3$Parole_Status)[c(5,3,2),,])
mosaicplot(table(med.all3$Prison_Type,med.all3$Class1,med.all3$Parole_Status)[c(5,3,2),,],col=c("blue","red"))
table(med.all3$Class1,med.all3$Crime1)
table(med.all3$Crime1[med.all3$Class1=="B"])
table(as.character(med.all3$Crime1[med.all3$Class1=="B"]))
sort(table(as.character(med.all3$Crime1[med.all3$Class1=="B"])))
sort(table(as.character(med.all3$Crime1[med.all3$Class1=="C"])))
fit = glm(Parole_Status~race+age+Prison_Type+Class1,data=med.all3)
table(med.all3$Parole_Status)
table(med.all3$Parole_Status)

table(med.all3$Parole_Status,useNA="always")
fit = glm(as.numeric(med.all3$Parole_Status)-1~race+age+Prison_Type+Class1,data=med.all3)
fit
#Call:  glm(formula = as.numeric(med.all3$Parole_Status) - 1 ~ race + 
#             age + Prison_Type + Class1, data = med.all3)

#Coefficients:
#  (Intercept)   raceASIAN/PACIFIC           raceBLACK        raceHISPANIC           raceOTHER  
#0.2825903           0.2167126           0.0222662           0.0339787           0.0483382  
#raceUNKNOWN           raceWHITE                 age      Prison_TypeMAX      Prison_TypeMED  
#0.0700440           0.0885928           0.0005176          -0.1820211          -0.0823597  
#Prison_TypeMEDICAL      Prison_TypeMIN             Class1B             Class1C             Class1D  
#0.0003097           0.0842677          -0.0509749          -0.0416579          -0.0062728  
#Class1E  
#-0.0176578  

#Degrees of Freedom: 17196 Total (i.e. Null);  17181 Residual
#(311 observations deleted due to missingness)
#Null Deviance:      3108 
#Residual Deviance: 3022 	AIC: 18930
#summary(fit)

#Looking at some more fits:
fit = glm(as.numeric(med.all3$Parole_Status)-1~race+age+Prison_Type+Class1,data=med.all3,family="binomial")
summary(fit)
head(as.numeric(med.all3$Parole_Status))
head((med.all3$Parole_Status))
levels(med.all3$race)
ll = levels(med.all3$race)
ll
ll = ll[c(4,1,2,3,5:7)]
fit = glm(as.numeric(med.all3$Parole_Status)-1~age+C(race,base="HISPANIC")+Prison_Type+Class1,data=med.all3,family="binomial")
fit = glm(as.numeric(med.all3$Parole_Status)-1~age+C(race,base=4)+Prison_Type+Class1,data=med.all3,family="binomial")
summary(fit)
fit = glm(as.numeric(med.all3$Parole_Status)-1~age+C(race,base=4)+C(Prison_Type,base=3)+Class1,data=med.all3,family="binomial")
summary(fit)
levels(med.all3$Prison_Type)
aov(fit)
summary(aov(fit))
as.character(med.all3$newintvdate)

#Now trying to do some some strings and mosaic plots:
subsr(as.character(med.all3$newintvdate),9,10)
substr(as.character(med.all3$newintvdate),9,10)
table(substr(as.character(med.all3$newintvdate),9,10),med.all3$Parole_Status)
mosaicplot(table(substr(as.character(med.all3$newintvdate),9,10),med.all3$Parole_Status))
mosaicplot(table(substr(as.character(med.all3$newintvdate),7,8),med.all3$Parole_Status))
mosaicplot(table(substr(as.character(med.all3$newintvdate),6,7),med.all3$Parole_Status))
table(med.all3$month)
med.all3$Crime4
med.all3$Crime4 == ""
med.all3$Crime4 != ""
med.all3$Crime4 != "" +
  med.all3$Crime3 != "" +
  (med.all3$Crime4 != "") +
  (med.all3$Crime3 != "") +
  (med.all3$Crime2 != "") +
  0

#Now looking at total crimes:
totcrime = med.all3$Crime4 != "" +
  totcrime = med.all3$Crime4 != ""
totcrime = (med.all3$Crime4 != "") +
  (med.all3$Crime3 != "") +
  (med.all3$Crime2 != "")
table(totcrime+1)
totcrime = totcrime + 1
barplot(totcrime)

#This is who has 1, 2, 3, and 4 crimes (based on above data):
barplot(table(totcrime))
fit = glm(as.numeric(med.all3$Parole_Status)-1~age+C(race,base=4)+C(Prison_Type,base=3)+Class1+totcrime,data=med.all3,family="binomial")
summary(fit)
mosaicplot(table(totcrime,med.all3$race))
mosaicplot(table(med.all3$race,totcrime))
mosaicplot(table(med.all3$race,sample(totcrime,length(totcrime)))
)
mosaicplot(table(med.all3$race,sample(totcrime,length(totcrime))))
mosaicplot(table(med.all3$race,sample(totcrime,length(totcrime))))
mosaicplot(table(med.all3$Parole_Status,totcrime)))
mosaicplot(table(med.all3$Parole_Status,totcrime))

#Mosaic plot showing the people paroled and how many crimes they have:
mosaicplot(table(totcrime,med.all3$Parole_Status))

table(med.all3$max_sentence)
table(med.all3$max_sent_yr)
hist((med.all3$max_sent_yr))
hist((med.all3$max_sent_yr[med.al3$max_sent_yr < 100]))
hist((med.all3$max_sent_yr[med.all3$max_sent_yr < 100]))

#Looking at frequency of sentence:
hist((med.all3$max_sent_yr[med.all3$max_sent_yr < 100]),breaks=100)
table(med.all3$Crime1[med.all3$max_sent_yr==25])
table(as.character(med.all3$Crime1[med.all3$max_sent_yr==25]))
sort(table(as.character(med.all3$Crime1[med.all3$max_sent_yr==25])))
table(as.character(med.all3$Crime1[med.all3$max_sent_yr==25]),med.all3$Parole_Status[med.all3$max_sent_yr==25])

#Data in March.
write.csv(med.all3, "march_parole_data.csv")


mosaicplot(table(med.all3$Prison_Type,med.all3$Class1,med.all3$Parole_Status))

#Publishable mosaic plot of crime/class/prison type:
mosaicplot(table(med.all3$Prison_Type,med.all3$Class1,med.all3$Parole_Status)[c(5,3,2),,],col=c("lightblue","darkcyan"), main = "Release Rate based on Crime Class and Prison Type")