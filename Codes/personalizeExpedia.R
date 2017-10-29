library(data.table)
library(Amelia)
library(ggplot2)
library(scales)
library(ca)
library(polycor)
library(ltm)
library(mice)
library(VIM)
library(lattice)
library(corrplot)
library(psych)
library(stats)
library(psy)


##Get Sample
df <- fread('train.csv',nrows = 20000,header = TRUE,na.strings = c('NULL'))
write.csv(df,file='20000trainsample.csv',row.names=FALSE)


PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = RC1, y = RC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}



####(1)Load Data
full=read.csv("20000trainsample.csv",header=T,stringsAsFactors=TRUE)




####(2)Explore the Data
###(2.1)Basic Statistics
#1353 people have conducted searches in the train set
#547 people out of the 703 people eventually booked hotels
#Out of 10000 hotels in the recommendation results, 888 hotels were clicked
#The Click Through Rate in the train set is 4.44%.
#The Conversion Rate in the train set is 61.60%.
#The Satisifaction Rate in the train set is 40.4%
srchppl=full$srch_id[20000]
cat(paste('There are',srchppl,'people who conducted hotel searches in the train set'))
bkhotels=sum(full$booking_bool)
cat(paste('There are',bkhotels,'people who eventually booked hotels in the train set'))
ckhotels=sum(full$click_bool)
cat(paste('Out of 20000 hotels in the recommendation results,',ckhotels,'hotels were clicked'))
ctrate=percent(round((ckhotels/20000),4))
cat(paste('The Click Through Rate in the train set is',ctrate))
cvrate=percent(round((bkhotels/ckhotels),4))
cat(paste('The Conversion Rate in the train set is',cvrate))
satrate=percent(round((bkhotels/srchppl),4))
cat(paste('The Satisifaction Rate in the train set is',satrate))
bkrate=percent(sum(full$booking_bool)/nrow(full))
bkrate

###(2.2)Data Preview
#How many features do we have in our dataset:
#We have 54 varibles
str(full)

###(2.3)How many features have missing values(NA)
Find_Missing_Features <- function(data, lower,upper){
  M <- sapply(data, function(x) sum(is.na(x))); M[M>nrow(data)*lower & M<=nrow(data)*upper ]
}

#More than 80% of the cases is missing:
#visitor_hist_starrating,visitor_hist_adr_usd,srch_query_affinity_score
#compn_rate,compn_inv,compn_rate_percent_diff
#gross_bookings_usd
Find_Missing_Features(full,0.5,1)

#More than 30% but lower than 50% of the cases is missing:
#orig_destination_distance
#comp5_inv
Find_Missing_Features(full,0.3,0.5)

#More than 10% but lower than 30% of the cases is missing:
#orig_destination_distance
Find_Missing_Features(full,0.1,0.3)

#Less than 10% of the cases is missing:
#prop_review_score
Find_Missing_Features(full,0,0.1)

####(3)Data Wrangling
###(3.1)Search_Id: Not Useful in Click Prediction




###(3.2)date_time variable:
#Maybe we could Create useful features from this variable :
#First, let's create Month and TimeoftheDay(TofD) variables:
head(full$date_time[1:10])
full$Month=month.abb[as.numeric(substr(full$date_time,6,7))]
head(full$Month)
full$TofD=as.numeric(substr(full$date_time,12,13))
head(full$TofD)
class(full$TofD)



#Reclassify TofD into a cateogrical data with 4 levels
TofDlvs<- c('Morning','Afternoon','Night','Midnight')
full$TofD <- ifelse(full$TofD >=6 & full$TofD <12, TofDlvs[1], 
                    ifelse(full$TofD >=12 & full$TofD <18, TofDlvs[2], 
                           ifelse(full$TofD >=18 & full$TofD <24, TofDlvs[3], 
                                  TofDlvs[4])))

##Are the new variabels related the clicking result?
#Frequency Table:
FrePer_wClicking_Table <- function(f1,f2){
  ft<-table(f1,f2)
  colnames(ft)<-c('No_Clicking','Clicking')
  pt<-prop.table(ft,1)
  Clicking_Rate<-round(pt[,2],3)
  total <- cbind(ft,Clicking_Rate)
  total
}





##Month vs Clicking:
#First, use chi-Squared Test:
#X-squared = 4.038, df = 7, p-value = 0.7754
#Month and clicking result are two vary independent variables
chisq.test(as.data.frame.matrix(table(full$Month,full$click_bool)))

#Then, let's use Mosaic Plot
#No relationship
mosaicplot(table(full$Month,full$click_bool))

#Last, create frequency and percentage table
FrePer_wClicking_Table(full$Month,full$click_bool)[,3]


##TimeoftheDay vs Clicking:
#First, use chi-Squared Test:
#X-squared = 1.2774, df = 3, p-value = 0.7345
#TofD and clicking result are two vary independent variables
chisq.test(as.data.frame.matrix(table(full$TofD,full$click_bool)))


#Then, let's use Mosaic Plot
#No relationship
mosaicplot(table(full$TofD,full$click_bool))

#Last, create frequency and percentage table
#to double check
FrePer_wClicking_Table(full$TofD,full$click_bool)



#####Month vs booking_window
#Create TraMon(Travel Month Variable)
full$TraMon=as.Date(substr(full$date_time,1,10),'%Y-%m-%d')+full$srch_booking_window
full$TraMon=month.abb[as.numeric(substr(full$TraMon,6,7))]
head(full$TraMon)



##TraMon vs Clicking:
#First, use chi-Squared Test:
#X-squared = 9.1855, df = 11, p-value = 0.6048
#TraMon and clicking result are two vary independent variables
chisq.test(as.data.frame.matrix(table(full$TraMon,full$click_bool)))


#Then, let's use Mosaic Plot
#It seems no relationship
mosaicplot(table(full$TraMon,full$click_bool))

#Last, create frequency and percentage table
FrePer_wClicking_Table(full$TraMon,full$click_bool)







###(3.3)site_id variable:
#Too many levels
#Not so sinificant relationship
nlevels(factor(full$site_id))
mosaicplot(table(full$site_id,full$click_bool))
FrePer_wClicking_Table(full$site_id,full$click_bool)
full$site_id<-as.factor(full$site_id)
chisq.test(as.data.frame.matrix(table(full$site_id,full$click_bool),simulate.p.value = TRUE))
#drop this variable

###(3.4)visitor_location_country_id 
#Too many levels
#Not so sinificant relationship
nlevels(factor(full$visitor_location_country_id))
mosaicplot(table(full$visitor_location_country_id,full$click_bool))
FrePer_wClicking_Table(full$visitor_location_country_id,full$click_bool)
full$site_id<-as.factor(full$visitor_location_country_id)
chisq.test(as.data.frame.matrix(table(full$visitor_location_country_id,full$click_bool),simulate.p.value = TRUE))


#drop this variable


###(3.5)prop_country_id
#Too many levels
#Not so sinificant relationship
nlevels(factor(full$prop_country_id))
mosaicplot(table(full$prop_country_id,full$click_bool))
FrePer_wClicking_Table(full$prop_country_id,full$click_bool)
full$prop_country_id<-as.factor(full$prop_country_id)
chisq.test(as.data.frame.matrix(table(full$prop_country_id,full$click_bool),simulate.p.value = TRUE))

#drop this variable

##(3.6)prop_id
###Prop_id vs click
##Too many levels
full$prop_id<-as.factor(full$prop_id)
nlevels(full$prop_id)

#Not A single Hotel is the Most Popular
t<-table(full$prop_id[full$click_bool==1])
names(t[which.max(t)])
sum(full$click_bool[full$prop_id==21018])

###Improvement
#How to sort cross table by a value of a column, such as booking_rate

 

###(3.7)visitor_hist_starrating & visitor_hist_adr_usd
#How many missing values:
#19022 missing values,95% missing
sum(is.na(full$visitor_hist_starrating))
sum(is.na(full$visitor_hist_adr_usd))


##visitor_hist_starrating vs click_bool:
range(full$visitor_hist_starrating,na.rm=T)
vhscmedian <- round(aggregate(visitor_hist_starrating ~  click_bool, full, median),2)
ggplot(full,aes(x=factor(click_bool),y=visitor_hist_starrating,fill=factor(click_bool)))+geom_boxplot(na.rm = T)+
   geom_text(data = vhscmedian, aes(label = visitor_hist_starrating, y = visitor_hist_starrating+0.1))+ggtitle('visitor_hist_starrating vs click_bool')


##visitor_hist_adr_usd vs click_bool:
range(full$visitor_hist_adr_usd,na.rm=T)
vhavmedian <- round(aggregate(visitor_hist_adr_usd ~  click_bool, full, median),2)
ggplot(full,aes(x=factor(click_bool),y=visitor_hist_adr_usd,fill=factor(click_bool)))+geom_boxplot(na.rm = T)+
  geom_text(data = vhavmedian, aes(label = visitor_hist_adr_usd, y = visitor_hist_adr_usd+8))

ggplot(full, aes(x=visitor_hist_adr_usd)) + geom_density()  


##visitor_hist_adr_usd and visitor_hist_starrating
#Could we predict the missing values? Maybe no
full$visitor_hist_starrating[is.na(full$visitor_hist_starrating)]=8
full$visitor_hist_adr_usd[is.na(full$visitor_hist_adr_usd)]=600
ggplot(full,aes(x=visitor_hist_adr_usd,y=visitor_hist_starrating))+geom_point(color='red',size=3)



###srch_length_of_stay and visitor_hist_starrating
ggplot(full,aes(x=srch_length_of_stay,y=visitor_hist_starrating))+geom_point(color='red',size=3)+
  scale_x_continuous(breaks=seq(min(full$srch_length_of_stay), max(full$srch_length_of_stay), by = 1))+
  scale_y_continuous(breaks=seq(0, max(full$visitor_hist_starrating), by = 1))
                                                                                                                                                              
FrePer_wClicking_Table(full$srch_length_of_stay[full$srch_length_of_stay>=8],full$click_bool[full$srch_length_of_stay>=8])
FrePer_wClicking_Table(full$srch_length_of_stay[full$srch_length_of_stay<8],full$click_bool[full$srch_length_of_stay<8])


##Create RG(Return Guest) Variable:
full <- transform(full, RG= ifelse(is.na(full$visitor_hist_starrating),
  paste0('NRG',srch_length_of_stay), paste0('RG',srch_length_of_stay)))


#Chi-Squared Test:
#X-squared = 6.8602, df = NA, p-value = 0.98
chisq.test(as.data.frame.matrix(table(full$RG,full$click_bool)),simulate.p.value=T)

#Correspondence Analysis:
RGB=as.data.frame.matrix(table(full$RG,full$click_bool))
CRGB=ca(RGB, graph = FALSE)
CRGB$rowcoord
CRGB$colcoord

#RG vs Clicking
FrePer_wClicking_Table(full$RG,full$click_bool)


###Create New Features from visitor_hist_starrating and prop_starrating:
##Does 1 rating differ from 2+ ratings?
#Not really
ggplot(full[!is.na(full$visitor_hist_starrating),],aes(visitor_hist_starrating,fill=factor(booking_bool)))+geom_histogram()+
  theme_bw()


###Does user historical rating has relationship with prop historical rating 
#Yes
sum(is.na(full$visitor_hist_starrating))
full$visitor_hist_starrating[full$visitor_hist_starrating==8]=NA

ggplot(full[!is.na(full$visitor_hist_starrating),],aes(visitor_hist_starrating,fill=factor(click_bool)))+
  geom_histogram()+facet_grid(~prop_starrating)

aggregate(click_bool ~  prop_starrating, full, sum)
range(full$prop_starrating[full$click_bool==1 & full$visitor_hist_starrating<3],na.rm = T)
FrePer_wClicking_Table(full$prop_starrating[full$visitor_hist_starrating<3],
                       full$click_bool[full$visitor_hist_starrating<3])

range(full$prop_starrating[full$click_bool==1 & full$visitor_hist_starrating<3.5 &full$visitor_hist_starrating>=3],na.rm = T)
FrePer_wClicking_Table(full$prop_starrating[full$visitor_hist_starrating<3.5&
                                              full$visitor_hist_starrating>=3],
                       full$click_bool[full$visitor_hist_starrating<3.5 &
                                         full$visitor_hist_starrating>=3])

range(full$prop_starrating[full$click_bool==1 & full$visitor_hist_starrating<4 &full$visitor_hist_starrating>=3.5],na.rm = T)
FrePer_wClicking_Table(full$prop_starrating[full$visitor_hist_starrating<4 &
                                              full$visitor_hist_starrating>=3.5],
                       full$click_bool[full$visitor_hist_starrating<4 &
                                         full$visitor_hist_starrating>=3.5])

range(full$prop_starrating[full$click_bool==1 & full$visitor_hist_starrating<=5 &full$visitor_hist_starrating>=4],na.rm = T)
FrePer_wClicking_Table(full$prop_starrating[full$visitor_hist_starrating<=5 &
                                              full$visitor_hist_starrating>=4],
                       full$click_bool[full$visitor_hist_starrating<=5 &
                                         full$visitor_hist_starrating>=4])

range(full$prop_starrating[full$click_bool==1 & is.na(full$visitor_hist_starrating)])
FrePer_wClicking_Table(full$prop_starrating[is.na(full$visitor_hist_starrating)],
                       full$click_bool[is.na(full$visitor_hist_starrating)])




##Create New Variable SRMD(Star Rating Match Distance);
full$SRMD<-NA
full$SRMD[!is.na(full$visitor_hist_starrating)]<-full$visitor_hist_starrating-full$prop_starrating

SRMDmedian <- round(aggregate(SRMD ~  click_bool, full, median),2)
ggplot(full,aes(x=factor(click_bool),y=SRMD,fill=factor(click_bool)))+geom_boxplot(na.rm = T)+
  geom_text(data = SRMDmedian, aes(label = SRMD, y = SRMD+0.05))+ggtitle('Before MICE SRMD(Star Rating Matching Distance) vs click_bool')



###Use MICE to fill in missing values:
##Find Related Variables
#### Asign Conditon Variables
full$Condition=NA
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 1 &full$srch_children_count == 0]='1a'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 1 &full$srch_children_count >0]='1amorec'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 2 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 2 &full$srch_children_count > 0]='2amorec'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 3 &full$srch_children_count == 0]='3a'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 3 &full$srch_children_count > 0]='3amorec'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 4 &full$srch_children_count == 0]='4a'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 4 &full$srch_children_count > 0]='4amorec'
full$Condition[full$srch_room_count==1 & full$srch_adults_count >=5]='5a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 1]='1a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 2 &full$srch_children_count == 0]='1a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 2 &full$srch_children_count > 0]='1amorec'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 3 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 3 &full$srch_children_count >0]='2amorec'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 4 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 4 &full$srch_children_count >0]='2amorec'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 5 &full$srch_children_count == 0]='3a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 5 &full$srch_children_count >0]='3amorec'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 6 &full$srch_children_count == 0]='3a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 6 &full$srch_children_count >0]='3amorec'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 3 &full$srch_children_count == 0]='1a'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 3 &full$srch_children_count >0]='1amorec'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 6 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 6 &full$srch_children_count > 0]='2amorec'
full$Condition[full$srch_room_count==4 & full$srch_adults_count == 4 &full$srch_children_count == 0]='1a'
full$Condition[full$srch_room_count==4 & full$srch_adults_count == 4 &full$srch_children_count > 0]='1amorec'
full$Condition[full$srch_room_count==4 & full$srch_adults_count == 8 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==4 & full$srch_adults_count == 8 &full$srch_children_count > 0]='2amorec'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 7 &full$srch_children_count == 0]='3a'


###Create New Variable
full$AtoR=as.numeric(full$srch_adults_count/full$srch_room_count)
full$AtoR[full$AtoR==0.5]=1

aggregate(visitor_hist_starrating~srch_adults_count,full,mean)
aggregate(visitor_hist_starrating~srch_children_count,full,mean)
aggregate(visitor_hist_starrating~srch_length_of_stay,full,mean)
aggregate(visitor_hist_starrating~srch_room_count,full,mean)
aggregate(visitor_hist_starrating~Condition,full,mean)
aggregate(visitor_hist_starrating~AtoR,full,mean)

#Create a new dataset
##
fillvhr<-full[,c( 'visitor_hist_starrating','srch_adults_count','srch_children_count','srch_length_of_stay','srch_room_count','Condition','AtoR')]
str(fillvhr)
fillvhr$Condition<-as.factor(fillvhr$Condition)
md.pattern(fillvhr) 
filledvhr <- mice(fillvhr,m=5,maxit=7,meth='pmm',seed=500)
summary(filledvhr)

#Compare with original result
fill1 <- complete(filledvhr,1)
#Create new feature L2s(Location_score2_scale)
fill1$prop_starrating<-full$prop_starrating
fill1$click_bool<-full$click_bool
fill1$SRMD<-fill1$visitor_hist_starrating-fill1$prop_starrating

SRMDmedian2 <- round(aggregate(SRMD ~  click_bool, fill1, median),2)
ggplot(fill1,aes(x=factor(click_bool),y=SRMD,fill=factor(click_bool)))+geom_boxplot(na.rm = T)+
  geom_text(data = SRMDmedian2, aes(label = SRMD, y = SRMD+0.15))+ggtitle('After MICE SRMD(Star Rating Matching Distance) vs click_bool')

##Assign the imputed values to original data
full$visitor_hist_starrating<-fill1$visitor_hist_starrating
full$SRMD<-fill1$SRMD


##Does visitor_hist_adr_usd lowew than 100 and higher than 100 matter?
#Not really
full$visitor_hist_adr_usd[full$visitor_hist_adr_usd==600]=NA
ggplot(full[!is.na(full$visitor_hist_adr_usd),],aes(visitor_hist_adr_usd,fill=factor(booking_bool)))+geom_histogram()+
  theme_bw()

#Improvement
#Create new varialbes from Hist_ADR



###(3.8)prop_starring&prop_review_score
###Prop_Starrating vs clicking:
#Point-Biserial Correlation
#Too weak
biserial.cor(full$prop_starrating, full$click_bool, level = 2)

FrePer_wClicking_Table(full$prop_starrating, full$click_bool)

##Does Prop_Starrating has relationship with Booking result?
#Seems like only 2,3,4,5 and new hotels are booked 
range(full$prop_starrating)
ggplot(full, aes(x = prop_starrating, fill = factor(booking_bool))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=seq(0,5)) 



#Create New Feature RTD(Rating Distance)
full$MRT <- ave(full$prop_starrating, full$srch_id)
full$RTD <- full$prop_starrating-full$MRT

##Boxplot
RTDmedian <- round(aggregate(RTD ~  click_bool, full, median),2)
ggplot(full,aes(x=factor(click_bool),y=RTD,fill=factor(click_bool)))+geom_boxplot()+
  geom_text(data = RTDmedian, aes(label = RTD, y = RTD+0.15))+ggtitle('RTD(Rating Distance) vs click_bool')



###prop_review_score vs click:
##Fill missing values:
sum(is.na(full$prop_review_score))
full$prop_starrating[is.na(full$prop_review_score)]
table(full$prop_starrating[is.na(full$prop_review_score)])
aggregate(prop_review_score~prop_starrating,full,median)
full$prop_review_score[is.na(full$prop_review_score) & full$prop_starrating==0]=3.5
full$prop_review_score[is.na(full$prop_review_score) & full$prop_starrating==2]=3.5
full$prop_review_score[is.na(full$prop_review_score) & full$prop_starrating==3]=4
full$prop_review_score[is.na(full$prop_review_score) & full$prop_starrating==4]=4

#Point-Biserial Correlation
#Too weak
biserial.cor(full$prop_review_score, full$click_bool, level = 2)

FrePer_wClicking_Table(full$prop_review_score, full$click_bool)

##Does prop_review_score has relationship with Booking result?
#yes..
range(full$prop_review_score)
ggplot(full, aes(x = prop_review_score, fill = factor(booking_bool))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=seq(0,5,0.5)) 
 


##Create RSD(Review Score Distance)
full$MRS <- ave(full$prop_review_score, full$srch_id)
full$RSD <- full$prop_review_score-full$MRS

##Boxplot
RSDmedian <- round(aggregate(RSD ~  click_bool, full, median),2)
ggplot(full,aes(x=factor(click_bool),y=RSD,fill=factor(click_bool)))+geom_boxplot()+
  geom_text(data = RSDmedian, aes(label = RSD, y = RSD+0.15))+ggtitle('RSD(Review Score Distance) vs click_bool')




####(3.9)prop_brand_bool
#prop_brand_bool vs click
chisq.test(as.data.frame.matrix(table(full$prop_brand_bool,full$click_bool)))
FrePer_wClicking_Table(full$prop_brand_bool,full$click_bool)

#Drop this feature


####(3.10)prop_location_score1 & prop_location_score2
###prop_location_score1  vs booking_bool
###A glance at the missing values:
sum(is.na(full$prop_location_score1))
sum(is.na(full$prop_location_score2))

range(full$prop_location_score2,na.rm=T)
full$prop_location_score2[is.na(full$prop_location_score2)]=2

###prop_location_score1 vs booking_bool
#Point-Biserial Correlation
#Too weak
biserial.cor(full$prop_location_score1, full$click_bool, level = 2)

##Does prop_location_score1 has relationship with click result?
#yes..
ggplot(full,aes(prop_location_score1,fill=factor(booking_bool)))+geom_histogram()+
  theme_bw()


###prop_location_score2  vs click_bool
#Point-Biserial Correlation
#Too weak
biserial.cor(full$prop_location_score2, full$click_bool, level = 2)


##Does prop_location_score2 has relationship with click result?
#yes..
ggplot(full,aes(prop_location_score2,fill=factor(click_bool)))+geom_histogram()+
  theme_bw()

hist(full$prop_location_score2[full$click_bool==1])
ggplot(full, aes(x=prop_location_score2)) + geom_density()  



###What does the value in score tell us?
#Location_score1
#The higher the location_score1, the higher the price
#Therefore, the higher the location_score1, the better the location
ggplot(full,aes(x=prop_location_score1,y=prop_log_historical_price))+geom_boxplot(outlier.colour="red")+facet_grid(~prop_starrating)+scale_y_continuous(breaks=seq(0,7))
range(full$prop_location_score1)


#Location_score1 and Location_score2 are positively correlated
#The higher the location_score2, the higher the price
#Therefore, the higher the location_score2, the better the location
range(full$prop_location_score2)
hist(full$prop_location_score2,at=seq(0,2, by=0.1),freq=F)
ggplot(full,aes(x=prop_location_score2,y=prop_log_historical_price))+geom_boxplot(outlier.colour="red")+facet_grid(~prop_starrating)+scale_y_continuous(breaks=seq(0,7))+
              ggtitle('Before MICE Location Score 2 vs log_historical Price Group By Prop_starrating')



###Fill Out Missing Values of prop_location_score2
###Find related variables besides prop_location_score1
#What is determined by the location?
##prop_starrating,prop_review_score,prop_log_historical_price
#Rating:
ggplot(full,aes(x=prop_location_score2,y=prop_log_historical_price))+geom_boxplot(outlier.colour="red")+facet_grid(~prop_starrating)+scale_y_continuous(breaks=seq(0,7))+
  ggtitle("Location_score2 vs prop_log_historical_price Groupby Prop_starrating") 
ggplot(full, aes(x=prop_starrating)) + geom_density()  


#prop_review_score:
range(full$prop_review_score)
ggplot(full, aes(x=prop_review_score)) + geom_density()  
ggplot(full,aes(x=prop_location_score2,y=prop_review_score))+geom_boxplot(outlier.colour="red")+facet_grid(~prop_starrating)+scale_y_continuous(breaks=seq(0,5))
ggplot(full,aes(x=prop_location_score2,y=prop_log_historical_price))+geom_boxplot(outlier.colour="red")+facet_grid(~prop_review_score)+scale_y_continuous(breaks=seq(0,7))+
  ggtitle("Location_score2 vs prop_log_historical_price Groupby prop_review_score") 
 

#prop_log_historical_price:
range(full$prop_log_historical_price)
ggplot(full,aes(x=prop_location_score2,y=prop_log_historical_price))+geom_boxplot(outlier.colour="red")+facet_grid(~prop_starrating)+scale_y_continuous(breaks=seq(0,6.5))+
  ggtitle("Location_score2 vs prop_log_historical_price Groupby prop_starrating") 


###Use MICE to fill in missing values:
#Create a new dataset
full$prop_location_score2[full$prop_location_score2==2]=NA
fillloc2<-full[,c('prop_location_score1','prop_location_score2','prop_starrating','prop_review_score','prop_log_historical_price')]
str(fillloc2)
md.pattern(fillloc2) 
filledloc2 <- mice(fillloc2,m=5,maxit=7,meth='pmm',seed=500)
summary(filledloc2)

#Compare with original result
##Looks Good
fill2 <- complete(filledloc2,1)
ggplot(fill2,aes(x=prop_location_score2,y=prop_log_historical_price))+geom_boxplot(outlier.colour="red")+facet_grid(~prop_starrating)+scale_y_continuous(breaks=seq(0,7))+
  ggtitle('After MICE Location Score 2 vs log_historical Price Group By Prop_starrating')

#Replace the computed values with new values:
full$prop_location_score2<-fill2$prop_location_score2


##Create L1D(Location Score 1 Distance)
full$ML1 <- ave(full$prop_location_score1, full$srch_id)
full$L1D <- full$prop_location_score1-full$ML1

##Boxplot
L1Dmedian <- round(aggregate(L1D ~  click_bool, full, median),2)
ggplot(full,aes(x=factor(click_bool),y=L1D,fill=factor(click_bool)))+geom_boxplot()+
  geom_text(data = L1Dmedian, aes(label = L1D, y = L1D+0.15))+ggtitle('L1D(Location Score 1 Distance) vs click_bool')


##Create L2D(Location Score 1 Distance)
full$ML2 <- ave(full$prop_location_score2, full$srch_id)
full$L2D <- full$prop_location_score2-full$ML2

##Boxplot
L2Dmedian <- round(aggregate(L2D ~  click_bool, full, median),2)
ggplot(full,aes(x=factor(click_bool),y=L2D,fill=factor(click_bool)))+geom_boxplot()+
  geom_text(data = L2Dmedian, aes(label = L2D, y = L2D+0.025))+ggtitle('L2D(Location Score 2 Distance) vs click_bool')



###(3.11)prop_log_historical_price and price_usd
###prop_log_historical_price vs clicking
ggplot(full,aes(full$prop_log_historical_price,fill=factor(full$click_bool)))+geom_histogram()+
  theme_bw()



###price_usd vs clicking
##Create PD(Price Distance)
full$MP <- ave(full$price_usd, full$srch_id)
full$PD <- full$price_usd-full$MP
PDmedian <- round(aggregate(PD ~  click_bool, full, median),2)
PDmedian


###(3.12)position and promotion_flag
###position vs clicking
table(full$position,full$click_bool)
ggplot(full,aes(full$position,fill=factor(full$click_bool)))+geom_histogram()+
  theme_bw()


####promotion_flag
table(full$promotion_flag,full$click_bool)
FrePer_wClicking_Table(full$promotion_flag,full$click_bool)


###(3.13)srch_length_of_stay &srch_booking_window  
FrePer_wClicking_Table(full$srch_length_of_stay,full$click_bool)

ggplot(full,aes(x=srch_length_of_stay,fill=factor(click_bool)))+
  geom_bar(stat='count')+scale_x_continuous(breaks=c(1:15))+theme_bw()

ggplot(full,aes(x=srch_booking_window,fill=factor(click_bool)))+
  geom_bar(stat='count')+scale_x_continuous(breaks=c(1:15))+theme_bw()

###(3.14)srch_adults_count ,srch_children_count,srch_room_count
###srch_adults_count vs click
ggplot(full,aes(x=srch_adults_count,fill=factor(click_bool)))+
  geom_bar(stat='count')+scale_x_continuous(breaks=c(1:15))+theme_bw()
FrePer_wClicking_Table(full$srch_adults_count,full$click_bool)


###srch_children_count vs click
ggplot(full,aes(x=srch_children_count,fill=factor(click_bool)))+
  geom_bar(stat='count')+scale_x_continuous(breaks=c(1:15))+theme_bw()
FrePer_wClicking_Table(full$srch_children_count,full$click_bool)


# Create Adults and Children crosstable
adlcld=table(full$srch_adults_count,full$srch_children_count)
rownames(adlcld)=c('1 Adult','2 Adults','3 Adults','4 Adults','5 Adults','6 Adults','7 Adults','8 Adults')
colnames(adlcld)=c('0 Child','1 Child','2 Children','3 Children','4 Children','5 Children')
adlcld

#Create Adults and search room count crosstable:
table(full$srch_adults_count,full$srch_room_count)
full$srch_room_count[full$srch_adults_count==1 & full$srch_children_count>=2]

## Asign Conditon Variables
full$Condition=NA
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 1 &full$srch_children_count == 0]='1a'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 1 &full$srch_children_count >0]='1amorec'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 2 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 2 &full$srch_children_count > 0]='2amorec'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 3 &full$srch_children_count == 0]='3a'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 3 &full$srch_children_count > 0]='3amorec'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 4 &full$srch_children_count == 0]='4a'
full$Condition[full$srch_room_count==1 & full$srch_adults_count == 4 &full$srch_children_count > 0]='4amorec'
full$Condition[full$srch_room_count==1 & full$srch_adults_count >=5]='5a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 1]='1a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 2 &full$srch_children_count == 0]='1a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 2 &full$srch_children_count > 0]='1amorec'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 3 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 3 &full$srch_children_count >0]='2amorec'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 4 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 4 &full$srch_children_count >0]='2amorec'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 5 &full$srch_children_count == 0]='3a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 5 &full$srch_children_count >0]='3amorec'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 6 &full$srch_children_count == 0]='3a'
full$Condition[full$srch_room_count==2 & full$srch_adults_count == 6 &full$srch_children_count >0]='3amorec'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 3 &full$srch_children_count == 0]='1a'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 3 &full$srch_children_count >0]='1amorec'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 6 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 6 &full$srch_children_count > 0]='2amorec'
full$Condition[full$srch_room_count==4 & full$srch_adults_count == 4 &full$srch_children_count == 0]='1a'
full$Condition[full$srch_room_count==4 & full$srch_adults_count == 4 &full$srch_children_count > 0]='1amorec'
full$Condition[full$srch_room_count==4 & full$srch_adults_count == 8 &full$srch_children_count == 0]='2a'
full$Condition[full$srch_room_count==4 & full$srch_adults_count == 8 &full$srch_children_count > 0]='2amorec'
full$Condition[full$srch_room_count==3 & full$srch_adults_count == 7 &full$srch_children_count == 0]='3a'

# Create condition and booking crosstable
FrePer_wClicking_Table(full$Condition,full$click_bool)


##Assign AtoR Variable:
full$AtoR=as.numeric(full$srch_adults_count/full$srch_room_count)
full$AtoR[full$AtoR==0.5]=1

FrePer_wClicking_Table(full$AtoR,full$click_bool)


##Create Type Variable
table(full$AtoR,full$booking_bool)
full$Type[full$AtoR ==1 & full$srch_children_count ==0]='single'
full$Type[full$AtoR ==1 & full$srch_children_count >0]='sinpar'
full$Type[full$AtoR <=2 & full$AtoR >1 & full$srch_children_count ==0]='FrsaCpl'
full$Type[full$AtoR <=2 & full$AtoR >1 & full$srch_children_count >0 & full$srch_children_count <=2]='smallfam'
full$Type[full$AtoR <=2 & full$AtoR >1 & full$srch_children_count >2 ]='bigfam'
full$Type[full$AtoR <=4 & full$AtoR >2 & full$srch_children_count ==0]='frs'
full$Type[full$AtoR <=4 & full$AtoR >2 & full$srch_children_count >0]='bigfam'
full$Type[full$AtoR >5]='bigfam'


FrePer_wClicking_Table(full$Type,full$click_bool)



####(3.15)srch_saturday_night_bool & random_bool
#srch_saturday_night_bool
FrePer_wClicking_Table(full$srch_saturday_night_bool,full$click_bool)

#random_bool
FrePer_wClicking_Table(full$random_bool,full$click_bool)



####(3.16)srch_query_affinity_score & orig_destination_distance  
#srch_query_affinity_score
sum(is.na(full$srch_query_affinity_score))
range(full$srch_query_affinity_score,na.rm=T)
#orig_destination_distance
sum(is.na(full$orig_destination_distance))
full$orig_destination_distance[ full$srch_destination_id==23246]
 
###4.PCA for all numerica Variables
###Create Numeric Variables Dataframe
train_numeric<-full[,c(5,9:10,12:13,14:17,19:24,27,59,67,69,71)]
head(full[,c(11,52,54:58,60:61,63,65,72)])
train_categorical<-full[,c()]
str(train_numeric)
train_numeric$promotion_flag<-as.numeric(train_numeric$promotion_flag)
train_numeric$srch_saturday_night_bool<-as.numeric(train_numeric$srch_saturday_night_bool)
train_numeric$random_bool<-as.numeric(train_numeric$random_bool)
str(full)
####Find features that are highly correlated with other features or 
###almost not correlated with any feature
c=cor(train_numeric)
round(c,2)
MCorrTest=corr.test(train_numeric,adjust='none')
M=round(MCorrTest$p,2)
MTest=ifelse(M<0.1,T,F)
colSums(MTest)-1
19*0.75
###Highly correlated Features are:
###prop_starrating,prop_review_score,prop_location_score1,prop_location_score2,prop_log_historical_price
###srch_length_of_stay,srch_booking_window,SRMD
###Adjust train_numeric
train_numeric_adjusted<-train_numeric[,-c(2:6,10,17)]
str(train_numeric_adjusted)
str(train_numeric)

####Continue with PCA
###The Bartlett Test
##It is good for PCA dimension reduction
bartlett.test(train_numeric_adjusted)
###KMO
##It is not good for PCA factor analysis
KMO(train_numeric_adjusted)


###Create Corrplot to guess how many PCs will we get
##Probably 3
c=cor(train_numeric_adjusted)
corrplot(c,method='ellipse',order='AOE')

###Should we scale?
##Yes
head(train_numeric_adjusted)
trainPCA=prcomp(train_numeric_adjusted,scale=T)
summary(trainPCA)
print(trainPCA)
round(trainPCA$rotation,2)
plot(trainPCA)
abline(1,0,col="red")
PCA_Plot(trainPCA)

###Use 10 Pcs which are generated from 20 numeric variables
###factor scores
###Get Scores
trainPCA$x
plot(trainPCA$x[,1:2])

for (i in 1:10) {
  eval(parse(text = paste0('full$PCfs', i, ' <- trainPCA$x[,i]')))
}

head(full$PCfs1)


####Common Factor Analysis
train_CFA<-factanal(train_numeric_adjusted,5)
print(train_CFA$loadings)

head(train_numeric_adjusted[,c(5:7)])
###Summated scale Factor Reliability Evaluation
##failed

cronbach(train_numeric[,c(5:7)])



####Only keep useful variables
head(full[,c(11,52,54:58,60:61,63,65,72:82)])
head(full[,52:54])
full_prePCA = full[,c(5,9:17,19:24,27,52,54:61,63,65,67,69,71:72)]
full_afterPCA =  full[,c(9:14,19,52,54:61,63,65,72:82)]
str(full_prePCA)
str(full_afterPCA)







###Write CSV
write.csv(full_prePCA,file='C:\\Users\\Yimei Tang\\Desktop\\DePaul\\CSC 424 Advanced Data Analysis\\Homework\\Homework 3\\train_prePCA.csv')
write.csv(full_afterPCA,file='C:\\Users\\Yimei Tang\\Desktop\\DePaul\\CSC 424 Advanced Data Analysis\\Homework\\Homework 3\\train_afterPCA.csv')


set.seed(754)
full$Month<-as.factor(full$Month)
full$TofD<-as.factor(full$TofD)
full$TraMon<-as.factor(full$TraMon)
full$RG<-as.factor(full$RG)
full$promotion_flag<-as.factor(full$promotion_flag)
full$Condition<-as.factor(full$Condition)
full$srch_saturday_night_bool<-as.factor(full$srch_saturday_night_bool)
full$random_bool<-as.factor(full$random_bool)

##Current Useful List
rf_model <- randomForest(factor(click_bool) ~ Month+TofD+TraMon+
                           visitor_hist_starrating+RG+SRMD+
                         prop_starrating+RTD+
                         prop_review_score+RSD+
                           prop_location_score1+L1D+
                           prop_location_score2+L2D+
                           prop_log_historical_price+
                           position+
                           PD+price_usd+
                           promotion_flag+
                           srch_length_of_stay+
                           srch_booking_window+
                           srch_adults_count+
                           srch_children_count+
                           srch_room_count+
                           srch_saturday_night_bool+
                           random_bool
                           ,
                         data = full,ntree=1000,importance=T)



print(rf_model)
plot(rf_model, ylim=c(0,1))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + theme_bw()


######Continue
#####Fit more original data in
#####Study how to set the parameters(such as tree depth node in random forest)
#####Search google: how to build a good decision tree
####I learnt: do not discretize data unless your know why it should be discretized, such as child < age <18... otherwise you are losing a lot of data.
####More on crossvalid

 

 





 