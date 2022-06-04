rm(list=ls())

library(lubridate)

smoking1<-read.csv("C:/Users/cuisa/Desktop/PCCP/smoking/smoking.csv",header = T)
smoking2<-read.csv("C:/Users/cuisa/Desktop/PCCP/smoking/smoking1.csv",header = T)

colnames(smoking1)[1]<-"mrn"
colnames(smoking1)

smoking<-merge(smoking1,smoking2,by.x = "mrn", by.y = "mrn")

#### Calculate age today 9/17/2020###
smoking$dob.x<-as.character(smoking$dob.x)
date_of_birth<-ymd(smoking$dob.x)
today<-rep("2020/9/16",length(date_of_birth))
date_of_today<-ymd(today)
days<-difftime(date_of_today,date_of_birth, units="days") 
age<-floor(days/365)

quantile(age,c(0.25,0.5,0.75))
sd(age)

### Calculate Gender ###
length(smoking[,1])
length(which(smoking$gender=="Female"))
length(which(smoking$gender=="Male"))


### Calculate Race ###
levels(smoking$race)
length(which(smoking$race=="Asian"))
length(which(smoking$race=="Black or African American"))
length(which(smoking$race=="White"))

length(which(smoking$race==""))
length(which(smoking$race=="Declined"))
length(which(smoking$race=="Not Reported"))
length(which(smoking$race=="Unknown"))

sum(length(which(smoking$race=="")),
    length(which(smoking$race=="Declined")),
    length(which(smoking$race=="Not Reported")),
    length(which(smoking$race=="Unknown")))


### Calculate BMI ###
smoking$First.BMI.2019
quantile(smoking$First.BMI.2019[-139],c(0.25,0.5,0.75),na.rm = T)
sd(smoking$First.BMI.2019[-139],na.rm = T)

### Calculate medicine 1###
levels(smoking$smoking_cess_meds_1)
length(which(smoking$smoking_cess_meds_1=="Chantix 0.5 MG Oral Tablet" ))
length(which(smoking$smoking_cess_meds_1=="Chantix 1 MG Oral Tablet" ))
length(which(smoking$smoking_cess_meds_1=="Chantix Continuing Month Pak 1 MG Oral Tablet" ))
length(which(smoking$smoking_cess_meds_1=="Chantix Starting Month Pak 0.5 MG X 11 & 1 MG X 42 Oral Tablet" ))
length(which(smoking$smoking_cess_meds_1=="CVS Nicotine 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="CVS Nicotine 2 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="CVS Nicotine 7 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="CVS Nicotine Polacrilex 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="GoodSense Nicotine 4 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="HM Nicotine 14 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="HM Nicotine 7 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicorelief 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette 2 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette 4 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette Starter Kit 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette Starter Kit 4 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 14 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 21-14-7 MG/24HR Transdermal Kit" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 21 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 7 MG/24HR TDSY" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 7 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Mini 2 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Mini 4 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Polacrilex 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Polacrilex 4 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Polacrilex 4 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Step 1 21 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Step 2 14 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Step 3 7 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotrol 10 MG Inhalation Inhaler" ))



## Quit and Refer  ##

length(which(smoking$referred_to_courtney==1))
length(which(smoking$wk24_quit_smoking==1))
length(which(smoking$wk24_quit_smoking==2))
length(which(smoking$wk24_quit_smoking==1&smoking$referred_to_courtney==1))
length(which(smoking$wk24_quit_smoking==1&smoking$referred_to_courtney==2))
length(which(smoking$wk24_quit_smoking==2&smoking$referred_to_courtney==1))
length(which(smoking$wk24_quit_smoking==2&smoking$referred_to_courtney==2))


## export subject id ##

export = data.frame(subject=smoking$subject_id,mrn = smoking$mrn)
write.csv(export,file="C:/Users/cuisa/Desktop/PCCP/smoking/subject_mrn_and_id.csv",row.names = F)


rm(list=ls())

library(lubridate)

smoking1<-read.csv("C:/Users/cuisa/Desktop/PCCP/smoking/smoking.csv",header = T)
smoking2<-read.csv("C:/Users/cuisa/Desktop/PCCP/smoking/smoking1.csv",header = T)

colnames(smoking1)[1]<-"mrn"
colnames(smoking1)

smoking<-merge(smoking1,smoking2,by.x = "mrn", by.y = "mrn")



need_to_remove = c(which(smoking$subject_id==7),which(smoking$subject_id==10),
                   which(smoking$subject_id==12),which(smoking$subject_id==17),
                   which(smoking$subject_id==19),which(smoking$subject_id==25),which(smoking$subject_id==27),
                   which(smoking$subject_id==32),which(smoking$subject_id==33),which(smoking$subject_id==40),
                   which(smoking$subject_id==41),which(smoking$subject_id==47),which(smoking$subject_id==63),
                   which(smoking$subject_id==64),which(smoking$subject_id==74),which(smoking$subject_id==75),
                   which(smoking$subject_id==85),which(smoking$subject_id==102),which(smoking$subject_id==104),
                   which(smoking$subject_id==105),which(smoking$subject_id==109),which(smoking$subject_id==117),
                   which(smoking$subject_id==118),which(smoking$subject_id==123),which(smoking$subject_id==124),
                   which(smoking$subject_id==126),which(smoking$subject_id==138),which(smoking$subject_id==146),
                   which(smoking$subject_id==150),which(smoking$subject_id==155),which(smoking$subject_id==158),
                   which(smoking$subject_id==165),which(smoking$subject_id==175),which(smoking$subject_id==180),
                   which(smoking$subject_id==187),which(smoking$subject_id==188),which(smoking$subject_id==189),
                   which(smoking$subject_id==191),which(smoking$subject_id==193),which(smoking$subject_id==194),
                   which(smoking$subject_id==207),which(smoking$subject_id==212),which(smoking$subject_id==217),
                   which(smoking$subject_id==223),which(smoking$subject_id==224),which(smoking$subject_id==229),
                   which(smoking$subject_id==233),which(smoking$subject_id==240),which(smoking$subject_id==241),
                   which(smoking$subject_id==245),which(smoking$subject_id==249),which(smoking$subject_id==264),
                   which(smoking$subject_id==267),which(smoking$subject_id==268),which(smoking$subject_id==269))









need_to_check=c(which(smoking$mrn==2443),which(smoking$mrn==4851),which(smoking$mrn==9638),which(smoking$mrn==175753),
                which(smoking$mrn==282239),which(smoking$mrn==325634),which(smoking$mrn==454683),which(smoking$mrn==460799),
                which(smoking$mrn==474493),which(smoking$mrn==474633),which(smoking$mrn==553143),which(smoking$mrn==567623),
                which(smoking$mrn==595411),which(smoking$mrn==792450),which(smoking$mrn==1028826),which(smoking$mrn==1036139),
                which(smoking$mrn==20666),which(smoking$mrn==834825),which(smoking$mrn==871947))

need_to_check_frame=smoking[need_to_check,c(39,49,59,69,79,89,99,109,119,129,139,149)]











smoking = smoking[-need_to_remove,]

#### Calculate age today 9/17/2020###
smoking$dob.x<-as.character(smoking$dob.x)
date_of_birth<-ymd(smoking$dob.x)
today<-rep("2020/9/16",length(date_of_birth))
date_of_today<-ymd(today)
days<-difftime(date_of_today,date_of_birth, units="days") 
age<-as.integer(floor(days/365))
smoking$age=age

quantile(age,c(0.25,0.5,0.75))
sd(age)


### Calculate Gender ###
length(smoking[,1])
length(which(smoking$gender=="Female"))
length(which(smoking$gender=="Male"))


### Calculate Race ###
levels(smoking$race)
length(which(smoking$race=="Asian"))
length(which(smoking$race=="Black or African American"))
length(which(smoking$race=="White"))

length(which(smoking$race==""))
length(which(smoking$race=="Declined"))
length(which(smoking$race=="Not Reported"))
length(which(smoking$race=="Unknown"))

sum(length(which(smoking$race=="")),
    length(which(smoking$race=="Declined")),
    length(which(smoking$race=="Not Reported")),
    length(which(smoking$race=="Unknown")))


smoking$race = as.character(smoking$race)
other_race_label=c(which(smoking$race==""),which(smoking$race=="Declined"),which(smoking$race=="Not Reported"),which(smoking$race=="Unknown"))
smoking$race[other_race_label]="Others"


### Calculate BMI ###
smoking$First.BMI.2019
quantile(smoking$First.BMI.2019[-122],c(0.25,0.5,0.75),na.rm = T)
sd(smoking$First.BMI.2019[-122],na.rm = T)


### Calculate medicine 1###
levels(smoking$smoking_cess_meds_1)
length(which(smoking$smoking_cess_meds_1=="Chantix 0.5 MG Oral Tablet" ))
length(which(smoking$smoking_cess_meds_1=="Chantix 1 MG Oral Tablet" ))
length(which(smoking$smoking_cess_meds_1=="Chantix Continuing Month Pak 1 MG Oral Tablet" ))
length(which(smoking$smoking_cess_meds_1=="Chantix Starting Month Pak 0.5 MG X 11 & 1 MG X 42 Oral Tablet" ))
length(which(smoking$smoking_cess_meds_1=="CVS Nicotine 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="CVS Nicotine 2 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="CVS Nicotine 7 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="CVS Nicotine Polacrilex 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="GoodSense Nicotine 4 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="HM Nicotine 14 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="HM Nicotine 7 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicorelief 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette 2 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette 4 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette Starter Kit 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicorette Starter Kit 4 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 14 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 21-14-7 MG/24HR Transdermal Kit" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 21 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 7 MG/24HR TDSY" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine 7 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Mini 2 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Mini 4 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Polacrilex 2 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Polacrilex 4 MG Mouth/Throat Gum" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Polacrilex 4 MG Mouth/Throat Lozenge" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Step 1 21 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Step 2 14 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotine Step 3 7 MG/24HR Transdermal Patch 24 Hour" ))
length(which(smoking$smoking_cess_meds_1=="Nicotrol 10 MG Inhalation Inhaler" ))



## Quit and Refer  ##

length(which(smoking$referred_to_courtney==1))
length(which(smoking$referred_to_courtney==2))
length(which(smoking$wk24_quit_smoking==1))
length(which(smoking$wk24_quit_smoking==2))

length(which(smoking$wk24_reduced_smoking==1))
length(which(smoking$wk24_reduced_smoking==2))

length(which(smoking$wk24_quit_smoking==1&smoking$referred_to_courtney==1))
length(which(smoking$wk24_quit_smoking==1&smoking$referred_to_courtney==2))
length(which(smoking$wk24_quit_smoking==2&smoking$referred_to_courtney==1))
length(which(smoking$wk24_quit_smoking==2&smoking$referred_to_courtney==2))




write.csv(smoking$mrn[no_quit_label],file = "C:/Users/cuisa/Desktop/PCCP/smoking/no_quit_mrn.csv")


## demographic stat over quit or didn't quit ##
quit_label = c(which(smoking$wk24_quit_smoking==1))
no_quit_label = c(which(smoking$wk24_quit_smoking==2))


length(which(smoking[quit_label,]$gender=="Female"))
length(which(smoking[quit_label,]$gender=="Male"))
length(which(smoking[no_quit_label,]$gender=="Female"))
length(which(smoking[no_quit_label,]$gender=="Male"))
chisq.test(matrix(c(10,9,39,61),2,2))

length(which(smoking[quit_label,]$race=="Black or African American"))
length(which(smoking[quit_label,]$race=="White"))
length(which(smoking[quit_label,]$race=="Asian"))
length(which(smoking[quit_label,]$race=="Others"))
length(which(smoking[no_quit_label,]$race=="Black or African American"))
length(which(smoking[no_quit_label,]$race=="White"))
length(which(smoking[no_quit_label,]$race=="Asian"))
length(which(smoking[no_quit_label,]$race=="Others"))
chisq.test(matrix(c(17,1,1,78,12,10),3,2))


quantile(smoking$age[quit_label],c(0.25,0.5,0.75))
sd(smoking$age[quit_label])
quantile(smoking$age[no_quit_label],c(0.25,0.5,0.75))
sd(smoking$age[no_quit_label])
wilcox.test(smoking$age[quit_label],smoking$age[no_quit_label])


smoking$First.BMI.2019[quit_label]
smoking$First.BMI.2019[no_quit_label]
no_quit_label_bmi = no_quit_label[-57]
quantile(smoking$First.BMI.2019[quit_label],c(0.25,0.5,0.75),na.rm = T)
sd(smoking$First.BMI.2019[quit_label],na.rm = T)
quantile(smoking$First.BMI.2019[no_quit_label_bmi],c(0.25,0.5,0.75),na.rm = T)
sd(smoking$First.BMI.2019[no_quit_label_bmi],na.rm = T)
wilcox.test(smoking$First.BMI.2019[quit_label],smoking$First.BMI.2019[no_quit_label_bmi])


chisq.test(matrix(c(11,8,35,62),2,2))


check_med = data.frame( smoking$smoking_cess_meds_1,
                        smoking$smoking_cess_meds_2,
                        smoking$smoking_cess_meds_3,
                        smoking$smoking_cess_meds_4,
                        smoking$smoking_cess_meds_5,
                        smoking$smoking_cess_meds_6,
                        smoking$smoking_cess_meds_7,
                        smoking$smoking_cess_meds_8,
                        smoking$smoking_cess_meds_9,
                        smoking$smoking_cess_meds_10,
                        smoking$smoking_cess_meds_11,
                        smoking$smoking_cess_meds_12,
                        smoking$smoking_cess_meds_13,
                        smoking$smoking_cess_meds_14,
                        smoking$smoking_cess_meds_15,
                        smoking$smoking_cess_meds_16,
                        smoking$smoking_cess_meds_17,
                        smoking$smoking_cess_meds_18,
                        smoking$smoking_cess_meds_19,
                        smoking$smoking_cess_meds_20,
                        smoking$smoking_cess_meds_21,
                        smoking$smoking_cess_meds_22,
                        smoking$smoking_cess_meds_23,
                        smoking$smoking_cess_meds_24,
                        smoking$smoking_cess_meds_25,
                        smoking$smoking_cess_meds_26,
                        smoking$smoking_cess_meds_27,
                        smoking$smoking_cess_meds_28)

smoking$med_cat = rep(1,214)
smoking$med_cat[c(1,2,3,8,24,27,28,30,37,39,42,45,52,55,56,57,59,65,66,67,76,
                  77,79,81,86,92,109,110,115,126,141,147,155,158,163,164,165,166,
                  171,173,181,182,183,185,186,192,193,195,199,208,209,213)]=2

smoking$med_cat[c(4,9,15,18,19,21,23,25,32,36,41,44,46,58,70,75,80,89,96,98,99,104,105,108,
                  118,120,122,124,125,129,131,136,146,153,157,159,172,187,190,191,194,197,201,203,212)]=3

length(which(smoking[quit_label,]$med_cat==1))
length(which(smoking[quit_label,]$med_cat==2))
length(which(smoking[quit_label,]$med_cat==3))

length(which(smoking[no_quit_label,]$med_cat==1))
length(which(smoking[no_quit_label,]$med_cat==2))
length(which(smoking[no_quit_label,]$med_cat==3))

chisq.test(matrix(c(4,10,5,22,53,25),3,2))


smoking$SUD=rep(0,length(smoking$mrn))



smoking$SUD[c(which(smoking$mrn==1962),which(smoking$mrn==8445),which(smoking$mrn==17237),which(smoking$mrn==21022),
              which(smoking$mrn==27677),which(smoking$mrn==29082), which(smoking$mrn==34374),which(smoking$mrn==36792),
              which(smoking$mrn==36868), which(smoking$mrn==45920), which(smoking$mrn==46420), which(smoking$mrn==63321), 
              which(smoking$mrn==66822), which(smoking$mrn==66996), which(smoking$mrn==68819), which(smoking$mrn==71772), 
              which(smoking$mrn==84269), which(smoking$mrn==106309), which(smoking$mrn==126030), which(smoking$mrn==148610), 
              which(smoking$mrn==175884), which(smoking$mrn==195018), which(smoking$mrn==202537), which(smoking$mrn==203396), 
              which(smoking$mrn==220617), which(smoking$mrn==220954), which(smoking$mrn==262633), which(smoking$mrn==277234), 
              which(smoking$mrn==281448), which(smoking$mrn==282239), which(smoking$mrn==363149), which(smoking$mrn==365053), 
              which(smoking$mrn==369618), which(smoking$mrn==390741), which(smoking$mrn==395108), which(smoking$mrn==422586), 
              which(smoking$mrn==436955), which(smoking$mrn==473176), which(smoking$mrn==488897), which(smoking$mrn==498417), 
              which(smoking$mrn==563998), which(smoking$mrn==567623), which(smoking$mrn==579386), which(smoking$mrn==579740), 
              which(smoking$mrn==582640), which(smoking$mrn==615479), which(smoking$mrn==617984), which(smoking$mrn==703156), 
              which(smoking$mrn==771947),  which(smoking$mrn==857584),  which(smoking$mrn==871947),  which(smoking$mrn==1036139), 
              which(smoking$mrn==1043043),  which(smoking$mrn==1046465))]=1

length(which(smoking$SUD==1))
length(which(smoking[quit_label,]$SUD==0))
length(which(smoking[quit_label,]$SUD==1))
length(which(smoking[no_quit_label,]$SUD==0))
length(which(smoking[no_quit_label,]$SUD==1))
chisq.test( matrix(c(2,17,19,81),nrow = 2))
fisher.test(matrix(c(2,17,19,81),nrow = 2))

### Merge CD4 and viral load data ###

smoking3<-read.csv("C:/Users/cuisa/Desktop/PCCP/smoking/smoking2.csv",header = T)

colnames(smoking3)[1] ='mrn'

smoking<-merge(smoking,smoking3,by.x = "mrn", by.y = "mrn")
smoking$mrn[is.na(smoking$wk24_quit_smoking)]
smoking$mrn[which(smoking$wk24_quit_smoking==3)]

missing_mrn = data.frame(mrn=c(smoking$mrn[is.na(smoking$wk24_quit_smoking)],smoking$mrn[which(smoking$wk24_quit_smoking==3)]))

smoking$CD4_within_6m[which(smoking$CD4_within_6m=="")]=NA
smoking$CD4_within_6m[which(smoking$CD4_within_6m=="N/A")]=NA
smoking$CD4_within_6m=as.numeric(smoking$CD4_within_6m)
quantile(smoking$CD4_within_6m[quit_label],c(0.25,0.5,0.75),na.rm = T)
quantile(smoking$CD4_within_6m[no_quit_label],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(smoking$CD4_within_6m[quit_label],smoking$CD4_within_6m[no_quit_label])

refer_label = which(smoking$referred_to_courtney==1)
no_refer_label = which(smoking$referred_to_courtney==2)

quantile(smoking$CD4_within_6m[refer_label],c(0.25,0.5,0.75),na.rm = T)
quantile(smoking$CD4_within_6m[no_refer_label],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(smoking$CD4_within_6m[refer_label],smoking$CD4_within_6m[no_refer_label])

smoking$viral_load_with_6m[which(smoking$viral_load_with_6m=="")]=NA
smoking$viral_load_with_6m[which(smoking$viral_load_with_6m=="N/A")]=NA
length(which(smoking$viral_load_with_6m=="< 20"))
sum(1*is.na(smoking$viral_load_with_6m))

length(which(smoking[quit_label,]$viral_load_with_6m=="< 20"))
sum(1*is.na(smoking[quit_label]$viral_load_with_6m))

length(which(smoking[no_quit_label,]$viral_load_with_6m=="< 20"))
sum(1*is.na(smoking[no_quit_label]$viral_load_with_6m))
chisq.test( matrix(c(15,4,64,36),nrow = 2))



length(which(smoking[refer_label,]$viral_load_with_6m=="< 20"))
sum(1*is.na(smoking[refer_label]$viral_load_with_6m))

length(which(smoking[no_refer_label,]$viral_load_with_6m=="< 20"))
sum(1*is.na(smoking[no_refer_label]$viral_load_with_6m))
chisq.test( matrix(c(51,24,82,53),nrow = 2))




#### Reduced 

smoking$wk24_reduced_smoking[c(12,18,37,40,43,57,80,82,91,113,130,204,154,159,174,187)]=1

reduce_label = c(which(smoking$wk24_reduced_smoking==1))
no_reduce_label = c(which(smoking$wk24_reduced_smoking==2))


length(reduce_label)
length(no_reduce_label)




length(which(smoking$wk24_reduced_smoking==1&smoking$referred_to_courtney==1))
length(which(smoking$wk24_reduced_smoking==1&smoking$referred_to_courtney==2))
length(which(smoking$wk24_reduced_smoking==2&smoking$referred_to_courtney==1))
length(which(smoking$wk24_reduced_smoking==2&smoking$referred_to_courtney==2))

chisq.test(matrix(c(30,25,17,33),2,2))


length(which(smoking[reduce_label,]$gender=="Female"))
length(which(smoking[reduce_label,]$gender=="Male"))
length(which(smoking[no_reduce_label,]$gender=="Female"))
length(which(smoking[no_reduce_label,]$gender=="Male"))
chisq.test(matrix(c(20,35,24,29),2,2))

length(which(smoking[reduce_label,]$race=="Black or African American"))
length(which(smoking[reduce_label,]$race=="White"))
length(which(smoking[reduce_label,]$race=="Asian"))
length(which(smoking[reduce_label,]$race=="Others"))
length(which(smoking[no_reduce_label,]$race=="Black or African American"))
length(which(smoking[no_reduce_label,]$race=="White"))
length(which(smoking[no_reduce_label,]$race=="Asian"))
length(which(smoking[no_reduce_label,]$race=="Others"))
chisq.test(matrix(c(44,5,6,43,5,5),3,2))

length(which(smoking[reduce_label,]$med_cat==1))
length(which(smoking[reduce_label,]$med_cat==2))
length(which(smoking[reduce_label,]$med_cat==3))

length(which(smoking[no_reduce_label,]$med_cat==1))
length(which(smoking[no_reduce_label,]$med_cat==2))
length(which(smoking[no_reduce_label,]$med_cat==3))

chisq.test(matrix(c(14,24,18,9,32,12),3,2))




quantile(smoking$age[reduce_label],c(0.25,0.5,0.75))
sd(smoking$age[reduce_label])
quantile(smoking$age[no_reduce_label],c(0.25,0.5,0.75))
sd(smoking$age[no_reduce_label])
wilcox.test(smoking$age[reduce_label],smoking$age[no_reduce_label])



smoking$First.BMI.2019[reduce_label]
smoking$First.BMI.2019[no_reduce_label]
quantile(smoking$First.BMI.2019[reduce_label],c(0.25,0.5,0.75),na.rm = T)
sd(smoking$First.BMI.2019[reduce_label],na.rm = T)
quantile(smoking$First.BMI.2019[no_reduce_label],c(0.25,0.5,0.75),na.rm = T)
sd(smoking$First.BMI.2019[no_reduce_label],na.rm = T)
wilcox.test(smoking$First.BMI.2019[reduce_label],smoking$First.BMI.2019[no_reduce_label])



length(which(smoking[reduce_label,]$SUD==0))
length(which(smoking[reduce_label,]$SUD==1))
length(which(smoking[no_reduce_label,]$SUD==0))
length(which(smoking[no_reduce_label,]$SUD==1))
chisq.test( matrix(c(5,50,13,40),nrow = 2))



quantile(smoking$CD4_within_6m[reduce_label],c(0.25,0.5,0.75),na.rm = T)
quantile(smoking$CD4_within_6m[no_reduce_label],c(0.25,0.5,0.75),na.rm = T)
wilcox.test(smoking$CD4_within_6m[reduce_label],smoking$CD4_within_6m[no_reduce_label])




length(which(smoking[reduce_label,]$viral_load_with_6m=="< 20"))
sum(1*is.na(smoking[reduce_label]$viral_load_with_6m))

length(which(smoking[no_reduce_label,]$viral_load_with_6m=="< 20"))
sum(1*is.na(smoking[no_reduce_label]$viral_load_with_6m))
chisq.test( matrix(c(37,18,32,21),nrow = 2))








