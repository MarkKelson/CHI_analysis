##########
#CHI analysis
#Loading packages#########
library(meta)
library(readxl)
library(tidyverse)
library(grid)


# Importing data ####
CHI_cont <- read_excel(path="/Users/markkelson/Dropbox/CHI/Code/CHIdatacontinuous.xlsx",sheet=1)
summarydat <- read_excel(path="/Users/markkelson/Dropbox/CHI/Code/SummaryData.xlsx",sheet=1)
#Replacing NA's in summary dat with "Missing"
summarydat[summarydat=="NA"] <- "Missing"

#Redeclaring perc female as a number
summarydat$Perc_Female <- as.numeric(summarydat$Perc_Female)

#Getting non-alphabetical order to some of the variables
#Caseload
summarydat$CM_Caseload <- factor(summarydat$CM_Caseload,levels = c("Light","Medium","High","Missing"))
#Frequency
summarydat$CM_Frequency <- factor(summarydat$CM_Frequency,levels = c("Occasional","Medium","Frequent","VeryFrequent","Missing"))
#Timelimit
summarydat$CM_Time_limit <- factor(summarydat$CM_Time_limit,levels = c("Short","Medium","Long","Missing"))
#Complexity
summarydat$CM_Complexity <- factor(summarydat$CM_Complexity,levels = c("Low","Medium","High","Medium/High","Missing"))
#Availability
summarydat$CM_Availability <- factor(summarydat$CM_Availability,levels = c("OfficeHours","High","Missing"))
#Remote
summarydat$CM_Remote <- factor(summarydat$CM_Remote,levels = c("Remote","In-person","Both","Missing"))
#CM_Type
summarydat$CM_Type <- factor(summarydat$CM_Type,levels = c("HousingFirst","AssertiveCommunityTreatment","CriticalTimeIntervention","IntensiveCaseManagement","StandardCaseManagement"))

#Reading in binary data
CHI_bin <- read_excel(path="/Users/markkelson/Dropbox/CHI/Code/CHIdatabinary.xlsx",sheet=1)

#Reading in generic inverse variance data   
CHI_geninv <- read_excel(path="/Users/markkelson/Dropbox/CHI/Code/CHIdatageninv.xlsx",sheet=1)

# Creating variables ####
CHI_cont$Followup_months_cat <- rep(NA,length(CHI_cont$Followup_months))
CHI_cont$Followup_months_cat[CHI_cont$Followup_months<12] <- "<12"
CHI_cont$Followup_months_cat[CHI_cont$Followup_months>=12] <- ">=12"

CHI_bin$Followup_months_cat <- rep(NA,length(CHI_bin$Followup_months))
CHI_bin$Followup_months_cat[CHI_bin$Followup_months<12] <- "<12"
CHI_bin$Followup_months_cat[CHI_bin$Followup_months>=12] <- ">=12"


#Aligning the directions of effects so that all high scores are bad
#This could be improved
CHI_cont$Me[CHI_cont$Highscore=="Good"] <- CHI_cont$Me[CHI_cont$Highscore=="Good"]*(-1)
CHI_cont$Mc[CHI_cont$Highscore=="Good"] <- CHI_cont$Mc[CHI_cont$Highscore=="Good"]*(-1)

CHI_bin$event.e[CHI_bin$Highscore=="Good"] <- CHI_bin$n.e[CHI_bin$Highscore=="Good"]-CHI_bin$event.e[CHI_bin$Highscore=="Good"]
CHI_bin$event.c[CHI_bin$Highscore=="Good"] <- CHI_bin$n.c[CHI_bin$Highscore=="Good"]-CHI_bin$event.c[CHI_bin$Highscore=="Good"]

CHI_geninv$TE[CHI_geninv$Highscore=="Good"] <- CHI_geninv$TE[CHI_geninv$Highscore=="Good"]*(-1)

#Converting the continuous estimates into effect sizes and storing them
#I run a meta- analysis on all of the data (not a sensible analysis)
#but it gives me study level estimates
contTE <- metacont(data=CHI_cont,
                   mean.e=Me,
                   sd.e=Se,
                   n.e=Ne,
                   mean.c=Mc,
                   sd.c=Sc,
                   n.c=Nc,
                   studlab=Author,
                   sm="SMD",
                   hakn=T)

#Adding TE and seTE variables to the CHI_cont file
CHI_cont$TE <- contTE$TE
CHI_cont$seTE <- contTE$seTE



#Merging study level information in ####
CHIreviewdat <- merge(CHI_cont,summarydat,by.y ="Author")

CHIreviewdatbin <- merge(CHI_bin,summarydat,by.y ="Author")


#Homelessness Continuous short ####

CHI_cont_homeless_short <- CHIreviewdat[CHIreviewdat$Outcome_category=="Homelessness"&
                                        CHIreviewdat$Order==1&
                                        CHIreviewdat$Followup_months<12&
                                        CHIreviewdat$Control=="UsualCare",]

# Question Homelessness less than 1 year followup ####

m_cont_homeless_short <- metacont(data=CHI_cont_homeless_short,
               mean.e=Me,
               sd.e=Se,
               n.e=Ne,
               mean.c=Mc,
               sd.c=Sc,
               n.c=Nc,
               studlab=Author,
               sm="SMD",
               hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_short.png',height=400,width=800) 

forest(m_cont_homeless_short,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Homelessness outcome less than 1 year", .5, .9, gp=gpar(cex=2))

dev.off()

#Homelessness long term outcomes##########
CHI_cont_homeless_long <- CHIreviewdat[CHIreviewdat$Outcome_category=="Homelessness"&
                                       CHIreviewdat$Order==1&
                                       CHIreviewdat$Followup_months>=12&
                                        CHIreviewdat$Control=="UsualCare",]
#Dropping unused levels in time limit
CHI_cont_homeless_long$CM_Time_limit <- CHI_cont_homeless_long$CM_Time_limit[drop=T]
#Dropping unused levels in remote
CHI_cont_homeless_long$CM_Remote <- CHI_cont_homeless_long$CM_Remote[drop=T]
#Dropping unused levels in complexity
CHI_cont_homeless_long$CM_Complexity <- CHI_cont_homeless_long$CM_Complexity[drop=T]

#Question Homelessness more than 1 year followup overall###

m_cont_homeless_long <- metacont(data=CHI_cont_homeless_long,
                                         mean.e=Me,
                                         sd.e=Se,
                                         n.e=Ne,
                                         mean.c=Mc,
                                         sd.c=Sc,
                                         n.c=Nc,
                                         studlab=Author,
                                         sm="SMD",
                                         hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long.png',
    height=500,width=750) 

forest(m_cont_homeless_long,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer", .5, 0.95, gp=gpar(cex=2))

dev.off()
#Question Homelessness more than 1 year followup by CM type###

m_cont_homeless_long_CM_Type <- metacont(data=CHI_cont_homeless_long,
                                  mean.e=Me,
                                  sd.e=Se,
                                  n.e=Ne,
                                  mean.c=Mc,
                                  sd.c=Sc,
                                  n.c=Nc,
                                  studlab=Author,
                                  sm="SMD",
                                  hakn=T,
                                  byvar=CM_Type)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Type.png',
    height=870,width=800) 

forest(m_cont_homeless_long_CM_Type,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by case management type", .5, 0.97, gp=gpar(cex=2))

dev.off()

#Funnel plot
homelesslong_egger <- metabias(m_cont_homeless_long, method.bias = "linreg")


png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_funnel.png',height=900,width=900) 

funnel(m_cont_homeless_long,studlab=T,cex=3,cex.studlab = 2,cex.axis=3,
       xlim=range(c(-2,0.5)))
grid.text(paste("Funnel plot - long term homelessness outcomes\n p-value=",round(homelesslong_egger$p.value,2)), 
          .35, 0.9, gp=gpar(cex=2))

dev.off()

#Metaregression to formally compare
m_reg_cont_homeless_long_CM_Type <- metareg(m_cont_homeless_long,~CM_Type)
summary(m_reg_cont_homeless_long_CM_Type)


####
#Question Homelessness more than 1 year followup by team/individual###
m_cont_homeless_long_CM_Team <- metacont(data=CHI_cont_homeless_long,
                                 mean.e=Me,
                                 sd.e=Se,
                                 n.e=Ne,
                                 mean.c=Mc,
                                 sd.c=Sc,
                                 n.c=Nc,
                                 studlab=Author,
                                 sm="SMD",
                                 hakn=T,
                                 byvar=CM_Team)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Team.png',height=900,width=900) 

forest(m_cont_homeless_long_CM_Team,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by group/individual", .5, 0.95, gp=gpar(cex=2))

dev.off()

###
#Question Homelessness more than 1 year followup by manager###
m_cont_homeless_long_CM_Manager <- metacont(data=CHI_cont_homeless_long,
                                                   mean.e=Me,
                                                   sd.e=Se,
                                                   n.e=Ne,
                                                   mean.c=Mc,
                                                   sd.c=Sc,
                                                   n.c=Nc,
                                                   studlab=Author,
                                                   sm="SMD",
                                                   hakn=T,
                                                   byvar=CM_Manager)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Manager.png',height=800,width=900) 

forest(m_cont_homeless_long_CM_Manager,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by manager type", .5, 0.9, gp=gpar(cex=2))

dev.off()
###
#Question Homelessness more than 1 year followup by conditionality###
m_cont_homeless_long_CM_Conditionality <- metacont(data=CHI_cont_homeless_long,
                                          mean.e=Me,
                                          sd.e=Se,
                                          n.e=Ne,
                                          mean.c=Mc,
                                          sd.c=Sc,
                                          n.c=Nc,
                                          studlab=Author,
                                          sm="SMD",
                                          hakn=T,
                                          byvar=CM_Conditionality)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Conditionality.png',height=800,width=900) 

forest(m_cont_homeless_long_CM_Conditionality,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by conditionality", .5, 0.9, gp=gpar(cex=2))

dev.off()

###
#Question Homelessness more than 1 year followup by continuity###
m_cont_homeless_long_CM_Continuity <- metacont(data=CHI_cont_homeless_long,
                                                   mean.e=Me,
                                                   sd.e=Se,
                                                   n.e=Ne,
                                                   mean.c=Mc,
                                                   sd.c=Sc,
                                                   n.c=Nc,
                                                   studlab=Author,
                                                   sm="SMD",
                                                   hakn=T,
                                                   byvar=CM_Continuity)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Continuity.png',height=900,width=900) 

forest(m_cont_homeless_long_CM_Continuity,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by continuity", .5, 0.9, gp=gpar(cex=2))

dev.off()

###
#Question Homelessness more than 1 year followup by caseload###
m_cont_homeless_long_CM_Caseload <- metacont(data=CHI_cont_homeless_long,
                                               mean.e=Me,
                                               sd.e=Se,
                                               n.e=Ne,
                                               mean.c=Mc,
                                               sd.c=Sc,
                                               n.c=Nc,
                                               studlab=Author,
                                               sm="SMD",
                                               hakn=T,
                                               byvar=CM_Caseload)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Caseload.png',height=900,width=900) 

forest(m_cont_homeless_long_CM_Caseload,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by caseload", .5, 0.95, gp=gpar(cex=2))

dev.off()

###
#Question Homelessness more than 1 year followup by frequency of contact###
m_cont_homeless_long_CM_Frequency <- metacont(data=CHI_cont_homeless_long,
                                             mean.e=Me,
                                             sd.e=Se,
                                             n.e=Ne,
                                             mean.c=Mc,
                                             sd.c=Sc,
                                             n.c=Nc,
                                             studlab=Author,
                                             sm="SMD",
                                             hakn=T,
                                             byvar=CM_Frequency)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Frequency.png',height=900,width=900) 

forest(m_cont_homeless_long_CM_Frequency,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by frequency of contact", .5, 0.95, gp=gpar(cex=2))

dev.off()

###
#Question Homelessness more than 1 year followup by availability###
m_cont_homeless_long_CM_Availability <- metacont(data=CHI_cont_homeless_long,
                                             mean.e=Me,
                                             sd.e=Se,
                                             n.e=Ne,
                                             mean.c=Mc,
                                             sd.c=Sc,
                                             n.c=Nc,
                                             studlab=Author,
                                             sm="SMD",
                                             hakn=T,
                                             byvar=CM_Availability)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Availability.png',height=900,width=900) 

forest(m_cont_homeless_long_CM_Availability,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by availability of case manager", .5, 0.9, gp=gpar(cex=2))

dev.off()

###
#Question Homelessness more than 1 year followup by time limit###
m_cont_homeless_long_CM_Time_limit <- metacont(data=CHI_cont_homeless_long,
                                                 mean.e=Me,
                                                 sd.e=Se,
                                                 n.e=Ne,
                                                 mean.c=Mc,
                                                 sd.c=Sc,
                                                 n.c=Nc,
                                                 studlab=Author,
                                                 sm="SMD",
                                                 hakn=T,
                                                 byvar=CM_Time_limit)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Time_limit.png',height=900,width=900) 

forest(m_cont_homeless_long_CM_Time_limit,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by time limit", .5, 0.95, gp=gpar(cex=2))

dev.off()

#Metaregression to formally check
m_reg_cont_homeless_long_CM_Time_limit <- metareg(m_cont_homeless_long,~CM_Time_limit)
summary(m_reg_cont_homeless_long_CM_Time_limit)



###
#Question Homelessness more than 1 year followup by remote###

m_cont_homeless_long_CM_Remote <- metacont(data=CHI_cont_homeless_long,
                                               mean.e=Me,
                                               sd.e=Se,
                                               n.e=Ne,
                                               mean.c=Mc,
                                               sd.c=Sc,
                                               n.c=Nc,
                                               studlab=Author,
                                               sm="SMD",
                                               hakn=T,
                                               byvar=CM_Remote)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Remote.png',height=800,width=900) 

forest(m_cont_homeless_long_CM_Remote,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by remote or in-person", .5, 0.95, gp=gpar(cex=2))

dev.off()

#Metaregression to formally check

m_reg_cont_homeless_long_CM_Remote <- metareg(m_cont_homeless_long,~CM_Remote)
summary(m_reg_cont_homeless_long_CM_Remote)


###
#Question Homelessness more than 1 year followup by Arranging###
m_cont_homeless_long_CM_Arranging <- metacont(data=CHI_cont_homeless_long,
                                               mean.e=Me,
                                               sd.e=Se,
                                               n.e=Ne,
                                               mean.c=Mc,
                                               sd.c=Sc,
                                               n.c=Nc,
                                               studlab=Author,
                                               sm="SMD",
                                               hakn=T,
                                               byvar=CM_Arranging)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Arranging.png',height=800,width=900) 

forest(m_cont_homeless_long_CM_Arranging,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by service provision", .5, 0.9, gp=gpar(cex=2))

dev.off()


###
#Question Homelessness more than 1 year followup by complexity###
m_cont_homeless_long_CM_Complexity <- metacont(data=CHI_cont_homeless_long,
                                               mean.e=Me,
                                               sd.e=Se,
                                               n.e=Ne,
                                               mean.c=Mc,
                                               sd.c=Sc,
                                               n.c=Nc,
                                               studlab=Author,
                                               sm="SMD",
                                               hakn=T,
                                               byvar=CM_Complexity)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_CM_Complexity.png',height=950,width=900) 

forest(m_cont_homeless_long_CM_Complexity,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Homelessness outcomes one year or longer\nsplit by complexity of need", .5, 0.95, gp=gpar(cex=2))

dev.off()

#Metaregression to formally check

m_reg_cont_homeless_long_CM_Complexity <- metareg(m_cont_homeless_long,~CM_Complexity)
summary(m_reg_cont_homeless_long_CM_Complexity)


#
#Question Homelessness more than 1 year followup by gender###


m_reg_cont_homeless_long_Perc_Female <- metareg(m_cont_homeless_long,~Perc_Female)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_long_Perc_Female_bubble.png',height=800,width=900) 

bubble(m_reg_cont_homeless_long_Perc_Female,
       main="Homelessness outcomes for long term follow-up by % female",
       sub=paste("Slope = ",
                 round(m_reg_cont_homeless_long_Perc_Female$beta[2],3),
                 "p-value=",
                 round(m_reg_cont_homeless_long_Perc_Female$pval[2],2)),
       xlab = "Percentage female participants",
       studlab=T)

dev.off()






#Binary approach####
CHI_bin_homeless_short <- CHIreviewdatbin[CHIreviewdatbin$Outcome_category=="Homelessness"&
                                            CHIreviewdatbin$Order==1&
                                            CHIreviewdatbin$Followup_months<12&
                                            CHIreviewdatbin$Control=="UsualCare",]

# Question Homelessness binary less than 1 year followup ####

m_bin_homeless_short <- metabin(data=CHI_bin_homeless_short,
                                  event.e=event.e,
                                  n.e=n.e,
                                  event.c=event.c,
                                  n.c=n.c,
                                  studlab=Author,
                                  sm="RR",
                                  hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Homelessness_bin_short.png',height=400,width=900) 

forest(m_bin_homeless_short,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Binary homelessness outcome less than 1 year", .5, .9, gp=gpar(cex=2))

dev.off()




#Generic Inverse variance approach####
#Need to extract TE and seTE from the above meta object and combine with the 
#directly observed estimates



#Merging study-level summary info together
CHI_geninv_summary <- merge(CHI_geninv,summarydat,by.y="Author")
#This merges the broader outcomes data with the geninv ones.
# Every study has a TE and an seTE here
CHI_geninv_merged <- merge(CHIreviewdat,CHI_geninv_summary,all.x=T,all.y=T)
#Merging study level information in ####


CHI_geninv_homeless_long <- CHI_geninv_merged[CHI_geninv_merged$Outcome_category=="Homelessness"&
                                              CHI_geninv_merged$Order==1&
                                              CHI_geninv_merged$Followup_months>=12&
                                              CHI_geninv_merged$Control=="UsualCare",]

# Gen InvQuestion Homelessness less than 1 year followup ####

m_geninv_homeless_long <- metagen(data=CHI_geninv_homeless_long,
                                  TE=TE,
                                  seTE=seTE,
                                  studlab=Author,
                                  sm="SMD",
                                  hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Gen_inv_Homelessness_long.png',height=900,width=900) 

forest(m_geninv_homeless_long,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Generic inverse variance - Homelessness outcome one year or longer", .5, .9, gp=gpar(cex=2))

dev.off()

# Gen InvQuestion Homelessness less than 1 year followup ####

m_geninv_homeless_long_CM_Type <- metagen(data=CHI_geninv_homeless_long,
                                  TE=TE,
                                  seTE=seTE,
                                  studlab=Author,
                                  sm="SMD",
                                  hakn=T,
                                  byvar=CM_Type)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Gen_inv_Homelessness_long_CM_Type.png',height=1000,width=900) 

forest(m_geninv_homeless_long_CM_Type,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Generic inverse variance - Homelessness outcome one year or longer\n by case management type", .5, .95, gp=gpar(cex=2))

dev.off()







#Mental Health Continuous short ####

CHI_cont_MH_short <- CHIreviewdat[CHIreviewdat$Outcome_category=="MentalHealth"&
                                            CHIreviewdat$Order==1&
                                            CHIreviewdat$Followup_months<12&
                                            CHIreviewdat$Control=="UsualCare",]

# Question MH less than 1 year followup ####

m_cont_MH_short <- metacont(data=CHI_cont_MH_short,
                                  mean.e=Me,
                                  sd.e=Se,
                                  n.e=Ne,
                                  mean.c=Mc,
                                  sd.c=Sc,
                                  n.c=Nc,
                                  studlab=Author,
                                  sm="SMD",
                                  hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_short.png',height=400,width=800) 

forest(m_cont_MH_short,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Mental Health outcomes less than 1 year", .5, .9, gp=gpar(cex=2))

dev.off()

# Mental Health Long####

CHI_cont_MH_long <- CHIreviewdat[CHIreviewdat$Outcome_category=="MentalHealth"&
                                     CHIreviewdat$Order==1&
                                     CHIreviewdat$Followup_months>=12&
                                     CHIreviewdat$Control=="UsualCare",]


m_cont_MH_long <- metacont(data=CHI_cont_MH_long,
                                 mean.e=Me,
                                 sd.e=Se,
                                 n.e=Ne,
                                 mean.c=Mc,
                                 sd.c=Sc,
                                 n.c=Nc,
                                 studlab=Author,
                                 sm="SMD",
                                 hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long.png',
    height=500,width=750) 

forest(m_cont_MH_long,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer", .5, 0.95, gp=gpar(cex=2))

dev.off()



#Funnel plot
MHlong_egger <- metabias(m_cont_MH_long, method.bias = "linreg")


png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_funnel.png',height=900,width=900) 

funnel(m_cont_MH_long,studlab=T,cex=3,cex.studlab = 2,cex.axis=3,
       xlim=range(c(-2,0.5)))
grid.text(paste("Funnel plot - long term Mental Health outcomes\n p-value=",round(MHlong_egger$p.value,2)), 
          .35, 0.9, gp=gpar(cex=2))

dev.off()






####
#Question Mental Health more than 1 year followup by team/individual###
m_cont_MH_long_CM_Type <- metacont(data=CHI_cont_MH_long,
                                   mean.e=Me,
                                   sd.e=Se,
                                   n.e=Ne,
                                   mean.c=Mc,
                                   sd.c=Sc,
                                   n.c=Nc,
                                   studlab=Author,
                                   sm="SMD",
                                   hakn=T,
                                   byvar=CM_Type)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Type.png',height=900,width=900) 

forest(m_cont_MH_long_CM_Type,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by case management type", .5, 0.85, gp=gpar(cex=2))

dev.off()

#Formally exploring differences between case management type and MH outcomes.
m_reg_cont_MH_long_CM_Type <- metareg(m_cont_MH_long,~CM_Type)
summary(m_reg_cont_MH_long_CM_Type)










####
#Question Mental Health more than 1 year followup by team/individual###
m_cont_MH_long_CM_Team <- metacont(data=CHI_cont_MH_long,
                                    mean.e=Me,
                                    sd.e=Se,
                                    n.e=Ne,
                                    mean.c=Mc,
                                    sd.c=Sc,
                                    n.c=Nc,
                                    studlab=Author,
                                    sm="SMD",
                                    hakn=T,
                                    byvar=CM_Team)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Team.png',height=900,width=900) 

forest(m_cont_MH_long_CM_Team,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by team/individual case manager", .5, 0.8, gp=gpar(cex=2))

dev.off()

###
#Question Mental Health more than 1 year followup by manager###
m_cont_MH_long_CM_Manager <- metacont(data=CHI_cont_MH_long,
                                                   mean.e=Me,
                                                   sd.e=Se,
                                                   n.e=Ne,
                                                   mean.c=Mc,
                                                   sd.c=Sc,
                                                   n.c=Nc,
                                                   studlab=Author,
                                                   sm="SMD",
                                                   hakn=T,
                                                   byvar=CM_Manager)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Manager.png',height=700,width=900) 

forest(m_cont_MH_long_CM_Manager,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by manager type", .5, .9, gp=gpar(cex=2))

dev.off()
###
#Question Mental Health more than 1 year followup by conditionality###
m_cont_MH_long_CM_Conditionality <- metacont(data=CHI_cont_MH_long,
                                             mean.e=Me,
                                             sd.e=Se,
                                             n.e=Ne,
                                             mean.c=Mc,
                                             sd.c=Sc,
                                             n.c=Nc,
                                             studlab=Author,
                                             sm="SMD",
                                             hakn=T,
                                             byvar=CM_Conditionality)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Conditionality.png',height=900,width=900) 

forest(m_cont_MH_long_CM_Conditionality,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by conditionality", .5, 0.8, gp=gpar(cex=2))

dev.off()

###
#Question Mental Health more than 1 year followup by continuity###
m_cont_MH_long_CM_Continuity <- metacont(data=CHI_cont_MH_long,
                                                   mean.e=Me,
                                                   sd.e=Se,
                                                   n.e=Ne,
                                                   mean.c=Mc,
                                                   sd.c=Sc,
                                                   n.c=Nc,
                                                   studlab=Author,
                                                   sm="SMD",
                                                   hakn=T,
                                                   byvar=CM_Continuity)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Continuity.png',height=900,width=900) 

forest(m_cont_MH_long_CM_Continuity,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by continuity", .5, 0.85, gp=gpar(cex=2))

dev.off()

###
#Question Mental Health more than 1 year followup by caseload###
m_cont_MH_long_CM_Caseload <- metacont(data=CHI_cont_MH_long,
                                       mean.e=Me,
                                       sd.e=Se,
                                       n.e=Ne,
                                       mean.c=Mc,
                                       sd.c=Sc,
                                       n.c=Nc,
                                       studlab=Author,
                                       sm="SMD",
                                       hakn=T,
                                       byvar=CM_Caseload)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Caseload.png',height=900,width=900) 

forest(m_cont_MH_long_CM_Caseload,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by caseload", .5, 0.85, gp=gpar(cex=2))

dev.off()

###
#Question Mental Health more than 1 year followup by frequency of contact###
m_cont_MH_long_CM_Frequency <- metacont(data=CHI_cont_MH_long,
                                             mean.e=Me,
                                             sd.e=Se,
                                             n.e=Ne,
                                             mean.c=Mc,
                                             sd.c=Sc,
                                             n.c=Nc,
                                             studlab=Author,
                                             sm="SMD",
                                             hakn=T,
                                             byvar=CM_Frequency)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Frequency.png',height=900,width=900) 

forest(m_cont_MH_long_CM_Frequency,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by frequency of contact", .5, 0.85, gp=gpar(cex=2))

dev.off()

###
#Question Mental Health more than 1 year followup by availability###
m_cont_MH_long_CM_Availability <- metacont(data=CHI_cont_MH_long,
                                           mean.e=Me,
                                           sd.e=Se,
                                           n.e=Ne,
                                           mean.c=Mc,
                                           sd.c=Sc,
                                           n.c=Nc,
                                           studlab=Author,
                                           sm="SMD",
                                           hakn=T,
                                           byvar=CM_Availability)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Availability.png',height=900,width=900) 

forest(m_cont_MH_long_CM_Availability,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by availability of case manager", .5, 0.85, gp=gpar(cex=2))

dev.off()

###
#Question Mental Health more than 1 year followup by time limit###
m_cont_MH_long_CM_Time_limit <- metacont(data=CHI_cont_MH_long,
                                                 mean.e=Me,
                                                 sd.e=Se,
                                                 n.e=Ne,
                                                 mean.c=Mc,
                                                 sd.c=Sc,
                                                 n.c=Nc,
                                                 studlab=Author,
                                                 sm="SMD",
                                                 hakn=T,
                                                 byvar=CM_Time_limit)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Time_limit.png',height=800,width=900) 

forest(m_cont_MH_long_CM_Time_limit,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by time limit", .5, 0.9, gp=gpar(cex=2))

dev.off()

###
#Question Mental Health more than 1 year followup by remote###
m_cont_MH_long_CM_Remote <- metacont(data=CHI_cont_MH_long,
                                     mean.e=Me,
                                     sd.e=Se,
                                     n.e=Ne,
                                     mean.c=Mc,
                                     sd.c=Sc,
                                     n.c=Nc,
                                     studlab=Author,
                                     sm="SMD",
                                     hakn=T,
                                     byvar=CM_Remote)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Remote.png',height=800,width=900) 

forest(m_cont_MH_long_CM_Remote,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by remote or in-person", .5, 0.9, gp=gpar(cex=2))

dev.off()

###
#Question Mental Health more than 1 year followup by Arranging###
m_cont_MH_long_CM_Arranging <- metacont(data=CHI_cont_MH_long,
                                               mean.e=Me,
                                               sd.e=Se,
                                               n.e=Ne,
                                               mean.c=Mc,
                                               sd.c=Sc,
                                               n.c=Nc,
                                               studlab=Author,
                                               sm="SMD",
                                               hakn=T,
                                               byvar=CM_Arranging)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Arranging.png',height=800,width=900) 

forest(m_cont_MH_long_CM_Arranging,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by service provision", .5, 0.9, gp=gpar(cex=2))

dev.off()


###
#Question Mental Health more than 1 year followup by complexity###
m_cont_MH_long_CM_Complexity <- metacont(data=CHI_cont_MH_long,
                                         mean.e=Me,
                                         sd.e=Se,
                                         n.e=Ne,
                                         mean.c=Mc,
                                         sd.c=Sc,
                                         n.c=Nc,
                                         studlab=Author,
                                         sm="SMD",
                                         hakn=T,
                                         byvar=CM_Complexity)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_CM_Complexity.png',height=800,width=900) 

forest(m_cont_MH_long_CM_Complexity,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Mental Health outcomes one year or longer\nsplit by complexity of need", .5, 0.9, gp=gpar(cex=2))

dev.off()

#
#Question Mental Health more than 1 year followup by gender###


m_reg_cont_homeless_long_Perc_Female <- metareg(m_cont_MH_long,~Perc_Female)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MH_long_Perc_Female_bubble.png',height=800,width=900) 

bubble(m_reg_cont_homeless_long_Perc_Female,
       main="Mental Health outcomes for long term follow-up by % female",
       sub=paste("Slope = ",
                 round(m_reg_cont_homeless_long_Perc_Female$beta[2],3),
                 "p-value=",
                 round(m_reg_cont_homeless_long_Perc_Female$pval[2],2)),
       xlab = "Percentage female participants",
       studlab=T)

dev.off()






#Binary approach####
CHI_bin_MH_short <- CHIreviewdatbin[CHIreviewdatbin$Outcome_category=="MentalHealth"&
                                            CHIreviewdatbin$Order==1&
                                            CHIreviewdatbin$Followup_months<12&
                                            CHIreviewdatbin$Control=="UsualCare",]

# Question Mental Health binary less than 1 year followup ####

m_bin_MH_short <- metabin(data=CHI_bin_MH_short,
                                  event.e=event.e,
                                  n.e=n.e,
                                  event.c=event.c,
                                  n.c=n.c,
                                  studlab=Author,
                                  sm="RR",
                                  hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/MentalHealth_bin_short.png',height=400,width=900) 

forest(m_bin_MH_short,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Binary homelessness outcome less than 1 year", .5, .9, gp=gpar(cex=2))

dev.off()

#Substance Use ####

#Substance Use Continuous short ####

CHI_cont_SubstanceUse_short <- CHIreviewdat[CHIreviewdat$Outcome_category=="SubstanceUse"&
                                      CHIreviewdat$Order==1&
                                      CHIreviewdat$Followup_months<12&
                                      CHIreviewdat$Control=="UsualCare",]

# Question SubstanceUse less than 1 year followup ####

m_cont_SubstanceUse_short <- metacont(data=CHI_cont_SubstanceUse_short,
                            mean.e=Me,
                            sd.e=Se,
                            n.e=Ne,
                            mean.c=Mc,
                            sd.c=Sc,
                            n.c=Nc,
                            studlab=Author,
                            sm="SMD",
                            hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/SubstanceUse_short.png',height=400,width=800) 

forest(m_cont_SubstanceUse_short,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Substance Use outcomes less than 1 year", .5, .9, gp=gpar(cex=2))

dev.off()

# SubstanceUse Long####

CHI_cont_SubstanceUse_long <- CHIreviewdat[CHIreviewdat$Outcome_category=="SubstanceUse"&
                                     CHIreviewdat$Order==1&
                                     CHIreviewdat$Followup_months>=12&
                                     CHIreviewdat$Control=="UsualCare",]


m_cont_SubstanceUse_long <- metacont(data=CHI_cont_SubstanceUse_long,
                           mean.e=Me,
                           sd.e=Se,
                           n.e=Ne,
                           mean.c=Mc,
                           sd.c=Sc,
                           n.c=Nc,
                           studlab=Author,
                           sm="SMD",
                           hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/SubstanceUse_long.png',
    height=500,width=750) 

forest(m_cont_SubstanceUse_long,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Substance Use outcomes one year or longer", .5, 0.95, gp=gpar(cex=2))

dev.off()




#PhysicalHealth ####

#PhysicalHealth Continuous short ####

CHI_cont_PhysicalHealth_short <- CHIreviewdat[CHIreviewdat$Outcome_category=="PhysicalHealth"&
                                                CHIreviewdat$Order==1&
                                                CHIreviewdat$Followup_months<12&
                                                CHIreviewdat$Control=="UsualCare",]

# Question PhysicalHealth less than 1 year followup ####

m_cont_PhysicalHealth_short <- metacont(data=CHI_cont_PhysicalHealth_short,
                                      mean.e=Me,
                                      sd.e=Se,
                                      n.e=Ne,
                                      mean.c=Mc,
                                      sd.c=Sc,
                                      n.c=Nc,
                                      studlab=Author,
                                      sm="SMD",
                                      hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/PhysicalHealth_short.png',height=400,width=800) 

forest(m_cont_PhysicalHealth_short,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Physical Health outcomes less than 1 year", .5, .9, gp=gpar(cex=2))

dev.off()

# SubstanceUse Long####

CHI_cont_PhysicalHealth_long <- CHIreviewdat[CHIreviewdat$Outcome_category=="PhysicalHealth"&
                                               CHIreviewdat$Order==1&
                                               CHIreviewdat$Followup_months>=12&
                                               CHIreviewdat$Control=="UsualCare",]


m_cont_PhysicalHealth_long <- metacont(data=CHI_cont_PhysicalHealth_long,
                                     mean.e=Me,
                                     sd.e=Se,
                                     n.e=Ne,
                                     mean.c=Mc,
                                     sd.c=Sc,
                                     n.c=Nc,
                                     studlab=Author,
                                     sm="SMD",
                                     hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/PhysicalHealth_long.png',
    height=500,width=750) 

forest(m_cont_PhysicalHealth_long,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Physical Health outcomes one year or longer", .5, 0.95, gp=gpar(cex=2))

dev.off()


#CapabilitiesandWellbeing ####

#CapabilitiesandWellbeing Continuous short ####

CHI_cont_CapabilitiesandWellbeing_short <- CHIreviewdat[CHIreviewdat$Outcome_category=="CapabilitiesandWellbeing"&
                                                            CHIreviewdat$Order==1&
                                                            CHIreviewdat$Followup_months<12&
                                                            CHIreviewdat$Control=="UsualCare",]

# Question CapabilitiesandWellbeing less than 1 year followup ####

m_cont_CapabilitiesandWellbeing_short <- metacont(data=CHI_cont_CapabilitiesandWellbeing_short,
                                                  mean.e=Me,
                                                  sd.e=Se,
                                                  n.e=Ne,
                                                  mean.c=Mc,
                                                  sd.c=Sc,
                                                  n.c=Nc,
                                                  studlab=Author,
                                                  sm="SMD",
                                                  hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/CapabilitiesandWellbeing_short.png',height=400,width=800) 

forest(m_cont_CapabilitiesandWellbeing_short,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Capabilities and wellbeing outcomes less than 1 year", .5, .9, gp=gpar(cex=2))

dev.off()

# SubstanceUse Long####

CHI_cont_CapabilitiesandWellbeing_long <- CHIreviewdat[CHIreviewdat$Outcome_category=="CapabilitiesandWellbeing"&
                                                           CHIreviewdat$Order==1&
                                                           CHIreviewdat$Followup_months>=12&
                                                           CHIreviewdat$Control=="UsualCare",]


m_cont_CapabilitiesandWellbeing_long <- metacont(data=CHI_cont_CapabilitiesandWellbeing_long,
                                                 mean.e=Me,
                                                 sd.e=Se,
                                                 n.e=Ne,
                                                 mean.c=Mc,
                                                 sd.c=Sc,
                                                 n.c=Nc,
                                                 studlab=Author,
                                                 sm="SMD",
                                                 hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/CapabilitiesandWellbeing_long.png',
    height=500,width=750) 

forest(m_cont_CapabilitiesandWellbeing_long,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Capabilities and wellbeing outcomes one year or longer", .5, 0.95, gp=gpar(cex=2))

dev.off()


#Employment ####

#Employment Continuous short ####

CHI_cont_Employment_short <- CHIreviewdat[CHIreviewdat$Outcome_category=="Employment"&
                                              CHIreviewdat$Order==1&
                                              CHIreviewdat$Followup_months<12&
                                              CHIreviewdat$Control=="UsualCare",]

# Question Employment less than 1 year followup ####

m_cont_Employment_short <- metacont(data=CHI_cont_Employment_short,
                                    mean.e=Me,
                                    sd.e=Se,
                                    n.e=Ne,
                                    mean.c=Mc,
                                    sd.c=Sc,
                                    n.c=Nc,
                                    studlab=Author,
                                    sm="SMD",
                                    hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Employment_short.png',height=400,width=800) 

forest(m_cont_Employment_short,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control") 

grid.text("Employment outcomes less than 1 year", .5, .9, gp=gpar(cex=2))

dev.off()

# SubstanceUse Long####

CHI_cont_Employment_long <- CHIreviewdat[CHIreviewdat$Outcome_category=="Employment"&
                                             CHIreviewdat$Order==1&
                                             CHIreviewdat$Followup_months>=12&
                                             CHIreviewdat$Control=="UsualCare",]


m_cont_Employment_long <- metacont(data=CHI_cont_Employment_long,
                                   mean.e=Me,
                                   sd.e=Se,
                                   n.e=Ne,
                                   mean.c=Mc,
                                   sd.c=Sc,
                                   n.c=Nc,
                                   studlab=Author,
                                   sm="SMD",
                                   hakn=T)

png(file = '/Users/markkelson/Dropbox/CHI/Code/Employment_long.png',
    height=500,width=750) 

forest(m_cont_Employment_long,digits.mean=1,digits.sd=1,
       label.left = "Favours intervention",
       label.right = "Favours control")

grid.text("Employment outcomes one year or longer", .5, 0.95, gp=gpar(cex=2))

dev.off()

