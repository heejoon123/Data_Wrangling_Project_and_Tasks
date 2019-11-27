# PLOT 1 : 
gender_total_meds <- aggregate(
  demo_med_df$`Number of Prescription Medicines Taken`~demo_med_df$Gender, demo_med_df,
  sum)

colnames(gender_total_meds) <- c("Gender", "Total Prescription Medications Taken")

p1 <- barplot(gender_total_meds$`Total Prescription Medications Taken`,
        names = gender_total_meds$Gender,
        main="Total Prescription Medications Taken Based on Gender",
        xlab="Gender", ylab="Total Number of Prescription Medications", 
        col = c("violet", "steelblue"),
        ylim=c(0, 50000))
text(x=p1, y= gender_total_meds$`Total Prescription Medications Taken`+1800, 
     labels=as.character(gender_total_meds$`Total Prescription Medications Taken`))

# PLOT 2 : age and 
ICD <- data.frame(matrix(data=NA, 
                         nrow=length(unique(demo_med_df$`ICD-10-CM Code 1 Description`)), 
                         ncol=2))
ICD[,1] <- sort(unique(demo_med_df$`ICD-10-CM Code 1 Description`))
counts <- count(demo_med_df$`ICD-10-CM Code 1 Description`)
head(counts)
ICD[,2] <- counts[,2]

colnames(ICD) <- c("ICD Descriptions", "Frequency")

ICD_more400 <- ICD[which(ICD[,2] > 400),]
t(as.matrix(ICD_more400))

p2 <- barplot(ICD_more400$Frequency,
        xlab="ICD-10-CM Code 1 Descriptions",
        ylab = "Frequency",
        main="Most Common ICD-10-CM Code 1 Descriptions",
        col=c("turquoise", "lightpink", "green", "thistle", "orange", "red"),
        ylim = c(0, 4000))
text(x=p2, y= ICD_more400$Frequency+120, 
     labels=as.character(ICD_more400$Frequency))
legend("topright",
       legend=ICD_more400$`ICD Descriptions`, 
       fill=c("turquoise", "lightpink", "green", "thistle", "orange", "red"),
       cex=0.7)

## PLOT 3 :Common Drug
drug_freq <- data.frame(count(demo_med_df$`Generic Drug Name`))
colnames(drug_freq) <- c("Medication Name", "Frequency")

most_common_dr <- drug_freq[which(drug_freq[,2] > 360),]
most_common_dr

p3 <- barplot(most_common_dr$Frequency, xlab="Generic Drug Name",
              ylab="Frequency", 
              main="Top 5 Most Common Prescription Medication",
              col=c("lightseagreen", "tan4", "darkgreen", "violetred", "steelblue"),
              ylim=c(0, 650))
text(x=p3, y= most_common_dr$Frequency+20, 
     labels=as.character(most_common_dr$Frequency))
legend("topright",
       legend=most_common_dr$`Medication Name`, 
       fill=c("lightseagreen", "tan4", "darkgreen", "violetred", "steelblue"),
       cex=0.7)

## PLOT 4: RACE / ICD Description
race_ICD <- data.frame(matrix(data=NA, 
                                  nrow=length(unique(demo_med_df$`ICD-10-CM Code 1 Description`)), 
                                  ncol=1+length(unique(demo_med_df$Race))))

colnames(race_ICD) <- c("ICD Description", unique(demo_med_df$Race))
race_ICD[,1] <- sort(unique(demo_med_df$`ICD-10-CM Code 1 Description`))
race_ICD[,2] <- count()

b <- data.frame(table(demo_med_df$`ICD-10-CM Code 1 Description`, demo_med_df$Race))
colnames(b) <- c("ICD", "Race", "Frequency")

library(dplyr)
B <- b %>% 
  group_by(b$ICD, b$Race) %>%
  tally()

B2 <-  b %>% 
  group_by(b$ICD, b$Race) %>%
  count()

B2[,4:6] <- NULL

library(dplyr)
library(magrittr)
Bmax <- B2[ B2$Frequency == ave(B2$Frequency, B2$Race, FUN=max), ]

p4 <- barplot(Bmax$Frequency, xlab="Race", ylab="Frequency",
              main="Most Common ICD-10-CM Code 1 Description for Race",
              names.arg = Bmax$Race, cex.names = 0.6, 
              col="lightblue", ylim=c(0,1400))
text(x=p4, y= Bmax$Frequency+40, 
     labels=as.character(Bmax$Frequency))
legend("topright",
       legend=Bmax$ICD[1], 
       fill="lightblue",
       cex=0.7)

# P5 Race and Drugs
race_drugs <- data.frame(table(demo_med_df$`Generic Drug Name`, demo_med_df$Race))
colnames(race_drugs) <- c("Medication", "Race", "Frequency")

RD2 <-  race_drugs %>% 
  group_by(race_drugs$Medication, race_drugs$Race) %>%
  count()
RD2[,4:6] <- NULL

RD2max <- RD2[ RD2$Frequency == ave(RD2$Frequency, RD2$Race, FUN=max), ]

med_cols <- c("lightblue", "lightpink", "darkcyan", "darkorchid4", "darkorchid4", "darkorchid4")

p5 <- barplot(RD2max$Frequency, xlab="Race", ylab= "Frequency", 
              main="Most Common Medication by Race (2013-2014)",
              col=med_cols, names.arg = RD2max$Race, cex.names=0.55,
              ylim=c(0, 300))
text(x=p5, y=RD2max$Frequency+20, labels = as.character(RD2max$Frequency))
legend("topright",
       legend=unique(RD2max$Medication), 
       fill=c("lightblue", "lightpink", "darkcyan", "darkorchid4"),
       cex=0.9)

## PLOT 6: Marital Status and ICD 
marriage_ICD <- data.frame(table(demo_med_df$`ICD-10-CM Code 1 Description`,
                                 demo_med_df$`Marital status`))
colnames(marriage_ICD) <- c("ICD", "Marital Status", "Frequency")

MarICD <-  marriage_ICD %>% 
  group_by(marriage_ICD$`Marital Status`, marriage_ICD$ICD) %>%
  count()
MarICD[,4:6] <-NULL
MarICD_max <- MarICD[MarICD$Frequency == ave(MarICD$Frequency, 
                                             MarICD$Marital.Status, FUN=max), ]
MarICD_max<-MarICD_max[!(MarICD_max$Marital.Status=="Missing" | 
                           MarICD_max$Marital.Status=="Don't Know" |
                           MarICD_max$Marital.Status=="Refused"),]

p6 <- barplot(MarICD_max$Frequency, xlab="Marital Status", ylab="Frequency",
              main="Most Common ICD Descriptions per Marital Status",
              col="aquamarine3", names.arg=MarICD_max$Marital.Status, cex.names = 0.65,
              ylim=c(0,1400))
text(x=p6, y=MarICD_max$Frequency+50, labels=as.character(MarICD_max$Frequency))
legend("topright", legend=unique(MarICD_max$ICD), fill="aquamarine3", cex=0.7)

# PLOT 7 : age and drugs
age_med <- data.frame(table(demo_med_df$`Age at Screening (Years)`, 
                            demo_med_df$`Generic Drug Name`))
colnames(age_med) <- c("Age", "Medication", "Frequency")

age_med$`Age Range` <- cut(as.numeric(age_med$Age), breaks=c(0, 20, 40, 60, 80, 100))
typeof(age_med$`Age Range`)
age_med$`Age Range` <- as.character(age_med$`Age Range`)
age_med$`Age Range` <- gsub( "\\(", "", as.character(age_med$`Age Range`))
age_med$`Age Range` <- gsub( "\\]", "", as.character(age_med$`Age Range`))
age_med$`Age Range` <- gsub( "\\,", " to ", as.character(age_med$`Age Range`))

AgeMed <-  age_med %>% 
  group_by(age_med$Medication, age_med$`Age Range`) %>%
  count()

AgeMed[,5:7] <- NULL

agemed_max <- AgeMed[AgeMed$Frequency == ave(AgeMed$Frequency, 
                                             AgeMed$Age.Range, FUN=max), ]

age_color <- c("lightblue", "lightpink", "lightpink", "thistle", "forestgreen", "forestgreen")

am <- as.matrix(agemed_max[,4:3])

library(ggplot2)

p7 <- ggplot(agemed_max, aes(x=agemed_max$Age.Range, y=agemed_max$Frequency, 
                             fill=agemed_max$Medication,
                             label=agemed_max$Frequency))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_discrete(name="Medication",
                      labels=unique(agemed_max$Medication))+
  xlab("Age Range") + ylab("Frequency")+
  ylim(c(0,100))+
  ggtitle("Most Common Medications by Age (2013-2014)")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme(panel.background = element_rect(fill="white"))
p7

## Plot 8 : Age and Prescriptions Taken
age_total_meds <- aggregate(
  demo_med_df$`Number of Prescription Medicines Taken`~demo_med_df$`Age at Screening (Years)`, 
  demo_med_df,
  sum)
colnames(age_total_meds) <- c("Age", "Total Prescription Medications", "Age Range")

age_total_meds[,3] <- cut(as.numeric(age_total_meds$Age), breaks=c(0, 20, 40, 60, 80, 100))

age_total_meds$`Age Range` <- as.character(age_total_meds$`Age Range`)
age_total_meds$`Age Range` <- gsub( "\\(", "", as.character(age_total_meds$`Age Range`))
age_total_meds$`Age Range` <- gsub( "\\]", "", as.character(age_total_meds$`Age Range`))
age_total_meds$`Age Range` <- gsub( "\\,", " to ", as.character(age_total_meds$`Age Range`))

age_total_meds<- age_total_meds[-1,]

age_total_meds2 <- aggregate(
  age_total_meds$`Total Prescription Medications`~age_total_meds$`Age Range`,
  age_total_meds, sum)

colnames(age_total_meds2) <- c("Age Range", "Total Prescription Medications")

ggplot(age_total_meds2, aes(x=age_total_meds2$`Age Range`, 
                             y=age_total_meds2$`Total Prescription Medications`,
                             fill=age_total_meds2$`Age Range`,
                            label=age_total_meds2$`Total Prescription Medications`))+
  scale_fill_discrete(name="Age Ranges")+
  geom_bar(stat='identity')+
  xlab("Age Range") + ylab("Number of Prescriptions") +
  ggtitle("Total Number of Prescriptions Taken by Age (2013-2014)")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme(panel.background = element_rect(fill="white"),axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  ylim(c(0, 50000))


## PLOT 9: Age and ICD
age_ICD <- data.frame(table(demo_med_df$`Age at Screening (Years)`, 
                            demo_med_df$`ICD-10-CM Code 1 Description`))
colnames(age_ICD) <- c("Age", "ICD", "Frequency")

age_ICD$`Age Range` <- cut(as.numeric(age_ICD$Age), breaks=c(0, 20, 40, 60, 80, 100))
typeof(age_ICD$`Age Range`)
age_ICD$`Age Range` <- as.character(age_ICD$`Age Range`)
age_ICD$`Age Range` <- gsub( "\\(", "", as.character(age_ICD$`Age Range`))
age_ICD$`Age Range` <- gsub( "\\]", "", as.character(age_ICD$`Age Range`))
age_ICD$`Age Range` <- gsub( "\\,", " to ", as.character(age_ICD$`Age Range`))

AgeICD <-  age_ICD %>% 
  group_by(age_ICD$ICD, age_ICD$`Age Range`) %>%
  count()

AgeICD[,5:7] <- NULL

ageICD_max <- AgeICD[AgeICD$Frequency == ave(AgeICD$Frequency, 
                                             AgeICD$Age.Range, FUN=max), ]

p9 <- ggplot(ageICD_max, aes(x=ageICD_max$Age.Range, y=ageICD_max$Frequency, 
                             fill=ageICD_max$ICD,
                             label=ageICD_max$Frequency))+
  geom_bar(stat="identity")+
  scale_fill_discrete(name="ICD Description",
                      labels=unique(ageICD_max$ICD))+
  xlab("Age Range") + ylab("Frequency")+
  ylim(c(0,100))+
  ggtitle("Most Common ICD-10 Descriptions by Age (2013-2014)")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme(panel.background = element_rect(fill="white"))
p9

## Plot 9: Marital Status and Medications
marriage_med <- data.frame(table(demo_med_df$`Generic Drug Name`,
                                 demo_med_df$`Marital status`))
colnames(marriage_med) <- c("Medication", "Marital Status", "Frequency")

Marmed <-  marriage_med %>% 
  group_by(marriage_med$`Marital Status`, marriage_med$Medication) %>%
  count()
Marmed[,4:6] <-NULL
Marmed_max <- Marmed[Marmed$Frequency == ave(Marmed$Frequency, 
                                             Marmed$Marital.Status, FUN=max), ]
Marmed_max<-Marmed_max[!(Marmed_max$Marital.Status=="Missing" | 
                           Marmed_max$Marital.Status=="Don't Know" |
                           Marmed_max$Marital.Status=="Refused"),]

(p9 <- ggplot(Marmed_max, aes(x=Marmed_max$Marital.Status, y=Marmed_max$Frequency,
                             fill=Marmed_max$Medication,
                             label=Marmed_max$Frequency))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_discrete(name="Medication", labels=unique(Marmed_max$Medication))+
  xlab("Marital Status") + ylab("Frequency")+
  ggtitle("Most Common Medication per Marital Status")+
  geom_text(size=3, position = position_stack(vjust = 0.5))+
  theme(panel.background = element_rect(fill="white"), 
        axis.text.x = element_text(angle = 45, hjust = 1)))

## PLOT 10: Education Level Medication
ed_med <- data.frame(table(demo_med_df$`Generic Drug Name`,
                                 demo_med_df$`Highest Education`))
colnames(ed_med) <- c("Medication", "Education", "Frequency")

EDmed <-  ed_med %>% 
  group_by(ed_med$Education, ed_med$Medication) %>%
  count()

EDmed[,4:6] <-NULL

EDmax <- EDmed[EDmed$Frequency == ave(EDmed$Frequency, 
                                             EDmed$Education, FUN=max), ]

EDmax<-EDmax[!(EDmax$Education=="Missing" | 
                      EDmax$Education=="Don't Know" |
                      EDmax$Education=="Refused"),]

(p10 <- ggplot(EDmax, aes(x=EDmax$Education, y=EDmax$Frequency,
                              fill=EDmax$Medication,
                              label=EDmax$Frequency))+
    geom_bar(stat="identity")+
    scale_fill_discrete(name="Medication", labels=unique(EDmax$Medication))+
    xlab("Highest Education") + ylab("Frequency")+
    ggtitle("Most Common Medication per Highest Education Level")+
    geom_text(size=3, position = position_stack(vjust = 0.5))+
    theme(panel.background = element_rect(fill="white"), 
          axis.text.x = element_text(angle = 45, hjust = 1)))


# PLOT 11: Education per ICD
ed_ICD <- data.frame(table(demo_med_df$`ICD-10-CM Code 1 Description`,
                           demo_med_df$`Highest Education`))
colnames(ed_ICD) <- c("ICD", "Education", "Frequency")

EDicd <-  ed_ICD %>% 
  group_by(ed_ICD$Education, ed_ICD$ICD) %>%
  count()

EDicd[,4:6] <-NULL

ED_ICDmax <- EDicd[EDicd$Frequency == ave(EDicd$Frequency, 
                                      EDicd$Education, FUN=max), ]

ED_ICDmax<-ED_ICDmax[!(ED_ICDmax$Education=="Missing" | 
                 ED_ICDmax$Education=="Don't Know" |
                 ED_ICDmax$Education=="Refused"),]

(p10 <- ggplot(ED_ICDmax, aes(x=ED_ICDmax$Education, y=ED_ICDmax$Frequency,
                              fill=ED_ICDmax$Education,
                              color = ED_ICDmax$ICD,
                              label=ED_ICDmax$Frequency))+
    geom_bar(stat="identity",)+
    scale_fill_discrete(name="Education Level", labels=ED_ICDmax$Education)+
    scale_color_manual(name="ICD Description", labels=unique(ED_ICDmax$ICD),
                       values="Black")+
    xlab("Highest Education") + ylab("Frequency")+
    ggtitle("Most Common ICD-10 Description per Highest Education Level")+
    geom_text(size=3, position = position_stack(vjust = 0.5))+
    theme(panel.background = element_rect(fill="white"), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()))
