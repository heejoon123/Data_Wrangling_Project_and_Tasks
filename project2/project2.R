library("RODBC")
library(dplyr)
library(ggplot2)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)
library(stringr)

# connect to database for SQL
my_conn <- odbcConnect("qbs181", "hahn", "hahn@qbs181")

# load tables into R as dataframes
phonecall_df <- sqlQuery(my_conn, "SELECT * FROM [qbs181].[hahn].PhoneCall")
call_duration <- sqlQuery(my_conn, "SELECT * FROM [qbs181].[hahn].CallDuration")
pc_encounter <- sqlQuery(my_conn, "SELECT * FROM [qbs181].[hahn].PhoneCall_Encounter")
demo_df <- sqlQuery(my_conn, "SELECT * FROM [qbs181].[hahn].Demographics")
cond_df <- sqlQuery(my_conn, "SELECT * FROM [qbs181].[hahn].Conditions")
tm_df <- sqlQuery(my_conn, "SELECT * FROM [qbs181].[hahn].TextMessages")

# Adding a new column to the newly merged table
pc_encounter$`Enrollment Group` <- NA
# putting the coded values into the table
pc_encounter$`Enrollment Group`<-ifelse(pc_encounter$EncounterCode == 125060000,
"Clinical Alert", ifelse(pc_encounter$EncounterCode==125060001,
"Health Coaching", ifelse(pc_encounter$EncounterCode==125060002, 
"Technical Question", ifelse(pc_encounter$EncounterCode==125060003, 
"Administrative", ifelse(pc_encounter$EncounterCode==125060004, 
"Other", "Lack of Engagement")))))

# Getting the 10 random rows
pc_encounter[sample(nrow(pc_encounter),10),]

# counting
count_groups <- sqldf("SELECT DISTINCT `Enrollment Group`, 
                      COUNT(*) FROM pc_encounter GROUP BY `Enrollment Group`")
count_groups

# Merge tables
encounter_call <- sqldf("select A.*, B.* 
                   from pc_encounter A 
                   inner join call_duration B 
                   on A.CustomerId = B.tri_CustomerIDEntityReference")
encounter_call[sample(nrow(encounter_call),10),]

# counting
count_calltypes <- sqldf("SELECT DISTINCT CallType,CallOutcome,COUNT(*) 
                         FROM encounter_call 
                         GROUP BY CallType, CallOutcome")
count_calltypes

duration_eg <- sqldf("SELECT DISTINCT `Enrollment Group`, SUM(CallDuration)
                     FROM encounter_call
                     GROUP BY `Enrollment GROUP`")
duration_eg

# Merging the tables
dctm_df <- sqldf("SELECT D.*, C.*, T.* 
                 FROM demo_df D 
                 INNER JOIN cond_df C 
                 ON D.ID = C.tri_patientid 
                 INNER JOIN tm_df T 
                 ON T.tri_contactid=D.ID")
#dctm_df[sample(nrow(dctm_df), 10),]

# Find number of texts/week, by the type of sender
dctm_df$TextSentDate <- as.Date(dctm_df$TextSentDate, format="%m/%d/%y")

dctm_df$Week <- strftime(dctm_df$TextSentDate, format = "%V")

tm_week <- sqldf("SELECT DISTINCT SenderName, Week, COUNT(*)
                 FROM dctm_df
                 GROUP BY SenderName, Week")
tm_week[sample(nrow(tm_week),10),]

# draw a visual using 
names(tm_week)[3] <- "Number of Texts"

ggplot(tm_week, aes(fill=tm_week$SenderName,
                    y=tm_week$`Number of Texts`,
                    x=tm_week$Week))+
  geom_bar(position="dodge", stat="identity")+
  xlab("Week")+
  ylab("Number of Texts")+
  ggtitle("Total Number of Texts Sent")+
  labs(fill="Sender Name")+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())
        
# Find number of texts/week, by the type chronic condition
tm_condition <- sqldf("SELECT DISTINCT tri_name, Week, COUNT(*)
                      FROM dctm_df
                      GROUP BY tri_name, Week")

tm_condition[sample(nrow(tm_condition),10),]

# draw a visual using 
names(tm_condition)[3] <- "Number of Texts"
names(tm_condition)[1] <- "Chronic Condition"

ggplot(tm_condition, aes(fill=tm_condition$`Chronic Condition`,
                    y=tm_condition$`Number of Texts`,
                    x=tm_condition$Week))+
  geom_bar(position="dodge", stat="identity")+
  xlab("Week")+
  ylab("Number of Texts")+
  ggtitle("Total Number of Texts Sent based on Condition")+
  labs(fill="Chronic Condition")+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())






