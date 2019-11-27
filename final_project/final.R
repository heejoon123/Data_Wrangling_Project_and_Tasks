library("RODBC")
library(dplyr)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyverse)

# Connecting to SQL Server for Analysis in R Question 1 and 3
my_connect <- odbcConnect("qbs181",uid = "hahn", pwd = "hahn@qbs181")

# Reading the ICBP data downloaded to a csv file in R as a dataframe
ICBP.df <- read.csv(file="IC_BP_v2.csv", header=TRUE)

# Renaming "BPAlert" to "BP Status"
colnames(ICBP.df)[4] <- "BP Status"

# Appropriately changing the values based on following definitions:
# Hypo1 and Normal are "Controlled Blood Pressure" = 1
# Hypo2, HTN1, HTN2, HTN3 are "Uncontrolled Blood Pressure" = 0
ICBP.df$`BP Status` <- sub(pattern="\\Hypo1|Normal", "Controlled BP", 
                           ICBP.df$`BP Status`)
ICBP.df$`BP Status` <- sub(pattern="\\Hypo2|HTN1|HTN2|HTN3", "Uncontrolled BP",
                           ICBP.df$`BP Status`)

ICBP.df$`BP Status Dichotomous` <- sub(pattern="\\Controlled BP", 1, 
                                       ICBP.df$`BP Status`)
ICBP.df$`BP Status Dichotomous`<- sub(pattern="\\Uncontrolled BP", 0,
                                      ICBP.df$`BP Status Dichotomous`)

# Get the Demographics table from server
demo_df <- sqlQuery(my_connect, "SELECT * FROM [qbs181].[hahn].Demographics")

# Merge both demographics and ICBP
BP_demo.df <- sqldf("SELECT A.*, B.StartDate, B.EndDate FROM `ICBP.df` A
                    INNER JOIN demo_df B
                    ON A.ID = B.ID")

BP_demo.df[sample(nrow(BP_demo.df), 10), c(1,6:8)]

# Converting the Start and End Dates of Enrollment into Date Types
class(BP_demo.df$StartDate)
class(BP_demo.df$EndDate)

BP_demo.df$StartDate <- as.Date(BP_demo.df$StartDate, "%Y-%m-%d")
BP_demo.df$EndDate <- as.Date(BP_demo.df$EndDate, "%Y-%m-%d")

# Calculating the differences of the End and Start Dates by Week with integer and 
# non-integer types for a total of 12-week interval
BP_dates_summary <- BP_demo.df %>%
  group_by(ID) %>%
  summarise(Week = as.integer(ceiling(difftime(max(EndDate),
                                                min(StartDate),
                                                units="weeks"))),
            Weeks = as.numeric(difftime(max(EndDate), min(StartDate),
                                        units="weeks")))

BP_dates_summary$Week[which(BP_dates_summary$Week >= 12)] <- 12
BP_dates_summary$Weeks[which(BP_dates_summary$Weeks >= 12)] <- 12

# Calculating the Averages of each row for Systolic and Diastolic Values
BP_demo.df$Avg_Score <- rowMeans(BP_demo.df[c('SystolicValue',
                                              'Diastolicvalue')], 
                                              na.rm=TRUE)

BP_Demo_summary <- BP_dates_summary %>% inner_join(BP_demo.df, by="ID")

# Grouping them together by ID and Week values and perhaps Dichotomous BP status
BP_D2 <- BP_Demo_summary %>%
  group_by(ID, Week, Weeks) %>%
  mutate(Avg_Weeks = mean(Avg_Score))

BP_D3 <- BP_D2 %>%
  group_by(ID, Week, Weeks,`BP Status Dichotomous`) %>%
  summarise(Avg_Weeks = mean(Avg_Score))

BP_D4 <- BP_Demo_summary %>%
  group_by(ID, Week, Weeks) %>%
  summarise(Avg_Weeks = mean(Avg_Score))

# The NA omit is for visualization purposes
BP_D4 <- na.omit(BP_D4)

BP_D3[sample(nrow(BP_D3), 10),]
BP_D4[sample(nrow(BP_D4), 10),]

# Plotting
ggplot(BP_D4, aes(x=Weeks, y=Avg_Weeks))+
  geom_point()+
  ggtitle("Average Scores per Week")+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Week")+
  ylab("Average Score")

ggplot(BP_D4, aes(x=as.factor(Week), y=Avg_Weeks, fill=as.factor(Week)))+
  geom_boxplot()+
  ggtitle("Average Scores per Week")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Average Score")+
  xlab("Week")+
  scale_fill_brewer(palette = "Set3")

ggplot(BP_D4, aes(x=as.factor(Week), y=Avg_Weeks, fill=as.factor(Week)))+
  geom_violin()+
  stat_summary(fun.y= mean, geom="point", size=2, color="red")+
  ggtitle("Average Scores per Week")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Average Score")+
  xlab("Week")+
  scale_fill_brewer(palette = "Set3")

# Counting how many have gone from 0 to 1 for BP status
num_changes <- sum(head(BP_D3$`BP Status Dichotomous`, 
                        -1)==0 & tail(BP_D3$`BP Status Dichotomous`,-1)==1)
(num_changes)

# Merge and group by ID for those with the latest text sent date
Conditions_df <- sqlQuery(my_connect,
                          "SELECT * FROM [qbs181].[hahn].Conditions")
colnames(Conditions_df)[1] <- "ID"

TM.df <- sqlQuery(my_connect,
                  "SELECT * FROM [qbs181].[hahn].TextMessages")
colnames(TM.df)[1] <- "ID"

merged <- list(demo_df, Conditions_df, TM.df) %>%
  reduce(inner_join, by="ID")

class(merged$TextSentDate)

merged$TextSentDate <- as.Date(merged$TextSentDate, "%m/%d/%y")

merged2 <- merged %>%
  group_by(ID) %>%
  mutate(Max_Date = max(TextSentDate))

Merged_Final <- merged %>%
  group_by(ID) %>%
  summarise(Max_Date = max(TextSentDate))

Merged_Final[sample(nrow(Merged_Final), 10),]
