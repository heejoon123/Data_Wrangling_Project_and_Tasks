# Loading the data : User can click on the file once folder window opens
demographics <- read.csv(file.choose())
pmeds <- read.csv(file.choose())

# replacing column names
colnames(demographics) <- c("SEQN", "Data Release Cycle", 
                            "Interview and Examination Status",
                            "Gender", "Age at Screening (Years)",
                            "Age at Screening (Months)", 
                            "Recode of Race", 
                            "Recode of Race with Non-Hispanic Asian Category",
                            "Six Month Time Period of Examination",
                            "Age at Examination (Months)",
                            "Served on Active Duty",
                            "Serve in Foreign Country",
                            "Country of Birth", "US Citizen",
                            "Length of Living in US",
                            "Highest degree/level of education",
                            "Highest degree/level of education completed",
                            "Marital status", "Pregnancy Status",
                            "Language of Sample Person Interview",
                            "Proxy Respondent Used for Interview", 
                            "Interpreter used for Interview",
                            "Language of Family Interview",
                            "Proxy Respondent Used for Family Interview",
                            "Interpreter used for Family Interview",
                            "Langauge of MEC CAPI Interview",
                            "Proxy Respondent Used for MEC CAPI Interview",
                            "Interpreter used for MEC CAPI Interview",
                            "Language of MEC ACASI Interview",
                            "Total Number of People in Household",
                            "Total Number of People in Family",
                            "Number of Children 5 years or younger in Household",
                            "Number of Children 6-17 years in Household",
                            "Number of adults 60(+) years in Household",
                            "HH Reference Gender",
                            "HH Reference Age (Years)",
                            "HH Reference Country of Birth",
                            "HH Reference Education Level",
                            "HH Reference Marital Status",
                            "HH Reference Spouse Education Level",
                            "Sample 2 Year Interview Weight",
                            "Sample 2 Year MEC Exam Weight",
                            "Masked Variance Unit pseudo-PSU Variable for 
                            Variance Estimation",
                            "Masked Variance unit pseudo-stratum Variable 
                            for Variance Estimation",
                            "Total Household Income (dollars)",
                            "Total Family Income (dollars)",
                            "Ratio of Family Income to Poverty Guidelines")

colnames(pmeds) <- c("SEQN", "Took Prescription Med. Past Month", "Generic Drug Name",
                     "Generic Drug Code", "Interviewer Saw Medicine Container",
                     "Number of Days Taken Medicine", "ICD-10-CM Code 1",
                     "ICD-10-CM Code 2", "ICD-10-CM Code 3", "ICD-10-CM Code 1 Description",
                     "ICD-10-CM Code 2 Description", "ICD-10-CM Code 3 Description",
                     "Number of Prescription Medicines Taken")

# inner join in R
demo_med_df <- merge(x=demographics, y=pmeds, by="SEQN")

# Removing the column Data Release Cycle Since it is not telling any information
demo_med_df$`Data Release Cycle` <- NULL

# Making a matrix to find the number of NA elements in all columns of merged data
x <- data.frame(matrix(data = NA, nrow=ncol(demo_med_df), ncol = 3))
for ( i in 1:ncol(demo_med_df)){
  x[,1] <- colnames(demo_med_df)
  x[i,2] <- sum(is.na(demo_med_df[i]))
  x[i,3] <- i
}

colnames(x) <- c("Name of Column", "Number of NAs", "Column Index")
head(x)

num_rows <- nrow(pmeds)/2

# First step --> Looking at those with more than half of the dataset with NAs
more2 <- x[which(x[,2] >= num_rows),]
head(more2)

idx <- more2$`Column Index`
is.vector(idx)
# Returns true, so it is a vector
demo_med_df <- demo_med_df[,-idx]

# Get object types 
obj_types <- data.frame(matrix(data=NA, nrow=ncol(demo_med_df), ncol=2))
for(i in 1:ncol(demo_med_df)){
  obj_types[,1] <- colnames(demo_med_df)
  obj_types[i,2] <- class(demo_med_df[,i])
}

## replacing values for interview status
demo_med_df$`Interview and Examination Status`[which(demo_med_df$`Interview and Examination Status` == 1)] <- 'Interview Only'

demo_med_df$`Interview and Examination Status`[which(demo_med_df$`Interview and Examination Status` == 2)] <- 'Interview and Examination'

## Gender 
demo_med_df$Gender[which(demo_med_df$Gender==1)] <- 'Male'
demo_med_df$Gender[which(demo_med_df$Gender==2)] <- 'Female'

## Recode of Race
colnames(demo_med_df)[5] <- "Race/Hispanic Origin"
demo_med_df$`Race/Hispanic Origin`[which(demo_med_df$`Race/Hispanic Origin`==1)] <- 'Mexican American'
demo_med_df$`Race/Hispanic Origin`[which(demo_med_df$`Race/Hispanic Origin`==2)] <- 'Other Hispanic'
demo_med_df$`Race/Hispanic Origin`[which(demo_med_df$`Race/Hispanic Origin`==3)] <- 'Non-Hispanic White'
demo_med_df$`Race/Hispanic Origin`[which(demo_med_df$`Race/Hispanic Origin`==4)] <- 'Non-Hispanic Black'
demo_med_df$`Race/Hispanic Origin`[which(demo_med_df$`Race/Hispanic Origin`==5)] <- 'Other Race'

## 
for (i in 1:nrow(demo_med_df)){
  if(demo_med_df$`Recode of Race with Non-Hispanic Asian Category`[i]==6){
    demo_med_df$`Race/Hispanic Origin`[i] <- "Asian"
  } 
}

colnames(demo_med_df)[5] <- "Race"
demo_med_df[6] <- NULL

demo_med_df$Race[which(demo_med_df$Race=='Non-Hispanic White')] <- sub(pattern="\\Non-Hispanic", 
                                                                         "NH", demo_med_df$Race[which(demo_med_df$Race=='Non-Hispanic White')])
demo_med_df$Race[which(demo_med_df$Race=='Non-Hispanic Black')] <- sub(pattern="\\Non-Hispanic", 
                                                                         "NH", demo_med_df$Race[which(demo_med_df$Race=='Non-Hispanic Black')])
## 6 Month Time Period 
colnames(demo_med_df)[6] <- "Six Month Period"

demo_med_df$`Six Month Period`[which(demo_med_df$`Six Month Period`==1)] <- "November-April"
demo_med_df$`Six Month Period`[which(demo_med_df$`Six Month Period`==2)] <- "May-October"
demo_med_df$`Six Month Period`[which(is.na(demo_med_df$`Six Month Period`))] <- "Missing"

## Active Duty
colnames(demo_med_df)[7] <- "Active Duty"

demo_med_df$`Active Duty`[which(is.na(demo_med_df$`Active Duty`))] <- "Missing"
demo_med_df$`Active Duty`[which(demo_med_df$`Active Duty`=='1')] <- "Yes"
demo_med_df$`Active Duty`[which(demo_med_df$`Active Duty`=='2')] <- "No"
demo_med_df$`Active Duty`[which(demo_med_df$`Active Duty`=='7')] <- "Refused"
demo_med_df$`Active Duty`[which(demo_med_df$`Active Duty`=='9')] <- "Don't Know"


# Country of birth
demo_med_df$`Country of Birth`[which(demo_med_df$`Country of Birth`==1)] <- "US"
demo_med_df$`Country of Birth`[which(demo_med_df$`Country of Birth`==2)] <- "Other"
demo_med_df$`Country of Birth`[which(demo_med_df$`Country of Birth`==77)] <- "Refused"
demo_med_df$`Country of Birth`[which(demo_med_df$`Country of Birth`==99)] <- "Don't Know"

# US Citizen
demo_med_df$`US Citizen`[which(is.na(demo_med_df$`US Citizen`))] <- "Missing"
demo_med_df$`US Citizen`[which(demo_med_df$`US Citizen`==1)] <- "Citizen"
demo_med_df$`US Citizen`[which(demo_med_df$`US Citizen`==2)] <- "Not Citizen"
demo_med_df$`US Citizen`[which(demo_med_df$`US Citizen`==7)] <- "Refused"
demo_med_df$`US Citizen`[which(demo_med_df$`US Citizen`==9)] <- "Don't Know"

# Education Level
colnames(demo_med_df)[10] <- "Highest Education"
demo_med_df$`Highest Education`[which(
  demo_med_df$`Highest Education`==1)] <- "Less than 9th Grade"
demo_med_df$`Highest Education`[which(
  demo_med_df$`Highest Education`==2)] <- "9th-12th (No Diploma)"
demo_med_df$`Highest Education`[which(
  demo_med_df$`Highest Education`==3)] <- "HS Graduate/GED"
demo_med_df$`Highest Education`[which(
  demo_med_df$`Highest Education`==4)] <- "Some College/AA"
demo_med_df$`Highest Education`[which(
  demo_med_df$`Highest Education`==5)] <- "College Grad(+)"
demo_med_df$`Highest Education`[which(demo_med_df$`Highest Education`==7)] <- "Refused"
demo_med_df$`Highest Education`[which(demo_med_df$`Highest Education`==9)] <- "Don't Know"
demo_med_df$`Highest Education`[which(is.na(demo_med_df$`Highest Education`))] <- "Missing"

## Marital Status
demo_med_df$`Marital status`[which(demo_med_df$`Marital status`==1)] <- "Married"
demo_med_df$`Marital status`[which(demo_med_df$`Marital status`==2)] <- "Widowed"
demo_med_df$`Marital status`[which(demo_med_df$`Marital status`==3)] <- "Divorced"
demo_med_df$`Marital status`[which(demo_med_df$`Marital status`==4)] <- "Separated"
demo_med_df$`Marital status`[which(demo_med_df$`Marital status`==5)] <- "Never Married"
demo_med_df$`Marital status`[which(demo_med_df$`Marital status`==6)] <- "Living with Partner"
demo_med_df$`Marital status`[which(demo_med_df$`Marital status`==77)] <- "Refused"
demo_med_df$`Marital status`[which(demo_med_df$`Marital status`==99)] <- "Don't Know"
demo_med_df$`Marital status`[which(is.na(demo_med_df$`Marital status`))] <- "Missing"

## Language of SP Interview
demo_med_df$`Language of Sample Person Interview`[which(
  demo_med_df$`Language of Sample Person Interview`==1)] <- "English"
demo_med_df$`Language of Sample Person Interview`[which(
  demo_med_df$`Language of Sample Person Interview`==2)] <- "Spanish"

# removing some columns not needed
demo_med_df[,c(13:14, 16:21)] <- NULL

# Language of Family Interview
demo_med_df$`Language of Family Interview`[which(
  demo_med_df$`Language of Family Interview`==1)] <- "English"
demo_med_df$`Language of Family Interview`[which(
  demo_med_df$`Language of Family Interview`==2)] <- "Spanish"
demo_med_df$`Language of Family Interview`[which(
  is.na(demo_med_df$`Language of Family Interview`))] <- "Missing"

# Number of children
colnames(demo_med_df)[16] <- "Total Number of Children in Household"
demo_med_df$`Total Number of Children in Household` <- demo_med_df$`Total Number of Children in Household` + 
  demo_med_df$`Number of Children 6-17 years in Household`

# removing some columns not needed
demo_med_df[,17] <- NULL
demo_med_df[,c(18:23)]<- NULL

colnames(demo_med_df)[18:19] <- c("Interview Weight", "MEC Exam Weight")

demo_med_df[,c(20:21)]<- NULL

# Annual Household Income
demo_med_df$`Total Household Income (dollars)` <- as.numeric(
  demo_med_df$`Total Household Income (dollars)`)

demo_med_df$`Total Household Income (dollars)`[which(is.na(
  demo_med_df$`Total Household Income (dollars)`))] <- ceiling(mean(
    demo_med_df$`Total Household Income (dollars)`, na.rm=TRUE))

demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==11)] <- 12

demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==1)] <- "$0 to $4,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==2)] <- "$5,000 to $9,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==3)] <- "$10,000 to $14,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==4)] <- "$15,000 to $19,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==5)] <- "$20,000 to $24,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==6)] <- "$25,000 to $34,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==7)] <- "$35,000 to $44,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==8)] <- "$45,000 to $54,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==9)] <- "$55,000 to $64,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==10)] <- "$65,000 to $74,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==12)] <- "$20,000 and over"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==13)] <- "<$20,000"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==14)] <- "$75,000 to $99,999"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==15)] <- "$100,000 and Over"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==77)] <- "Refused"
demo_med_df$`Total Household Income (dollars)`[which(
  demo_med_df$`Total Household Income (dollars)`==99)] <- "Don't Know"

# Annual Family Income
demo_med_df$`Total Family Income (dollars)` <- as.numeric(
  demo_med_df$`Total Family Income (dollars)`)

demo_med_df$`Total Family Income (dollars)`[which(is.na(
  demo_med_df$`Total Family Income (dollars)`))] <- ceiling(mean(
    demo_med_df$`Total Family Income (dollars)`, na.rm=TRUE))

demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==11)] <- 12

demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==1)] <- "$0 to $4,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==2)] <- "$5,000 to $9,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==3)] <- "$10,000 to $14,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==4)] <- "$15,000 to $19,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==5)] <- "$20,000 to $24,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==6)] <- "$25,000 to $34,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==7)] <- "$35,000 to $44,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==8)] <- "$45,000 to $54,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==9)] <- "$55,000 to $64,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==10)] <- "$65,000 to $74,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==12)] <- "$20,000 and over"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==13)] <- "<$20,000"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==14)] <- "$75,000 to $99,999"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==15)] <- "$100,000 and Over"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==77)] <- "Refused"
demo_med_df$`Total Family Income (dollars)`[which(
  demo_med_df$`Total Family Income (dollars)`==99)] <- "Don't Know"

##Ratio
typeof(demo_med_df$`Ratio of Family Income to Poverty Guidelines`)

demo_med_df$`Ratio of Family Income to Poverty Guidelines`[which(is.na(
  demo_med_df$`Ratio of Family Income to Poverty Guidelines`))] <- round(mean(
    demo_med_df$`Ratio of Family Income to Poverty Guidelines`, na.rm=TRUE), digits = 2)

# Took Prescription medicine past month
demo_med_df$`Took Prescription Med. Past Month`[which(
  demo_med_df$`Took Prescription Med. Past Month`==1)] <- "Yes"
demo_med_df$`Took Prescription Med. Past Month`[which(
  demo_med_df$`Took Prescription Med. Past Month`==2)] <- "No"
demo_med_df$`Took Prescription Med. Past Month`[which(
  demo_med_df$`Took Prescription Med. Past Month`==7)] <- "Refused"
demo_med_df$`Took Prescription Med. Past Month`[which(
  demo_med_df$`Took Prescription Med. Past Month`==9)] <- "Don't Know"

## Generic Drug Name
demo_med_df$`Generic Drug Name` <- sub("^$","Missing", demo_med_df$`Generic Drug Name`)
demo_med_df$`Generic Drug Name` <- sub("55555", "Unknown", demo_med_df$`Generic Drug Name`)
demo_med_df$`Generic Drug Name` <- sub("77777", "Refused", demo_med_df$`Generic Drug Name`)
demo_med_df$`Generic Drug Name` <- sub("99999", "Don't Know", demo_med_df$`Generic Drug Name`)

## Generic Drug Code
demo_med_df$`Generic Drug Code` <- sub("^$","Missing", demo_med_df$`Generic Drug Code`)

demo_med_df[,26] <- NULL

# Number of Days Medicine Taken
demo_med_df$`Number of Days Taken Medicine` <- as.numeric(
  demo_med_df$`Number of Days Taken Medicine`)

demo_med_df$`Number of Days Taken Medicine`[which(is.na(
  demo_med_df$`Number of Days Taken Medicine`))] <- floor(mean(
    demo_med_df$`Number of Days Taken Medicine`, na.rm=TRUE))

demo_med_df[,c(28:29, 31:32)] <- NULL

demo_med_df <- demo_med_df[!(demo_med_df$`Generic Drug Code`=="Missing"),]
demo_med_df$`ICD-10-CM Code 1 Description` <- sub("^$","Missing", 
                                                  demo_med_df$`ICD-10-CM Code 1 Description`)
demo_med_df <- demo_med_df[!(demo_med_df$`ICD-10-CM Code 1 Description`=="Missing"),]

# How to change from all caps to lower case for Drug Names
library(stringr)
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
demo_med_df$`Generic Drug Name` <- tolower(demo_med_df$`Generic Drug Name`)
demo_med_df$`Generic Drug Name` <- sapply(demo_med_df$`Generic Drug Name`, CapStr)
