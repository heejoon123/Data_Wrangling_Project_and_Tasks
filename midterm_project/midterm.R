library("RODBC")
library(dplyr)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)
library(stringr)

# connect to personal server
my_connect <- odbcConnect("HA_Server")

#install.packages("Hmisc")
#install.packages("SASxport")
library(Hmisc)
library(SASxport)
library(foreign)
# <- sasxport.get("C:/Users/winte/Desktop/Homework/Dartmouth QBS/QBS181/DIQ_I.XPT")

# writing this to a csv file
# write.csv(df, file="midterm_data.csv")

df <- read.csv(file = "diq_i.csv")
diq_df <- sqlQuery(my_connect, "SELECT * FROM [qbs181].dbo.diq_i")

library(data.table)
names <-colnames(diq_df)
names <- gsub('[\"]', '', names)
#names
setnames(diq_df, old=colnames(diq_df), new=names)
#colnames(diq_df)

# Determining if there are any rows with only NA values other than the sequence number
num_NA <- which(rowSums(is.na(diq_df[,2:54]))==ncol(diq_df[,2:54]))
num_NA

diq_df[is.na(diq_df)] <- "X"
length(which(diq_df$did040=="X"))
diq_df[sample(nrow(diq_df), 10), 1:5]

# first checking whether there is a NA value in seqn
length(which(diq_df$seqn=="X"))
x <- diq_df$seqn[1:10]
# convert to numeric
diq_df$seqn <- as.numeric(as.character(diq_df$seqn))
# double checking that the number of rows matches the number of rows originally table -> it does!
length(diq_df$seqn)
diq_df[sample(nrow(diq_df), 10), 1:2]

### diq010 ###
diq_df$diq010[diq_df$diq010=='1'] <- 'Yes'
diq_df$diq010[diq_df$diq010=='2'] <- 'No'
diq_df$diq010[diq_df$diq010=='3'] <- 'Borderline'
diq_df$diq010[diq_df$diq010=='7'] <- 'Refused'
diq_df$diq010[diq_df$diq010=='9'] <- 'Dont Know'


sqldf("SELECT COUNT(diq010) FROM diq_df WHERE diq010='Yes' ")
sqldf("SELECT COUNT(diq010) FROM diq_df WHERE diq010='No' ")
sqldf("SELECT COUNT(diq010) FROM diq_df WHERE diq010='Borderline' ")
sqldf("SELECT COUNT(diq010) FROM diq_df WHERE diq010='Refused' ")
sqldf("SELECT COUNT(diq010) FROM diq_df WHERE diq010='Dont Know' ")
sqldf("SELECT COUNT(diq010) FROM diq_df WHERE diq010='X' ")

### did040 ###
diq_df$did040[diq_df$did040=='80'] <- '80 years or older'
diq_df$did040[diq_df$did040=='666'] <- 'Less than 1 year'
diq_df$did040[diq_df$did040=='777'] <- 'Refused'
diq_df$did040[diq_df$did040=='999'] <- 'Dont Know'

(plus80 <- length(diq_df$did040[diq_df$did040=='80 years or older']))
(less1 <- length(diq_df$did040[diq_df$did040=='Less than 1 year']))
(refuse <- length(diq_df$did040[diq_df$did040=='Refused']))
(dk <- length(diq_df$did040[diq_df$did040=='Dont Know']))
(x <- length(diq_df$did040[diq_df$did040=='X']))
(range_age <- nrow(diq_df) - (plus80 + less1 + refuse + dk + x))

### diq160 ###
diq_df$diq160[diq_df$diq160=='1'] <- 'Yes'
diq_df$diq160[diq_df$diq160=='2'] <- 'No'
diq_df$diq160[diq_df$diq160=='7'] <- 'Refused'
diq_df$diq160[diq_df$diq160=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq160) FROM diq_df WHERE diq160='Yes' ")
sqldf("SELECT COUNT(diq160) FROM diq_df WHERE diq160='No' ")
sqldf("SELECT COUNT(diq160) FROM diq_df WHERE diq160='Refused' ")
sqldf("SELECT COUNT(diq160) FROM diq_df WHERE diq160='Dont Know' ")
sqldf("SELECT COUNT(diq160) FROM diq_df WHERE diq160='X' ")

### diq170 ###
diq_df$diq170[diq_df$diq170=='1'] <- 'Yes'
diq_df$diq170[diq_df$diq170=='2'] <- 'No'
diq_df$diq170[diq_df$diq170=='7'] <- 'Refused'
diq_df$diq170[diq_df$diq170=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq170) FROM diq_df WHERE diq170='Yes' ")
sqldf("SELECT COUNT(diq170) FROM diq_df WHERE diq170='No' ")
sqldf("SELECT COUNT(diq170) FROM diq_df WHERE diq170='Refused' ")
sqldf("SELECT COUNT(diq170) FROM diq_df WHERE diq170='Dont Know' ")
sqldf("SELECT COUNT(diq170) FROM diq_df WHERE diq170='X' ")

### diq172 ###
diq_df$diq172[diq_df$diq172=='1'] <- 'Yes'
diq_df$diq172[diq_df$diq172=='2'] <- 'No'
diq_df$diq172[diq_df$diq172=='7'] <- 'Refused'
diq_df$diq172[diq_df$diq172=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq172) FROM diq_df WHERE diq172='Yes' ")
sqldf("SELECT COUNT(diq172) FROM diq_df WHERE diq172='No' ")
sqldf("SELECT COUNT(diq172) FROM diq_df WHERE diq172='Refused' ")
sqldf("SELECT COUNT(diq172) FROM diq_df WHERE diq172='Dont Know' ")
sqldf("SELECT COUNT(diq172) FROM diq_df WHERE diq172='X' ")

### diq175a ###
diq_df$diq175a[diq_df$diq175a=='10'] <- 'risk'
diq_df$diq175a[diq_df$diq175a=='77'] <- 'Refused'
diq_df$diq175a[diq_df$diq175a=='99'] <- 'Dont Know'


sqldf("SELECT COUNT(diq175a) FROM diq_df WHERE diq175a='risk' ")
sqldf("SELECT COUNT(diq175a) FROM diq_df WHERE diq175a='Refused' ")
sqldf("SELECT COUNT(diq175a) FROM diq_df WHERE diq175a='Dont Know' ")
sqldf("SELECT COUNT(diq175a) FROM diq_df WHERE diq175a='X' ")

# look at the 10 random rows of the table
diq_df[sample(nrow(diq_df), 10), 1:7]

### diq175b ###
diq_df$diq175b[diq_df$diq175b=='11'] <- 'risk'

sqldf("SELECT COUNT(diq175b) FROM diq_df WHERE diq175b='risk' ")
sqldf("SELECT COUNT(diq175b) FROM diq_df WHERE diq175b='X' ")

### diq175c ###
diq_df$diq175c[diq_df$diq175c=='12'] <- 'risk'

sqldf("SELECT COUNT(diq175c) FROM diq_df WHERE diq175c='risk' ")
sqldf("SELECT COUNT(diq175c) FROM diq_df WHERE diq175c='X' ")

### diq175d ###
diq_df$diq175d[diq_df$diq175d=='13'] <- 'risk'

sqldf("SELECT COUNT(diq175d) FROM diq_df WHERE diq175d='risk' ")
sqldf("SELECT COUNT(diq175d) FROM diq_df WHERE diq175d='X' ")

### diq175e ###
diq_df$diq175e[diq_df$diq175e=='14'] <- 'Race'

sqldf("SELECT COUNT(diq175e) FROM diq_df WHERE diq175e='risk' ")
sqldf("SELECT COUNT(diq175e) FROM diq_df WHERE diq175e='X' ")

### diq175f ###
diq_df$diq175f[diq_df$diq175f=='15'] <- 'risk'

sqldf("SELECT COUNT(diq175f) FROM diq_df WHERE diq175f='risk' ")
sqldf("SELECT COUNT(diq175f) FROM diq_df WHERE diq175f='X' ")

# look at the 10 random rows of the table
diq_df[sample(nrow(diq_df), 10), c(1,8:12)]

### diq175g ###
diq_df$diq175g[diq_df$diq175g=='16'] <- 'risk'

sqldf("SELECT COUNT(diq175g) FROM diq_df WHERE diq175g='risk' ")
sqldf("SELECT COUNT(diq175g) FROM diq_df WHERE diq175g='X' ")

# look at the 10 random rows of the table
diq_df[sample(nrow(diq_df), 10), c(1,8:12)]

### diq175h ###
diq_df$diq175h[diq_df$diq175h=='17'] <- 'risk'

sqldf("SELECT COUNT(diq175h) FROM diq_df WHERE diq175h='risk' ")
sqldf("SELECT COUNT(diq175h) FROM diq_df WHERE diq175h='X' ")

### diq175i ###
diq_df$diq175i[diq_df$diq175i=='18'] <- 'risk'

sqldf("SELECT COUNT(diq175i) FROM diq_df WHERE diq175i='risk' ")
sqldf("SELECT COUNT(diq175i) FROM diq_df WHERE diq175i='X' ")

# look at the 10 random rows of the table
diq_df[sample(nrow(diq_df), 10), c(1,13:15)]

### diq175j ###
diq_df$diq175j[diq_df$diq175j=='19'] <- 'risk'

sqldf("SELECT COUNT(diq175j) FROM diq_df WHERE diq175j='risk' ")
sqldf("SELECT COUNT(diq175j) FROM diq_df WHERE diq175j='X' ")

### diq175k ###
diq_df$diq175k[diq_df$diq175k=='20'] <- 'risk'

sqldf("SELECT COUNT(diq175k) FROM diq_df WHERE diq175k='risk' ")
sqldf("SELECT COUNT(diq175k) FROM diq_df WHERE diq175k='X' ")

### diq175l ###
diq_df$diq175l[diq_df$diq175l=='21'] <- 'risk'

sqldf("SELECT COUNT(diq175l) FROM diq_df WHERE diq175l='risk' ")
sqldf("SELECT COUNT(diq175l) FROM diq_df WHERE diq175l='X' ")

diq_df[sample(nrow(diq_df), 10), c(1,16:18)]

### diq175m --> tingling/numbness in hands or feet###
diq_df$diq175m[diq_df$diq175m=='22'] <- 'risk'

sqldf("SELECT COUNT(diq175m) FROM diq_df WHERE diq175m='risk' ")
sqldf("SELECT COUNT(diq175m) FROM diq_df WHERE diq175m='X' ")

diq_df[sample(nrow(diq_df), 10), c(1,19)]


### diq175n ###
diq_df$diq175n[diq_df$diq175n=='23'] <- 'risk'

sqldf("SELECT COUNT(diq175n) FROM diq_df WHERE diq175n='risk' ")
sqldf("SELECT COUNT(diq175n) FROM diq_df WHERE diq175n='X' ")

### diq175o ###
diq_df$diq175o[diq_df$diq175o=='24'] <- 'risk'

sqldf("SELECT COUNT(diq175o) FROM diq_df WHERE diq175o='risk' ")
sqldf("SELECT COUNT(diq175o) FROM diq_df WHERE diq175o='X' ")

diq_df[sample(nrow(diq_df), 10), c(1,20:21)]

### diq175p ###
diq_df$diq175p[diq_df$diq175p=='25'] <- 'risk'

sqldf("SELECT COUNT(diq175p) FROM diq_df WHERE diq175p='risk' ")
sqldf("SELECT COUNT(diq175p) FROM diq_df WHERE diq175p='X' ")

### diq175q ###
diq_df$diq175q[diq_df$diq175q=='26'] <- 'risk'

sqldf("SELECT COUNT(diq175q) FROM diq_df WHERE diq175q='risk' ")
sqldf("SELECT COUNT(diq175q) FROM diq_df WHERE diq175q='X' ")

### diq175r ###
diq_df$diq175r[diq_df$diq175r=='27'] <- 'risk'

sqldf("SELECT COUNT(diq175r) FROM diq_df WHERE diq175r='risk' ")
sqldf("SELECT COUNT(diq175r) FROM diq_df WHERE diq175r='X' ")

diq_df[sample(nrow(diq_df), 10), c(1,22:24)]

### diq175s ###
diq_df$diq175s[diq_df$diq175s=='28'] <- 'risk'

sqldf("SELECT COUNT(diq175s) FROM diq_df WHERE diq175s='risk' ")
sqldf("SELECT COUNT(diq175s) FROM diq_df WHERE diq175s='X' ")

### diq175t ###
diq_df$diq175t[diq_df$diq175t=='29'] <- 'risk'

sqldf("SELECT COUNT(diq175t) FROM diq_df WHERE diq175t='risk' ")
sqldf("SELECT COUNT(diq175t) FROM diq_df WHERE diq175t='X' ")

### diq175u ###
diq_df$diq175u[diq_df$diq175u=='30'] <- 'risk'

sqldf("SELECT COUNT(diq175u) FROM diq_df WHERE diq175u= 'risk' ")
sqldf("SELECT COUNT(diq175u) FROM diq_df WHERE diq175u='X' ")


diq_df[sample(nrow(diq_df), 10), c(1,25:27)]
### diq175v ###
diq_df$diq175v[diq_df$diq175v=='31'] <- 'risk'

sqldf("SELECT COUNT(diq175v) FROM diq_df WHERE diq175v='risk' ")
sqldf("SELECT COUNT(diq175v) FROM diq_df WHERE diq175v='X' ")

### diq175w ###
diq_df$diq175w[diq_df$diq175w=='32'] <- 'risk'

sqldf("SELECT COUNT(diq175w) FROM diq_df WHERE diq175w= 'risk' ")
sqldf("SELECT COUNT(diq175w) FROM diq_df WHERE diq175w='X' ")

### diq175x ###
diq_df$diq175x[diq_df$diq175x=='33'] <- 'risk'

sqldf("SELECT COUNT(diq175x) FROM diq_df WHERE diq175x= 'risk' ")
sqldf("SELECT COUNT(diq175x) FROM diq_df WHERE diq175x='X' ")

diq_df[sample(nrow(diq_df), 10), c(1,28:30)]

### diq180 ###
diq_df$diq180[diq_df$diq180=='1'] <- 'Yes'
diq_df$diq180[diq_df$diq180=='2'] <- 'No'
diq_df$diq180[diq_df$diq180=='7'] <- 'Refused'
diq_df$diq180[diq_df$diq180=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq180) FROM diq_df WHERE diq180='Yes' ")
sqldf("SELECT COUNT(diq180) FROM diq_df WHERE diq180='No' ")
sqldf("SELECT COUNT(diq180) FROM diq_df WHERE diq180='Refused' ")
sqldf("SELECT COUNT(diq180) FROM diq_df WHERE diq180='Dont Know' ")
sqldf("SELECT COUNT(diq180) FROM diq_df WHERE diq180='X' ")

### diq050 ###
diq_df$diq050[diq_df$diq050=='1'] <- 'Yes'
diq_df$diq050[diq_df$diq050=='2'] <- 'No'
diq_df$diq050[diq_df$diq050=='7'] <- 'Refused'
diq_df$diq050[diq_df$diq050=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq050) FROM diq_df WHERE diq050='Yes' ")
sqldf("SELECT COUNT(diq050) FROM diq_df WHERE diq050='No' ")
sqldf("SELECT COUNT(diq050) FROM diq_df WHERE diq050='Refused' ")
sqldf("SELECT COUNT(diq050) FROM diq_df WHERE diq050='Dont Know' ")
sqldf("SELECT COUNT(diq050) FROM diq_df WHERE diq050='X' ")

### did060 ###
diq_df$did060[diq_df$did060=='666'] <- 'Less than 1 month'
diq_df$did060[diq_df$did060=='777'] <- 'Refused'
diq_df$did060[diq_df$did060=='999'] <- 'Dont Know'

(less1month <- length(diq_df$did060[diq_df$did060=='Less than 1 month']))
(refuse060 <- length(diq_df$did060[diq_df$did060=='Refused']))
(dk060 <- length(diq_df$did060[diq_df$did060=='Dont Know']))
(x060 <- length(diq_df$did060[diq_df$did060=='X']))
(range_age <- nrow(diq_df) - (less1month + refuse060 + dk060 + x060))

### diq060 ###
diq_df$diq060u[diq_df$diq060u=='1'] <- 'Months'
diq_df$diq060u[diq_df$diq060u=='2'] <- 'Years'

sqldf("SELECT COUNT(diq060u) FROM diq_df WHERE diq060u='Months' ")
sqldf("SELECT COUNT(diq060u) FROM diq_df WHERE diq060u='Years' ")
sqldf("SELECT COUNT(diq060u) FROM diq_df WHERE diq060u='X' ")

### diq070 ###
diq_df$diq070[diq_df$diq070=='1'] <- 'Yes'
diq_df$diq070[diq_df$diq070=='2'] <- 'No'
diq_df$diq070[diq_df$diq070=='7'] <- 'Refused'
diq_df$diq070[diq_df$diq070=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq070) FROM diq_df WHERE diq070='Yes' ")
sqldf("SELECT COUNT(diq070) FROM diq_df WHERE diq070='No' ")
sqldf("SELECT COUNT(diq070) FROM diq_df WHERE diq070='Refused' ")
sqldf("SELECT COUNT(diq070) FROM diq_df WHERE diq070='Dont Know' ")
sqldf("SELECT COUNT(diq070) FROM diq_df WHERE diq070='X' ")

diq_df[sample(nrow(diq_df), 10), c(1,31:35)]
### diq230 ###
diq_df$diq230[diq_df$diq230=='1'] <- '1 Year ago or less'
diq_df$diq230[diq_df$diq230=='2'] <- 'More than 1 year ago - 2 years ago'
diq_df$diq230[diq_df$diq230=='3'] <- 'More than 2 years ago - 5 years ago'
diq_df$diq230[diq_df$diq230=='4'] <- 'More than 5 years ago'
diq_df$diq230[diq_df$diq230=='5'] <- 'Never'
diq_df$diq230[diq_df$diq230=='7'] <- 'Refused'
diq_df$diq230[diq_df$diq230=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq230) FROM diq_df WHERE diq230='1 Year ago or less' ")
sqldf("SELECT COUNT(diq230) FROM diq_df WHERE diq230='More than 1 year ago - 2 years ago' ")
sqldf("SELECT COUNT(diq230) FROM diq_df WHERE diq230='More than 2 years ago - 5 years ago' ")
sqldf("SELECT COUNT(diq230) FROM diq_df WHERE diq230='More than 5 years ago' ")
sqldf("SELECT COUNT(diq230) FROM diq_df WHERE diq230='Never' ")
sqldf("SELECT COUNT(diq230) FROM diq_df WHERE diq230='Refused' ")
sqldf("SELECT COUNT(diq230) FROM diq_df WHERE diq230='Dont Know' ")
sqldf("SELECT COUNT(diq230) FROM diq_df WHERE diq230='X' ")

### diq240 ###
diq_df$diq240[diq_df$diq240=='1'] <- 'Yes'
diq_df$diq240[diq_df$diq240=='2'] <- 'No'
diq_df$diq240[diq_df$diq240=='7'] <- 'Refused'
diq_df$diq240[diq_df$diq240=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq240) FROM diq_df WHERE diq240='Yes' ")
sqldf("SELECT COUNT(diq240) FROM diq_df WHERE diq240='No' ")
sqldf("SELECT COUNT(diq240) FROM diq_df WHERE diq240='Refused' ")
sqldf("SELECT COUNT(diq240) FROM diq_df WHERE diq240='Dont Know' ")
sqldf("SELECT COUNT(diq240) FROM diq_df WHERE diq240='X' ")

### did250 ###
diq_df$did250[diq_df$did250=='0'] <- 'None'
diq_df$did250[diq_df$did250=='7777'] <- 'Refused'
diq_df$did250[diq_df$did250=='9999'] <- 'Dont Know'

(none <- length(diq_df$did250[diq_df$did250=='None']))
(refuse250 <- length(diq_df$did250[diq_df$did250=='Refused']))
(dk250 <- length(diq_df$did250[diq_df$did250=='Dont Know']))
(x250 <- length(diq_df$did250[diq_df$did250=='X']))
(range_age <- nrow(diq_df) - (none + refuse250 + dk250 + x250))

diq_df[sample(nrow(diq_df), 10), c(1,36:39)]

### did260 ###
diq_df$did260[diq_df$did260=='0'] <- 'Never'
diq_df$did260[diq_df$did260=='777'] <- 'Refused'
diq_df$did260[diq_df$did260=='999'] <- 'Dont Know'

(never <- length(diq_df$did260[diq_df$did260=='Never']))
(refuse260 <- length(diq_df$did260[diq_df$did260=='Refused']))
(dk260 <- length(diq_df$did260[diq_df$did260=='Dont Know']))
(x260 <- length(diq_df$did260[diq_df$did260=='X']))
(range_age <- nrow(diq_df) - (never + refuse260 + dk260 + x260))

diq_df[sample(nrow(diq_df), 10), c(1,36:39)]

### diq260u ###
diq_df$diq260u[diq_df$diq260u=='1'] <- 'Per day'
diq_df$diq260u[diq_df$diq260u=='2'] <- 'Per week'
diq_df$diq260u[diq_df$diq260u=='3'] <- 'Per month'
diq_df$diq260u[diq_df$diq260u=='4'] <- 'Per year'

sqldf("SELECT COUNT(diq260u) FROM diq_df WHERE diq260u='Per day' ")
sqldf("SELECT COUNT(diq260u) FROM diq_df WHERE diq260u='Per week' ")
sqldf("SELECT COUNT(diq260u) FROM diq_df WHERE diq260u='Per month' ")
sqldf("SELECT COUNT(diq260u) FROM diq_df WHERE diq260u='Per year' ")
sqldf("SELECT COUNT(diq260u) FROM diq_df WHERE diq260u='X' ")

### diq275 ###
diq_df$diq275[diq_df$diq275=='1'] <- 'Yes'
diq_df$diq275[diq_df$diq275=='2'] <- 'No'
diq_df$diq275[diq_df$diq275=='7'] <- 'Refused'
diq_df$diq275[diq_df$diq275=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq275) FROM diq_df WHERE diq275='Yes' ")
sqldf("SELECT COUNT(diq275) FROM diq_df WHERE diq275='No' ")
sqldf("SELECT COUNT(diq275) FROM diq_df WHERE diq275='Refused' ")
sqldf("SELECT COUNT(diq275) FROM diq_df WHERE diq275='Dont Know' ")
sqldf("SELECT COUNT(diq275) FROM diq_df WHERE diq275='X' ")

### diq280 ###
diq_df$diq280[diq_df$diq280=='777'] <- 'Refused'
diq_df$diq280[diq_df$diq280=='999'] <- 'Dont Know'

(refuse280 <- length(diq_df$diq280[diq_df$diq280=='Refused']))
(dk280 <- length(diq_df$diq280[diq_df$diq280=='Dont Know']))
(x280 <- length(diq_df$diq280[diq_df$diq280=='X']))
(range_age <- nrow(diq_df) - (refuse280 + dk280 + x280))

### diq291 ###
diq_df$diq291[diq_df$diq291=='1'] <- '<6'
diq_df$diq291[diq_df$diq291=='2'] <- '<7'
diq_df$diq291[diq_df$diq291=='3'] <- '<8'
diq_df$diq291[diq_df$diq291=='4'] <- '<9'
diq_df$diq291[diq_df$diq291=='5'] <- '<10'
diq_df$diq291[diq_df$diq291=='6'] <- 'Provider did not specify goal'
diq_df$diq291[diq_df$diq291=='77'] <- 'Refused'
diq_df$diq291[diq_df$diq291=='99'] <- 'Dont Know'

sqldf("SELECT COUNT(diq291) FROM diq_df WHERE diq291='<6' ")
sqldf("SELECT COUNT(diq291) FROM diq_df WHERE diq291='<7' ")
sqldf("SELECT COUNT(diq291) FROM diq_df WHERE diq291='<8' ")
sqldf("SELECT COUNT(diq291) FROM diq_df WHERE diq291='<9' ")
sqldf("SELECT COUNT(diq291) FROM diq_df WHERE diq291='<10' ")
sqldf("SELECT COUNT(diq291) FROM diq_df WHERE diq291='Provider did not specify goal' ")
sqldf("SELECT COUNT(diq291) FROM diq_df WHERE diq291='Refused' ")
sqldf("SELECT COUNT(diq291) FROM diq_df WHERE diq291='Dont Know' ")
sqldf("SELECT COUNT(diq291) FROM diq_df WHERE diq291='X' ")

diq_df[sample(nrow(diq_df), 10), c(1,40:43)]

### diq300s ###
diq_df$diq300s[diq_df$diq300s=='7777'] <- 'Refused'
diq_df$diq300s[diq_df$diq300s=='9999'] <- 'Dont Know'

(refuse300s <- length(diq_df$diq300s[diq_df$diq300s=='Refused']))
(dk300s <- length(diq_df$diq300s[diq_df$diq300s=='Dont Know']))
(x300s <- length(diq_df$diq300s[diq_df$diq300s=='X']))
(range_age <- nrow(diq_df) - (refuse300s + dk300s + x300s))

### diq300d ###
diq_df$diq300d[diq_df$diq300d=='7777'] <- 'Refused'
diq_df$diq300d[diq_df$diq300d=='9999'] <- 'Dont Know'

(refuse300d <- length(diq_df$diq300d[diq_df$diq300d=='Refused']))
(dk300d <- length(diq_df$diq300d[diq_df$diq300d=='Dont Know']))
(x300d <- length(diq_df$diq300d[diq_df$diq300d=='X']))
(range_age <- nrow(diq_df) - (refuse300d + dk300d + x300d))

### diq310s ###
diq_df$did310s[diq_df$did310s=='6666'] <- 'Provider did not specify goal'
diq_df$did310s[diq_df$did310s=='7777'] <- 'Refused'
diq_df$did310s[diq_df$did310s=='9999'] <- 'Dont Know'

(nogoal <- length(diq_df$did310s[diq_df$did310s=='Provider did not specify goal']))
(refuse310s <- length(diq_df$did310s[diq_df$did310s=='Refused']))
(dk310s <- length(diq_df$did310s[diq_df$did310s=='Dont Know']))
(x310s <- length(diq_df$did310s[diq_df$did310s=='X']))
(range_age <- nrow(diq_df) - (nogoal + refuse310s + dk310s + x310s))

diq_df[sample(nrow(diq_df), 10), c(1,44:46)]

### diq310d ###
diq_df$did310d[diq_df$did310d=='6666'] <- 'Provider did not specify goal'
diq_df$did310d[diq_df$did310d=='7777'] <- 'Refused'
diq_df$did310d[diq_df$did310d=='9999'] <- 'Dont Know'

(nogoal <- length(diq_df$did310s[diq_df$did310s=='Provider did not specify goal']))
(refuse <- length(diq_df$did310d[diq_df$did310d=='Refused']))
(dk <- length(diq_df$did310d[diq_df$did310d=='Dont Know']))
(x <- length(diq_df$did310d[diq_df$did310d=='X']))
(range_age <- nrow(diq_df) - (nogoal + refuse + dk + x))

### diq320 ###
diq_df$did320[diq_df$did320=='5555'] <- 'Never heard of LDL'
diq_df$did320[diq_df$did320=='6666'] <- 'Never had cholesterol test'
diq_df$did320[diq_df$did320=='7777'] <- 'Refused'
diq_df$did320[diq_df$did320=='9999'] <- 'Dont Know'

(noldl <- length(diq_df$did320[diq_df$did320=='Never heard of LDL']))
(notest <- length(diq_df$did320[diq_df$did320=='Never had cholesterol test']))
(refuse <- length(diq_df$did320[diq_df$did320=='Refused']))
(dk <- length(diq_df$did320[diq_df$did320=='Dont Know']))
(x <- length(diq_df$did320[diq_df$did320=='X']))
(range_age <- nrow(diq_df) - (noldl + notest + refuse + dk + x))

diq_df[sample(nrow(diq_df), 10), c(1,47:48)]

### diq330 ###
diq_df$did330[diq_df$did330=='6666'] <- 'Provider did not specify goal'
diq_df$did330[diq_df$did330=='7777'] <- 'Refused'
diq_df$did330[diq_df$did330=='9999'] <- 'Dont Know'

(nogoal <- length(diq_df$did330[diq_df$did330=='Provider did not specify goal']))
(refuse <- length(diq_df$did330[diq_df$did330=='Refused']))
(dk <- length(diq_df$did330[diq_df$did330=='Dont Know']))
(x <- length(diq_df$did330[diq_df$did330=='X']))
(range_age <- nrow(diq_df) - (nogoal+ refuse + dk + x))

### diq341 ###
diq_df$did341[diq_df$did341=='0'] <- 'none'
diq_df$did341[diq_df$did341=='7777'] <- 'Refused'
diq_df$did341[diq_df$did341=='9999'] <- 'Dont Know'

(none <- length(diq_df$did341[diq_df$did341=='none']))
(refuse <- length(diq_df$did341[diq_df$did341=='Refused']))
(dk <- length(diq_df$did341[diq_df$did341=='Dont Know']))
(x <- length(diq_df$did341[diq_df$did341=='X']))
(range_age <- nrow(diq_df) - (none+ refuse + dk + x))

### diq350 ###
diq_df$did350[diq_df$did350=='0'] <- 'none'
diq_df$did350[diq_df$did350=='7777'] <- 'Refused'
diq_df$did350[diq_df$did350=='9999'] <- 'Dont Know'

(none <- length(diq_df$did350[diq_df$did350=='none']))
(refuse <- length(diq_df$did350[diq_df$did350=='Refused']))
(dk <- length(diq_df$did350[diq_df$did350=='Dont Know']))
(x <- length(diq_df$did350[diq_df$did350=='X']))
(range_age <- nrow(diq_df) - (none+ refuse + dk + x))

diq_df[sample(nrow(diq_df), 10), c(1,49:51)]

### diq350u ###
diq_df$diq350u[diq_df$diq350u=='1'] <- 'Per day'
diq_df$diq350u[diq_df$diq350u=='2'] <- 'Per week'
diq_df$diq350u[diq_df$diq350u=='3'] <- 'Per month'
diq_df$diq350u[diq_df$diq350u=='4'] <- 'Per year'

sqldf("SELECT COUNT(diq350u) FROM diq_df WHERE diq350u='Per day' ")
sqldf("SELECT COUNT(diq350u) FROM diq_df WHERE diq350u='Per week' ")
sqldf("SELECT COUNT(diq350u) FROM diq_df WHERE diq350u='Per month' ")
sqldf("SELECT COUNT(diq350u) FROM diq_df WHERE diq350u='Per year' ")
sqldf("SELECT COUNT(diq350u) FROM diq_df WHERE diq350u='X' ")

### diq360 ###
diq_df$diq360[diq_df$diq360=='1'] <- '<1 month'
diq_df$diq360[diq_df$diq360=='2'] <- '1-12 months'
diq_df$diq360[diq_df$diq360=='3'] <- '13-24 months'
diq_df$diq360[diq_df$diq360=='4'] <- '>2 years'
diq_df$diq360[diq_df$diq360=='5'] <- 'never'
diq_df$diq360[diq_df$diq360=='7'] <- 'Refused'
diq_df$diq360[diq_df$diq360=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq360) FROM diq_df WHERE diq360='<1 month' ")
sqldf("SELECT COUNT(diq360) FROM diq_df WHERE diq360='1-12 months' ")
sqldf("SELECT COUNT(diq360) FROM diq_df WHERE diq360='13-24 months' ")
sqldf("SELECT COUNT(diq360) FROM diq_df WHERE diq360='>2 years' ")
sqldf("SELECT COUNT(diq360) FROM diq_df WHERE diq360='never' ")
sqldf("SELECT COUNT(diq360) FROM diq_df WHERE diq360='Refused' ")
sqldf("SELECT COUNT(diq360) FROM diq_df WHERE diq360='Dont Know' ")
sqldf("SELECT COUNT(diq360) FROM diq_df WHERE diq360='X' ")

### diq080 ###
diq_df$diq080[diq_df$diq080=='1'] <- 'Yes'
diq_df$diq080[diq_df$diq080=='2'] <- 'No'
diq_df$diq080[diq_df$diq080=='7'] <- 'Refused'
diq_df$diq080[diq_df$diq080=='9'] <- 'Dont Know'

sqldf("SELECT COUNT(diq080) FROM diq_df WHERE diq080='Yes' ")
sqldf("SELECT COUNT(diq080) FROM diq_df WHERE diq080='No' ")
sqldf("SELECT COUNT(diq080) FROM diq_df WHERE diq080='Refused' ")
sqldf("SELECT COUNT(diq080) FROM diq_df WHERE diq080='Dont Know' ")
sqldf("SELECT COUNT(diq080) FROM diq_df WHERE diq080='X' ")

diq_df[sample(nrow(diq_df), 10), c(1,53:54)]
```

The counts of each values were printed out in the code chunk above when cleaning the data, to ensure that the data still retains the same amount of rows and not missing any data that it should be keeping.

For personal preference, though the dictionary is helpful, the column names were changed to make it easier to understand what each column indicated.
```{r replace column names}
newColNames <- c("Sequence Number", "Doctor diagnosed diabetes",
                 "Age when first diagnosed diabetes",
                 "Told to have prediabetes",
                 "Told to have health risk for diabetes",
                 "Feel could be at risk for diabetes",
                 "Family History","Overweight", "Age", "Poor diet",
                 "Race", "Had a baby weighed over 9 lbs at birth",
                 "Lack of Physical Activity", "High blood pressure",
                 "High blood sugar", "High cholesterol", 
                 "Hypoglycemic", "Extreme hunger",
                 "Tingling/numbness in hands or feet", "Blurred vision",
                 "Increased fatigue", "Anyone could be at risk",
                 "Doctor Warning", "Other Risk, specify", 
                 "Gestational Diabetes", "Frequent urination", "Thirst",
                 "Craving for sweet/eating a lot of sugar", 
                 "Medication", "Polycistic ovarian syndrome", 
                 "Had blood tested past three years", "Taking Insulin now",
                 "How long taking insulin", "Unit of measure (month/year)",
                 "Take diabetic pills to lower blood sugar",
                 "How long ago saw a diabetes specialist",
                 "Is there one doctor you see for diabetes",
                 "Past year how many times seen doctor",
                 "How often check blood for glucose/sugar",
                 "Unit of measure (day/week/month/year)", 
                 "Past year Dr checked for A1C",
                 "Last A1C Level", "What Dr say A1C should be", 
                 "Recent SBP", "Recent DBP", "What Dr say SBP should be",
                 "What Dr say DBP should be", "Most Recent LDL",
                 "What Dr say LDL should be", 
                 "Past year times Dr check feet for sores",
                 "How often feet checked personally",
                 "Unit of measure 2 (day/week/month/year)",
                 "Last time pupis dilated for exam", 
                 "Diabetes affected eyes/had retinopathy")
#length(newColNames)

colnames(diq_df) <- newColNames

# to show that the column names have been properly changed
diq_df[sample(nrow(diq_df), 10), c(1:2)]

# DIQ_data <- diq_df
# sqlSave(my_connect, DIQ_data, rownames=FALSE)
# write.csv(diq_df, file="hahn_diq_clean.csv")
