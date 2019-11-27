/*Name: Heejoon Ahn
Date: October 6, 2019
Assignment 1
*/

/*Rename the following...
TriAge to Age
GenderCode to Gender
ContactID to ID
Address1Stateorprovince to State
TriImagineCareenrollmentemailssentdate to EmailSentdate
Trienrollmentcompletedate to Completedate

Then calculate the time (in days) to complete enrollment and create a new column to have this data
*/
SELECT * FROM [qbs181].hahn.Demographics;

ALTER TABLE [qbs181].hahn.Demographics 
ADD StartDate date;

UPDATE [qbs181].hahn.Demographics
SET StartDate = try_convert(date, tri_imaginecareenrollmentemailsentdate)

ALTER TABLE [qbs181].hahn.Demographics
ADD EndDate date;

UPDATE [qbs181].hahn.Demographics
SET EndDate =try_convert(date, tri_enrollmentcompletedate)

EXEC sp_rename '[qbs181].hahn.Demographics.tri_age', 'Age', 'COLUMN';
EXEC sp_rename '[qbs181].hahn.Demographics.gendercode', 'Gender', 'COLUMN';
EXEC sp_rename '[qbs181].hahn.Demographics.contactid', 'ID', 'COLUMN';
EXEC sp_rename '[qbs181].hahn.Demographics.address1_stateorprovince', 'State', 'COLUMN';
EXEC sp_rename '[qbs181].hahn.Demographics.tri_imaginecareenrollmentemailsentdate', 'EmailSentDate', 'COLUMN';
EXEC sp_rename '[qbs181].hahn.Demographics.tri_enrollmentcompletedate', 'CompleteDate', 'COLUMN';

SELECT * from [qbs181].hahn.Demographics;

SELECT TOP 10 Age, Gender, ID, State, EmailSentDate, CompleteDate,
datediff(day,StartDate,EndDate) as TimeSpent
FROM [qbs181].hahn.Demographics
ORDER BY NEWID();

/*
Problem 2: Create a new column "Enrollment Status"
a) Insert Status = Complete :code is 167410011
b) Insert Status = Email sent :code is 16741001
c) Insert Status = Non responder: Code is 167410004
d) Insert Status = Facilitated Enrollment: Code is 167410005
e) Insert Status = Incomplete Enrollments: Code is 167410002
f) Insert Status = Opted Out: Code is 167410003
g) Insert Status = Unprocessed: Code is 167410000
h) Insert Status = Second email sent: Code is 167410006
*/

SELECT * FROM [qbs181].hahn.Demographics;

SELECT DATA_TYPE 
FROM INFORMATION_SCHEMA.COLUMNS
WHERE 
     TABLE_NAME = 'Demographics' AND 
     COLUMN_NAME = 'tri_imaginecareenrollmentstatus';

ALTER TABLE [qbs181].hahn.Demographics 
ADD Enrollment_Status nvarchar(50);

UPDATE [qbs181].hahn.Demographics
SET Enrollment_Status = 'complete'
WHERE tri_imaginecareenrollmentstatus = 167410011;

UPDATE [qbs181].hahn.Demographics
SET Enrollment_Status = 'Email sent'
WHERE tri_imaginecareenrollmentstatus = 167410001;

UPDATE [qbs181].hahn.Demographics
SET Enrollment_Status = 'Non responder'
WHERE tri_imaginecareenrollmentstatus = 167410004;

UPDATE [qbs181].hahn.Demographics
SET Enrollment_Status = 'Facilitated Enrollment'
WHERE tri_imaginecareenrollmentstatus = 167410005;

UPDATE [qbs181].hahn.Demographics
SET Enrollment_Status = 'Incomplete Enrollments'
WHERE tri_imaginecareenrollmentstatus = 167410002;

UPDATE [qbs181].hahn.Demographics
SET Enrollment_Status = 'Opted Out'
WHERE tri_imaginecareenrollmentstatus = 167410003;

UPDATE [qbs181].hahn.Demographics
SET Enrollment_Status = 'Unprocessed'
WHERE tri_imaginecareenrollmentstatus = 167410000;

UPDATE [qbs181].hahn.Demographics
SET Enrollment_Status = 'Second Email Sent'
WHERE tri_imaginecareenrollmentstatus = 167410006;

SELECT TOP 10 tri_imaginecareenrollmentstatus, Enrollment_Status 
FROM [qbs181].hahn.Demographics
WHERE Enrollment_Status IS NOT NULL
ORDER BY NEWID();

/*
Problem 3: Create a new Column "Sex"
a) Insert sex = female if code = 2
b) Insert sex = male if code = 1
c) Insert sex = other if code = 167410000
d) Insert sex = Unknown if code = 'NULL' 
*/

SELECT * FROM [qbs181].hahn.Demographics

SELECT DATA_TYPE 
FROM INFORMATION_SCHEMA.COLUMNS
WHERE 
     TABLE_NAME = 'Demographics' AND 
     COLUMN_NAME = 'Gender';

ALTER TABLE [qbs181].hahn.Demographics 
ADD Sex nvarchar(50);

UPDATE [qbs181].hahn.Demographics
SET Sex = 'female'
WHERE Gender = '2';

UPDATE [qbs181].hahn.Demographics
SET Sex = 'male'
WHERE Gender = '1';

UPDATE [qbs181].hahn.Demographics
SET Sex = 'other'
WHERE tri_imaginecareenrollmentstatus= 167410000;

UPDATE [qbs181].hahn.Demographics
SET Sex = 'Unknown'
WHERE Sex IS NULL;

SELECT TOP 10 Gender, tri_imaginecareenrollmentstatus, Sex FROM [qbs181].hahn.Demographics
ORDER BY NEWID();

SELECT TOP 10 Gender, tri_imaginecareenrollmentstatus, Sex FROM [qbs181].hahn.Demographics

SELECT COUNT(Sex) FROM [qbs181].hahn.Demographics
WHERE Sex = 'other';

/*
Problem 4: Create a new column "Age group" and create age groups with an interval of 25 years.
For example 0-25 years as '0-25', 26-50 as '26-50' and so on...
*/

SELECT * FROM [qbs181].hahn.Demographics

SELECT DATA_TYPE 
FROM INFORMATION_SCHEMA.COLUMNS
WHERE 
     TABLE_NAME = 'Demographics' AND 
     COLUMN_NAME = 'Age';

ALTER TABLE [qbs181].hahn.Demographics 
ADD Age_Group nvarchar(50);

UPDATE [qbs181].hahn.Demographics
SET Age_Group = '0-25'
WHERE Age <= 25;

UPDATE [qbs181].hahn.Demographics
SET Age_Group = '26-50'
WHERE Age >= 26 AND tri_age <=50;

UPDATE [qbs181].hahn.Demographics
SET Age_Group = '51-75'
WHERE Age >= 51 AND tri_age <=75;

UPDATE [qbs181].hahn.Demographics
SET Age_Group = '76-100'
WHERE Age >= 76 AND tri_age <=100;

UPDATE [qbs181].hahn.Demographics
SET Age_Group = '101-125'
WHERE Age >= 101 AND tri_age <=125;

SELECT TOP 10 Age, Age_Group 
FROM [qbs181].hahn.Demographics
ORDER BY NEWID();
