/*Viewing the the Data for Knowing Columns for Final Exam Tasks*/
SELECT TOP 10 * FROM [hahn].Demographics
SELECT TOP 10 * FROM [hahn].Conditions
SELECT TOP 10 * FROM [hahn].[TextMessages]


/* Question 2: Merge the tables Demographics, Conditions and TextMessages. 
Obtain the final dataset such that we have 1 Row per ID 
by choosing on the latest date when the text was sent (if sent on multiple days)*/

SELECT TOP 10 ID, Age, Sex, tri_name, Max_Date FROM (([hahn].Demographics A
INNER JOIN [hahn].Conditions B 
ON A.ID = B.tri_patientid)
INNER JOIN (
    SELECT tri_contactID, max(TextSentDate) AS Max_Date 
    FROM [hahn].TextMessages
    GROUP BY tri_contactId
)AS C ON A.ID = C.tri_contactId)
ORDER BY NEWID();
