# Data_Wrangling_Project_and_Tasks

For QBS181 2019 Fall Term, this repository is to showcase the code from SQL and R for given assignments, presentations, and projects assigned through the term. 

Permission was granted by Dartmouth College and faculty to make this work public. 

## Project 1

Project 1 provided the task of working in SQL and handling the data. One such task was renaming columns permanently, working with dates in SQL, and putting values into a new column with other values for "Enrollment Status" and "Sex". This was mainly to show our understanding of how to handle and clean data using SQL with a SQL Data Studio, which was done on Azure Data Studio for this project. The specific tasks are outlined below.

## Project 2

Project 2 provided the task of working in SQL within Rstudio to handle data further. This required connecting to the SQL server via ODBC in R, and then loading the data into Rstudio as a dataframe. This also required knowledge of how to work with dates, counting values with sqldf() function, and even visualization tools for the data such as ggplot() in R. 

## Project 3

Project 3 was a presentation for a chosen dataset. For my personal project, NHANES data from 2013-2014 was selected for analysis between medication and demographics data provided by kaggle. This involved cleaning the data by handling NA values, renaming columns, generating the right data types or converting to the proper data types, It also involved in calculating the counts and frequencies of certain values. This was a lot of Project 2 with skills in handling with strings, regular expressions in R, dplyr, and more skills. The presentation involved data visualization of the handled and cleaned data. 

## Midterm Project

The Midterm Project involved loading in a public data set and cleaning and handling it extensively. We were tasked with identifying the errors of the data and describing how to handle the issues present with the data. This involved with extensively using Rstudio and the SQL via Rstudio after connecting via ODBC driver. NA values were handled appropriately, converting to the correct data type, and replacing values based on their definitions to clean the data. Proving that the cleaning and handling was appropriately happening was done by counting the number of values after having cleaned the data.

## Final Project

The task of the final project was to load a csv file and merge it with a SQL table, which was done in Rstudio with a connection to the SQL server. This then involved renaming columns and altering the values appropriately via their definitions. That particular task was done using the handling of strings and regular expressions in Rstudio. Merging of tables involved using the SQL commands, and handling of dates was involved to get the 12-week intervals for the participants after merging them. The average scores were calculated via Rstudio using their functions and the dplyr package. Once that was done, data visualization was performed to illustrate the distribution of data throughout the 12-weeks for the scores. Another task was to extenxively use SQL to merge three tables Demographics, Conditions and TextMessages, and obtain the final dataset such that each ID value had the latest (most recent) date of when the text sent was sent. This was then also performed in Rstudio, which involved using tidyverse and dplyr. 


