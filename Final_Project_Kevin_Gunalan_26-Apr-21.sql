/*Creating Database*/
CREATE DATABASE Arrests; 

/* Load the dataset into the table; use SQL to display a few records*/
SELECT * FROM arrests.arrest_data LIMIT 5;

/* Finding highest age */
SELECT 'Descent Code' , MAX(Age) FROM arrests.arrest_data
