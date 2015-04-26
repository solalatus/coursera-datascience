Basic Information
=================

Completion of the homework assingment from "Getting and cleaning data". 
-----------------------------------------------------------------------

Script takes as input the UCI HAR Dataset, tries to work as much as possible from the description .txt files.

Reads the activity labels, reads the feature set, endeavors to load only relevant data columns.

After this is done, merges datastructures, calculates averages and outputs a new dataframe with the average of relevant features per subject per activity.

Output columns:
---------------
* Feature: Measured variable name
* Mean: Mean value
* Label: Activity label given by the original UCI HAR
* Subject ID: identifier of test persons
