# AIRator
Repo for automatic processing of amphetamine-induced rotations data

##Description
This is a repository for the automatic processing of amphetamine-induced rotations data. The repository contains a R Shiny app that can be used to process the data from the rotations test to perform both allocation and analysis. 

The app can be access at https://hdm4xa-alrik-sch0rling.shinyapps.io/airator/

##Notes on the allocation
The anticlustering() function from the anticlust package is used for allocation with the following settings: 
- objective = "kplus"
- categories = lesion
- method = "local-maximum"
- repetitions = 100

This might be changed in a later version of the app.