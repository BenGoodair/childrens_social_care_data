# childrens_social_care_data
Welcome to the repository for our NF project cleaning children's social care


## What is made available?
The basic files available to use are:
 - Final cleaned datasets
 - Raw data downloaded from public sources
 - Code to reproduce the harmonisation processes
 - Metadata of variable definitions and links to data sources


## How to download the data?
To download the data follow three steps:
1) Navigate to the ‘Final_data’ folder
2) Select the data of interest - and click ‘view raw’
3) Right click on the web page, and click ‘save as’


## How to use the data?
Once saved, the data can be used in excel, or any software used for data analysis. The data is structured in ‘long format’ where possible - meaning all years, all locations, and all variables are in a long list, and can be filtered to select a specific year, for a specific location, and a specific variable (eg. 2018, Barking and Dagenham, outsourced adult social care expenditure). 


## How to understand the data?
Variable names are labelled mostly as they are in the raw data (and harmonised across years), or created with an intention of them reading intuitively. However, for each, it is not expected that they can be understood by the variable names alone. Consequently, in each data repository, we have produced a metadata document, with variable definitions, so users can search for the information they are interested in and identify which variables are useful. 

The data is reported in the units as per the raw data. Consequently, there are some values which may appear unclear such as ‘c’, ‘z’. These usually refer to data which is suppressed in the raw for being a small value, and consequently, potentially risking identifying the individuals in the dataset. In theory, data should be suppressed with a random element which means it is impossible to impute or understand what that value is - and therefore it is likely best practice to exclude it from your own analyses.

## How to check the data?
Because the data process involves merging millions of observations from hundreds of datasets, it is possible that the code has induced some errors in the values. Consequently, we advise that if you want to be sure of the precise values, that you double check the results against data from the raw data sources.

To do this you have several options - open the metadata, locate the variable you are using, click on the link to the raw data, and download the official datasets to check the values. Or you can download the raw datasets uploaded to the repository themselves. These will not have been edited in any way other than downloaded and saved in csv format - so any errors will also be in the official datasets.

## How to reproduce the data?
The code is built with the intention of the harmonisation being replicable on local machines. Coded in R, any R user should be able to run the master code to produce the data themselves.

The master code pipes functions to clean and merge each dataset (stored in the ‘code’/’functions’ folder of the repository).

## How to ask for help?

Ask me, I'd be so happy to help for any research need! benjamin.goodair@bsg.ox.ac.uk or @BenGoodair 
