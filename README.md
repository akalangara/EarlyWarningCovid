# Project Description

For my masters thesis, I analyzed publicly available data from 5 sources including the CDC COVID Data Tracker and GISAID Data.

Utilizing these data, I assessed whether Delta variant proportion data could act as an early indicator for COVID-19 case surges. The analysis accounted for state vaccination rates, seroprevalence, and age distribution. The potential for a reporting lag was also taken into account using distinct models for no lag, 2 week lag, and 4 week lag.

Analysis began with data ingestion, cleaning, transformation, and exploratory data analysis and visualization. Each of the data sets were merged, and the percent change in incidence and Delta variant proportion per 2 weeks were determined for each of the noted jurisdictions. The final dataset was modeled using generalized estimating equations with autoregressive correlation matrix to reflect the data structure.

Conclusions suggest monitoring changes in COVID-19 variant proportion data can act as a leading indicator of COVID-19 case incidence surges.

# Project File Information

The project uses the `here` package which requires the same folder structure as shown in this repository. The `Exploratory_Analysis` directory is empty but will be the directory in which files and graphs generated by exploratory analysis are deposited. The `Figures` folder contains an image used in a file, and the `Processed_Data` directory will contain cleaned and intermediary datasets in the analysis process. The `R` directory contains the scripts for analysis and the `Raw_Data` directory** contains raw datasets to be used in analysis.

The analysis is conducted in steps utilizing separate R scripts located in the `R` directory. Prior to running each of the scripts, set the directory to file location. This will ensure that the `here` package works properly. Analysis can be done by running the scripts in the order of number. As noted, scripts will generate files in the `Exploratory_Analysis` and `Processed_Data` directories which are currently empty.

**Note: Due to file size, `COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv` is not present in the `Raw_Data` directory. 
This data can be downloaded [here](https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4). To reduce file size download, it is advised that the API is used to only download data that is relevant to the project time period of focus (2021-04-19 to 2021-10-31)
