Metadata Climate Change in Streamflow (MD project)
authors: R.Willem Vervoort, Michaela Dolk, Floris van Ogtrop
institution: Centre for Carbon Water and Food, Sydney Institute of
Agriculture, The University of Sydney
contact: willem.vervoort@sydney.edu.au
The data in this project are sourced from the Bureau of Meteorology
website. 
They are based on a sample of the hydrological reference stations and
closest rainfall and high quality temperature stations as well as gridded
rainfall data
These stations are given in CatchmentCharact.csv and in the Stations R
object in the Rdata file
The objects in the Rdata file are:
This metadata file as a text object, called README_datadescribe
Stations a dataframe with the catchment characteristcs
flow_rain_maxT_weekly: a dataframe with column headers Date, decade,
station, Flow, Rain, maxT, stations are stacked
flow_zoo: all flow data for the catchments as a zoo data frame, 13
catchments in columns
rain_zoo: all rain data for the catchments as a zoo data frame, 13
catchments in columns
maxT_zoo: all maxT data for the catchments as a zoo data frame, 13
catchments in columns
GridRainAllDataout: all daily rainfall including gridded data stacked
catchments
weekGridRainAllDataout: all weekly rainfall including gridded data 
stacked catchments
The objects in the zip file are:
This metadata file
CatchmentCharact.csv
flow_rain_maxT_weekly.csv: a dataframe with column headers Date, decade,
station, Flow, Rain, maxT, stations are stacked
weeklyDataIncludingGridded.csv: the weekly rainfall data included gridded
as a stacked dataframe with 3 columns
DailyDataIncludingGridded.csv: the daily rainfall data included gridded
as a stacked dataframe with 3 columns
