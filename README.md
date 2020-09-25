# Dental Office Analysis

This repository contains the code to replicate the analysis of dental office visits using SafeGraph data.

********************************************

# Project and directory structure

This section describes the directory structure of the project.  The project is divided into two primary parts.  Part 1 builds the dataset(s) to be used in the analysis phase.  Part 2 contains scripts to run the analysis and generate output (tables and figures).  

## Part 1: Build

- `inputs` directory contains the raw data that should not be modified or overwritten.  
- `cache` directory stores copies of data during intermediate steps of data processing   
- `code` directory contains all scripts to read in and process the data  


## Part 2: Analysis

- `cache` directory stores copies of data during intermediate analysis steps     
- `code` directory contains all scripts to read in and preprocess the data   

The`output` directory contains figures and tables generated from analysis scripts
The `functions` folder contains all functions specific to this analysis.  

## Building the Project

The project root directory contains a file called `project_init.R` that initializes the project (installs/loads packages etc.).  You should run this file each time you open R to begin working on the project.  

*Note that all file references within project are relative to the root directory of the project.*

********************************************

# Data

The primary data source for the project is www.safegraph.com.  To reproduce the analysis, you will need the weekly patterns data (https://docs.safegraph.com/docs/weekly-patterns).  You can sign up to access the data here: https://www.safegraph.com/covid-19-data-consortium.


## Data Processing

The SafeGraph patterns dataset provides foot traffic information on points of interest (POI) defined by a building or location perimeter.  We subset the POI data for locations listed as Offices of Dentists (NAICS code 621210).  We compile the following foot traffic data for these POIs:

- daily visitation

- median distance traveled by those who visit in a given week

- dwell time by those who visit in a given week

- locations visited on the same day within a week

Visitation data is normalized by the number of devices residing in the county where the POI is located.  The foot traffic data is reported at the POI.  We aggregate the distance and dwell data up to the national level by calculating the average weighted by the visit counts.

We calculate metrics by metro and nonmetro areas.  We use the CDC county designation to classify counties as either metro or nonmetro.