FUTURE FIRE 
![FutureFire](https://github.com/cs-105/R/blob/Claire/FutureFireLogo.png)
---
Wildfire Simulator that integrates geography with current and historical weather to form a fire prediction at a selected location around the United States. Returns a visual prediction of the fire and growth over time. You can see the deployed app here: [FutureFire](https://firemap.shinyapps.io/firemap/).

## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Setup](#setup)
* [Data](#data)

## General info
This project is simple fire simulator
	
## Technologies
Project is created with:
* [R language - 4.1.2](https://www.r-project.org/)
* [Open Weather API](https://openweathermap.org/api)
* [Leaflet](https://leafletjs.com/)
* [Vegetation Index](https://neo.gsfc.nasa.gov/view.php?datasetId=MOD_NDVI_M)
* [RStudio](https://www.rstudio.com/products/rstudio/download/)
	
## Setup
To run this locally
1. Clone the project
2. Install [R language - 4.1.2](https://www.r-project.org/), This can look different depending on your system
3. Install [RStudio](https://www.rstudio.com/products/rstudio/download/)
4. Open the project in RStudio
5. Run Installations.R to download all necesary packges
6. Replace the given Open Weather API key with your personal key
7. Run the Map.R file

## Data
The project is retrieving weather data from [OpenWeatherMap](https://openweathermap.org/) in the form of JSON. It uses some historical weather data to determine the probability of a fire starting, and current weather to incorperate wind and other enviornmental factors in the fire simulation.
Approximate vegetation index received from [NASA](https://neo.gsfc.nasa.gov/view.php?datasetId=MOD_NDVI_M)
in the form of a csv file.  The project uses this to determine if there's enough fuel for a fire to burn in this area.
