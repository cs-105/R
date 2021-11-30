FUTURE FIRE 
![FutureFire](https://github.com/cs-105/R/blob/Claire/FutureFireLogo.png)
---
Wildfire Simulator that integrates geography with current and historical weather to form a fire prediction at a selected location around the United States. Returns a visual prediction of the fire and growth over time. [FutureFire](https://firemap.shinyapps.io/fireMapApp/)

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
6. Run the Map.R file

```
$ cd ../lorem
$ npm install
$ npm start
```

## Data
Data is received from [OpenWeatherMap](https://openweathermap.org/) in the form of JSON. Historical weather data as well as future data is received to form the prediction.
Approximate vegetation index received from [NASA](https://neo.gsfc.nasa.gov/view.php?datasetId=MOD_NDVI_M)
in the form of a csv file. 
