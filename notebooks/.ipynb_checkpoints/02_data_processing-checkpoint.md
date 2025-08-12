# Introduction to the Problem

This notebook discusses data processing in Python and R done as the first step in my analysis of hurricane rainfall over three East Texas rivers (the Brazos, Trinity, and Neches). The goal is to determine if the tracks hurricanes take through the region might be able to predict river flooding risk. 

I found all hurricanes that directly dropped rain over these three river basins between 2005 and 2021 and split them into three groups based on track, as discussed in the [README](https://github.com/JordanRSimons/analyzing-hurricane-data/blob/main/README.md).

In the sections below, I will showcase how I prepared gathered data in a variety of formats for exploritory visualization-based analysis (see [notebook 1](https://github.com/JordanRSimons/analyzing-hurricane-data/blob/main/notebooks/01_visualization_and_exploratory_analysis.md) for the visualizations themselves). Different types of data required two different workflows, one mostly in Python and one mostly in R. For readability, this notebook is divided based on language.

The R section primarily showcases my handling of data involving inconsistent entries and managing date formatting. The Python section primarily showcases a smooth function-based plot creation workflow and management of different map projections.

I used both METAR format weather station data from the ASOS or AWOS sensor network ([archived online](https://mesonet.agron.iastate.edu/request/download.phtml?network=TX_ASOS) at the Iowa Environmental Mesonet), as well as netCDF meteorological data from the [NCEP North American Regional Reanalysis](https://psl.noaa.gov/data/gridded/data.narr.html) (NARR) model database for this project

# Data Processing in R

In this project, R was primarily used to handle weather station data in the METAR format, which can be read as csvs. The challenge with this analysis is that each of more than 75 weather stations in East Texas, each with their own data collection processes.

I import libraries and begin with a function definition. The checkHurricanes function is ran over every weather station to check if the station was running during each hurricane impact in the dataset. The dataset hurricanes contains hurricane start and end times based on analysis of East Texas radar data.

```r
library(tidyverse)
library(lubridate)
library(data.table)

# libraries needed for mapping
library(sf)
library(maps)
library(rnaturalearth)

# function takes list of hurricane start/end times and weather station
# checks whether station was active and adds TRUE/FALSE column
checkHurricanes <- function(hurricanes, stationName){
  
  # create path to station file
  stationNamePath <- paste0("stations/",stationName)

  # read file in as "site"
  site <- data.table::fread(stationNamePath, header=TRUE)  

  # remove unneeded columns
  site <- site %>% select(station, valid, p01i, tmpf, sknt, gust, drct, alti, mslp)

  # replace all missing values marked as "M" with NA for cleaner processing
  site[site=="M"]<-NA

  # make sure the rainfall variable is numeric
  site$p01i <- as.numeric(site$p01i)
  
  # get rid of any rows that had no precip measurement
  site <- site[!is.na(site$p01i)]

  # read file in as "site"
  site$valid <- lubridate::ymd_hm(site$valid, truncated=2) 
  
  # create column (TRUE if any hurricane present)
  site$anyHurricane <- site$valid %in% unlist(Map(`:`,hurricanes$start, hurricanes$end)) 

  # loop through the hurricanes
  for(i in 1:length(hurricanes$stormWindow)){  
    name <- hurricanes$stormWindow[i] #  variable for hurricane name
    
    # Column with TRUE if that hurricane present
    site$newColumn <- site$valid %in% unlist(Map(`:`,hurricanes$start[i],hurricanes$end[i]))

    # rename that column with hurricane name
    names(site)[names(site)=="newColumn"] <- name  
  }

  # save station output with TRUE/FALSE to stormsAdded
  fwrite(site,paste0("output/stormsAdded/",stationName)) 

  # keep rows with a hurricane
  siteTrue <- site %>% filter(anyHurricane==TRUE)  

  # save a second version with only times during the hurricane windows to duringStormsOnly
  fwrite(siteTrue,paste0("output/duringStormsOnly/",stationName)) 
}
```

Running this function then allows the generation of a single large dataset that includes data from all stations, marked with whether the station was active or not. 

```r

# read in the hurricane date windows file
hurricanes <- data.table::fread("hurricaneWindows.csv", header=TRUE)

#reformat the start and end times as date/time (rather than character strings)
hurricanes$start <- as.POSIXct(hurricanes$start)
hurricanes$end <- as.POSIXct(hurricanes$end)

# get list of all weather station files in the stations directory
stations <- data.frame(list.files(path="stations/"))
names(stations) <- c("site")

for(i in 1:length(stations$site)){
  checkHurricanes(hurricanes, stations$site[i])
}


# create a file with all of the storms and stations, with stations marked TRUE for times when a hurricane was present
allStormsAllSites <- list.files(path="output/duringStormsOnly", pattern="*.csv", full.names=TRUE) %>% map_df(~fread(.))

# save for station output to duringStormsOnly
fwrite(allStormsAllSites,"output/allStormsAllSites.csv") 
```

For the purposes of this showcase, the file allStormsAllSites2 used in the following analysis can be considered to be a copy of allStormsAllSites.

My goal is to quantify the total rainfall at each station during the times a hurricane was present in order to determine which group of hurricanes pose the greatest flooding risk to a given area.

The issue is that these stations don't have a total rainfall variable. Instead, they have a variable called p01i, defined as “one hour precipitation for the period from the observation time to the time of previous hourly precipitation reset.” 

To better understand exactly what this means, consider a backyard rain gauge. Every so often, someone checks it, and records how much water is present: Perhaps at 10:22, there is 0.1" of water, and at 10:44, there is 0.3" of water. This is the form of the p01i data. Critically, between 10:22 and 10:42, 0.2" of rain fell, NOT 0.3". Then, once every hour, the rain gauge is emptied, a reset. This normally occurs on the hour in the data, but not always. Perhaps, at 11:02, this station records 0.01" of rain. This signifies that soon after 10:44, the station was reset, and only a tiny bit more rain fell since then. Data reporting times are different between stations and also change over time, making this data difficult to work with.

To handle these observation timing discrepancies, I  computed the difference from each measure back to the preceding one. The value corresponding to XX:42 would be 0.2. If this value is negative, it denotes a reset occurred since the last measure. But if all values in an hour are 0 or if heavy rain falls in the beginning of a new hour, this won’t be the case. So I also check if the hour or day has turned over. If this happens more than 19 minutes after a reset (to account for resets sometimes being slightly off the hour), I can mark a reset. When a reset has occurred, I record the original p01i value. When a reset hasn’t occurred, I record the difference computed earlier instead, the new rainfall since the last measure. With this new column, a simple sum or mean can give meaningful information about storm totals.

```r
# create a column with just the hour from the dateTime
allStormsAllSites2$hour <- hour(allStormsAllSites2$valid) 

# create a column with just the day of week (1-7) from the dateTime
allStormsAllSites2$day <- day(allStormsAllSites2$valid)

# difference in rainfall from one sample to next
allStormsAllSites2$diff_p01i <- allStormsAllSites2$p01i - lag(allStormsAllSites2$p01i)

# time difference between this sample and the previous one
allStormsAllSites2$timeIncrement = round(as.numeric(allStormsAllSites2$valid - lag(allStormsAllSites2$valid), units="hours"),2)

# if the difference from one sample to the next is negative, that means there was a reset
# mark it TRUE if it's negative, FALSE otherwise
allStormsAllSites2$negativeDiff <- ifelse(allStormsAllSites2$diff_p01i<0,TRUE,FALSE)

# if the hour changed from one sample to the next, then TRUE. Otherwise FALSE
allStormsAllSites2$hourChange <- ifelse(allStormsAllSites2$hour - lag(allStormsAllSites2$hour) !=0,TRUE,FALSE)

# if the day changed from one sample to the next, then TRUE. Otherwise FALSE
allStormsAllSites2$dayChange <- ifelse(allStormsAllSites2$day - lag(allStormsAllSites2$day) !=0,TRUE,FALSE)

# if the day changed from one sample to the next, then TRUE. Otherwise FALSE
allStormsAllSites2$stationChange <- ifelse(allStormsAllSites2$station != lag(allStormsAllSites2$station),TRUE,FALSE)

# if any of the above conditions are true, mark reset=TRUE. 
# Otherwise, those are multiple samples within the same hour with no reset
allStormsAllSites2$reset <- ifelse(allStormsAllSites2$negativeDiff==TRUE | allStormsAllSites2$hourChange==TRUE | allStormsAllSites2$dayChange==TRUE | allStormsAllSites2$stationChange==TRUE,TRUE,FALSE)

# find the time since the previous reset and set that to resetLag
allStormsAllSites2 <- allStormsAllSites2 %>% group_by(reset, station) %>% mutate(resetLag = ifelse(reset==TRUE,valid-lag(valid),NA)) %>% ungroup


# if reset was true, record the value from p01i (the first reading of that hour). Otherwise, record the difference
# between that sample and the previous one (how much new rain fell since the last sample)
# Only reset for an hour/day change if the previous reset was more than 19 minutes ago.
allStormsAllSites2$rainfall=ifelse((allStormsAllSites2$reset==TRUE & allStormsAllSites2$resetLag > 19) | allStormsAllSites2$negativeDiff==TRUE,allStormsAllSites2$p01i,allStormsAllSites2$diff_p01i)

# rate of rainfall per hour during that sample window
allStormsAllSites2$inchPerHour=round(allStormsAllSites2$rainfall/allStormsAllSites2$timeIncrement,2)

# put the important columns right after the dateTime to make them easier to see
allStormsAllSites2 <- allStormsAllSites2 %>% relocate(c(p01i,rainfall, diff_p01i,reset,resetLag,timeIncrement, negativeDiff,hourChange, dayChange,stationChange,inchPerHour, day, hour), .after=valid)
```
We now have a rainfall column where each row shows the new rainfall since the last measurement - summing this column would give the total rainfall. Each storm also has a TRUE/FALSE column as described above corresponding to whether or not the station was active at the time a storm occurred. By substituting the rainfall values to replace the TRUE values, it is easy to sum individual storm's rainfall.

```r
# function to replace the TRUE values with rain total. Replace FALSE or missing measures with NA
replaceTrue <- function(x)(ifelse(x==FALSE, NA,allStormsAllSites2$rainfall))

# call the function to replace TRUE (i.e., storm present) with rainfall. Do that for anyHurricane and each hurricane column
allStormsAllSites2 <- allStormsAllSites2 %>% mutate_at(vars(anyHurricane:ncol(allStormsAllSites2)), replaceTrue)

#save the file with rainfall totals per hurricane/site
fwrite(allStormsAllSites2,"output/rainPerHurricaneSite.csv")

# summarize totals by station for each storm
summaryTotals <- allStormsAllSites2 %>% group_by(station) %>% summarize_at(vars(anyHurricane:Nicholas_9), sum, na.rm=TRUE)

# save the summary totals per site and hurricane
fwrite(summaryTotals,"output/summaryTotals.csv") 
```

As a final step to prepare for plotting, I join the summaryTotals dataset with two other datasets. Since the summaryTotals file contains storm totals at all stations for all hurricanes, I can join this dataset with a separate dataset containing latitude and longitude position data for each station.

```r
# read in the new stationLocations file as "locations"
locations <- fread("stationLocations.csv")
locations <- data.frame(locations)
colnames(locations) <- c("station","name","Lat","Lon")

# merge location info with summaryTotals to create new combined file and write it
summaryLocationTotals <- full_join(locations, summaryTotals, by="station")
fwrite(summaryLocationTotals,"output/summaryLocationTotals.csv") 
```

Then I do another join with a dataset that summarizes the runtimes of various stations.

```r
# add total storms encountered and station start/stop dates to the summary
summaryLocationTotalsCount <- full_join(summaryLocationTotals, stationRunTime, by="station")
```

For plotting purposes, we will need a categorical variable for rainfall totals.

```r
summaryLocationTotalsCount <- summaryLocationTotalsCount %>% mutate(
  rainGroup = case_when(
    anyHurricane <= 3 ~ "0 - 2.99",
    anyHurricane > 3 & anyHurricane <= 9 ~ "3 - 8.99",
    anyHurricane > 9 & anyHurricane <=15 ~ "9 - 14.99",
    anyHurricane > 15 & anyHurricane <=40 ~ "15 - 39.99",
    anyHurricane > 40 ~ "40+"
  )
```

The plot needs to be initialized with map data.

```r
# load world map data and state map data
world_map_data <- ne_countries(scale = "medium", returnclass = "sf")
state_map_data <- map('state', fill = TRUE, plot = FALSE) %>% st_as_sf()

# create a color palette
cbbPalette <- c("#000000", "#009E73", "#E69F00", "#b012e0", "#56B4E9", "#0072B2", "#D55E00", "#CC79A7")
```

Finally, plots can be made using ggplot similar to this example. See the second notebook for a discussion of the plots themselves.

```r
ggplot() + 
  # add world map data
  geom_sf(data = world_map_data) +
  
  # add state map data
  geom_sf(data=state_map_data) + 
  
  # add points from my locations file
  # geom_point(data=summaryLocationTotals, aes(x=Lon,y=Lat), color="red") +
  geom_point(data=summaryLocationTotalsCount, aes(x=Lon,y=Lat,color=rainGroup)) +
  scale_colour_manual(values=cbbPalette) +
  
  # add horizontal lines
  geom_hline(yintercept=30.5, linetype="dashed", color="blue") +
  geom_hline(yintercept=32, linetype="dashed", color="blue") +
  
  # add vertical line
  geom_vline(xintercept=-95.3698, linetype="dashed", color="green") +
  
  # add a theme that gets rid of the default gray plotting grid
  theme_bw() + 
  
  # specify the edges of the map
  coord_sf(xlim=c(-103,-91),ylim=c(25,36)
           
  ) # end plot
```

# Data Processing in Python

In this project, Python was primarily used to handle data in the netCDF format. NetCDF files are multidimensional. In this case, the datasets contain 3 dimensions, time, and spatial coordinates x and y. Many data variables, some with data for many different altitude levels, are recorded at all time steps.

My Python code uses a pair of functions to generate plots, storing each in a dictionary for quick retrieval. By changing the dictionary’s key, different plots can be displayed, making it well-suited for exploratory analysis (see the next notebook). Over the second summer of research, our needs evolved rapidly, and some elements of these functions were hardcoded in order to meet immediate goals. In these sections, I will omit some code for clarity and discuss potential improvements.

With these functions, plots can be displayed with just two lines of code...

```python
pltData = createData(ds, '+24', 'trailing', analysis = 'meanOfMeans')
pltMap = makePlot(pltData, 'filled', 'pr_wtr', 'prwtrMean', 'Precipitable Water Test', 'Precipitable Water (kg/m^2)', window = 'full')
pltMap
```

or even a single dictionary entry.

```python
prwtrDict['all-18']
```

```python
import numpy as np 
import xarray as xr 
import cartopy.crs as ccrs 
import cartopy.feature as cfeature 
import matplotlib.pyplot as plt 
import matplotlib as mpl
from matplotlib import ticker 
from matplotlib.colors import BoundaryNorm 
import matplotlib.patches as mpatches 
from math import ceil, floor 
import seaborn as sns 

ds = xr.open_dataset('data/prwtrData/prwtrAllStormsExtended.nc')
```

## Function 1: createData

```python
#INPUT: A massive netCDF weather dataset
#    a list of start/end dates for all storms in data, 
#    a list of the storms wanted in the output, 
#    the desired level of the dataset ('level'), 
#    and the desired analysis to be performed

#OUTPUT: A single .nc dataset containing only data for times corresponding to the selected storms with analysis applied.
def createData(fullData, allDates, selStorms, level = None, analysis = None, allStorms = None) :
```
The dataset fullData is a large netCDF dataset containing weather data from across the United States that contains data from the months in which hurricanes in the dataset occurred.

The first parameter, allDates, allows the user to select the exact date ranges to consider for each hurricane. These date ranges were derived from observational radar data to cover the full time range during which the hurricane or its remnants dumped rain on the relevant portion of East Texas. This allows flexibility to test other date ranges, such as the day after landfall. If more date ranges were to be added in the future, these alternatives could be created by using a function to adjust the dates.

```python
if allDates == 'hourExtended' :
        allDates = ['2005-09-22T16', '2005-09-26T01',
        '2007-08-15T02', '2007-08-19T23:59',
        '2007-09-11T08', '2007-09-14T16',
        '2008-08-04T07', '2008-08-07T20',
        ...,
        '2021-09-11T16', '2021-09-16T06'
        ]
```

Next, selStorms selects which hurricanes to include in the output dataset. Options include each of three general categories based on storm track: northwest, northeast, and drag, or all storms.

```python
    if selStorms == 'all' :
        selStorms = ['Rita', 'Erin', 'Humberto', 'Edouard', ..., 'Nicholas']
```

Next, these parameters are applied to the fullData. Some data types, such as geopotential height (a measure of the altitude at which a specific air pressure value is found) come with multiple levels, requiring an additional extraction.

The analysis parameter allows for the data to come with an action like a mean or a median already applied to the data. The meanOfMeans analysis takes the mean of the data variable across each storm, then outputs a mean of these individual storm means.

```python
    # filters to a specific level if requested
    if level != None :
        fullData = fullData.sel(level = level)
    
    # take the first selected storm
    storm0 = selStorms[0]
    
    # find it's position in the full list of storms
    k0 = allStorms.index(storm0)
    
    # slice the full dataset down to just contain the data corresponding to the start/end dates of this first storm.
    data = fullData.sel(time = slice(allDates[k0*2],allDates[k0*2+1]))

    # take an average over the time dimension of the data variable
    if analysis == 'meanOfMeans' :
        data = data.mean(dim = 'time')
    
    # loop through remaining storms and concatenate their individual data together with data0.
    for storm in selStorms[1:] :
        
        # like before, find the selected storm's index value in allStorms 
        # then get data between that storm's start/end date
        k = allStorms.index(storm)
        datak = fullData.sel(time = slice(allDates[k*2],allDates[k*2+1]))
        
        if analysis == 'meanOfMeans' :
            datak = datak.mean(dim = 'time')
        
        # selected storm 0's data was created outside the loop, so selected storm 1's data needs special treatment.
        if selStorms.index(storm) == 1:
            data = xr.concat([data,datak], dim= 'time')
            continue
        
        # selected storms 2 and beyond can simply have their data combined with "data" itself.
        else :
            data = xr.concat([data,datak], dim = 'time')
```

Finally, the other analysis types can be applied to the data, depending on the request. This could potentially be more cleanly coded by having the analysis parameter input a function.

```python
    if analysis == 'sum' :
        data = data.sum(dim = 'time')  
        
    if analysis == 'mean' or analysis == 'meanOfMeans' :
        data = data.mean(dim = 'time')
    
    # Turn data taken hourly into a daily sum or average
    if analysis == 'dailySum' :
        data = data.resample(time = '1D').sum(dim ='time')
    
    if analysis == 'dailyMean' :
        data = data.resample(time = '1D').mean(dim = 'time')
        
    return data
```

We now have a dataset that has been properly filtered to a workable size and is ready for plotting.

# Function 2: makePlot

The makePlot function does exactly as the name suggests, and turns the created dataset from the last function into the desired plot. Similar to the analysis parameter, some of these inputs could likely be functions instead of text.

```python
#INPUT: A dataset filtered by createData() such that there is a data variable given with position coordinates 'x' and 'y',
#    the type of plot desired, 
#    the data variable to plot ('varname'), 
#    a list of numerical values to place contours,
#    a title for the graph ('title'), 
#    a label for the colorbar ('label'), 
#    and an optional input that can invert the color map.

#OUTPUT: A map complete with grid lines and a colorbar for the requested variable
def makePlot(ds, plotType, variable, levels, title, cbarLabel = None, window = 'area', color = None, display = False) :
```

The two types of plot this function can produce are contour line plots and filled contour plots, to address varying standards across different variables.

The levels parameter allows each type of plot to use a different provided array of values to set the scale of the colorbar. Similarly, a title and colorbar label for the resulting plot can be given as well.

Because the Earth is spherical, it is mathematically impossible to perfectly represent all spacial relationships between places on a flat map. Most people are used to seeing projections like the Plate Carree projection, which plots on a grid of latitudes and longitudes, and ends up severely deforming landmasses close to the poles.

Meanwhile, NARR is parameterized using coordinates x and y, which are derived from the Northern Lambert Conformal Conic map projection, good for visualizing large areas. This projection draws latitude lines as perfect concentric circles, creating a cone-shaped map. For maps of Texas, I will use the same projection as the data source.

The cartopy Python package allows transformations between these projections, as seen in the following function code.

The window parameter allows plots to be made with either a zoomed in view of East Texas or a zoomed out view of the central US.

```python
    # define the map projection the data's x,y values are given in
    data_crs = ccrs.LambertConformal(central_longitude=-107,central_latitude=50, standard_parallels=[50, 50.000001], false_easting = 5632642.22547, false_northing = 4612545.65137)

    # set the bounds of the region, and the needed scaling to fill the plot with data filtered from another projection
    
    if window == 'area' :
        minlon = -103; maxlon = -90; minlat = 25; maxlat = 37
        minlonScale = -2; maxlonScale = 5; minlatScale = -0.5; maxlatScale = 0
    
    if window == 'full' :
        minlon = -107; maxlon = -82; minlat = 20; maxlat = 44
        minlonScale = -3.5; maxlonScale = 13.5; minlatScale = -0.5; maxlatScale = -3.5
    
    # This code takes points with "source crs" in latitude and longitude (Plate Carree) and transforms the coordinates into data_crs (Lambert).
    # These new computed points computed from the corners of the area of interest are then used to slice the data.
    # You can only slice the netCDF file on dimension variables (x and y), hence this conversion.
    minx, miny = data_crs.transform_point(minlon + minlonScale, minlat + minlatScale, src_crs=ccrs.PlateCarree())
    maxx, maxy = data_crs.transform_point(maxlon + maxlonScale, maxlat + maxlatScale, src_crs=ccrs.PlateCarree())
```

Setting up the plot's gridlines requires more fun with map projections:

```python
    fig = plt.figure()
    
    # the axes ax are set up as a subplot
    # they need to plot data with a Lambert Conformal projection centered on North America.
    ax = fig.add_subplot(1,1,1,projection = ccrs.LambertConformal(central_longitude=-97.0,central_latitude=53, standard_parallels=[53]))

    #Add features to ax, make the features stand out more in the filled contour plots
    if plotType == 'filled' and window != 'full' :
        ax.add_feature(cfeature.COASTLINE.with_scale('10m'), zorder=10, linewidth = 5)
        ax.add_feature(cfeature.RIVERS.with_scale('10m'), zorder = 20, linewidth = 3)
        ax.add_feature(cfeature.BORDERS.with_scale('10m'), linestyle = '-', zorder = 10, linewidth = 5)
        ax.add_feature(cfeature.STATES.with_scale('10m'), linestyle = '-', zorder = 10, linewidth = 5)
    else :
        ax.add_feature(cfeature.COASTLINE.with_scale('10m'), zorder=10, linewidth = 3)
        ax.add_feature(cfeature.RIVERS.with_scale('10m'), zorder = 20, linewidth = 2)
        ax.add_feature(cfeature.BORDERS.with_scale('10m'), linestyle = '-', zorder = 10, linewidth = 3)
        ax.add_feature(cfeature.STATES.with_scale('10m'), linestyle = '-', zorder = 10, linewidth = 3)

    # zoom in using the desired bounds, defined in Plate Carree
    ax.set_extent([minlon,maxlon,minlat,maxlat], crs = ccrs.PlateCarree())
    
    # draw a box around the area of interest in larger maps
    if window == 'full' :
        
        # SW, NW, NE, SE
        coords = np.array([[-103,25], [-104,37], [-89,37], [-90,25]])

        # draw a red box around the area of interest, defined in Plate Carree
        ax.add_patch(mpatches.Polygon(xy = coords, edgecolor = 'red', facecolor = 'none', linewidth = 2.5, transform = ccrs.PlateCarree(), zorder = 200))

    plt.title(title, size = 40, weight='bold')

    # add gridlines to the axes ax
    # make the lines based on Plate Carree, lat/lon  
    # remove inline labels
    gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, x_inline=False, y_inline=False, linewidth=0.75, color='k',alpha=0.75)

    # remove gridline labels on the top and right of the plot
    gl.top_labels = gl.right_labels = False

    # choose locations for the gridlines, depending on size of plot
    if window == 'full' :
        gl.ylocator = ticker.FixedLocator([20,25,30,35, 40, 45])
        gl.xlocator = ticker.FixedLocator([-105, -100, -95, -90, -85, -80])
    else :
        gl.ylocator = ticker.FixedLocator([25,27.5,30,32.5, 35, 37.5])
        gl.xlocator = ticker.FixedLocator([-102.5, -100, -97.5, -95, -92.5, -90, -87.5])

    # this opens a style defining system to adapt the gridline labels to change label size.
    gl.xlabel_style = {'size': 15}
    gl.ylabel_style = {'size': 15}
```

The plot itself (either contour or filled contour) can then be drawn.


```python
    # count the number of colors needed: down arrow, all boxes, up arrow
    # create a hex code color palette with the correct number of colors
    
    numcol = len(levels) + 1
    
    if plotType == 'line' :
        
        # set the line plot color palette.
        palette = sns.color_palette("viridis", numcol).as_hex()
        
        if color == 'reverse' :
            
            palette = sns.color_palette("viridis_r", numcol).as_hex()
            
        
        # the contour plot uses a transform parameter to tell the plot that 'lon' and 'lat' 
        # should be plotted with a Lambert Conformal Conic projection
        # a high zorder value ensures that the contours aren't covered by other borders.
        cplot = ax.contour(ds['lon'], ds['lat'], ds[variable], transform = ccrs.PlateCarree(), levels=levels, colors = palette, linewidths = 2.5, zorder = 100)

        # labels the contours on the plot
        ax.clabel(cplot, levels = levels, fontsize = 12)
    
    
    if plotType == 'filled' :
        
        # set the normalization so colors are placed according to levels
        norm = BoundaryNorm(boundaries = levels, ncolors = len(levels))

        #The filled plot color palette.
        palette = sns.color_palette("cubehelix_r", numcol).as_hex()
        
        if color == 'reverse' : 
            
            palette = sns.color_palette("cubehelix", numcol).as_hex()

        # the contour plot uses a transform parameter to tell the plot that 'lon' and 'lat' 
        # should be plotted with a Lambert Conformal Conic projection
        cplot = ax.contourf(ds['lon'], ds['lat'], ds[variable], colors = palette, levels = levels, norm = norm, transform = ccrs.PlateCarree(),extend='both')

    # create the color bar
    cbar = fig.colorbar(cplot, orientation='vertical', shrink = 0.75, ticks = levels)
    cbar.set_label(label = cbarLabel, size=25, weight='bold')

    # adjust the colorbar label size using its .ax (axis) method
    cbar.ax.tick_params(labelsize=15) 
    
    # close figure if requested, lets it be stored while not displaying
    if display == False :
        plt.close(fig)
    
    #Return the figure so it can be stored in a variable.
    return fig
```

Since this function allows easy storage of plots, I can then loop through all possible plot outputs to generate a plot dictionary for easy exploration of the resulting visualizations.

This is the example dictionary for the data variable precipitable water. With additional looping and improved functions, a dictionary with all data variables could be created.

```python
# definitions to set up for looping 
# this loop also can save the resulting plots as images
prwtrDict = {}
stormGroups = ['nw', 'ne', 'drag', 'dragXH', 'distant', 'trailing', 'all']
allDates = ['0', '-6', '-12', '-18', '-24']

for gr in stormGroups :

    # creates the plot titles based on storm group, while also calling the group correctly in the function
    # could be optimized in the future
    
    # result: 
    # title: NW, NE, Drag, Distant, Trailing Bands
    #key: nw, ne, drag, distant, trailing
    if len(gr) == 2 :
        grTitle = gr.upper()
    elif gr == 'trailing' :
        grTitle = 'Trailing Band'
    else :
        grTitle = gr.capitalize()
        
    grKey = gr
    
    for da in allDates :
        
        # result: 
        # title and key both just contain the inputted number.
        daTitle = daKey = str(da)
            
                
            
        # create title
        title = '{} Storms Landfall {} Precipitable Water'.format(grTitle, daTitle)

        # create plot dictionary key:
        # desired key examples: nw0, nw-6
        key = '{}{}'.format(grKey, daKey)

        # create the data for each set of parameters using the created title
        data = createData(ds, da, gr, analysis = 'meanOfMeans')

        # make the plot
        thePlot = makePlot(data, 'filled', 'pr_wtr', 'prwtrMean', title, 'Precipitable Water (kg/m^2)', window = 'full', display = False)


        # place thePlot into the dictionary with the key defined above
        prwtrDict[key] = thePlot
        
        # saves out the plot images to allow gif creation
        # thePlot.savefig('data/prwtrPlots/'+str(key)+'.jpg')
```

Now, as promised, a plot can be called using one line of code. 

```python
prwtrDict['all-18']
```

See the [notebook 1](https://github.com/JordanRSimons/analyzing-hurricane-data/blob/main/notebooks/01_visualization_and_exploratory_analysis.md) for a discussion of the plots themselves.


