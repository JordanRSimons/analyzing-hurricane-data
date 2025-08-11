![Storm Count](../images/numberMeasuresPerCount.png)

# Recap and Introduction

In this notebook, I will discuss statistical analysis done in R on an [IBTrACS](https://www.ncei.noaa.gov/products/international-best-track-archive) csv dataset containing a variety of data about hurricanes since the 1980s, such as strength, path, timing, and more. As part of the analysis, I created several polished data tables along with an interactive Shiny app that display relevant plots.

The goal of this analysis is exploritory in nature, aiming to roughly confirm expected trends (such as a correlation between wind speed and pressure) while seeing if any unexpected ones emerge from a general analysis. Any unlikely trends discovered would help inform our broader analysis of hurricane-driven river flooding in East Texas. For more on this broader project, see the other two notebooks in this repository. 

We will use the following libraries, the IBTrACS dataset, and a secondary dataset containing the defined date windows when hurricanes were droping rain in East Texas, as per radar data.

```r
# libraries
library(tidyverse) 
library(lubridate) 
library(knitr) 
library(data.table) 
library(shiny) 
library(Hmisc) # for p-value matrix

fullData1980 = read_csv('Data/ibtracsData/IBTrACSsince1980.csv')
windows = read_csv('Data/230713HurricaneWindows.csv')
```

# Data Processing and Displayable Data Tables

We begin by filtering the data.

```r
# remove first row that doesn't include data, 
# isolate the relevant general time range to improve future load times, 
# select useful columns
fullDataFiltered <- fullData1980[-1,] %>% 
  filter(SEASON > 2003) %>%
  select(NAME, ISO_TIME, SEASON, LAT, LON, WMO_WIND, USA_SSHS, WMO_PRES, STORM_SPEED, STORM_DIR, BASIN, SUBBASIN)

# filters the data further to only include the storm windows
fullDataFiltered <- fullDataFiltered %>% filter(ISO_TIME %in% unlist(Map(`:`,windows$hourSt, windows$hourEnd)))
```

Next, note that we don't actually care about all hurricanes occuring on these dates, just the ones in our list. We also need to worry about a duplicate name - within the defined windows, a second Hurricane Humberto occured outside the area of interest.

```r
# list of all storms
allStorms <- list('RITA', 'ERIN', 'HUMBERTO', 'EDOUARD', 'GUSTAV', 'IKE', 'HERMINE', 'LEE', 'ISAAC', 'BILL', 'CINDY', 'HARVEY', 'BARRY', 'IMELDA', 'CRISTOBAL', 'LAURA', 'BETA', 'DELTA', 'NICHOLAS')

# filter to only names that match,
# remove a second Humberto that occurs away from the area during another storm's window in 2019
# remove extra datapoints that occur around landfalls, keeping only times with the normal 3-hourly measure
dsAllStorms <- fullDataFiltered %>%
  filter(NAME %in% allStorms) %>%
  filter(NAME != 'HUMBERTO' | SEASON != 2019) %>%
  filter(NAME != "IKE" | day(ISO_TIME) > 10) %>%
  filter(hour(ISO_TIME) %% 3 == 0 )
```
A few variables are then added as new columns. Primarily, this includes our primary hurricane groupings based on track (see the other two notebooks for more details): northeast, northwest, and drag (along the coast) as well as a few other "auxiliary groups" whose meanings are not important for this showcase (distant, trailing, and drag without Harvey, dragxH). Storms are all in exactly one primary group, but some get placed in one or more the auxiliary group as well.

Another variable is created to code the direction of movement as cardinal directions instead of a degree number. Some of the code is omitted for clarity.

```r
# add storm category column and special category columns
dsAllStorms <- dsAllStorms %>% mutate(
  trackGroup = case_match(NAME,
    c('ERIN', 'HERMINE', 'BILL', 'IMELDA') ~ 'nw',
    c('RITA', 'HUMBERTO', 'GUSTAV', 'IKE', 'LEE', 'ISAAC', 'CINDY', 'BARRY', 'CRISTOBAL', 'LAURA', 'DELTA') ~ 'ne',
    c('EDOUARD', 'HARVEY', 'BETA', 'NICHOLAS') ~ 'drag'
  ),
  ...

dsAllStorms <- dsAllStorms %>% mutate(STORM_DIR = as.numeric(STORM_DIR))

dsAllStorms <- dsAllStorms %>% mutate(
  dirCat = case_when(
    STORM_DIR >= 0 & STORM_DIR < 30 ~ 'NNE',
    STORM_DIR >= 30 & STORM_DIR < 60 ~ 'NE',
    ...,
    STORM_DIR >= 300 & STORM_DIR < 330 ~ 'NW',
    STORM_DIR >= 330 & STORM_DIR < 360 ~ 'NNW'
    
  )
)
```

Next, we can employ a standard group_by summarize pattern to quickly compute summary statistics.

```r
# group by track group
groupCat <- dsAllStorms %>% group_by(trackGroup) %>%
  summarize(numMeasures = n(),
            meanPres = round(mean(as.numeric(WMO_PRES), na.rm = TRUE),0),
            medPres = median(as.numeric(WMO_PRES), na.rm = TRUE),
            sdPres = round(sd(as.numeric(WMO_PRES), na.rm = TRUE),0),
            meanWndSpd = round(mean(as.numeric(WMO_WIND), na.rm = TRUE),0),
            medWndSpd = median(as.numeric(WMO_WIND), na.rm = TRUE),
            sdWndSpd = round(sd(as.numeric(WMO_WIND), na.rm = TRUE),0),
            maxCat = max(as.numeric(USA_SSHS)),
            medCat = median(as.numeric(USA_SSHS)),
            medMonth = median(month(ISO_TIME))
            
  )
```

Most of the variable names are We can display the resulting data table nicely using kable.

```r
# we take the transpose with the t() and then put row names in place.
fancyGroupCat <- groupCat %>% t()
colnames(fancyGroupCat) <- fancyGroupCat[1,]
fancyGroupCat <- fancyGroupCat[-1,]

kable(fancyGroupCat, caption = "Stats Across all Measures by Group")
```
<div align="center">
    <img src="../images/trackStatsKable.png" alt="Data table" width="200">
</div>

Similar operations create tables for the auxiliary categories, so I will omit the code for brevity.

The code is then combined into a single dataset.

```r

# takes a list of datasets to join and the method to "reduce" them together.
groupTable <- list(groupAllCat, groupCat, groupTrailingCat, groupDistantCat, groupDragXHCat) %>%
  purrr::reduce(full_join)

# combine the different category columns into one column.
# unite names the new col 'cat' then combines the other 5, removing nas.
groupTable <- groupTable %>% 
  unite(col = 'cat','cat', 'trailingCat', 'distantCat', 'dragXHCat', 'allJoiner', na.rm = TRUE) 

# move the created cat column from the far right (index is total number of columns)
# to the front, then put the range of the second column to the second to last column after
# this moves cat to the far left
groupTable <- groupTable[,c(ncol(groupTable), 2:(ncol(groupTable)-1))]
```

Critically, the summary stats produced this way are not evenly weighted by storm, they are weighted by measure. This means that storms which stick around longer (like Hurricane Harvey) and therefore have recieved more measurements (taken regularly every three hours), bias the sample.

An alternative that is less biased is to group by storm instead. This leaves you with only 19 data points for future statistical analysis instead of 400, but removes major biases.

The code for this storm-based grouping is omitted for brevity.

# Building Shiny Apps

In this section, I will discuss the construction of the Shiny app that allowed easy analysis of the basic statistical comparison tests possible with our sample of 19 hurricanes, using the by storm grouping discussed in the last section.

```r
#ibtracs data grouped by storm
dsByStorm <- read_csv('Data/ibtracsData/ibtracsByStorm.csv')
```

This section requires a functon from an [outside source](http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software). The idea here is that we will simultaniously compute a matrix of p-values and a matrix of correlations containing all possible combinations of our data variables. This function will allow for easy manipulation of these matrices.

```r
#FUNCTIONS

#Turns correlation (cor) and p value matrices into a data table I can filter and analyze.

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}
```
We do the needed computations:

```r
# compute correlation matrix for dsByStorm
# select columns with numeric values that make sense to compare
byStormNumeric <- dsByStorm %>% select(month, numMeasures:maxSSHS)

# create the corr matrix, selecting only observations with a complete pair.
byStormCorM <- cor(byStormNumeric, use = 'pairwise.complete.obs')

# round
byStormCorM <- round(fullCorM, 2)

# this package takes the data from above and gives the same corr matrix
# alongside a matrix with the count of values and a matrix of p values.
byStormStats <- Hmisc::rcorr(as.matrix(byStormNumeric))

# this uses the external function to turn the the corr and p value matrices into
# a data table which I can filter to look for interesting patterns.
byStormStatTable <- flattenCorrMatrix(byStormStats$r, byStormStats$P)

# filter the completed data table to only include significant correlations with p<0.05
# arrange in descending order of raw correlation strength (abs of cor)
byStormSig <- byStormStatTable %>% filter(p <= 0.05) %>%
  arrange(desc(abs(cor)))

# remove values in byStormSig where mean of one variable is correlated to its standard deviation...
# manually selecting columns that compare different variables:
byStormSig <- byStormSig[c(11,14,15,20:25,28:30,35,37,38,40:43,46,48:52,55:63,65:72,74:80,82:87),] %>% arrange(row, column)

#The p value matrices from above, for use in app
fullPM <- fullStats$P
byStormPM <- byStormStats$P
```

Now, we are ready to run the shiny app.

```r
# One Data Point Per Hurricane App

ui <- fluidPage(
  
  # application title
  titlePanel("Data For Each Hurricane"),
  
  # the first row. Each row has capacity 12 width, spread across its columns
  fluidRow(
    
    # the first two columns let you select variables called selX and selY
    column(4,
           varSelectInput("selX", 
                        "Select x-axis Variable", 
                      data = dsByStorm %>% select(month, numMeasures:maxSSHS))
  ),
    
    column(4,
           varSelectInput("selY", 
                        "Select y-axis Variable", 
                      data = dsByStorm %>% select(month, numMeasures:maxSSHS))
  ),
           
    # the third column lets you select the variable that colors the plot
    column(4,
           varSelectInput("selCol", 
                        "Select Color Variable", 
                      data = dsByStorm %>% select(cat, trailingCat, distantCat, dragXHCat, dirCat))
  ),

  # this new row shows a plotOutput called "plot".
  # it needs a column of 12 to be defined to scale correctly
  fluidRow(
    column(12,
          plotOutput("plot") )
    )
))


# define server logic required to draw a scatterplot
server <- function(input, output) {
  
  
  # the output called "plot" is now rendered using the selected input variables.
  # throughout, the format ds[[input$variable]] is used to 
  # eliminate glitches caused by !!input that are used in class notes
  output$plot <- renderPlot({
    
    corr = round(cor(dsByStorm[[input$selX]], dsByStorm[[input$selY]], use = 'pairwise.complete.obs'),2)
    
    pVal <- round(byStormPM[paste0(input$selY),paste0(input$selX)],2)
  
    #Creating the geom point
    ggplot(data = dsByStorm) +
    geom_point(mapping = aes(x = dsByStorm[[input$selX]], y = dsByStorm[[input$selY]], color = dsByStorm[[input$selCol]]), na.rm = TRUE) +
    xlab(paste0(input$selX)) + ylab(paste0(input$selY)) +
    ggtitle(paste0(input$selY,' vs. ',input$selX,': Correlation: ',corr,', p-value: ',pVal)) +
    
    # special code to alter the plot title  
    theme(plot.title = element_text(size = 20, face = "bold")) +
      
    # add a label to the color bar  
    labs(color = paste0(input$selCol))
  })
}

# run the application 
shinyApp(ui = ui, server = server)
```

When ran, the app appears as follows:

![Shiny Demo](../images/shinyDemo.png)

The variable SSHS refers to the Saffir-Simpson Hurricane Scale which ranks hurricanes from Category 1 to Category 5. This dataset uses negative values for subtropical storms (-2), tropical depressions (-1), and tropical storms (0).

# Useful Trends

Using this Shiny app, we found that high wind speeds are tightly correlated with lower pressures (not surprising but a good proof of concept). Much more intriguing is that stronger storms (both high winds and low pressure) tend to have faster movement speed. This trend is most significant when comparing mean or median values of strength measures to minimum speed. This implies that stronger storms tend to not ever slow down to a crawl. There could be a trivial interpretation here where storms moving faster have less time to weaken over land while dropping rain in the area so their strength metrics tend to be higher.

Due to our small sample size and imformal statistical approach, this analysis is not ironclad, however this potential relationship between speed and strength does offer a potential explanation for why the generally more intense northeast storm category (which include the category 5 Hurricane Rita) don't necesarily produce the worst river flooding.

