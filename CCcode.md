---
title: "MapBiomasClassification"
output: 
  html_document: 
    keep_md: yes
---



```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------------------------------------------ tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.4
## v tibble  1.4.2     v dplyr   0.7.5
## v tidyr   0.8.1     v stringr 1.3.1
## v readr   1.1.1     v forcats 0.3.0
```

```
## -- Conflicts --------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(raster)
```

```
## Loading required package: sp
```

```
## 
## Attaching package: 'raster'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```r
library(readxl)
```
##Load Data
Maps from Zip

```r
unzip(zipfile="MapBiomas_23_ASCII_unclassified_allYears.zip",files="ASCII/brazillc_2000_5km_int.txt")  # unzip your file 

map <- raster("ASCII/brazillc_2000_5km_int.txt")
```


Classification from Excel

```r
classification <- read_excel("MapBiomas_CRAFTY_classifications.xlsx", sheet = "PastureB", range="B1:C21", col_names=F)  
```

MapBiomas land areas from csv

```r
mb_areas <- read_csv("LandCover Data - MapBiomas - Collection 2.3 - 2018.01.04 Municipios.csv")  
```


##Classify

##Compare map areas to MapBiomas area 



##Clean up

```r
unlink("ASCII", recursive = T)
```
