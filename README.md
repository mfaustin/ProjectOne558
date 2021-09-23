Vignette
================
Mark Austin
10/05/2021

-   [Packages Used](#packages-used)
-   [API Functions](#api-functions)
-   [Exploring Data](#exploring-data)

## Packages Used

``` r
   library(tidyverse)
   library(jsonlite)
```

## API Functions

``` r
getPokeNameID <- function(sortName=FALSE){
  
  apiData<-fromJSON("https://pokeapi.co/api/v2/pokemon/?limit=1222")
  
  allNames<-as_tibble(apiData$results)
  
  allNames<-allNames %>% mutate(ID=basename(url)) %>% select(-url)
  
  if (sortName) {
    allNames<-allNames %>% arrange(name)
  }
  
  return(allNames)
  
}

getPokeNameID(sortName = TRUE)
```

    ## # A tibble: 1,118 x 2
    ##    name             ID   
    ##    <chr>            <chr>
    ##  1 abomasnow        460  
    ##  2 abomasnow-mega   10060
    ##  3 abra             63   
    ##  4 absol            359  
    ##  5 absol-mega       10057
    ##  6 accelgor         617  
    ##  7 aegislash-blade  10026
    ##  8 aegislash-shield 681  
    ##  9 aerodactyl       142  
    ## 10 aerodactyl-mega  10042
    ## # ... with 1,108 more rows

## Exploring Data
