Vignette
================
Mark Austin
10/05/2021

-   [Packages Used](#packages-used)
-   [API Functions](#api-functions)
-   [Exploring Data](#exploring-data)

## Packages Used

`tidyverse`

`jsonlite`

``` r
   library(tidyverse)
   library(jsonlite)
```

## API Functions

``` r
getPokeNameID <- function(sortName=FALSE){
  
  apiData<-fromJSON("https://pokeapi.co/api/v2/pokemon/?limit=1222")
  
  allNames<-as_tibble(apiData$results)
  
  allNames<-allNames %>% mutate(ID=as.numeric(basename(url)))
  
  if (sortName) {
    allNames<-allNames %>% arrange(name)
  }
  
  return(allNames)
  
}
```

Example usage with output

``` r
getPokeNameID(sortName = TRUE)
```

    ## # A tibble: 1,118 x 3
    ##    name             url                                         ID
    ##    <chr>            <chr>                                    <dbl>
    ##  1 abomasnow        https://pokeapi.co/api/v2/pokemon/460/     460
    ##  2 abomasnow-mega   https://pokeapi.co/api/v2/pokemon/10060/ 10060
    ##  3 abra             https://pokeapi.co/api/v2/pokemon/63/       63
    ##  4 absol            https://pokeapi.co/api/v2/pokemon/359/     359
    ##  5 absol-mega       https://pokeapi.co/api/v2/pokemon/10057/ 10057
    ##  6 accelgor         https://pokeapi.co/api/v2/pokemon/617/     617
    ##  7 aegislash-blade  https://pokeapi.co/api/v2/pokemon/10026/ 10026
    ##  8 aegislash-shield https://pokeapi.co/api/v2/pokemon/681/     681
    ##  9 aerodactyl       https://pokeapi.co/api/v2/pokemon/142/     142
    ## 10 aerodactyl-mega  https://pokeapi.co/api/v2/pokemon/10042/ 10042
    ## # ... with 1,108 more rows

``` r
getOnePokeData<-function(pokemon,basestat=FALSE,type=FALSE){
  
  ##Get list of pokemon and process user pokemon input
  pokeNameID<-getPokeNameID()
  
  if (is.numeric(pokemon)){
    pokeNameID<-pokeNameID%>%filter(ID==pokemon)
  } else if (is.character(pokemon)){
    pokeNameID<-pokeNameID%>%filter(name==tolower(pokemon))
  } else {
    stop("Please enter either pokemon integer or quoated name value")
  }
  
  PokeList<- fromJSON(pokeNameID$url,flatten = TRUE)
  
  ###Function Default Data
  name<-PokeList$name
  height<-PokeList$height
  id<-PokeList$id
  species<-PokeList$species$name
  weight<-PokeList$weight
  base_experience<-PokeList$base_experience
  
  LocalDF<-data.frame(name,id,species,height,weight,base_experience)
  
  ##process and add base stat data if user selects basestat TRUE
  if (basestat){
    hp<-PokeList$stats$base_stat[1]
    attack<-PokeList$stats$base_stat[2]
    defense<-PokeList$stats$base_stat[3]
    special_attack<-PokeList$stats$base_stat[4]
    special_defense<-PokeList$stats$base_stat[5]
    speed<-PokeList$stats$base_stat[6]
    
    LocalDF<-LocalDF%>%mutate(hp,attack,defense,special_attack   ,special_defense ,speed)
  }
  
  ##process and add type data if user selects type TRUE
  if(type){
    ##All pokemon has at least one type so assign here
    type_one<-PokeList$types$type.name[1]
    
    ##check if more than one type and set 
    ##second type as needed
    if(length(PokeList$types$slot)>1){
      type_two<-PokeList$types$type.name[2]
    }else{
      type_two<-"None"
    }
    
    LocalDF<-LocalDF%>%mutate(type_one,type_two)
  }
  
  
  return(LocalDF)
  
}
```

Examples of ways to use getOnePokeData()

``` r
getOnePokeData("Venusaur")
getOnePokeData(pokemon=8,basestat = TRUE)
getOnePokeData(435,type = TRUE)
getOnePokeData(10032,basestat = TRUE,type = TRUE)
```

``` r
getEveryPokeData<-function(basestat=FALSE,type=FALSE){
  
  ###Get current number of pokemon to process
  #getPokeNameID
  pokeNameID<-getPokeNameID()
  idVals<-pokeNameID$ID
  #idVals<-1:2
  
  ###Loop through every pokemon and build data frame
  ###by adding new rows
  ###Most of the time spent here is in the numerous 
  ###   calls to API address since there are so many pokemon
  allPoke<-data.frame()
  for (i in idVals) {
    allPoke<-rbind(allPoke,getOnePokeData(i,basestat,type))
  }
  
  return(allPoke)
}
#getEveryPokeData()
```

## Exploring Data

``` r
allPoke<-getEveryPokeData(basestat = TRUE,type = TRUE)
```

``` r
head(allPoke)
```

    ##         name id    species height weight base_experience hp attack defense
    ## 1  bulbasaur  1  bulbasaur      7     69              64 45     49      49
    ## 2    ivysaur  2    ivysaur     10    130             142 60     62      63
    ## 3   venusaur  3   venusaur     20   1000             236 80     82      83
    ## 4 charmander  4 charmander      6     85              62 39     52      43
    ## 5 charmeleon  5 charmeleon     11    190             142 58     64      58
    ## 6  charizard  6  charizard     17    905             240 78     84      78
    ##   special_attack special_defense speed type_one type_two
    ## 1             65              65    45    grass   poison
    ## 2             80              80    60    grass   poison
    ## 3            100             100    80    grass   poison
    ## 4             60              50    65     fire     None
    ## 5             80              65    80     fire     None
    ## 6            109              85   100     fire   flying

``` r
g<-ggplot(data = allPoke,aes(x=weight,y=height))
g+geom_point(aes(color=type_one))
```

![](images/plotheightweight-1.png)<!-- -->

``` r
allPoke %>% group_by(type_one) %>% summarise(mweight=mean(weight),sdweight=sd(weight))
```

    ## # A tibble: 18 x 3
    ##    type_one mweight sdweight
    ##    <chr>      <dbl>    <dbl>
    ##  1 bug         576.    1560.
    ##  2 dark        786.    1874.
    ##  3 dragon     1331.    1385.
    ##  4 electric    705.    1936.
    ##  5 fairy       788.    2128.
    ##  6 fighting   1399.    2617.
    ##  7 fire       1061.    2071.
    ##  8 flying     1478.    3211.
    ##  9 ghost       802.    2025.
    ## 10 grass       800.    2032.
    ## 11 ground     1540.    2777.
    ## 12 ice        1095.    1571.
    ## 13 normal      683.    1659.
    ## 14 poison     1051.    2456.
    ## 15 psychic     892.    1925.
    ## 16 rock       1111.    1718.
    ## 17 steel      2850.    3479.
    ## 18 water       926.    1915.

``` r
allPoke %>% summary()
```

    ##      name                 id            species              height       
    ##  Length:1118        Min.   :    1.0   Length:1118        Min.   :   1.00  
    ##  Class :character   1st Qu.:  280.2   Class :character   1st Qu.:   5.00  
    ##  Mode  :character   Median :  559.5   Mode  :character   Median :  10.00  
    ##                     Mean   : 2350.6                      Mean   :  21.43  
    ##                     3rd Qu.:  838.8                      3rd Qu.:  16.00  
    ##                     Max.   :10220.0                      Max.   :1000.00  
    ##      weight        base_experience       hp             attack      
    ##  Min.   :    1.0   Min.   : 36     Min.   :  1.00   Min.   :  5.00  
    ##  1st Qu.:   88.0   1st Qu.: 70     1st Qu.: 50.00   1st Qu.: 55.00  
    ##  Median :  302.5   Median :162     Median : 68.00   Median : 78.50  
    ##  Mean   :  993.3   Mean   :157     Mean   : 70.03   Mean   : 80.68  
    ##  3rd Qu.:  800.0   3rd Qu.:207     3rd Qu.: 80.75   3rd Qu.:100.00  
    ##  Max.   :10000.0   Max.   :608     Max.   :255.00   Max.   :190.00  
    ##     defense       special_attack   special_defense      speed       
    ##  Min.   :  5.00   Min.   : 10.00   Min.   : 20.00   Min.   :  5.00  
    ##  1st Qu.: 50.25   1st Qu.: 50.00   1st Qu.: 50.00   1st Qu.: 45.00  
    ##  Median : 70.00   Median : 65.00   Median : 70.00   Median : 67.00  
    ##  Mean   : 74.76   Mean   : 73.23   Mean   : 72.57   Mean   : 69.48  
    ##  3rd Qu.: 90.00   3rd Qu.: 95.00   3rd Qu.: 90.00   3rd Qu.: 90.00  
    ##  Max.   :250.00   Max.   :194.00   Max.   :250.00   Max.   :200.00  
    ##    type_one           type_two        
    ##  Length:1118        Length:1118       
    ##  Class :character   Class :character  
    ##  Mode  :character   Mode  :character  
    ##                                       
    ##                                       
    ## 

``` r
summary(allPoke$weight)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     1.0    88.0   302.5   993.3   800.0 10000.0

``` r
cor(allPoke$hp,allPoke$weight)
```

    ## [1] 0.3641855

``` r
cor(allPoke$height,allPoke$weight)
```

    ## [1] 0.7527635

``` r
shortPoke<-allPoke %>% filter(height<500) 
cor(shortPoke$height,shortPoke$weight)
```

    ## [1] 0.8344578

``` r
summary(shortPoke$height)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     1.0     5.0    10.0    19.9    16.0   430.0

``` r
g<-ggplot(data = shortPoke,aes(x=weight,y=height))
g+geom_point(aes(color=type_one))
```

![](images/summarystats-1.png)<!-- -->
