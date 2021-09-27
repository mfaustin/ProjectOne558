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
allPoke
```

    ##          name id    species height weight base_experience  hp attack defense
    ## 1   bulbasaur  1  bulbasaur      7     69              64  45     49      49
    ## 2     ivysaur  2    ivysaur     10    130             142  60     62      63
    ## 3    venusaur  3   venusaur     20   1000             236  80     82      83
    ## 4  charmander  4 charmander      6     85              62  39     52      43
    ## 5  charmeleon  5 charmeleon     11    190             142  58     64      58
    ## 6   charizard  6  charizard     17    905             240  78     84      78
    ## 7    squirtle  7   squirtle      5     90              63  44     48      65
    ## 8   wartortle  8  wartortle     10    225             142  59     63      80
    ## 9   blastoise  9  blastoise     16    855             239  79     83     100
    ## 10   caterpie 10   caterpie      3     29              39  45     30      35
    ## 11    metapod 11    metapod      7     99              72  50     20      55
    ## 12 butterfree 12 butterfree     11    320             178  60     45      50
    ## 13     weedle 13     weedle      3     32              39  40     35      30
    ## 14     kakuna 14     kakuna      6    100              72  45     25      50
    ## 15   beedrill 15   beedrill     10    295             178  65     90      40
    ## 16     pidgey 16     pidgey      3     18              50  40     45      40
    ## 17  pidgeotto 17  pidgeotto     11    300             122  63     60      55
    ## 18    pidgeot 18    pidgeot     15    395             216  83     80      75
    ## 19    rattata 19    rattata      3     35              51  30     56      35
    ## 20   raticate 20   raticate      7    185             145  55     81      60
    ## 21    spearow 21    spearow      3     20              52  40     60      30
    ## 22     fearow 22     fearow     12    380             155  65     90      65
    ## 23      ekans 23      ekans     20     69              58  35     60      44
    ## 24      arbok 24      arbok     35    650             157  60     95      69
    ## 25    pikachu 25    pikachu      4     60             112  35     55      40
    ## 26     raichu 26     raichu      8    300             218  60     90      55
    ## 27  sandshrew 27  sandshrew      6    120              60  50     75      85
    ## 28  sandslash 28  sandslash     10    295             158  75    100     110
    ## 29  nidoran-f 29  nidoran-f      4     70              55  55     47      52
    ## 30   nidorina 30   nidorina      8    200             128  70     62      67
    ## 31  nidoqueen 31  nidoqueen     13    600             227  90     92      87
    ## 32  nidoran-m 32  nidoran-m      5     90              55  46     57      40
    ## 33   nidorino 33   nidorino      9    195             128  61     72      57
    ## 34   nidoking 34   nidoking     14    620             227  81    102      77
    ## 35   clefairy 35   clefairy      6     75             113  70     45      48
    ## 36   clefable 36   clefable     13    400             217  95     70      73
    ## 37     vulpix 37     vulpix      6     99              60  38     41      40
    ## 38  ninetales 38  ninetales     11    199             177  73     76      75
    ## 39 jigglypuff 39 jigglypuff      5     55              95 115     45      20
    ## 40 wigglytuff 40 wigglytuff     10    120             196 140     70      45
    ## 41      zubat 41      zubat      8     75              49  40     45      35
    ## 42     golbat 42     golbat     16    550             159  75     80      70
    ## 43     oddish 43     oddish      5     54              64  45     50      55
    ## 44      gloom 44      gloom      8     86             138  60     65      70
    ## 45  vileplume 45  vileplume     12    186             221  75     80      85
    ## 46      paras 46      paras      3     54              57  35     70      55
    ## 47   parasect 47   parasect     10    295             142  60     95      80
    ## 48    venonat 48    venonat     10    300              61  60     55      50
    ## 49   venomoth 49   venomoth     15    125             158  70     65      60
    ## 50    diglett 50    diglett      2      8              53  10     55      25
    ## 51    dugtrio 51    dugtrio      7    333             149  35    100      50
    ## 52     meowth 52     meowth      4     42              58  40     45      35
    ## 53    persian 53    persian     10    320             154  65     70      60
    ## 54    psyduck 54    psyduck      8    196              64  50     52      48
    ## 55    golduck 55    golduck     17    766             175  80     82      78
    ## 56     mankey 56     mankey      5    280              61  40     80      35
    ## 57   primeape 57   primeape     10    320             159  65    105      60
    ## 58  growlithe 58  growlithe      7    190              70  55     70      45
    ## 59   arcanine 59   arcanine     19   1550             194  90    110      80
    ## 60    poliwag 60    poliwag      6    124              60  40     50      40
    ## 61  poliwhirl 61  poliwhirl     10    200             135  65     65      65
    ## 62  poliwrath 62  poliwrath     13    540             230  90     95      95
    ## 63       abra 63       abra      9    195              62  25     20      15
    ## 64    kadabra 64    kadabra     13    565             140  40     35      30
    ## 65   alakazam 65   alakazam     15    480             225  55     50      45
    ## 66     machop 66     machop      8    195              61  70     80      50
    ## 67    machoke 67    machoke     15    705             142  80    100      70
    ## 68    machamp 68    machamp     16   1300             227  90    130      80
    ## 69 bellsprout 69 bellsprout      7     40              60  50     75      35
    ## 70 weepinbell 70 weepinbell     10     64             137  65     90      50
    ## 71 victreebel 71 victreebel     17    155             221  80    105      65
    ##    special_attack special_defense speed type_one type_two
    ## 1              65              65    45    grass   poison
    ## 2              80              80    60    grass   poison
    ## 3             100             100    80    grass   poison
    ## 4              60              50    65     fire     None
    ## 5              80              65    80     fire     None
    ## 6             109              85   100     fire   flying
    ## 7              50              64    43    water     None
    ## 8              65              80    58    water     None
    ## 9              85             105    78    water     None
    ## 10             20              20    45      bug     None
    ## 11             25              25    30      bug     None
    ## 12             90              80    70      bug   flying
    ## 13             20              20    50      bug   poison
    ## 14             25              25    35      bug   poison
    ## 15             45              80    75      bug   poison
    ## 16             35              35    56   normal   flying
    ## 17             50              50    71   normal   flying
    ## 18             70              70   101   normal   flying
    ## 19             25              35    72   normal     None
    ## 20             50              70    97   normal     None
    ## 21             31              31    70   normal   flying
    ## 22             61              61   100   normal   flying
    ## 23             40              54    55   poison     None
    ## 24             65              79    80   poison     None
    ## 25             50              50    90 electric     None
    ## 26             90              80   110 electric     None
    ## 27             20              30    40   ground     None
    ## 28             45              55    65   ground     None
    ## 29             40              40    41   poison     None
    ## 30             55              55    56   poison     None
    ## 31             75              85    76   poison   ground
    ## 32             40              40    50   poison     None
    ## 33             55              55    65   poison     None
    ## 34             85              75    85   poison   ground
    ## 35             60              65    35    fairy     None
    ## 36             95              90    60    fairy     None
    ## 37             50              65    65     fire     None
    ## 38             81             100   100     fire     None
    ## 39             45              25    20   normal    fairy
    ## 40             85              50    45   normal    fairy
    ## 41             30              40    55   poison   flying
    ## 42             65              75    90   poison   flying
    ## 43             75              65    30    grass   poison
    ## 44             85              75    40    grass   poison
    ## 45            110              90    50    grass   poison
    ## 46             45              55    25      bug    grass
    ## 47             60              80    30      bug    grass
    ## 48             40              55    45      bug   poison
    ## 49             90              75    90      bug   poison
    ## 50             35              45    95   ground     None
    ## 51             50              70   120   ground     None
    ## 52             40              40    90   normal     None
    ## 53             65              65   115   normal     None
    ## 54             65              50    55    water     None
    ## 55             95              80    85    water     None
    ## 56             35              45    70 fighting     None
    ## 57             60              70    95 fighting     None
    ## 58             70              50    60     fire     None
    ## 59            100              80    95     fire     None
    ## 60             40              40    90    water     None
    ## 61             50              50    90    water     None
    ## 62             70              90    70    water fighting
    ## 63            105              55    90  psychic     None
    ## 64            120              70   105  psychic     None
    ## 65            135              95   120  psychic     None
    ## 66             35              35    35 fighting     None
    ## 67             50              60    45 fighting     None
    ## 68             65              85    55 fighting     None
    ## 69             70              30    40    grass   poison
    ## 70             85              45    55    grass   poison
    ## 71            100              70    70    grass   poison
    ##  [ reached 'max' / getOption("max.print") -- omitted 1047 rows ]

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
