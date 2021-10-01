Pokemon API Vignette
================
Mark Austin
10/05/2021

-   [Required R Packages](#required-r-packages)
-   [Pokemon API Query and Data Proccessing
    Functions](#pokemon-api-query-and-data-proccessing-functions)
    -   [Pokemon Endpoint Functions.](#pokemon-endpoint-functions)
    -   [Species Endpoint Functions](#species-endpoint-functions)
    -   [Evolution Chain Endpoint
        Functions.](#evolution-chain-endpoint-functions)
-   [Exploring Data](#exploring-data)
    -   [Get Full Data Frames](#get-full-data-frames)
    -   [Creating New Variables](#creating-new-variables)
    -   [Contingency Tables](#contingency-tables)
    -   [Box Plot](#box-plot)

## Required R Packages

The following R packages are required to run R code used in this
Vignette and/or create this document.

-   `tidyverse` The tidyverse package is used for data handling and
    plotting.

-   `jsonlite` The jsonlite package is used to contact the API and
    return data.

-   `knitr` The knitr package is used for document image handling.

-   `rmarkdown` The rmarkdown package is used by a render program to
    render this document.

## Pokemon API Query and Data Proccessing Functions

I created the following functions to query and process data from the
[Pokemon API](https://pokeapi.co/) using [Pokemon API
Documentation](https://pokeapi.co/docs/v2).

### Pokemon Endpoint Functions.

Most data relevant to individual pokemon is obtained from the [Pokemon
endpoint](https://pokeapi.co/docs/v2#pokemon). This endpoint returns a
complex list of lists with more data than most users would need. I’ve
provided three functions to query and process pokemon endpoint data. The
functions all return data frames.

1.  `getPokeNameIDFunction` In order to query individual pokemon, the
    user must provide either a name or id value. This function returns a
    list of all possible pokemon for this endpoint so that the user will
    know what pokemon are available. The names can be sorted as an
    option.

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

Example `getPokeNameIDFunction` usage with output.

``` r
head(getPokeNameID(sortName = TRUE))
```

<div class="kable-table">

| name           | url                                        |    ID |
|:---------------|:-------------------------------------------|------:|
| abomasnow      | <https://pokeapi.co/api/v2/pokemon/460/>   |   460 |
| abomasnow-mega | <https://pokeapi.co/api/v2/pokemon/10060/> | 10060 |
| abra           | <https://pokeapi.co/api/v2/pokemon/63/>    |    63 |
| absol          | <https://pokeapi.co/api/v2/pokemon/359/>   |   359 |
| absol-mega     | <https://pokeapi.co/api/v2/pokemon/10057/> | 10057 |
| accelgor       | <https://pokeapi.co/api/v2/pokemon/617/>   |   617 |

</div>

2.  `getOnePokeData` Given a pokemon name or id, this function returns a
    data frame with data for that pokemon. Given how much data is
    available and the complexity of processing data, I give the user a
    few options for the amount of data returned. The default option
    returns top level data including
    `species,height,weight,base_experience`. Turning the basestat
    function additionally returns
    `hp,attack,defense,special_attack,special_defense ,speed`. Finally,
    turning the type option on additionally returns primary and
    secondary types `type_one,type_two`.

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

Examples of ways to use `getOnePokeData`.

``` r
getOnePokeData("Venusaur")
getOnePokeData(pokemon=8,basestat = TRUE)
getOnePokeData(435,type = TRUE)
getOnePokeData(10032,basestat = TRUE,type = TRUE)
```

3.  `getEveryPokeData` This function returns data for ALL pokemon and
    returns one data frame. The amount of data returned is dependent on
    the basetat and type options as described in `getOnePokeData`.

``` r
getEveryPokeData<-function(basestat=FALSE,type=FALSE){
  
  ###Get current number of pokemon to process
  #getPokeNameID
  pokeNameID<-getPokeNameID()
  idVals<-pokeNameID$ID
  
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
```

Example of `getEveryPokeData` data frame data.

``` r
everyPoke<-getEveryPokeData(basestat = TRUE,type = TRUE)
head(everyPoke)
```

<div class="kable-table">

| name       |  id | species    | height | weight | base\_experience |  hp | attack | defense | special\_attack | special\_defense | speed | type\_one | type\_two |
|:-----------|----:|:-----------|-------:|-------:|-----------------:|----:|-------:|--------:|----------------:|-----------------:|------:|:----------|:----------|
| bulbasaur  |   1 | bulbasaur  |      7 |     69 |               64 |  45 |     49 |      49 |              65 |               65 |    45 | grass     | poison    |
| ivysaur    |   2 | ivysaur    |     10 |    130 |              142 |  60 |     62 |      63 |              80 |               80 |    60 | grass     | poison    |
| venusaur   |   3 | venusaur   |     20 |   1000 |              236 |  80 |     82 |      83 |             100 |              100 |    80 | grass     | poison    |
| charmander |   4 | charmander |      6 |     85 |               62 |  39 |     52 |      43 |              60 |               50 |    65 | fire      | None      |
| charmeleon |   5 | charmeleon |     11 |    190 |              142 |  58 |     64 |      58 |              80 |               65 |    80 | fire      | None      |
| charizard  |   6 | charizard  |     17 |    905 |              240 |  78 |     84 |      78 |             109 |               85 |   100 | fire      | flying    |

</div>

### Species Endpoint Functions

Most pokemon species map to one individual pokemon but there are some
species that map to several indidual pokemon. Collective species data is
obtained from the [Pokemon Species
endpoint](https://pokeapi.co/docs/v2#pokemon-species). Because species
data is less complex, I was able to return more default data from this
endpoint than the pokemon endpoint. I’ve provided three functions to
query and process pokemon endpoint data. The functions all return data
frames.

1.  `getSpeciesNameID` This function returns a data frame with a list of
    possible species names and id values so that the user will know what
    is available. Optional sorting my name is provided.

``` r
getSpeciesNameID <- function(sortName=FALSE){
  
  apiData<-fromJSON("https://pokeapi.co/api/v2/pokemon-species/?limit=1222")
  
  allNames<-as_tibble(apiData$results)
  
  allNames<-allNames %>% mutate(ID=as.numeric(basename(url)))
  
  if (sortName) {
    allNames<-allNames %>% arrange(name)
  }
  
  return(allNames)
  
}
```

2.  `getOneSpeciesData` Given species name or id this function returns a
    data frame for one species with the following data.
    `species,shape,generation,base_happiness,capture_rate,gender_rate,hatch_counter,is_baby,is_legendary,is_mythical`.

``` r
getOneSpeciesData<-function(species){
   
   ##Get list of species and process user species input
   pokeSpeciesID<-getSpeciesNameID()
   
   if (is.numeric(species)){
     pokeSpeciesID<-pokeSpeciesID%>%filter(ID==species)
   } else if (is.character(species)){
     pokeSpeciesID<-pokeSpeciesID%>%filter(name==tolower(species))
   } else {
     stop("Please enter either species integer or quoated name value")
   }
   
   PokeList<- fromJSON(pokeSpeciesID$url,flatten = TRUE)
   
   ###Function Data to return
   species<-PokeList$name
   shape<-PokeList$shape$name
   generation<-PokeList$generation$name
   base_happiness<-PokeList$base_happiness
   capture_rate<-PokeList$capture_rate
   gender_rate<-PokeList$gender_rate
   hatch_counter<-PokeList$hatch_counter
   is_baby<-PokeList$is_baby
   is_legendary<-PokeList$is_legendary
   is_mythical<-PokeList$is_mythical

   
   LocalDF<-data.frame(species,shape,generation,base_happiness,  
            capture_rate,gender_rate,hatch_counter,  
            is_baby,is_legendary,is_mythical)
   

   
   return(LocalDF)
   
 }
```

3.  `getEverySpeciesData` This function returns data for every species
    as a data frame with optional sorting of the data based on the
    sortName option. The following data is returned  
    `species,shape,generation,base_happiness,capture_rate,gender_rate,hatch_counter,is_baby,is_legendary,is_mythical`.

``` r
getEverySpeciesData<-function(sortName=FALSE){
   
   ###Get current number of species to process
   pokeSpeciesID<-getSpeciesNameID()
   idVals<-pokeSpeciesID$ID
   
   
   ###Loop through every species and build data frame
   ###by adding new rows
   ###Most of the time spent here is in the numerous 
   ###   calls to API address since there are so many species
   allPoke<-data.frame()
   for (i in idVals) {
     allPoke<-rbind(allPoke,getOneSpeciesData(i))
   }
   
   if (sortName) {
     allPoke<-allPoke %>% arrange(species)
   }
   
   return(allPoke)
 }
```

Example of `getEverySpeciesData` data frame data.

``` r
everyPokeSpecies<-getEverySpeciesData(sortName = TRUE)
head(everyPokeSpecies)
```

<div class="kable-table">

| species    | shape     | generation     | base\_happiness | capture\_rate | gender\_rate | hatch\_counter | is\_baby | is\_legendary | is\_mythical |
|:-----------|:----------|:---------------|----------------:|--------------:|-------------:|---------------:|:---------|:--------------|:-------------|
| abomasnow  | upright   | generation-iv  |              70 |            60 |            4 |             20 | FALSE    | FALSE         | FALSE        |
| abra       | upright   | generation-i   |              70 |           200 |            2 |             20 | FALSE    | FALSE         | FALSE        |
| absol      | quadruped | generation-iii |              35 |            30 |            4 |             25 | FALSE    | FALSE         | FALSE        |
| accelgor   | arms      | generation-v   |              70 |            75 |            4 |             15 | FALSE    | FALSE         | FALSE        |
| aegislash  | blob      | generation-vi  |              70 |            45 |            4 |             20 | FALSE    | FALSE         | FALSE        |
| aerodactyl | wings     | generation-i   |              70 |            45 |            1 |             35 | FALSE    | FALSE         | FALSE        |

</div>

### Evolution Chain Endpoint Functions.

Many pokemon can [evolve](https://pokemondb.net/evolution) into another
more powerful pokemon. Evolution chain data is obtained from the
[Pokemon Evolution Chain
Endpoint](https://pokeapi.co/docs/v2#evolution-chains). This endpoint
only takes ID and those IDs are linked to one part of a chain.

I’ve provided three functions to query and process pokemon evolution
chain endpoint data. The functions both return data frames.

1.  `getOneEvolveData` This function takes an ID number for one of the
    chains and returns the chain data for that chain as data frame. Each
    data frame row has a value for a chain level or None if that chain
    does not have all three stages.

``` r
getOneEvolveData<-function(ID){
  
  ###Construct URL from the given ID and call API
  basicURL<-"https://pokeapi.co/api/v2/evolution-chain/"
  queryURL<-paste0(basicURL,ID)
  queryResult<-fromJSON(queryURL)
  
  ###Parse results into stages or no evolve categories
  stageOne<-queryResult$chain$species$name
  stageTwo<-queryResult[["chain"]][["evolves_to"]][["species"]][["name"]]
  stageThree<-queryResult[["chain"]][["evolves_to"]][["evolves_to"]][[1]][["species"]][["name"]] 
  if (is.null(stageTwo)){
    stageTwo<-"None"
  }
  if (is.null(stageThree)){
    stageThree<-"None"
  }
  
  localDF<-data.frame(stageOne,stageTwo,stageThree)
  return(localDF)
}
```

An example of data frame returned from `getOneEvolveData`

``` r
  getOneEvolveData(57)
```

<div class="kable-table">

| stageOne | stageTwo | stageThree |
|:---------|:---------|:-----------|
| mime-jr  | mr-mime  | mr-rime    |

</div>

2.  `getAllEvolveSeries` This function returns a data frame of all the
    evolve stage items. The function will optionally sort on the first
    stage value.

``` r
getAllEvolveSeries<-function(sortName=FALSE){
  
  metaEvolve<-fromJSON("https://pokeapi.co/api/v2/evolution-chain/?limit=600")
  
  metaEvolveDF<-as_tibble(metaEvolve$results)
  
  metaEvolveDF<-metaEvolveDF %>% mutate(ID=as.numeric(basename(url)))
  
  ##Loop through all the ID values and build a data frame
  ## for all the evolution chain data
  allEvolve<-data.frame()
  for (loopID in metaEvolveDF$ID) {
    allEvolve<-rbind(allEvolve,getOneEvolveData(loopID))
  } 
  
   if (sortName) {
     allEvolve<-allEvolve %>% arrange(stageOne)
   }
  
  return(allEvolve)
}
```

3.  `getAllEvolveStages` This function takes data parsed by chain and
    converts the data into a data frame containing species name and
    stage value for that species. The function will optionally sort on
    species.

``` r
getAllEvolveStages<-function(sortName=FALSE){
  
  resultsEvolve<-getAllEvolveSeries()
  
  ###Handles the first one which they all have
  ###Now can do stageTwo and three
  allEvolve<-data.frame()
  species<-resultsEvolve$stageOne
  stages<-ifelse(resultsEvolve$stageTwo=="None",stage<-"noEvolve",stage<-"one")
  stages
  allEvolve<-data.frame(species,stages)
  ###Need to use rbind to add other parts after this part
  species<-resultsEvolve$stageTwo
  stages<-ifelse(resultsEvolve$stageTwo=="None",stage<-"noEvolve",stage<-"two")
  twoEvolve<-data.frame(species,stages)
  twoEvolve<-twoEvolve %>% filter(species!="None")
  allEvolve<-rbind(allEvolve,twoEvolve)
  
  species<-resultsEvolve$stageThree
  stages<-ifelse(resultsEvolve$stageThree=="None",stage<-"noEvolve",stage<-"three")
  threeEvolve<-data.frame(species,stages)
  threeEvolve<-threeEvolve %>% filter(species!="None")
  allEvolve<-rbind(allEvolve,threeEvolve)
  ###Later use distinct function to remove duplicate rows
  allEvolve<-allEvolve %>% distinct(species,.keep_all = TRUE)
  
  allEvolve$stages<-as.factor(allEvolve$stages)
  allEvolve$stages<-ordered(allEvolve$stages,levels=c("one","two","three","noEvolve"))
  
  if (sortName) {
     allEvolve<-allEvolve %>% arrange(species)
   }
  
  return(allEvolve)
}
```

An example of output from `getAllEvolveStages`.

``` r
  evolveStages<-getAllEvolveStages(sortName = TRUE)
  head(evolveStages)
```

<div class="kable-table">

| species    | stages   |
|:-----------|:---------|
| abomasnow  | two      |
| abra       | one      |
| absol      | noEvolve |
| accelgor   | two      |
| aegislash  | three    |
| aerodactyl | noEvolve |

</div>

## Exploring Data

### Get Full Data Frames

``` r
allPoke<-getEveryPokeData(basestat = TRUE,type = TRUE)
allSpecies<-getEverySpeciesData()
allStages<-getAllEvolveStages()
```

``` r
str(allPoke)
```

    ## 'data.frame':    1118 obs. of  14 variables:
    ##  $ name           : chr  "bulbasaur" "ivysaur" "venusaur" "charmander" ...
    ##  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ species        : chr  "bulbasaur" "ivysaur" "venusaur" "charmander" ...
    ##  $ height         : int  7 10 20 6 11 17 5 10 16 3 ...
    ##  $ weight         : int  69 130 1000 85 190 905 90 225 855 29 ...
    ##  $ base_experience: int  64 142 236 62 142 240 63 142 239 39 ...
    ##  $ hp             : int  45 60 80 39 58 78 44 59 79 45 ...
    ##  $ attack         : int  49 62 82 52 64 84 48 63 83 30 ...
    ##  $ defense        : int  49 63 83 43 58 78 65 80 100 35 ...
    ##  $ special_attack : int  65 80 100 60 80 109 50 65 85 20 ...
    ##  $ special_defense: int  65 80 100 50 65 85 64 80 105 20 ...
    ##  $ speed          : int  45 60 80 65 80 100 43 58 78 45 ...
    ##  $ type_one       : chr  "grass" "grass" "grass" "fire" ...
    ##  $ type_two       : chr  "poison" "poison" "poison" "None" ...

``` r
str(allSpecies)
```

    ## 'data.frame':    898 obs. of  10 variables:
    ##  $ species       : chr  "bulbasaur" "ivysaur" "venusaur" "charmander" ...
    ##  $ shape         : chr  "quadruped" "quadruped" "quadruped" "upright" ...
    ##  $ generation    : chr  "generation-i" "generation-i" "generation-i" "generation-i" ...
    ##  $ base_happiness: int  70 70 70 70 70 70 70 70 70 70 ...
    ##  $ capture_rate  : int  45 45 45 45 45 45 45 45 45 255 ...
    ##  $ gender_rate   : int  1 1 1 1 1 1 1 1 1 4 ...
    ##  $ hatch_counter : int  20 20 20 20 20 20 20 20 20 15 ...
    ##  $ is_baby       : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ is_legendary  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ is_mythical   : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...

``` r
str(allStages)
```

    ## 'data.frame':    897 obs. of  2 variables:
    ##  $ species: chr  "bulbasaur" "charmander" "squirtle" "caterpie" ...
    ##  $ stages : Ord.factor w/ 4 levels "one"<"two"<"three"<..: 1 1 1 1 1 1 1 1 1 1 ...

### Creating New Variables

[Total Point
Example](https://bulbapedia.bulbagarden.net/wiki/Kricketot_(Pok%C3%A9mon)#Base_stats)

``` r
###total points
allPoke %>% mutate(totalPts=(hp+attack+defense+special_attack   +special_defense +speed)) %>% select(name,id,species,height,weight,base_experience,totalPts,everything())
```

<div class="kable-table">

| name                       |    id | species      | height | weight | base\_experience | totalPts |  hp | attack | defense | special\_attack | special\_defense | speed | type\_one | type\_two |
|:---------------------------|------:|:-------------|-------:|-------:|-----------------:|---------:|----:|-------:|--------:|----------------:|-----------------:|------:|:----------|:----------|
| bulbasaur                  |     1 | bulbasaur    |      7 |     69 |               64 |      318 |  45 |     49 |      49 |              65 |               65 |    45 | grass     | poison    |
| ivysaur                    |     2 | ivysaur      |     10 |    130 |              142 |      405 |  60 |     62 |      63 |              80 |               80 |    60 | grass     | poison    |
| venusaur                   |     3 | venusaur     |     20 |   1000 |              236 |      525 |  80 |     82 |      83 |             100 |              100 |    80 | grass     | poison    |
| charmander                 |     4 | charmander   |      6 |     85 |               62 |      309 |  39 |     52 |      43 |              60 |               50 |    65 | fire      | None      |
| charmeleon                 |     5 | charmeleon   |     11 |    190 |              142 |      405 |  58 |     64 |      58 |              80 |               65 |    80 | fire      | None      |
| charizard                  |     6 | charizard    |     17 |    905 |              240 |      534 |  78 |     84 |      78 |             109 |               85 |   100 | fire      | flying    |
| squirtle                   |     7 | squirtle     |      5 |     90 |               63 |      314 |  44 |     48 |      65 |              50 |               64 |    43 | water     | None      |
| wartortle                  |     8 | wartortle    |     10 |    225 |              142 |      405 |  59 |     63 |      80 |              65 |               80 |    58 | water     | None      |
| blastoise                  |     9 | blastoise    |     16 |    855 |              239 |      530 |  79 |     83 |     100 |              85 |              105 |    78 | water     | None      |
| caterpie                   |    10 | caterpie     |      3 |     29 |               39 |      195 |  45 |     30 |      35 |              20 |               20 |    45 | bug       | None      |
| metapod                    |    11 | metapod      |      7 |     99 |               72 |      205 |  50 |     20 |      55 |              25 |               25 |    30 | bug       | None      |
| butterfree                 |    12 | butterfree   |     11 |    320 |              178 |      395 |  60 |     45 |      50 |              90 |               80 |    70 | bug       | flying    |
| weedle                     |    13 | weedle       |      3 |     32 |               39 |      195 |  40 |     35 |      30 |              20 |               20 |    50 | bug       | poison    |
| kakuna                     |    14 | kakuna       |      6 |    100 |               72 |      205 |  45 |     25 |      50 |              25 |               25 |    35 | bug       | poison    |
| beedrill                   |    15 | beedrill     |     10 |    295 |              178 |      395 |  65 |     90 |      40 |              45 |               80 |    75 | bug       | poison    |
| pidgey                     |    16 | pidgey       |      3 |     18 |               50 |      251 |  40 |     45 |      40 |              35 |               35 |    56 | normal    | flying    |
| pidgeotto                  |    17 | pidgeotto    |     11 |    300 |              122 |      349 |  63 |     60 |      55 |              50 |               50 |    71 | normal    | flying    |
| pidgeot                    |    18 | pidgeot      |     15 |    395 |              216 |      479 |  83 |     80 |      75 |              70 |               70 |   101 | normal    | flying    |
| rattata                    |    19 | rattata      |      3 |     35 |               51 |      253 |  30 |     56 |      35 |              25 |               35 |    72 | normal    | None      |
| raticate                   |    20 | raticate     |      7 |    185 |              145 |      413 |  55 |     81 |      60 |              50 |               70 |    97 | normal    | None      |
| spearow                    |    21 | spearow      |      3 |     20 |               52 |      262 |  40 |     60 |      30 |              31 |               31 |    70 | normal    | flying    |
| fearow                     |    22 | fearow       |     12 |    380 |              155 |      442 |  65 |     90 |      65 |              61 |               61 |   100 | normal    | flying    |
| ekans                      |    23 | ekans        |     20 |     69 |               58 |      288 |  35 |     60 |      44 |              40 |               54 |    55 | poison    | None      |
| arbok                      |    24 | arbok        |     35 |    650 |              157 |      448 |  60 |     95 |      69 |              65 |               79 |    80 | poison    | None      |
| pikachu                    |    25 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| raichu                     |    26 | raichu       |      8 |    300 |              218 |      485 |  60 |     90 |      55 |              90 |               80 |   110 | electric  | None      |
| sandshrew                  |    27 | sandshrew    |      6 |    120 |               60 |      300 |  50 |     75 |      85 |              20 |               30 |    40 | ground    | None      |
| sandslash                  |    28 | sandslash    |     10 |    295 |              158 |      450 |  75 |    100 |     110 |              45 |               55 |    65 | ground    | None      |
| nidoran-f                  |    29 | nidoran-f    |      4 |     70 |               55 |      275 |  55 |     47 |      52 |              40 |               40 |    41 | poison    | None      |
| nidorina                   |    30 | nidorina     |      8 |    200 |              128 |      365 |  70 |     62 |      67 |              55 |               55 |    56 | poison    | None      |
| nidoqueen                  |    31 | nidoqueen    |     13 |    600 |              227 |      505 |  90 |     92 |      87 |              75 |               85 |    76 | poison    | ground    |
| nidoran-m                  |    32 | nidoran-m    |      5 |     90 |               55 |      273 |  46 |     57 |      40 |              40 |               40 |    50 | poison    | None      |
| nidorino                   |    33 | nidorino     |      9 |    195 |              128 |      365 |  61 |     72 |      57 |              55 |               55 |    65 | poison    | None      |
| nidoking                   |    34 | nidoking     |     14 |    620 |              227 |      505 |  81 |    102 |      77 |              85 |               75 |    85 | poison    | ground    |
| clefairy                   |    35 | clefairy     |      6 |     75 |              113 |      323 |  70 |     45 |      48 |              60 |               65 |    35 | fairy     | None      |
| clefable                   |    36 | clefable     |     13 |    400 |              217 |      483 |  95 |     70 |      73 |              95 |               90 |    60 | fairy     | None      |
| vulpix                     |    37 | vulpix       |      6 |     99 |               60 |      299 |  38 |     41 |      40 |              50 |               65 |    65 | fire      | None      |
| ninetales                  |    38 | ninetales    |     11 |    199 |              177 |      505 |  73 |     76 |      75 |              81 |              100 |   100 | fire      | None      |
| jigglypuff                 |    39 | jigglypuff   |      5 |     55 |               95 |      270 | 115 |     45 |      20 |              45 |               25 |    20 | normal    | fairy     |
| wigglytuff                 |    40 | wigglytuff   |     10 |    120 |              196 |      435 | 140 |     70 |      45 |              85 |               50 |    45 | normal    | fairy     |
| zubat                      |    41 | zubat        |      8 |     75 |               49 |      245 |  40 |     45 |      35 |              30 |               40 |    55 | poison    | flying    |
| golbat                     |    42 | golbat       |     16 |    550 |              159 |      455 |  75 |     80 |      70 |              65 |               75 |    90 | poison    | flying    |
| oddish                     |    43 | oddish       |      5 |     54 |               64 |      320 |  45 |     50 |      55 |              75 |               65 |    30 | grass     | poison    |
| gloom                      |    44 | gloom        |      8 |     86 |              138 |      395 |  60 |     65 |      70 |              85 |               75 |    40 | grass     | poison    |
| vileplume                  |    45 | vileplume    |     12 |    186 |              221 |      490 |  75 |     80 |      85 |             110 |               90 |    50 | grass     | poison    |
| paras                      |    46 | paras        |      3 |     54 |               57 |      285 |  35 |     70 |      55 |              45 |               55 |    25 | bug       | grass     |
| parasect                   |    47 | parasect     |     10 |    295 |              142 |      405 |  60 |     95 |      80 |              60 |               80 |    30 | bug       | grass     |
| venonat                    |    48 | venonat      |     10 |    300 |               61 |      305 |  60 |     55 |      50 |              40 |               55 |    45 | bug       | poison    |
| venomoth                   |    49 | venomoth     |     15 |    125 |              158 |      450 |  70 |     65 |      60 |              90 |               75 |    90 | bug       | poison    |
| diglett                    |    50 | diglett      |      2 |      8 |               53 |      265 |  10 |     55 |      25 |              35 |               45 |    95 | ground    | None      |
| dugtrio                    |    51 | dugtrio      |      7 |    333 |              149 |      425 |  35 |    100 |      50 |              50 |               70 |   120 | ground    | None      |
| meowth                     |    52 | meowth       |      4 |     42 |               58 |      290 |  40 |     45 |      35 |              40 |               40 |    90 | normal    | None      |
| persian                    |    53 | persian      |     10 |    320 |              154 |      440 |  65 |     70 |      60 |              65 |               65 |   115 | normal    | None      |
| psyduck                    |    54 | psyduck      |      8 |    196 |               64 |      320 |  50 |     52 |      48 |              65 |               50 |    55 | water     | None      |
| golduck                    |    55 | golduck      |     17 |    766 |              175 |      500 |  80 |     82 |      78 |              95 |               80 |    85 | water     | None      |
| mankey                     |    56 | mankey       |      5 |    280 |               61 |      305 |  40 |     80 |      35 |              35 |               45 |    70 | fighting  | None      |
| primeape                   |    57 | primeape     |     10 |    320 |              159 |      455 |  65 |    105 |      60 |              60 |               70 |    95 | fighting  | None      |
| growlithe                  |    58 | growlithe    |      7 |    190 |               70 |      350 |  55 |     70 |      45 |              70 |               50 |    60 | fire      | None      |
| arcanine                   |    59 | arcanine     |     19 |   1550 |              194 |      555 |  90 |    110 |      80 |             100 |               80 |    95 | fire      | None      |
| poliwag                    |    60 | poliwag      |      6 |    124 |               60 |      300 |  40 |     50 |      40 |              40 |               40 |    90 | water     | None      |
| poliwhirl                  |    61 | poliwhirl    |     10 |    200 |              135 |      385 |  65 |     65 |      65 |              50 |               50 |    90 | water     | None      |
| poliwrath                  |    62 | poliwrath    |     13 |    540 |              230 |      510 |  90 |     95 |      95 |              70 |               90 |    70 | water     | fighting  |
| abra                       |    63 | abra         |      9 |    195 |               62 |      310 |  25 |     20 |      15 |             105 |               55 |    90 | psychic   | None      |
| kadabra                    |    64 | kadabra      |     13 |    565 |              140 |      400 |  40 |     35 |      30 |             120 |               70 |   105 | psychic   | None      |
| alakazam                   |    65 | alakazam     |     15 |    480 |              225 |      500 |  55 |     50 |      45 |             135 |               95 |   120 | psychic   | None      |
| machop                     |    66 | machop       |      8 |    195 |               61 |      305 |  70 |     80 |      50 |              35 |               35 |    35 | fighting  | None      |
| machoke                    |    67 | machoke      |     15 |    705 |              142 |      405 |  80 |    100 |      70 |              50 |               60 |    45 | fighting  | None      |
| machamp                    |    68 | machamp      |     16 |   1300 |              227 |      505 |  90 |    130 |      80 |              65 |               85 |    55 | fighting  | None      |
| bellsprout                 |    69 | bellsprout   |      7 |     40 |               60 |      300 |  50 |     75 |      35 |              70 |               30 |    40 | grass     | poison    |
| weepinbell                 |    70 | weepinbell   |     10 |     64 |              137 |      390 |  65 |     90 |      50 |              85 |               45 |    55 | grass     | poison    |
| victreebel                 |    71 | victreebel   |     17 |    155 |              221 |      490 |  80 |    105 |      65 |             100 |               70 |    70 | grass     | poison    |
| tentacool                  |    72 | tentacool    |      9 |    455 |               67 |      335 |  40 |     40 |      35 |              50 |              100 |    70 | water     | poison    |
| tentacruel                 |    73 | tentacruel   |     16 |    550 |              180 |      515 |  80 |     70 |      65 |              80 |              120 |   100 | water     | poison    |
| geodude                    |    74 | geodude      |      4 |    200 |               60 |      300 |  40 |     80 |     100 |              30 |               30 |    20 | rock      | ground    |
| graveler                   |    75 | graveler     |     10 |   1050 |              137 |      390 |  55 |     95 |     115 |              45 |               45 |    35 | rock      | ground    |
| golem                      |    76 | golem        |     14 |   3000 |              223 |      495 |  80 |    120 |     130 |              55 |               65 |    45 | rock      | ground    |
| ponyta                     |    77 | ponyta       |     10 |    300 |               82 |      410 |  50 |     85 |      55 |              65 |               65 |    90 | fire      | None      |
| rapidash                   |    78 | rapidash     |     17 |    950 |              175 |      500 |  65 |    100 |      70 |              80 |               80 |   105 | fire      | None      |
| slowpoke                   |    79 | slowpoke     |     12 |    360 |               63 |      315 |  90 |     65 |      65 |              40 |               40 |    15 | water     | psychic   |
| slowbro                    |    80 | slowbro      |     16 |    785 |              172 |      490 |  95 |     75 |     110 |             100 |               80 |    30 | water     | psychic   |
| magnemite                  |    81 | magnemite    |      3 |     60 |               65 |      325 |  25 |     35 |      70 |              95 |               55 |    45 | electric  | steel     |
| magneton                   |    82 | magneton     |     10 |    600 |              163 |      465 |  50 |     60 |      95 |             120 |               70 |    70 | electric  | steel     |
| farfetchd                  |    83 | farfetchd    |      8 |    150 |              132 |      377 |  52 |     90 |      55 |              58 |               62 |    60 | normal    | flying    |
| doduo                      |    84 | doduo        |     14 |    392 |               62 |      310 |  35 |     85 |      45 |              35 |               35 |    75 | normal    | flying    |
| dodrio                     |    85 | dodrio       |     18 |    852 |              165 |      470 |  60 |    110 |      70 |              60 |               60 |   110 | normal    | flying    |
| seel                       |    86 | seel         |     11 |    900 |               65 |      325 |  65 |     45 |      55 |              45 |               70 |    45 | water     | None      |
| dewgong                    |    87 | dewgong      |     17 |   1200 |              166 |      475 |  90 |     70 |      80 |              70 |               95 |    70 | water     | ice       |
| grimer                     |    88 | grimer       |      9 |    300 |               65 |      325 |  80 |     80 |      50 |              40 |               50 |    25 | poison    | None      |
| muk                        |    89 | muk          |     12 |    300 |              175 |      500 | 105 |    105 |      75 |              65 |              100 |    50 | poison    | None      |
| shellder                   |    90 | shellder     |      3 |     40 |               61 |      305 |  30 |     65 |     100 |              45 |               25 |    40 | water     | None      |
| cloyster                   |    91 | cloyster     |     15 |   1325 |              184 |      525 |  50 |     95 |     180 |              85 |               45 |    70 | water     | ice       |
| gastly                     |    92 | gastly       |     13 |      1 |               62 |      310 |  30 |     35 |      30 |             100 |               35 |    80 | ghost     | poison    |
| haunter                    |    93 | haunter      |     16 |      1 |              142 |      405 |  45 |     50 |      45 |             115 |               55 |    95 | ghost     | poison    |
| gengar                     |    94 | gengar       |     15 |    405 |              225 |      500 |  60 |     65 |      60 |             130 |               75 |   110 | ghost     | poison    |
| onix                       |    95 | onix         |     88 |   2100 |               77 |      385 |  35 |     45 |     160 |              30 |               45 |    70 | rock      | ground    |
| drowzee                    |    96 | drowzee      |     10 |    324 |               66 |      328 |  60 |     48 |      45 |              43 |               90 |    42 | psychic   | None      |
| hypno                      |    97 | hypno        |     16 |    756 |              169 |      483 |  85 |     73 |      70 |              73 |              115 |    67 | psychic   | None      |
| krabby                     |    98 | krabby       |      4 |     65 |               65 |      325 |  30 |    105 |      90 |              25 |               25 |    50 | water     | None      |
| kingler                    |    99 | kingler      |     13 |    600 |              166 |      475 |  55 |    130 |     115 |              50 |               50 |    75 | water     | None      |
| voltorb                    |   100 | voltorb      |      5 |    104 |               66 |      330 |  40 |     30 |      50 |              55 |               55 |   100 | electric  | None      |
| electrode                  |   101 | electrode    |     12 |    666 |              172 |      490 |  60 |     50 |      70 |              80 |               80 |   150 | electric  | None      |
| exeggcute                  |   102 | exeggcute    |      4 |     25 |               65 |      325 |  60 |     40 |      80 |              60 |               45 |    40 | grass     | psychic   |
| exeggutor                  |   103 | exeggutor    |     20 |   1200 |              186 |      530 |  95 |     95 |      85 |             125 |               75 |    55 | grass     | psychic   |
| cubone                     |   104 | cubone       |      4 |     65 |               64 |      320 |  50 |     50 |      95 |              40 |               50 |    35 | ground    | None      |
| marowak                    |   105 | marowak      |     10 |    450 |              149 |      425 |  60 |     80 |     110 |              50 |               80 |    45 | ground    | None      |
| hitmonlee                  |   106 | hitmonlee    |     15 |    498 |              159 |      455 |  50 |    120 |      53 |              35 |              110 |    87 | fighting  | None      |
| hitmonchan                 |   107 | hitmonchan   |     14 |    502 |              159 |      455 |  50 |    105 |      79 |              35 |              110 |    76 | fighting  | None      |
| lickitung                  |   108 | lickitung    |     12 |    655 |               77 |      385 |  90 |     55 |      75 |              60 |               75 |    30 | normal    | None      |
| koffing                    |   109 | koffing      |      6 |     10 |               68 |      340 |  40 |     65 |      95 |              60 |               45 |    35 | poison    | None      |
| weezing                    |   110 | weezing      |     12 |     95 |              172 |      490 |  65 |     90 |     120 |              85 |               70 |    60 | poison    | None      |
| rhyhorn                    |   111 | rhyhorn      |     10 |   1150 |               69 |      345 |  80 |     85 |      95 |              30 |               30 |    25 | ground    | rock      |
| rhydon                     |   112 | rhydon       |     19 |   1200 |              170 |      485 | 105 |    130 |     120 |              45 |               45 |    40 | ground    | rock      |
| chansey                    |   113 | chansey      |     11 |    346 |              395 |      450 | 250 |      5 |       5 |              35 |              105 |    50 | normal    | None      |
| tangela                    |   114 | tangela      |     10 |    350 |               87 |      435 |  65 |     55 |     115 |             100 |               40 |    60 | grass     | None      |
| kangaskhan                 |   115 | kangaskhan   |     22 |    800 |              172 |      490 | 105 |     95 |      80 |              40 |               80 |    90 | normal    | None      |
| horsea                     |   116 | horsea       |      4 |     80 |               59 |      295 |  30 |     40 |      70 |              70 |               25 |    60 | water     | None      |
| seadra                     |   117 | seadra       |     12 |    250 |              154 |      440 |  55 |     65 |      95 |              95 |               45 |    85 | water     | None      |
| goldeen                    |   118 | goldeen      |      6 |    150 |               64 |      320 |  45 |     67 |      60 |              35 |               50 |    63 | water     | None      |
| seaking                    |   119 | seaking      |     13 |    390 |              158 |      450 |  80 |     92 |      65 |              65 |               80 |    68 | water     | None      |
| staryu                     |   120 | staryu       |      8 |    345 |               68 |      340 |  30 |     45 |      55 |              70 |               55 |    85 | water     | None      |
| starmie                    |   121 | starmie      |     11 |    800 |              182 |      520 |  60 |     75 |      85 |             100 |               85 |   115 | water     | psychic   |
| mr-mime                    |   122 | mr-mime      |     13 |    545 |              161 |      460 |  40 |     45 |      65 |             100 |              120 |    90 | psychic   | fairy     |
| scyther                    |   123 | scyther      |     15 |    560 |              100 |      500 |  70 |    110 |      80 |              55 |               80 |   105 | bug       | flying    |
| jynx                       |   124 | jynx         |     14 |    406 |              159 |      455 |  65 |     50 |      35 |             115 |               95 |    95 | ice       | psychic   |
| electabuzz                 |   125 | electabuzz   |     11 |    300 |              172 |      490 |  65 |     83 |      57 |              95 |               85 |   105 | electric  | None      |
| magmar                     |   126 | magmar       |     13 |    445 |              173 |      495 |  65 |     95 |      57 |             100 |               85 |    93 | fire      | None      |
| pinsir                     |   127 | pinsir       |     15 |    550 |              175 |      500 |  65 |    125 |     100 |              55 |               70 |    85 | bug       | None      |
| tauros                     |   128 | tauros       |     14 |    884 |              172 |      490 |  75 |    100 |      95 |              40 |               70 |   110 | normal    | None      |
| magikarp                   |   129 | magikarp     |      9 |    100 |               40 |      200 |  20 |     10 |      55 |              15 |               20 |    80 | water     | None      |
| gyarados                   |   130 | gyarados     |     65 |   2350 |              189 |      540 |  95 |    125 |      79 |              60 |              100 |    81 | water     | flying    |
| lapras                     |   131 | lapras       |     25 |   2200 |              187 |      535 | 130 |     85 |      80 |              85 |               95 |    60 | water     | ice       |
| ditto                      |   132 | ditto        |      3 |     40 |              101 |      288 |  48 |     48 |      48 |              48 |               48 |    48 | normal    | None      |
| eevee                      |   133 | eevee        |      3 |     65 |               65 |      325 |  55 |     55 |      50 |              45 |               65 |    55 | normal    | None      |
| vaporeon                   |   134 | vaporeon     |     10 |    290 |              184 |      525 | 130 |     65 |      60 |             110 |               95 |    65 | water     | None      |
| jolteon                    |   135 | jolteon      |      8 |    245 |              184 |      525 |  65 |     65 |      60 |             110 |               95 |   130 | electric  | None      |
| flareon                    |   136 | flareon      |      9 |    250 |              184 |      525 |  65 |    130 |      60 |              95 |              110 |    65 | fire      | None      |
| porygon                    |   137 | porygon      |      8 |    365 |               79 |      395 |  65 |     60 |      70 |              85 |               75 |    40 | normal    | None      |
| omanyte                    |   138 | omanyte      |      4 |     75 |               71 |      355 |  35 |     40 |     100 |              90 |               55 |    35 | rock      | water     |
| omastar                    |   139 | omastar      |     10 |    350 |              173 |      495 |  70 |     60 |     125 |             115 |               70 |    55 | rock      | water     |
| kabuto                     |   140 | kabuto       |      5 |    115 |               71 |      355 |  30 |     80 |      90 |              55 |               45 |    55 | rock      | water     |
| kabutops                   |   141 | kabutops     |     13 |    405 |              173 |      495 |  60 |    115 |     105 |              65 |               70 |    80 | rock      | water     |
| aerodactyl                 |   142 | aerodactyl   |     18 |    590 |              180 |      515 |  80 |    105 |      65 |              60 |               75 |   130 | rock      | flying    |
| snorlax                    |   143 | snorlax      |     21 |   4600 |              189 |      540 | 160 |    110 |      65 |              65 |              110 |    30 | normal    | None      |
| articuno                   |   144 | articuno     |     17 |    554 |              261 |      580 |  90 |     85 |     100 |              95 |              125 |    85 | ice       | flying    |
| zapdos                     |   145 | zapdos       |     16 |    526 |              261 |      580 |  90 |     90 |      85 |             125 |               90 |   100 | electric  | flying    |
| moltres                    |   146 | moltres      |     20 |    600 |              261 |      580 |  90 |    100 |      90 |             125 |               85 |    90 | fire      | flying    |
| dratini                    |   147 | dratini      |     18 |     33 |               60 |      300 |  41 |     64 |      45 |              50 |               50 |    50 | dragon    | None      |
| dragonair                  |   148 | dragonair    |     40 |    165 |              147 |      420 |  61 |     84 |      65 |              70 |               70 |    70 | dragon    | None      |
| dragonite                  |   149 | dragonite    |     22 |   2100 |              270 |      600 |  91 |    134 |      95 |             100 |              100 |    80 | dragon    | flying    |
| mewtwo                     |   150 | mewtwo       |     20 |   1220 |              306 |      680 | 106 |    110 |      90 |             154 |               90 |   130 | psychic   | None      |
| mew                        |   151 | mew          |      4 |     40 |              270 |      600 | 100 |    100 |     100 |             100 |              100 |   100 | psychic   | None      |
| chikorita                  |   152 | chikorita    |      9 |     64 |               64 |      318 |  45 |     49 |      65 |              49 |               65 |    45 | grass     | None      |
| bayleef                    |   153 | bayleef      |     12 |    158 |              142 |      405 |  60 |     62 |      80 |              63 |               80 |    60 | grass     | None      |
| meganium                   |   154 | meganium     |     18 |   1005 |              236 |      525 |  80 |     82 |     100 |              83 |              100 |    80 | grass     | None      |
| cyndaquil                  |   155 | cyndaquil    |      5 |     79 |               62 |      309 |  39 |     52 |      43 |              60 |               50 |    65 | fire      | None      |
| quilava                    |   156 | quilava      |      9 |    190 |              142 |      405 |  58 |     64 |      58 |              80 |               65 |    80 | fire      | None      |
| typhlosion                 |   157 | typhlosion   |     17 |    795 |              240 |      534 |  78 |     84 |      78 |             109 |               85 |   100 | fire      | None      |
| totodile                   |   158 | totodile     |      6 |     95 |               63 |      314 |  50 |     65 |      64 |              44 |               48 |    43 | water     | None      |
| croconaw                   |   159 | croconaw     |     11 |    250 |              142 |      405 |  65 |     80 |      80 |              59 |               63 |    58 | water     | None      |
| feraligatr                 |   160 | feraligatr   |     23 |    888 |              239 |      530 |  85 |    105 |     100 |              79 |               83 |    78 | water     | None      |
| sentret                    |   161 | sentret      |      8 |     60 |               43 |      215 |  35 |     46 |      34 |              35 |               45 |    20 | normal    | None      |
| furret                     |   162 | furret       |     18 |    325 |              145 |      415 |  85 |     76 |      64 |              45 |               55 |    90 | normal    | None      |
| hoothoot                   |   163 | hoothoot     |      7 |    212 |               52 |      262 |  60 |     30 |      30 |              36 |               56 |    50 | normal    | flying    |
| noctowl                    |   164 | noctowl      |     16 |    408 |              158 |      452 | 100 |     50 |      50 |              86 |               96 |    70 | normal    | flying    |
| ledyba                     |   165 | ledyba       |     10 |    108 |               53 |      265 |  40 |     20 |      30 |              40 |               80 |    55 | bug       | flying    |
| ledian                     |   166 | ledian       |     14 |    356 |              137 |      390 |  55 |     35 |      50 |              55 |              110 |    85 | bug       | flying    |
| spinarak                   |   167 | spinarak     |      5 |     85 |               50 |      250 |  40 |     60 |      40 |              40 |               40 |    30 | bug       | poison    |
| ariados                    |   168 | ariados      |     11 |    335 |              140 |      400 |  70 |     90 |      70 |              60 |               70 |    40 | bug       | poison    |
| crobat                     |   169 | crobat       |     18 |    750 |              241 |      535 |  85 |     90 |      80 |              70 |               80 |   130 | poison    | flying    |
| chinchou                   |   170 | chinchou     |      5 |    120 |               66 |      330 |  75 |     38 |      38 |              56 |               56 |    67 | water     | electric  |
| lanturn                    |   171 | lanturn      |     12 |    225 |              161 |      460 | 125 |     58 |      58 |              76 |               76 |    67 | water     | electric  |
| pichu                      |   172 | pichu        |      3 |     20 |               41 |      205 |  20 |     40 |      15 |              35 |               35 |    60 | electric  | None      |
| cleffa                     |   173 | cleffa       |      3 |     30 |               44 |      218 |  50 |     25 |      28 |              45 |               55 |    15 | fairy     | None      |
| igglybuff                  |   174 | igglybuff    |      3 |     10 |               42 |      210 |  90 |     30 |      15 |              40 |               20 |    15 | normal    | fairy     |
| togepi                     |   175 | togepi       |      3 |     15 |               49 |      245 |  35 |     20 |      65 |              40 |               65 |    20 | fairy     | None      |
| togetic                    |   176 | togetic      |      6 |     32 |              142 |      405 |  55 |     40 |      85 |              80 |              105 |    40 | fairy     | flying    |
| natu                       |   177 | natu         |      2 |     20 |               64 |      320 |  40 |     50 |      45 |              70 |               45 |    70 | psychic   | flying    |
| xatu                       |   178 | xatu         |     15 |    150 |              165 |      470 |  65 |     75 |      70 |              95 |               70 |    95 | psychic   | flying    |
| mareep                     |   179 | mareep       |      6 |     78 |               56 |      280 |  55 |     40 |      40 |              65 |               45 |    35 | electric  | None      |
| flaaffy                    |   180 | flaaffy      |      8 |    133 |              128 |      365 |  70 |     55 |      55 |              80 |               60 |    45 | electric  | None      |
| ampharos                   |   181 | ampharos     |     14 |    615 |              230 |      510 |  90 |     75 |      85 |             115 |               90 |    55 | electric  | None      |
| bellossom                  |   182 | bellossom    |      4 |     58 |              221 |      490 |  75 |     80 |      95 |              90 |              100 |    50 | grass     | None      |
| marill                     |   183 | marill       |      4 |     85 |               88 |      250 |  70 |     20 |      50 |              20 |               50 |    40 | water     | fairy     |
| azumarill                  |   184 | azumarill    |      8 |    285 |              189 |      420 | 100 |     50 |      80 |              60 |               80 |    50 | water     | fairy     |
| sudowoodo                  |   185 | sudowoodo    |     12 |    380 |              144 |      410 |  70 |    100 |     115 |              30 |               65 |    30 | rock      | None      |
| politoed                   |   186 | politoed     |     11 |    339 |              225 |      500 |  90 |     75 |      75 |              90 |              100 |    70 | water     | None      |
| hoppip                     |   187 | hoppip       |      4 |      5 |               50 |      250 |  35 |     35 |      40 |              35 |               55 |    50 | grass     | flying    |
| skiploom                   |   188 | skiploom     |      6 |     10 |              119 |      340 |  55 |     45 |      50 |              45 |               65 |    80 | grass     | flying    |
| jumpluff                   |   189 | jumpluff     |      8 |     30 |              207 |      460 |  75 |     55 |      70 |              55 |               95 |   110 | grass     | flying    |
| aipom                      |   190 | aipom        |      8 |    115 |               72 |      360 |  55 |     70 |      55 |              40 |               55 |    85 | normal    | None      |
| sunkern                    |   191 | sunkern      |      3 |     18 |               36 |      180 |  30 |     30 |      30 |              30 |               30 |    30 | grass     | None      |
| sunflora                   |   192 | sunflora     |      8 |     85 |              149 |      425 |  75 |     75 |      55 |             105 |               85 |    30 | grass     | None      |
| yanma                      |   193 | yanma        |     12 |    380 |               78 |      390 |  65 |     65 |      45 |              75 |               45 |    95 | bug       | flying    |
| wooper                     |   194 | wooper       |      4 |     85 |               42 |      210 |  55 |     45 |      45 |              25 |               25 |    15 | water     | ground    |
| quagsire                   |   195 | quagsire     |     14 |    750 |              151 |      430 |  95 |     85 |      85 |              65 |               65 |    35 | water     | ground    |
| espeon                     |   196 | espeon       |      9 |    265 |              184 |      525 |  65 |     65 |      60 |             130 |               95 |   110 | psychic   | None      |
| umbreon                    |   197 | umbreon      |     10 |    270 |              184 |      525 |  95 |     65 |     110 |              60 |              130 |    65 | dark      | None      |
| murkrow                    |   198 | murkrow      |      5 |     21 |               81 |      405 |  60 |     85 |      42 |              85 |               42 |    91 | dark      | flying    |
| slowking                   |   199 | slowking     |     20 |    795 |              172 |      490 |  95 |     75 |      80 |             100 |              110 |    30 | water     | psychic   |
| misdreavus                 |   200 | misdreavus   |      7 |     10 |               87 |      435 |  60 |     60 |      60 |              85 |               85 |    85 | ghost     | None      |
| unown                      |   201 | unown        |      5 |     50 |              118 |      336 |  48 |     72 |      48 |              72 |               48 |    48 | psychic   | None      |
| wobbuffet                  |   202 | wobbuffet    |     13 |    285 |              142 |      405 | 190 |     33 |      58 |              33 |               58 |    33 | psychic   | None      |
| girafarig                  |   203 | girafarig    |     15 |    415 |              159 |      455 |  70 |     80 |      65 |              90 |               65 |    85 | normal    | psychic   |
| pineco                     |   204 | pineco       |      6 |     72 |               58 |      290 |  50 |     65 |      90 |              35 |               35 |    15 | bug       | None      |
| forretress                 |   205 | forretress   |     12 |   1258 |              163 |      465 |  75 |     90 |     140 |              60 |               60 |    40 | bug       | steel     |
| dunsparce                  |   206 | dunsparce    |     15 |    140 |              145 |      415 | 100 |     70 |      70 |              65 |               65 |    45 | normal    | None      |
| gligar                     |   207 | gligar       |     11 |    648 |               86 |      430 |  65 |     75 |     105 |              35 |               65 |    85 | ground    | flying    |
| steelix                    |   208 | steelix      |     92 |   4000 |              179 |      510 |  75 |     85 |     200 |              55 |               65 |    30 | steel     | ground    |
| snubbull                   |   209 | snubbull     |      6 |     78 |               60 |      300 |  60 |     80 |      50 |              40 |               40 |    30 | fairy     | None      |
| granbull                   |   210 | granbull     |     14 |    487 |              158 |      450 |  90 |    120 |      75 |              60 |               60 |    45 | fairy     | None      |
| qwilfish                   |   211 | qwilfish     |      5 |     39 |               88 |      440 |  65 |     95 |      85 |              55 |               55 |    85 | water     | poison    |
| scizor                     |   212 | scizor       |     18 |   1180 |              175 |      500 |  70 |    130 |     100 |              55 |               80 |    65 | bug       | steel     |
| shuckle                    |   213 | shuckle      |      6 |    205 |              177 |      505 |  20 |     10 |     230 |              10 |              230 |     5 | bug       | rock      |
| heracross                  |   214 | heracross    |     15 |    540 |              175 |      500 |  80 |    125 |      75 |              40 |               95 |    85 | bug       | fighting  |
| sneasel                    |   215 | sneasel      |      9 |    280 |               86 |      430 |  55 |     95 |      55 |              35 |               75 |   115 | dark      | ice       |
| teddiursa                  |   216 | teddiursa    |      6 |     88 |               66 |      330 |  60 |     80 |      50 |              50 |               50 |    40 | normal    | None      |
| ursaring                   |   217 | ursaring     |     18 |   1258 |              175 |      500 |  90 |    130 |      75 |              75 |               75 |    55 | normal    | None      |
| slugma                     |   218 | slugma       |      7 |    350 |               50 |      250 |  40 |     40 |      40 |              70 |               40 |    20 | fire      | None      |
| magcargo                   |   219 | magcargo     |      8 |    550 |              151 |      430 |  60 |     50 |     120 |              90 |               80 |    30 | fire      | rock      |
| swinub                     |   220 | swinub       |      4 |     65 |               50 |      250 |  50 |     50 |      40 |              30 |               30 |    50 | ice       | ground    |
| piloswine                  |   221 | piloswine    |     11 |    558 |              158 |      450 | 100 |    100 |      80 |              60 |               60 |    50 | ice       | ground    |
| corsola                    |   222 | corsola      |      6 |     50 |              144 |      410 |  65 |     55 |      95 |              65 |               95 |    35 | water     | rock      |
| remoraid                   |   223 | remoraid     |      6 |    120 |               60 |      300 |  35 |     65 |      35 |              65 |               35 |    65 | water     | None      |
| octillery                  |   224 | octillery    |      9 |    285 |              168 |      480 |  75 |    105 |      75 |             105 |               75 |    45 | water     | None      |
| delibird                   |   225 | delibird     |      9 |    160 |              116 |      330 |  45 |     55 |      45 |              65 |               45 |    75 | ice       | flying    |
| mantine                    |   226 | mantine      |     21 |   2200 |              170 |      485 |  85 |     40 |      70 |              80 |              140 |    70 | water     | flying    |
| skarmory                   |   227 | skarmory     |     17 |    505 |              163 |      465 |  65 |     80 |     140 |              40 |               70 |    70 | steel     | flying    |
| houndour                   |   228 | houndour     |      6 |    108 |               66 |      330 |  45 |     60 |      30 |              80 |               50 |    65 | dark      | fire      |
| houndoom                   |   229 | houndoom     |     14 |    350 |              175 |      500 |  75 |     90 |      50 |             110 |               80 |    95 | dark      | fire      |
| kingdra                    |   230 | kingdra      |     18 |   1520 |              243 |      540 |  75 |     95 |      95 |              95 |               95 |    85 | water     | dragon    |
| phanpy                     |   231 | phanpy       |      5 |    335 |               66 |      330 |  90 |     60 |      60 |              40 |               40 |    40 | ground    | None      |
| donphan                    |   232 | donphan      |     11 |   1200 |              175 |      500 |  90 |    120 |     120 |              60 |               60 |    50 | ground    | None      |
| porygon2                   |   233 | porygon2     |      6 |    325 |              180 |      515 |  85 |     80 |      90 |             105 |               95 |    60 | normal    | None      |
| stantler                   |   234 | stantler     |     14 |    712 |              163 |      465 |  73 |     95 |      62 |              85 |               65 |    85 | normal    | None      |
| smeargle                   |   235 | smeargle     |     12 |    580 |               88 |      250 |  55 |     20 |      35 |              20 |               45 |    75 | normal    | None      |
| tyrogue                    |   236 | tyrogue      |      7 |    210 |               42 |      210 |  35 |     35 |      35 |              35 |               35 |    35 | fighting  | None      |
| hitmontop                  |   237 | hitmontop    |     14 |    480 |              159 |      455 |  50 |     95 |      95 |              35 |              110 |    70 | fighting  | None      |
| smoochum                   |   238 | smoochum     |      4 |     60 |               61 |      305 |  45 |     30 |      15 |              85 |               65 |    65 | ice       | psychic   |
| elekid                     |   239 | elekid       |      6 |    235 |               72 |      360 |  45 |     63 |      37 |              65 |               55 |    95 | electric  | None      |
| magby                      |   240 | magby        |      7 |    214 |               73 |      365 |  45 |     75 |      37 |              70 |               55 |    83 | fire      | None      |
| miltank                    |   241 | miltank      |     12 |    755 |              172 |      490 |  95 |     80 |     105 |              40 |               70 |   100 | normal    | None      |
| blissey                    |   242 | blissey      |     15 |    468 |              608 |      540 | 255 |     10 |      10 |              75 |              135 |    55 | normal    | None      |
| raikou                     |   243 | raikou       |     19 |   1780 |              261 |      580 |  90 |     85 |      75 |             115 |              100 |   115 | electric  | None      |
| entei                      |   244 | entei        |     21 |   1980 |              261 |      580 | 115 |    115 |      85 |              90 |               75 |   100 | fire      | None      |
| suicune                    |   245 | suicune      |     20 |   1870 |              261 |      580 | 100 |     75 |     115 |              90 |              115 |    85 | water     | None      |
| larvitar                   |   246 | larvitar     |      6 |    720 |               60 |      300 |  50 |     64 |      50 |              45 |               50 |    41 | rock      | ground    |
| pupitar                    |   247 | pupitar      |     12 |   1520 |              144 |      410 |  70 |     84 |      70 |              65 |               70 |    51 | rock      | ground    |
| tyranitar                  |   248 | tyranitar    |     20 |   2020 |              270 |      600 | 100 |    134 |     110 |              95 |              100 |    61 | rock      | dark      |
| lugia                      |   249 | lugia        |     52 |   2160 |              306 |      680 | 106 |     90 |     130 |              90 |              154 |   110 | psychic   | flying    |
| ho-oh                      |   250 | ho-oh        |     38 |   1990 |              306 |      680 | 106 |    130 |      90 |             110 |              154 |    90 | fire      | flying    |
| celebi                     |   251 | celebi       |      6 |     50 |              270 |      600 | 100 |    100 |     100 |             100 |              100 |   100 | psychic   | grass     |
| treecko                    |   252 | treecko      |      5 |     50 |               62 |      310 |  40 |     45 |      35 |              65 |               55 |    70 | grass     | None      |
| grovyle                    |   253 | grovyle      |      9 |    216 |              142 |      405 |  50 |     65 |      45 |              85 |               65 |    95 | grass     | None      |
| sceptile                   |   254 | sceptile     |     17 |    522 |              239 |      530 |  70 |     85 |      65 |             105 |               85 |   120 | grass     | None      |
| torchic                    |   255 | torchic      |      4 |     25 |               62 |      310 |  45 |     60 |      40 |              70 |               50 |    45 | fire      | None      |
| combusken                  |   256 | combusken    |      9 |    195 |              142 |      405 |  60 |     85 |      60 |              85 |               60 |    55 | fire      | fighting  |
| blaziken                   |   257 | blaziken     |     19 |    520 |              239 |      530 |  80 |    120 |      70 |             110 |               70 |    80 | fire      | fighting  |
| mudkip                     |   258 | mudkip       |      4 |     76 |               62 |      310 |  50 |     70 |      50 |              50 |               50 |    40 | water     | None      |
| marshtomp                  |   259 | marshtomp    |      7 |    280 |              142 |      405 |  70 |     85 |      70 |              60 |               70 |    50 | water     | ground    |
| swampert                   |   260 | swampert     |     15 |    819 |              241 |      535 | 100 |    110 |      90 |              85 |               90 |    60 | water     | ground    |
| poochyena                  |   261 | poochyena    |      5 |    136 |               56 |      220 |  35 |     55 |      35 |              30 |               30 |    35 | dark      | None      |
| mightyena                  |   262 | mightyena    |     10 |    370 |              147 |      420 |  70 |     90 |      70 |              60 |               60 |    70 | dark      | None      |
| zigzagoon                  |   263 | zigzagoon    |      4 |    175 |               56 |      240 |  38 |     30 |      41 |              30 |               41 |    60 | normal    | None      |
| linoone                    |   264 | linoone      |      5 |    325 |              147 |      420 |  78 |     70 |      61 |              50 |               61 |   100 | normal    | None      |
| wurmple                    |   265 | wurmple      |      3 |     36 |               56 |      195 |  45 |     45 |      35 |              20 |               30 |    20 | bug       | None      |
| silcoon                    |   266 | silcoon      |      6 |    100 |               72 |      205 |  50 |     35 |      55 |              25 |               25 |    15 | bug       | None      |
| beautifly                  |   267 | beautifly    |     10 |    284 |              178 |      395 |  60 |     70 |      50 |             100 |               50 |    65 | bug       | flying    |
| cascoon                    |   268 | cascoon      |      7 |    115 |               72 |      205 |  50 |     35 |      55 |              25 |               25 |    15 | bug       | None      |
| dustox                     |   269 | dustox       |     12 |    316 |              173 |      385 |  60 |     50 |      70 |              50 |               90 |    65 | bug       | poison    |
| lotad                      |   270 | lotad        |      5 |     26 |               44 |      220 |  40 |     30 |      30 |              40 |               50 |    30 | water     | grass     |
| lombre                     |   271 | lombre       |     12 |    325 |              119 |      340 |  60 |     50 |      50 |              60 |               70 |    50 | water     | grass     |
| ludicolo                   |   272 | ludicolo     |     15 |    550 |              216 |      480 |  80 |     70 |      70 |              90 |              100 |    70 | water     | grass     |
| seedot                     |   273 | seedot       |      5 |     40 |               44 |      220 |  40 |     40 |      50 |              30 |               30 |    30 | grass     | None      |
| nuzleaf                    |   274 | nuzleaf      |     10 |    280 |              119 |      340 |  70 |     70 |      40 |              60 |               40 |    60 | grass     | dark      |
| shiftry                    |   275 | shiftry      |     13 |    596 |              216 |      480 |  90 |    100 |      60 |              90 |               60 |    80 | grass     | dark      |
| taillow                    |   276 | taillow      |      3 |     23 |               54 |      270 |  40 |     55 |      30 |              30 |               30 |    85 | normal    | flying    |
| swellow                    |   277 | swellow      |      7 |    198 |              159 |      455 |  60 |     85 |      60 |              75 |               50 |   125 | normal    | flying    |
| wingull                    |   278 | wingull      |      6 |     95 |               54 |      270 |  40 |     30 |      30 |              55 |               30 |    85 | water     | flying    |
| pelipper                   |   279 | pelipper     |     12 |    280 |              154 |      440 |  60 |     50 |     100 |              95 |               70 |    65 | water     | flying    |
| ralts                      |   280 | ralts        |      4 |     66 |               40 |      198 |  28 |     25 |      25 |              45 |               35 |    40 | psychic   | fairy     |
| kirlia                     |   281 | kirlia       |      8 |    202 |               97 |      278 |  38 |     35 |      35 |              65 |               55 |    50 | psychic   | fairy     |
| gardevoir                  |   282 | gardevoir    |     16 |    484 |              233 |      518 |  68 |     65 |      65 |             125 |              115 |    80 | psychic   | fairy     |
| surskit                    |   283 | surskit      |      5 |     17 |               54 |      269 |  40 |     30 |      32 |              50 |               52 |    65 | bug       | water     |
| masquerain                 |   284 | masquerain   |      8 |     36 |              159 |      454 |  70 |     60 |      62 |             100 |               82 |    80 | bug       | flying    |
| shroomish                  |   285 | shroomish    |      4 |     45 |               59 |      295 |  60 |     40 |      60 |              40 |               60 |    35 | grass     | None      |
| breloom                    |   286 | breloom      |     12 |    392 |              161 |      460 |  60 |    130 |      80 |              60 |               60 |    70 | grass     | fighting  |
| slakoth                    |   287 | slakoth      |      8 |    240 |               56 |      280 |  60 |     60 |      60 |              35 |               35 |    30 | normal    | None      |
| vigoroth                   |   288 | vigoroth     |     14 |    465 |              154 |      440 |  80 |     80 |      80 |              55 |               55 |    90 | normal    | None      |
| slaking                    |   289 | slaking      |     20 |   1305 |              252 |      670 | 150 |    160 |     100 |              95 |               65 |   100 | normal    | None      |
| nincada                    |   290 | nincada      |      5 |     55 |               53 |      266 |  31 |     45 |      90 |              30 |               30 |    40 | bug       | ground    |
| ninjask                    |   291 | ninjask      |      8 |    120 |              160 |      456 |  61 |     90 |      45 |              50 |               50 |   160 | bug       | flying    |
| shedinja                   |   292 | shedinja     |      8 |     12 |               83 |      236 |   1 |     90 |      45 |              30 |               30 |    40 | bug       | ghost     |
| whismur                    |   293 | whismur      |      6 |    163 |               48 |      240 |  64 |     51 |      23 |              51 |               23 |    28 | normal    | None      |
| loudred                    |   294 | loudred      |     10 |    405 |              126 |      360 |  84 |     71 |      43 |              71 |               43 |    48 | normal    | None      |
| exploud                    |   295 | exploud      |     15 |    840 |              221 |      490 | 104 |     91 |      63 |              91 |               73 |    68 | normal    | None      |
| makuhita                   |   296 | makuhita     |     10 |    864 |               47 |      237 |  72 |     60 |      30 |              20 |               30 |    25 | fighting  | None      |
| hariyama                   |   297 | hariyama     |     23 |   2538 |              166 |      474 | 144 |    120 |      60 |              40 |               60 |    50 | fighting  | None      |
| azurill                    |   298 | azurill      |      2 |     20 |               38 |      190 |  50 |     20 |      40 |              20 |               40 |    20 | normal    | fairy     |
| nosepass                   |   299 | nosepass     |     10 |    970 |               75 |      375 |  30 |     45 |     135 |              45 |               90 |    30 | rock      | None      |
| skitty                     |   300 | skitty       |      6 |    110 |               52 |      260 |  50 |     45 |      45 |              35 |               35 |    50 | normal    | None      |
| delcatty                   |   301 | delcatty     |     11 |    326 |              140 |      400 |  70 |     65 |      65 |              55 |               55 |    90 | normal    | None      |
| sableye                    |   302 | sableye      |      5 |    110 |              133 |      380 |  50 |     75 |      75 |              65 |               65 |    50 | dark      | ghost     |
| mawile                     |   303 | mawile       |      6 |    115 |              133 |      380 |  50 |     85 |      85 |              55 |               55 |    50 | steel     | fairy     |
| aron                       |   304 | aron         |      4 |    600 |               66 |      330 |  50 |     70 |     100 |              40 |               40 |    30 | steel     | rock      |
| lairon                     |   305 | lairon       |      9 |   1200 |              151 |      430 |  60 |     90 |     140 |              50 |               50 |    40 | steel     | rock      |
| aggron                     |   306 | aggron       |     21 |   3600 |              239 |      530 |  70 |    110 |     180 |              60 |               60 |    50 | steel     | rock      |
| meditite                   |   307 | meditite     |      6 |    112 |               56 |      280 |  30 |     40 |      55 |              40 |               55 |    60 | fighting  | psychic   |
| medicham                   |   308 | medicham     |     13 |    315 |              144 |      410 |  60 |     60 |      75 |              60 |               75 |    80 | fighting  | psychic   |
| electrike                  |   309 | electrike    |      6 |    152 |               59 |      295 |  40 |     45 |      40 |              65 |               40 |    65 | electric  | None      |
| manectric                  |   310 | manectric    |     15 |    402 |              166 |      475 |  70 |     75 |      60 |             105 |               60 |   105 | electric  | None      |
| plusle                     |   311 | plusle       |      4 |     42 |              142 |      405 |  60 |     50 |      40 |              85 |               75 |    95 | electric  | None      |
| minun                      |   312 | minun        |      4 |     42 |              142 |      405 |  60 |     40 |      50 |              75 |               85 |    95 | electric  | None      |
| volbeat                    |   313 | volbeat      |      7 |    177 |              151 |      430 |  65 |     73 |      75 |              47 |               85 |    85 | bug       | None      |
| illumise                   |   314 | illumise     |      6 |    177 |              151 |      430 |  65 |     47 |      75 |              73 |               85 |    85 | bug       | None      |
| roselia                    |   315 | roselia      |      3 |     20 |              140 |      400 |  50 |     60 |      45 |             100 |               80 |    65 | grass     | poison    |
| gulpin                     |   316 | gulpin       |      4 |    103 |               60 |      302 |  70 |     43 |      53 |              43 |               53 |    40 | poison    | None      |
| swalot                     |   317 | swalot       |     17 |    800 |              163 |      467 | 100 |     73 |      83 |              73 |               83 |    55 | poison    | None      |
| carvanha                   |   318 | carvanha     |      8 |    208 |               61 |      305 |  45 |     90 |      20 |              65 |               20 |    65 | water     | dark      |
| sharpedo                   |   319 | sharpedo     |     18 |    888 |              161 |      460 |  70 |    120 |      40 |              95 |               40 |    95 | water     | dark      |
| wailmer                    |   320 | wailmer      |     20 |   1300 |               80 |      400 | 130 |     70 |      35 |              70 |               35 |    60 | water     | None      |
| wailord                    |   321 | wailord      |    145 |   3980 |              175 |      500 | 170 |     90 |      45 |              90 |               45 |    60 | water     | None      |
| numel                      |   322 | numel        |      7 |    240 |               61 |      305 |  60 |     60 |      40 |              65 |               45 |    35 | fire      | ground    |
| camerupt                   |   323 | camerupt     |     19 |   2200 |              161 |      460 |  70 |    100 |      70 |             105 |               75 |    40 | fire      | ground    |
| torkoal                    |   324 | torkoal      |      5 |    804 |              165 |      470 |  70 |     85 |     140 |              85 |               70 |    20 | fire      | None      |
| spoink                     |   325 | spoink       |      7 |    306 |               66 |      330 |  60 |     25 |      35 |              70 |               80 |    60 | psychic   | None      |
| grumpig                    |   326 | grumpig      |      9 |    715 |              165 |      470 |  80 |     45 |      65 |              90 |              110 |    80 | psychic   | None      |
| spinda                     |   327 | spinda       |     11 |     50 |              126 |      360 |  60 |     60 |      60 |              60 |               60 |    60 | normal    | None      |
| trapinch                   |   328 | trapinch     |      7 |    150 |               58 |      290 |  45 |    100 |      45 |              45 |               45 |    10 | ground    | None      |
| vibrava                    |   329 | vibrava      |     11 |    153 |              119 |      340 |  50 |     70 |      50 |              50 |               50 |    70 | ground    | dragon    |
| flygon                     |   330 | flygon       |     20 |    820 |              234 |      520 |  80 |    100 |      80 |              80 |               80 |   100 | ground    | dragon    |
| cacnea                     |   331 | cacnea       |      4 |    513 |               67 |      335 |  50 |     85 |      40 |              85 |               40 |    35 | grass     | None      |
| cacturne                   |   332 | cacturne     |     13 |    774 |              166 |      475 |  70 |    115 |      60 |             115 |               60 |    55 | grass     | dark      |
| swablu                     |   333 | swablu       |      4 |     12 |               62 |      310 |  45 |     40 |      60 |              40 |               75 |    50 | normal    | flying    |
| altaria                    |   334 | altaria      |     11 |    206 |              172 |      490 |  75 |     70 |      90 |              70 |              105 |    80 | dragon    | flying    |
| zangoose                   |   335 | zangoose     |     13 |    403 |              160 |      458 |  73 |    115 |      60 |              60 |               60 |    90 | normal    | None      |
| seviper                    |   336 | seviper      |     27 |    525 |              160 |      458 |  73 |    100 |      60 |             100 |               60 |    65 | poison    | None      |
| lunatone                   |   337 | lunatone     |     10 |   1680 |              161 |      460 |  90 |     55 |      65 |              95 |               85 |    70 | rock      | psychic   |
| solrock                    |   338 | solrock      |     12 |   1540 |              161 |      460 |  90 |     95 |      85 |              55 |               65 |    70 | rock      | psychic   |
| barboach                   |   339 | barboach     |      4 |     19 |               58 |      288 |  50 |     48 |      43 |              46 |               41 |    60 | water     | ground    |
| whiscash                   |   340 | whiscash     |      9 |    236 |              164 |      468 | 110 |     78 |      73 |              76 |               71 |    60 | water     | ground    |
| corphish                   |   341 | corphish     |      6 |    115 |               62 |      308 |  43 |     80 |      65 |              50 |               35 |    35 | water     | None      |
| crawdaunt                  |   342 | crawdaunt    |     11 |    328 |              164 |      468 |  63 |    120 |      85 |              90 |               55 |    55 | water     | dark      |
| baltoy                     |   343 | baltoy       |      5 |    215 |               60 |      300 |  40 |     40 |      55 |              40 |               70 |    55 | ground    | psychic   |
| claydol                    |   344 | claydol      |     15 |   1080 |              175 |      500 |  60 |     70 |     105 |              70 |              120 |    75 | ground    | psychic   |
| lileep                     |   345 | lileep       |     10 |    238 |               71 |      355 |  66 |     41 |      77 |              61 |               87 |    23 | rock      | grass     |
| cradily                    |   346 | cradily      |     15 |    604 |              173 |      495 |  86 |     81 |      97 |              81 |              107 |    43 | rock      | grass     |
| anorith                    |   347 | anorith      |      7 |    125 |               71 |      355 |  45 |     95 |      50 |              40 |               50 |    75 | rock      | bug       |
| armaldo                    |   348 | armaldo      |     15 |    682 |              173 |      495 |  75 |    125 |     100 |              70 |               80 |    45 | rock      | bug       |
| feebas                     |   349 | feebas       |      6 |     74 |               40 |      200 |  20 |     15 |      20 |              10 |               55 |    80 | water     | None      |
| milotic                    |   350 | milotic      |     62 |   1620 |              189 |      540 |  95 |     60 |      79 |             100 |              125 |    81 | water     | None      |
| castform                   |   351 | castform     |      3 |      8 |              147 |      420 |  70 |     70 |      70 |              70 |               70 |    70 | normal    | None      |
| kecleon                    |   352 | kecleon      |     10 |    220 |              154 |      440 |  60 |     90 |      70 |              60 |              120 |    40 | normal    | None      |
| shuppet                    |   353 | shuppet      |      6 |     23 |               59 |      295 |  44 |     75 |      35 |              63 |               33 |    45 | ghost     | None      |
| banette                    |   354 | banette      |     11 |    125 |              159 |      455 |  64 |    115 |      65 |              83 |               63 |    65 | ghost     | None      |
| duskull                    |   355 | duskull      |      8 |    150 |               59 |      295 |  20 |     40 |      90 |              30 |               90 |    25 | ghost     | None      |
| dusclops                   |   356 | dusclops     |     16 |    306 |              159 |      455 |  40 |     70 |     130 |              60 |              130 |    25 | ghost     | None      |
| tropius                    |   357 | tropius      |     20 |   1000 |              161 |      460 |  99 |     68 |      83 |              72 |               87 |    51 | grass     | flying    |
| chimecho                   |   358 | chimecho     |      6 |     10 |              159 |      455 |  75 |     50 |      80 |              95 |               90 |    65 | psychic   | None      |
| absol                      |   359 | absol        |     12 |    470 |              163 |      465 |  65 |    130 |      60 |              75 |               60 |    75 | dark      | None      |
| wynaut                     |   360 | wynaut       |      6 |    140 |               52 |      260 |  95 |     23 |      48 |              23 |               48 |    23 | psychic   | None      |
| snorunt                    |   361 | snorunt      |      7 |    168 |               60 |      300 |  50 |     50 |      50 |              50 |               50 |    50 | ice       | None      |
| glalie                     |   362 | glalie       |     15 |   2565 |              168 |      480 |  80 |     80 |      80 |              80 |               80 |    80 | ice       | None      |
| spheal                     |   363 | spheal       |      8 |    395 |               58 |      290 |  70 |     40 |      50 |              55 |               50 |    25 | ice       | water     |
| sealeo                     |   364 | sealeo       |     11 |    876 |              144 |      410 |  90 |     60 |      70 |              75 |               70 |    45 | ice       | water     |
| walrein                    |   365 | walrein      |     14 |   1506 |              239 |      530 | 110 |     80 |      90 |              95 |               90 |    65 | ice       | water     |
| clamperl                   |   366 | clamperl     |      4 |    525 |               69 |      345 |  35 |     64 |      85 |              74 |               55 |    32 | water     | None      |
| huntail                    |   367 | huntail      |     17 |    270 |              170 |      485 |  55 |    104 |     105 |              94 |               75 |    52 | water     | None      |
| gorebyss                   |   368 | gorebyss     |     18 |    226 |              170 |      485 |  55 |     84 |     105 |             114 |               75 |    52 | water     | None      |
| relicanth                  |   369 | relicanth    |     10 |    234 |              170 |      485 | 100 |     90 |     130 |              45 |               65 |    55 | water     | rock      |
| luvdisc                    |   370 | luvdisc      |      6 |     87 |              116 |      330 |  43 |     30 |      55 |              40 |               65 |    97 | water     | None      |
| bagon                      |   371 | bagon        |      6 |    421 |               60 |      300 |  45 |     75 |      60 |              40 |               30 |    50 | dragon    | None      |
| shelgon                    |   372 | shelgon      |     11 |   1105 |              147 |      420 |  65 |     95 |     100 |              60 |               50 |    50 | dragon    | None      |
| salamence                  |   373 | salamence    |     15 |   1026 |              270 |      600 |  95 |    135 |      80 |             110 |               80 |   100 | dragon    | flying    |
| beldum                     |   374 | beldum       |      6 |    952 |               60 |      300 |  40 |     55 |      80 |              35 |               60 |    30 | steel     | psychic   |
| metang                     |   375 | metang       |     12 |   2025 |              147 |      420 |  60 |     75 |     100 |              55 |               80 |    50 | steel     | psychic   |
| metagross                  |   376 | metagross    |     16 |   5500 |              270 |      600 |  80 |    135 |     130 |              95 |               90 |    70 | steel     | psychic   |
| regirock                   |   377 | regirock     |     17 |   2300 |              261 |      580 |  80 |    100 |     200 |              50 |              100 |    50 | rock      | None      |
| regice                     |   378 | regice       |     18 |   1750 |              261 |      580 |  80 |     50 |     100 |             100 |              200 |    50 | ice       | None      |
| registeel                  |   379 | registeel    |     19 |   2050 |              261 |      580 |  80 |     75 |     150 |              75 |              150 |    50 | steel     | None      |
| latias                     |   380 | latias       |     14 |    400 |              270 |      600 |  80 |     80 |      90 |             110 |              130 |   110 | dragon    | psychic   |
| latios                     |   381 | latios       |     20 |    600 |              270 |      600 |  80 |     90 |      80 |             130 |              110 |   110 | dragon    | psychic   |
| kyogre                     |   382 | kyogre       |     45 |   3520 |              302 |      670 | 100 |    100 |      90 |             150 |              140 |    90 | water     | None      |
| groudon                    |   383 | groudon      |     35 |   9500 |              302 |      670 | 100 |    150 |     140 |             100 |               90 |    90 | ground    | None      |
| rayquaza                   |   384 | rayquaza     |     70 |   2065 |              306 |      680 | 105 |    150 |      90 |             150 |               90 |    95 | dragon    | flying    |
| jirachi                    |   385 | jirachi      |      3 |     11 |              270 |      600 | 100 |    100 |     100 |             100 |              100 |   100 | steel     | psychic   |
| deoxys-normal              |   386 | deoxys       |     17 |    608 |              270 |      600 |  50 |    150 |      50 |             150 |               50 |   150 | psychic   | None      |
| turtwig                    |   387 | turtwig      |      4 |    102 |               64 |      318 |  55 |     68 |      64 |              45 |               55 |    31 | grass     | None      |
| grotle                     |   388 | grotle       |     11 |    970 |              142 |      405 |  75 |     89 |      85 |              55 |               65 |    36 | grass     | None      |
| torterra                   |   389 | torterra     |     22 |   3100 |              236 |      525 |  95 |    109 |     105 |              75 |               85 |    56 | grass     | ground    |
| chimchar                   |   390 | chimchar     |      5 |     62 |               62 |      309 |  44 |     58 |      44 |              58 |               44 |    61 | fire      | None      |
| monferno                   |   391 | monferno     |      9 |    220 |              142 |      405 |  64 |     78 |      52 |              78 |               52 |    81 | fire      | fighting  |
| infernape                  |   392 | infernape    |     12 |    550 |              240 |      534 |  76 |    104 |      71 |             104 |               71 |   108 | fire      | fighting  |
| piplup                     |   393 | piplup       |      4 |     52 |               63 |      314 |  53 |     51 |      53 |              61 |               56 |    40 | water     | None      |
| prinplup                   |   394 | prinplup     |      8 |    230 |              142 |      405 |  64 |     66 |      68 |              81 |               76 |    50 | water     | None      |
| empoleon                   |   395 | empoleon     |     17 |    845 |              239 |      530 |  84 |     86 |      88 |             111 |              101 |    60 | water     | steel     |
| starly                     |   396 | starly       |      3 |     20 |               49 |      245 |  40 |     55 |      30 |              30 |               30 |    60 | normal    | flying    |
| staravia                   |   397 | staravia     |      6 |    155 |              119 |      340 |  55 |     75 |      50 |              40 |               40 |    80 | normal    | flying    |
| staraptor                  |   398 | staraptor    |     12 |    249 |              218 |      485 |  85 |    120 |      70 |              50 |               60 |   100 | normal    | flying    |
| bidoof                     |   399 | bidoof       |      5 |    200 |               50 |      250 |  59 |     45 |      40 |              35 |               40 |    31 | normal    | None      |
| bibarel                    |   400 | bibarel      |     10 |    315 |              144 |      410 |  79 |     85 |      60 |              55 |               60 |    71 | normal    | water     |
| kricketot                  |   401 | kricketot    |      3 |     22 |               39 |      194 |  37 |     25 |      41 |              25 |               41 |    25 | bug       | None      |
| kricketune                 |   402 | kricketune   |     10 |    255 |              134 |      384 |  77 |     85 |      51 |              55 |               51 |    65 | bug       | None      |
| shinx                      |   403 | shinx        |      5 |     95 |               53 |      263 |  45 |     65 |      34 |              40 |               34 |    45 | electric  | None      |
| luxio                      |   404 | luxio        |      9 |    305 |              127 |      363 |  60 |     85 |      49 |              60 |               49 |    60 | electric  | None      |
| luxray                     |   405 | luxray       |     14 |    420 |              235 |      523 |  80 |    120 |      79 |              95 |               79 |    70 | electric  | None      |
| budew                      |   406 | budew        |      2 |     12 |               56 |      280 |  40 |     30 |      35 |              50 |               70 |    55 | grass     | poison    |
| roserade                   |   407 | roserade     |      9 |    145 |              232 |      515 |  60 |     70 |      65 |             125 |              105 |    90 | grass     | poison    |
| cranidos                   |   408 | cranidos     |      9 |    315 |               70 |      350 |  67 |    125 |      40 |              30 |               30 |    58 | rock      | None      |
| rampardos                  |   409 | rampardos    |     16 |   1025 |              173 |      495 |  97 |    165 |      60 |              65 |               50 |    58 | rock      | None      |
| shieldon                   |   410 | shieldon     |      5 |    570 |               70 |      350 |  30 |     42 |     118 |              42 |               88 |    30 | rock      | steel     |
| bastiodon                  |   411 | bastiodon    |     13 |   1495 |              173 |      495 |  60 |     52 |     168 |              47 |              138 |    30 | rock      | steel     |
| burmy                      |   412 | burmy        |      2 |     34 |               45 |      224 |  40 |     29 |      45 |              29 |               45 |    36 | bug       | None      |
| wormadam-plant             |   413 | wormadam     |      5 |     65 |              148 |      424 |  60 |     59 |      85 |              79 |              105 |    36 | bug       | grass     |
| mothim                     |   414 | mothim       |      9 |    233 |              148 |      424 |  70 |     94 |      50 |              94 |               50 |    66 | bug       | flying    |
| combee                     |   415 | combee       |      3 |     55 |               49 |      244 |  30 |     30 |      42 |              30 |               42 |    70 | bug       | flying    |
| vespiquen                  |   416 | vespiquen    |     12 |    385 |              166 |      474 |  70 |     80 |     102 |              80 |              102 |    40 | bug       | flying    |
| pachirisu                  |   417 | pachirisu    |      4 |     39 |              142 |      405 |  60 |     45 |      70 |              45 |               90 |    95 | electric  | None      |
| buizel                     |   418 | buizel       |      7 |    295 |               66 |      330 |  55 |     65 |      35 |              60 |               30 |    85 | water     | None      |
| floatzel                   |   419 | floatzel     |     11 |    335 |              173 |      495 |  85 |    105 |      55 |              85 |               50 |   115 | water     | None      |
| cherubi                    |   420 | cherubi      |      4 |     33 |               55 |      275 |  45 |     35 |      45 |              62 |               53 |    35 | grass     | None      |
| cherrim                    |   421 | cherrim      |      5 |     93 |              158 |      450 |  70 |     60 |      70 |              87 |               78 |    85 | grass     | None      |
| shellos                    |   422 | shellos      |      3 |     63 |               65 |      325 |  76 |     48 |      48 |              57 |               62 |    34 | water     | None      |
| gastrodon                  |   423 | gastrodon    |      9 |    299 |              166 |      475 | 111 |     83 |      68 |              92 |               82 |    39 | water     | ground    |
| ambipom                    |   424 | ambipom      |     12 |    203 |              169 |      482 |  75 |    100 |      66 |              60 |               66 |   115 | normal    | None      |
| drifloon                   |   425 | drifloon     |      4 |     12 |               70 |      348 |  90 |     50 |      34 |              60 |               44 |    70 | ghost     | flying    |
| drifblim                   |   426 | drifblim     |     12 |    150 |              174 |      498 | 150 |     80 |      44 |              90 |               54 |    80 | ghost     | flying    |
| buneary                    |   427 | buneary      |      4 |     55 |               70 |      350 |  55 |     66 |      44 |              44 |               56 |    85 | normal    | None      |
| lopunny                    |   428 | lopunny      |     12 |    333 |              168 |      480 |  65 |     76 |      84 |              54 |               96 |   105 | normal    | None      |
| mismagius                  |   429 | mismagius    |      9 |     44 |              173 |      495 |  60 |     60 |      60 |             105 |              105 |   105 | ghost     | None      |
| honchkrow                  |   430 | honchkrow    |      9 |    273 |              177 |      505 | 100 |    125 |      52 |             105 |               52 |    71 | dark      | flying    |
| glameow                    |   431 | glameow      |      5 |     39 |               62 |      310 |  49 |     55 |      42 |              42 |               37 |    85 | normal    | None      |
| purugly                    |   432 | purugly      |     10 |    438 |              158 |      452 |  71 |     82 |      64 |              64 |               59 |   112 | normal    | None      |
| chingling                  |   433 | chingling    |      2 |      6 |               57 |      285 |  45 |     30 |      50 |              65 |               50 |    45 | psychic   | None      |
| stunky                     |   434 | stunky       |      4 |    192 |               66 |      329 |  63 |     63 |      47 |              41 |               41 |    74 | poison    | dark      |
| skuntank                   |   435 | skuntank     |     10 |    380 |              168 |      479 | 103 |     93 |      67 |              71 |               61 |    84 | poison    | dark      |
| bronzor                    |   436 | bronzor      |      5 |    605 |               60 |      300 |  57 |     24 |      86 |              24 |               86 |    23 | steel     | psychic   |
| bronzong                   |   437 | bronzong     |     13 |   1870 |              175 |      500 |  67 |     89 |     116 |              79 |              116 |    33 | steel     | psychic   |
| bonsly                     |   438 | bonsly       |      5 |    150 |               58 |      290 |  50 |     80 |      95 |              10 |               45 |    10 | rock      | None      |
| mime-jr                    |   439 | mime-jr      |      6 |    130 |               62 |      310 |  20 |     25 |      45 |              70 |               90 |    60 | psychic   | fairy     |
| happiny                    |   440 | happiny      |      6 |    244 |              110 |      220 | 100 |      5 |       5 |              15 |               65 |    30 | normal    | None      |
| chatot                     |   441 | chatot       |      5 |     19 |              144 |      411 |  76 |     65 |      45 |              92 |               42 |    91 | normal    | flying    |
| spiritomb                  |   442 | spiritomb    |     10 |   1080 |              170 |      485 |  50 |     92 |     108 |              92 |              108 |    35 | ghost     | dark      |
| gible                      |   443 | gible        |      7 |    205 |               60 |      300 |  58 |     70 |      45 |              40 |               45 |    42 | dragon    | ground    |
| gabite                     |   444 | gabite       |     14 |    560 |              144 |      410 |  68 |     90 |      65 |              50 |               55 |    82 | dragon    | ground    |
| garchomp                   |   445 | garchomp     |     19 |    950 |              270 |      600 | 108 |    130 |      95 |              80 |               85 |   102 | dragon    | ground    |
| munchlax                   |   446 | munchlax     |      6 |   1050 |               78 |      390 | 135 |     85 |      40 |              40 |               85 |     5 | normal    | None      |
| riolu                      |   447 | riolu        |      7 |    202 |               57 |      285 |  40 |     70 |      40 |              35 |               40 |    60 | fighting  | None      |
| lucario                    |   448 | lucario      |     12 |    540 |              184 |      525 |  70 |    110 |      70 |             115 |               70 |    90 | fighting  | steel     |
| hippopotas                 |   449 | hippopotas   |      8 |    495 |               66 |      330 |  68 |     72 |      78 |              38 |               42 |    32 | ground    | None      |
| hippowdon                  |   450 | hippowdon    |     20 |   3000 |              184 |      525 | 108 |    112 |     118 |              68 |               72 |    47 | ground    | None      |
| skorupi                    |   451 | skorupi      |      8 |    120 |               66 |      330 |  40 |     50 |      90 |              30 |               55 |    65 | poison    | bug       |
| drapion                    |   452 | drapion      |     13 |    615 |              175 |      500 |  70 |     90 |     110 |              60 |               75 |    95 | poison    | dark      |
| croagunk                   |   453 | croagunk     |      7 |    230 |               60 |      300 |  48 |     61 |      40 |              61 |               40 |    50 | poison    | fighting  |
| toxicroak                  |   454 | toxicroak    |     13 |    444 |              172 |      490 |  83 |    106 |      65 |              86 |               65 |    85 | poison    | fighting  |
| carnivine                  |   455 | carnivine    |     14 |    270 |              159 |      454 |  74 |    100 |      72 |              90 |               72 |    46 | grass     | None      |
| finneon                    |   456 | finneon      |      4 |     70 |               66 |      330 |  49 |     49 |      56 |              49 |               61 |    66 | water     | None      |
| lumineon                   |   457 | lumineon     |     12 |    240 |              161 |      460 |  69 |     69 |      76 |              69 |               86 |    91 | water     | None      |
| mantyke                    |   458 | mantyke      |     10 |    650 |               69 |      345 |  45 |     20 |      50 |              60 |              120 |    50 | water     | flying    |
| snover                     |   459 | snover       |     10 |    505 |               67 |      334 |  60 |     62 |      50 |              62 |               60 |    40 | grass     | ice       |
| abomasnow                  |   460 | abomasnow    |     22 |   1355 |              173 |      494 |  90 |     92 |      75 |              92 |               85 |    60 | grass     | ice       |
| weavile                    |   461 | weavile      |     11 |    340 |              179 |      510 |  70 |    120 |      65 |              45 |               85 |   125 | dark      | ice       |
| magnezone                  |   462 | magnezone    |     12 |   1800 |              241 |      535 |  70 |     70 |     115 |             130 |               90 |    60 | electric  | steel     |
| lickilicky                 |   463 | lickilicky   |     17 |   1400 |              180 |      515 | 110 |     85 |      95 |              80 |               95 |    50 | normal    | None      |
| rhyperior                  |   464 | rhyperior    |     24 |   2828 |              241 |      535 | 115 |    140 |     130 |              55 |               55 |    40 | ground    | rock      |
| tangrowth                  |   465 | tangrowth    |     20 |   1286 |              187 |      535 | 100 |    100 |     125 |             110 |               50 |    50 | grass     | None      |
| electivire                 |   466 | electivire   |     18 |   1386 |              243 |      540 |  75 |    123 |      67 |              95 |               85 |    95 | electric  | None      |
| magmortar                  |   467 | magmortar    |     16 |    680 |              243 |      540 |  75 |     95 |      67 |             125 |               95 |    83 | fire      | None      |
| togekiss                   |   468 | togekiss     |     15 |    380 |              245 |      545 |  85 |     50 |      95 |             120 |              115 |    80 | fairy     | flying    |
| yanmega                    |   469 | yanmega      |     19 |    515 |              180 |      515 |  86 |     76 |      86 |             116 |               56 |    95 | bug       | flying    |
| leafeon                    |   470 | leafeon      |     10 |    255 |              184 |      525 |  65 |    110 |     130 |              60 |               65 |    95 | grass     | None      |
| glaceon                    |   471 | glaceon      |      8 |    259 |              184 |      525 |  65 |     60 |     110 |             130 |               95 |    65 | ice       | None      |
| gliscor                    |   472 | gliscor      |     20 |    425 |              179 |      510 |  75 |     95 |     125 |              45 |               75 |    95 | ground    | flying    |
| mamoswine                  |   473 | mamoswine    |     25 |   2910 |              239 |      530 | 110 |    130 |      80 |              70 |               60 |    80 | ice       | ground    |
| porygon-z                  |   474 | porygon-z    |      9 |    340 |              241 |      535 |  85 |     80 |      70 |             135 |               75 |    90 | normal    | None      |
| gallade                    |   475 | gallade      |     16 |    520 |              233 |      518 |  68 |    125 |      65 |              65 |              115 |    80 | psychic   | fighting  |
| probopass                  |   476 | probopass    |     14 |   3400 |              184 |      525 |  60 |     55 |     145 |              75 |              150 |    40 | rock      | steel     |
| dusknoir                   |   477 | dusknoir     |     22 |   1066 |              236 |      525 |  45 |    100 |     135 |              65 |              135 |    45 | ghost     | None      |
| froslass                   |   478 | froslass     |     13 |    266 |              168 |      480 |  70 |     80 |      70 |              80 |               70 |   110 | ice       | ghost     |
| rotom                      |   479 | rotom        |      3 |      3 |              154 |      440 |  50 |     50 |      77 |              95 |               77 |    91 | electric  | ghost     |
| uxie                       |   480 | uxie         |      3 |      3 |              261 |      580 |  75 |     75 |     130 |              75 |              130 |    95 | psychic   | None      |
| mesprit                    |   481 | mesprit      |      3 |      3 |              261 |      580 |  80 |    105 |     105 |             105 |              105 |    80 | psychic   | None      |
| azelf                      |   482 | azelf        |      3 |      3 |              261 |      580 |  75 |    125 |      70 |             125 |               70 |   115 | psychic   | None      |
| dialga                     |   483 | dialga       |     54 |   6830 |              306 |      680 | 100 |    120 |     120 |             150 |              100 |    90 | steel     | dragon    |
| palkia                     |   484 | palkia       |     42 |   3360 |              306 |      680 |  90 |    120 |     100 |             150 |              120 |   100 | water     | dragon    |
| heatran                    |   485 | heatran      |     17 |   4300 |              270 |      600 |  91 |     90 |     106 |             130 |              106 |    77 | fire      | steel     |
| regigigas                  |   486 | regigigas    |     37 |   4200 |              302 |      670 | 110 |    160 |     110 |              80 |              110 |   100 | normal    | None      |
| giratina-altered           |   487 | giratina     |     45 |   7500 |              306 |      680 | 150 |    100 |     120 |             100 |              120 |    90 | ghost     | dragon    |
| cresselia                  |   488 | cresselia    |     15 |    856 |              270 |      600 | 120 |     70 |     120 |              75 |              130 |    85 | psychic   | None      |
| phione                     |   489 | phione       |      4 |     31 |              216 |      480 |  80 |     80 |      80 |              80 |               80 |    80 | water     | None      |
| manaphy                    |   490 | manaphy      |      3 |     14 |              270 |      600 | 100 |    100 |     100 |             100 |              100 |   100 | water     | None      |
| darkrai                    |   491 | darkrai      |     15 |    505 |              270 |      600 |  70 |     90 |      90 |             135 |               90 |   125 | dark      | None      |
| shaymin-land               |   492 | shaymin      |      2 |     21 |              270 |      600 | 100 |    100 |     100 |             100 |              100 |   100 | grass     | None      |
| arceus                     |   493 | arceus       |     32 |   3200 |              324 |      720 | 120 |    120 |     120 |             120 |              120 |   120 | normal    | None      |
| victini                    |   494 | victini      |      4 |     40 |              270 |      600 | 100 |    100 |     100 |             100 |              100 |   100 | psychic   | fire      |
| snivy                      |   495 | snivy        |      6 |     81 |               62 |      308 |  45 |     45 |      55 |              45 |               55 |    63 | grass     | None      |
| servine                    |   496 | servine      |      8 |    160 |              145 |      413 |  60 |     60 |      75 |              60 |               75 |    83 | grass     | None      |
| serperior                  |   497 | serperior    |     33 |    630 |              238 |      528 |  75 |     75 |      95 |              75 |               95 |   113 | grass     | None      |
| tepig                      |   498 | tepig        |      5 |     99 |               62 |      308 |  65 |     63 |      45 |              45 |               45 |    45 | fire      | None      |
| pignite                    |   499 | pignite      |     10 |    555 |              146 |      418 |  90 |     93 |      55 |              70 |               55 |    55 | fire      | fighting  |
| emboar                     |   500 | emboar       |     16 |   1500 |              238 |      528 | 110 |    123 |      65 |             100 |               65 |    65 | fire      | fighting  |
| oshawott                   |   501 | oshawott     |      5 |     59 |               62 |      308 |  55 |     55 |      45 |              63 |               45 |    45 | water     | None      |
| dewott                     |   502 | dewott       |      8 |    245 |              145 |      413 |  75 |     75 |      60 |              83 |               60 |    60 | water     | None      |
| samurott                   |   503 | samurott     |     15 |    946 |              238 |      528 |  95 |    100 |      85 |             108 |               70 |    70 | water     | None      |
| patrat                     |   504 | patrat       |      5 |    116 |               51 |      255 |  45 |     55 |      39 |              35 |               39 |    42 | normal    | None      |
| watchog                    |   505 | watchog      |     11 |    270 |              147 |      420 |  60 |     85 |      69 |              60 |               69 |    77 | normal    | None      |
| lillipup                   |   506 | lillipup     |      4 |     41 |               55 |      275 |  45 |     60 |      45 |              25 |               45 |    55 | normal    | None      |
| herdier                    |   507 | herdier      |      9 |    147 |              130 |      370 |  65 |     80 |      65 |              35 |               65 |    60 | normal    | None      |
| stoutland                  |   508 | stoutland    |     12 |    610 |              225 |      500 |  85 |    110 |      90 |              45 |               90 |    80 | normal    | None      |
| purrloin                   |   509 | purrloin     |      4 |    101 |               56 |      281 |  41 |     50 |      37 |              50 |               37 |    66 | dark      | None      |
| liepard                    |   510 | liepard      |     11 |    375 |              156 |      446 |  64 |     88 |      50 |              88 |               50 |   106 | dark      | None      |
| pansage                    |   511 | pansage      |      6 |    105 |               63 |      316 |  50 |     53 |      48 |              53 |               48 |    64 | grass     | None      |
| simisage                   |   512 | simisage     |     11 |    305 |              174 |      498 |  75 |     98 |      63 |              98 |               63 |   101 | grass     | None      |
| pansear                    |   513 | pansear      |      6 |    110 |               63 |      316 |  50 |     53 |      48 |              53 |               48 |    64 | fire      | None      |
| simisear                   |   514 | simisear     |     10 |    280 |              174 |      498 |  75 |     98 |      63 |              98 |               63 |   101 | fire      | None      |
| panpour                    |   515 | panpour      |      6 |    135 |               63 |      316 |  50 |     53 |      48 |              53 |               48 |    64 | water     | None      |
| simipour                   |   516 | simipour     |     10 |    290 |              174 |      498 |  75 |     98 |      63 |              98 |               63 |   101 | water     | None      |
| munna                      |   517 | munna        |      6 |    233 |               58 |      292 |  76 |     25 |      45 |              67 |               55 |    24 | psychic   | None      |
| musharna                   |   518 | musharna     |     11 |    605 |              170 |      487 | 116 |     55 |      85 |             107 |               95 |    29 | psychic   | None      |
| pidove                     |   519 | pidove       |      3 |     21 |               53 |      264 |  50 |     55 |      50 |              36 |               30 |    43 | normal    | flying    |
| tranquill                  |   520 | tranquill    |      6 |    150 |              125 |      358 |  62 |     77 |      62 |              50 |               42 |    65 | normal    | flying    |
| unfezant                   |   521 | unfezant     |     12 |    290 |              220 |      488 |  80 |    115 |      80 |              65 |               55 |    93 | normal    | flying    |
| blitzle                    |   522 | blitzle      |      8 |    298 |               59 |      295 |  45 |     60 |      32 |              50 |               32 |    76 | electric  | None      |
| zebstrika                  |   523 | zebstrika    |     16 |    795 |              174 |      497 |  75 |    100 |      63 |              80 |               63 |   116 | electric  | None      |
| roggenrola                 |   524 | roggenrola   |      4 |    180 |               56 |      280 |  55 |     75 |      85 |              25 |               25 |    15 | rock      | None      |
| boldore                    |   525 | boldore      |      9 |   1020 |              137 |      390 |  70 |    105 |     105 |              50 |               40 |    20 | rock      | None      |
| gigalith                   |   526 | gigalith     |     17 |   2600 |              232 |      515 |  85 |    135 |     130 |              60 |               80 |    25 | rock      | None      |
| woobat                     |   527 | woobat       |      4 |     21 |               65 |      323 |  65 |     45 |      43 |              55 |               43 |    72 | psychic   | flying    |
| swoobat                    |   528 | swoobat      |      9 |    105 |              149 |      425 |  67 |     57 |      55 |              77 |               55 |   114 | psychic   | flying    |
| drilbur                    |   529 | drilbur      |      3 |     85 |               66 |      328 |  60 |     85 |      40 |              30 |               45 |    68 | ground    | None      |
| excadrill                  |   530 | excadrill    |      7 |    404 |              178 |      508 | 110 |    135 |      60 |              50 |               65 |    88 | ground    | steel     |
| audino                     |   531 | audino       |     11 |    310 |              390 |      445 | 103 |     60 |      86 |              60 |               86 |    50 | normal    | None      |
| timburr                    |   532 | timburr      |      6 |    125 |               61 |      305 |  75 |     80 |      55 |              25 |               35 |    35 | fighting  | None      |
| gurdurr                    |   533 | gurdurr      |     12 |    400 |              142 |      405 |  85 |    105 |      85 |              40 |               50 |    40 | fighting  | None      |
| conkeldurr                 |   534 | conkeldurr   |     14 |    870 |              227 |      505 | 105 |    140 |      95 |              55 |               65 |    45 | fighting  | None      |
| tympole                    |   535 | tympole      |      5 |     45 |               59 |      294 |  50 |     50 |      40 |              50 |               40 |    64 | water     | None      |
| palpitoad                  |   536 | palpitoad    |      8 |    170 |              134 |      384 |  75 |     65 |      55 |              65 |               55 |    69 | water     | ground    |
| seismitoad                 |   537 | seismitoad   |     15 |    620 |              229 |      509 | 105 |     95 |      75 |              85 |               75 |    74 | water     | ground    |
| throh                      |   538 | throh        |     13 |    555 |              163 |      465 | 120 |    100 |      85 |              30 |               85 |    45 | fighting  | None      |
| sawk                       |   539 | sawk         |     14 |    510 |              163 |      465 |  75 |    125 |      75 |              30 |               75 |    85 | fighting  | None      |
| sewaddle                   |   540 | sewaddle     |      3 |     25 |               62 |      310 |  45 |     53 |      70 |              40 |               60 |    42 | bug       | grass     |
| swadloon                   |   541 | swadloon     |      5 |     73 |              133 |      380 |  55 |     63 |      90 |              50 |               80 |    42 | bug       | grass     |
| leavanny                   |   542 | leavanny     |     12 |    205 |              225 |      500 |  75 |    103 |      80 |              70 |               80 |    92 | bug       | grass     |
| venipede                   |   543 | venipede     |      4 |     53 |               52 |      260 |  30 |     45 |      59 |              30 |               39 |    57 | bug       | poison    |
| whirlipede                 |   544 | whirlipede   |     12 |    585 |              126 |      360 |  40 |     55 |      99 |              40 |               79 |    47 | bug       | poison    |
| scolipede                  |   545 | scolipede    |     25 |   2005 |              218 |      485 |  60 |    100 |      89 |              55 |               69 |   112 | bug       | poison    |
| cottonee                   |   546 | cottonee     |      3 |      6 |               56 |      280 |  40 |     27 |      60 |              37 |               50 |    66 | grass     | fairy     |
| whimsicott                 |   547 | whimsicott   |      7 |     66 |              168 |      480 |  60 |     67 |      85 |              77 |               75 |   116 | grass     | fairy     |
| petilil                    |   548 | petilil      |      5 |     66 |               56 |      280 |  45 |     35 |      50 |              70 |               50 |    30 | grass     | None      |
| lilligant                  |   549 | lilligant    |     11 |    163 |              168 |      480 |  70 |     60 |      75 |             110 |               75 |    90 | grass     | None      |
| basculin-red-striped       |   550 | basculin     |     10 |    180 |              161 |      460 |  70 |     92 |      65 |              80 |               55 |    98 | water     | None      |
| sandile                    |   551 | sandile      |      7 |    152 |               58 |      292 |  50 |     72 |      35 |              35 |               35 |    65 | ground    | dark      |
| krokorok                   |   552 | krokorok     |     10 |    334 |              123 |      351 |  60 |     82 |      45 |              45 |               45 |    74 | ground    | dark      |
| krookodile                 |   553 | krookodile   |     15 |    963 |              234 |      519 |  95 |    117 |      80 |              65 |               70 |    92 | ground    | dark      |
| darumaka                   |   554 | darumaka     |      6 |    375 |               63 |      315 |  70 |     90 |      45 |              15 |               45 |    50 | fire      | None      |
| darmanitan-standard        |   555 | darmanitan   |     13 |    929 |              168 |      480 | 105 |    140 |      55 |              30 |               55 |    95 | fire      | None      |
| maractus                   |   556 | maractus     |     10 |    280 |              161 |      461 |  75 |     86 |      67 |             106 |               67 |    60 | grass     | None      |
| dwebble                    |   557 | dwebble      |      3 |    145 |               65 |      325 |  50 |     65 |      85 |              35 |               35 |    55 | bug       | rock      |
| crustle                    |   558 | crustle      |     14 |   2000 |              170 |      485 |  70 |    105 |     125 |              65 |               75 |    45 | bug       | rock      |
| scraggy                    |   559 | scraggy      |      6 |    118 |               70 |      348 |  50 |     75 |      70 |              35 |               70 |    48 | dark      | fighting  |
| scrafty                    |   560 | scrafty      |     11 |    300 |              171 |      488 |  65 |     90 |     115 |              45 |              115 |    58 | dark      | fighting  |
| sigilyph                   |   561 | sigilyph     |     14 |    140 |              172 |      490 |  72 |     58 |      80 |             103 |               80 |    97 | psychic   | flying    |
| yamask                     |   562 | yamask       |      5 |     15 |               61 |      303 |  38 |     30 |      85 |              55 |               65 |    30 | ghost     | None      |
| cofagrigus                 |   563 | cofagrigus   |     17 |    765 |              169 |      483 |  58 |     50 |     145 |              95 |              105 |    30 | ghost     | None      |
| tirtouga                   |   564 | tirtouga     |      7 |    165 |               71 |      355 |  54 |     78 |     103 |              53 |               45 |    22 | water     | rock      |
| carracosta                 |   565 | carracosta   |     12 |    810 |              173 |      495 |  74 |    108 |     133 |              83 |               65 |    32 | water     | rock      |
| archen                     |   566 | archen       |      5 |     95 |               71 |      401 |  55 |    112 |      45 |              74 |               45 |    70 | rock      | flying    |
| archeops                   |   567 | archeops     |     14 |    320 |              177 |      567 |  75 |    140 |      65 |             112 |               65 |   110 | rock      | flying    |
| trubbish                   |   568 | trubbish     |      6 |    310 |               66 |      329 |  50 |     50 |      62 |              40 |               62 |    65 | poison    | None      |
| garbodor                   |   569 | garbodor     |     19 |   1073 |              166 |      474 |  80 |     95 |      82 |              60 |               82 |    75 | poison    | None      |
| zorua                      |   570 | zorua        |      7 |    125 |               66 |      330 |  40 |     65 |      40 |              80 |               40 |    65 | dark      | None      |
| zoroark                    |   571 | zoroark      |     16 |    811 |              179 |      510 |  60 |    105 |      60 |             120 |               60 |   105 | dark      | None      |
| minccino                   |   572 | minccino     |      4 |     58 |               60 |      300 |  55 |     50 |      40 |              40 |               40 |    75 | normal    | None      |
| cinccino                   |   573 | cinccino     |      5 |     75 |              165 |      470 |  75 |     95 |      60 |              65 |               60 |   115 | normal    | None      |
| gothita                    |   574 | gothita      |      4 |     58 |               58 |      290 |  45 |     30 |      50 |              55 |               65 |    45 | psychic   | None      |
| gothorita                  |   575 | gothorita    |      7 |    180 |              137 |      390 |  60 |     45 |      70 |              75 |               85 |    55 | psychic   | None      |
| gothitelle                 |   576 | gothitelle   |     15 |    440 |              221 |      490 |  70 |     55 |      95 |              95 |              110 |    65 | psychic   | None      |
| solosis                    |   577 | solosis      |      3 |     10 |               58 |      290 |  45 |     30 |      40 |             105 |               50 |    20 | psychic   | None      |
| duosion                    |   578 | duosion      |      6 |     80 |              130 |      370 |  65 |     40 |      50 |             125 |               60 |    30 | psychic   | None      |
| reuniclus                  |   579 | reuniclus    |     10 |    201 |              221 |      490 | 110 |     65 |      75 |             125 |               85 |    30 | psychic   | None      |
| ducklett                   |   580 | ducklett     |      5 |     55 |               61 |      305 |  62 |     44 |      50 |              44 |               50 |    55 | water     | flying    |
| swanna                     |   581 | swanna       |     13 |    242 |              166 |      473 |  75 |     87 |      63 |              87 |               63 |    98 | water     | flying    |
| vanillite                  |   582 | vanillite    |      4 |     57 |               61 |      305 |  36 |     50 |      50 |              65 |               60 |    44 | ice       | None      |
| vanillish                  |   583 | vanillish    |     11 |    410 |              138 |      395 |  51 |     65 |      65 |              80 |               75 |    59 | ice       | None      |
| vanilluxe                  |   584 | vanilluxe    |     13 |    575 |              241 |      535 |  71 |     95 |      85 |             110 |               95 |    79 | ice       | None      |
| deerling                   |   585 | deerling     |      6 |    195 |               67 |      335 |  60 |     60 |      50 |              40 |               50 |    75 | normal    | grass     |
| sawsbuck                   |   586 | sawsbuck     |     19 |    925 |              166 |      475 |  80 |    100 |      70 |              60 |               70 |    95 | normal    | grass     |
| emolga                     |   587 | emolga       |      4 |     50 |              150 |      428 |  55 |     75 |      60 |              75 |               60 |   103 | electric  | flying    |
| karrablast                 |   588 | karrablast   |      5 |     59 |               63 |      315 |  50 |     75 |      45 |              40 |               45 |    60 | bug       | None      |
| escavalier                 |   589 | escavalier   |     10 |    330 |              173 |      495 |  70 |    135 |     105 |              60 |              105 |    20 | bug       | steel     |
| foongus                    |   590 | foongus      |      2 |     10 |               59 |      294 |  69 |     55 |      45 |              55 |               55 |    15 | grass     | poison    |
| amoonguss                  |   591 | amoonguss    |      6 |    105 |              162 |      464 | 114 |     85 |      70 |              85 |               80 |    30 | grass     | poison    |
| frillish                   |   592 | frillish     |     12 |    330 |               67 |      335 |  55 |     40 |      50 |              65 |               85 |    40 | water     | ghost     |
| jellicent                  |   593 | jellicent    |     22 |   1350 |              168 |      480 | 100 |     60 |      70 |              85 |              105 |    60 | water     | ghost     |
| alomomola                  |   594 | alomomola    |     12 |    316 |              165 |      470 | 165 |     75 |      80 |              40 |               45 |    65 | water     | None      |
| joltik                     |   595 | joltik       |      1 |      6 |               64 |      319 |  50 |     47 |      50 |              57 |               50 |    65 | bug       | electric  |
| galvantula                 |   596 | galvantula   |      8 |    143 |              165 |      472 |  70 |     77 |      60 |              97 |               60 |   108 | bug       | electric  |
| ferroseed                  |   597 | ferroseed    |      6 |    188 |               61 |      305 |  44 |     50 |      91 |              24 |               86 |    10 | grass     | steel     |
| ferrothorn                 |   598 | ferrothorn   |     10 |   1100 |              171 |      489 |  74 |     94 |     131 |              54 |              116 |    20 | grass     | steel     |
| klink                      |   599 | klink        |      3 |    210 |               60 |      300 |  40 |     55 |      70 |              45 |               60 |    30 | steel     | None      |
| klang                      |   600 | klang        |      6 |    510 |              154 |      440 |  60 |     80 |      95 |              70 |               85 |    50 | steel     | None      |
| klinklang                  |   601 | klinklang    |      6 |    810 |              234 |      520 |  60 |    100 |     115 |              70 |               85 |    90 | steel     | None      |
| tynamo                     |   602 | tynamo       |      2 |      3 |               55 |      275 |  35 |     55 |      40 |              45 |               40 |    60 | electric  | None      |
| eelektrik                  |   603 | eelektrik    |     12 |    220 |              142 |      405 |  65 |     85 |      70 |              75 |               70 |    40 | electric  | None      |
| eelektross                 |   604 | eelektross   |     21 |    805 |              232 |      515 |  85 |    115 |      80 |             105 |               80 |    50 | electric  | None      |
| elgyem                     |   605 | elgyem       |      5 |     90 |               67 |      335 |  55 |     55 |      55 |              85 |               55 |    30 | psychic   | None      |
| beheeyem                   |   606 | beheeyem     |     10 |    345 |              170 |      485 |  75 |     75 |      75 |             125 |               95 |    40 | psychic   | None      |
| litwick                    |   607 | litwick      |      3 |     31 |               55 |      275 |  50 |     30 |      55 |              65 |               55 |    20 | ghost     | fire      |
| lampent                    |   608 | lampent      |      6 |    130 |              130 |      370 |  60 |     40 |      60 |              95 |               60 |    55 | ghost     | fire      |
| chandelure                 |   609 | chandelure   |     10 |    343 |              234 |      520 |  60 |     55 |      90 |             145 |               90 |    80 | ghost     | fire      |
| axew                       |   610 | axew         |      6 |    180 |               64 |      320 |  46 |     87 |      60 |              30 |               40 |    57 | dragon    | None      |
| fraxure                    |   611 | fraxure      |     10 |    360 |              144 |      410 |  66 |    117 |      70 |              40 |               50 |    67 | dragon    | None      |
| haxorus                    |   612 | haxorus      |     18 |   1055 |              243 |      540 |  76 |    147 |      90 |              60 |               70 |    97 | dragon    | None      |
| cubchoo                    |   613 | cubchoo      |      5 |     85 |               61 |      305 |  55 |     70 |      40 |              60 |               40 |    40 | ice       | None      |
| beartic                    |   614 | beartic      |     26 |   2600 |              177 |      505 |  95 |    130 |      80 |              70 |               80 |    50 | ice       | None      |
| cryogonal                  |   615 | cryogonal    |     11 |   1480 |              180 |      515 |  80 |     50 |      50 |              95 |              135 |   105 | ice       | None      |
| shelmet                    |   616 | shelmet      |      4 |     77 |               61 |      305 |  50 |     40 |      85 |              40 |               65 |    25 | bug       | None      |
| accelgor                   |   617 | accelgor     |      8 |    253 |              173 |      495 |  80 |     70 |      40 |             100 |               60 |   145 | bug       | None      |
| stunfisk                   |   618 | stunfisk     |      7 |    110 |              165 |      471 | 109 |     66 |      84 |              81 |               99 |    32 | ground    | electric  |
| mienfoo                    |   619 | mienfoo      |      9 |    200 |               70 |      350 |  45 |     85 |      50 |              55 |               50 |    65 | fighting  | None      |
| mienshao                   |   620 | mienshao     |     14 |    355 |              179 |      510 |  65 |    125 |      60 |              95 |               60 |   105 | fighting  | None      |
| druddigon                  |   621 | druddigon    |     16 |   1390 |              170 |      485 |  77 |    120 |      90 |              60 |               90 |    48 | dragon    | None      |
| golett                     |   622 | golett       |     10 |    920 |               61 |      303 |  59 |     74 |      50 |              35 |               50 |    35 | ground    | ghost     |
| golurk                     |   623 | golurk       |     28 |   3300 |              169 |      483 |  89 |    124 |      80 |              55 |               80 |    55 | ground    | ghost     |
| pawniard                   |   624 | pawniard     |      5 |    102 |               68 |      340 |  45 |     85 |      70 |              40 |               40 |    60 | dark      | steel     |
| bisharp                    |   625 | bisharp      |     16 |    700 |              172 |      490 |  65 |    125 |     100 |              60 |               70 |    70 | dark      | steel     |
| bouffalant                 |   626 | bouffalant   |     16 |    946 |              172 |      490 |  95 |    110 |      95 |              40 |               95 |    55 | normal    | None      |
| rufflet                    |   627 | rufflet      |      5 |    105 |               70 |      350 |  70 |     83 |      50 |              37 |               50 |    60 | normal    | flying    |
| braviary                   |   628 | braviary     |     15 |    410 |              179 |      510 | 100 |    123 |      75 |              57 |               75 |    80 | normal    | flying    |
| vullaby                    |   629 | vullaby      |      5 |     90 |               74 |      370 |  70 |     55 |      75 |              45 |               65 |    60 | dark      | flying    |
| mandibuzz                  |   630 | mandibuzz    |     12 |    395 |              179 |      510 | 110 |     65 |     105 |              55 |               95 |    80 | dark      | flying    |
| heatmor                    |   631 | heatmor      |     14 |    580 |              169 |      484 |  85 |     97 |      66 |             105 |               66 |    65 | fire      | None      |
| durant                     |   632 | durant       |      3 |    330 |              169 |      484 |  58 |    109 |     112 |              48 |               48 |   109 | bug       | steel     |
| deino                      |   633 | deino        |      8 |    173 |               60 |      300 |  52 |     65 |      50 |              45 |               50 |    38 | dark      | dragon    |
| zweilous                   |   634 | zweilous     |     14 |    500 |              147 |      420 |  72 |     85 |      70 |              65 |               70 |    58 | dark      | dragon    |
| hydreigon                  |   635 | hydreigon    |     18 |   1600 |              270 |      600 |  92 |    105 |      90 |             125 |               90 |    98 | dark      | dragon    |
| larvesta                   |   636 | larvesta     |     11 |    288 |               72 |      360 |  55 |     85 |      55 |              50 |               55 |    60 | bug       | fire      |
| volcarona                  |   637 | volcarona    |     16 |    460 |              248 |      550 |  85 |     60 |      65 |             135 |              105 |   100 | bug       | fire      |
| cobalion                   |   638 | cobalion     |     21 |   2500 |              261 |      580 |  91 |     90 |     129 |              90 |               72 |   108 | steel     | fighting  |
| terrakion                  |   639 | terrakion    |     19 |   2600 |              261 |      580 |  91 |    129 |      90 |              72 |               90 |   108 | rock      | fighting  |
| virizion                   |   640 | virizion     |     20 |   2000 |              261 |      580 |  91 |     90 |      72 |              90 |              129 |   108 | grass     | fighting  |
| tornadus-incarnate         |   641 | tornadus     |     15 |    630 |              261 |      580 |  79 |    115 |      70 |             125 |               80 |   111 | flying    | None      |
| thundurus-incarnate        |   642 | thundurus    |     15 |    610 |              261 |      580 |  79 |    115 |      70 |             125 |               80 |   111 | electric  | flying    |
| reshiram                   |   643 | reshiram     |     32 |   3300 |              306 |      680 | 100 |    120 |     100 |             150 |              120 |    90 | dragon    | fire      |
| zekrom                     |   644 | zekrom       |     29 |   3450 |              306 |      680 | 100 |    150 |     120 |             120 |              100 |    90 | dragon    | electric  |
| landorus-incarnate         |   645 | landorus     |     15 |    680 |              270 |      600 |  89 |    125 |      90 |             115 |               80 |   101 | ground    | flying    |
| kyurem                     |   646 | kyurem       |     30 |   3250 |              297 |      660 | 125 |    130 |      90 |             130 |               90 |    95 | dragon    | ice       |
| keldeo-ordinary            |   647 | keldeo       |     14 |    485 |              261 |      580 |  91 |     72 |      90 |             129 |               90 |   108 | water     | fighting  |
| meloetta-aria              |   648 | meloetta     |      6 |     65 |              270 |      600 | 100 |     77 |      77 |             128 |              128 |    90 | normal    | psychic   |
| genesect                   |   649 | genesect     |     15 |    825 |              270 |      600 |  71 |    120 |      95 |             120 |               95 |    99 | bug       | steel     |
| chespin                    |   650 | chespin      |      4 |     90 |               63 |      313 |  56 |     61 |      65 |              48 |               45 |    38 | grass     | None      |
| quilladin                  |   651 | quilladin    |      7 |    290 |              142 |      405 |  61 |     78 |      95 |              56 |               58 |    57 | grass     | None      |
| chesnaught                 |   652 | chesnaught   |     16 |    900 |              239 |      530 |  88 |    107 |     122 |              74 |               75 |    64 | grass     | fighting  |
| fennekin                   |   653 | fennekin     |      4 |     94 |               61 |      307 |  40 |     45 |      40 |              62 |               60 |    60 | fire      | None      |
| braixen                    |   654 | braixen      |     10 |    145 |              143 |      409 |  59 |     59 |      58 |              90 |               70 |    73 | fire      | None      |
| delphox                    |   655 | delphox      |     15 |    390 |              240 |      534 |  75 |     69 |      72 |             114 |              100 |   104 | fire      | psychic   |
| froakie                    |   656 | froakie      |      3 |     70 |               63 |      314 |  41 |     56 |      40 |              62 |               44 |    71 | water     | None      |
| frogadier                  |   657 | frogadier    |      6 |    109 |              142 |      405 |  54 |     63 |      52 |              83 |               56 |    97 | water     | None      |
| greninja                   |   658 | greninja     |     15 |    400 |              239 |      530 |  72 |     95 |      67 |             103 |               71 |   122 | water     | dark      |
| bunnelby                   |   659 | bunnelby     |      4 |     50 |               47 |      237 |  38 |     36 |      38 |              32 |               36 |    57 | normal    | None      |
| diggersby                  |   660 | diggersby    |     10 |    424 |              148 |      423 |  85 |     56 |      77 |              50 |               77 |    78 | normal    | ground    |
| fletchling                 |   661 | fletchling   |      3 |     17 |               56 |      278 |  45 |     50 |      43 |              40 |               38 |    62 | normal    | flying    |
| fletchinder                |   662 | fletchinder  |      7 |    160 |              134 |      382 |  62 |     73 |      55 |              56 |               52 |    84 | fire      | flying    |
| talonflame                 |   663 | talonflame   |     12 |    245 |              175 |      499 |  78 |     81 |      71 |              74 |               69 |   126 | fire      | flying    |
| scatterbug                 |   664 | scatterbug   |      3 |     25 |               40 |      200 |  38 |     35 |      40 |              27 |               25 |    35 | bug       | None      |
| spewpa                     |   665 | spewpa       |      3 |     84 |               75 |      213 |  45 |     22 |      60 |              27 |               30 |    29 | bug       | None      |
| vivillon                   |   666 | vivillon     |     12 |    170 |              185 |      411 |  80 |     52 |      50 |              90 |               50 |    89 | bug       | flying    |
| litleo                     |   667 | litleo       |      6 |    135 |               74 |      369 |  62 |     50 |      58 |              73 |               54 |    72 | fire      | normal    |
| pyroar                     |   668 | pyroar       |     15 |    815 |              177 |      507 |  86 |     68 |      72 |             109 |               66 |   106 | fire      | normal    |
| flabebe                    |   669 | flabebe      |      1 |      1 |               61 |      303 |  44 |     38 |      39 |              61 |               79 |    42 | fairy     | None      |
| floette                    |   670 | floette      |      2 |      9 |              130 |      371 |  54 |     45 |      47 |              75 |               98 |    52 | fairy     | None      |
| florges                    |   671 | florges      |     11 |    100 |              248 |      552 |  78 |     65 |      68 |             112 |              154 |    75 | fairy     | None      |
| skiddo                     |   672 | skiddo       |      9 |    310 |               70 |      350 |  66 |     65 |      48 |              62 |               57 |    52 | grass     | None      |
| gogoat                     |   673 | gogoat       |     17 |    910 |              186 |      531 | 123 |    100 |      62 |              97 |               81 |    68 | grass     | None      |
| pancham                    |   674 | pancham      |      6 |     80 |               70 |      348 |  67 |     82 |      62 |              46 |               48 |    43 | fighting  | None      |
| pangoro                    |   675 | pangoro      |     21 |   1360 |              173 |      495 |  95 |    124 |      78 |              69 |               71 |    58 | fighting  | dark      |
| furfrou                    |   676 | furfrou      |     12 |    280 |              165 |      472 |  75 |     80 |      60 |              65 |               90 |   102 | normal    | None      |
| espurr                     |   677 | espurr       |      3 |     35 |               71 |      355 |  62 |     48 |      54 |              63 |               60 |    68 | psychic   | None      |
| meowstic-male              |   678 | meowstic     |      6 |     85 |              163 |      466 |  74 |     48 |      76 |              83 |               81 |   104 | psychic   | None      |
| honedge                    |   679 | honedge      |      8 |     20 |               65 |      325 |  45 |     80 |     100 |              35 |               37 |    28 | steel     | ghost     |
| doublade                   |   680 | doublade     |      8 |     45 |              157 |      448 |  59 |    110 |     150 |              45 |               49 |    35 | steel     | ghost     |
| aegislash-shield           |   681 | aegislash    |     17 |    530 |              234 |      500 |  60 |     50 |     140 |              50 |              140 |    60 | steel     | ghost     |
| spritzee                   |   682 | spritzee     |      2 |      5 |               68 |      341 |  78 |     52 |      60 |              63 |               65 |    23 | fairy     | None      |
| aromatisse                 |   683 | aromatisse   |      8 |    155 |              162 |      462 | 101 |     72 |      72 |              99 |               89 |    29 | fairy     | None      |
| swirlix                    |   684 | swirlix      |      4 |     35 |               68 |      341 |  62 |     48 |      66 |              59 |               57 |    49 | fairy     | None      |
| slurpuff                   |   685 | slurpuff     |      8 |     50 |              168 |      480 |  82 |     80 |      86 |              85 |               75 |    72 | fairy     | None      |
| inkay                      |   686 | inkay        |      4 |     35 |               58 |      288 |  53 |     54 |      53 |              37 |               46 |    45 | dark      | psychic   |
| malamar                    |   687 | malamar      |     15 |    470 |              169 |      482 |  86 |     92 |      88 |              68 |               75 |    73 | dark      | psychic   |
| binacle                    |   688 | binacle      |      5 |    310 |               61 |      306 |  42 |     52 |      67 |              39 |               56 |    50 | rock      | water     |
| barbaracle                 |   689 | barbaracle   |     13 |    960 |              175 |      500 |  72 |    105 |     115 |              54 |               86 |    68 | rock      | water     |
| skrelp                     |   690 | skrelp       |      5 |     73 |               64 |      320 |  50 |     60 |      60 |              60 |               60 |    30 | poison    | water     |
| dragalge                   |   691 | dragalge     |     18 |    815 |              173 |      494 |  65 |     75 |      90 |              97 |              123 |    44 | poison    | dragon    |
| clauncher                  |   692 | clauncher    |      5 |     83 |               66 |      330 |  50 |     53 |      62 |              58 |               63 |    44 | water     | None      |
| clawitzer                  |   693 | clawitzer    |     13 |    353 |              100 |      500 |  71 |     73 |      88 |             120 |               89 |    59 | water     | None      |
| helioptile                 |   694 | helioptile   |      5 |     60 |               58 |      289 |  44 |     38 |      33 |              61 |               43 |    70 | electric  | normal    |
| heliolisk                  |   695 | heliolisk    |     10 |    210 |              168 |      481 |  62 |     55 |      52 |             109 |               94 |   109 | electric  | normal    |
| tyrunt                     |   696 | tyrunt       |      8 |    260 |               72 |      362 |  58 |     89 |      77 |              45 |               45 |    48 | rock      | dragon    |
| tyrantrum                  |   697 | tyrantrum    |     25 |   2700 |              182 |      521 |  82 |    121 |     119 |              69 |               59 |    71 | rock      | dragon    |
| amaura                     |   698 | amaura       |     13 |    252 |               72 |      362 |  77 |     59 |      50 |              67 |               63 |    46 | rock      | ice       |
| aurorus                    |   699 | aurorus      |     27 |   2250 |              104 |      521 | 123 |     77 |      72 |              99 |               92 |    58 | rock      | ice       |
| sylveon                    |   700 | sylveon      |     10 |    235 |              184 |      525 |  95 |     65 |      65 |             110 |              130 |    60 | fairy     | None      |
| hawlucha                   |   701 | hawlucha     |      8 |    215 |              175 |      500 |  78 |     92 |      75 |              74 |               63 |   118 | fighting  | flying    |
| dedenne                    |   702 | dedenne      |      2 |     22 |              151 |      431 |  67 |     58 |      57 |              81 |               67 |   101 | electric  | fairy     |
| carbink                    |   703 | carbink      |      3 |     57 |              100 |      500 |  50 |     50 |     150 |              50 |              150 |    50 | rock      | fairy     |
| goomy                      |   704 | goomy        |      3 |     28 |               60 |      300 |  45 |     50 |      35 |              55 |               75 |    40 | dragon    | None      |
| sliggoo                    |   705 | sliggoo      |      8 |    175 |              158 |      452 |  68 |     75 |      53 |              83 |              113 |    60 | dragon    | None      |
| goodra                     |   706 | goodra       |     20 |   1505 |              270 |      600 |  90 |    100 |      70 |             110 |              150 |    80 | dragon    | None      |
| klefki                     |   707 | klefki       |      2 |     30 |              165 |      470 |  57 |     80 |      91 |              80 |               87 |    75 | steel     | fairy     |
| phantump                   |   708 | phantump     |      4 |     70 |               62 |      309 |  43 |     70 |      48 |              50 |               60 |    38 | ghost     | grass     |
| trevenant                  |   709 | trevenant    |     15 |    710 |              166 |      474 |  85 |    110 |      76 |              65 |               82 |    56 | ghost     | grass     |
| pumpkaboo-average          |   710 | pumpkaboo    |      4 |     50 |               67 |      335 |  49 |     66 |      70 |              44 |               55 |    51 | ghost     | grass     |
| gourgeist-average          |   711 | gourgeist    |      9 |    125 |              173 |      494 |  65 |     90 |     122 |              58 |               75 |    84 | ghost     | grass     |
| bergmite                   |   712 | bergmite     |     10 |    995 |               61 |      304 |  55 |     69 |      85 |              32 |               35 |    28 | ice       | None      |
| avalugg                    |   713 | avalugg      |     20 |   5050 |              180 |      514 |  95 |    117 |     184 |              44 |               46 |    28 | ice       | None      |
| noibat                     |   714 | noibat       |      5 |     80 |               49 |      245 |  40 |     30 |      35 |              45 |               40 |    55 | flying    | dragon    |
| noivern                    |   715 | noivern      |     15 |    850 |              187 |      535 |  85 |     70 |      80 |              97 |               80 |   123 | flying    | dragon    |
| xerneas                    |   716 | xerneas      |     30 |   2150 |              306 |      680 | 126 |    131 |      95 |             131 |               98 |    99 | fairy     | None      |
| yveltal                    |   717 | yveltal      |     58 |   2030 |              306 |      680 | 126 |    131 |      95 |             131 |               98 |    99 | dark      | flying    |
| zygarde                    |   718 | zygarde      |     50 |   3050 |              270 |      600 | 108 |    100 |     121 |              81 |               95 |    95 | dragon    | ground    |
| diancie                    |   719 | diancie      |      7 |     88 |              270 |      600 |  50 |    100 |     150 |             100 |              150 |    50 | rock      | fairy     |
| hoopa                      |   720 | hoopa        |      5 |     90 |              270 |      600 |  80 |    110 |      60 |             150 |              130 |    70 | psychic   | ghost     |
| volcanion                  |   721 | volcanion    |     17 |   1950 |              270 |      600 |  80 |    110 |     120 |             130 |               90 |    70 | fire      | water     |
| rowlet                     |   722 | rowlet       |      3 |     15 |               64 |      320 |  68 |     55 |      55 |              50 |               50 |    42 | grass     | flying    |
| dartrix                    |   723 | dartrix      |      7 |    160 |              147 |      420 |  78 |     75 |      75 |              70 |               70 |    52 | grass     | flying    |
| decidueye                  |   724 | decidueye    |     16 |    366 |              239 |      530 |  78 |    107 |      75 |             100 |              100 |    70 | grass     | ghost     |
| litten                     |   725 | litten       |      4 |     43 |               64 |      320 |  45 |     65 |      40 |              60 |               40 |    70 | fire      | None      |
| torracat                   |   726 | torracat     |      7 |    250 |              147 |      420 |  65 |     85 |      50 |              80 |               50 |    90 | fire      | None      |
| incineroar                 |   727 | incineroar   |     18 |    830 |              239 |      530 |  95 |    115 |      90 |              80 |               90 |    60 | fire      | dark      |
| popplio                    |   728 | popplio      |      4 |     75 |               64 |      320 |  50 |     54 |      54 |              66 |               56 |    40 | water     | None      |
| brionne                    |   729 | brionne      |      6 |    175 |              147 |      420 |  60 |     69 |      69 |              91 |               81 |    50 | water     | None      |
| primarina                  |   730 | primarina    |     18 |    440 |              239 |      530 |  80 |     74 |      74 |             126 |              116 |    60 | water     | fairy     |
| pikipek                    |   731 | pikipek      |      3 |     12 |               53 |      265 |  35 |     75 |      30 |              30 |               30 |    65 | normal    | flying    |
| trumbeak                   |   732 | trumbeak     |      6 |    148 |              124 |      355 |  55 |     85 |      50 |              40 |               50 |    75 | normal    | flying    |
| toucannon                  |   733 | toucannon    |     11 |    260 |              218 |      485 |  80 |    120 |      75 |              75 |               75 |    60 | normal    | flying    |
| yungoos                    |   734 | yungoos      |      4 |     60 |               51 |      253 |  48 |     70 |      30 |              30 |               30 |    45 | normal    | None      |
| gumshoos                   |   735 | gumshoos     |      7 |    142 |              146 |      418 |  88 |    110 |      60 |              55 |               60 |    45 | normal    | None      |
| grubbin                    |   736 | grubbin      |      4 |     44 |               60 |      300 |  47 |     62 |      45 |              55 |               45 |    46 | bug       | None      |
| charjabug                  |   737 | charjabug    |      5 |    105 |              140 |      400 |  57 |     82 |      95 |              55 |               75 |    36 | bug       | electric  |
| vikavolt                   |   738 | vikavolt     |     15 |    450 |              225 |      500 |  77 |     70 |      90 |             145 |               75 |    43 | bug       | electric  |
| crabrawler                 |   739 | crabrawler   |      6 |     70 |               68 |      338 |  47 |     82 |      57 |              42 |               47 |    63 | fighting  | None      |
| crabominable               |   740 | crabominable |     17 |   1800 |              167 |      478 |  97 |    132 |      77 |              62 |               67 |    43 | fighting  | ice       |
| oricorio-baile             |   741 | oricorio     |      6 |     34 |              167 |      476 |  75 |     70 |      70 |              98 |               70 |    93 | fire      | flying    |
| cutiefly                   |   742 | cutiefly     |      1 |      2 |               61 |      304 |  40 |     45 |      40 |              55 |               40 |    84 | bug       | fairy     |
| ribombee                   |   743 | ribombee     |      2 |      5 |              162 |      464 |  60 |     55 |      60 |              95 |               70 |   124 | bug       | fairy     |
| rockruff                   |   744 | rockruff     |      5 |     92 |               56 |      280 |  45 |     65 |      40 |              30 |               40 |    60 | rock      | None      |
| lycanroc-midday            |   745 | lycanroc     |      8 |    250 |              170 |      487 |  75 |    115 |      65 |              55 |               65 |   112 | rock      | None      |
| wishiwashi-solo            |   746 | wishiwashi   |      2 |      3 |               61 |      175 |  45 |     20 |      20 |              25 |               25 |    40 | water     | None      |
| mareanie                   |   747 | mareanie     |      4 |     80 |               61 |      305 |  50 |     53 |      62 |              43 |               52 |    45 | poison    | water     |
| toxapex                    |   748 | toxapex      |      7 |    145 |              173 |      495 |  50 |     63 |     152 |              53 |              142 |    35 | poison    | water     |
| mudbray                    |   749 | mudbray      |     10 |   1100 |               77 |      385 |  70 |    100 |      70 |              45 |               55 |    45 | ground    | None      |
| mudsdale                   |   750 | mudsdale     |     25 |   9200 |              175 |      500 | 100 |    125 |     100 |              55 |               85 |    35 | ground    | None      |
| dewpider                   |   751 | dewpider     |      3 |     40 |               54 |      269 |  38 |     40 |      52 |              40 |               72 |    27 | water     | bug       |
| araquanid                  |   752 | araquanid    |     18 |    820 |              159 |      454 |  68 |     70 |      92 |              50 |              132 |    42 | water     | bug       |
| fomantis                   |   753 | fomantis     |      3 |     15 |               50 |      250 |  40 |     55 |      35 |              50 |               35 |    35 | grass     | None      |
| lurantis                   |   754 | lurantis     |      9 |    185 |              168 |      480 |  70 |    105 |      90 |              80 |               90 |    45 | grass     | None      |
| morelull                   |   755 | morelull     |      2 |     15 |               57 |      285 |  40 |     35 |      55 |              65 |               75 |    15 | grass     | fairy     |
| shiinotic                  |   756 | shiinotic    |     10 |    115 |              142 |      405 |  60 |     45 |      80 |              90 |              100 |    30 | grass     | fairy     |
| salandit                   |   757 | salandit     |      6 |     48 |               64 |      320 |  48 |     44 |      40 |              71 |               40 |    77 | poison    | fire      |
| salazzle                   |   758 | salazzle     |     12 |    222 |              168 |      480 |  68 |     64 |      60 |             111 |               60 |   117 | poison    | fire      |
| stufful                    |   759 | stufful      |      5 |     68 |               68 |      340 |  70 |     75 |      50 |              45 |               50 |    50 | normal    | fighting  |
| bewear                     |   760 | bewear       |     21 |   1350 |              175 |      500 | 120 |    125 |      80 |              55 |               60 |    60 | normal    | fighting  |
| bounsweet                  |   761 | bounsweet    |      3 |     32 |               42 |      210 |  42 |     30 |      38 |              30 |               38 |    32 | grass     | None      |
| steenee                    |   762 | steenee      |      7 |     82 |              102 |      290 |  52 |     40 |      48 |              40 |               48 |    62 | grass     | None      |
| tsareena                   |   763 | tsareena     |     12 |    214 |              230 |      510 |  72 |    120 |      98 |              50 |               98 |    72 | grass     | None      |
| comfey                     |   764 | comfey       |      1 |      3 |              170 |      485 |  51 |     52 |      90 |              82 |              110 |   100 | fairy     | None      |
| oranguru                   |   765 | oranguru     |     15 |    760 |              172 |      490 |  90 |     60 |      80 |              90 |              110 |    60 | normal    | psychic   |
| passimian                  |   766 | passimian    |     20 |    828 |              172 |      490 | 100 |    120 |      90 |              40 |               60 |    80 | fighting  | None      |
| wimpod                     |   767 | wimpod       |      5 |    120 |               46 |      230 |  25 |     35 |      40 |              20 |               30 |    80 | bug       | water     |
| golisopod                  |   768 | golisopod    |     20 |   1080 |              186 |      530 |  75 |    125 |     140 |              60 |               90 |    40 | bug       | water     |
| sandygast                  |   769 | sandygast    |      5 |    700 |               64 |      320 |  55 |     55 |      80 |              70 |               45 |    15 | ghost     | ground    |
| palossand                  |   770 | palossand    |     13 |   2500 |              168 |      480 |  85 |     75 |     110 |             100 |               75 |    35 | ghost     | ground    |
| pyukumuku                  |   771 | pyukumuku    |      3 |     12 |              144 |      410 |  55 |     60 |     130 |              30 |              130 |     5 | water     | None      |
| type-null                  |   772 | type-null    |     19 |   1205 |              107 |      534 |  95 |     95 |      95 |              95 |               95 |    59 | normal    | None      |
| silvally                   |   773 | silvally     |     23 |   1005 |              257 |      570 |  95 |     95 |      95 |              95 |               95 |    95 | normal    | None      |
| minior-red-meteor          |   774 | minior       |      3 |    400 |              154 |      440 |  60 |     60 |     100 |              60 |              100 |    60 | rock      | flying    |
| komala                     |   775 | komala       |      4 |    199 |              168 |      480 |  65 |    115 |      65 |              75 |               95 |    65 | normal    | None      |
| turtonator                 |   776 | turtonator   |     20 |   2120 |              170 |      485 |  60 |     78 |     135 |              91 |               85 |    36 | fire      | dragon    |
| togedemaru                 |   777 | togedemaru   |      3 |     33 |              152 |      435 |  65 |     98 |      63 |              40 |               73 |    96 | electric  | steel     |
| mimikyu-disguised          |   778 | mimikyu      |      2 |      7 |              167 |      476 |  55 |     90 |      80 |              50 |              105 |    96 | ghost     | fairy     |
| bruxish                    |   779 | bruxish      |      9 |    190 |              166 |      475 |  68 |    105 |      70 |              70 |               70 |    92 | water     | psychic   |
| drampa                     |   780 | drampa       |     30 |   1850 |              170 |      485 |  78 |     60 |      85 |             135 |               91 |    36 | normal    | dragon    |
| dhelmise                   |   781 | dhelmise     |     39 |   2100 |              181 |      517 |  70 |    131 |     100 |              86 |               90 |    40 | ghost     | grass     |
| jangmo-o                   |   782 | jangmo-o     |      6 |    297 |               60 |      300 |  45 |     55 |      65 |              45 |               45 |    45 | dragon    | None      |
| hakamo-o                   |   783 | hakamo-o     |     12 |    470 |              147 |      420 |  55 |     75 |      90 |              65 |               70 |    65 | dragon    | fighting  |
| kommo-o                    |   784 | kommo-o      |     16 |    782 |              270 |      600 |  75 |    110 |     125 |             100 |              105 |    85 | dragon    | fighting  |
| tapu-koko                  |   785 | tapu-koko    |     18 |    205 |              257 |      570 |  70 |    115 |      85 |              95 |               75 |   130 | electric  | fairy     |
| tapu-lele                  |   786 | tapu-lele    |     12 |    186 |              257 |      570 |  70 |     85 |      75 |             130 |              115 |    95 | psychic   | fairy     |
| tapu-bulu                  |   787 | tapu-bulu    |     19 |    455 |              257 |      570 |  70 |    130 |     115 |              85 |               95 |    75 | grass     | fairy     |
| tapu-fini                  |   788 | tapu-fini    |     13 |    212 |              257 |      570 |  70 |     75 |     115 |              95 |              130 |    85 | water     | fairy     |
| cosmog                     |   789 | cosmog       |      2 |      1 |               40 |      200 |  43 |     29 |      31 |              29 |               31 |    37 | psychic   | None      |
| cosmoem                    |   790 | cosmoem      |      1 |   9999 |              140 |      400 |  43 |     29 |     131 |              29 |              131 |    37 | psychic   | None      |
| solgaleo                   |   791 | solgaleo     |     34 |   2300 |              306 |      680 | 137 |    137 |     107 |             113 |               89 |    97 | psychic   | steel     |
| lunala                     |   792 | lunala       |     40 |   1200 |              306 |      680 | 137 |    113 |      89 |             137 |              107 |    97 | psychic   | ghost     |
| nihilego                   |   793 | nihilego     |     12 |    555 |              257 |      570 | 109 |     53 |      47 |             127 |              131 |   103 | rock      | poison    |
| buzzwole                   |   794 | buzzwole     |     24 |   3336 |              257 |      570 | 107 |    139 |     139 |              53 |               53 |    79 | bug       | fighting  |
| pheromosa                  |   795 | pheromosa    |     18 |    250 |              257 |      570 |  71 |    137 |      37 |             137 |               37 |   151 | bug       | fighting  |
| xurkitree                  |   796 | xurkitree    |     38 |   1000 |              257 |      570 |  83 |     89 |      71 |             173 |               71 |    83 | electric  | None      |
| celesteela                 |   797 | celesteela   |     92 |   9999 |              257 |      570 |  97 |    101 |     103 |             107 |              101 |    61 | steel     | flying    |
| kartana                    |   798 | kartana      |      3 |      1 |              257 |      570 |  59 |    181 |     131 |              59 |               31 |   109 | grass     | steel     |
| guzzlord                   |   799 | guzzlord     |     55 |   8880 |              257 |      570 | 223 |    101 |      53 |              97 |               53 |    43 | dark      | dragon    |
| necrozma                   |   800 | necrozma     |     24 |   2300 |              270 |      600 |  97 |    107 |     101 |             127 |               89 |    79 | psychic   | None      |
| magearna                   |   801 | magearna     |     10 |    805 |              270 |      600 |  80 |     95 |     115 |             130 |              115 |    65 | steel     | fairy     |
| marshadow                  |   802 | marshadow    |      7 |    222 |              270 |      600 |  90 |    125 |      80 |              90 |               90 |   125 | fighting  | ghost     |
| poipole                    |   803 | poipole      |      6 |     18 |              189 |      420 |  67 |     73 |      67 |              73 |               67 |    73 | poison    | None      |
| naganadel                  |   804 | naganadel    |     36 |   1500 |              243 |      540 |  73 |     73 |      73 |             127 |               73 |   121 | poison    | dragon    |
| stakataka                  |   805 | stakataka    |     55 |   8200 |              257 |      570 |  61 |    131 |     211 |              53 |              101 |    13 | rock      | steel     |
| blacephalon                |   806 | blacephalon  |     18 |    130 |              257 |      570 |  53 |    127 |      53 |             151 |               79 |   107 | fire      | ghost     |
| zeraora                    |   807 | zeraora      |     15 |    445 |              270 |      600 |  88 |    112 |      75 |             102 |               80 |   143 | electric  | None      |
| meltan                     |   808 | meltan       |      2 |     80 |              135 |      300 |  46 |     65 |      65 |              55 |               35 |    34 | steel     | None      |
| melmetal                   |   809 | melmetal     |     25 |   8000 |              270 |      600 | 135 |    143 |     143 |              80 |               65 |    34 | steel     | None      |
| grookey                    |   810 | grookey      |      3 |     50 |               62 |      310 |  50 |     65 |      50 |              40 |               40 |    65 | grass     | None      |
| thwackey                   |   811 | thwackey     |      7 |    140 |              147 |      420 |  70 |     85 |      70 |              55 |               60 |    80 | grass     | None      |
| rillaboom                  |   812 | rillaboom    |     21 |    900 |              265 |      530 | 100 |    125 |      90 |              60 |               70 |    85 | grass     | None      |
| scorbunny                  |   813 | scorbunny    |      3 |     45 |               62 |      310 |  50 |     71 |      40 |              40 |               40 |    69 | fire      | None      |
| raboot                     |   814 | raboot       |      6 |     90 |              147 |      420 |  65 |     86 |      60 |              55 |               60 |    94 | fire      | None      |
| cinderace                  |   815 | cinderace    |     14 |    330 |              265 |      530 |  80 |    116 |      75 |              65 |               75 |   119 | fire      | None      |
| sobble                     |   816 | sobble       |      3 |     40 |               62 |      310 |  50 |     40 |      40 |              70 |               40 |    70 | water     | None      |
| drizzile                   |   817 | drizzile     |      7 |    115 |              147 |      420 |  65 |     60 |      55 |              95 |               55 |    90 | water     | None      |
| inteleon                   |   818 | inteleon     |     19 |    452 |              265 |      530 |  70 |     85 |      65 |             125 |               65 |   120 | water     | None      |
| skwovet                    |   819 | skwovet      |      3 |     25 |               55 |      275 |  70 |     55 |      55 |              35 |               35 |    25 | normal    | None      |
| greedent                   |   820 | greedent     |      6 |     60 |              161 |      460 | 120 |     95 |      95 |              55 |               75 |    20 | normal    | None      |
| rookidee                   |   821 | rookidee     |      2 |     18 |               49 |      245 |  38 |     47 |      35 |              33 |               35 |    57 | flying    | None      |
| corvisquire                |   822 | corvisquire  |      8 |    160 |              128 |      365 |  68 |     67 |      55 |              43 |               55 |    77 | flying    | None      |
| corviknight                |   823 | corviknight  |     22 |    750 |              248 |      495 |  98 |     87 |     105 |              53 |               85 |    67 | flying    | steel     |
| blipbug                    |   824 | blipbug      |      4 |     80 |               36 |      180 |  25 |     20 |      20 |              25 |               45 |    45 | bug       | None      |
| dottler                    |   825 | dottler      |      4 |    195 |              117 |      335 |  50 |     35 |      80 |              50 |               90 |    30 | bug       | psychic   |
| orbeetle                   |   826 | orbeetle     |      4 |    408 |              253 |      505 |  60 |     45 |     110 |              80 |              120 |    90 | bug       | psychic   |
| nickit                     |   827 | nickit       |      6 |     89 |               49 |      245 |  40 |     28 |      28 |              47 |               52 |    50 | dark      | None      |
| thievul                    |   828 | thievul      |     12 |    199 |              159 |      455 |  70 |     58 |      58 |              87 |               92 |    90 | dark      | None      |
| gossifleur                 |   829 | gossifleur   |      4 |     22 |               50 |      250 |  40 |     40 |      60 |              40 |               60 |    10 | grass     | None      |
| eldegoss                   |   830 | eldegoss     |      5 |     25 |              161 |      460 |  60 |     50 |      90 |              80 |              120 |    60 | grass     | None      |
| wooloo                     |   831 | wooloo       |      6 |     60 |              122 |      270 |  42 |     40 |      55 |              40 |               45 |    48 | normal    | None      |
| dubwool                    |   832 | dubwool      |     13 |    430 |              172 |      490 |  72 |     80 |     100 |              60 |               90 |    88 | normal    | None      |
| chewtle                    |   833 | chewtle      |      3 |     85 |               57 |      284 |  50 |     64 |      50 |              38 |               38 |    44 | water     | None      |
| drednaw                    |   834 | drednaw      |     10 |   1155 |              170 |      485 |  90 |    115 |      90 |              48 |               68 |    74 | water     | rock      |
| yamper                     |   835 | yamper       |      3 |    135 |               54 |      270 |  59 |     45 |      50 |              40 |               50 |    26 | electric  | None      |
| boltund                    |   836 | boltund      |     10 |    340 |              172 |      490 |  69 |     90 |      60 |              90 |               60 |   121 | electric  | None      |
| rolycoly                   |   837 | rolycoly     |      3 |    120 |               48 |      240 |  30 |     40 |      50 |              40 |               50 |    30 | rock      | None      |
| carkol                     |   838 | carkol       |     11 |    780 |              144 |      410 |  80 |     60 |      90 |              60 |               70 |    50 | rock      | fire      |
| coalossal                  |   839 | coalossal    |     28 |   3105 |              255 |      510 | 110 |     80 |     120 |              80 |               90 |    30 | rock      | fire      |
| applin                     |   840 | applin       |      2 |      5 |               52 |      260 |  40 |     40 |      80 |              40 |               40 |    20 | grass     | dragon    |
| flapple                    |   841 | flapple      |      3 |     10 |              170 |      485 |  70 |    110 |      80 |              95 |               60 |    70 | grass     | dragon    |
| appletun                   |   842 | appletun     |      4 |    130 |              170 |      485 | 110 |     85 |      80 |             100 |               80 |    30 | grass     | dragon    |
| silicobra                  |   843 | silicobra    |     22 |     76 |               63 |      315 |  52 |     57 |      75 |              35 |               50 |    46 | ground    | None      |
| sandaconda                 |   844 | sandaconda   |     38 |    655 |              179 |      510 |  72 |    107 |     125 |              65 |               70 |    71 | ground    | None      |
| cramorant                  |   845 | cramorant    |      8 |    180 |              166 |      475 |  70 |     85 |      55 |              85 |               95 |    85 | flying    | water     |
| arrokuda                   |   846 | arrokuda     |      5 |     10 |               56 |      280 |  41 |     63 |      40 |              40 |               30 |    66 | water     | None      |
| barraskewda                |   847 | barraskewda  |     13 |    300 |              172 |      490 |  61 |    123 |      60 |              60 |               50 |   136 | water     | None      |
| toxel                      |   848 | toxel        |      4 |    110 |               48 |      242 |  40 |     38 |      35 |              54 |               35 |    40 | electric  | poison    |
| toxtricity-amped           |   849 | toxtricity   |     16 |    400 |              176 |      502 |  75 |     98 |      70 |             114 |               70 |    75 | electric  | poison    |
| sizzlipede                 |   850 | sizzlipede   |      7 |     10 |               61 |      305 |  50 |     65 |      45 |              50 |               50 |    45 | fire      | bug       |
| centiskorch                |   851 | centiskorch  |     30 |   1200 |              184 |      525 | 100 |    115 |      65 |              90 |               90 |    65 | fire      | bug       |
| clobbopus                  |   852 | clobbopus    |      6 |     40 |               62 |      310 |  50 |     68 |      60 |              50 |               50 |    32 | fighting  | None      |
| grapploct                  |   853 | grapploct    |     16 |    390 |              168 |      480 |  80 |    118 |      90 |              70 |               80 |    42 | fighting  | None      |
| sinistea                   |   854 | sinistea     |      1 |      2 |               62 |      308 |  40 |     45 |      45 |              74 |               54 |    50 | ghost     | None      |
| polteageist                |   855 | polteageist  |      2 |      4 |              178 |      508 |  60 |     65 |      65 |             134 |              114 |    70 | ghost     | None      |
| hatenna                    |   856 | hatenna      |      4 |     34 |               53 |      265 |  42 |     30 |      45 |              56 |               53 |    39 | psychic   | None      |
| hattrem                    |   857 | hattrem      |      6 |     48 |              130 |      370 |  57 |     40 |      65 |              86 |               73 |    49 | psychic   | None      |
| hatterene                  |   858 | hatterene    |     21 |     51 |              255 |      510 |  57 |     90 |      95 |             136 |              103 |    29 | psychic   | fairy     |
| impidimp                   |   859 | impidimp     |      4 |     55 |               53 |      265 |  45 |     45 |      30 |              55 |               40 |    50 | dark      | fairy     |
| morgrem                    |   860 | morgrem      |      8 |    125 |              130 |      370 |  65 |     60 |      45 |              75 |               55 |    70 | dark      | fairy     |
| grimmsnarl                 |   861 | grimmsnarl   |     15 |    610 |              255 |      510 |  95 |    120 |      65 |              95 |               75 |    60 | dark      | fairy     |
| obstagoon                  |   862 | obstagoon    |     16 |    460 |              260 |      520 |  93 |     90 |     101 |              60 |               81 |    95 | dark      | normal    |
| perrserker                 |   863 | perrserker   |      8 |    280 |              154 |      440 |  70 |    110 |     100 |              50 |               60 |    50 | steel     | None      |
| cursola                    |   864 | cursola      |     10 |      4 |              179 |      510 |  60 |     95 |      50 |             145 |              130 |    30 | ghost     | None      |
| sirfetchd                  |   865 | sirfetchd    |      8 |   1170 |              177 |      507 |  62 |    135 |      95 |              68 |               82 |    65 | fighting  | None      |
| mr-rime                    |   866 | mr-rime      |     15 |    582 |              182 |      520 |  80 |     85 |      75 |             110 |              100 |    70 | ice       | psychic   |
| runerigus                  |   867 | runerigus    |     16 |    666 |              169 |      483 |  58 |     95 |     145 |              50 |              105 |    30 | ground    | ghost     |
| milcery                    |   868 | milcery      |      2 |      3 |               54 |      270 |  45 |     40 |      40 |              50 |               61 |    34 | fairy     | None      |
| alcremie                   |   869 | alcremie     |      3 |      5 |              173 |      495 |  65 |     60 |      75 |             110 |              121 |    64 | fairy     | None      |
| falinks                    |   870 | falinks      |     30 |    620 |              165 |      470 |  65 |    100 |     100 |              70 |               60 |    75 | fighting  | None      |
| pincurchin                 |   871 | pincurchin   |      3 |     10 |              152 |      435 |  48 |    101 |      95 |              91 |               85 |    15 | electric  | None      |
| snom                       |   872 | snom         |      3 |     38 |               37 |      185 |  30 |     25 |      35 |              45 |               30 |    20 | ice       | bug       |
| frosmoth                   |   873 | frosmoth     |     13 |    420 |              168 |      475 |  70 |     65 |      60 |             125 |               90 |    65 | ice       | bug       |
| stonjourner                |   874 | stonjourner  |     25 |   5200 |              165 |      470 | 100 |    125 |     135 |              20 |               20 |    70 | rock      | None      |
| eiscue-ice                 |   875 | eiscue       |     14 |    890 |              165 |      470 |  75 |     80 |     110 |              65 |               90 |    50 | ice       | None      |
| indeedee-male              |   876 | indeedee     |      9 |    280 |              166 |      475 |  60 |     65 |      55 |             105 |               95 |    95 | psychic   | normal    |
| morpeko                    |   877 | morpeko      |      3 |     30 |              153 |      436 |  58 |     95 |      58 |              70 |               58 |    97 | electric  | dark      |
| cufant                     |   878 | cufant       |     12 |   1000 |               66 |      330 |  72 |     80 |      49 |              40 |               49 |    40 | steel     | None      |
| copperajah                 |   879 | copperajah   |     30 |   6500 |              175 |      500 | 122 |    130 |      69 |              80 |               69 |    30 | steel     | None      |
| dracozolt                  |   880 | dracozolt    |     18 |   1900 |              177 |      505 |  90 |    100 |      90 |              80 |               70 |    75 | electric  | dragon    |
| arctozolt                  |   881 | arctozolt    |     23 |   1500 |              177 |      505 |  90 |    100 |      90 |              90 |               80 |    55 | electric  | ice       |
| dracovish                  |   882 | dracovish    |     23 |   2150 |              177 |      505 |  90 |     90 |     100 |              70 |               80 |    75 | water     | dragon    |
| arctovish                  |   883 | arctovish    |     20 |   1750 |              177 |      505 |  90 |     90 |     100 |              80 |               90 |    55 | water     | ice       |
| duraludon                  |   884 | duraludon    |     18 |    400 |              187 |      535 |  70 |     95 |     115 |             120 |               50 |    85 | steel     | dragon    |
| dreepy                     |   885 | dreepy       |      5 |     20 |               54 |      270 |  28 |     60 |      30 |              40 |               30 |    82 | dragon    | ghost     |
| drakloak                   |   886 | drakloak     |     14 |    110 |              144 |      410 |  68 |     80 |      50 |              60 |               50 |   102 | dragon    | ghost     |
| dragapult                  |   887 | dragapult    |     30 |    500 |              300 |      600 |  88 |    120 |      75 |             100 |               75 |   142 | dragon    | ghost     |
| zacian-hero                |   888 | zacian       |     28 |   1100 |              335 |      670 |  92 |    130 |     115 |              80 |              115 |   138 | fairy     | None      |
| zamazenta-hero             |   889 | zamazenta    |     29 |   2100 |              335 |      670 |  92 |    130 |     115 |              80 |              115 |   138 | fighting  | None      |
| eternatus                  |   890 | eternatus    |    200 |   9500 |              345 |      690 | 140 |     85 |      95 |             145 |               95 |   130 | poison    | dragon    |
| kubfu                      |   891 | kubfu        |      6 |    120 |               77 |      385 |  60 |     90 |      60 |              53 |               50 |    72 | fighting  | None      |
| urshifu-single-strike      |   892 | urshifu      |     19 |   1050 |              275 |      550 | 100 |    130 |     100 |              63 |               60 |    97 | fighting  | dark      |
| zarude                     |   893 | zarude       |     18 |    700 |              300 |      600 | 105 |    120 |     105 |              70 |               95 |   105 | dark      | grass     |
| regieleki                  |   894 | regieleki    |     12 |   1450 |              290 |      580 |  80 |    100 |      50 |             100 |               50 |   200 | electric  | None      |
| regidrago                  |   895 | regidrago    |     21 |   2000 |              290 |      580 | 200 |    100 |      50 |             100 |               50 |    80 | dragon    | None      |
| glastrier                  |   896 | glastrier    |     22 |   8000 |              290 |      580 | 100 |    145 |     130 |              65 |              110 |    30 | ice       | None      |
| spectrier                  |   897 | spectrier    |     20 |    445 |              290 |      580 | 100 |     65 |      60 |             145 |               80 |   130 | ghost     | None      |
| calyrex                    |   898 | calyrex      |     11 |     77 |              250 |      500 | 100 |     80 |      80 |              80 |               80 |    80 | psychic   | grass     |
| deoxys-attack              | 10001 | deoxys       |     17 |    608 |              270 |      600 |  50 |    180 |      20 |             180 |               20 |   150 | psychic   | None      |
| deoxys-defense             | 10002 | deoxys       |     17 |    608 |              270 |      600 |  50 |     70 |     160 |              70 |              160 |    90 | psychic   | None      |
| deoxys-speed               | 10003 | deoxys       |     17 |    608 |              270 |      600 |  50 |     95 |      90 |              95 |               90 |   180 | psychic   | None      |
| wormadam-sandy             | 10004 | wormadam     |      5 |     65 |              148 |      424 |  60 |     79 |     105 |              59 |               85 |    36 | bug       | ground    |
| wormadam-trash             | 10005 | wormadam     |      5 |     65 |              148 |      424 |  60 |     69 |      95 |              69 |               95 |    36 | bug       | steel     |
| shaymin-sky                | 10006 | shaymin      |      4 |     52 |              270 |      600 | 100 |    103 |      75 |             120 |               75 |   127 | grass     | flying    |
| giratina-origin            | 10007 | giratina     |     69 |   6500 |              306 |      680 | 150 |    120 |     100 |             120 |              100 |    90 | ghost     | dragon    |
| rotom-heat                 | 10008 | rotom        |      3 |      3 |              182 |      520 |  50 |     65 |     107 |             105 |              107 |    86 | electric  | fire      |
| rotom-wash                 | 10009 | rotom        |      3 |      3 |              182 |      520 |  50 |     65 |     107 |             105 |              107 |    86 | electric  | water     |
| rotom-frost                | 10010 | rotom        |      3 |      3 |              182 |      520 |  50 |     65 |     107 |             105 |              107 |    86 | electric  | ice       |
| rotom-fan                  | 10011 | rotom        |      3 |      3 |              182 |      520 |  50 |     65 |     107 |             105 |              107 |    86 | electric  | flying    |
| rotom-mow                  | 10012 | rotom        |      3 |      3 |              182 |      520 |  50 |     65 |     107 |             105 |              107 |    86 | electric  | grass     |
| castform-sunny             | 10013 | castform     |      3 |      8 |              147 |      420 |  70 |     70 |      70 |              70 |               70 |    70 | fire      | None      |
| castform-rainy             | 10014 | castform     |      3 |      8 |              147 |      420 |  70 |     70 |      70 |              70 |               70 |    70 | water     | None      |
| castform-snowy             | 10015 | castform     |      3 |      8 |              147 |      420 |  70 |     70 |      70 |              70 |               70 |    70 | ice       | None      |
| basculin-blue-striped      | 10016 | basculin     |     10 |    180 |              161 |      460 |  70 |     92 |      65 |              80 |               55 |    98 | water     | None      |
| darmanitan-zen             | 10017 | darmanitan   |     13 |    929 |              189 |      540 | 105 |     30 |     105 |             140 |              105 |    55 | fire      | psychic   |
| meloetta-pirouette         | 10018 | meloetta     |      6 |     65 |              270 |      600 | 100 |    128 |      90 |              77 |               77 |   128 | normal    | fighting  |
| tornadus-therian           | 10019 | tornadus     |     14 |    630 |              261 |      580 |  79 |    100 |      80 |             110 |               90 |   121 | flying    | None      |
| thundurus-therian          | 10020 | thundurus    |     30 |    610 |              261 |      580 |  79 |    105 |      70 |             145 |               80 |   101 | electric  | flying    |
| landorus-therian           | 10021 | landorus     |     13 |    680 |              270 |      600 |  89 |    145 |      90 |             105 |               80 |    91 | ground    | flying    |
| kyurem-black               | 10022 | kyurem       |     33 |   3250 |              315 |      700 | 125 |    170 |     100 |             120 |               90 |    95 | dragon    | ice       |
| kyurem-white               | 10023 | kyurem       |     36 |   3250 |              315 |      700 | 125 |    120 |      90 |             170 |              100 |    95 | dragon    | ice       |
| keldeo-resolute            | 10024 | keldeo       |     14 |    485 |              261 |      580 |  91 |     72 |      90 |             129 |               90 |   108 | water     | fighting  |
| meowstic-female            | 10025 | meowstic     |      6 |     85 |              163 |      466 |  74 |     48 |      76 |              83 |               81 |   104 | psychic   | None      |
| aegislash-blade            | 10026 | aegislash    |     17 |    530 |              234 |      500 |  60 |    140 |      50 |             140 |               50 |    60 | steel     | ghost     |
| pumpkaboo-small            | 10027 | pumpkaboo    |      3 |     35 |               67 |      335 |  44 |     66 |      70 |              44 |               55 |    56 | ghost     | grass     |
| pumpkaboo-large            | 10028 | pumpkaboo    |      5 |     75 |               67 |      335 |  54 |     66 |      70 |              44 |               55 |    46 | ghost     | grass     |
| pumpkaboo-super            | 10029 | pumpkaboo    |      8 |    150 |               67 |      335 |  59 |     66 |      70 |              44 |               55 |    41 | ghost     | grass     |
| gourgeist-small            | 10030 | gourgeist    |      7 |     95 |              173 |      494 |  55 |     85 |     122 |              58 |               75 |    99 | ghost     | grass     |
| gourgeist-large            | 10031 | gourgeist    |     11 |    140 |              173 |      494 |  75 |     95 |     122 |              58 |               75 |    69 | ghost     | grass     |
| gourgeist-super            | 10032 | gourgeist    |     17 |    390 |              173 |      494 |  85 |    100 |     122 |              58 |               75 |    54 | ghost     | grass     |
| venusaur-mega              | 10033 | venusaur     |     24 |   1555 |              281 |      625 |  80 |    100 |     123 |             122 |              120 |    80 | grass     | poison    |
| charizard-mega-x           | 10034 | charizard    |     17 |   1105 |              285 |      634 |  78 |    130 |     111 |             130 |               85 |   100 | fire      | dragon    |
| charizard-mega-y           | 10035 | charizard    |     17 |   1005 |              285 |      634 |  78 |    104 |      78 |             159 |              115 |   100 | fire      | flying    |
| blastoise-mega             | 10036 | blastoise    |     16 |   1011 |              284 |      630 |  79 |    103 |     120 |             135 |              115 |    78 | water     | None      |
| alakazam-mega              | 10037 | alakazam     |     12 |    480 |              270 |      600 |  55 |     50 |      65 |             175 |              105 |   150 | psychic   | None      |
| gengar-mega                | 10038 | gengar       |     14 |    405 |              270 |      600 |  60 |     65 |      80 |             170 |               95 |   130 | ghost     | poison    |
| kangaskhan-mega            | 10039 | kangaskhan   |     22 |   1000 |              207 |      590 | 105 |    125 |     100 |              60 |              100 |   100 | normal    | None      |
| pinsir-mega                | 10040 | pinsir       |     17 |    590 |              210 |      600 |  65 |    155 |     120 |              65 |               90 |   105 | bug       | flying    |
| gyarados-mega              | 10041 | gyarados     |     65 |   3050 |              224 |      640 |  95 |    155 |     109 |              70 |              130 |    81 | water     | dark      |
| aerodactyl-mega            | 10042 | aerodactyl   |     21 |    790 |              215 |      615 |  80 |    135 |      85 |              70 |               95 |   150 | rock      | flying    |
| mewtwo-mega-x              | 10043 | mewtwo       |     23 |   1270 |              351 |      780 | 106 |    190 |     100 |             154 |              100 |   130 | psychic   | fighting  |
| mewtwo-mega-y              | 10044 | mewtwo       |     15 |    330 |              351 |      780 | 106 |    150 |      70 |             194 |              120 |   140 | psychic   | None      |
| ampharos-mega              | 10045 | ampharos     |     14 |    615 |              275 |      610 |  90 |     95 |     105 |             165 |              110 |    45 | electric  | dragon    |
| scizor-mega                | 10046 | scizor       |     20 |   1250 |              210 |      600 |  70 |    150 |     140 |              65 |              100 |    75 | bug       | steel     |
| heracross-mega             | 10047 | heracross    |     17 |    625 |              210 |      600 |  80 |    185 |     115 |              40 |              105 |    75 | bug       | fighting  |
| houndoom-mega              | 10048 | houndoom     |     19 |    495 |              210 |      600 |  75 |     90 |      90 |             140 |               90 |   115 | dark      | fire      |
| tyranitar-mega             | 10049 | tyranitar    |     25 |   2550 |              315 |      700 | 100 |    164 |     150 |              95 |              120 |    71 | rock      | dark      |
| blaziken-mega              | 10050 | blaziken     |     19 |    520 |              284 |      630 |  80 |    160 |      80 |             130 |               80 |   100 | fire      | fighting  |
| gardevoir-mega             | 10051 | gardevoir    |     16 |    484 |              278 |      618 |  68 |     85 |      65 |             165 |              135 |   100 | psychic   | fairy     |
| mawile-mega                | 10052 | mawile       |     10 |    235 |              168 |      480 |  50 |    105 |     125 |              55 |               95 |    50 | steel     | fairy     |
| aggron-mega                | 10053 | aggron       |     22 |   3950 |              284 |      630 |  70 |    140 |     230 |              60 |               80 |    50 | steel     | None      |
| medicham-mega              | 10054 | medicham     |     13 |    315 |              179 |      510 |  60 |    100 |      85 |              80 |               85 |   100 | fighting  | psychic   |
| manectric-mega             | 10055 | manectric    |     18 |    440 |              201 |      575 |  70 |     75 |      80 |             135 |               80 |   135 | electric  | None      |
| banette-mega               | 10056 | banette      |     12 |    130 |              194 |      555 |  64 |    165 |      75 |              93 |               83 |    75 | ghost     | None      |
| absol-mega                 | 10057 | absol        |     12 |    490 |              198 |      565 |  65 |    150 |      60 |             115 |               60 |   115 | dark      | None      |
| garchomp-mega              | 10058 | garchomp     |     19 |    950 |              315 |      700 | 108 |    170 |     115 |             120 |               95 |    92 | dragon    | ground    |
| lucario-mega               | 10059 | lucario      |     13 |    575 |              219 |      625 |  70 |    145 |      88 |             140 |               70 |   112 | fighting  | steel     |
| abomasnow-mega             | 10060 | abomasnow    |     27 |   1850 |              208 |      594 |  90 |    132 |     105 |             132 |              105 |    30 | grass     | ice       |
| floette-eternal            | 10061 | floette      |      2 |      9 |              243 |      551 |  74 |     65 |      67 |             125 |              128 |    92 | fairy     | None      |
| latias-mega                | 10062 | latias       |     18 |    520 |              315 |      700 |  80 |    100 |     120 |             140 |              150 |   110 | dragon    | psychic   |
| latios-mega                | 10063 | latios       |     23 |    700 |              315 |      700 |  80 |    130 |     100 |             160 |              120 |   110 | dragon    | psychic   |
| swampert-mega              | 10064 | swampert     |     19 |   1020 |              286 |      635 | 100 |    150 |     110 |              95 |              110 |    70 | water     | ground    |
| sceptile-mega              | 10065 | sceptile     |     19 |    552 |              284 |      630 |  70 |    110 |      75 |             145 |               85 |   145 | grass     | dragon    |
| sableye-mega               | 10066 | sableye      |      5 |   1610 |              168 |      480 |  50 |     85 |     125 |              85 |              115 |    20 | dark      | ghost     |
| altaria-mega               | 10067 | altaria      |     15 |    206 |              207 |      590 |  75 |    110 |     110 |             110 |              105 |    80 | dragon    | fairy     |
| gallade-mega               | 10068 | gallade      |     16 |    564 |              278 |      618 |  68 |    165 |      95 |              65 |              115 |   110 | psychic   | fighting  |
| audino-mega                | 10069 | audino       |     15 |    320 |              425 |      545 | 103 |     60 |     126 |              80 |              126 |    50 | normal    | fairy     |
| sharpedo-mega              | 10070 | sharpedo     |     25 |   1303 |              196 |      560 |  70 |    140 |      70 |             110 |               65 |   105 | water     | dark      |
| slowbro-mega               | 10071 | slowbro      |     20 |   1200 |              207 |      590 |  95 |     75 |     180 |             130 |               80 |    30 | water     | psychic   |
| steelix-mega               | 10072 | steelix      |    105 |   7400 |              214 |      610 |  75 |    125 |     230 |              55 |               95 |    30 | steel     | ground    |
| pidgeot-mega               | 10073 | pidgeot      |     22 |    505 |              261 |      579 |  83 |     80 |      80 |             135 |               80 |   121 | normal    | flying    |
| glalie-mega                | 10074 | glalie       |     21 |   3502 |              203 |      580 |  80 |    120 |      80 |             120 |               80 |   100 | ice       | None      |
| diancie-mega               | 10075 | diancie      |     11 |    278 |              315 |      700 |  50 |    160 |     110 |             160 |              110 |   110 | rock      | fairy     |
| metagross-mega             | 10076 | metagross    |     25 |   9429 |              315 |      700 |  80 |    145 |     150 |             105 |              110 |   110 | steel     | psychic   |
| kyogre-primal              | 10077 | kyogre       |     98 |   4300 |              347 |      770 | 100 |    150 |      90 |             180 |              160 |    90 | water     | None      |
| groudon-primal             | 10078 | groudon      |     50 |   9997 |              347 |      770 | 100 |    180 |     160 |             150 |               90 |    90 | ground    | fire      |
| rayquaza-mega              | 10079 | rayquaza     |    108 |   3920 |              351 |      780 | 105 |    180 |     100 |             180 |              100 |   115 | dragon    | flying    |
| pikachu-rock-star          | 10080 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-belle              | 10081 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-pop-star           | 10082 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-phd                | 10083 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-libre              | 10084 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-cosplay            | 10085 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| hoopa-unbound              | 10086 | hoopa        |     65 |   4900 |              306 |      680 |  80 |    160 |      60 |             170 |              130 |    80 | psychic   | dark      |
| camerupt-mega              | 10087 | camerupt     |     25 |   3205 |              196 |      560 |  70 |    120 |     100 |             145 |              105 |    20 | fire      | ground    |
| lopunny-mega               | 10088 | lopunny      |     13 |    283 |              203 |      580 |  65 |    136 |      94 |              54 |               96 |   135 | normal    | fighting  |
| salamence-mega             | 10089 | salamence    |     18 |   1126 |              315 |      700 |  95 |    145 |     130 |             120 |               90 |   120 | dragon    | flying    |
| beedrill-mega              | 10090 | beedrill     |     14 |    405 |              223 |      495 |  65 |    150 |      40 |              15 |               80 |   145 | bug       | poison    |
| rattata-alola              | 10091 | rattata      |      3 |     38 |               51 |      253 |  30 |     56 |      35 |              25 |               35 |    72 | dark      | normal    |
| raticate-alola             | 10092 | raticate     |      7 |    255 |              145 |      413 |  75 |     71 |      70 |              40 |               80 |    77 | dark      | normal    |
| raticate-totem-alola       | 10093 | raticate     |     14 |   1050 |              145 |      413 |  75 |     71 |      70 |              40 |               80 |    77 | dark      | normal    |
| pikachu-original-cap       | 10094 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-hoenn-cap          | 10095 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-sinnoh-cap         | 10096 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-unova-cap          | 10097 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-kalos-cap          | 10098 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| pikachu-alola-cap          | 10099 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| raichu-alola               | 10100 | raichu       |      7 |    210 |              218 |      485 |  60 |     85 |      50 |              95 |               85 |   110 | electric  | psychic   |
| sandshrew-alola            | 10101 | sandshrew    |      7 |    400 |               60 |      300 |  50 |     75 |      90 |              10 |               35 |    40 | ice       | steel     |
| sandslash-alola            | 10102 | sandslash    |     12 |    550 |              158 |      450 |  75 |    100 |     120 |              25 |               65 |    65 | ice       | steel     |
| vulpix-alola               | 10103 | vulpix       |      6 |     99 |               60 |      299 |  38 |     41 |      40 |              50 |               65 |    65 | ice       | None      |
| ninetales-alola            | 10104 | ninetales    |     11 |    199 |              177 |      505 |  73 |     67 |      75 |              81 |              100 |   109 | ice       | fairy     |
| diglett-alola              | 10105 | diglett      |      2 |     10 |               53 |      265 |  10 |     55 |      30 |              35 |               45 |    90 | ground    | steel     |
| dugtrio-alola              | 10106 | dugtrio      |      7 |    666 |              149 |      425 |  35 |    100 |      60 |              50 |               70 |   110 | ground    | steel     |
| meowth-alola               | 10107 | meowth       |      4 |     42 |               58 |      290 |  40 |     35 |      35 |              50 |               40 |    90 | dark      | None      |
| persian-alola              | 10108 | persian      |     11 |    330 |              154 |      440 |  65 |     60 |      60 |              75 |               65 |   115 | dark      | None      |
| geodude-alola              | 10109 | geodude      |      4 |    203 |               60 |      300 |  40 |     80 |     100 |              30 |               30 |    20 | rock      | electric  |
| graveler-alola             | 10110 | graveler     |     10 |   1100 |              137 |      390 |  55 |     95 |     115 |              45 |               45 |    35 | rock      | electric  |
| golem-alola                | 10111 | golem        |     17 |   3160 |              223 |      495 |  80 |    120 |     130 |              55 |               65 |    45 | rock      | electric  |
| grimer-alola               | 10112 | grimer       |      7 |    420 |               65 |      325 |  80 |     80 |      50 |              40 |               50 |    25 | poison    | dark      |
| muk-alola                  | 10113 | muk          |     10 |    520 |              175 |      500 | 105 |    105 |      75 |              65 |              100 |    50 | poison    | dark      |
| exeggutor-alola            | 10114 | exeggutor    |    109 |   4156 |              186 |      530 |  95 |    105 |      85 |             125 |               75 |    45 | grass     | dragon    |
| marowak-alola              | 10115 | marowak      |     10 |    340 |              149 |      425 |  60 |     80 |     110 |              50 |               80 |    45 | fire      | ghost     |
| greninja-battle-bond       | 10116 | greninja     |     15 |    400 |              239 |      530 |  72 |     95 |      67 |             103 |               71 |   122 | water     | dark      |
| greninja-ash               | 10117 | greninja     |     15 |    400 |              288 |      640 |  72 |    145 |      67 |             153 |               71 |   132 | water     | dark      |
| zygarde-10                 | 10118 | zygarde      |     12 |    335 |              219 |      486 |  54 |    100 |      71 |              61 |               85 |   115 | dragon    | ground    |
| zygarde-50                 | 10119 | zygarde      |     50 |   3050 |              270 |      600 | 108 |    100 |     121 |              81 |               95 |    95 | dragon    | ground    |
| zygarde-complete           | 10120 | zygarde      |     45 |   6100 |              319 |      708 | 216 |    100 |     121 |              91 |               95 |    85 | dragon    | ground    |
| gumshoos-totem             | 10121 | gumshoos     |     14 |    600 |              146 |      418 |  88 |    110 |      60 |              55 |               60 |    45 | normal    | None      |
| vikavolt-totem             | 10122 | vikavolt     |     26 |   1475 |              225 |      500 |  77 |     70 |      90 |             145 |               75 |    43 | bug       | electric  |
| oricorio-pom-pom           | 10123 | oricorio     |      6 |     34 |              167 |      476 |  75 |     70 |      70 |              98 |               70 |    93 | electric  | flying    |
| oricorio-pau               | 10124 | oricorio     |      6 |     34 |              167 |      476 |  75 |     70 |      70 |              98 |               70 |    93 | psychic   | flying    |
| oricorio-sensu             | 10125 | oricorio     |      6 |     34 |              167 |      476 |  75 |     70 |      70 |              98 |               70 |    93 | ghost     | flying    |
| lycanroc-midnight          | 10126 | lycanroc     |     11 |    250 |              170 |      487 |  85 |    115 |      75 |              55 |               75 |    82 | rock      | None      |
| wishiwashi-school          | 10127 | wishiwashi   |     82 |    786 |              217 |      620 |  45 |    140 |     130 |             140 |              135 |    30 | water     | None      |
| lurantis-totem             | 10128 | lurantis     |     15 |    580 |              168 |      480 |  70 |    105 |      90 |              80 |               90 |    45 | grass     | None      |
| salazzle-totem             | 10129 | salazzle     |     21 |    810 |              168 |      480 |  68 |     64 |      60 |             111 |               60 |   117 | poison    | fire      |
| minior-orange-meteor       | 10130 | minior       |      3 |    400 |              154 |      440 |  60 |     60 |     100 |              60 |              100 |    60 | rock      | flying    |
| minior-yellow-meteor       | 10131 | minior       |      3 |    400 |              154 |      440 |  60 |     60 |     100 |              60 |              100 |    60 | rock      | flying    |
| minior-green-meteor        | 10132 | minior       |      3 |    400 |              154 |      440 |  60 |     60 |     100 |              60 |              100 |    60 | rock      | flying    |
| minior-blue-meteor         | 10133 | minior       |      3 |    400 |              154 |      440 |  60 |     60 |     100 |              60 |              100 |    60 | rock      | flying    |
| minior-indigo-meteor       | 10134 | minior       |      3 |    400 |              154 |      440 |  60 |     60 |     100 |              60 |              100 |    60 | rock      | flying    |
| minior-violet-meteor       | 10135 | minior       |      3 |    400 |              154 |      440 |  60 |     60 |     100 |              60 |              100 |    60 | rock      | flying    |
| minior-red                 | 10136 | minior       |      3 |      3 |              175 |      500 |  60 |    100 |      60 |             100 |               60 |   120 | rock      | flying    |
| minior-orange              | 10137 | minior       |      3 |      3 |              175 |      500 |  60 |    100 |      60 |             100 |               60 |   120 | rock      | flying    |
| minior-yellow              | 10138 | minior       |      3 |      3 |              175 |      500 |  60 |    100 |      60 |             100 |               60 |   120 | rock      | flying    |
| minior-green               | 10139 | minior       |      3 |      3 |              175 |      500 |  60 |    100 |      60 |             100 |               60 |   120 | rock      | flying    |
| minior-blue                | 10140 | minior       |      3 |      3 |              175 |      500 |  60 |    100 |      60 |             100 |               60 |   120 | rock      | flying    |
| minior-indigo              | 10141 | minior       |      3 |      3 |              175 |      500 |  60 |    100 |      60 |             100 |               60 |   120 | rock      | flying    |
| minior-violet              | 10142 | minior       |      3 |      3 |              175 |      500 |  60 |    100 |      60 |             100 |               60 |   120 | rock      | flying    |
| mimikyu-busted             | 10143 | mimikyu      |      2 |      7 |              167 |      476 |  55 |     90 |      80 |              50 |              105 |    96 | ghost     | fairy     |
| mimikyu-totem-disguised    | 10144 | mimikyu      |      4 |     28 |              167 |      476 |  55 |     90 |      80 |              50 |              105 |    96 | ghost     | fairy     |
| mimikyu-totem-busted       | 10145 | mimikyu      |      4 |     28 |              167 |      476 |  55 |     90 |      80 |              50 |              105 |    96 | ghost     | fairy     |
| kommo-o-totem              | 10146 | kommo-o      |     24 |   2075 |              270 |      600 |  75 |    110 |     125 |             100 |              105 |    85 | dragon    | fighting  |
| magearna-original          | 10147 | magearna     |     10 |    805 |              270 |      600 |  80 |     95 |     115 |             130 |              115 |    65 | steel     | fairy     |
| pikachu-partner-cap        | 10148 | pikachu      |      4 |     60 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| marowak-totem              | 10149 | marowak      |     17 |    980 |              149 |      425 |  60 |     80 |     110 |              50 |               80 |    45 | fire      | ghost     |
| ribombee-totem             | 10150 | ribombee     |      4 |     20 |              162 |      464 |  60 |     55 |      60 |              95 |               70 |   124 | bug       | fairy     |
| rockruff-own-tempo         | 10151 | rockruff     |      5 |     92 |               56 |      280 |  45 |     65 |      40 |              30 |               40 |    60 | rock      | None      |
| lycanroc-dusk              | 10152 | lycanroc     |      8 |    250 |              170 |      487 |  75 |    117 |      65 |              55 |               65 |   110 | rock      | None      |
| araquanid-totem            | 10153 | araquanid    |     31 |   2175 |              159 |      454 |  68 |     70 |      92 |              50 |              132 |    42 | water     | bug       |
| togedemaru-totem           | 10154 | togedemaru   |      6 |    130 |              152 |      435 |  65 |     98 |      63 |              40 |               73 |    96 | electric  | steel     |
| necrozma-dusk              | 10155 | necrozma     |     38 |   4600 |              306 |      680 |  97 |    157 |     127 |             113 |              109 |    77 | psychic   | steel     |
| necrozma-dawn              | 10156 | necrozma     |     42 |   3500 |              306 |      680 |  97 |    113 |     109 |             157 |              127 |    77 | psychic   | ghost     |
| necrozma-ultra             | 10157 | necrozma     |     75 |   2300 |              339 |      754 |  97 |    167 |      97 |             167 |               97 |   129 | psychic   | dragon    |
| meowth-galar               | 10158 | meowth       |      4 |     75 |               58 |      290 |  50 |     65 |      55 |              40 |               40 |    40 | steel     | None      |
| ponyta-galar               | 10159 | ponyta       |      8 |    240 |               82 |      410 |  50 |     85 |      55 |              65 |               65 |    90 | psychic   | None      |
| rapidash-galar             | 10160 | rapidash     |     17 |    800 |              175 |      500 |  65 |    100 |      70 |              80 |               80 |   105 | psychic   | fairy     |
| slowpoke-galar             | 10161 | slowpoke     |     12 |    360 |               63 |      315 |  90 |     65 |      65 |              40 |               40 |    15 | psychic   | None      |
| slowbro-galar              | 10162 | slowbro      |     16 |    705 |              172 |      490 |  95 |    100 |      95 |             100 |               70 |    30 | poison    | psychic   |
| farfetchd-galar            | 10163 | farfetchd    |      8 |    420 |              132 |      377 |  52 |     95 |      55 |              58 |               62 |    55 | fighting  | None      |
| weezing-galar              | 10164 | weezing      |     30 |    160 |              172 |      490 |  65 |     90 |     120 |              85 |               70 |    60 | poison    | fairy     |
| mr-mime-galar              | 10165 | mr-mime      |     14 |    568 |              161 |      460 |  50 |     65 |      65 |              90 |               90 |   100 | ice       | psychic   |
| articuno-galar             | 10166 | articuno     |     17 |    509 |              261 |      580 |  90 |     85 |      85 |             125 |              100 |    95 | psychic   | flying    |
| zapdos-galar               | 10167 | zapdos       |     16 |    582 |              261 |      580 |  90 |    125 |      90 |              85 |               90 |   100 | fighting  | flying    |
| moltres-galar              | 10168 | moltres      |     20 |    660 |              261 |      580 |  90 |     85 |      90 |             100 |              125 |    90 | dark      | flying    |
| slowking-galar             | 10169 | slowking     |     18 |    795 |              172 |      490 |  95 |     65 |      80 |             110 |              110 |    30 | poison    | psychic   |
| corsola-galar              | 10170 | corsola      |      6 |      5 |              144 |      410 |  60 |     55 |     100 |              65 |              100 |    30 | ghost     | None      |
| zigzagoon-galar            | 10171 | zigzagoon    |      4 |    175 |               56 |      240 |  38 |     30 |      41 |              30 |               41 |    60 | dark      | normal    |
| linoone-galar              | 10172 | linoone      |      5 |    325 |              147 |      420 |  78 |     70 |      61 |              50 |               61 |   100 | dark      | normal    |
| darumaka-galar             | 10173 | darumaka     |      7 |    400 |               63 |      315 |  70 |     90 |      45 |              15 |               45 |    50 | ice       | None      |
| darmanitan-standard-galar  | 10174 | darmanitan   |     17 |   1200 |              168 |      480 | 105 |    140 |      55 |              30 |               55 |    95 | ice       | None      |
| darmanitan-zen-galar       | 10175 | darmanitan   |     17 |   1200 |              168 |      540 | 105 |    160 |      55 |              30 |               55 |   135 | ice       | fire      |
| yamask-galar               | 10176 | yamask       |      5 |     15 |               61 |      303 |  38 |     55 |      85 |              30 |               65 |    30 | ground    | ghost     |
| stunfisk-galar             | 10177 | stunfisk     |      7 |    205 |              165 |      471 | 109 |     81 |      99 |              66 |               84 |    32 | ground    | steel     |
| toxtricity-low-key         | 10178 | toxtricity   |     16 |    400 |              176 |      502 |  75 |     98 |      70 |             114 |               70 |    75 | electric  | poison    |
| eiscue-noice               | 10179 | eiscue       |     14 |    890 |              165 |      470 |  75 |     80 |      70 |              65 |               50 |   130 | ice       | None      |
| indeedee-female            | 10180 | indeedee     |      9 |     28 |              166 |      475 |  70 |     55 |      65 |              95 |              105 |    85 | psychic   | normal    |
| zacian-crowned             | 10181 | zacian       |     28 |   3550 |              335 |      720 |  92 |    170 |     115 |              80 |              115 |   148 | fairy     | steel     |
| zamazenta-crowned          | 10182 | zamazenta    |     29 |   7850 |              335 |      720 |  92 |    130 |     145 |              80 |              145 |   128 | fighting  | steel     |
| urshifu-rapid-strike       | 10183 | urshifu      |     19 |   1050 |              275 |      550 | 100 |    130 |     100 |              63 |               60 |    97 | fighting  | water     |
| calyrex-ice-rider          | 10184 | calyrex      |     24 |   8091 |              340 |      680 | 100 |    165 |     150 |              85 |              130 |    50 | psychic   | ice       |
| calyrex-shadow-rider       | 10185 | calyrex      |     24 |    536 |              340 |      680 | 100 |     85 |      80 |             165 |              100 |   150 | psychic   | ghost     |
| venusaur-gmax              | 10186 | venusaur     |    240 |  10000 |              236 |      525 |  80 |     82 |      83 |             100 |              100 |    80 | grass     | poison    |
| charizard-gmax             | 10187 | charizard    |    280 |  10000 |              240 |      534 |  78 |     84 |      78 |             109 |               85 |   100 | fire      | flying    |
| blastoise-gmax             | 10188 | blastoise    |    250 |  10000 |              239 |      530 |  79 |     83 |     100 |              85 |              105 |    78 | water     | None      |
| butterfree-gmax            | 10189 | butterfree   |    170 |  10000 |              178 |      395 |  60 |     45 |      50 |              90 |               80 |    70 | bug       | flying    |
| pikachu-gmax               | 10190 | pikachu      |    210 |  10000 |              112 |      320 |  35 |     55 |      40 |              50 |               50 |    90 | electric  | None      |
| meowth-gmax                | 10191 | meowth       |    330 |  10000 |               58 |      290 |  40 |     45 |      35 |              40 |               40 |    90 | normal    | None      |
| machamp-gmax               | 10192 | machamp      |    250 |  10000 |              227 |      505 |  90 |    130 |      80 |              65 |               85 |    55 | fighting  | None      |
| gengar-gmax                | 10193 | gengar       |    200 |  10000 |              225 |      500 |  60 |     65 |      60 |             130 |               75 |   110 | ghost     | poison    |
| kingler-gmax               | 10194 | kingler      |    190 |  10000 |              166 |      475 |  55 |    130 |     115 |              50 |               50 |    75 | water     | None      |
| lapras-gmax                | 10195 | lapras       |    240 |  10000 |              187 |      535 | 130 |     85 |      80 |              85 |               95 |    60 | water     | ice       |
| eevee-gmax                 | 10196 | eevee        |    180 |  10000 |               65 |      325 |  55 |     55 |      50 |              45 |               65 |    55 | normal    | None      |
| snorlax-gmax               | 10197 | snorlax      |    350 |  10000 |              189 |      540 | 160 |    110 |      65 |              65 |              110 |    30 | normal    | None      |
| garbodor-gmax              | 10198 | garbodor     |    210 |  10000 |              166 |      474 |  80 |     95 |      82 |              60 |               82 |    75 | poison    | None      |
| melmetal-gmax              | 10199 | melmetal     |    250 |  10000 |              270 |      600 | 135 |    143 |     143 |              80 |               65 |    34 | steel     | None      |
| rillaboom-gmax             | 10200 | rillaboom    |    280 |  10000 |              265 |      530 | 100 |    125 |      90 |              60 |               70 |    85 | grass     | None      |
| cinderace-gmax             | 10201 | cinderace    |    270 |  10000 |              265 |      530 |  80 |    116 |      75 |              65 |               75 |   119 | fire      | None      |
| inteleon-gmax              | 10202 | inteleon     |    400 |  10000 |              265 |      530 |  70 |     85 |      65 |             125 |               65 |   120 | water     | None      |
| corviknight-gmax           | 10203 | corviknight  |    140 |  10000 |              248 |      495 |  98 |     87 |     105 |              53 |               85 |    67 | flying    | steel     |
| orbeetle-gmax              | 10204 | orbeetle     |    140 |  10000 |              253 |      505 |  60 |     45 |     110 |              80 |              120 |    90 | bug       | psychic   |
| drednaw-gmax               | 10205 | drednaw      |    240 |  10000 |              170 |      485 |  90 |    115 |      90 |              48 |               68 |    74 | water     | rock      |
| coalossal-gmax             | 10206 | coalossal    |    420 |  10000 |              255 |      510 | 110 |     80 |     120 |              80 |               90 |    30 | rock      | fire      |
| flapple-gmax               | 10207 | flapple      |    240 |  10000 |              170 |      485 |  70 |    110 |      80 |              95 |               60 |    70 | grass     | dragon    |
| appletun-gmax              | 10208 | appletun     |    240 |  10000 |              170 |      485 | 110 |     85 |      80 |             100 |               80 |    30 | grass     | dragon    |
| sandaconda-gmax            | 10209 | sandaconda   |    220 |  10000 |              179 |      510 |  72 |    107 |     125 |              65 |               70 |    71 | ground    | None      |
| toxtricity-amped-gmax      | 10210 | toxtricity   |    240 |  10000 |              176 |      502 |  75 |     98 |      70 |             114 |               70 |    75 | electric  | poison    |
| centiskorch-gmax           | 10211 | centiskorch  |    750 |  10000 |              184 |      525 | 100 |    115 |      65 |              90 |               90 |    65 | fire      | bug       |
| hatterene-gmax             | 10212 | hatterene    |    260 |  10000 |              255 |      510 |  57 |     90 |      95 |             136 |              103 |    29 | psychic   | fairy     |
| grimmsnarl-gmax            | 10213 | grimmsnarl   |    320 |  10000 |              255 |      510 |  95 |    120 |      65 |              95 |               75 |    60 | dark      | fairy     |
| alcremie-gmax              | 10214 | alcremie     |    300 |  10000 |              173 |      495 |  65 |     60 |      75 |             110 |              121 |    64 | fairy     | None      |
| copperajah-gmax            | 10215 | copperajah   |    230 |  10000 |              175 |      500 | 122 |    130 |      69 |              80 |               69 |    30 | steel     | None      |
| duraludon-gmax             | 10216 | duraludon    |    430 |  10000 |              187 |      535 |  70 |     95 |     115 |             120 |               50 |    85 | steel     | dragon    |
| eternatus-eternamax        | 10217 | eternatus    |   1000 |  10000 |              345 |     1125 | 255 |    115 |     250 |             125 |              250 |   130 | poison    | dragon    |
| urshifu-single-strike-gmax | 10218 | urshifu      |    290 |  10000 |              275 |      550 | 100 |    130 |     100 |              63 |               60 |    97 | fighting  | dark      |
| urshifu-rapid-strike-gmax  | 10219 | urshifu      |    260 |  10000 |              275 |      550 | 100 |    130 |     100 |              63 |               60 |    97 | fighting  | water     |
| toxtricity-low-key-gmax    | 10220 | toxtricity   |    240 |  10000 |              176 |      502 |  75 |     98 |      70 |             114 |               70 |    75 | electric  | poison    |

</div>

``` r
###height to weight ratio
allPoke %>%mutate(hgtwgt_ratio=height/weight)%>%summary(hgtwgt_ratio)
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
    ##    type_one           type_two          hgtwgt_ratio     
    ##  Length:1118        Length:1118        Min.   : 0.00010  
    ##  Class :character   Class :character   1st Qu.: 0.02047  
    ##  Mode  :character   Mode  :character   Median : 0.03478  
    ##                                        Mean   : 0.11000  
    ##                                        3rd Qu.: 0.06667  
    ##                                        Max.   :16.00000

### Contingency Tables

``` r
#table(allPoke$type_one,allPoke$species)
#allPoke %>% distinct()
#inner_join(allPoke,evolveStages,by="species") %>% select(name,stages,everything())

combinePoke<-inner_join(allPoke,evolveStages,by="species") %>% select(name,stages,everything())

tOne<-table(combinePoke$stages,combinePoke$type_one )
tOne<-table(combinePoke$type_one,combinePoke$stages )
kable(addmargins(tOne),caption = "Contingency Table of Type by Stage")
```

|          | one | two | three | noEvolve |  Sum |
|:---------|----:|----:|------:|---------:|-----:|
| bug      |  27 |  34 |    12 |       11 |   84 |
| dark     |  16 |  19 |     4 |        9 |   48 |
| dragon   |   7 |   9 |    10 |       17 |   43 |
| electric |  12 |  31 |     8 |       26 |   77 |
| fairy    |   7 |  10 |     3 |        4 |   24 |
| fighting |  13 |  20 |     3 |        9 |   45 |
| fire     |  18 |  25 |    15 |       11 |   69 |
| flying   |   2 |   2 |     2 |        3 |    9 |
| ghost    |  15 |  16 |     5 |       10 |   46 |
| grass    |  31 |  38 |    19 |        8 |   96 |
| ground   |  16 |  17 |     3 |        6 |   42 |
| ice      |  11 |  16 |     4 |        8 |   39 |
| normal   |  43 |  39 |    11 |       25 |  118 |
| poison   |  16 |  21 |     3 |        3 |   43 |
| psychic  |  18 |  19 |    12 |       31 |   80 |
| rock     |  20 |  20 |     7 |       26 |   73 |
| steel    |   8 |  12 |     7 |       13 |   40 |
| water    |  45 |  54 |    20 |       22 |  141 |
| Sum      | 325 | 402 |   148 |      242 | 1117 |

Contingency Table of Type by Stage

``` r
table(allSpecies$shape)
```

    ## 
    ##     armor      arms      ball      blob bug-wings      fish     heads  humanoid 
    ##        40        47        42        38        20        39        16       137 
    ##      legs quadruped  squiggle tentacles   upright     wings 
    ##        23       167        37        23       189        80

``` r
table(allSpecies$generation,allSpecies$shape)
```

    ##                  
    ##                   armor arms ball blob bug-wings fish heads humanoid legs
    ##   generation-i        7    5    7    7         4    6     4       21    7
    ##   generation-ii       4    0    5    2         3    4     0       15    2
    ##   generation-iii      8    8    8    4         5   12     1       27    4
    ##   generation-iv       2    8    3    3         4    2     3       16    2
    ##   generation-v        9   12    7    7         1    7     4       25    1
    ##   generation-vi       2    7    3    7         1    0     3        6    1
    ##   generation-vii      6    5    5    3         1    5     0       15    2
    ##   generation-viii     2    2    4    5         1    3     1       12    4
    ##                  
    ##                   quadruped squiggle tentacles upright wings
    ##   generation-i           25        8         4      34    12
    ##   generation-ii          22        5         1      24    13
    ##   generation-iii         20        5         0      24     9
    ##   generation-iv          25        3         2      26     8
    ##   generation-v           29        1         5      33    15
    ##   generation-vi          14        4         2      16     6
    ##   generation-vii         16        6         4      11     9
    ##   generation-viii        16        5         5      21     8

``` r
#table(allSpecies$shape,combinePoke$type_one)
```

### Box Plot

``` r
###Total hit points versus evolution stages
```

``` r
head(allPoke)
```

<div class="kable-table">

| name       |  id | species    | height | weight | base\_experience |  hp | attack | defense | special\_attack | special\_defense | speed | type\_one | type\_two |
|:-----------|----:|:-----------|-------:|-------:|-----------------:|----:|-------:|--------:|----------------:|-----------------:|------:|:----------|:----------|
| bulbasaur  |   1 | bulbasaur  |      7 |     69 |               64 |  45 |     49 |      49 |              65 |               65 |    45 | grass     | poison    |
| ivysaur    |   2 | ivysaur    |     10 |    130 |              142 |  60 |     62 |      63 |              80 |               80 |    60 | grass     | poison    |
| venusaur   |   3 | venusaur   |     20 |   1000 |              236 |  80 |     82 |      83 |             100 |              100 |    80 | grass     | poison    |
| charmander |   4 | charmander |      6 |     85 |               62 |  39 |     52 |      43 |              60 |               50 |    65 | fire      | None      |
| charmeleon |   5 | charmeleon |     11 |    190 |              142 |  58 |     64 |      58 |              80 |               65 |    80 | fire      | None      |
| charizard  |   6 | charizard  |     17 |    905 |              240 |  78 |     84 |      78 |             109 |               85 |   100 | fire      | flying    |

</div>

``` r
g<-ggplot(data = allPoke,aes(x=weight,y=height))
g+geom_point(aes(color=type_one))
```

![](images/plotheightweight-1.png)<!-- -->

``` r
allPoke %>% group_by(type_one) %>% summarise(mweight=mean(weight),sdweight=sd(weight))
```

<div class="kable-table">

| type\_one |   mweight | sdweight |
|:----------|----------:|---------:|
| bug       |  576.4471 | 1560.404 |
| dark      |  786.3750 | 1874.439 |
| dragon    | 1331.1628 | 1384.587 |
| electric  |  704.8442 | 1936.162 |
| fairy     |  787.7917 | 2128.368 |
| fighting  | 1398.5111 | 2617.478 |
| fire      | 1061.4928 | 2071.127 |
| flying    | 1477.5556 | 3211.211 |
| ghost     |  802.0870 | 2024.955 |
| grass     |  800.3542 | 2032.037 |
| ground    | 1540.1905 | 2777.023 |
| ice       | 1094.7692 | 1570.560 |
| normal    |  682.6271 | 1659.021 |
| poison    | 1050.6279 | 2456.101 |
| psychic   |  891.5750 | 1925.071 |
| rock      | 1111.0548 | 1718.358 |
| steel     | 2850.1500 | 3478.657 |
| water     |  926.2199 | 1914.515 |

</div>

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
