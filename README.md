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
getEveryPokeData()
```

    ##           name  id    species height weight base_experience
    ## 1    bulbasaur   1  bulbasaur      7     69              64
    ## 2      ivysaur   2    ivysaur     10    130             142
    ## 3     venusaur   3   venusaur     20   1000             236
    ## 4   charmander   4 charmander      6     85              62
    ## 5   charmeleon   5 charmeleon     11    190             142
    ## 6    charizard   6  charizard     17    905             240
    ## 7     squirtle   7   squirtle      5     90              63
    ## 8    wartortle   8  wartortle     10    225             142
    ## 9    blastoise   9  blastoise     16    855             239
    ## 10    caterpie  10   caterpie      3     29              39
    ## 11     metapod  11    metapod      7     99              72
    ## 12  butterfree  12 butterfree     11    320             178
    ## 13      weedle  13     weedle      3     32              39
    ## 14      kakuna  14     kakuna      6    100              72
    ## 15    beedrill  15   beedrill     10    295             178
    ## 16      pidgey  16     pidgey      3     18              50
    ## 17   pidgeotto  17  pidgeotto     11    300             122
    ## 18     pidgeot  18    pidgeot     15    395             216
    ## 19     rattata  19    rattata      3     35              51
    ## 20    raticate  20   raticate      7    185             145
    ## 21     spearow  21    spearow      3     20              52
    ## 22      fearow  22     fearow     12    380             155
    ## 23       ekans  23      ekans     20     69              58
    ## 24       arbok  24      arbok     35    650             157
    ## 25     pikachu  25    pikachu      4     60             112
    ## 26      raichu  26     raichu      8    300             218
    ## 27   sandshrew  27  sandshrew      6    120              60
    ## 28   sandslash  28  sandslash     10    295             158
    ## 29   nidoran-f  29  nidoran-f      4     70              55
    ## 30    nidorina  30   nidorina      8    200             128
    ## 31   nidoqueen  31  nidoqueen     13    600             227
    ## 32   nidoran-m  32  nidoran-m      5     90              55
    ## 33    nidorino  33   nidorino      9    195             128
    ## 34    nidoking  34   nidoking     14    620             227
    ## 35    clefairy  35   clefairy      6     75             113
    ## 36    clefable  36   clefable     13    400             217
    ## 37      vulpix  37     vulpix      6     99              60
    ## 38   ninetales  38  ninetales     11    199             177
    ## 39  jigglypuff  39 jigglypuff      5     55              95
    ## 40  wigglytuff  40 wigglytuff     10    120             196
    ## 41       zubat  41      zubat      8     75              49
    ## 42      golbat  42     golbat     16    550             159
    ## 43      oddish  43     oddish      5     54              64
    ## 44       gloom  44      gloom      8     86             138
    ## 45   vileplume  45  vileplume     12    186             221
    ## 46       paras  46      paras      3     54              57
    ## 47    parasect  47   parasect     10    295             142
    ## 48     venonat  48    venonat     10    300              61
    ## 49    venomoth  49   venomoth     15    125             158
    ## 50     diglett  50    diglett      2      8              53
    ## 51     dugtrio  51    dugtrio      7    333             149
    ## 52      meowth  52     meowth      4     42              58
    ## 53     persian  53    persian     10    320             154
    ## 54     psyduck  54    psyduck      8    196              64
    ## 55     golduck  55    golduck     17    766             175
    ## 56      mankey  56     mankey      5    280              61
    ## 57    primeape  57   primeape     10    320             159
    ## 58   growlithe  58  growlithe      7    190              70
    ## 59    arcanine  59   arcanine     19   1550             194
    ## 60     poliwag  60    poliwag      6    124              60
    ## 61   poliwhirl  61  poliwhirl     10    200             135
    ## 62   poliwrath  62  poliwrath     13    540             230
    ## 63        abra  63       abra      9    195              62
    ## 64     kadabra  64    kadabra     13    565             140
    ## 65    alakazam  65   alakazam     15    480             225
    ## 66      machop  66     machop      8    195              61
    ## 67     machoke  67    machoke     15    705             142
    ## 68     machamp  68    machamp     16   1300             227
    ## 69  bellsprout  69 bellsprout      7     40              60
    ## 70  weepinbell  70 weepinbell     10     64             137
    ## 71  victreebel  71 victreebel     17    155             221
    ## 72   tentacool  72  tentacool      9    455              67
    ## 73  tentacruel  73 tentacruel     16    550             180
    ## 74     geodude  74    geodude      4    200              60
    ## 75    graveler  75   graveler     10   1050             137
    ## 76       golem  76      golem     14   3000             223
    ## 77      ponyta  77     ponyta     10    300              82
    ## 78    rapidash  78   rapidash     17    950             175
    ## 79    slowpoke  79   slowpoke     12    360              63
    ## 80     slowbro  80    slowbro     16    785             172
    ## 81   magnemite  81  magnemite      3     60              65
    ## 82    magneton  82   magneton     10    600             163
    ## 83   farfetchd  83  farfetchd      8    150             132
    ## 84       doduo  84      doduo     14    392              62
    ## 85      dodrio  85     dodrio     18    852             165
    ## 86        seel  86       seel     11    900              65
    ## 87     dewgong  87    dewgong     17   1200             166
    ## 88      grimer  88     grimer      9    300              65
    ## 89         muk  89        muk     12    300             175
    ## 90    shellder  90   shellder      3     40              61
    ## 91    cloyster  91   cloyster     15   1325             184
    ## 92      gastly  92     gastly     13      1              62
    ## 93     haunter  93    haunter     16      1             142
    ## 94      gengar  94     gengar     15    405             225
    ## 95        onix  95       onix     88   2100              77
    ## 96     drowzee  96    drowzee     10    324              66
    ## 97       hypno  97      hypno     16    756             169
    ## 98      krabby  98     krabby      4     65              65
    ## 99     kingler  99    kingler     13    600             166
    ## 100    voltorb 100    voltorb      5    104              66
    ## 101  electrode 101  electrode     12    666             172
    ## 102  exeggcute 102  exeggcute      4     25              65
    ## 103  exeggutor 103  exeggutor     20   1200             186
    ## 104     cubone 104     cubone      4     65              64
    ## 105    marowak 105    marowak     10    450             149
    ## 106  hitmonlee 106  hitmonlee     15    498             159
    ## 107 hitmonchan 107 hitmonchan     14    502             159
    ## 108  lickitung 108  lickitung     12    655              77
    ## 109    koffing 109    koffing      6     10              68
    ## 110    weezing 110    weezing     12     95             172
    ## 111    rhyhorn 111    rhyhorn     10   1150              69
    ## 112     rhydon 112     rhydon     19   1200             170
    ## 113    chansey 113    chansey     11    346             395
    ## 114    tangela 114    tangela     10    350              87
    ## 115 kangaskhan 115 kangaskhan     22    800             172
    ## 116     horsea 116     horsea      4     80              59
    ## 117     seadra 117     seadra     12    250             154
    ## 118    goldeen 118    goldeen      6    150              64
    ## 119    seaking 119    seaking     13    390             158
    ## 120     staryu 120     staryu      8    345              68
    ## 121    starmie 121    starmie     11    800             182
    ## 122    mr-mime 122    mr-mime     13    545             161
    ## 123    scyther 123    scyther     15    560             100
    ## 124       jynx 124       jynx     14    406             159
    ## 125 electabuzz 125 electabuzz     11    300             172
    ## 126     magmar 126     magmar     13    445             173
    ## 127     pinsir 127     pinsir     15    550             175
    ## 128     tauros 128     tauros     14    884             172
    ## 129   magikarp 129   magikarp      9    100              40
    ## 130   gyarados 130   gyarados     65   2350             189
    ## 131     lapras 131     lapras     25   2200             187
    ## 132      ditto 132      ditto      3     40             101
    ## 133      eevee 133      eevee      3     65              65
    ## 134   vaporeon 134   vaporeon     10    290             184
    ## 135    jolteon 135    jolteon      8    245             184
    ## 136    flareon 136    flareon      9    250             184
    ## 137    porygon 137    porygon      8    365              79
    ## 138    omanyte 138    omanyte      4     75              71
    ## 139    omastar 139    omastar     10    350             173
    ## 140     kabuto 140     kabuto      5    115              71
    ## 141   kabutops 141   kabutops     13    405             173
    ## 142 aerodactyl 142 aerodactyl     18    590             180
    ## 143    snorlax 143    snorlax     21   4600             189
    ## 144   articuno 144   articuno     17    554             261
    ## 145     zapdos 145     zapdos     16    526             261
    ## 146    moltres 146    moltres     20    600             261
    ## 147    dratini 147    dratini     18     33              60
    ## 148  dragonair 148  dragonair     40    165             147
    ## 149  dragonite 149  dragonite     22   2100             270
    ## 150     mewtwo 150     mewtwo     20   1220             306
    ## 151        mew 151        mew      4     40             270
    ## 152  chikorita 152  chikorita      9     64              64
    ## 153    bayleef 153    bayleef     12    158             142
    ## 154   meganium 154   meganium     18   1005             236
    ## 155  cyndaquil 155  cyndaquil      5     79              62
    ## 156    quilava 156    quilava      9    190             142
    ## 157 typhlosion 157 typhlosion     17    795             240
    ## 158   totodile 158   totodile      6     95              63
    ## 159   croconaw 159   croconaw     11    250             142
    ## 160 feraligatr 160 feraligatr     23    888             239
    ## 161    sentret 161    sentret      8     60              43
    ## 162     furret 162     furret     18    325             145
    ## 163   hoothoot 163   hoothoot      7    212              52
    ## 164    noctowl 164    noctowl     16    408             158
    ## 165     ledyba 165     ledyba     10    108              53
    ## 166     ledian 166     ledian     14    356             137
    ##  [ reached 'max' / getOption("max.print") -- omitted 952 rows ]

## Exploring Data
