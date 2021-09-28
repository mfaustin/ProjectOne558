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

``` r
getOneEvolveData<-function(queryURL){
  queryResult<-fromJSON(queryURL)
  #print(queryResult)
  #print(queryResult$chain$species$name)
  stageOne<-queryResult$chain$species$name
  stageTwo<-queryResult[["chain"]][["evolves_to"]][["species"]][["name"]]
  stageThree<-queryResult[["chain"]][["evolves_to"]][["evolves_to"]][[1]][["species"]][["name"]] 
  if (is.null(stageTwo)){
    stageTwo<-"None"
  }
  if (is.null(stageThree)){
    stageThree<-"None"
  }
  #print(stageThree)
  
  localDF<-data.frame(stageOne,stageTwo,stageThree)
  return(localDF)
}
```

``` r
getAllEvolveSeries<-function(){
  
  metaEvolve<-fromJSON("https://pokeapi.co/api/v2/evolution-chain/?limit=600")
  
  allEvolve<-data.frame()
  for (loopURL in metaEvolve$results$url) {
    #print(loopURL)
    allEvolve<-rbind(allEvolve,getOneEvolveData(loopURL))
    
  } 
  
  return(allEvolve)
}

resultsEvolve<-getAllEvolveSeries()
resultsEvolve
```

    ##       stageOne   stageTwo stageThree
    ## 1    bulbasaur    ivysaur   venusaur
    ## 2   charmander charmeleon  charizard
    ## 3     squirtle  wartortle  blastoise
    ## 4     caterpie    metapod butterfree
    ## 5       weedle     kakuna   beedrill
    ## 6       pidgey  pidgeotto    pidgeot
    ## 7      rattata   raticate       None
    ## 8      spearow     fearow       None
    ## 9        ekans      arbok       None
    ## 10       pichu    pikachu     raichu
    ## 11   sandshrew  sandslash       None
    ## 12   nidoran-f   nidorina  nidoqueen
    ## 13   nidoran-m   nidorino   nidoking
    ## 14      cleffa   clefairy   clefable
    ## 15      vulpix  ninetales       None
    ## 16   igglybuff jigglypuff wigglytuff
    ## 17       zubat     golbat     crobat
    ## 18      oddish      gloom  vileplume
    ## 19      oddish      gloom  bellossom
    ## 20       paras   parasect       None
    ## 21     venonat   venomoth       None
    ## 22     diglett    dugtrio       None
    ## 23      meowth    persian       None
    ## 24      meowth perrserker       None
    ## 25     psyduck    golduck       None
    ## 26      mankey   primeape       None
    ## 27   growlithe   arcanine       None
    ## 28     poliwag  poliwhirl  poliwrath
    ## 29     poliwag  poliwhirl   politoed
    ## 30        abra    kadabra   alakazam
    ## 31      machop    machoke    machamp
    ## 32  bellsprout weepinbell victreebel
    ## 33   tentacool tentacruel       None
    ## 34     geodude   graveler      golem
    ## 35      ponyta   rapidash       None
    ## 36    slowpoke    slowbro       None
    ## 37    slowpoke   slowking       None
    ## 38   magnemite   magneton  magnezone
    ## 39   farfetchd  sirfetchd       None
    ## 40       doduo     dodrio       None
    ## 41        seel    dewgong       None
    ## 42      grimer        muk       None
    ## 43    shellder   cloyster       None
    ## 44      gastly    haunter     gengar
    ## 45        onix    steelix       None
    ## 46     drowzee      hypno       None
    ## 47      krabby    kingler       None
    ## 48     voltorb  electrode       None
    ## 49   exeggcute  exeggutor       None
    ## 50      cubone    marowak       None
    ## 51     tyrogue  hitmonlee       None
    ## 52     tyrogue hitmonchan       None
    ## 53     tyrogue  hitmontop       None
    ## 54   lickitung lickilicky       None
    ## 55     koffing    weezing       None
    ## 56     rhyhorn     rhydon  rhyperior
    ## 57     happiny    chansey    blissey
    ## 58     tangela  tangrowth       None
    ## 59  kangaskhan       None       None
    ## 60      horsea     seadra    kingdra
    ## 61     goldeen    seaking       None
    ## 62      staryu    starmie       None
    ## 63     mime-jr    mr-mime    mr-rime
    ## 64     scyther     scizor       None
    ## 65    smoochum       jynx       None
    ## 66      elekid electabuzz electivire
    ## 67       magby     magmar  magmortar
    ## 68      pinsir       None       None
    ## 69      tauros       None       None
    ## 70    magikarp   gyarados       None
    ## 71      lapras       None       None
    ## 72       ditto       None       None
    ## 73       eevee   vaporeon       None
    ## 74       eevee    jolteon       None
    ## 75       eevee    flareon       None
    ## 76       eevee     espeon       None
    ## 77       eevee    umbreon       None
    ## 78       eevee    leafeon       None
    ## 79       eevee    glaceon       None
    ## 80       eevee    sylveon       None
    ## 81     porygon   porygon2  porygon-z
    ## 82     omanyte    omastar       None
    ## 83      kabuto   kabutops       None
    ## 84  aerodactyl       None       None
    ## 85    munchlax    snorlax       None
    ## 86    articuno       None       None
    ## 87      zapdos       None       None
    ## 88     moltres       None       None
    ## 89     dratini  dragonair  dragonite
    ## 90      mewtwo       None       None
    ## 91         mew       None       None
    ## 92   chikorita    bayleef   meganium
    ## 93   cyndaquil    quilava typhlosion
    ## 94    totodile   croconaw feraligatr
    ## 95     sentret     furret       None
    ## 96    hoothoot    noctowl       None
    ## 97      ledyba     ledian       None
    ## 98    spinarak    ariados       None
    ## 99    chinchou    lanturn       None
    ## 100     togepi    togetic   togekiss
    ## 101       natu       xatu       None
    ## 102     mareep    flaaffy   ampharos
    ## 103    azurill     marill  azumarill
    ## 104     bonsly  sudowoodo       None
    ## 105     hoppip   skiploom   jumpluff
    ## 106      aipom    ambipom       None
    ## 107    sunkern   sunflora       None
    ## 108      yanma    yanmega       None
    ## 109     wooper   quagsire       None
    ## 110    murkrow  honchkrow       None
    ## 111 misdreavus  mismagius       None
    ## 112      unown       None       None
    ## 113     wynaut  wobbuffet       None
    ## 114  girafarig       None       None
    ## 115     pineco forretress       None
    ## 116  dunsparce       None       None
    ## 117     gligar    gliscor       None
    ## 118   snubbull   granbull       None
    ## 119   qwilfish       None       None
    ## 120    shuckle       None       None
    ## 121  heracross       None       None
    ## 122    sneasel    weavile       None
    ## 123  teddiursa   ursaring       None
    ## 124     slugma   magcargo       None
    ## 125     swinub  piloswine  mamoswine
    ## 126    corsola    cursola       None
    ## 127   remoraid  octillery       None
    ## 128   delibird       None       None
    ## 129    mantyke    mantine       None
    ## 130   skarmory       None       None
    ## 131   houndour   houndoom       None
    ## 132     phanpy    donphan       None
    ## 133   stantler       None       None
    ## 134   smeargle       None       None
    ## 135    miltank       None       None
    ## 136     raikou       None       None
    ## 137      entei       None       None
    ## 138    suicune       None       None
    ## 139   larvitar    pupitar  tyranitar
    ## 140      lugia       None       None
    ## 141      ho-oh       None       None
    ## 142     celebi       None       None
    ## 143    treecko    grovyle   sceptile
    ## 144    torchic  combusken   blaziken
    ## 145     mudkip  marshtomp   swampert
    ## 146  poochyena  mightyena       None
    ## 147  zigzagoon    linoone  obstagoon
    ## 148    wurmple    silcoon  beautifly
    ## 149    wurmple    cascoon  beautifly
    ## 150      lotad     lombre   ludicolo
    ## 151     seedot    nuzleaf    shiftry
    ## 152    taillow    swellow       None
    ## 153    wingull   pelipper       None
    ## 154      ralts     kirlia  gardevoir
    ## 155      ralts     kirlia    gallade
    ## 156    surskit masquerain       None
    ## 157  shroomish    breloom       None
    ## 158    slakoth   vigoroth    slaking
    ## 159    nincada    ninjask       None
    ## 160    nincada   shedinja       None
    ## 161    whismur    loudred    exploud
    ## 162   makuhita   hariyama       None
    ## 163   nosepass  probopass       None
    ## 164     skitty   delcatty       None
    ## 165    sableye       None       None
    ## 166     mawile       None       None
    ## 167       aron     lairon     aggron
    ## 168   meditite   medicham       None
    ## 169  electrike  manectric       None
    ## 170     plusle       None       None
    ## 171      minun       None       None
    ## 172    volbeat       None       None
    ## 173   illumise       None       None
    ## 174      budew    roselia   roserade
    ## 175     gulpin     swalot       None
    ## 176   carvanha   sharpedo       None
    ## 177    wailmer    wailord       None
    ## 178      numel   camerupt       None
    ## 179    torkoal       None       None
    ## 180     spoink    grumpig       None
    ## 181     spinda       None       None
    ## 182   trapinch    vibrava     flygon
    ## 183     cacnea   cacturne       None
    ## 184     swablu    altaria       None
    ## 185   zangoose       None       None
    ## 186    seviper       None       None
    ## 187   lunatone       None       None
    ## 188    solrock       None       None
    ## 189   barboach   whiscash       None
    ## 190   corphish  crawdaunt       None
    ## 191     baltoy    claydol       None
    ## 192     lileep    cradily       None
    ## 193    anorith    armaldo       None
    ## 194     feebas    milotic       None
    ## 195   castform       None       None
    ## 196    kecleon       None       None
    ## 197    shuppet    banette       None
    ## 198    duskull   dusclops   dusknoir
    ## 199    tropius       None       None
    ## 200  chingling   chimecho       None
    ## 201      absol       None       None
    ## 202    snorunt     glalie       None
    ## 203    snorunt   froslass       None
    ## 204     spheal     sealeo    walrein
    ## 205   clamperl    huntail       None
    ## 206   clamperl   gorebyss       None
    ## 207  relicanth       None       None
    ## 208    luvdisc       None       None
    ## 209      bagon    shelgon  salamence
    ## 210     beldum     metang  metagross
    ## 211   regirock       None       None
    ## 212     regice       None       None
    ## 213  registeel       None       None
    ## 214     latias       None       None
    ## 215     latios       None       None
    ## 216     kyogre       None       None
    ## 217    groudon       None       None
    ## 218   rayquaza       None       None
    ## 219    jirachi       None       None
    ## 220     deoxys       None       None
    ## 221    turtwig     grotle   torterra
    ## 222   chimchar   monferno  infernape
    ## 223     piplup   prinplup   empoleon
    ## 224     starly   staravia  staraptor
    ## 225     bidoof    bibarel       None
    ## 226  kricketot kricketune       None
    ## 227      shinx      luxio     luxray
    ## 228   cranidos  rampardos       None
    ## 229   shieldon  bastiodon       None
    ## 230      burmy   wormadam       None
    ## 231      burmy     mothim       None
    ## 232     combee  vespiquen       None
    ## 233  pachirisu       None       None
    ## 234     buizel   floatzel       None
    ## 235    cherubi    cherrim       None
    ## 236    shellos  gastrodon       None
    ## 237   drifloon   drifblim       None
    ## 238    buneary    lopunny       None
    ## 239    glameow    purugly       None
    ## 240     stunky   skuntank       None
    ## 241    bronzor   bronzong       None
    ## 242     chatot       None       None
    ## 243  spiritomb       None       None
    ## 244      gible     gabite   garchomp
    ## 245      riolu    lucario       None
    ## 246 hippopotas  hippowdon       None
    ## 247    skorupi    drapion       None
    ## 248   croagunk  toxicroak       None
    ## 249  carnivine       None       None
    ## 250    finneon   lumineon       None
    ## 251     snover  abomasnow       None
    ## 252      rotom       None       None
    ## 253       uxie       None       None
    ## 254    mesprit       None       None
    ## 255      azelf       None       None
    ## 256     dialga       None       None
    ## 257     palkia       None       None
    ## 258    heatran       None       None
    ## 259  regigigas       None       None
    ## 260   giratina       None       None
    ## 261  cresselia       None       None
    ## 262     phione    manaphy       None
    ## 263    darkrai       None       None
    ## 264    shaymin       None       None
    ## 265     arceus       None       None
    ## 266    victini       None       None
    ## 267      snivy    servine  serperior
    ## 268      tepig    pignite     emboar
    ## 269   oshawott     dewott   samurott
    ## 270     patrat    watchog       None
    ## 271   lillipup    herdier  stoutland
    ## 272   purrloin    liepard       None
    ## 273    pansage   simisage       None
    ## 274    pansear   simisear       None
    ## 275    panpour   simipour       None
    ## 276      munna   musharna       None
    ## 277     pidove  tranquill   unfezant
    ## 278    blitzle  zebstrika       None
    ## 279 roggenrola    boldore   gigalith
    ## 280     woobat    swoobat       None
    ## 281    drilbur  excadrill       None
    ## 282     audino       None       None
    ## 283    timburr    gurdurr conkeldurr
    ## 284    tympole  palpitoad seismitoad
    ## 285      throh       None       None
    ## 286       sawk       None       None
    ## 287   sewaddle   swadloon   leavanny
    ## 288   venipede whirlipede  scolipede
    ## 289   cottonee whimsicott       None
    ## 290    petilil  lilligant       None
    ## 291   basculin       None       None
    ## 292    sandile   krokorok krookodile
    ## 293   darumaka darmanitan       None
    ## 294   maractus       None       None
    ## 295    dwebble    crustle       None
    ## 296    scraggy    scrafty       None
    ## 297   sigilyph       None       None
    ## 298     yamask cofagrigus       None
    ## 299     yamask  runerigus       None
    ## 300   tirtouga carracosta       None
    ## 301     archen   archeops       None
    ## 302   trubbish   garbodor       None
    ## 303      zorua    zoroark       None
    ## 304   minccino   cinccino       None
    ## 305    gothita  gothorita gothitelle
    ## 306    solosis    duosion  reuniclus
    ## 307   ducklett     swanna       None
    ## 308  vanillite  vanillish  vanilluxe
    ## 309   deerling   sawsbuck       None
    ## 310     emolga       None       None
    ## 311 karrablast escavalier       None
    ## 312    foongus  amoonguss       None
    ## 313   frillish  jellicent       None
    ## 314  alomomola       None       None
    ## 315     joltik galvantula       None
    ## 316  ferroseed ferrothorn       None
    ## 317      klink      klang  klinklang
    ## 318     tynamo  eelektrik eelektross
    ## 319     elgyem   beheeyem       None
    ## 320    litwick    lampent chandelure
    ## 321       axew    fraxure    haxorus
    ## 322    cubchoo    beartic       None
    ## 323  cryogonal       None       None
    ## 324    shelmet   accelgor       None
    ## 325   stunfisk       None       None
    ## 326    mienfoo   mienshao       None
    ## 327  druddigon       None       None
    ## 328     golett     golurk       None
    ## 329   pawniard    bisharp       None
    ## 330 bouffalant       None       None
    ## 331    rufflet   braviary       None
    ## 332    vullaby  mandibuzz       None
    ## 333    heatmor       None       None
    ##  [ reached 'max' / getOption("max.print") -- omitted 156 rows ]

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
