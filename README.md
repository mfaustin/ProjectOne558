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

`knitr`

``` r
   library(tidyverse)
   library(jsonlite)
   library(knitr)
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

<div class="kable-table">

| name                       | url                                        |    ID |
|:---------------------------|:-------------------------------------------|------:|
| abomasnow                  | <https://pokeapi.co/api/v2/pokemon/460/>   |   460 |
| abomasnow-mega             | <https://pokeapi.co/api/v2/pokemon/10060/> | 10060 |
| abra                       | <https://pokeapi.co/api/v2/pokemon/63/>    |    63 |
| absol                      | <https://pokeapi.co/api/v2/pokemon/359/>   |   359 |
| absol-mega                 | <https://pokeapi.co/api/v2/pokemon/10057/> | 10057 |
| accelgor                   | <https://pokeapi.co/api/v2/pokemon/617/>   |   617 |
| aegislash-blade            | <https://pokeapi.co/api/v2/pokemon/10026/> | 10026 |
| aegislash-shield           | <https://pokeapi.co/api/v2/pokemon/681/>   |   681 |
| aerodactyl                 | <https://pokeapi.co/api/v2/pokemon/142/>   |   142 |
| aerodactyl-mega            | <https://pokeapi.co/api/v2/pokemon/10042/> | 10042 |
| aggron                     | <https://pokeapi.co/api/v2/pokemon/306/>   |   306 |
| aggron-mega                | <https://pokeapi.co/api/v2/pokemon/10053/> | 10053 |
| aipom                      | <https://pokeapi.co/api/v2/pokemon/190/>   |   190 |
| alakazam                   | <https://pokeapi.co/api/v2/pokemon/65/>    |    65 |
| alakazam-mega              | <https://pokeapi.co/api/v2/pokemon/10037/> | 10037 |
| alcremie                   | <https://pokeapi.co/api/v2/pokemon/869/>   |   869 |
| alcremie-gmax              | <https://pokeapi.co/api/v2/pokemon/10214/> | 10214 |
| alomomola                  | <https://pokeapi.co/api/v2/pokemon/594/>   |   594 |
| altaria                    | <https://pokeapi.co/api/v2/pokemon/334/>   |   334 |
| altaria-mega               | <https://pokeapi.co/api/v2/pokemon/10067/> | 10067 |
| amaura                     | <https://pokeapi.co/api/v2/pokemon/698/>   |   698 |
| ambipom                    | <https://pokeapi.co/api/v2/pokemon/424/>   |   424 |
| amoonguss                  | <https://pokeapi.co/api/v2/pokemon/591/>   |   591 |
| ampharos                   | <https://pokeapi.co/api/v2/pokemon/181/>   |   181 |
| ampharos-mega              | <https://pokeapi.co/api/v2/pokemon/10045/> | 10045 |
| anorith                    | <https://pokeapi.co/api/v2/pokemon/347/>   |   347 |
| appletun                   | <https://pokeapi.co/api/v2/pokemon/842/>   |   842 |
| appletun-gmax              | <https://pokeapi.co/api/v2/pokemon/10208/> | 10208 |
| applin                     | <https://pokeapi.co/api/v2/pokemon/840/>   |   840 |
| araquanid                  | <https://pokeapi.co/api/v2/pokemon/752/>   |   752 |
| araquanid-totem            | <https://pokeapi.co/api/v2/pokemon/10153/> | 10153 |
| arbok                      | <https://pokeapi.co/api/v2/pokemon/24/>    |    24 |
| arcanine                   | <https://pokeapi.co/api/v2/pokemon/59/>    |    59 |
| arceus                     | <https://pokeapi.co/api/v2/pokemon/493/>   |   493 |
| archen                     | <https://pokeapi.co/api/v2/pokemon/566/>   |   566 |
| archeops                   | <https://pokeapi.co/api/v2/pokemon/567/>   |   567 |
| arctovish                  | <https://pokeapi.co/api/v2/pokemon/883/>   |   883 |
| arctozolt                  | <https://pokeapi.co/api/v2/pokemon/881/>   |   881 |
| ariados                    | <https://pokeapi.co/api/v2/pokemon/168/>   |   168 |
| armaldo                    | <https://pokeapi.co/api/v2/pokemon/348/>   |   348 |
| aromatisse                 | <https://pokeapi.co/api/v2/pokemon/683/>   |   683 |
| aron                       | <https://pokeapi.co/api/v2/pokemon/304/>   |   304 |
| arrokuda                   | <https://pokeapi.co/api/v2/pokemon/846/>   |   846 |
| articuno                   | <https://pokeapi.co/api/v2/pokemon/144/>   |   144 |
| articuno-galar             | <https://pokeapi.co/api/v2/pokemon/10166/> | 10166 |
| audino                     | <https://pokeapi.co/api/v2/pokemon/531/>   |   531 |
| audino-mega                | <https://pokeapi.co/api/v2/pokemon/10069/> | 10069 |
| aurorus                    | <https://pokeapi.co/api/v2/pokemon/699/>   |   699 |
| avalugg                    | <https://pokeapi.co/api/v2/pokemon/713/>   |   713 |
| axew                       | <https://pokeapi.co/api/v2/pokemon/610/>   |   610 |
| azelf                      | <https://pokeapi.co/api/v2/pokemon/482/>   |   482 |
| azumarill                  | <https://pokeapi.co/api/v2/pokemon/184/>   |   184 |
| azurill                    | <https://pokeapi.co/api/v2/pokemon/298/>   |   298 |
| bagon                      | <https://pokeapi.co/api/v2/pokemon/371/>   |   371 |
| baltoy                     | <https://pokeapi.co/api/v2/pokemon/343/>   |   343 |
| banette                    | <https://pokeapi.co/api/v2/pokemon/354/>   |   354 |
| banette-mega               | <https://pokeapi.co/api/v2/pokemon/10056/> | 10056 |
| barbaracle                 | <https://pokeapi.co/api/v2/pokemon/689/>   |   689 |
| barboach                   | <https://pokeapi.co/api/v2/pokemon/339/>   |   339 |
| barraskewda                | <https://pokeapi.co/api/v2/pokemon/847/>   |   847 |
| basculin-blue-striped      | <https://pokeapi.co/api/v2/pokemon/10016/> | 10016 |
| basculin-red-striped       | <https://pokeapi.co/api/v2/pokemon/550/>   |   550 |
| bastiodon                  | <https://pokeapi.co/api/v2/pokemon/411/>   |   411 |
| bayleef                    | <https://pokeapi.co/api/v2/pokemon/153/>   |   153 |
| beartic                    | <https://pokeapi.co/api/v2/pokemon/614/>   |   614 |
| beautifly                  | <https://pokeapi.co/api/v2/pokemon/267/>   |   267 |
| beedrill                   | <https://pokeapi.co/api/v2/pokemon/15/>    |    15 |
| beedrill-mega              | <https://pokeapi.co/api/v2/pokemon/10090/> | 10090 |
| beheeyem                   | <https://pokeapi.co/api/v2/pokemon/606/>   |   606 |
| beldum                     | <https://pokeapi.co/api/v2/pokemon/374/>   |   374 |
| bellossom                  | <https://pokeapi.co/api/v2/pokemon/182/>   |   182 |
| bellsprout                 | <https://pokeapi.co/api/v2/pokemon/69/>    |    69 |
| bergmite                   | <https://pokeapi.co/api/v2/pokemon/712/>   |   712 |
| bewear                     | <https://pokeapi.co/api/v2/pokemon/760/>   |   760 |
| bibarel                    | <https://pokeapi.co/api/v2/pokemon/400/>   |   400 |
| bidoof                     | <https://pokeapi.co/api/v2/pokemon/399/>   |   399 |
| binacle                    | <https://pokeapi.co/api/v2/pokemon/688/>   |   688 |
| bisharp                    | <https://pokeapi.co/api/v2/pokemon/625/>   |   625 |
| blacephalon                | <https://pokeapi.co/api/v2/pokemon/806/>   |   806 |
| blastoise                  | <https://pokeapi.co/api/v2/pokemon/9/>     |     9 |
| blastoise-gmax             | <https://pokeapi.co/api/v2/pokemon/10188/> | 10188 |
| blastoise-mega             | <https://pokeapi.co/api/v2/pokemon/10036/> | 10036 |
| blaziken                   | <https://pokeapi.co/api/v2/pokemon/257/>   |   257 |
| blaziken-mega              | <https://pokeapi.co/api/v2/pokemon/10050/> | 10050 |
| blipbug                    | <https://pokeapi.co/api/v2/pokemon/824/>   |   824 |
| blissey                    | <https://pokeapi.co/api/v2/pokemon/242/>   |   242 |
| blitzle                    | <https://pokeapi.co/api/v2/pokemon/522/>   |   522 |
| boldore                    | <https://pokeapi.co/api/v2/pokemon/525/>   |   525 |
| boltund                    | <https://pokeapi.co/api/v2/pokemon/836/>   |   836 |
| bonsly                     | <https://pokeapi.co/api/v2/pokemon/438/>   |   438 |
| bouffalant                 | <https://pokeapi.co/api/v2/pokemon/626/>   |   626 |
| bounsweet                  | <https://pokeapi.co/api/v2/pokemon/761/>   |   761 |
| braixen                    | <https://pokeapi.co/api/v2/pokemon/654/>   |   654 |
| braviary                   | <https://pokeapi.co/api/v2/pokemon/628/>   |   628 |
| breloom                    | <https://pokeapi.co/api/v2/pokemon/286/>   |   286 |
| brionne                    | <https://pokeapi.co/api/v2/pokemon/729/>   |   729 |
| bronzong                   | <https://pokeapi.co/api/v2/pokemon/437/>   |   437 |
| bronzor                    | <https://pokeapi.co/api/v2/pokemon/436/>   |   436 |
| bruxish                    | <https://pokeapi.co/api/v2/pokemon/779/>   |   779 |
| budew                      | <https://pokeapi.co/api/v2/pokemon/406/>   |   406 |
| buizel                     | <https://pokeapi.co/api/v2/pokemon/418/>   |   418 |
| bulbasaur                  | <https://pokeapi.co/api/v2/pokemon/1/>     |     1 |
| buneary                    | <https://pokeapi.co/api/v2/pokemon/427/>   |   427 |
| bunnelby                   | <https://pokeapi.co/api/v2/pokemon/659/>   |   659 |
| burmy                      | <https://pokeapi.co/api/v2/pokemon/412/>   |   412 |
| butterfree                 | <https://pokeapi.co/api/v2/pokemon/12/>    |    12 |
| butterfree-gmax            | <https://pokeapi.co/api/v2/pokemon/10189/> | 10189 |
| buzzwole                   | <https://pokeapi.co/api/v2/pokemon/794/>   |   794 |
| cacnea                     | <https://pokeapi.co/api/v2/pokemon/331/>   |   331 |
| cacturne                   | <https://pokeapi.co/api/v2/pokemon/332/>   |   332 |
| calyrex                    | <https://pokeapi.co/api/v2/pokemon/898/>   |   898 |
| calyrex-ice-rider          | <https://pokeapi.co/api/v2/pokemon/10184/> | 10184 |
| calyrex-shadow-rider       | <https://pokeapi.co/api/v2/pokemon/10185/> | 10185 |
| camerupt                   | <https://pokeapi.co/api/v2/pokemon/323/>   |   323 |
| camerupt-mega              | <https://pokeapi.co/api/v2/pokemon/10087/> | 10087 |
| carbink                    | <https://pokeapi.co/api/v2/pokemon/703/>   |   703 |
| carkol                     | <https://pokeapi.co/api/v2/pokemon/838/>   |   838 |
| carnivine                  | <https://pokeapi.co/api/v2/pokemon/455/>   |   455 |
| carracosta                 | <https://pokeapi.co/api/v2/pokemon/565/>   |   565 |
| carvanha                   | <https://pokeapi.co/api/v2/pokemon/318/>   |   318 |
| cascoon                    | <https://pokeapi.co/api/v2/pokemon/268/>   |   268 |
| castform                   | <https://pokeapi.co/api/v2/pokemon/351/>   |   351 |
| castform-rainy             | <https://pokeapi.co/api/v2/pokemon/10014/> | 10014 |
| castform-snowy             | <https://pokeapi.co/api/v2/pokemon/10015/> | 10015 |
| castform-sunny             | <https://pokeapi.co/api/v2/pokemon/10013/> | 10013 |
| caterpie                   | <https://pokeapi.co/api/v2/pokemon/10/>    |    10 |
| celebi                     | <https://pokeapi.co/api/v2/pokemon/251/>   |   251 |
| celesteela                 | <https://pokeapi.co/api/v2/pokemon/797/>   |   797 |
| centiskorch                | <https://pokeapi.co/api/v2/pokemon/851/>   |   851 |
| centiskorch-gmax           | <https://pokeapi.co/api/v2/pokemon/10211/> | 10211 |
| chandelure                 | <https://pokeapi.co/api/v2/pokemon/609/>   |   609 |
| chansey                    | <https://pokeapi.co/api/v2/pokemon/113/>   |   113 |
| charizard                  | <https://pokeapi.co/api/v2/pokemon/6/>     |     6 |
| charizard-gmax             | <https://pokeapi.co/api/v2/pokemon/10187/> | 10187 |
| charizard-mega-x           | <https://pokeapi.co/api/v2/pokemon/10034/> | 10034 |
| charizard-mega-y           | <https://pokeapi.co/api/v2/pokemon/10035/> | 10035 |
| charjabug                  | <https://pokeapi.co/api/v2/pokemon/737/>   |   737 |
| charmander                 | <https://pokeapi.co/api/v2/pokemon/4/>     |     4 |
| charmeleon                 | <https://pokeapi.co/api/v2/pokemon/5/>     |     5 |
| chatot                     | <https://pokeapi.co/api/v2/pokemon/441/>   |   441 |
| cherrim                    | <https://pokeapi.co/api/v2/pokemon/421/>   |   421 |
| cherubi                    | <https://pokeapi.co/api/v2/pokemon/420/>   |   420 |
| chesnaught                 | <https://pokeapi.co/api/v2/pokemon/652/>   |   652 |
| chespin                    | <https://pokeapi.co/api/v2/pokemon/650/>   |   650 |
| chewtle                    | <https://pokeapi.co/api/v2/pokemon/833/>   |   833 |
| chikorita                  | <https://pokeapi.co/api/v2/pokemon/152/>   |   152 |
| chimchar                   | <https://pokeapi.co/api/v2/pokemon/390/>   |   390 |
| chimecho                   | <https://pokeapi.co/api/v2/pokemon/358/>   |   358 |
| chinchou                   | <https://pokeapi.co/api/v2/pokemon/170/>   |   170 |
| chingling                  | <https://pokeapi.co/api/v2/pokemon/433/>   |   433 |
| cinccino                   | <https://pokeapi.co/api/v2/pokemon/573/>   |   573 |
| cinderace                  | <https://pokeapi.co/api/v2/pokemon/815/>   |   815 |
| cinderace-gmax             | <https://pokeapi.co/api/v2/pokemon/10201/> | 10201 |
| clamperl                   | <https://pokeapi.co/api/v2/pokemon/366/>   |   366 |
| clauncher                  | <https://pokeapi.co/api/v2/pokemon/692/>   |   692 |
| clawitzer                  | <https://pokeapi.co/api/v2/pokemon/693/>   |   693 |
| claydol                    | <https://pokeapi.co/api/v2/pokemon/344/>   |   344 |
| clefable                   | <https://pokeapi.co/api/v2/pokemon/36/>    |    36 |
| clefairy                   | <https://pokeapi.co/api/v2/pokemon/35/>    |    35 |
| cleffa                     | <https://pokeapi.co/api/v2/pokemon/173/>   |   173 |
| clobbopus                  | <https://pokeapi.co/api/v2/pokemon/852/>   |   852 |
| cloyster                   | <https://pokeapi.co/api/v2/pokemon/91/>    |    91 |
| coalossal                  | <https://pokeapi.co/api/v2/pokemon/839/>   |   839 |
| coalossal-gmax             | <https://pokeapi.co/api/v2/pokemon/10206/> | 10206 |
| cobalion                   | <https://pokeapi.co/api/v2/pokemon/638/>   |   638 |
| cofagrigus                 | <https://pokeapi.co/api/v2/pokemon/563/>   |   563 |
| combee                     | <https://pokeapi.co/api/v2/pokemon/415/>   |   415 |
| combusken                  | <https://pokeapi.co/api/v2/pokemon/256/>   |   256 |
| comfey                     | <https://pokeapi.co/api/v2/pokemon/764/>   |   764 |
| conkeldurr                 | <https://pokeapi.co/api/v2/pokemon/534/>   |   534 |
| copperajah                 | <https://pokeapi.co/api/v2/pokemon/879/>   |   879 |
| copperajah-gmax            | <https://pokeapi.co/api/v2/pokemon/10215/> | 10215 |
| corphish                   | <https://pokeapi.co/api/v2/pokemon/341/>   |   341 |
| corsola                    | <https://pokeapi.co/api/v2/pokemon/222/>   |   222 |
| corsola-galar              | <https://pokeapi.co/api/v2/pokemon/10170/> | 10170 |
| corviknight                | <https://pokeapi.co/api/v2/pokemon/823/>   |   823 |
| corviknight-gmax           | <https://pokeapi.co/api/v2/pokemon/10203/> | 10203 |
| corvisquire                | <https://pokeapi.co/api/v2/pokemon/822/>   |   822 |
| cosmoem                    | <https://pokeapi.co/api/v2/pokemon/790/>   |   790 |
| cosmog                     | <https://pokeapi.co/api/v2/pokemon/789/>   |   789 |
| cottonee                   | <https://pokeapi.co/api/v2/pokemon/546/>   |   546 |
| crabominable               | <https://pokeapi.co/api/v2/pokemon/740/>   |   740 |
| crabrawler                 | <https://pokeapi.co/api/v2/pokemon/739/>   |   739 |
| cradily                    | <https://pokeapi.co/api/v2/pokemon/346/>   |   346 |
| cramorant                  | <https://pokeapi.co/api/v2/pokemon/845/>   |   845 |
| cranidos                   | <https://pokeapi.co/api/v2/pokemon/408/>   |   408 |
| crawdaunt                  | <https://pokeapi.co/api/v2/pokemon/342/>   |   342 |
| cresselia                  | <https://pokeapi.co/api/v2/pokemon/488/>   |   488 |
| croagunk                   | <https://pokeapi.co/api/v2/pokemon/453/>   |   453 |
| crobat                     | <https://pokeapi.co/api/v2/pokemon/169/>   |   169 |
| croconaw                   | <https://pokeapi.co/api/v2/pokemon/159/>   |   159 |
| crustle                    | <https://pokeapi.co/api/v2/pokemon/558/>   |   558 |
| cryogonal                  | <https://pokeapi.co/api/v2/pokemon/615/>   |   615 |
| cubchoo                    | <https://pokeapi.co/api/v2/pokemon/613/>   |   613 |
| cubone                     | <https://pokeapi.co/api/v2/pokemon/104/>   |   104 |
| cufant                     | <https://pokeapi.co/api/v2/pokemon/878/>   |   878 |
| cursola                    | <https://pokeapi.co/api/v2/pokemon/864/>   |   864 |
| cutiefly                   | <https://pokeapi.co/api/v2/pokemon/742/>   |   742 |
| cyndaquil                  | <https://pokeapi.co/api/v2/pokemon/155/>   |   155 |
| darkrai                    | <https://pokeapi.co/api/v2/pokemon/491/>   |   491 |
| darmanitan-standard        | <https://pokeapi.co/api/v2/pokemon/555/>   |   555 |
| darmanitan-standard-galar  | <https://pokeapi.co/api/v2/pokemon/10174/> | 10174 |
| darmanitan-zen             | <https://pokeapi.co/api/v2/pokemon/10017/> | 10017 |
| darmanitan-zen-galar       | <https://pokeapi.co/api/v2/pokemon/10175/> | 10175 |
| dartrix                    | <https://pokeapi.co/api/v2/pokemon/723/>   |   723 |
| darumaka                   | <https://pokeapi.co/api/v2/pokemon/554/>   |   554 |
| darumaka-galar             | <https://pokeapi.co/api/v2/pokemon/10173/> | 10173 |
| decidueye                  | <https://pokeapi.co/api/v2/pokemon/724/>   |   724 |
| dedenne                    | <https://pokeapi.co/api/v2/pokemon/702/>   |   702 |
| deerling                   | <https://pokeapi.co/api/v2/pokemon/585/>   |   585 |
| deino                      | <https://pokeapi.co/api/v2/pokemon/633/>   |   633 |
| delcatty                   | <https://pokeapi.co/api/v2/pokemon/301/>   |   301 |
| delibird                   | <https://pokeapi.co/api/v2/pokemon/225/>   |   225 |
| delphox                    | <https://pokeapi.co/api/v2/pokemon/655/>   |   655 |
| deoxys-attack              | <https://pokeapi.co/api/v2/pokemon/10001/> | 10001 |
| deoxys-defense             | <https://pokeapi.co/api/v2/pokemon/10002/> | 10002 |
| deoxys-normal              | <https://pokeapi.co/api/v2/pokemon/386/>   |   386 |
| deoxys-speed               | <https://pokeapi.co/api/v2/pokemon/10003/> | 10003 |
| dewgong                    | <https://pokeapi.co/api/v2/pokemon/87/>    |    87 |
| dewott                     | <https://pokeapi.co/api/v2/pokemon/502/>   |   502 |
| dewpider                   | <https://pokeapi.co/api/v2/pokemon/751/>   |   751 |
| dhelmise                   | <https://pokeapi.co/api/v2/pokemon/781/>   |   781 |
| dialga                     | <https://pokeapi.co/api/v2/pokemon/483/>   |   483 |
| diancie                    | <https://pokeapi.co/api/v2/pokemon/719/>   |   719 |
| diancie-mega               | <https://pokeapi.co/api/v2/pokemon/10075/> | 10075 |
| diggersby                  | <https://pokeapi.co/api/v2/pokemon/660/>   |   660 |
| diglett                    | <https://pokeapi.co/api/v2/pokemon/50/>    |    50 |
| diglett-alola              | <https://pokeapi.co/api/v2/pokemon/10105/> | 10105 |
| ditto                      | <https://pokeapi.co/api/v2/pokemon/132/>   |   132 |
| dodrio                     | <https://pokeapi.co/api/v2/pokemon/85/>    |    85 |
| doduo                      | <https://pokeapi.co/api/v2/pokemon/84/>    |    84 |
| donphan                    | <https://pokeapi.co/api/v2/pokemon/232/>   |   232 |
| dottler                    | <https://pokeapi.co/api/v2/pokemon/825/>   |   825 |
| doublade                   | <https://pokeapi.co/api/v2/pokemon/680/>   |   680 |
| dracovish                  | <https://pokeapi.co/api/v2/pokemon/882/>   |   882 |
| dracozolt                  | <https://pokeapi.co/api/v2/pokemon/880/>   |   880 |
| dragalge                   | <https://pokeapi.co/api/v2/pokemon/691/>   |   691 |
| dragapult                  | <https://pokeapi.co/api/v2/pokemon/887/>   |   887 |
| dragonair                  | <https://pokeapi.co/api/v2/pokemon/148/>   |   148 |
| dragonite                  | <https://pokeapi.co/api/v2/pokemon/149/>   |   149 |
| drakloak                   | <https://pokeapi.co/api/v2/pokemon/886/>   |   886 |
| drampa                     | <https://pokeapi.co/api/v2/pokemon/780/>   |   780 |
| drapion                    | <https://pokeapi.co/api/v2/pokemon/452/>   |   452 |
| dratini                    | <https://pokeapi.co/api/v2/pokemon/147/>   |   147 |
| drednaw                    | <https://pokeapi.co/api/v2/pokemon/834/>   |   834 |
| drednaw-gmax               | <https://pokeapi.co/api/v2/pokemon/10205/> | 10205 |
| dreepy                     | <https://pokeapi.co/api/v2/pokemon/885/>   |   885 |
| drifblim                   | <https://pokeapi.co/api/v2/pokemon/426/>   |   426 |
| drifloon                   | <https://pokeapi.co/api/v2/pokemon/425/>   |   425 |
| drilbur                    | <https://pokeapi.co/api/v2/pokemon/529/>   |   529 |
| drizzile                   | <https://pokeapi.co/api/v2/pokemon/817/>   |   817 |
| drowzee                    | <https://pokeapi.co/api/v2/pokemon/96/>    |    96 |
| druddigon                  | <https://pokeapi.co/api/v2/pokemon/621/>   |   621 |
| dubwool                    | <https://pokeapi.co/api/v2/pokemon/832/>   |   832 |
| ducklett                   | <https://pokeapi.co/api/v2/pokemon/580/>   |   580 |
| dugtrio                    | <https://pokeapi.co/api/v2/pokemon/51/>    |    51 |
| dugtrio-alola              | <https://pokeapi.co/api/v2/pokemon/10106/> | 10106 |
| dunsparce                  | <https://pokeapi.co/api/v2/pokemon/206/>   |   206 |
| duosion                    | <https://pokeapi.co/api/v2/pokemon/578/>   |   578 |
| duraludon                  | <https://pokeapi.co/api/v2/pokemon/884/>   |   884 |
| duraludon-gmax             | <https://pokeapi.co/api/v2/pokemon/10216/> | 10216 |
| durant                     | <https://pokeapi.co/api/v2/pokemon/632/>   |   632 |
| dusclops                   | <https://pokeapi.co/api/v2/pokemon/356/>   |   356 |
| dusknoir                   | <https://pokeapi.co/api/v2/pokemon/477/>   |   477 |
| duskull                    | <https://pokeapi.co/api/v2/pokemon/355/>   |   355 |
| dustox                     | <https://pokeapi.co/api/v2/pokemon/269/>   |   269 |
| dwebble                    | <https://pokeapi.co/api/v2/pokemon/557/>   |   557 |
| eelektrik                  | <https://pokeapi.co/api/v2/pokemon/603/>   |   603 |
| eelektross                 | <https://pokeapi.co/api/v2/pokemon/604/>   |   604 |
| eevee                      | <https://pokeapi.co/api/v2/pokemon/133/>   |   133 |
| eevee-gmax                 | <https://pokeapi.co/api/v2/pokemon/10196/> | 10196 |
| eiscue-ice                 | <https://pokeapi.co/api/v2/pokemon/875/>   |   875 |
| eiscue-noice               | <https://pokeapi.co/api/v2/pokemon/10179/> | 10179 |
| ekans                      | <https://pokeapi.co/api/v2/pokemon/23/>    |    23 |
| eldegoss                   | <https://pokeapi.co/api/v2/pokemon/830/>   |   830 |
| electabuzz                 | <https://pokeapi.co/api/v2/pokemon/125/>   |   125 |
| electivire                 | <https://pokeapi.co/api/v2/pokemon/466/>   |   466 |
| electrike                  | <https://pokeapi.co/api/v2/pokemon/309/>   |   309 |
| electrode                  | <https://pokeapi.co/api/v2/pokemon/101/>   |   101 |
| elekid                     | <https://pokeapi.co/api/v2/pokemon/239/>   |   239 |
| elgyem                     | <https://pokeapi.co/api/v2/pokemon/605/>   |   605 |
| emboar                     | <https://pokeapi.co/api/v2/pokemon/500/>   |   500 |
| emolga                     | <https://pokeapi.co/api/v2/pokemon/587/>   |   587 |
| empoleon                   | <https://pokeapi.co/api/v2/pokemon/395/>   |   395 |
| entei                      | <https://pokeapi.co/api/v2/pokemon/244/>   |   244 |
| escavalier                 | <https://pokeapi.co/api/v2/pokemon/589/>   |   589 |
| espeon                     | <https://pokeapi.co/api/v2/pokemon/196/>   |   196 |
| espurr                     | <https://pokeapi.co/api/v2/pokemon/677/>   |   677 |
| eternatus                  | <https://pokeapi.co/api/v2/pokemon/890/>   |   890 |
| eternatus-eternamax        | <https://pokeapi.co/api/v2/pokemon/10217/> | 10217 |
| excadrill                  | <https://pokeapi.co/api/v2/pokemon/530/>   |   530 |
| exeggcute                  | <https://pokeapi.co/api/v2/pokemon/102/>   |   102 |
| exeggutor                  | <https://pokeapi.co/api/v2/pokemon/103/>   |   103 |
| exeggutor-alola            | <https://pokeapi.co/api/v2/pokemon/10114/> | 10114 |
| exploud                    | <https://pokeapi.co/api/v2/pokemon/295/>   |   295 |
| falinks                    | <https://pokeapi.co/api/v2/pokemon/870/>   |   870 |
| farfetchd                  | <https://pokeapi.co/api/v2/pokemon/83/>    |    83 |
| farfetchd-galar            | <https://pokeapi.co/api/v2/pokemon/10163/> | 10163 |
| fearow                     | <https://pokeapi.co/api/v2/pokemon/22/>    |    22 |
| feebas                     | <https://pokeapi.co/api/v2/pokemon/349/>   |   349 |
| fennekin                   | <https://pokeapi.co/api/v2/pokemon/653/>   |   653 |
| feraligatr                 | <https://pokeapi.co/api/v2/pokemon/160/>   |   160 |
| ferroseed                  | <https://pokeapi.co/api/v2/pokemon/597/>   |   597 |
| ferrothorn                 | <https://pokeapi.co/api/v2/pokemon/598/>   |   598 |
| finneon                    | <https://pokeapi.co/api/v2/pokemon/456/>   |   456 |
| flaaffy                    | <https://pokeapi.co/api/v2/pokemon/180/>   |   180 |
| flabebe                    | <https://pokeapi.co/api/v2/pokemon/669/>   |   669 |
| flapple                    | <https://pokeapi.co/api/v2/pokemon/841/>   |   841 |
| flapple-gmax               | <https://pokeapi.co/api/v2/pokemon/10207/> | 10207 |
| flareon                    | <https://pokeapi.co/api/v2/pokemon/136/>   |   136 |
| fletchinder                | <https://pokeapi.co/api/v2/pokemon/662/>   |   662 |
| fletchling                 | <https://pokeapi.co/api/v2/pokemon/661/>   |   661 |
| floatzel                   | <https://pokeapi.co/api/v2/pokemon/419/>   |   419 |
| floette                    | <https://pokeapi.co/api/v2/pokemon/670/>   |   670 |
| floette-eternal            | <https://pokeapi.co/api/v2/pokemon/10061/> | 10061 |
| florges                    | <https://pokeapi.co/api/v2/pokemon/671/>   |   671 |
| flygon                     | <https://pokeapi.co/api/v2/pokemon/330/>   |   330 |
| fomantis                   | <https://pokeapi.co/api/v2/pokemon/753/>   |   753 |
| foongus                    | <https://pokeapi.co/api/v2/pokemon/590/>   |   590 |
| forretress                 | <https://pokeapi.co/api/v2/pokemon/205/>   |   205 |
| fraxure                    | <https://pokeapi.co/api/v2/pokemon/611/>   |   611 |
| frillish                   | <https://pokeapi.co/api/v2/pokemon/592/>   |   592 |
| froakie                    | <https://pokeapi.co/api/v2/pokemon/656/>   |   656 |
| frogadier                  | <https://pokeapi.co/api/v2/pokemon/657/>   |   657 |
| froslass                   | <https://pokeapi.co/api/v2/pokemon/478/>   |   478 |
| frosmoth                   | <https://pokeapi.co/api/v2/pokemon/873/>   |   873 |
| furfrou                    | <https://pokeapi.co/api/v2/pokemon/676/>   |   676 |
| furret                     | <https://pokeapi.co/api/v2/pokemon/162/>   |   162 |
| gabite                     | <https://pokeapi.co/api/v2/pokemon/444/>   |   444 |
| gallade                    | <https://pokeapi.co/api/v2/pokemon/475/>   |   475 |
| gallade-mega               | <https://pokeapi.co/api/v2/pokemon/10068/> | 10068 |
| galvantula                 | <https://pokeapi.co/api/v2/pokemon/596/>   |   596 |
| garbodor                   | <https://pokeapi.co/api/v2/pokemon/569/>   |   569 |
| garbodor-gmax              | <https://pokeapi.co/api/v2/pokemon/10198/> | 10198 |
| garchomp                   | <https://pokeapi.co/api/v2/pokemon/445/>   |   445 |
| garchomp-mega              | <https://pokeapi.co/api/v2/pokemon/10058/> | 10058 |
| gardevoir                  | <https://pokeapi.co/api/v2/pokemon/282/>   |   282 |
| gardevoir-mega             | <https://pokeapi.co/api/v2/pokemon/10051/> | 10051 |
| gastly                     | <https://pokeapi.co/api/v2/pokemon/92/>    |    92 |
| gastrodon                  | <https://pokeapi.co/api/v2/pokemon/423/>   |   423 |
| genesect                   | <https://pokeapi.co/api/v2/pokemon/649/>   |   649 |
| gengar                     | <https://pokeapi.co/api/v2/pokemon/94/>    |    94 |
| gengar-gmax                | <https://pokeapi.co/api/v2/pokemon/10193/> | 10193 |
| gengar-mega                | <https://pokeapi.co/api/v2/pokemon/10038/> | 10038 |
| geodude                    | <https://pokeapi.co/api/v2/pokemon/74/>    |    74 |
| geodude-alola              | <https://pokeapi.co/api/v2/pokemon/10109/> | 10109 |
| gible                      | <https://pokeapi.co/api/v2/pokemon/443/>   |   443 |
| gigalith                   | <https://pokeapi.co/api/v2/pokemon/526/>   |   526 |
| girafarig                  | <https://pokeapi.co/api/v2/pokemon/203/>   |   203 |
| giratina-altered           | <https://pokeapi.co/api/v2/pokemon/487/>   |   487 |
| giratina-origin            | <https://pokeapi.co/api/v2/pokemon/10007/> | 10007 |
| glaceon                    | <https://pokeapi.co/api/v2/pokemon/471/>   |   471 |
| glalie                     | <https://pokeapi.co/api/v2/pokemon/362/>   |   362 |
| glalie-mega                | <https://pokeapi.co/api/v2/pokemon/10074/> | 10074 |
| glameow                    | <https://pokeapi.co/api/v2/pokemon/431/>   |   431 |
| glastrier                  | <https://pokeapi.co/api/v2/pokemon/896/>   |   896 |
| gligar                     | <https://pokeapi.co/api/v2/pokemon/207/>   |   207 |
| gliscor                    | <https://pokeapi.co/api/v2/pokemon/472/>   |   472 |
| gloom                      | <https://pokeapi.co/api/v2/pokemon/44/>    |    44 |
| gogoat                     | <https://pokeapi.co/api/v2/pokemon/673/>   |   673 |
| golbat                     | <https://pokeapi.co/api/v2/pokemon/42/>    |    42 |
| goldeen                    | <https://pokeapi.co/api/v2/pokemon/118/>   |   118 |
| golduck                    | <https://pokeapi.co/api/v2/pokemon/55/>    |    55 |
| golem                      | <https://pokeapi.co/api/v2/pokemon/76/>    |    76 |
| golem-alola                | <https://pokeapi.co/api/v2/pokemon/10111/> | 10111 |
| golett                     | <https://pokeapi.co/api/v2/pokemon/622/>   |   622 |
| golisopod                  | <https://pokeapi.co/api/v2/pokemon/768/>   |   768 |
| golurk                     | <https://pokeapi.co/api/v2/pokemon/623/>   |   623 |
| goodra                     | <https://pokeapi.co/api/v2/pokemon/706/>   |   706 |
| goomy                      | <https://pokeapi.co/api/v2/pokemon/704/>   |   704 |
| gorebyss                   | <https://pokeapi.co/api/v2/pokemon/368/>   |   368 |
| gossifleur                 | <https://pokeapi.co/api/v2/pokemon/829/>   |   829 |
| gothita                    | <https://pokeapi.co/api/v2/pokemon/574/>   |   574 |
| gothitelle                 | <https://pokeapi.co/api/v2/pokemon/576/>   |   576 |
| gothorita                  | <https://pokeapi.co/api/v2/pokemon/575/>   |   575 |
| gourgeist-average          | <https://pokeapi.co/api/v2/pokemon/711/>   |   711 |
| gourgeist-large            | <https://pokeapi.co/api/v2/pokemon/10031/> | 10031 |
| gourgeist-small            | <https://pokeapi.co/api/v2/pokemon/10030/> | 10030 |
| gourgeist-super            | <https://pokeapi.co/api/v2/pokemon/10032/> | 10032 |
| granbull                   | <https://pokeapi.co/api/v2/pokemon/210/>   |   210 |
| grapploct                  | <https://pokeapi.co/api/v2/pokemon/853/>   |   853 |
| graveler                   | <https://pokeapi.co/api/v2/pokemon/75/>    |    75 |
| graveler-alola             | <https://pokeapi.co/api/v2/pokemon/10110/> | 10110 |
| greedent                   | <https://pokeapi.co/api/v2/pokemon/820/>   |   820 |
| greninja                   | <https://pokeapi.co/api/v2/pokemon/658/>   |   658 |
| greninja-ash               | <https://pokeapi.co/api/v2/pokemon/10117/> | 10117 |
| greninja-battle-bond       | <https://pokeapi.co/api/v2/pokemon/10116/> | 10116 |
| grimer                     | <https://pokeapi.co/api/v2/pokemon/88/>    |    88 |
| grimer-alola               | <https://pokeapi.co/api/v2/pokemon/10112/> | 10112 |
| grimmsnarl                 | <https://pokeapi.co/api/v2/pokemon/861/>   |   861 |
| grimmsnarl-gmax            | <https://pokeapi.co/api/v2/pokemon/10213/> | 10213 |
| grookey                    | <https://pokeapi.co/api/v2/pokemon/810/>   |   810 |
| grotle                     | <https://pokeapi.co/api/v2/pokemon/388/>   |   388 |
| groudon                    | <https://pokeapi.co/api/v2/pokemon/383/>   |   383 |
| groudon-primal             | <https://pokeapi.co/api/v2/pokemon/10078/> | 10078 |
| grovyle                    | <https://pokeapi.co/api/v2/pokemon/253/>   |   253 |
| growlithe                  | <https://pokeapi.co/api/v2/pokemon/58/>    |    58 |
| grubbin                    | <https://pokeapi.co/api/v2/pokemon/736/>   |   736 |
| grumpig                    | <https://pokeapi.co/api/v2/pokemon/326/>   |   326 |
| gulpin                     | <https://pokeapi.co/api/v2/pokemon/316/>   |   316 |
| gumshoos                   | <https://pokeapi.co/api/v2/pokemon/735/>   |   735 |
| gumshoos-totem             | <https://pokeapi.co/api/v2/pokemon/10121/> | 10121 |
| gurdurr                    | <https://pokeapi.co/api/v2/pokemon/533/>   |   533 |
| guzzlord                   | <https://pokeapi.co/api/v2/pokemon/799/>   |   799 |
| gyarados                   | <https://pokeapi.co/api/v2/pokemon/130/>   |   130 |
| gyarados-mega              | <https://pokeapi.co/api/v2/pokemon/10041/> | 10041 |
| hakamo-o                   | <https://pokeapi.co/api/v2/pokemon/783/>   |   783 |
| happiny                    | <https://pokeapi.co/api/v2/pokemon/440/>   |   440 |
| hariyama                   | <https://pokeapi.co/api/v2/pokemon/297/>   |   297 |
| hatenna                    | <https://pokeapi.co/api/v2/pokemon/856/>   |   856 |
| hatterene                  | <https://pokeapi.co/api/v2/pokemon/858/>   |   858 |
| hatterene-gmax             | <https://pokeapi.co/api/v2/pokemon/10212/> | 10212 |
| hattrem                    | <https://pokeapi.co/api/v2/pokemon/857/>   |   857 |
| haunter                    | <https://pokeapi.co/api/v2/pokemon/93/>    |    93 |
| hawlucha                   | <https://pokeapi.co/api/v2/pokemon/701/>   |   701 |
| haxorus                    | <https://pokeapi.co/api/v2/pokemon/612/>   |   612 |
| heatmor                    | <https://pokeapi.co/api/v2/pokemon/631/>   |   631 |
| heatran                    | <https://pokeapi.co/api/v2/pokemon/485/>   |   485 |
| heliolisk                  | <https://pokeapi.co/api/v2/pokemon/695/>   |   695 |
| helioptile                 | <https://pokeapi.co/api/v2/pokemon/694/>   |   694 |
| heracross                  | <https://pokeapi.co/api/v2/pokemon/214/>   |   214 |
| heracross-mega             | <https://pokeapi.co/api/v2/pokemon/10047/> | 10047 |
| herdier                    | <https://pokeapi.co/api/v2/pokemon/507/>   |   507 |
| hippopotas                 | <https://pokeapi.co/api/v2/pokemon/449/>   |   449 |
| hippowdon                  | <https://pokeapi.co/api/v2/pokemon/450/>   |   450 |
| hitmonchan                 | <https://pokeapi.co/api/v2/pokemon/107/>   |   107 |
| hitmonlee                  | <https://pokeapi.co/api/v2/pokemon/106/>   |   106 |
| hitmontop                  | <https://pokeapi.co/api/v2/pokemon/237/>   |   237 |
| ho-oh                      | <https://pokeapi.co/api/v2/pokemon/250/>   |   250 |
| honchkrow                  | <https://pokeapi.co/api/v2/pokemon/430/>   |   430 |
| honedge                    | <https://pokeapi.co/api/v2/pokemon/679/>   |   679 |
| hoopa                      | <https://pokeapi.co/api/v2/pokemon/720/>   |   720 |
| hoopa-unbound              | <https://pokeapi.co/api/v2/pokemon/10086/> | 10086 |
| hoothoot                   | <https://pokeapi.co/api/v2/pokemon/163/>   |   163 |
| hoppip                     | <https://pokeapi.co/api/v2/pokemon/187/>   |   187 |
| horsea                     | <https://pokeapi.co/api/v2/pokemon/116/>   |   116 |
| houndoom                   | <https://pokeapi.co/api/v2/pokemon/229/>   |   229 |
| houndoom-mega              | <https://pokeapi.co/api/v2/pokemon/10048/> | 10048 |
| houndour                   | <https://pokeapi.co/api/v2/pokemon/228/>   |   228 |
| huntail                    | <https://pokeapi.co/api/v2/pokemon/367/>   |   367 |
| hydreigon                  | <https://pokeapi.co/api/v2/pokemon/635/>   |   635 |
| hypno                      | <https://pokeapi.co/api/v2/pokemon/97/>    |    97 |
| igglybuff                  | <https://pokeapi.co/api/v2/pokemon/174/>   |   174 |
| illumise                   | <https://pokeapi.co/api/v2/pokemon/314/>   |   314 |
| impidimp                   | <https://pokeapi.co/api/v2/pokemon/859/>   |   859 |
| incineroar                 | <https://pokeapi.co/api/v2/pokemon/727/>   |   727 |
| indeedee-female            | <https://pokeapi.co/api/v2/pokemon/10180/> | 10180 |
| indeedee-male              | <https://pokeapi.co/api/v2/pokemon/876/>   |   876 |
| infernape                  | <https://pokeapi.co/api/v2/pokemon/392/>   |   392 |
| inkay                      | <https://pokeapi.co/api/v2/pokemon/686/>   |   686 |
| inteleon                   | <https://pokeapi.co/api/v2/pokemon/818/>   |   818 |
| inteleon-gmax              | <https://pokeapi.co/api/v2/pokemon/10202/> | 10202 |
| ivysaur                    | <https://pokeapi.co/api/v2/pokemon/2/>     |     2 |
| jangmo-o                   | <https://pokeapi.co/api/v2/pokemon/782/>   |   782 |
| jellicent                  | <https://pokeapi.co/api/v2/pokemon/593/>   |   593 |
| jigglypuff                 | <https://pokeapi.co/api/v2/pokemon/39/>    |    39 |
| jirachi                    | <https://pokeapi.co/api/v2/pokemon/385/>   |   385 |
| jolteon                    | <https://pokeapi.co/api/v2/pokemon/135/>   |   135 |
| joltik                     | <https://pokeapi.co/api/v2/pokemon/595/>   |   595 |
| jumpluff                   | <https://pokeapi.co/api/v2/pokemon/189/>   |   189 |
| jynx                       | <https://pokeapi.co/api/v2/pokemon/124/>   |   124 |
| kabuto                     | <https://pokeapi.co/api/v2/pokemon/140/>   |   140 |
| kabutops                   | <https://pokeapi.co/api/v2/pokemon/141/>   |   141 |
| kadabra                    | <https://pokeapi.co/api/v2/pokemon/64/>    |    64 |
| kakuna                     | <https://pokeapi.co/api/v2/pokemon/14/>    |    14 |
| kangaskhan                 | <https://pokeapi.co/api/v2/pokemon/115/>   |   115 |
| kangaskhan-mega            | <https://pokeapi.co/api/v2/pokemon/10039/> | 10039 |
| karrablast                 | <https://pokeapi.co/api/v2/pokemon/588/>   |   588 |
| kartana                    | <https://pokeapi.co/api/v2/pokemon/798/>   |   798 |
| kecleon                    | <https://pokeapi.co/api/v2/pokemon/352/>   |   352 |
| keldeo-ordinary            | <https://pokeapi.co/api/v2/pokemon/647/>   |   647 |
| keldeo-resolute            | <https://pokeapi.co/api/v2/pokemon/10024/> | 10024 |
| kingdra                    | <https://pokeapi.co/api/v2/pokemon/230/>   |   230 |
| kingler                    | <https://pokeapi.co/api/v2/pokemon/99/>    |    99 |
| kingler-gmax               | <https://pokeapi.co/api/v2/pokemon/10194/> | 10194 |
| kirlia                     | <https://pokeapi.co/api/v2/pokemon/281/>   |   281 |
| klang                      | <https://pokeapi.co/api/v2/pokemon/600/>   |   600 |
| klefki                     | <https://pokeapi.co/api/v2/pokemon/707/>   |   707 |
| klink                      | <https://pokeapi.co/api/v2/pokemon/599/>   |   599 |
| klinklang                  | <https://pokeapi.co/api/v2/pokemon/601/>   |   601 |
| koffing                    | <https://pokeapi.co/api/v2/pokemon/109/>   |   109 |
| komala                     | <https://pokeapi.co/api/v2/pokemon/775/>   |   775 |
| kommo-o                    | <https://pokeapi.co/api/v2/pokemon/784/>   |   784 |
| kommo-o-totem              | <https://pokeapi.co/api/v2/pokemon/10146/> | 10146 |
| krabby                     | <https://pokeapi.co/api/v2/pokemon/98/>    |    98 |
| kricketot                  | <https://pokeapi.co/api/v2/pokemon/401/>   |   401 |
| kricketune                 | <https://pokeapi.co/api/v2/pokemon/402/>   |   402 |
| krokorok                   | <https://pokeapi.co/api/v2/pokemon/552/>   |   552 |
| krookodile                 | <https://pokeapi.co/api/v2/pokemon/553/>   |   553 |
| kubfu                      | <https://pokeapi.co/api/v2/pokemon/891/>   |   891 |
| kyogre                     | <https://pokeapi.co/api/v2/pokemon/382/>   |   382 |
| kyogre-primal              | <https://pokeapi.co/api/v2/pokemon/10077/> | 10077 |
| kyurem                     | <https://pokeapi.co/api/v2/pokemon/646/>   |   646 |
| kyurem-black               | <https://pokeapi.co/api/v2/pokemon/10022/> | 10022 |
| kyurem-white               | <https://pokeapi.co/api/v2/pokemon/10023/> | 10023 |
| lairon                     | <https://pokeapi.co/api/v2/pokemon/305/>   |   305 |
| lampent                    | <https://pokeapi.co/api/v2/pokemon/608/>   |   608 |
| landorus-incarnate         | <https://pokeapi.co/api/v2/pokemon/645/>   |   645 |
| landorus-therian           | <https://pokeapi.co/api/v2/pokemon/10021/> | 10021 |
| lanturn                    | <https://pokeapi.co/api/v2/pokemon/171/>   |   171 |
| lapras                     | <https://pokeapi.co/api/v2/pokemon/131/>   |   131 |
| lapras-gmax                | <https://pokeapi.co/api/v2/pokemon/10195/> | 10195 |
| larvesta                   | <https://pokeapi.co/api/v2/pokemon/636/>   |   636 |
| larvitar                   | <https://pokeapi.co/api/v2/pokemon/246/>   |   246 |
| latias                     | <https://pokeapi.co/api/v2/pokemon/380/>   |   380 |
| latias-mega                | <https://pokeapi.co/api/v2/pokemon/10062/> | 10062 |
| latios                     | <https://pokeapi.co/api/v2/pokemon/381/>   |   381 |
| latios-mega                | <https://pokeapi.co/api/v2/pokemon/10063/> | 10063 |
| leafeon                    | <https://pokeapi.co/api/v2/pokemon/470/>   |   470 |
| leavanny                   | <https://pokeapi.co/api/v2/pokemon/542/>   |   542 |
| ledian                     | <https://pokeapi.co/api/v2/pokemon/166/>   |   166 |
| ledyba                     | <https://pokeapi.co/api/v2/pokemon/165/>   |   165 |
| lickilicky                 | <https://pokeapi.co/api/v2/pokemon/463/>   |   463 |
| lickitung                  | <https://pokeapi.co/api/v2/pokemon/108/>   |   108 |
| liepard                    | <https://pokeapi.co/api/v2/pokemon/510/>   |   510 |
| lileep                     | <https://pokeapi.co/api/v2/pokemon/345/>   |   345 |
| lilligant                  | <https://pokeapi.co/api/v2/pokemon/549/>   |   549 |
| lillipup                   | <https://pokeapi.co/api/v2/pokemon/506/>   |   506 |
| linoone                    | <https://pokeapi.co/api/v2/pokemon/264/>   |   264 |
| linoone-galar              | <https://pokeapi.co/api/v2/pokemon/10172/> | 10172 |
| litleo                     | <https://pokeapi.co/api/v2/pokemon/667/>   |   667 |
| litten                     | <https://pokeapi.co/api/v2/pokemon/725/>   |   725 |
| litwick                    | <https://pokeapi.co/api/v2/pokemon/607/>   |   607 |
| lombre                     | <https://pokeapi.co/api/v2/pokemon/271/>   |   271 |
| lopunny                    | <https://pokeapi.co/api/v2/pokemon/428/>   |   428 |
| lopunny-mega               | <https://pokeapi.co/api/v2/pokemon/10088/> | 10088 |
| lotad                      | <https://pokeapi.co/api/v2/pokemon/270/>   |   270 |
| loudred                    | <https://pokeapi.co/api/v2/pokemon/294/>   |   294 |
| lucario                    | <https://pokeapi.co/api/v2/pokemon/448/>   |   448 |
| lucario-mega               | <https://pokeapi.co/api/v2/pokemon/10059/> | 10059 |
| ludicolo                   | <https://pokeapi.co/api/v2/pokemon/272/>   |   272 |
| lugia                      | <https://pokeapi.co/api/v2/pokemon/249/>   |   249 |
| lumineon                   | <https://pokeapi.co/api/v2/pokemon/457/>   |   457 |
| lunala                     | <https://pokeapi.co/api/v2/pokemon/792/>   |   792 |
| lunatone                   | <https://pokeapi.co/api/v2/pokemon/337/>   |   337 |
| lurantis                   | <https://pokeapi.co/api/v2/pokemon/754/>   |   754 |
| lurantis-totem             | <https://pokeapi.co/api/v2/pokemon/10128/> | 10128 |
| luvdisc                    | <https://pokeapi.co/api/v2/pokemon/370/>   |   370 |
| luxio                      | <https://pokeapi.co/api/v2/pokemon/404/>   |   404 |
| luxray                     | <https://pokeapi.co/api/v2/pokemon/405/>   |   405 |
| lycanroc-dusk              | <https://pokeapi.co/api/v2/pokemon/10152/> | 10152 |
| lycanroc-midday            | <https://pokeapi.co/api/v2/pokemon/745/>   |   745 |
| lycanroc-midnight          | <https://pokeapi.co/api/v2/pokemon/10126/> | 10126 |
| machamp                    | <https://pokeapi.co/api/v2/pokemon/68/>    |    68 |
| machamp-gmax               | <https://pokeapi.co/api/v2/pokemon/10192/> | 10192 |
| machoke                    | <https://pokeapi.co/api/v2/pokemon/67/>    |    67 |
| machop                     | <https://pokeapi.co/api/v2/pokemon/66/>    |    66 |
| magby                      | <https://pokeapi.co/api/v2/pokemon/240/>   |   240 |
| magcargo                   | <https://pokeapi.co/api/v2/pokemon/219/>   |   219 |
| magearna                   | <https://pokeapi.co/api/v2/pokemon/801/>   |   801 |
| magearna-original          | <https://pokeapi.co/api/v2/pokemon/10147/> | 10147 |
| magikarp                   | <https://pokeapi.co/api/v2/pokemon/129/>   |   129 |
| magmar                     | <https://pokeapi.co/api/v2/pokemon/126/>   |   126 |
| magmortar                  | <https://pokeapi.co/api/v2/pokemon/467/>   |   467 |
| magnemite                  | <https://pokeapi.co/api/v2/pokemon/81/>    |    81 |
| magneton                   | <https://pokeapi.co/api/v2/pokemon/82/>    |    82 |
| magnezone                  | <https://pokeapi.co/api/v2/pokemon/462/>   |   462 |
| makuhita                   | <https://pokeapi.co/api/v2/pokemon/296/>   |   296 |
| malamar                    | <https://pokeapi.co/api/v2/pokemon/687/>   |   687 |
| mamoswine                  | <https://pokeapi.co/api/v2/pokemon/473/>   |   473 |
| manaphy                    | <https://pokeapi.co/api/v2/pokemon/490/>   |   490 |
| mandibuzz                  | <https://pokeapi.co/api/v2/pokemon/630/>   |   630 |
| manectric                  | <https://pokeapi.co/api/v2/pokemon/310/>   |   310 |
| manectric-mega             | <https://pokeapi.co/api/v2/pokemon/10055/> | 10055 |
| mankey                     | <https://pokeapi.co/api/v2/pokemon/56/>    |    56 |
| mantine                    | <https://pokeapi.co/api/v2/pokemon/226/>   |   226 |
| mantyke                    | <https://pokeapi.co/api/v2/pokemon/458/>   |   458 |
| maractus                   | <https://pokeapi.co/api/v2/pokemon/556/>   |   556 |
| mareanie                   | <https://pokeapi.co/api/v2/pokemon/747/>   |   747 |
| mareep                     | <https://pokeapi.co/api/v2/pokemon/179/>   |   179 |
| marill                     | <https://pokeapi.co/api/v2/pokemon/183/>   |   183 |
| marowak                    | <https://pokeapi.co/api/v2/pokemon/105/>   |   105 |
| marowak-alola              | <https://pokeapi.co/api/v2/pokemon/10115/> | 10115 |
| marowak-totem              | <https://pokeapi.co/api/v2/pokemon/10149/> | 10149 |
| marshadow                  | <https://pokeapi.co/api/v2/pokemon/802/>   |   802 |
| marshtomp                  | <https://pokeapi.co/api/v2/pokemon/259/>   |   259 |
| masquerain                 | <https://pokeapi.co/api/v2/pokemon/284/>   |   284 |
| mawile                     | <https://pokeapi.co/api/v2/pokemon/303/>   |   303 |
| mawile-mega                | <https://pokeapi.co/api/v2/pokemon/10052/> | 10052 |
| medicham                   | <https://pokeapi.co/api/v2/pokemon/308/>   |   308 |
| medicham-mega              | <https://pokeapi.co/api/v2/pokemon/10054/> | 10054 |
| meditite                   | <https://pokeapi.co/api/v2/pokemon/307/>   |   307 |
| meganium                   | <https://pokeapi.co/api/v2/pokemon/154/>   |   154 |
| melmetal                   | <https://pokeapi.co/api/v2/pokemon/809/>   |   809 |
| melmetal-gmax              | <https://pokeapi.co/api/v2/pokemon/10199/> | 10199 |
| meloetta-aria              | <https://pokeapi.co/api/v2/pokemon/648/>   |   648 |
| meloetta-pirouette         | <https://pokeapi.co/api/v2/pokemon/10018/> | 10018 |
| meltan                     | <https://pokeapi.co/api/v2/pokemon/808/>   |   808 |
| meowstic-female            | <https://pokeapi.co/api/v2/pokemon/10025/> | 10025 |
| meowstic-male              | <https://pokeapi.co/api/v2/pokemon/678/>   |   678 |
| meowth                     | <https://pokeapi.co/api/v2/pokemon/52/>    |    52 |
| meowth-alola               | <https://pokeapi.co/api/v2/pokemon/10107/> | 10107 |
| meowth-galar               | <https://pokeapi.co/api/v2/pokemon/10158/> | 10158 |
| meowth-gmax                | <https://pokeapi.co/api/v2/pokemon/10191/> | 10191 |
| mesprit                    | <https://pokeapi.co/api/v2/pokemon/481/>   |   481 |
| metagross                  | <https://pokeapi.co/api/v2/pokemon/376/>   |   376 |
| metagross-mega             | <https://pokeapi.co/api/v2/pokemon/10076/> | 10076 |
| metang                     | <https://pokeapi.co/api/v2/pokemon/375/>   |   375 |
| metapod                    | <https://pokeapi.co/api/v2/pokemon/11/>    |    11 |
| mew                        | <https://pokeapi.co/api/v2/pokemon/151/>   |   151 |
| mewtwo                     | <https://pokeapi.co/api/v2/pokemon/150/>   |   150 |
| mewtwo-mega-x              | <https://pokeapi.co/api/v2/pokemon/10043/> | 10043 |
| mewtwo-mega-y              | <https://pokeapi.co/api/v2/pokemon/10044/> | 10044 |
| mienfoo                    | <https://pokeapi.co/api/v2/pokemon/619/>   |   619 |
| mienshao                   | <https://pokeapi.co/api/v2/pokemon/620/>   |   620 |
| mightyena                  | <https://pokeapi.co/api/v2/pokemon/262/>   |   262 |
| milcery                    | <https://pokeapi.co/api/v2/pokemon/868/>   |   868 |
| milotic                    | <https://pokeapi.co/api/v2/pokemon/350/>   |   350 |
| miltank                    | <https://pokeapi.co/api/v2/pokemon/241/>   |   241 |
| mime-jr                    | <https://pokeapi.co/api/v2/pokemon/439/>   |   439 |
| mimikyu-busted             | <https://pokeapi.co/api/v2/pokemon/10143/> | 10143 |
| mimikyu-disguised          | <https://pokeapi.co/api/v2/pokemon/778/>   |   778 |
| mimikyu-totem-busted       | <https://pokeapi.co/api/v2/pokemon/10145/> | 10145 |
| mimikyu-totem-disguised    | <https://pokeapi.co/api/v2/pokemon/10144/> | 10144 |
| minccino                   | <https://pokeapi.co/api/v2/pokemon/572/>   |   572 |
| minior-blue                | <https://pokeapi.co/api/v2/pokemon/10140/> | 10140 |
| minior-blue-meteor         | <https://pokeapi.co/api/v2/pokemon/10133/> | 10133 |
| minior-green               | <https://pokeapi.co/api/v2/pokemon/10139/> | 10139 |
| minior-green-meteor        | <https://pokeapi.co/api/v2/pokemon/10132/> | 10132 |
| minior-indigo              | <https://pokeapi.co/api/v2/pokemon/10141/> | 10141 |
| minior-indigo-meteor       | <https://pokeapi.co/api/v2/pokemon/10134/> | 10134 |
| minior-orange              | <https://pokeapi.co/api/v2/pokemon/10137/> | 10137 |
| minior-orange-meteor       | <https://pokeapi.co/api/v2/pokemon/10130/> | 10130 |
| minior-red                 | <https://pokeapi.co/api/v2/pokemon/10136/> | 10136 |
| minior-red-meteor          | <https://pokeapi.co/api/v2/pokemon/774/>   |   774 |
| minior-violet              | <https://pokeapi.co/api/v2/pokemon/10142/> | 10142 |
| minior-violet-meteor       | <https://pokeapi.co/api/v2/pokemon/10135/> | 10135 |
| minior-yellow              | <https://pokeapi.co/api/v2/pokemon/10138/> | 10138 |
| minior-yellow-meteor       | <https://pokeapi.co/api/v2/pokemon/10131/> | 10131 |
| minun                      | <https://pokeapi.co/api/v2/pokemon/312/>   |   312 |
| misdreavus                 | <https://pokeapi.co/api/v2/pokemon/200/>   |   200 |
| mismagius                  | <https://pokeapi.co/api/v2/pokemon/429/>   |   429 |
| moltres                    | <https://pokeapi.co/api/v2/pokemon/146/>   |   146 |
| moltres-galar              | <https://pokeapi.co/api/v2/pokemon/10168/> | 10168 |
| monferno                   | <https://pokeapi.co/api/v2/pokemon/391/>   |   391 |
| morelull                   | <https://pokeapi.co/api/v2/pokemon/755/>   |   755 |
| morgrem                    | <https://pokeapi.co/api/v2/pokemon/860/>   |   860 |
| morpeko                    | <https://pokeapi.co/api/v2/pokemon/877/>   |   877 |
| mothim                     | <https://pokeapi.co/api/v2/pokemon/414/>   |   414 |
| mr-mime                    | <https://pokeapi.co/api/v2/pokemon/122/>   |   122 |
| mr-mime-galar              | <https://pokeapi.co/api/v2/pokemon/10165/> | 10165 |
| mr-rime                    | <https://pokeapi.co/api/v2/pokemon/866/>   |   866 |
| mudbray                    | <https://pokeapi.co/api/v2/pokemon/749/>   |   749 |
| mudkip                     | <https://pokeapi.co/api/v2/pokemon/258/>   |   258 |
| mudsdale                   | <https://pokeapi.co/api/v2/pokemon/750/>   |   750 |
| muk                        | <https://pokeapi.co/api/v2/pokemon/89/>    |    89 |
| muk-alola                  | <https://pokeapi.co/api/v2/pokemon/10113/> | 10113 |
| munchlax                   | <https://pokeapi.co/api/v2/pokemon/446/>   |   446 |
| munna                      | <https://pokeapi.co/api/v2/pokemon/517/>   |   517 |
| murkrow                    | <https://pokeapi.co/api/v2/pokemon/198/>   |   198 |
| musharna                   | <https://pokeapi.co/api/v2/pokemon/518/>   |   518 |
| naganadel                  | <https://pokeapi.co/api/v2/pokemon/804/>   |   804 |
| natu                       | <https://pokeapi.co/api/v2/pokemon/177/>   |   177 |
| necrozma                   | <https://pokeapi.co/api/v2/pokemon/800/>   |   800 |
| necrozma-dawn              | <https://pokeapi.co/api/v2/pokemon/10156/> | 10156 |
| necrozma-dusk              | <https://pokeapi.co/api/v2/pokemon/10155/> | 10155 |
| necrozma-ultra             | <https://pokeapi.co/api/v2/pokemon/10157/> | 10157 |
| nickit                     | <https://pokeapi.co/api/v2/pokemon/827/>   |   827 |
| nidoking                   | <https://pokeapi.co/api/v2/pokemon/34/>    |    34 |
| nidoqueen                  | <https://pokeapi.co/api/v2/pokemon/31/>    |    31 |
| nidoran-f                  | <https://pokeapi.co/api/v2/pokemon/29/>    |    29 |
| nidoran-m                  | <https://pokeapi.co/api/v2/pokemon/32/>    |    32 |
| nidorina                   | <https://pokeapi.co/api/v2/pokemon/30/>    |    30 |
| nidorino                   | <https://pokeapi.co/api/v2/pokemon/33/>    |    33 |
| nihilego                   | <https://pokeapi.co/api/v2/pokemon/793/>   |   793 |
| nincada                    | <https://pokeapi.co/api/v2/pokemon/290/>   |   290 |
| ninetales                  | <https://pokeapi.co/api/v2/pokemon/38/>    |    38 |
| ninetales-alola            | <https://pokeapi.co/api/v2/pokemon/10104/> | 10104 |
| ninjask                    | <https://pokeapi.co/api/v2/pokemon/291/>   |   291 |
| noctowl                    | <https://pokeapi.co/api/v2/pokemon/164/>   |   164 |
| noibat                     | <https://pokeapi.co/api/v2/pokemon/714/>   |   714 |
| noivern                    | <https://pokeapi.co/api/v2/pokemon/715/>   |   715 |
| nosepass                   | <https://pokeapi.co/api/v2/pokemon/299/>   |   299 |
| numel                      | <https://pokeapi.co/api/v2/pokemon/322/>   |   322 |
| nuzleaf                    | <https://pokeapi.co/api/v2/pokemon/274/>   |   274 |
| obstagoon                  | <https://pokeapi.co/api/v2/pokemon/862/>   |   862 |
| octillery                  | <https://pokeapi.co/api/v2/pokemon/224/>   |   224 |
| oddish                     | <https://pokeapi.co/api/v2/pokemon/43/>    |    43 |
| omanyte                    | <https://pokeapi.co/api/v2/pokemon/138/>   |   138 |
| omastar                    | <https://pokeapi.co/api/v2/pokemon/139/>   |   139 |
| onix                       | <https://pokeapi.co/api/v2/pokemon/95/>    |    95 |
| oranguru                   | <https://pokeapi.co/api/v2/pokemon/765/>   |   765 |
| orbeetle                   | <https://pokeapi.co/api/v2/pokemon/826/>   |   826 |
| orbeetle-gmax              | <https://pokeapi.co/api/v2/pokemon/10204/> | 10204 |
| oricorio-baile             | <https://pokeapi.co/api/v2/pokemon/741/>   |   741 |
| oricorio-pau               | <https://pokeapi.co/api/v2/pokemon/10124/> | 10124 |
| oricorio-pom-pom           | <https://pokeapi.co/api/v2/pokemon/10123/> | 10123 |
| oricorio-sensu             | <https://pokeapi.co/api/v2/pokemon/10125/> | 10125 |
| oshawott                   | <https://pokeapi.co/api/v2/pokemon/501/>   |   501 |
| pachirisu                  | <https://pokeapi.co/api/v2/pokemon/417/>   |   417 |
| palkia                     | <https://pokeapi.co/api/v2/pokemon/484/>   |   484 |
| palossand                  | <https://pokeapi.co/api/v2/pokemon/770/>   |   770 |
| palpitoad                  | <https://pokeapi.co/api/v2/pokemon/536/>   |   536 |
| pancham                    | <https://pokeapi.co/api/v2/pokemon/674/>   |   674 |
| pangoro                    | <https://pokeapi.co/api/v2/pokemon/675/>   |   675 |
| panpour                    | <https://pokeapi.co/api/v2/pokemon/515/>   |   515 |
| pansage                    | <https://pokeapi.co/api/v2/pokemon/511/>   |   511 |
| pansear                    | <https://pokeapi.co/api/v2/pokemon/513/>   |   513 |
| paras                      | <https://pokeapi.co/api/v2/pokemon/46/>    |    46 |
| parasect                   | <https://pokeapi.co/api/v2/pokemon/47/>    |    47 |
| passimian                  | <https://pokeapi.co/api/v2/pokemon/766/>   |   766 |
| patrat                     | <https://pokeapi.co/api/v2/pokemon/504/>   |   504 |
| pawniard                   | <https://pokeapi.co/api/v2/pokemon/624/>   |   624 |
| pelipper                   | <https://pokeapi.co/api/v2/pokemon/279/>   |   279 |
| perrserker                 | <https://pokeapi.co/api/v2/pokemon/863/>   |   863 |
| persian                    | <https://pokeapi.co/api/v2/pokemon/53/>    |    53 |
| persian-alola              | <https://pokeapi.co/api/v2/pokemon/10108/> | 10108 |
| petilil                    | <https://pokeapi.co/api/v2/pokemon/548/>   |   548 |
| phanpy                     | <https://pokeapi.co/api/v2/pokemon/231/>   |   231 |
| phantump                   | <https://pokeapi.co/api/v2/pokemon/708/>   |   708 |
| pheromosa                  | <https://pokeapi.co/api/v2/pokemon/795/>   |   795 |
| phione                     | <https://pokeapi.co/api/v2/pokemon/489/>   |   489 |
| pichu                      | <https://pokeapi.co/api/v2/pokemon/172/>   |   172 |
| pidgeot                    | <https://pokeapi.co/api/v2/pokemon/18/>    |    18 |
| pidgeot-mega               | <https://pokeapi.co/api/v2/pokemon/10073/> | 10073 |
| pidgeotto                  | <https://pokeapi.co/api/v2/pokemon/17/>    |    17 |
| pidgey                     | <https://pokeapi.co/api/v2/pokemon/16/>    |    16 |
| pidove                     | <https://pokeapi.co/api/v2/pokemon/519/>   |   519 |
| pignite                    | <https://pokeapi.co/api/v2/pokemon/499/>   |   499 |
| pikachu                    | <https://pokeapi.co/api/v2/pokemon/25/>    |    25 |
| pikachu-alola-cap          | <https://pokeapi.co/api/v2/pokemon/10099/> | 10099 |
| pikachu-belle              | <https://pokeapi.co/api/v2/pokemon/10081/> | 10081 |
| pikachu-cosplay            | <https://pokeapi.co/api/v2/pokemon/10085/> | 10085 |
| pikachu-gmax               | <https://pokeapi.co/api/v2/pokemon/10190/> | 10190 |
| pikachu-hoenn-cap          | <https://pokeapi.co/api/v2/pokemon/10095/> | 10095 |
| pikachu-kalos-cap          | <https://pokeapi.co/api/v2/pokemon/10098/> | 10098 |
| pikachu-libre              | <https://pokeapi.co/api/v2/pokemon/10084/> | 10084 |
| pikachu-original-cap       | <https://pokeapi.co/api/v2/pokemon/10094/> | 10094 |
| pikachu-partner-cap        | <https://pokeapi.co/api/v2/pokemon/10148/> | 10148 |
| pikachu-phd                | <https://pokeapi.co/api/v2/pokemon/10083/> | 10083 |
| pikachu-pop-star           | <https://pokeapi.co/api/v2/pokemon/10082/> | 10082 |
| pikachu-rock-star          | <https://pokeapi.co/api/v2/pokemon/10080/> | 10080 |
| pikachu-sinnoh-cap         | <https://pokeapi.co/api/v2/pokemon/10096/> | 10096 |
| pikachu-unova-cap          | <https://pokeapi.co/api/v2/pokemon/10097/> | 10097 |
| pikipek                    | <https://pokeapi.co/api/v2/pokemon/731/>   |   731 |
| piloswine                  | <https://pokeapi.co/api/v2/pokemon/221/>   |   221 |
| pincurchin                 | <https://pokeapi.co/api/v2/pokemon/871/>   |   871 |
| pineco                     | <https://pokeapi.co/api/v2/pokemon/204/>   |   204 |
| pinsir                     | <https://pokeapi.co/api/v2/pokemon/127/>   |   127 |
| pinsir-mega                | <https://pokeapi.co/api/v2/pokemon/10040/> | 10040 |
| piplup                     | <https://pokeapi.co/api/v2/pokemon/393/>   |   393 |
| plusle                     | <https://pokeapi.co/api/v2/pokemon/311/>   |   311 |
| poipole                    | <https://pokeapi.co/api/v2/pokemon/803/>   |   803 |
| politoed                   | <https://pokeapi.co/api/v2/pokemon/186/>   |   186 |
| poliwag                    | <https://pokeapi.co/api/v2/pokemon/60/>    |    60 |
| poliwhirl                  | <https://pokeapi.co/api/v2/pokemon/61/>    |    61 |
| poliwrath                  | <https://pokeapi.co/api/v2/pokemon/62/>    |    62 |
| polteageist                | <https://pokeapi.co/api/v2/pokemon/855/>   |   855 |
| ponyta                     | <https://pokeapi.co/api/v2/pokemon/77/>    |    77 |
| ponyta-galar               | <https://pokeapi.co/api/v2/pokemon/10159/> | 10159 |
| poochyena                  | <https://pokeapi.co/api/v2/pokemon/261/>   |   261 |
| popplio                    | <https://pokeapi.co/api/v2/pokemon/728/>   |   728 |
| porygon                    | <https://pokeapi.co/api/v2/pokemon/137/>   |   137 |
| porygon-z                  | <https://pokeapi.co/api/v2/pokemon/474/>   |   474 |
| porygon2                   | <https://pokeapi.co/api/v2/pokemon/233/>   |   233 |
| primarina                  | <https://pokeapi.co/api/v2/pokemon/730/>   |   730 |
| primeape                   | <https://pokeapi.co/api/v2/pokemon/57/>    |    57 |
| prinplup                   | <https://pokeapi.co/api/v2/pokemon/394/>   |   394 |
| probopass                  | <https://pokeapi.co/api/v2/pokemon/476/>   |   476 |
| psyduck                    | <https://pokeapi.co/api/v2/pokemon/54/>    |    54 |
| pumpkaboo-average          | <https://pokeapi.co/api/v2/pokemon/710/>   |   710 |
| pumpkaboo-large            | <https://pokeapi.co/api/v2/pokemon/10028/> | 10028 |
| pumpkaboo-small            | <https://pokeapi.co/api/v2/pokemon/10027/> | 10027 |
| pumpkaboo-super            | <https://pokeapi.co/api/v2/pokemon/10029/> | 10029 |
| pupitar                    | <https://pokeapi.co/api/v2/pokemon/247/>   |   247 |
| purrloin                   | <https://pokeapi.co/api/v2/pokemon/509/>   |   509 |
| purugly                    | <https://pokeapi.co/api/v2/pokemon/432/>   |   432 |
| pyroar                     | <https://pokeapi.co/api/v2/pokemon/668/>   |   668 |
| pyukumuku                  | <https://pokeapi.co/api/v2/pokemon/771/>   |   771 |
| quagsire                   | <https://pokeapi.co/api/v2/pokemon/195/>   |   195 |
| quilava                    | <https://pokeapi.co/api/v2/pokemon/156/>   |   156 |
| quilladin                  | <https://pokeapi.co/api/v2/pokemon/651/>   |   651 |
| qwilfish                   | <https://pokeapi.co/api/v2/pokemon/211/>   |   211 |
| raboot                     | <https://pokeapi.co/api/v2/pokemon/814/>   |   814 |
| raichu                     | <https://pokeapi.co/api/v2/pokemon/26/>    |    26 |
| raichu-alola               | <https://pokeapi.co/api/v2/pokemon/10100/> | 10100 |
| raikou                     | <https://pokeapi.co/api/v2/pokemon/243/>   |   243 |
| ralts                      | <https://pokeapi.co/api/v2/pokemon/280/>   |   280 |
| rampardos                  | <https://pokeapi.co/api/v2/pokemon/409/>   |   409 |
| rapidash                   | <https://pokeapi.co/api/v2/pokemon/78/>    |    78 |
| rapidash-galar             | <https://pokeapi.co/api/v2/pokemon/10160/> | 10160 |
| raticate                   | <https://pokeapi.co/api/v2/pokemon/20/>    |    20 |
| raticate-alola             | <https://pokeapi.co/api/v2/pokemon/10092/> | 10092 |
| raticate-totem-alola       | <https://pokeapi.co/api/v2/pokemon/10093/> | 10093 |
| rattata                    | <https://pokeapi.co/api/v2/pokemon/19/>    |    19 |
| rattata-alola              | <https://pokeapi.co/api/v2/pokemon/10091/> | 10091 |
| rayquaza                   | <https://pokeapi.co/api/v2/pokemon/384/>   |   384 |
| rayquaza-mega              | <https://pokeapi.co/api/v2/pokemon/10079/> | 10079 |
| regice                     | <https://pokeapi.co/api/v2/pokemon/378/>   |   378 |
| regidrago                  | <https://pokeapi.co/api/v2/pokemon/895/>   |   895 |
| regieleki                  | <https://pokeapi.co/api/v2/pokemon/894/>   |   894 |
| regigigas                  | <https://pokeapi.co/api/v2/pokemon/486/>   |   486 |
| regirock                   | <https://pokeapi.co/api/v2/pokemon/377/>   |   377 |
| registeel                  | <https://pokeapi.co/api/v2/pokemon/379/>   |   379 |
| relicanth                  | <https://pokeapi.co/api/v2/pokemon/369/>   |   369 |
| remoraid                   | <https://pokeapi.co/api/v2/pokemon/223/>   |   223 |
| reshiram                   | <https://pokeapi.co/api/v2/pokemon/643/>   |   643 |
| reuniclus                  | <https://pokeapi.co/api/v2/pokemon/579/>   |   579 |
| rhydon                     | <https://pokeapi.co/api/v2/pokemon/112/>   |   112 |
| rhyhorn                    | <https://pokeapi.co/api/v2/pokemon/111/>   |   111 |
| rhyperior                  | <https://pokeapi.co/api/v2/pokemon/464/>   |   464 |
| ribombee                   | <https://pokeapi.co/api/v2/pokemon/743/>   |   743 |
| ribombee-totem             | <https://pokeapi.co/api/v2/pokemon/10150/> | 10150 |
| rillaboom                  | <https://pokeapi.co/api/v2/pokemon/812/>   |   812 |
| rillaboom-gmax             | <https://pokeapi.co/api/v2/pokemon/10200/> | 10200 |
| riolu                      | <https://pokeapi.co/api/v2/pokemon/447/>   |   447 |
| rockruff                   | <https://pokeapi.co/api/v2/pokemon/744/>   |   744 |
| rockruff-own-tempo         | <https://pokeapi.co/api/v2/pokemon/10151/> | 10151 |
| roggenrola                 | <https://pokeapi.co/api/v2/pokemon/524/>   |   524 |
| rolycoly                   | <https://pokeapi.co/api/v2/pokemon/837/>   |   837 |
| rookidee                   | <https://pokeapi.co/api/v2/pokemon/821/>   |   821 |
| roselia                    | <https://pokeapi.co/api/v2/pokemon/315/>   |   315 |
| roserade                   | <https://pokeapi.co/api/v2/pokemon/407/>   |   407 |
| rotom                      | <https://pokeapi.co/api/v2/pokemon/479/>   |   479 |
| rotom-fan                  | <https://pokeapi.co/api/v2/pokemon/10011/> | 10011 |
| rotom-frost                | <https://pokeapi.co/api/v2/pokemon/10010/> | 10010 |
| rotom-heat                 | <https://pokeapi.co/api/v2/pokemon/10008/> | 10008 |
| rotom-mow                  | <https://pokeapi.co/api/v2/pokemon/10012/> | 10012 |
| rotom-wash                 | <https://pokeapi.co/api/v2/pokemon/10009/> | 10009 |
| rowlet                     | <https://pokeapi.co/api/v2/pokemon/722/>   |   722 |
| rufflet                    | <https://pokeapi.co/api/v2/pokemon/627/>   |   627 |
| runerigus                  | <https://pokeapi.co/api/v2/pokemon/867/>   |   867 |
| sableye                    | <https://pokeapi.co/api/v2/pokemon/302/>   |   302 |
| sableye-mega               | <https://pokeapi.co/api/v2/pokemon/10066/> | 10066 |
| salamence                  | <https://pokeapi.co/api/v2/pokemon/373/>   |   373 |
| salamence-mega             | <https://pokeapi.co/api/v2/pokemon/10089/> | 10089 |
| salandit                   | <https://pokeapi.co/api/v2/pokemon/757/>   |   757 |
| salazzle                   | <https://pokeapi.co/api/v2/pokemon/758/>   |   758 |
| salazzle-totem             | <https://pokeapi.co/api/v2/pokemon/10129/> | 10129 |
| samurott                   | <https://pokeapi.co/api/v2/pokemon/503/>   |   503 |
| sandaconda                 | <https://pokeapi.co/api/v2/pokemon/844/>   |   844 |
| sandaconda-gmax            | <https://pokeapi.co/api/v2/pokemon/10209/> | 10209 |
| sandile                    | <https://pokeapi.co/api/v2/pokemon/551/>   |   551 |
| sandshrew                  | <https://pokeapi.co/api/v2/pokemon/27/>    |    27 |
| sandshrew-alola            | <https://pokeapi.co/api/v2/pokemon/10101/> | 10101 |
| sandslash                  | <https://pokeapi.co/api/v2/pokemon/28/>    |    28 |
| sandslash-alola            | <https://pokeapi.co/api/v2/pokemon/10102/> | 10102 |
| sandygast                  | <https://pokeapi.co/api/v2/pokemon/769/>   |   769 |
| sawk                       | <https://pokeapi.co/api/v2/pokemon/539/>   |   539 |
| sawsbuck                   | <https://pokeapi.co/api/v2/pokemon/586/>   |   586 |
| scatterbug                 | <https://pokeapi.co/api/v2/pokemon/664/>   |   664 |
| sceptile                   | <https://pokeapi.co/api/v2/pokemon/254/>   |   254 |
| sceptile-mega              | <https://pokeapi.co/api/v2/pokemon/10065/> | 10065 |
| scizor                     | <https://pokeapi.co/api/v2/pokemon/212/>   |   212 |
| scizor-mega                | <https://pokeapi.co/api/v2/pokemon/10046/> | 10046 |
| scolipede                  | <https://pokeapi.co/api/v2/pokemon/545/>   |   545 |
| scorbunny                  | <https://pokeapi.co/api/v2/pokemon/813/>   |   813 |
| scrafty                    | <https://pokeapi.co/api/v2/pokemon/560/>   |   560 |
| scraggy                    | <https://pokeapi.co/api/v2/pokemon/559/>   |   559 |
| scyther                    | <https://pokeapi.co/api/v2/pokemon/123/>   |   123 |
| seadra                     | <https://pokeapi.co/api/v2/pokemon/117/>   |   117 |
| seaking                    | <https://pokeapi.co/api/v2/pokemon/119/>   |   119 |
| sealeo                     | <https://pokeapi.co/api/v2/pokemon/364/>   |   364 |
| seedot                     | <https://pokeapi.co/api/v2/pokemon/273/>   |   273 |
| seel                       | <https://pokeapi.co/api/v2/pokemon/86/>    |    86 |
| seismitoad                 | <https://pokeapi.co/api/v2/pokemon/537/>   |   537 |
| sentret                    | <https://pokeapi.co/api/v2/pokemon/161/>   |   161 |
| serperior                  | <https://pokeapi.co/api/v2/pokemon/497/>   |   497 |
| servine                    | <https://pokeapi.co/api/v2/pokemon/496/>   |   496 |
| seviper                    | <https://pokeapi.co/api/v2/pokemon/336/>   |   336 |
| sewaddle                   | <https://pokeapi.co/api/v2/pokemon/540/>   |   540 |
| sharpedo                   | <https://pokeapi.co/api/v2/pokemon/319/>   |   319 |
| sharpedo-mega              | <https://pokeapi.co/api/v2/pokemon/10070/> | 10070 |
| shaymin-land               | <https://pokeapi.co/api/v2/pokemon/492/>   |   492 |
| shaymin-sky                | <https://pokeapi.co/api/v2/pokemon/10006/> | 10006 |
| shedinja                   | <https://pokeapi.co/api/v2/pokemon/292/>   |   292 |
| shelgon                    | <https://pokeapi.co/api/v2/pokemon/372/>   |   372 |
| shellder                   | <https://pokeapi.co/api/v2/pokemon/90/>    |    90 |
| shellos                    | <https://pokeapi.co/api/v2/pokemon/422/>   |   422 |
| shelmet                    | <https://pokeapi.co/api/v2/pokemon/616/>   |   616 |
| shieldon                   | <https://pokeapi.co/api/v2/pokemon/410/>   |   410 |
| shiftry                    | <https://pokeapi.co/api/v2/pokemon/275/>   |   275 |
| shiinotic                  | <https://pokeapi.co/api/v2/pokemon/756/>   |   756 |
| shinx                      | <https://pokeapi.co/api/v2/pokemon/403/>   |   403 |
| shroomish                  | <https://pokeapi.co/api/v2/pokemon/285/>   |   285 |
| shuckle                    | <https://pokeapi.co/api/v2/pokemon/213/>   |   213 |
| shuppet                    | <https://pokeapi.co/api/v2/pokemon/353/>   |   353 |
| sigilyph                   | <https://pokeapi.co/api/v2/pokemon/561/>   |   561 |
| silcoon                    | <https://pokeapi.co/api/v2/pokemon/266/>   |   266 |
| silicobra                  | <https://pokeapi.co/api/v2/pokemon/843/>   |   843 |
| silvally                   | <https://pokeapi.co/api/v2/pokemon/773/>   |   773 |
| simipour                   | <https://pokeapi.co/api/v2/pokemon/516/>   |   516 |
| simisage                   | <https://pokeapi.co/api/v2/pokemon/512/>   |   512 |
| simisear                   | <https://pokeapi.co/api/v2/pokemon/514/>   |   514 |
| sinistea                   | <https://pokeapi.co/api/v2/pokemon/854/>   |   854 |
| sirfetchd                  | <https://pokeapi.co/api/v2/pokemon/865/>   |   865 |
| sizzlipede                 | <https://pokeapi.co/api/v2/pokemon/850/>   |   850 |
| skarmory                   | <https://pokeapi.co/api/v2/pokemon/227/>   |   227 |
| skiddo                     | <https://pokeapi.co/api/v2/pokemon/672/>   |   672 |
| skiploom                   | <https://pokeapi.co/api/v2/pokemon/188/>   |   188 |
| skitty                     | <https://pokeapi.co/api/v2/pokemon/300/>   |   300 |
| skorupi                    | <https://pokeapi.co/api/v2/pokemon/451/>   |   451 |
| skrelp                     | <https://pokeapi.co/api/v2/pokemon/690/>   |   690 |
| skuntank                   | <https://pokeapi.co/api/v2/pokemon/435/>   |   435 |
| skwovet                    | <https://pokeapi.co/api/v2/pokemon/819/>   |   819 |
| slaking                    | <https://pokeapi.co/api/v2/pokemon/289/>   |   289 |
| slakoth                    | <https://pokeapi.co/api/v2/pokemon/287/>   |   287 |
| sliggoo                    | <https://pokeapi.co/api/v2/pokemon/705/>   |   705 |
| slowbro                    | <https://pokeapi.co/api/v2/pokemon/80/>    |    80 |
| slowbro-galar              | <https://pokeapi.co/api/v2/pokemon/10162/> | 10162 |
| slowbro-mega               | <https://pokeapi.co/api/v2/pokemon/10071/> | 10071 |
| slowking                   | <https://pokeapi.co/api/v2/pokemon/199/>   |   199 |
| slowking-galar             | <https://pokeapi.co/api/v2/pokemon/10169/> | 10169 |
| slowpoke                   | <https://pokeapi.co/api/v2/pokemon/79/>    |    79 |
| slowpoke-galar             | <https://pokeapi.co/api/v2/pokemon/10161/> | 10161 |
| slugma                     | <https://pokeapi.co/api/v2/pokemon/218/>   |   218 |
| slurpuff                   | <https://pokeapi.co/api/v2/pokemon/685/>   |   685 |
| smeargle                   | <https://pokeapi.co/api/v2/pokemon/235/>   |   235 |
| smoochum                   | <https://pokeapi.co/api/v2/pokemon/238/>   |   238 |
| sneasel                    | <https://pokeapi.co/api/v2/pokemon/215/>   |   215 |
| snivy                      | <https://pokeapi.co/api/v2/pokemon/495/>   |   495 |
| snom                       | <https://pokeapi.co/api/v2/pokemon/872/>   |   872 |
| snorlax                    | <https://pokeapi.co/api/v2/pokemon/143/>   |   143 |
| snorlax-gmax               | <https://pokeapi.co/api/v2/pokemon/10197/> | 10197 |
| snorunt                    | <https://pokeapi.co/api/v2/pokemon/361/>   |   361 |
| snover                     | <https://pokeapi.co/api/v2/pokemon/459/>   |   459 |
| snubbull                   | <https://pokeapi.co/api/v2/pokemon/209/>   |   209 |
| sobble                     | <https://pokeapi.co/api/v2/pokemon/816/>   |   816 |
| solgaleo                   | <https://pokeapi.co/api/v2/pokemon/791/>   |   791 |
| solosis                    | <https://pokeapi.co/api/v2/pokemon/577/>   |   577 |
| solrock                    | <https://pokeapi.co/api/v2/pokemon/338/>   |   338 |
| spearow                    | <https://pokeapi.co/api/v2/pokemon/21/>    |    21 |
| spectrier                  | <https://pokeapi.co/api/v2/pokemon/897/>   |   897 |
| spewpa                     | <https://pokeapi.co/api/v2/pokemon/665/>   |   665 |
| spheal                     | <https://pokeapi.co/api/v2/pokemon/363/>   |   363 |
| spinarak                   | <https://pokeapi.co/api/v2/pokemon/167/>   |   167 |
| spinda                     | <https://pokeapi.co/api/v2/pokemon/327/>   |   327 |
| spiritomb                  | <https://pokeapi.co/api/v2/pokemon/442/>   |   442 |
| spoink                     | <https://pokeapi.co/api/v2/pokemon/325/>   |   325 |
| spritzee                   | <https://pokeapi.co/api/v2/pokemon/682/>   |   682 |
| squirtle                   | <https://pokeapi.co/api/v2/pokemon/7/>     |     7 |
| stakataka                  | <https://pokeapi.co/api/v2/pokemon/805/>   |   805 |
| stantler                   | <https://pokeapi.co/api/v2/pokemon/234/>   |   234 |
| staraptor                  | <https://pokeapi.co/api/v2/pokemon/398/>   |   398 |
| staravia                   | <https://pokeapi.co/api/v2/pokemon/397/>   |   397 |
| starly                     | <https://pokeapi.co/api/v2/pokemon/396/>   |   396 |
| starmie                    | <https://pokeapi.co/api/v2/pokemon/121/>   |   121 |
| staryu                     | <https://pokeapi.co/api/v2/pokemon/120/>   |   120 |
| steelix                    | <https://pokeapi.co/api/v2/pokemon/208/>   |   208 |
| steelix-mega               | <https://pokeapi.co/api/v2/pokemon/10072/> | 10072 |
| steenee                    | <https://pokeapi.co/api/v2/pokemon/762/>   |   762 |
| stonjourner                | <https://pokeapi.co/api/v2/pokemon/874/>   |   874 |
| stoutland                  | <https://pokeapi.co/api/v2/pokemon/508/>   |   508 |
| stufful                    | <https://pokeapi.co/api/v2/pokemon/759/>   |   759 |
| stunfisk                   | <https://pokeapi.co/api/v2/pokemon/618/>   |   618 |
| stunfisk-galar             | <https://pokeapi.co/api/v2/pokemon/10177/> | 10177 |
| stunky                     | <https://pokeapi.co/api/v2/pokemon/434/>   |   434 |
| sudowoodo                  | <https://pokeapi.co/api/v2/pokemon/185/>   |   185 |
| suicune                    | <https://pokeapi.co/api/v2/pokemon/245/>   |   245 |
| sunflora                   | <https://pokeapi.co/api/v2/pokemon/192/>   |   192 |
| sunkern                    | <https://pokeapi.co/api/v2/pokemon/191/>   |   191 |
| surskit                    | <https://pokeapi.co/api/v2/pokemon/283/>   |   283 |
| swablu                     | <https://pokeapi.co/api/v2/pokemon/333/>   |   333 |
| swadloon                   | <https://pokeapi.co/api/v2/pokemon/541/>   |   541 |
| swalot                     | <https://pokeapi.co/api/v2/pokemon/317/>   |   317 |
| swampert                   | <https://pokeapi.co/api/v2/pokemon/260/>   |   260 |
| swampert-mega              | <https://pokeapi.co/api/v2/pokemon/10064/> | 10064 |
| swanna                     | <https://pokeapi.co/api/v2/pokemon/581/>   |   581 |
| swellow                    | <https://pokeapi.co/api/v2/pokemon/277/>   |   277 |
| swinub                     | <https://pokeapi.co/api/v2/pokemon/220/>   |   220 |
| swirlix                    | <https://pokeapi.co/api/v2/pokemon/684/>   |   684 |
| swoobat                    | <https://pokeapi.co/api/v2/pokemon/528/>   |   528 |
| sylveon                    | <https://pokeapi.co/api/v2/pokemon/700/>   |   700 |
| taillow                    | <https://pokeapi.co/api/v2/pokemon/276/>   |   276 |
| talonflame                 | <https://pokeapi.co/api/v2/pokemon/663/>   |   663 |
| tangela                    | <https://pokeapi.co/api/v2/pokemon/114/>   |   114 |
| tangrowth                  | <https://pokeapi.co/api/v2/pokemon/465/>   |   465 |
| tapu-bulu                  | <https://pokeapi.co/api/v2/pokemon/787/>   |   787 |
| tapu-fini                  | <https://pokeapi.co/api/v2/pokemon/788/>   |   788 |
| tapu-koko                  | <https://pokeapi.co/api/v2/pokemon/785/>   |   785 |
| tapu-lele                  | <https://pokeapi.co/api/v2/pokemon/786/>   |   786 |
| tauros                     | <https://pokeapi.co/api/v2/pokemon/128/>   |   128 |
| teddiursa                  | <https://pokeapi.co/api/v2/pokemon/216/>   |   216 |
| tentacool                  | <https://pokeapi.co/api/v2/pokemon/72/>    |    72 |
| tentacruel                 | <https://pokeapi.co/api/v2/pokemon/73/>    |    73 |
| tepig                      | <https://pokeapi.co/api/v2/pokemon/498/>   |   498 |
| terrakion                  | <https://pokeapi.co/api/v2/pokemon/639/>   |   639 |
| thievul                    | <https://pokeapi.co/api/v2/pokemon/828/>   |   828 |
| throh                      | <https://pokeapi.co/api/v2/pokemon/538/>   |   538 |
| thundurus-incarnate        | <https://pokeapi.co/api/v2/pokemon/642/>   |   642 |
| thundurus-therian          | <https://pokeapi.co/api/v2/pokemon/10020/> | 10020 |
| thwackey                   | <https://pokeapi.co/api/v2/pokemon/811/>   |   811 |
| timburr                    | <https://pokeapi.co/api/v2/pokemon/532/>   |   532 |
| tirtouga                   | <https://pokeapi.co/api/v2/pokemon/564/>   |   564 |
| togedemaru                 | <https://pokeapi.co/api/v2/pokemon/777/>   |   777 |
| togedemaru-totem           | <https://pokeapi.co/api/v2/pokemon/10154/> | 10154 |
| togekiss                   | <https://pokeapi.co/api/v2/pokemon/468/>   |   468 |
| togepi                     | <https://pokeapi.co/api/v2/pokemon/175/>   |   175 |
| togetic                    | <https://pokeapi.co/api/v2/pokemon/176/>   |   176 |
| torchic                    | <https://pokeapi.co/api/v2/pokemon/255/>   |   255 |
| torkoal                    | <https://pokeapi.co/api/v2/pokemon/324/>   |   324 |
| tornadus-incarnate         | <https://pokeapi.co/api/v2/pokemon/641/>   |   641 |
| tornadus-therian           | <https://pokeapi.co/api/v2/pokemon/10019/> | 10019 |
| torracat                   | <https://pokeapi.co/api/v2/pokemon/726/>   |   726 |
| torterra                   | <https://pokeapi.co/api/v2/pokemon/389/>   |   389 |
| totodile                   | <https://pokeapi.co/api/v2/pokemon/158/>   |   158 |
| toucannon                  | <https://pokeapi.co/api/v2/pokemon/733/>   |   733 |
| toxapex                    | <https://pokeapi.co/api/v2/pokemon/748/>   |   748 |
| toxel                      | <https://pokeapi.co/api/v2/pokemon/848/>   |   848 |
| toxicroak                  | <https://pokeapi.co/api/v2/pokemon/454/>   |   454 |
| toxtricity-amped           | <https://pokeapi.co/api/v2/pokemon/849/>   |   849 |
| toxtricity-amped-gmax      | <https://pokeapi.co/api/v2/pokemon/10210/> | 10210 |
| toxtricity-low-key         | <https://pokeapi.co/api/v2/pokemon/10178/> | 10178 |
| toxtricity-low-key-gmax    | <https://pokeapi.co/api/v2/pokemon/10220/> | 10220 |
| tranquill                  | <https://pokeapi.co/api/v2/pokemon/520/>   |   520 |
| trapinch                   | <https://pokeapi.co/api/v2/pokemon/328/>   |   328 |
| treecko                    | <https://pokeapi.co/api/v2/pokemon/252/>   |   252 |
| trevenant                  | <https://pokeapi.co/api/v2/pokemon/709/>   |   709 |
| tropius                    | <https://pokeapi.co/api/v2/pokemon/357/>   |   357 |
| trubbish                   | <https://pokeapi.co/api/v2/pokemon/568/>   |   568 |
| trumbeak                   | <https://pokeapi.co/api/v2/pokemon/732/>   |   732 |
| tsareena                   | <https://pokeapi.co/api/v2/pokemon/763/>   |   763 |
| turtonator                 | <https://pokeapi.co/api/v2/pokemon/776/>   |   776 |
| turtwig                    | <https://pokeapi.co/api/v2/pokemon/387/>   |   387 |
| tympole                    | <https://pokeapi.co/api/v2/pokemon/535/>   |   535 |
| tynamo                     | <https://pokeapi.co/api/v2/pokemon/602/>   |   602 |
| type-null                  | <https://pokeapi.co/api/v2/pokemon/772/>   |   772 |
| typhlosion                 | <https://pokeapi.co/api/v2/pokemon/157/>   |   157 |
| tyranitar                  | <https://pokeapi.co/api/v2/pokemon/248/>   |   248 |
| tyranitar-mega             | <https://pokeapi.co/api/v2/pokemon/10049/> | 10049 |
| tyrantrum                  | <https://pokeapi.co/api/v2/pokemon/697/>   |   697 |
| tyrogue                    | <https://pokeapi.co/api/v2/pokemon/236/>   |   236 |
| tyrunt                     | <https://pokeapi.co/api/v2/pokemon/696/>   |   696 |
| umbreon                    | <https://pokeapi.co/api/v2/pokemon/197/>   |   197 |
| unfezant                   | <https://pokeapi.co/api/v2/pokemon/521/>   |   521 |
| unown                      | <https://pokeapi.co/api/v2/pokemon/201/>   |   201 |
| ursaring                   | <https://pokeapi.co/api/v2/pokemon/217/>   |   217 |
| urshifu-rapid-strike       | <https://pokeapi.co/api/v2/pokemon/10183/> | 10183 |
| urshifu-rapid-strike-gmax  | <https://pokeapi.co/api/v2/pokemon/10219/> | 10219 |
| urshifu-single-strike      | <https://pokeapi.co/api/v2/pokemon/892/>   |   892 |
| urshifu-single-strike-gmax | <https://pokeapi.co/api/v2/pokemon/10218/> | 10218 |
| uxie                       | <https://pokeapi.co/api/v2/pokemon/480/>   |   480 |
| vanillish                  | <https://pokeapi.co/api/v2/pokemon/583/>   |   583 |
| vanillite                  | <https://pokeapi.co/api/v2/pokemon/582/>   |   582 |
| vanilluxe                  | <https://pokeapi.co/api/v2/pokemon/584/>   |   584 |
| vaporeon                   | <https://pokeapi.co/api/v2/pokemon/134/>   |   134 |
| venipede                   | <https://pokeapi.co/api/v2/pokemon/543/>   |   543 |
| venomoth                   | <https://pokeapi.co/api/v2/pokemon/49/>    |    49 |
| venonat                    | <https://pokeapi.co/api/v2/pokemon/48/>    |    48 |
| venusaur                   | <https://pokeapi.co/api/v2/pokemon/3/>     |     3 |
| venusaur-gmax              | <https://pokeapi.co/api/v2/pokemon/10186/> | 10186 |
| venusaur-mega              | <https://pokeapi.co/api/v2/pokemon/10033/> | 10033 |
| vespiquen                  | <https://pokeapi.co/api/v2/pokemon/416/>   |   416 |
| vibrava                    | <https://pokeapi.co/api/v2/pokemon/329/>   |   329 |
| victini                    | <https://pokeapi.co/api/v2/pokemon/494/>   |   494 |
| victreebel                 | <https://pokeapi.co/api/v2/pokemon/71/>    |    71 |
| vigoroth                   | <https://pokeapi.co/api/v2/pokemon/288/>   |   288 |
| vikavolt                   | <https://pokeapi.co/api/v2/pokemon/738/>   |   738 |
| vikavolt-totem             | <https://pokeapi.co/api/v2/pokemon/10122/> | 10122 |
| vileplume                  | <https://pokeapi.co/api/v2/pokemon/45/>    |    45 |
| virizion                   | <https://pokeapi.co/api/v2/pokemon/640/>   |   640 |
| vivillon                   | <https://pokeapi.co/api/v2/pokemon/666/>   |   666 |
| volbeat                    | <https://pokeapi.co/api/v2/pokemon/313/>   |   313 |
| volcanion                  | <https://pokeapi.co/api/v2/pokemon/721/>   |   721 |
| volcarona                  | <https://pokeapi.co/api/v2/pokemon/637/>   |   637 |
| voltorb                    | <https://pokeapi.co/api/v2/pokemon/100/>   |   100 |
| vullaby                    | <https://pokeapi.co/api/v2/pokemon/629/>   |   629 |
| vulpix                     | <https://pokeapi.co/api/v2/pokemon/37/>    |    37 |
| vulpix-alola               | <https://pokeapi.co/api/v2/pokemon/10103/> | 10103 |
| wailmer                    | <https://pokeapi.co/api/v2/pokemon/320/>   |   320 |
| wailord                    | <https://pokeapi.co/api/v2/pokemon/321/>   |   321 |
| walrein                    | <https://pokeapi.co/api/v2/pokemon/365/>   |   365 |
| wartortle                  | <https://pokeapi.co/api/v2/pokemon/8/>     |     8 |
| watchog                    | <https://pokeapi.co/api/v2/pokemon/505/>   |   505 |
| weavile                    | <https://pokeapi.co/api/v2/pokemon/461/>   |   461 |
| weedle                     | <https://pokeapi.co/api/v2/pokemon/13/>    |    13 |
| weepinbell                 | <https://pokeapi.co/api/v2/pokemon/70/>    |    70 |
| weezing                    | <https://pokeapi.co/api/v2/pokemon/110/>   |   110 |
| weezing-galar              | <https://pokeapi.co/api/v2/pokemon/10164/> | 10164 |
| whimsicott                 | <https://pokeapi.co/api/v2/pokemon/547/>   |   547 |
| whirlipede                 | <https://pokeapi.co/api/v2/pokemon/544/>   |   544 |
| whiscash                   | <https://pokeapi.co/api/v2/pokemon/340/>   |   340 |
| whismur                    | <https://pokeapi.co/api/v2/pokemon/293/>   |   293 |
| wigglytuff                 | <https://pokeapi.co/api/v2/pokemon/40/>    |    40 |
| wimpod                     | <https://pokeapi.co/api/v2/pokemon/767/>   |   767 |
| wingull                    | <https://pokeapi.co/api/v2/pokemon/278/>   |   278 |
| wishiwashi-school          | <https://pokeapi.co/api/v2/pokemon/10127/> | 10127 |
| wishiwashi-solo            | <https://pokeapi.co/api/v2/pokemon/746/>   |   746 |
| wobbuffet                  | <https://pokeapi.co/api/v2/pokemon/202/>   |   202 |
| woobat                     | <https://pokeapi.co/api/v2/pokemon/527/>   |   527 |
| wooloo                     | <https://pokeapi.co/api/v2/pokemon/831/>   |   831 |
| wooper                     | <https://pokeapi.co/api/v2/pokemon/194/>   |   194 |
| wormadam-plant             | <https://pokeapi.co/api/v2/pokemon/413/>   |   413 |
| wormadam-sandy             | <https://pokeapi.co/api/v2/pokemon/10004/> | 10004 |
| wormadam-trash             | <https://pokeapi.co/api/v2/pokemon/10005/> | 10005 |
| wurmple                    | <https://pokeapi.co/api/v2/pokemon/265/>   |   265 |
| wynaut                     | <https://pokeapi.co/api/v2/pokemon/360/>   |   360 |
| xatu                       | <https://pokeapi.co/api/v2/pokemon/178/>   |   178 |
| xerneas                    | <https://pokeapi.co/api/v2/pokemon/716/>   |   716 |
| xurkitree                  | <https://pokeapi.co/api/v2/pokemon/796/>   |   796 |
| yamask                     | <https://pokeapi.co/api/v2/pokemon/562/>   |   562 |
| yamask-galar               | <https://pokeapi.co/api/v2/pokemon/10176/> | 10176 |
| yamper                     | <https://pokeapi.co/api/v2/pokemon/835/>   |   835 |
| yanma                      | <https://pokeapi.co/api/v2/pokemon/193/>   |   193 |
| yanmega                    | <https://pokeapi.co/api/v2/pokemon/469/>   |   469 |
| yungoos                    | <https://pokeapi.co/api/v2/pokemon/734/>   |   734 |
| yveltal                    | <https://pokeapi.co/api/v2/pokemon/717/>   |   717 |
| zacian-crowned             | <https://pokeapi.co/api/v2/pokemon/10181/> | 10181 |
| zacian-hero                | <https://pokeapi.co/api/v2/pokemon/888/>   |   888 |
| zamazenta-crowned          | <https://pokeapi.co/api/v2/pokemon/10182/> | 10182 |
| zamazenta-hero             | <https://pokeapi.co/api/v2/pokemon/889/>   |   889 |
| zangoose                   | <https://pokeapi.co/api/v2/pokemon/335/>   |   335 |
| zapdos                     | <https://pokeapi.co/api/v2/pokemon/145/>   |   145 |
| zapdos-galar               | <https://pokeapi.co/api/v2/pokemon/10167/> | 10167 |
| zarude                     | <https://pokeapi.co/api/v2/pokemon/893/>   |   893 |
| zebstrika                  | <https://pokeapi.co/api/v2/pokemon/523/>   |   523 |
| zekrom                     | <https://pokeapi.co/api/v2/pokemon/644/>   |   644 |
| zeraora                    | <https://pokeapi.co/api/v2/pokemon/807/>   |   807 |
| zigzagoon                  | <https://pokeapi.co/api/v2/pokemon/263/>   |   263 |
| zigzagoon-galar            | <https://pokeapi.co/api/v2/pokemon/10171/> | 10171 |
| zoroark                    | <https://pokeapi.co/api/v2/pokemon/571/>   |   571 |
| zorua                      | <https://pokeapi.co/api/v2/pokemon/570/>   |   570 |
| zubat                      | <https://pokeapi.co/api/v2/pokemon/41/>    |    41 |
| zweilous                   | <https://pokeapi.co/api/v2/pokemon/634/>   |   634 |
| zygarde                    | <https://pokeapi.co/api/v2/pokemon/718/>   |   718 |
| zygarde-10                 | <https://pokeapi.co/api/v2/pokemon/10118/> | 10118 |
| zygarde-50                 | <https://pokeapi.co/api/v2/pokemon/10119/> | 10119 |
| zygarde-complete           | <https://pokeapi.co/api/v2/pokemon/10120/> | 10120 |

</div>

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
head(resultsEvolve)
```

<div class="kable-table">

| stageOne   | stageTwo   | stageThree |
|:-----------|:-----------|:-----------|
| bulbasaur  | ivysaur    | venusaur   |
| charmander | charmeleon | charizard  |
| squirtle   | wartortle  | blastoise  |
| caterpie   | metapod    | butterfree |
| weedle     | kakuna     | beedrill   |
| pidgey     | pidgeotto  | pidgeot    |

</div>

``` r
getAllEvolveStages<-function(){
  
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
  
  return(allEvolve)
}

evolveStages<-getAllEvolveStages()
head(evolveStages)
```

<div class="kable-table">

| species    | stages |
|:-----------|:-------|
| bulbasaur  | one    |
| charmander | one    |
| squirtle   | one    |
| caterpie   | one    |
| weedle     | one    |
| pidgey     | one    |

</div>

``` r
str(evolveStages)
```

    ## 'data.frame':    897 obs. of  2 variables:
    ##  $ species: chr  "bulbasaur" "charmander" "squirtle" "caterpie" ...
    ##  $ stages : Ord.factor w/ 4 levels "one"<"two"<"three"<..: 1 1 1 1 1 1 1 1 1 1 ...

## Exploring Data

``` r
allPoke<-getEveryPokeData(basestat = TRUE,type = TRUE)
```

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
