HW2
================
Filip Axelsson
2020-11-05

``` r
# läser in alla bibliotek
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(readxl)
```

### Uppgift 1

\(a\) Uppgiften går ut på att visa hur slutpriset (Soldprice) beror på
boarean (Livingarea), detta gör vi genom att använda “ggplot”
tillsammans med “geom\_line” för att rita upp alla datapunkter med hjälp
utav en linje. För att få ett tydligare svar använder vi även
“geom\_smooth” som kommer tillsätta en modell med formlen y ~ x på
bästa sätt (Enkel linjär regression). Resultatet visas i “Figur 1”. 

\(b\) Uppgiften går ut på att visa hur trenden för pris per kvadratmeter
(Soldprice/Livingarea) utvecklar över en period. För att kunna göra
detta måste vi göra en variabel “ppkvm” genom att använda funktionen
“mutate”, efter det så använder vi “ggplot”, “geom\_line” och
“geom\_smooth” för att visa på bästa sätt hur priset per kvadratmeter
utvecklar sig, detta resultat visas i “Figur 2”. 

\(c\) I denna uppgift ska vi illustera hur en aspekt av data med hjälp
utav en boxplot, “geom\_boxplot”. Jag har valt att visa hur variansen
och det förväntade sålda priset (soldPrice) varierar av antalet rum
(rooms). Det första vi gör är att med hjälp utav “mutate”, gör om
variabeln “rooms” till “character” från “numeric”, efter detta kan vi
göra vår boxplot. Detta resultat visas i “Figur 3”.

``` r
df_booli <- read_csv("../HW_data/Booli_sold.csv") # läser in data
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_double(),
    ##   published = col_datetime(format = ""),
    ##   objectType = col_character(),
    ##   soldDate = col_date(format = ""),
    ##   soldPriceSource = col_character(),
    ##   url = col_character(),
    ##   apartmentNumber = col_character(),
    ##   location.address.streetAddress = col_character(),
    ##   location.position.isApproximate = col_logical(),
    ##   location.region.municipalityName = col_character(),
    ##   location.region.countyName = col_character(),
    ##   source.name = col_character(),
    ##   source.type = col_character(),
    ##   source.url = col_character()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
# a)
# ritar upp data med i form av en linje samt tillsätter en "smooth" linje.
df_booli %>%
  ggplot(aes(x = livingArea, y = soldPrice), ) +
  geom_line(col = "blue") +
  geom_smooth(method = "lm", se = FALSE, col = "black") +
  labs(x = "Boarea", y = "Sålt pris") +
  ggtitle("Figur 1: Hur det sålda priset beror av boarea")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 row(s) containing missing values (geom_path).

![](HW2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# b)
# ritar upp data med i form av en linje samt tillsätter en "smooth" linje. Vi har även gjort om en variabel.
df_booli %>%
  mutate(ppkvm = soldPrice / livingArea) %>% # modiferar en variabel
  ggplot(aes(y = ppkvm, x = soldDate)) +
  geom_line(col = "blue") +
  geom_smooth(method = "lm", se = FALSE, col = "black") +
  labs(x = "Försäljningsdatum", y = "Pris per kvadratmeter") +
  ggtitle("Figur 2: Utveckling av pris per kvadratmeter")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

![](HW2_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
# c)
# visar Var(soldPrice) och E[soldPrice], ritar upp en boxplot.
df_booli %>%
  mutate(rooms = as.character(rooms)) %>%
  ggplot(aes(y = soldPrice, x = rooms)) +
  geom_boxplot() +
  labs(x = "Antal rum", y = "Sålt pris") +
  ggtitle("Figur 3: Boxplot på de sålda priset med hänsyn till antal rum")
```

![](HW2_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

### Uppgift 2

Jag har valt att använda mig av “warnings = FALSE” på grund av att
annars hade vi fått flera sidors felmeddelanden vilket inte hade gett
någon viktig information till läsaren. Till exempel kan felmeddelandet
bestå av att man gör om en kolonn typ från “numeric” till “text”.

\(a\) & \(b\) Med hjälp av paketet “readxl” så kan vi änvända funktionen
“read\_xlsx” för att läsa in excel-filer. För att kunna läsa in alla
flikar i excel-filen så plockar vi fram först alla flikar med hjälp utav
“excel\_sheets”, vi använder sedan “set\_name” för att ge varje flik ett
namn i “R” (annars kommer dessa döpas till 1,2,3…). Det sista vi gör är
att använda oss av “map” som kommer “loopa” funktionen “read\_xlsx” över
alla våra flikar och på det sättet får vi fram tibbles av alla flikar i
excel-filen. 

\(c\) Vi ska nu visa de första och sista 5 raderna i den andra fliken i
excel “Antal avlidna per dag”. Detta med hjälp av “knitr” och “head”.
Eftersom “Datum\_avliden” har sina värden sparade som “datum” i excel
och den sista raden innheåller en sträng så kommer dessa namn läsas in
på ett konstigt sätt. Därav väljer vi att inte läsa in den sista raden
och detta med hjälp av argumentet “n\_max”. För att kunna välja ut de
första och sista 5 elementen använder vi “filter” med argumenten
“row\_number() %in% c(1:5,199:203)” som kommer ta ut alla radnummer
som ingår i listan. 

\(d\) & \(e\) I fliken “Veckodata Kommun\_stadsdel” så kommer kolonnen
“Stadsdel” läsas in som “logical” detta på grund av att det finns
tomma celler i excel, lättast att ändra på detta är genom att tillsätta
argumentet “col\_type” i funktionen “read\_xslx” som då tvinga att läsa
in denna kolonn som “text”. För kolonnerna “tot\_antal\_fall” och
“nya\_fall\_vecka” så läses dessa in som “text” på grund av att de är
sparade som “text” i excel, jag tyckte det var lättast att rätta till
detta genom att lägga till i argumentet “col\_type” att dessa kolonner
skulle vara “numeric”

``` r
path <- "../HW_data/Folkhalsomyndigheten_Covid19.xlsx"

# a) & b)
# tar in alla flikar från excel och spara dom som en tibble.
path %>%
  excel_sheets() %>% # tar fram alla flikar
  set_names() %>% # sätter namn
  map(read_xlsx, path = path) # tar en funktion och "loopar".
```

    ## $`Antal per dag region`
    ## # A tibble: 239 x 23
    ##    Statistikdatum      Totalt_antal_fa… Blekinge Dalarna Gotland Gävleborg
    ##    <dttm>                         <dbl>    <dbl>   <dbl>   <dbl>     <dbl>
    ##  1 2020-02-04 00:00:00                1        0       0       0         0
    ##  2 2020-02-05 00:00:00                0        0       0       0         0
    ##  3 2020-02-06 00:00:00                0        0       0       0         0
    ##  4 2020-02-07 00:00:00                0        0       0       0         0
    ##  5 2020-02-08 00:00:00                0        0       0       0         0
    ##  6 2020-02-09 00:00:00                0        0       0       0         0
    ##  7 2020-02-10 00:00:00                0        0       0       0         0
    ##  8 2020-02-11 00:00:00                0        0       0       0         0
    ##  9 2020-02-12 00:00:00                0        0       0       0         0
    ## 10 2020-02-13 00:00:00                0        0       0       0         0
    ## # … with 229 more rows, and 17 more variables: Halland <dbl>,
    ## #   Jämtland_Härjedalen <dbl>, Jönköping <dbl>, Kalmar <dbl>, Kronoberg <dbl>,
    ## #   Norrbotten <dbl>, Skåne <dbl>, Stockholm <dbl>, Sörmland <dbl>,
    ## #   Uppsala <dbl>, Värmland <dbl>, Västerbotten <dbl>, Västernorrland <dbl>,
    ## #   Västmanland <dbl>, Västra_Götaland <dbl>, Örebro <dbl>, Östergötland <dbl>
    ## 
    ## $`Antal avlidna per dag`
    ## # A tibble: 204 x 2
    ##    Datum_avliden Antal_avlidna
    ##    <chr>                 <dbl>
    ##  1 43901                     1
    ##  2 43902                     0
    ##  3 43903                     1
    ##  4 43904                     1
    ##  5 43905                     2
    ##  6 43906                     2
    ##  7 43907                     1
    ##  8 43908                     6
    ##  9 43909                     7
    ## 10 43910                     9
    ## # … with 194 more rows
    ## 
    ## $`Antal intensivvårdade per dag`
    ## # A tibble: 208 x 2
    ##    Datum_vårdstart     Antal_intensivvårdade
    ##    <dttm>                              <dbl>
    ##  1 2020-03-06 00:00:00                     1
    ##  2 2020-03-07 00:00:00                     1
    ##  3 2020-03-08 00:00:00                     1
    ##  4 2020-03-09 00:00:00                     0
    ##  5 2020-03-10 00:00:00                     2
    ##  6 2020-03-11 00:00:00                     1
    ##  7 2020-03-12 00:00:00                     0
    ##  8 2020-03-13 00:00:00                     2
    ##  9 2020-03-14 00:00:00                     6
    ## 10 2020-03-15 00:00:00                     5
    ## # … with 198 more rows
    ## 
    ## $`Totalt antal per region`
    ## # A tibble: 21 x 5
    ##    Region   Totalt_antal_fa… Fall_per_100000… Totalt_antal_int… Totalt_antal_av…
    ##    <chr>               <dbl>            <dbl>             <dbl>            <dbl>
    ##  1 Blekinge              712             446.                 9               17
    ##  2 Dalarna              2543             883.                67              177
    ##  3 Gotland               330             553.                 7                6
    ##  4 Gävlebo…             3379            1176.                76              168
    ##  5 Halland              2576             772.                41               87
    ##  6 Jämtlan…             1289             985.                20               64
    ##  7 Jönköpi…             5375            1478.                96              183
    ##  8 Kalmar                929             378.                31               64
    ##  9 Kronobe…             1705             846.                25              122
    ## 10 Norrbot…             1750             700.                60               88
    ## # … with 11 more rows
    ## 
    ## $`Totalt antal per kön`
    ## # A tibble: 3 x 4
    ##   Kön           Totalt_antal_fall Totalt_antal_intensivvård… Totalt_antal_avlid…
    ##   <chr>                     <dbl>                      <dbl>               <dbl>
    ## 1 Man                       40380                       1897                3223
    ## 2 Kvinna                    52476                        708                2670
    ## 3 Uppgift sakn…                 7                          0                   0
    ## 
    ## $`Totalt antal per åldersgrupp`
    ## # A tibble: 11 x 4
    ##    Åldersgrupp    Totalt_antal_fa… Totalt_antal_intensivvår… Totalt_antal_avlid…
    ##    <chr>                     <dbl>                     <dbl>               <dbl>
    ##  1 Ålder_0_9                   710                         8                   2
    ##  2 Ålder_10_19                4783                        18                   0
    ##  3 Ålder_20_29               15700                        97                  10
    ##  4 Ålder_30_39               14469                       119                  19
    ##  5 Ålder_40_49               15143                       289                  45
    ##  6 Ålder_50_59               16129                       661                 166
    ##  7 Ålder_60_69                9166                       781                 407
    ##  8 Ålder_70_79                6259                       515                1268
    ##  9 Ålder_80_89                6822                       113                2441
    ## 10 Ålder_90_plus              3661                         4                1535
    ## 11 Uppgift saknas               21                         0                   0
    ## 
    ## $`Veckodata Region`
    ## # A tibble: 693 x 10
    ##    veckonummer Region Antal_fall_vecka Kum_antal_fall Antal_intensivv…
    ##          <dbl> <chr>             <dbl>          <dbl>            <dbl>
    ##  1           6 Bleki…                0              0                0
    ##  2           7 Bleki…                0              0                0
    ##  3           8 Bleki…                0              0                0
    ##  4           9 Bleki…                0              0                0
    ##  5          10 Bleki…                0              0                0
    ##  6          11 Bleki…               10             10                0
    ##  7          12 Bleki…                2             12                0
    ##  8          13 Bleki…                9             21                1
    ##  9          14 Bleki…               15             36                1
    ## 10          15 Bleki…                6             42                0
    ## # … with 683 more rows, and 5 more variables: Kum_antal_intensivvårdade <dbl>,
    ## #   Antal_avlidna_vecka <dbl>, Kum_antal_avlidna <dbl>,
    ## #   Antal_fall_100000inv_vecka <dbl>, Kum_fall_100000inv <dbl>
    ## 
    ## $`Veckodata Kommun_stadsdel`
    ## # A tibble: 10,626 x 9
    ##    veckonummer KnKod KnNamn Stadsdel Kommun_stadsdel tot_antal_fall_…
    ##          <dbl> <chr> <chr>  <lgl>    <chr>                      <dbl>
    ##  1           6 1440  Ale    NA       Ale                            0
    ##  2           7 1440  Ale    NA       Ale                            0
    ##  3           8 1440  Ale    NA       Ale                            0
    ##  4           9 1440  Ale    NA       Ale                            0
    ##  5          10 1440  Ale    NA       Ale                            0
    ##  6          11 1440  Ale    NA       Ale                           NA
    ##  7          12 1440  Ale    NA       Ale                           NA
    ##  8          13 1440  Ale    NA       Ale                           NA
    ##  9          14 1440  Ale    NA       Ale                            6
    ## 10          15 1440  Ale    NA       Ale                            9
    ## # … with 10,616 more rows, and 3 more variables: antal_fall_per10000_inv <dbl>,
    ## #   tot_antal_fall <chr>, nya_fall_vecka <chr>
    ## 
    ## $`FOHM 30 Sep 2020`
    ## # A tibble: 1 x 1
    ##   Information                                                                   
    ##   <chr>                                                                         
    ## 1 Data uppdateras vardagar kl 14.00 med data fram till föregående dag. Veckodat…

``` r
# c)
# Visar de första och sista 5 elmenten i fliken "Antal avlidna per dag",
# där det sista elemet i excel filen är borttagen.
path %>%
  read_xlsx(sheet = "Antal avlidna per dag", n_max = 203) %>%
  filter(row_number() %in% c(1:5, 199:203)) %>%
  head(n = 10) %>%
  kable(
    col.names = c("Datum avliden", "Antal avlidna"),
    caption = "Tabell 1: Antalet avlidna för specifikt datum "
  )
```

| Datum avliden | Antal avlidna |
| :------------ | ------------: |
| 2020-03-11    |             1 |
| 2020-03-12    |             0 |
| 2020-03-13    |             1 |
| 2020-03-14    |             1 |
| 2020-03-15    |             2 |
| 2020-09-25    |             3 |
| 2020-09-26    |             2 |
| 2020-09-27    |             0 |
| 2020-09-28    |             0 |
| 2020-09-29    |             0 |

Tabell 1: Antalet avlidna för specifikt datum

``` r
# d) & e)

# Öppnar upp och läser in fliken "Veckodata Kommun_stadsdel",
# och ändrar "stadsdel" till "text" samt "tot_antal_fall" & "nya_fall_vecka, till "numeric"
path %>%
  read_xlsx(sheet = 8, col_types = c(rep("guess", 3), "text", rep("guess", 3), "numeric", "numeric"))
```

    ## # A tibble: 10,626 x 9
    ##    veckonummer KnKod KnNamn Stadsdel Kommun_stadsdel tot_antal_fall_…
    ##          <dbl> <chr> <chr>  <chr>    <chr>                      <dbl>
    ##  1           6 1440  Ale    <NA>     Ale                            0
    ##  2           7 1440  Ale    <NA>     Ale                            0
    ##  3           8 1440  Ale    <NA>     Ale                            0
    ##  4           9 1440  Ale    <NA>     Ale                            0
    ##  5          10 1440  Ale    <NA>     Ale                            0
    ##  6          11 1440  Ale    <NA>     Ale                           NA
    ##  7          12 1440  Ale    <NA>     Ale                           NA
    ##  8          13 1440  Ale    <NA>     Ale                           NA
    ##  9          14 1440  Ale    <NA>     Ale                            6
    ## 10          15 1440  Ale    <NA>     Ale                            9
    ## # … with 10,616 more rows, and 3 more variables: antal_fall_per10000_inv <dbl>,
    ## #   tot_antal_fall <dbl>, nya_fall_vecka <dbl>

### Uppgift 3

\(a\) Med hjälp utav “summarise”, “across” och “where” ska vi beräkna
det totala antalen fall per region och det totala antelet fall.
“Summarise” kommer skapa en ny kolonn, across kommer göra det över
alla kolonner och “where” kommer ta ut endast där det finns typerna är
“numeric”. Vi ser i “Tabell 2” att Gotland har lägst antal fall medan
Stockholm har flest antal fall. Genom att endast kolla på det totala
analet fall och jämföra regioner kanske inte är det smartaste, då varje
regionen varierar i population, som exempel en stad med 1000st invånare
där 10st är smittade,det är alltså 1% smittade, medan en annan stad med
100st invånare där 5st är smittade, det är 5% smittade. Skulle vi endast
jämföra det totala antalet smittade så skulle vi ha dragit slutsatsen
att staden med 1000st invånare är värst drabbad men i detta fall är det
staden med 100st invånare. 

\(b\) Vi ska nu plotta det totala antalet smittade sedan den 15 Mars.
“Filter” med argumentet “Statistikdatum \>=”2020-03-15" låter oss ta
ut endast den sökta datan, vi plottar sedan på vanligt sätt med
“ggplot”. Detta visas i “Figur 4”. Då jag var osäker på om man
skulle beräkna kumulativa summor, det vill säga vi hade en vektor med
totala fall \[1,2,3,0\] så hade den blivit \[1,3,6,6\], detta visas i
“Figur 5” och vi har kunnat göra denna plot med hjälp utav “mutate”
med argumenet “cumsum” som beräknar kumulativa summor. 

\(c\) Vi ska här skapa en plott med hjälp utav “geom\_col” för
variablerna “tot\_antal\_fall” och “nya\_fall\_vecka” för respektive
veckonummer. Det första jag gör är att välja ut just dessa två variabler
med hjälp utav “select”, jag gör detta för att sedan på ett smidigt sätt
använda “gather” som kommer samla ihop all data i en ny lista. Vi vill
göra detta eftersom vi har två “y-värden” att ta hänsyn och med hjälp
utav gather samlar vi alla “y-värden” i en kolonn och i den andra
kolonnen så beskriv vart det specifika värdet kommer ifrån. Vi kommer
alltså ha en kolonn där alla antal fall finns, och i den andra finns
vart det värdet kommer ifrån, det vill säga om värdet tillhör variabeln
“tot\_antal\_fall” eller “nya\_fall\_vecka”. När vi plottar detta så
kommer vi ange argumentet “fill=Type” vilket kommer rita ut hur värderna
förhåller sig till respektive variabel. (Jag valde att göra alla värden
till “numeric”, då det är allt vi behöver.) Detta visas i “Figur 6”. 

\(e\) Vi har ungefär 10800 rader i excelfilen medan i vår plott har vi
endast 28 x-värden, det “ggplot” har gjort är att summera alla
\(y\)-värden för respektive x-värde. Det vill säga för till exempel
veckonummer 20 så har “ggplot” räknat ihop alla regioners totala antal
fall och antalet nya fall för att sedan plottat det. Jag vill även
notera att det förekommer värden “\<15” i excel-filen, dessa har jag
inget ändrat eller gjort något åt, därav kommer dessa räknas med när
“ggplot” summerar ihop allt.

``` r
# a)
# beräknar totala antalet fall per region samt det totala.
path <- "../HW_data/Folkhalsomyndigheten_Covid19.xlsx"
path %>%
  read_xlsx(sheet = 1, ) %>%
  summarise(across(where(is.numeric), sum)) %>%
  gather() %>%
  head(n = 22) %>%
  kable(
    col.names = c("Region", "Totalt antal fall"),
    caption = "Tabell 2: Det totala antal fall"
  )
```

| Region               | Totalt antal fall |
| :------------------- | ----------------: |
| Totalt\_antal\_fall  |             92863 |
| Blekinge             |               712 |
| Dalarna              |              2543 |
| Gotland              |               330 |
| Gävleborg            |              3379 |
| Halland              |              2576 |
| Jämtland\_Härjedalen |              1289 |
| Jönköping            |              5375 |
| Kalmar               |               929 |
| Kronoberg            |              1705 |
| Norrbotten           |              1750 |
| Skåne                |              5861 |
| Stockholm            |             25146 |
| Sörmland             |              2521 |
| Uppsala              |              4072 |
| Värmland             |              1275 |
| Västerbotten         |              1030 |
| Västernorrland       |              1919 |
| Västmanland          |              3121 |
| Västra\_Götaland     |             20247 |
| Örebro               |              2991 |
| Östergötland         |              4092 |

Tabell 2: Det totala antal fall

``` r
# b)
# skapar en plott för totala antalet smittade från den 15 Mars
path %>%
  read_xlsx(sheet = 1, ) %>%
  filter(Statistikdatum >= "2020-03-15") %>%
  ggplot(aes(x = Statistikdatum, y = Totalt_antal_fall)) +
  geom_line(col = "blue") +
  labs(x = "Datum", y = "Totalt antal fall") +
  ggtitle("Figur 4: Utveckling av det totala antal fall per dag från 2020-03-15")
```

![](HW2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
path %>%
  read_xlsx(sheet = 1, ) %>%
  mutate(totalt = cumsum(Totalt_antal_fall)) %>% # cumsum gör kumulativa summor
  filter(Statistikdatum >= "2020-03-15") %>%
  ggplot(aes(x = Statistikdatum, y = totalt)) +
  geom_line(col = "blue") +
  labs(x = "Datum", y = "Totalt antal fall") +
  ggtitle("Figur 5: Utveckling av det totala antal fall per dag från 2020-03-15, kumulativt")
```

![](HW2_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# c)
path %>%
  read_xlsx(sheet = 8, col_type = c("numeric")) %>%
  select("totalt antal fall" = tot_antal_fall, "nya fall vecka" = nya_fall_vecka, veckonummer) %>% # väljer ut variabler
  gather("Type", "Value", -veckonummer) %>% # skapar en ny lista
  ggplot(aes(x = veckonummer, y = Value, fill = Type)) +
  geom_col(position = "stack") +
  labs(x = "Veckonummer", y = "Antal fall") +
  ggtitle("Figur 6: Det totala antalet fall och antalet nya fall per vecka")
```

![](HW2_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->
