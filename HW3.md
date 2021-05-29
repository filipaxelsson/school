HW3
================
Filip Axelsson
2020-11-05

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(RSQLite)
library(leaflet)
library(styler)
library(mapview)
```

    ## GDAL version >= 3.1.0 | setting mapviewOptions(fgb = TRUE)

### Uppgift 1

\(a\)

Vi ska här visualisera antalet “klick” som är gjord per dag, vi läser
först in vår data med hjälp utav “dbReadTable”. När vi har läst in vår
data, så måste vi göra om alla datum till “date”, vilket vi gör med
“mutate”, vi plottar sedan som vanligt med “ggplot” där vi använder
“geom\_bar”.

``` r
con <- dbConnect(RSQLite::SQLite(), "../HW_data/seo_marketing_data.sqlite")
# List all available tables
dbListTables(con)
```

    ## [1] "Clicks"           "Conversion_value"

``` r
# Tables are "Conversion value" and "Clicks"
# Pull all data in a table to R.
df_clicks <- dbReadTable(con, "Clicks")

df_clicks <- df_clicks %>%
  mutate(
    date = as.Date(date) # gör om date från "text" till "date"
    )

# a)
df_clicks %>%
  ggplot(aes(x = date)) +
  geom_bar(col = "cyan") +
  labs(x = "Datum", y = "Antal clicks") +
  ggtitle("Figur 1: Antal clicks per dag")
```

![](HW3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

\(b\)

Givet att varje “klick” motsvarar en kostnad, så ska vi ta reda på
vilken dag i veckan som är mest kostsam. Vi kommer göra detta genom att
först lägga till en ny variabel “day” som representerar vilken dag som
“klicket” utförs på. Sista momentet är endast att räkna hur många
gånger samma dag listas, vi gör detta med “count()”. Detta resultat
visas i “Tabell 1”, vi ser att en måndagar generar högst konstad.

``` r
# b)
df_clicks %>%
  mutate(day = wday(date, label = TRUE)) %>% # gör en kolonn som representerar veckodag
  group_by(day) %>%
  count() %>% # räknar alla gånger varje veckodag listas.
  head(n = 7) %>%
  kable(
    col.names = c("Dag", "Antal clicks"),
    caption = "Tabell 1: Totalt antal clicks per veckodag."
  )
```

| Dag | Antal clicks |
| :-- | -----------: |
| Sön |          538 |
| Mån |          540 |
| Tis |          392 |
| Ons |          233 |
| Tor |          234 |
| Fre |          321 |
| Lör |          470 |

Tabell 1: Totalt antal clicks per veckodag.

\(c\)

Vi ska nu göra ett histogram över värderna som kunderna har angivet. Vi
gör detta med att först läsa in vår data “Conversion\_value” och sedan
plottar som vanligt med “ggplot” med “geom\_histogram”, resultatet visas
i “Figur 2”

``` r
# c)
df_cv <- dbReadTable(con, "Conversion_value")

df_cv %>%
  ggplot(aes(x = value)) +
  geom_histogram(col = "blue") +
  labs(x = "Värde", y = "Antal kunder") +
  ggtitle("Figur 2: Antalet kunder som tillhandahåller ett värde.")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](HW3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> \(d\)

Vi ska generara “Conversion lag” (CL) för en kund, som är ett nummret av
antal dager från att kunden har “klickat” på en annons tills att kunden
har köpt det som annonserades. Vi ska sedan plotta fördelningen för CL.
Vi kan göra detta genom att först kombinera de två olika dataseten
“clicks” och “Conversion value”, detta görs med “inner\_join” vi
använder även argumentet “c(”id“=”id“)” vilket tar ut alla matchande
“id” och binder ihop allt till en ny data. Det vi även gör är att med
“mutate” göra om variabeln “date\_cv” till “date” från “character” och
sedan skapar en ny variabel “CL” som räknar ut skillnaden från första
klicket och det andra. Vi plottar sedan fördelningen med hjälp utav
“geom\_histogram” och “geom\_density” . Vi väljer dock en annat sätt
att beräkna CL, vi använder SQL, vi det är egentligen samma process som
ovan, förutom en annat “språk” på kodningen. Vi väljer ut (“SELECT”()
skillnaden mellan de olika datumen på clicken genom “JULIANDAY”, sen med
“INNER JOIN” som gör exakt samma sak som beskrivit ovan, vi väljer ut
skillnanden där “id” finns i båda listorna. Plottar sedan på sättet som
är beskrivet ovan. Resultatet visas i “Figur 3”.

``` r
# d)
df_combinded <- df_clicks %>%
  inner_join(df_cv, c("id" = "id"), suffix = c("_click", "_cv")) %>% # df där alla har clickat 2 ggr
  mutate(
    date_cv = as.Date(date_cv), # gör alla datum till "date"
    CL = julian(date_cv) - julian(date_click) # beräknar tiden till det andra clicket
  )

for_density <-dbGetQuery(con,'
SELECT JULIANDAY(Conversion_value.date)-JULIANDAY(Clicks.date) AS CL
FROM Clicks
INNER JOIN Conversion_value ON Clicks.id=Conversion_value.id;
           ')

for_density %>% 
  ggplot(aes(x = CL)) +
  geom_histogram(aes(y = ..density..), col = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(x = "Conversion lag") +
  ggtitle("Figur 3: Fördelning för \"Conversion lag\"")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](HW3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

\(e\) Vi ska visa två tabeller, en som visar antalet “clicks” och den
andra som visar medelvärdet av “conversion value”. Vi gör detta med
“pivot\_wider” i båda fallen, det som skiljer sig åt är “values\_fn”
där vi använder “mean” för att beräkna medelvärdet och “length” för att
beräkna antalet “clicks”. Detta resultat visas i “Tabell 2” och “Tabell
3”.

(Jag har även gjort en kod utifrån det som vi har använt i DataCamp,
dock verkar vissa funktioner vara “dödende”, men jag har ändå valt att
visa koden nedanför, inget skrivs ut eller används.)

``` r
# e)

df_combinded %>% 
  select(date_click,CL,value) %>%
  filter(CL>=25) %>%
  pivot_wider(names_from = CL, 
              values_from = value, 
              names_sort = TRUE, 
              values_fill = 0,
              values_fn = length) %>% #beräknar antalet element, dvs clicks.
  arrange(date_click) %>% 
  tail(n=5) %>% 
    kable(
      col.names = c("Date of click", "CL-25 days", "CL-26 days", "CL-27 days", "CL-28 days",
    "CL-29 days", "CL-30 days"),
      caption = "Tabell 2: Antalet som har \"klickat\" efter ett specifikt CL"
    )
```

| Date of click | CL-25 days | CL-26 days | CL-27 days | CL-28 days | CL-29 days | CL-30 days |
| :------------ | ---------: | ---------: | ---------: | ---------: | ---------: | ---------: |
| 2020-08-28    |          0 |          0 |          1 |          2 |          0 |          0 |
| 2020-08-29    |          0 |          1 |          1 |          2 |          1 |          1 |
| 2020-08-30    |          0 |          0 |          0 |          0 |          2 |          1 |
| 2020-08-31    |          0 |          0 |          0 |          0 |          1 |          1 |
| 2020-09-01    |          0 |          0 |          0 |          1 |          0 |          0 |

Tabell 2: Antalet som har “klickat” efter ett specifikt CL

``` r
df_combinded %>% 
  select(date_click,value,CL) %>% # tar alla önskade variabler
  filter(CL>=25) %>% # alla dagar från CL 25
  pivot_wider(names_from = CL, # tar namnen från CL
              values_from = value, # värden från value
              names_sort = TRUE, # samma namn upprepas inte.
              values_fill = 0, # värden med NA fylls med 0
              values_fn = mean) %>%  #beräknar medelvärdet
  arrange(date_click) %>% 
  tail(n=5) %>% 
    kable(
      col.names = c("Date of click", "CL-25 days", "CL-26 days", "CL-27 days", "CL-28 days",
    "CL-29 days", "CL-30 days"),
      caption = "Tabell 3: Medelvärdet för de som har \"klickat\" efter ett specifikt CL"
    )
```

| Date of click | CL-25 days | CL-26 days | CL-27 days | CL-28 days | CL-29 days | CL-30 days |
| :------------ | ---------: | ---------: | ---------: | ---------: | ---------: | ---------: |
| 2020-08-28    |          0 |   0.000000 |   5.689383 |  12.237238 |   0.000000 |   0.000000 |
| 2020-08-29    |          0 |   6.652542 |  11.286285 |  10.244544 |   6.850705 |   1.405680 |
| 2020-08-30    |          0 |   0.000000 |   0.000000 |   0.000000 |   7.675008 |  12.248421 |
| 2020-08-31    |          0 |   0.000000 |   0.000000 |   0.000000 |   7.414132 |   5.019909 |
| 2020-09-01    |          0 |   0.000000 |   0.000000 |   8.704662 |   0.000000 |   0.000000 |

Tabell 3: Medelvärdet för de som har “klickat” efter ett specifikt
CL

``` r
### Kod som visar att man kan göra på ett annat sätt, som vi har lärt oss i DataCamp, där vissa funktioner är "döende". 

# df_combinded %>%
# group_by(date_click, CL) %>%
# filter(CL >= 25) %>% # tar ut alla som har clickat igen efter minst 25 dagar
# tally() %>% # beräknar hur många gånger samma "CL" uppkommer
# spread(CL, n, fill = 0) %>% # gör tabellen "wide" med "CL" = key och "n" = value
# tail(n = 5) %>%
# kable(col.names = c(
# "Date of click", "CL-25 days", "CL-26 days", "CL-27 days", "CL-28 days",
# "CL-29 days", "CL-30 days"
# ))





# df_combinded %>%
# filter(CL >= 25) %>% # tar ut alla som har clickat igen efter minst 25 dagar
# spread(CL, value) %>% # gör tabellen "wide" med "CL" = key och "value" = value
# group_by(date_click) %>%
# summarise_at(vars(-id, -date_cv, -adgroup),
#             mean,
#            na.rm = TRUE) %>% # beräknar medelvärdet
# replace(is.na(.), 0) %>% # byter ut alla NA mot 0
# tail(n = 5) %>% # tar ut de sista 5 raderna
# kable(col.names = c(
# "Date of click", "CL-25 days", "CL-26 days", "CL-27 days", "CL-28 days",
# "CL-29 days", "CL-30 days"
# ))
```

### Uppgift 2

\(a\)

“sites” - är en gruppering av “StopAreas” som ska användas för att
förenkla sökningen i reseplaneraren

“stopAreas” - är en gruppering av “stopPoints” med samma trafikslag och
namn inom ett avgränsat geografiskt område, exempelvis terminal.

“stopPoints” - stopställen och ingår i “stopAreas”

“lines” - linjer annoserad mot resenär

“journeyPatterns” - en unik körväg för en linje

“trasportmodes” - trafikslag

Man kan egentilgen säga att alla datamängder hänger ihop med varandra
mer eller mindre, vi ser att i till exempel “sites” så har man grupperat
“StopAreas” för att göra det enklare att använda datamängden för
reseplaneraren istället för att använda hela datamängden “stopPoints”
eller “stopArea”. I “journeyPatterns” har man endast listat de aktiva
linjerna och åt vilken rikting de åker, “transportmodes” har man listat
trafikslagen. Om man till exempel kombinerar “journeyPatterns” med
“stopAreas” eller “stopPoints” kan man plocka ut de aktiva linjerna
och se i vilka zoner respektive linje tillhör, vilket vi även kommer
göra senare.

\(b\)

Det jag har gjort är för varje tabell är att ha använt “mutate” och
argumentet “as.numeric” för att göra om de variabler som önskades till
“numeric”.

``` r
con <- dbConnect(RSQLite::SQLite(), "../HW_data/sl-api.sqlite")
# List all available tables
dbListTables(con)
```

    ## [1] "journeyPatterns" "lines"           "sites"           "stopAreas"      
    ## [5] "stopPoints"      "transportmodes"

``` r
# Tables are "journeyPatterns", "lines", "sites" , "stopAreas", "stopPoints" and "transportmodes"
# Pull all data in a table to R.
journeyPatterns <- dbReadTable(con, "journeyPatterns")
lines <- dbReadTable(con, "lines")
sites <- dbReadTable(con, "sites")
stopAreas <- dbReadTable(con, "stopAreas")
stopPoints <- dbReadTable(con, "stopPoints")
Transport <- dbReadTable(con, "transportmodes")

# b)
# convertring the values into nummeric

sites <- sites %>%
  mutate(
    SiteId = as.numeric(SiteId),
    StopAreaNumber = as.numeric(StopAreaNumber)
  )

stopAreas <- stopAreas %>%
  mutate(
    StopPointNumber = as.numeric(StopPointNumber),
    StopAreaNumber = as.numeric(StopAreaNumber)
  )

stopPoints <- stopPoints %>%
  mutate(
    StopPointNumber = as.numeric(StopPointNumber),
    StopAreaNumber = as.numeric(StopAreaNumber)
  )
```

\(c\)

Vi ska skapa en tabell över antalet aktiva unika stop som genomförs på
en järnväg, det vill säga alla stop tåg, spårvagn och tunnelbannor gör.
När vi säger “aktiva” så referas det till stop som är en del av linjens
resemönster. Det första vi måste göra är att skapa en ny data där vi tar
ut endast de aktiva stoppen, detta gör vi med “inner\_join” som kommer
ta ut alla “JourneyPatternPointNumber” och “StopPointNumber” som matchar
varandra och sedan binda ihop dataseten “journeyPatterns” och
“stopArea”. Nästa steg blir att ta ut endast de transportsätten som
färdas på järnväg, detta gör vi med “filter”. Vi räknar sedan elementen
i respektive zone med hjälp utav “tally()”. Då det finns stop som inte
tillhör någon zone så har jag valt att inte visa dessa, detta genom att
endast visa de 3 sista elementen i vår tabell. Jag har även valt att
räkna precis alla stopp, om tar till exempel tunnelbanan, på TC stoppar
7 st linjer i en riktning, jag räknar även när de åker tillbaka så
totalt görs det 14 stopp på TC. Detta resultat visas nedanför i “Tabell
4”.

``` r
# c)

Journey_Stop <- journeyPatterns %>%
  mutate(JourneyPatternPointNumber = as.numeric(JourneyPatternPointNumber)) %>% 
  inner_join(stopAreas, c("JourneyPatternPointNumber" = "StopPointNumber")) 



Journey_Stop %>%
  filter(StopAreaTypeCode %in% c("METROSTN", "TRAMSTN", "RAILWSTN")) %>%
  group_by(ZoneShortName) %>%
  tally() %>%
  tail(n = 3) %>%
  kable(
    caption = "Tabell 4: Totalt antal stop i respektive zone för tåg, spårvagn och tunnelbana",
    col.names = c("Zone", "Antal stopp")
  )
```

| Zone | Antal stopp |
| :--- | ----------: |
| A    |         742 |
| B    |         226 |
| C    |          82 |

Tabell 4: Totalt antal stop i respektive zone för tåg, spårvagn och
tunnelbana

\(d\)

Vi ska nu välja en linje, och sedan plotta stoppen på en karta med
namnen för respektive stop. Jag kommer skriva en kod där jag enkelt kan
använda koden igen för en annan linje. genom att använda en hårdkodad
dataset kan vi enkelt välja ut det vi är ute efter det vill säga
longitud och latitud med respektive namn för stoppen. Vi väljer enklast
ut vilken linje vi är ute efter med “filter”. Eftersom våra latitud och
longitud värden inte är “numeric” gör vi om dessa med “mutate”, vi
väljer sedan ut variablerna “lon”, “lat” och “StopPointName” för att
kunna plotta kortan med “leaflet”.

``` r
# d)

route_of_line <- function(line) {
  data_for_line <- Journey_Stop %>% 
      filter(LineNumber == as.character(line)) %>%
      mutate(lon = as.numeric(LocationEastingCoordinate),
             lat = as.numeric(LocationNorthingCoordinate)) %>%
      select(lon,lat,StopPointName)

  m <- leaflet(data_for_line) %>%
    addTiles() %>%
    addMarkers(lng = ~lon, lat = ~lat, popup = ~StopPointName)
  return(m)
}


map_route <- route_of_line(10)
mapshot(map_route, file = "Leaflet-plot.png")
knitr::include_graphics("Leaflet-plot.png")
```

<img src="Leaflet-plot.png" width="992" />

\(e\)

stopAreas och stopPoints, vad har dessa två datamängder gemensamt, har
de några onödiga kolonner? Det första vi gör är att kolla om detta två
datamängder är identiska, vilket vi gör med “all\_equal()” och ser där
att det är korrekt. Redan där kan vi inse att det är onödigt att spara
två identiska datamängder med olika namn då det endast tar utrymme. Det
vi nu istället kan göra är att dela upp dessa datamängder, då är det
dock en preferenssak med att välja vilka variabler som ska tillhöra
vilken datamängd. Jag har valt att endast ta ut namnen hållplatserna, de
identifikationsnummerna för hållplatserna, vilken zone de tillhör och
vilken typ av transportmedel som stannar där. Efter vi har valt dessa
variabler så kan vi endast ta de unika elementen med “unique()” för att
slippa alla dubbleter och reducera vår datamängd ännumer. Resterande
infromation plus de identifikationsnummerna för hållplatserna behålls i
den andra datamängden. Detta för att vi ska kunna binda samman dom sen
igen om det önskas. Vi kan se nedan för att från början så tar den
urspungliga datamängden upp 3.52 MB, men efter uppdelning och reducering
av element, så tar “split\_stopAreas” 599 kB och “split\_stopPoints”
2.81 MB vilket då blir totalt 3.409 MB. Vi har alltså lyckats dela upp
dessa två datamängder och sparat utrymme i R, om vi nu använder
“inner\_join()” så kommer vi få tillbaka all infromation, men även
storleken på datamängden, det vill säga 3.52 MB. Vi kan även resonera
att till exempel när variablerna “LastModifiedUtcDateTime” och
“ExistsFromDate” är onödiga, då dessa också är identiska, dock skulle
man eventuellt kunna ha kvar “LastModifiedUtcDateTime” ifall ändringar
skulle göras. Det går även att argumentera att vi endast behövde ha kvar
antigen “stopAreas” eller “stopPoints” för att spara utrymme och
struntat i att ha delat upp dessa i två olika datamängder, som vi
beskrev ovanför.

``` r
# Kollar om stopAreas och stopPoints är identiska
all_equal(stopPoints, stopAreas)
```

    ## [1] TRUE

``` r
# Splittar upp vår datamängd.
split_stopPoints <- stopAreas %>%
  select(StopPointNumber,LocationNorthingCoordinate,LocationEastingCoordinate,StopAreaNumber, LastModifiedUtcDateTime,ExistsFromDate)


# Splittar upp vår datamängd.
split_stopAreas <- stopAreas %>% 
  select(StopPointName,StopAreaNumber,ZoneShortName, StopAreaTypeCode) %>%
  distinct() #tar endast ut unika element, inga multiplar av samma värde.

# Går tillbaka till vår urspungliga datamängd
combinded_split <-split_stopPoints %>% 
  inner_join(split_stopAreas, c("StopAreaNumber"="StopAreaNumber"))


# Kollar storleken att spara våra dataset.
pryr::object_size(stopAreas)
```

    ## Registered S3 method overwritten by 'pryr':
    ##   method      from
    ##   print.bytes Rcpp

    ## 3.52 MB

``` r
pryr::object_size(split_stopAreas)
```

    ## 599 kB

``` r
pryr::object_size(split_stopPoints)
```

    ## 2.81 MB

``` r
pryr::object_size(combinded_split)
```

    ## 3.52 MB

``` r
#kollar om den nya listan är samma som "orginalet"
all_equal(combinded_split,stopAreas)
```

    ## [1] TRUE
