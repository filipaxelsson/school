HW4
================
Filip Axelsson
2020-11-05

``` r
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
library(RSQLite)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(magick)
```

    ## Linking to ImageMagick 6.9.11.32
    ## Enabled features: cairo, fontconfig, freetype, lcms, pango, rsvg, webp
    ## Disabled features: fftw, ghostscript, x11

### Uppgift 1

\(a\)

I denna uppgift ska vi beräkna medelvärdet av enhetspriset och sedan ta
ut all data från “tracks” med hjälp utav funktionen “dbReadTable”,
därefter ska vi plotta ett histogram, där vi har lagt till medelvärdet
som en vertikal linje. För att utföra detta använder vi “SELECT AVG()”
som kommer välja ut medelvärdet av den sökta variabeln. Att plotta ett
histogram är inga konstigetheter, vi gör som i de tidigare HW, det vi
gör annorlunda är att vi lägger till funktionen “geom\_vline” som
kommer rita ut den vertikala linjen. Resultatet visas nedanför i “Figur
1”

``` r
# a)

con <- dbConnect(RSQLite::SQLite(), "../HW_data/chinook.db")
dbListTables(con) # kollar vad alla datamängeder heter
```

    ##  [1] "albums"          "artists"         "customers"       "employees"      
    ##  [5] "genres"          "invoice_items"   "invoices"        "media_types"    
    ##  [9] "playlist_track"  "playlists"       "sqlite_sequence" "sqlite_stat1"   
    ## [13] "tracks"

``` r
mean_UnitPrice <- dbGetQuery(con, "
SELECT AVG(UnitPrice)
FROM tracks
       ")

dbReadTable(con, "tracks") %>%
  ggplot(aes(x = UnitPrice)) +
  geom_vline(xintercept = mean_UnitPrice[[1]], col = "blue") +
  geom_histogram() +
  annotate(
    geom = "curve", x = 1.2, y = 2500, xend = mean_UnitPrice[[1]] + 0.01, yend = 2000,
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 1.2, y = 2500, label = "Medelvärdet på enhetspriset: 
           1.050805", hjust = "left") +
  labs(y = "Antal", x = "Enhetspris") +
  ggtitle("Figur 1: Antalet spår som har respektive enhetspris")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](HW4_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

\(b\)

Vi ska nu bestämma vilken genre som har minst antal spår, för att kunna
avgöra detta väljer vi först ut de sökta variablerna med “SELECT”, det
vi även gör är att beräkna hur många gånger varje “GenreID” finns i
listan med hjälp utav “COUNT()”. För att kunna utföra allt detta måste
vi även kombinera datamängden “tracks” med “genres”, detta för att
namnet på “GenreID” finns endast i listan “genres”, detta görs med
“INNER JOIN”. Det som även måste göras är att gruppera “GenreID”, om
vi inte gör det så kommer “COUNT()” beräkna antalet för respektive ID.
Resultatet syns nedanför i “Tabell 1”, det vill säga “Opera” har minst
antal spår, med antalet 1.

``` r
# b)

genre_tracks <- dbGetQuery(con, "
SELECT tracks.genreid,genres.Name,
COUNT(genres.genreid) AS Amount
FROM tracks
INNER JOIN genres ON tracks.genreid = genres.genreid
GROUP BY genres.genreid
ORDER BY Amount
LIMIT 3
          ")

genre_tracks %>%
  kable(
    col.names = c("Genre ID", "Genre", "Antal"),
    caption = "Tabell 1: Genre som har minst antal spår"
  )
```

<table>

<caption>

Tabell 1: Genre som har minst antal spår

</caption>

<thead>

<tr>

<th style="text-align:right;">

Genre ID

</th>

<th style="text-align:left;">

Genre

</th>

<th style="text-align:right;">

Antal

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

25

</td>

<td style="text-align:left;">

Opera

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:left;">

Rock And Roll

</td>

<td style="text-align:right;">

12

</td>

</tr>

<tr>

<td style="text-align:right;">

18

</td>

<td style="text-align:left;">

Science Fiction

</td>

<td style="text-align:right;">

13

</td>

</tr>

</tbody>

</table>

\(c\)

Vi ska nu bestämma vilken genre som har minsta antalet spår i en
spellista, vi gör beräkningar i princip på samma sätt som i b) förutom
att vi beräknar och grupperar på andra variabler. Det vi även måste göra
är att gå in i totalt 4 olika listor för att kunna kombinera varje
spellista med genres namn, vi gör detta med “INNER JOIN”. Resultatet
visas i “Tabell 2”, vi ser där att genren “Rock” har flest spår, 1297 st
i två olika spellistor, dock är det samma namn på spellistan men de har
olika ID, därav är det inte samma spellista.

``` r
# c)

tracks_in_playlist <- dbGetQuery(con, "
SELECT genres.genreid, genres.name,playlists.name,playlists.PlaylistId,
COUNT(playlists.PlaylistId) as amount
FROM tracks
INNER JOIN genres ON tracks.genreid = genres.genreid
INNER JOIN playlist_track ON playlist_track.TrackId = tracks.TrackId
INNER JOIN playlists ON playlist_track.PlaylistId = playlists.PlaylistId
GROUP BY playlists.PlaylistId, genres.genreid
ORDER BY amount DESC
LIMIT 3
        ")


tracks_in_playlist %>%
  kable(
    col.names = c("Genre ID", "Genre", "Namn på spellista", "Spellista ID", "Antal spår"),
    caption = "Tabell 2: Genre som har flest spår i en spellista"
  )
```

<table>

<caption>

Tabell 2: Genre som har flest spår i en spellista

</caption>

<thead>

<tr>

<th style="text-align:right;">

Genre ID

</th>

<th style="text-align:left;">

Genre

</th>

<th style="text-align:left;">

Namn på spellista

</th>

<th style="text-align:right;">

Spellista ID

</th>

<th style="text-align:right;">

Antal spår

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Rock

</td>

<td style="text-align:left;">

Music

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1297

</td>

</tr>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Rock

</td>

<td style="text-align:left;">

Music

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

1297

</td>

</tr>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

Rock

</td>

<td style="text-align:left;">

90’s Music

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

621

</td>

</tr>

</tbody>

</table>

\(d\) Vi ska nu beräkna vilken kompositör som har flest antal spår i en
spellista, detta är även en liknade process i uträkningen som i b) och
c), dock behöver vi endast gå in 3 olika listor nu för att kunna beräkna
svaret. Vi använder även “WHERE Composer IS NOT NULL” för att endast få
kompositörer som har ett namn (det vill säga inte en “tom sträng”). Vi
resultatet i “Tabell 3” att Steve Harris har flest antal spår, 80 st i
två olika spellistor. Även här så är namnet på spellistorna samma men
skillnaden är ID.

``` r
# d)


composer_in_playlist <- dbGetQuery(con, "
SELECT Composer, playlists.Name, playlists.PlaylistId,
COUNT(playlists.PlaylistId) AS amount
FROM tracks
INNER JOIN playlist_track ON playlist_track.TrackId = tracks.TrackId
INNER JOIN playlists ON playlist_track.PlaylistId = playlists.PlaylistId
WHERE Composer IS NOT NULL
GROUP BY Composer, playlists.PlaylistId
ORDER BY Amount DESC
LIMIT 3
        ")

composer_in_playlist %>%
  kable(
    col.names = c("Kompositör", "Namn på spellista", "Spellista ID", "Antal spår"),
    caption = "Tabell 3: Komposiör med flest antal spår i en spellista"
  )
```

<table>

<caption>

Tabell 3: Komposiör med flest antal spår i en spellista

</caption>

<thead>

<tr>

<th style="text-align:left;">

Kompositör

</th>

<th style="text-align:left;">

Namn på spellista

</th>

<th style="text-align:right;">

Spellista ID

</th>

<th style="text-align:right;">

Antal spår

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Steve Harris

</td>

<td style="text-align:left;">

Music

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

80

</td>

</tr>

<tr>

<td style="text-align:left;">

Steve Harris

</td>

<td style="text-align:left;">

Music

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

80

</td>

</tr>

<tr>

<td style="text-align:left;">

U2

</td>

<td style="text-align:left;">

Music

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

44

</td>

</tr>

</tbody>

</table>

### Uppgift 2

1)  Vi ska läsa in en data till R och begränsa oss till resultatet av
    “Samtliga” skolor för varje kommun. Vi kommer analysera
    medelvärdet för betygens poäng i 23 ämnen som finns i den insamlade
    datamängden. Vid inläsning av datamängden så stöter vi på problem,
    det finns en massa olika tecken som “ställer till det” , “.”, “..”,
    “-” och “~100”.

“-” - Betyg har ej kunnats sättas på grund av frånvaro.

“..” - Resultatuppgift baserat på färre än 10 elever och på grund av
sekretesskäl visas detta istället för utfall.

“.” - Om data saknas visas detta istället för utfall, notera även inga
elever har uppnåt A-E visas detta.

“~100” - Om antalet som ej uppnått A-E är 1-4 elever, så visas andelen
som uppnåt A-E som “~100”. Om en andelsuppgift för antigen pojkar eller
flickor redovisas med “~100” så kommer andelsuppgiften för det andra
könet att sekretessprickas “..”.

Jag har valt sätta alla värden som representeras av “.”, “-” till NA,
detta på grund av det finns ingen data kring dessa. Jag har även valt
att sätta “..” till NA detta på grund av det egentligen inte finns något
bra sätt att byta ut dessa värden, då det kan bli helt missvisande.
Detta kan göra att vi får en osäkerhet hos de kommuner som har väldigt
få invånare vilket resulterar till få elever.

“~100” har jag valt att sätta till “100.0”, detta på grund av det är
approximativt samma värde.

Detta är inte det enda problemet vid inläsningen, som vi ser nedanför
får vi en rad felmeddelande, detta är på grund av en textbeskriving
finns med längst ner, detta kontrollerar vi med att kolla vad som är fel
med raden. Vi kommer ta bort detta med hjälp utav “n\_max”. Det finns
även en textbeskriving högst upp (till och med rad 6), så med hjälp av
“skip” så kan vi hoppa över denna rad. Som jag beskrev ovanför så
sätter vi “.”,“..” och “-” till NA med hjälp utav “na” (alla dessa
operationer gör i read\_csv2). Det vi nu har kvar att göra är att
filtera ut “Samtliga” skolor, göra om “~100” till “100.0”, men det fanns
även en blanding av “.” och “,” i talen så jag gjorde om alla till “.”,
allt detta gjordes med funktionen “mutate\_all”. Funktionen som användes
är “list(~str\_replace())” vilket kommer gå igenom all rader/kolonner
och byta ut “strängarna”. Jag tog även bort blanksteget som fanns i
siffor över 1000 vilket gjordes med i princip samma process som ovan
förutom att jag använde “mutate\_at”. Det sista steget var att göra om
kolonner till numeric och detta gjordes med “mutate\_at”.

``` r
# a)
# Visar felmeddelandet efter att ha tagit bort de första 6 raderna
df_kontroll <- df_school <- read_csv2("../HW_data/exp_betyg_ak6_kommun_2018_19.csv", skip = 6)
```

    ## ℹ Using ',' as decimal and '.' as grouping mark. Use `read_delim()` for more control.

    ## Warning: Missing column names filled in: 'X16' [16]

    ## Warning: Duplicated column names deduplicated: 'Totalt' => 'Totalt_1' [10],
    ## 'Flickor' => 'Flickor_1' [11], 'Pojkar' => 'Pojkar_1' [12], 'Totalt' =>
    ## 'Totalt_2' [13], 'Flickor' => 'Flickor_2' [14], 'Pojkar' => 'Pojkar_2' [15]

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   Kommun = col_character(),
    ##   `Kommun-kod` = col_character(),
    ##   Län = col_character(),
    ##   `Läns-kod` = col_character(),
    ##   `Typ av huvudman` = col_character(),
    ##   Ämne = col_character(),
    ##   Totalt = col_character(),
    ##   Flickor = col_character(),
    ##   Pojkar = col_character(),
    ##   Totalt_1 = col_character(),
    ##   Flickor_1 = col_character(),
    ##   Pojkar_1 = col_character(),
    ##   Totalt_2 = col_character(),
    ##   Flickor_2 = col_character(),
    ##   Pojkar_2 = col_character(),
    ##   X16 = col_logical()
    ## )

    ## Warning: 15 parsing failures.
    ##   row col   expected    actual                                          file
    ## 17412  -- 16 columns 1 columns '../HW_data/exp_betyg_ak6_kommun_2018_19.csv'
    ## 17413  -- 16 columns 1 columns '../HW_data/exp_betyg_ak6_kommun_2018_19.csv'
    ## 17414  -- 16 columns 1 columns '../HW_data/exp_betyg_ak6_kommun_2018_19.csv'
    ## 17415  -- 16 columns 1 columns '../HW_data/exp_betyg_ak6_kommun_2018_19.csv'
    ## 17416  -- 16 columns 1 columns '../HW_data/exp_betyg_ak6_kommun_2018_19.csv'
    ## ..... ... .......... ......... .............................................
    ## See problems(...) for more details.

``` r
df_kontroll
```

    ## # A tibble: 17,426 x 16
    ##    Kommun `Kommun-kod` Län   `Läns-kod` `Typ av huvudma… Ämne  Totalt Flickor
    ##    <chr>  <chr>        <chr> <chr>      <chr>            <chr> <chr>  <chr>  
    ##  1 Ale    1440         Väst… 14         Samtliga         Bild  415    219    
    ##  2 Ale    1440         Väst… 14         Kommunal         Bild  393    207    
    ##  3 Ale    1440         Väst… 14         Enskild          Bild  22     12     
    ##  4 Aling… 1489         Väst… 14         Samtliga         Bild  530    262    
    ##  5 Aling… 1489         Väst… 14         Kommunal         Bild  433    207    
    ##  6 Aling… 1489         Väst… 14         Enskild          Bild  97     55     
    ##  7 Alves… 0764         Kron… 07         Samtliga         Bild  243    114    
    ##  8 Alves… 0764         Kron… 07         Kommunal         Bild  234    109    
    ##  9 Alves… 0764         Kron… 07         Enskild          Bild  ..     ..     
    ## 10 Aneby  0604         Jönk… 06         Samtliga         Bild  77     40     
    ## # … with 17,416 more rows, and 8 more variables: Pojkar <chr>, Totalt_1 <chr>,
    ## #   Flickor_1 <chr>, Pojkar_1 <chr>, Totalt_2 <chr>, Flickor_2 <chr>,
    ## #   Pojkar_2 <chr>, X16 <lgl>

``` r
# Läser in datamängden där vi sätter värden till NA, hoppar över rader och läser t.o.m rad X
df_school <- read_csv2("../HW_data/exp_betyg_ak6_kommun_2018_19.csv", skip = 6, na = c(".", "-", ".."), n_max = 17411)
```

    ## ℹ Using ',' as decimal and '.' as grouping mark. Use `read_delim()` for more control.

    ## Warning: Missing column names filled in: 'X16' [16]
    
    ## Warning: Duplicated column names deduplicated: 'Totalt' => 'Totalt_1' [10],
    ## 'Flickor' => 'Flickor_1' [11], 'Pojkar' => 'Pojkar_1' [12], 'Totalt' =>
    ## 'Totalt_2' [13], 'Flickor' => 'Flickor_2' [14], 'Pojkar' => 'Pojkar_2' [15]

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   Kommun = col_character(),
    ##   `Kommun-kod` = col_character(),
    ##   Län = col_character(),
    ##   `Läns-kod` = col_character(),
    ##   `Typ av huvudman` = col_character(),
    ##   Ämne = col_character(),
    ##   Totalt = col_character(),
    ##   Flickor = col_character(),
    ##   Pojkar = col_character(),
    ##   Totalt_1 = col_character(),
    ##   Flickor_1 = col_character(),
    ##   Pojkar_1 = col_character(),
    ##   Totalt_2 = col_double(),
    ##   Flickor_2 = col_double(),
    ##   Pojkar_2 = col_double(),
    ##   X16 = col_logical()
    ## )

``` r
df_school <- df_school %>%
  select(-X16) %>% # Tar bort en onödig kolonn som har skapats.
  filter(`Typ av huvudman` == "Samtliga") %>% # Filterar ut den sökta datamängden.
  mutate_all(list(~ str_replace(., "~100", "100.0"))) %>% # gör om alla "~100" till "100.0"
  mutate_all(list(~ str_replace(., ",", "."))) %>% # gör om alla "," till "."
  mutate_at(c(7:15), list(~ str_replace(., " ", ""))) %>% # tar bort alla blanksteg i t.ex 1 000
  mutate_at(c(7:15), as.numeric) # Gör om till numeric

df_school
```

    ## # A tibble: 6,647 x 15
    ##    Kommun `Kommun-kod` Län   `Läns-kod` `Typ av huvudma… Ämne  Totalt Flickor
    ##    <chr>  <chr>        <chr> <chr>      <chr>            <chr>  <dbl>   <dbl>
    ##  1 Ale    1440         Väst… 14         Samtliga         Bild     415     219
    ##  2 Aling… 1489         Väst… 14         Samtliga         Bild     530     262
    ##  3 Alves… 0764         Kron… 07         Samtliga         Bild     243     114
    ##  4 Aneby  0604         Jönk… 06         Samtliga         Bild      77      40
    ##  5 Arboga 1984         Väst… 19         Samtliga         Bild     145      63
    ##  6 Arjep… 2506         Norr… 25         Samtliga         Bild      25      14
    ##  7 Arvid… 2505         Norr… 25         Samtliga         Bild      55      18
    ##  8 Arvika 1784         Värm… 17         Samtliga         Bild     291     134
    ##  9 Asker… 1882         Öreb… 18         Samtliga         Bild     111      52
    ## 10 Avesta 2084         Dala… 20         Samtliga         Bild     255     143
    ## # … with 6,637 more rows, and 7 more variables: Pojkar <dbl>, Totalt_1 <dbl>,
    ## #   Flickor_1 <dbl>, Pojkar_1 <dbl>, Totalt_2 <dbl>, Flickor_2 <dbl>,
    ## #   Pojkar_2 <dbl>

\(b\)

Vi ska nu göra en plott som visar skillnaden mellan medelvärdet på betyg
mellan pojkar och flickor i varje län. Vi ska även ta hänsyn till att
det är olika många pojkar och flickor i respektive kommun och ämne, och
göra en vikting när vi beräknar medelvärdet. Jag har valt att göra
stapeldiagram med hjälp utav “geom\_col” detta för att det går enkelt
att avgöra om jämföra pojkar och flickor i respektive län men det går
även (kanske lite sämre) jämföra pojkar och flickor mellan län.
Resultatet visas nedanför “Figur 2”.

``` r
# b)
df_school %>%
  group_by(Län) %>%
  mutate(
    betyg_flickor = Flickor * Flickor_2, # Berkänar total poäng för flickor i varje kommun
    betyg_pojkar = Pojkar * Pojkar_2, # Berkänar total poäng för flickor i varje kommun
    medel_pojkar = sum(betyg_pojkar, na.rm = TRUE) / sum(Pojkar, na.rm = TRUE), # medelvärde för respektive län
    medel_flickor = sum(betyg_flickor, na.rm = TRUE) / sum(Flickor, na.rm = TRUE) # medelvärde för respektive län
  ) %>%
  select(Län, medel_pojkar, medel_flickor) %>%
  pivot_longer(!Län, names_to = "F_P", values_to = "medelvärde") %>%
  ggplot(aes(x = Län, y = medelvärde, fill = F_P)) +
  geom_col(position = "dodge") +
  labs(x = "Län", y = "Medelvärde på betyg") +
  scale_fill_discrete("Flickor eller Pojkar", labels = c("Flickor", "Pojkar")) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Figur 2: Medelvärdet på betyg för pojkar och flickor i respektive län")
```

![](HW4_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

\(c\)

Vi ska i denna uppgift göra en karta över Sverige där kommunerna är
färglagda i olika färger beroende på ifall medelvärdet på betyget för
“Engelska” är högre än “Idrott och hälsa”. Vi gör detta med att
använda “pivot\_wider” för att få varje ämne som en kolonn, vi använder
sedan “mutate” för att göra en kontroll variabel som avgör ifall
“Engelska\>Idrott och hälsa” som kommer generea “TRUE” eller “FALSE”.
Vi använder sedan koden som gavs i uppfiten med några modifieringar.
Reslutatet visas i “Figur 3”. Det är lite svår att avgöra men jag drar
slutasten att majoriten av kommunerna inte har ett medelvärde på betyget
för “Engelska” som är större än "Idrott och hälsa

``` r
# c)
df_eng_idrott <- df_school %>%
  select(Kommun, Ämne, Totalt_2, `Kommun-kod`) %>%
  filter(Ämne %in% c("Engelska", "Idrott och hälsa")) %>%
  pivot_wider(names_from = Ämne, values_from = Totalt_2) %>%
  mutate(kontroll = ifelse(Engelska > `Idrott och hälsa`, TRUE, FALSE))



kommun_karta <- read_csv("../HW_data/kommun_karta.csv")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   long = col_double(),
    ##   lat = col_double(),
    ##   order = col_double(),
    ##   hole = col_logical(),
    ##   piece = col_double(),
    ##   id = col_character(),
    ##   group = col_character()
    ## )

``` r
kommun_karta %>%
  inner_join(df_eng_idrott, c("id" = "Kommun-kod")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = kontroll)) +
  geom_polygon() +
  coord_fixed() +
  # theme_void()
  theme_minimal() +
  scale_fill_manual("Engelska > Idrott",
    values = c("FALSE" = "black", "TRUE" = "deepskyblue1"),
    labels = c("FALSKT", "SANT")
  ) +
  ggtitle("Figur 3: Jämförelse i medelvärdet för betyg mellan Idrott och Engelska") +
  theme(plot.title = element_text(size = 11))
```

![](HW4_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

\(d\)

Vi ska nu beräkna för varje ämne medelvärdet på betyget i hela Sverige,
resultatet visas “Tabell 4” där vi även ser att medelvärdet på betyget
för “Engelska” inte är större än “Idrott och hälsa” men det är väldigt
nära varandra.

``` r
# d)

df_school %>%
  select(Ämne, Totalt_2) %>% # Tar ut de sökta variablerna
  group_by(Ämne) %>%
  mutate(medelvärde = mean(Totalt_2, na.rm = TRUE)) %>% # beräknar medelvärdet.
  distinct(Ämne, medelvärde) %>% # Tar endast ut en kommun respektive medelvärde.
  kable(
    col.names = c("Ämne", "Medelvärde på betyg"),
    caption = "Tabell 4: Medelvärde på betyg för respektive ämne över hela Sverige",
  ) %>%
  kable_styling() %>%
  row_spec(c(3, 8), color = "white", background = "blue", bold = T) %>% # färgar rader
  save_kable("tabell.png") # sparar ner tabellen som en png

# eftersom kable_special endast fungar i HTML så sparar jag ner tabellen som en "screenshot"

knitr::include_graphics("tabell.png")
```

<img src="tabell.png" width="992" />
