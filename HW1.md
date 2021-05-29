HW1
================
Filip Axelsson
2020-11-03

### Erfarnhet av Git och R

Jag har använt R, det vill säga RStudio och R Markdown i olika
laborationer,fallstudier och rapporter i mina kurser de senaste året,
jag har dock aldrig haft någon struktur i min kod, har alltid min
“stil”. Har använt biblioteket “dplyr” och ggplot2 enstaka gånger,
men oftast har vi inte haft tillåttelse att använda några andra
paket/bibliotek än det som är standard. Jag har aldrig använt Git eller
Github, visste inte ens vad det var förens första lektionen i denna
kurs.

### Plott av data

Det vi först måste göra är att är att “aktivera” paketet “ggplot2”, för
att ens kunna göra en “ggplot”. Vi tar även hem “dplyr” och “readr”. Det
första vi gör är att skapa ett namn till vår länk där vår data finns, vi
använder sedan funktionen “read\_csv” för att läsa in datan. Eftersom vi
har tagit hem en väldigt stor datamängd som består av flygbolags olyckor
i olika tidsperioder ville jag skala ner den till endast några få
flygbolag och det totala antalet dödliga olyckor från 2000-2014 använder
jag “select” och “filter” för att tilslut kunna plotta med “ggplot”

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(readr)
 

#skapar ett namn till datasetet, som finns på en hemsida
urlfile <- "https://raw.githubusercontent.com/fivethirtyeight/data/master/airline-safety/airline-safety.csv"
 


data <- read_csv(url(urlfile)) #läser in vår data 
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   airline = col_character(),
    ##   avail_seat_km_per_week = col_double(),
    ##   incidents_85_99 = col_double(),
    ##   fatal_accidents_85_99 = col_double(),
    ##   fatalities_85_99 = col_double(),
    ##   incidents_00_14 = col_double(),
    ##   fatal_accidents_00_14 = col_double(),
    ##   fatalities_00_14 = col_double()
    ## )

``` r
data %>% # redigerar vår data
  select(fatal_accidents_00_14, airline) %>% #plockar ut data
  filter(airline %in% c("SAS*","KLM*","Air France","Finnair","Qantas*","Lufthansa*")) %>% #filterar data
  ggplot(aes(x = airline, y =   fatal_accidents_00_14))+geom_point()  #plottar data
```

![](HW1_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
 sessionInfo()
```

    ## R version 4.0.3 (2020-10-10)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] sv_SE.UTF-8/sv_SE.UTF-8/sv_SE.UTF-8/C/sv_SE.UTF-8/sv_SE.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] readr_1.4.0   ggplot2_3.3.2 dplyr_1.0.2  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] rstudioapi_0.11  knitr_1.30       magrittr_1.5     hms_0.5.3       
    ##  [5] munsell_0.5.0    tidyselect_1.1.0 colorspace_1.4-1 R6_2.5.0        
    ##  [9] rlang_0.4.8      fansi_0.4.1      stringr_1.4.0    tools_4.0.3     
    ## [13] grid_4.0.3       gtable_0.3.0     xfun_0.19        cli_2.1.0       
    ## [17] withr_2.3.0      htmltools_0.5.0  ellipsis_0.3.1   assertthat_0.2.1
    ## [21] yaml_2.2.1       digest_0.6.27    tibble_3.0.4     lifecycle_0.2.0 
    ## [25] crayon_1.3.4     farver_2.0.3     purrr_0.3.4      vctrs_0.3.4     
    ## [29] glue_1.4.2       evaluate_0.14    rmarkdown_2.5    labeling_0.4.2  
    ## [33] stringi_1.5.3    compiler_4.0.3   pillar_1.4.6     generics_0.1.0  
    ## [37] scales_1.1.1     pkgconfig_2.0.3
