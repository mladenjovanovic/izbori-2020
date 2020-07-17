Analiza izbora 2020
================
Mladen Jovanovic (@physical\_prep)
7/16/2020

# Podaci

Podaci su preuzeti sa sajta RIK-a “Rezultati po birackim mestima” (16-07-2020): https://www.rik.parlament.gov.rs/tekst/9386/ukupni-rezultati-izbora-za-narodne-poslanike-narodne-skupstine-2020-godine.php

# Analiza rezultata

Analiza rezultata je uradjena u R jeziku, koristeci R Markdown koji
omogucuje reprodukciju analize. Kod koji je koriscen ce biti vidljiv u
ovom dokumentu radi transparentnosti, a samim time i lakseg uocavanja
gresaka.

``` r
require(tidyverse)
require(cowplot)

elections <- read_csv("izbori.csv")

head(elections)
#> # A tibble: 6 x 33
#>   Okrug `Naziv okruga` Opstina `Naziv opstine` `Biracko mesto` `Upisani biraci`
#>   <dbl> <chr>            <dbl> <chr>                     <dbl>            <dbl>
#> 1     0 Град Београд     70092 Барајево                      1              655
#> 2     0 Град Београд     70092 Барајево                      2             1329
#> 3     0 Град Београд     70092 Барајево                      3              578
#> 4     0 Град Београд     70092 Барајево                      4              706
#> 5     0 Град Београд     70092 Барајево                      5              734
#> 6     0 Град Београд     70092 Барајево                      6              830
#> # … with 27 more variables: Primljeni <dbl>, Neupotrebljeni <dbl>,
#> #   Glasali <dbl>, `U glasackoj kutiji` <dbl>, Nevazeci <dbl>, Vazeci <dbl>,
#> #   `Aleksandar Vucic` <dbl>, `Ivica Dacic` <dbl>, `Vojislav Seselj` <dbl>,
#> #   `Istvan Pastor` <dbl>, `Aleksandar Sapic` <dbl>, `Gojko Zivkovic` <dbl>,
#> #   UDS <dbl>, `Muamer Zukorlic` <dbl>, Metla <dbl>, `Milan Stamatovic` <dbl>,
#> #   SDA <dbl>, Zavetnici <dbl>, `Narodni Blok` <dbl>, PSG <dbl>,
#> #   Suverenisti <dbl>, `Ujedinjena Dolina` <dbl>, `Grupa Gradjana 1 od
#> #   5` <dbl>, `Nek Maske Spadnu` <dbl>, `Ruska Stranka\n` <dbl>, `Cedomir
#> #   Jovanovic` <dbl>, Levijatan <dbl>
```

# Izlaznost po okruzima

``` r
izlaznost <- elections %>%
  group_by(`Naziv okruga`) %>%
  summarize(
    `Upisani biraci` = sum(`Upisani biraci`),
    `Primljeni` = sum(`Primljeni`),
    `Neupotrebljeni` = sum(`Neupotrebljeni`),
    `Glasali` = sum(`Glasali`),
    `U glasackoj kutiji` = sum(`U glasackoj kutiji`),
    `Nevazeci` = sum(`Nevazeci`),
    `Vazeci` = sum(`Vazeci`)
  ) %>%
  ungroup() %>%
  mutate(
    `Izlaznost` = `Glasali` / `Upisani biraci`,
    `Naziv okruga` = fct_reorder(`Naziv okruga`, `Izlaznost`),
    preko_50 = ifelse(`Izlaznost` >= 0.5, TRUE, FALSE)
  )

head(izlaznost)
#> # A tibble: 6 x 10
#>   `Naziv okruga` `Upisani biraci` Primljeni Neupotrebljeni Glasali
#>   <fct>                     <dbl>     <dbl>          <dbl>   <dbl>
#> 1 Борски округ             119034    118989          61239   57750
#> 2 Браничевски о…           179528    179479          86591   92888
#> 3 Град Београд            1604376   1607606         993632  613974
#> 4 Заводи за изв…             8646      8646           1473    7173
#> 5 Зајечарски ок…            99975     99933          51589   48344
#> 6 Западнобачки …           158970    158713          75112   83601
#> # … with 5 more variables: `U glasackoj kutiji` <dbl>, Nevazeci <dbl>,
#> #   Vazeci <dbl>, Izlaznost <dbl>, preko_50 <lgl>
```

``` r
ggplot(
  izlaznost,
  aes(
    x = `Naziv okruga`,
    y = `Izlaznost`,
    fill = `preko_50`
  )
) +
  theme_cowplot(8) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = 0.5, alpha = 0.5, linetype = "dashed") +
  coord_flip() +
  xlab(NULL) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("dark red", "dark grey"))
```

<img src="izbori_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" style="display: block; margin: auto;" />

# Broj glasova po okruzima

``` r
glasovi <- elections %>%
  gather(key = "Kandidat", value = "Broj glasova", -(1:12)) %>%
  group_by(`Naziv okruga`, `Kandidat`) %>%
  summarize(
    `Upisani biraci` = sum(`Upisani biraci`),
    `Primljeni` = sum(`Primljeni`),
    `Neupotrebljeni` = sum(`Neupotrebljeni`),
    `Glasali` = sum(`Glasali`),
    `U glasackoj kutiji` = sum(`U glasackoj kutiji`),
    `Nevazeci` = sum(`Nevazeci`),
    `Vazeci` = sum(`Vazeci`),
    `Broj glasova` = sum(`Broj glasova`)
  ) %>%
  ungroup() %>%
  mutate(
    `Procenat` = `Broj glasova` / `Vazeci`,
    preko_3_posto = ifelse(`Procenat` >= 0.03, TRUE, FALSE),
    threshold = `Vazeci` * 0.03)

head(glasovi)
#> # A tibble: 6 x 13
#>   `Naziv okruga` Kandidat `Upisani biraci` Primljeni Neupotrebljeni Glasali
#>   <chr>          <chr>               <dbl>     <dbl>          <dbl>   <dbl>
#> 1 Борски округ   Aleksan…           119034    118989          61239   57750
#> 2 Борски округ   Aleksan…           119034    118989          61239   57750
#> 3 Борски округ   Cedomir…           119034    118989          61239   57750
#> 4 Борски округ   Gojko Z…           119034    118989          61239   57750
#> 5 Борски округ   Grupa G…           119034    118989          61239   57750
#> 6 Борски округ   Istvan …           119034    118989          61239   57750
#> # … with 7 more variables: `U glasackoj kutiji` <dbl>, Nevazeci <dbl>,
#> #   Vazeci <dbl>, `Broj glasova` <dbl>, Procenat <dbl>, preko_3_posto <lgl>,
#> #   threshold <dbl>
```

``` r
ggplot(
  glasovi,
  aes(
    x = `Kandidat`,
    y = `Broj glasova`,
    fill = `preko_3_posto`
  )
) +
  theme_cowplot(6) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_hline(aes(yintercept = threshold), alpha = 0.5, linetype = "dashed") +
  coord_flip() +
  xlab(NULL) +
  ylab("Broj glasova (log10)") +
  facet_wrap(~`Naziv okruga`, scales = "free_x") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("dark red", "dark grey")) +
  scale_y_log10()
```

<img src="izbori_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(
  glasovi,
  aes(
    x = `Kandidat`,
    y = `Procenat`,
    fill = `preko_3_posto`
  )
) +
  theme_cowplot(5) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = 0.03, alpha = 0.5, linetype = "dashed") +
  coord_flip() +
  xlab(NULL) +
  ylab("Procenat broja glasova") +
  facet_wrap(~`Naziv okruga`, scales = "free_x") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("dark red", "dark grey")) 
```

<img src="izbori_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" style="display: block; margin: auto;" />

# Analiza poslednje decimale broja glasova

Ova analiza je bazirana na sledecem radu:

Beber, B., & Scacco, A. (2012). What the Numbers Say: A Digit-Based Test
for Election Fraud. Political Analysis, 20(2), 211-234.
<doi:10.1093/pan/mps003>

**Abstract** \>Is it possible to detect manipulation by looking only at
electoral returns? Drawing on work in psychology, we exploit
individuals’ biases in generating numbers to highlight suspicious
digit patterns in reported vote counts. First, we show that fair
election procedures produce returns where last digits occur with equal
frequency, but laboratory experiments indicate that individuals tend to
favor some numerals over others, even when subjects have incentives to
properly randomize. Second, individuals underestimate the likelihood of
digit repetition in sequences of random integers, so we should observe
relatively few instances of repeated numbers in manipulated vote
tallies. Third, laboratory experiments demonstrate a preference for
pairs of adjacent digits, which suggests that such pairs should be
abundant on fraudulent return sheets. Fourth, subjects avoid pairs of
distant numerals, so those should appear with lower frequency on tainted
returns. We test for deviations in digit patterns using data from
Sweden’s 2002 parliamentary elections, Senegal’s 2000 and 2007
presidential elections, and previously unavailable results from
Nigeria’s 2003 presidential election. In line with observers’
expectations, we find substantial evidence that manipulation occurred in
Nigeria as well as in Senegal in 2007.

``` r
require(stringr)

decimale <- elections %>%
  gather(key = "Kandidat", value = "Broj glasova", -(1:12)) %>%
  mutate(
    last_digit = factor(str_sub(`Broj glasova`, -1, -1))
  )

head(decimale)
#> # A tibble: 6 x 15
#>   Okrug `Naziv okruga` Opstina `Naziv opstine` `Biracko mesto` `Upisani biraci`
#>   <dbl> <chr>            <dbl> <chr>                     <dbl>            <dbl>
#> 1     0 Град Београд     70092 Барајево                      1              655
#> 2     0 Град Београд     70092 Барајево                      2             1329
#> 3     0 Град Београд     70092 Барајево                      3              578
#> 4     0 Град Београд     70092 Барајево                      4              706
#> 5     0 Град Београд     70092 Барајево                      5              734
#> 6     0 Град Београд     70092 Барајево                      6              830
#> # … with 9 more variables: Primljeni <dbl>, Neupotrebljeni <dbl>,
#> #   Glasali <dbl>, `U glasackoj kutiji` <dbl>, Nevazeci <dbl>, Vazeci <dbl>,
#> #   Kandidat <chr>, `Broj glasova` <dbl>, last_digit <fct>
```

### Analiza poslednje decimale ukupno

``` r
table(decimale$last_digit)
#> 
#>     0     1     2     3     4     5     6     7     8     9 
#> 73494 27723 17305 12881 10551  8892  7674  6872  6149  5552
```

``` r
ggplot(
  decimale,
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_hline(yintercept = 0.1, alpha = 0.5, linetype = "dashed") +
  xlab("Poslednja decimala") +
  ylab("Proporcija")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" style="display: block; margin: auto;" />

Kada se izbace rezultati gde je broj glasova 0:

``` r
ggplot(
  filter(decimale, `Broj glasova` > 0),
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_hline(yintercept = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0.11, alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.09, alpha = 0.5, linetype = "dashed") +
  xlab("Poslednja decimala") +
  ylab("Proporcija")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" style="display: block; margin: auto;" />

Kada se izbace rezultati gde je broj glasova manji ili jednak 10:

``` r
ggplot(
  filter(decimale, `Broj glasova` >= 10),
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_hline(yintercept = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0.11, alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.09, alpha = 0.5, linetype = "dashed") +
  xlab("Poslednja decimala") +
  ylab("Proporcija")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" style="display: block; margin: auto;" />

Kada se izbace rezultati gde je broj glasova manji ili jednak 100:

``` r
ggplot(
  filter(decimale, `Broj glasova` >= 100),
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_hline(yintercept = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0.11, alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.09, alpha = 0.5, linetype = "dashed") +
  xlab("Poslednja decimala") +
  ylab("Proporcija")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" style="display: block; margin: auto;" />

### Analiza poslednje decimale po kandidatu

``` r
table(decimale$Kandidat, decimale$last_digit)
#>                        
#>                            0    1    2    3    4    5    6    7    8    9
#>   Aleksandar Sapic      1544 1176 1016  879  783  731  651  609  544  500
#>   Aleksandar Vucic       841  881  852  858  840  806  856  856  836  807
#>   Cedomir Jovanovic     4608 1711  914  519  320  166   85   67   24   19
#>   Gojko Zivkovic        1423 1050  944  845  800  770  737  689  615  560
#>   Grupa Gradjana 1 od 5 3348 1398  976  706  592  422  374  259  200  158
#>   Istvan Pastor         4878 1566  659  362  263  188  158  131  106  122
#>   Ivica Dacic           1009  881  895  898  830  827  779  808  761  745
#>   Levijatan             3235 1382  949  739  582  471  358  304  232  181
#>   Metla                 1960 1222 1002  836  747  659  604  498  477  428
#>   Milan Stamatovic      3479 1592  998  738  512  357  241  207  186  123
#>   Muamer Zukorlic       4936 1921  753  319  168  106   81   57   49   43
#>   Narodni Blok          5563 1463  643  283  191  103   75   35   42   35
#>   Nek Maske Spadnu      5127 1500  804  430  229  142   82   60   42   17
#>   PSG                   2602 1262  966  745  678  565  474  429  376  336
#>   Ruska Stranka\n       5358 1758  666  308  152   83   46   28   20   14
#>   SDA                   7081  902  193   61   28   37   35   37   32   27
#>   Suverenisti           2236 1201  941  780  685  611  557  542  451  429
#>   UDS                   3522 1523  911  734  516  371  292  213  197  154
#>   Ujedinjena Dolina     7263  943  132   31   13    8   15   10    7   11
#>   Vojislav Seselj       1364 1139 1072  917  909  829  616  575  520  492
#>   Zavetnici             2117 1252 1019  893  713  640  558  458  432  351
```

``` r
ggplot(
  decimale,
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..))) +
  facet_wrap(~`Kandidat`, scales = "free_y") +
  xlab("Poslednja decimala") +
  ylab("Broj pojavljivanja")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-15-1.png" width="90%" style="display: block; margin: auto;" />

Kada se izbace rezultati gde je broj glasova 0:

``` r
ggplot(
  filter(decimale, `Broj glasova` > 0),
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..))) +
  facet_wrap(~`Kandidat`, scales = "free_y") +
  xlab("Poslednja decimala") +
  ylab("Broj pojavljivanja")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" style="display: block; margin: auto;" />

Kada se izbace rezultati gde je broj glasova manji ili jednak 10:

``` r
ggplot(
  filter(decimale, `Broj glasova` >= 10),
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..))) +
  facet_wrap(~`Kandidat`, scales = "free_y") +
  xlab("Poslednja decimala") +
  ylab("Broj pojavljivanja")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" style="display: block; margin: auto;" />

Kada se izbace rezultati gde je broj glasova manji ili jednak 100:

``` r
ggplot(
  filter(decimale, `Broj glasova` >= 100),
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..))) +
  facet_wrap(~`Kandidat`, scales = "free_y") +
  xlab("Poslednja decimala") +
  ylab("Broj pojavljivanja")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-18-1.png" width="90%" style="display: block; margin: auto;" />

### Analiza poslednje decimale po okrugu

``` r
ggplot(
  decimale,
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..))) +
  facet_wrap(~`Naziv okruga`, scales = "free_y") +
  xlab("Poslednja decimala") +
  ylab("Broj pojavljivanja")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-19-1.png" width="90%" style="display: block; margin: auto;" />

Kada se izbace rezultati gde je broj glasova 0:

``` r
ggplot(
  filter(decimale, `Broj glasova` > 0),
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..))) +
  facet_wrap(~`Naziv okruga`, scales = "free_y") +
  xlab("Poslednja decimala") +
  ylab("Broj pojavljivanja")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-20-1.png" width="90%" style="display: block; margin: auto;" />

Kada se izbace rezultati gde je broj glasova manji ili jednak 10:

``` r
ggplot(
  filter(decimale, `Broj glasova` >= 10),
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..))) +
  facet_wrap(~`Naziv okruga`, scales = "free_y") +
  xlab("Poslednja decimala") +
  ylab("Broj pojavljivanja")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-21-1.png" width="90%" style="display: block; margin: auto;" />

Kada se izbace rezultati gde je broj glasova manji ili jednak 100:

``` r
ggplot(
  filter(decimale, `Broj glasova` >= 100),
  aes(x = last_digit)
) +
  theme_cowplot(8) +
  geom_bar(aes(y = (..count..))) +
  facet_wrap(~`Naziv okruga`, scales = "free_y") +
  xlab("Poslednja decimala") +
  ylab("Broj pojavljivanja")
```

<img src="izbori_files/figure-gfm/unnamed-chunk-22-1.png" width="90%" style="display: block; margin: auto;" />

