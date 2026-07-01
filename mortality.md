# Mortality Analysis


``` r
source("scripts/mortality_analysis.R")
```


    Vedhæfter pakke: 'lubridate'

    De følgende objekter er maskerede fra 'package:data.table':

        hour, isoweek, mday, minute, month, quarter, second, wday, week,
        yday, year

    De følgende objekter er maskerede fra 'package:base':

        date, intersect, setdiff, union

    Warning: 12 failed to parse.

    Warning: 3 failed to parse.

    === DEATHS IN OFFICE ===
                      name     country portfolio   start_dt     end_dt     dod_dt
                    <char>      <char>    <char>     <POSc>     <POSc>     <POSc>
    1:      Joël Le Theule      France   Defense 1980-10-02 1980-12-22 1980-12-14
    2:    Pierre Bérégovoy      France   Defense 1993-03-09 1993-03-30 1993-05-01
    3:      Joël Le Theule      France Transport 1978-04-05 1980-10-02 1980-12-14
    4: Georgios Gennimatas      Greece   Finance 1993-10-13 1994-02-25 1994-04-25
    5:  Antonio Brancaccio       Italy  Interior 1995-01-17 1995-06-08 1995-08-26
    6:       Koos Rietkerk Netherlands  Interior 1982-11-04 1986-02-20 1986-02-20
    7:           Ien Dales Netherlands  Interior 1989-11-07 1994-01-10 1994-01-10
    8:     Sonja Ludvigsen      Norway    Health 1973-10-16 1974-07-12 1974-07-12

    Total Ministers who died in office:  8 

    === AVERAGE LIFESPAN BY COUNTRY (Kaplan-Meier Median) ===
            Country MinistersAnalyzed DeceasedSoFar MedianLifespanYrs
             <char>             <num>         <num>             <num>
     1:     Germany                28            22              87.3
     2:     Denmark                22            22              86.8
     3:      Sweden                33            27              84.8
     4:          UK                27            23              84.4
     5:       Italy                72            56              84.1
     6: Netherlands                35            33              82.5
     7:      Norway                35            30              81.4
     8:       Spain                37            34              81.0
     9:      France                36            28              80.8
    10:      Greece                26            25              79.5

    === COX PROPORTIONAL HAZARDS (Mortality Risk relative to Denmark) ===
                                 coef exp(coef)  se(coef)           z    Pr(>|z|)
    country_fFrance       0.007848938 1.0078798 0.2894828  0.02711366 0.978369077
    country_fGermany      0.141963171 1.1525342 0.3026556  0.46905851 0.639027818
    country_fGreece       0.783048647 2.1881330 0.2966126  2.63997088 0.008291315
    country_fItaly       -0.057924268 0.9437214 0.2529252 -0.22901738 0.818855411
    country_fNetherlands  0.340536043 1.4057009 0.2762343  1.23277978 0.217657939
    country_fNorway       0.341179262 1.4066054 0.2822572  1.20875301 0.226757751
    country_fSpain        0.487558004 1.6283350 0.2747832  1.77433691 0.076007439
    country_fSweden       0.121181613 1.1288299 0.2922351  0.41467164 0.678382293
    country_fUK           0.215425978 1.2403902 0.2995657  0.71912768 0.472062255
