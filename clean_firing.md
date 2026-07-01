# True Firing Data


``` r
source("scripts/clean_firing_data.R")
```


    Vedhæfter pakke: 'lubridate'

    De følgende objekter er maskerede fra 'package:data.table':

        hour, isoweek, mday, minute, month, quarter, second, wday, week,
        yday, year

    De følgende objekter er maskerede fra 'package:base':

        date, intersect, setdiff, union

    Warning: 3 failed to parse.

    === THE TRUE EMPIRICAL FATES OVER 50 YEARS ===
    Total Ministers Analyzed:  1155 

       portfolio Total Survived to Election (%) Truly Fired (%) Died in Office (%)
          <char> <int>                    <num>           <num>              <num>
    1:  Interior   275                     68.4            30.5                1.1
    2:    Health   222                     77.0            22.5                0.5
    3:   Finance   195                     80.5            19.0                0.5
    4:   Defense   219                     80.8            18.3                0.9
    5: Transport   244                     81.6            18.0                0.4

    === SURVIVED PAST THE PENSION POINT (1.5 YEARS) ===
       portfolio Total Reached 1.5 Yr Pension (%)
          <char> <int>                      <num>
    1:  Interior   275                       49.5
    2:   Finance   195                       55.9
    3: Transport   244                       56.6
    4:    Health   222                       60.4
    5:   Defense   219                       60.7

    === THE DEFENSE MEAT GRINDER: FIRING OVER TIME ===
                                 coef exp(coef)  se(coef)           z     Pr(>|z|)
    portfolio_fDefense   -0.006469053 0.9935518 0.2285420 -0.02830576 0.9774182902
    portfolio_fHealth     0.300751757 1.3508740 0.2174989  1.38277355 0.1667343123
    portfolio_fInterior   0.702945702 2.0196934 0.1980333  3.54963414 0.0003857669
    portfolio_fTransport  0.097132637 1.1020065 0.2240594  0.43351286 0.6646422268
