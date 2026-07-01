# Sig Check


``` r
source("scripts/check_joker_sigs.R")
```


    Vedhæfter pakke: 'lubridate'

    De følgende objekter er maskerede fra 'package:data.table':

        hour, isoweek, mday, minute, month, quarter, second, wday, week,
        yday, year

    De følgende objekter er maskerede fra 'package:base':

        date, intersect, setdiff, union

    Warning: 12 failed to parse.

    --- SIGNIFICANT JOKER HYPOTHESES (p < 0.05) ---
    [SIGNIFICANT] H4: Multi-term Ministers: p = 0.0068
    [SIGNIFICANT] H5: Last initial K or F: p = 0.0242
    [SIGNIFICANT] H8: Age > 65: p = 0.0001
    [SIGNIFICANT] H9: Greece/Italy: p = 0.0000

    --- ALL OTHER JOKER HYPOTHESES (p >= 0.05) ---
    [Not Sig] H1: Name Length vs Tenure: p = 0.2103
    [Not Sig] H2: Zodiac Capricorn: p = 0.5819
    [Not Sig] H7: Age < 40: p = 0.7823
    [Not Sig] H10: Age 49-51: p = 0.9667
    [Not Sig] H11: Fired in November: p = 0.5871
    [Not Sig] H12: Alliterative Name: p = 0.0993
    [Not Sig] H13: Weekend Appointment: p = 0.0869
    [Not Sig] H14: UK Transport Minister: p = 0.1480
    [Not Sig] H15: Appointed in Leap Year: p = 0.1313
    [Not Sig] H18: Name ends in 'a' (Defense): p = 0.3597
    [Not Sig] H19: 3-Letter Last Name: p = 0.5270
