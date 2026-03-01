# Response to Reviewers - Revision 1

**Manuscript ID**: BMJ-2024-XMAS-TRANSPORTMINISTER
**Title**: "THE TRANSPORTMINISTER CURSE: Debunking the Skatteminister myth"

---

Dear Dr. Kritiskansen,

We thank you for your thorough and constructive review. Your concerns are well-founded and have improved the manuscript considerably. We address each point below.

---

## MAJOR CONCERNS

### 1. The 30-day threshold requires justification and sensitivity analysis

**Reviewer concern**: The 30-day threshold for classifying "fired" is arbitrary.

**Response**: We agree this requires justification. We selected 30 days based on the typical notice period in Danish public sector employment and the observation that ministerial reshuffles occurring close to government termination often reflect pre-election positioning rather than performance-based dismissal.

We have conducted sensitivity analyses at multiple thresholds:

```r
# Sensitivity analysis results
thresholds <- c(14, 30, 60, 90)
# Results:
# Threshold | N_fired | Skatte_rate | Transport_rate | Interior HR (p)
# 14 days   | 143     | 7.7%        | 29.2%          | 2.98 (0.068)
# 30 days   | 176     | 7.7%        | 33.3%          | 3.30 (0.044)
# 60 days   | 208     | 11.5%       | 37.5%          | 3.15 (0.039)
# 90 days   | 231     | 15.4%       | 41.7%          | 2.89 (0.052)
```

**Key finding**: Tax Minister remains the safest portfolio across all thresholds. The Transport/Interior hazard elevation is robust, though significance varies.

**Changes made**: Supplementary Table S1 added with sensitivity analysis. Methods section now justifies the 30-day threshold.

---

### 2. Multiple testing correction

**Reviewer concern**: Multiple portfolio comparisons inflate Type I error.

**Response**: We have applied Bonferroni correction (α = 0.05/15 = 0.0033) and report FDR-adjusted p-values.

**Results after correction**:

| Portfolio | Raw p-value | Bonferroni-adjusted | FDR-adjusted |
|-----------|-------------|---------------------|--------------|
| Indenrigsminister | 0.044 | 0.66 (NS) | 0.33 (NS) |
| Arbejdsminister | 0.046 | 0.69 (NS) | 0.33 (NS) |
| Transportminister | 0.067 | 1.00 (NS) | 0.33 (NS) |

**Interpretation**: After multiple testing correction, no individual portfolio reaches statistical significance. However, we note:

1. The consistent direction of effects (Transport, Interior, Labor all elevated; Tax, Foreign Affairs, PM all protective)
2. The pattern is biologically plausible (high-visibility vs. technical portfolios)
3. Sample size limitations reduce power rather than validity

We have revised the manuscript to characterize findings as "suggestive patterns" rather than "significant effects."

**Changes made**: Methods now describes correction approach. Results text tempered. Supplementary Table S2 shows adjusted p-values.

---

### 3. The "ambiguous" category

**Reviewer concern**: 49.2% of observations are classified as "ambiguous."

**Response**: This reflects reality - many ministerial transitions occur in the final weeks of a government when electoral uncertainty is high. We conducted sensitivity analyses:

**Scenario A**: Ambiguous → Fired
- Skatteminister firing rate: 19.2% (still below average of 34.8%)
- Transport: 50.0% (elevated)

**Scenario B**: Ambiguous → Election exit
- Results identical to main analysis

**Conclusion**: Tax Minister remains relatively protected under both assumptions. We now report both scenarios in Supplementary Table S3.

**Changes made**: Supplementary analysis added. Discussion acknowledges this limitation.

---

### 4. Confounding by government duration

**Reviewer concern**: Short governments limit firing opportunities.

**Response**: Excellent point. We stratified by government duration:

```r
# Government duration categories:
# Short: < 2 years (n=12 governments)
# Medium: 2-4 years (n=18 governments)
# Long: > 4 years (n=7 governments)

# Firing rates by government duration:
# Short governments: 14.2% overall firing rate
# Medium governments: 24.8%
# Long governments: 31.5%
```

The pattern holds within strata: Tax Ministers remain among the safest even in long-duration governments where there is ample opportunity for dismissal.

We also estimated a competing risks model treating government termination as a competing event:

```r
# Fine-Gray subdistribution hazard for firing (competing risk: election)
# Interior Minister: SHR 2.87 (95% CI 1.01-8.15), p=0.048
# Transport Minister: SHR 2.54 (95% CI 0.89-7.24), p=0.081
# Tax Minister: SHR 0.65 (95% CI 0.15-2.82), p=0.567
```

**Changes made**: Stratified analysis added. Competing risks model in Supplementary Analysis S4.

---

## MINOR CONCERNS

### 5. Career tracking across governments

**Response**: We have added a multi-state analysis tracking ministerial careers:

- 44.3% of departing ministers never returned to cabinet
- 57.4% of "fired" ministers eventually returned vs. 46.0% of election-exit ministers

This suggests "firing" is often not a terminal career event. Added to Discussion.

### 6. Small sample sizes

**Response**: We agree and have tempered conclusions:

> "With only 2 firing events among 26 Tax Minister appointments, we characterize the Skatteminister as having **suggestively low** firing risk rather than definitively protective."

### 7. What happened to fired ministers?

**Response**: We added analysis of post-firing careers:

| Outcome | Fired ministers | Election-exit ministers |
|---------|-----------------|-------------------------|
| Returned to cabinet | 57.4% | 46.0% |
| Returned to parliament only | 23.9% | 31.3% |
| Left politics entirely | 18.8% | 22.7% |

Surprisingly, fired ministers are MORE likely to return to cabinet, suggesting "firing" may often reflect political circumstances rather than personal failure.

---

## EDITORIAL ISSUES

1. ✅ Changed "ministerial mortality" to "ministerial termination" throughout
2. ✅ Defined "political death" consistently as mid-term dismissal
3. ✅ Added CONSORT-style flow diagram (Figure S1)

---

## SUMMARY OF CHANGES

| Section | Change |
|---------|--------|
| Methods | 30-day threshold justified; multiple testing correction described |
| Results | Conclusions tempered; confidence intervals emphasized |
| Discussion | Limitations expanded; competing risks discussed |
| Supplement | Tables S1-S4 added with sensitivity analyses |
| Figures | Flow diagram added (Figure S1) |

We believe these revisions address all concerns while maintaining the paper's contribution to Danish political epidemiology.

Respectfully yours,

**The Authors**

*P.S. - We note that Dr. Kritiskansen's surname suggests a predisposition to criticism (kritisk = critical in Danish). We appreciate that this professional hazard was channeled constructively toward improving our manuscript.*
