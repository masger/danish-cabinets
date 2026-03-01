# Reviewer Comments - Round 1

**Manuscript ID**: BMJ-2024-XMAS-TRANSPORTMINISTER
**Reviewer**: Dr. Sven Kritiskansen, Department of Political Methodology, Aarhus University

---

## Overall Assessment

The authors present an interesting analysis attempting to debunk the "Skatteminister Syndrome" using survival analysis methods. While the premise is entertaining and appropriate for the BMJ Christmas issue, I have several methodological concerns that must be addressed before publication.

**Recommendation**: Major Revision

---

## Major Concerns

### 1. The 30-day threshold is arbitrary and inadequately justified

The authors classify ministers as "fired" if they left more than 30 days before government termination. This threshold appears chosen without theoretical or empirical justification.

**Required**:
- Provide rationale for the 30-day cutoff
- Present sensitivity analyses at alternative thresholds (e.g., 14 days, 60 days, 90 days)
- Report whether conclusions change with different thresholds

### 2. Multiple testing without correction

The authors test multiple portfolios against Finance Minister without adjusting for multiple comparisons. With 15+ comparisons, some "significant" findings may be false positives.

**Required**:
- Apply Bonferroni correction or report false discovery rate (FDR)-adjusted p-values
- Discuss which findings remain significant after correction

### 3. The "ambiguous" category is large and problematic

Nearly half (49.2%) of observations fall into the "Near government end" category. Excluding or treating these as censored may bias results.

**Required**:
- Sensitivity analysis including ambiguous cases as either "fired" or "election exit"
- Discussion of how this categorization uncertainty affects conclusions

### 4. Confounding by government duration

Short-lived governments mechanically reduce the opportunity for mid-term firings. A minister in a 6-month government cannot be "fired" in the same way as one in a 4-year government.

**Required**:
- Stratify analysis by government duration
- Consider competing risks framework

---

## Minor Concerns

### 5. Career tracking across governments

The authors mention career outcomes after leaving government but do not incorporate this into the main analysis. Ministers who "return quickly" may represent reshuffles rather than true exits.

**Suggestion**: Consider a multi-state survival model (ministerial position -> out of cabinet -> return to cabinet).

### 6. Small sample sizes

Several portfolios have fewer than 30 observations (e.g., Skatteminister n=26). Wide confidence intervals reflect this uncertainty, but the narrative tone suggests more certainty than the data support.

**Suggestion**: Temper conclusions about Tax Ministers given only 2 events among 26 observations.

### 7. Missing: What happened to the fired ministers?

The analysis would be enriched by examining what happened to ministers who were "fired." Did they:
- Return to parliament?
- Leave politics entirely?
- Face scandals?

This would help distinguish different types of ministerial "death."

---

## Minor Editorial Issues

1. Line 42: "ministerial mortality" should be "ministerial termination" for clarity
2. The term "political death" is used inconsistently - sometimes meaning firing, sometimes meaning career end
3. Consider adding a CONSORT-style flow diagram showing how observations were classified

---

## Summary

The paper has merit and the central finding (Tax Ministers are safe, Transport Ministers are dangerous) appears robust to visual inspection. However, the methodological concerns above must be addressed to ensure the conclusions are defensible. I look forward to a revised manuscript.

*Dr. Sven Kritiskansen*
*Aarhus University*

---

**Editor's note**: Please address all major concerns and as many minor concerns as feasible. We appreciate the festive spirit of the submission but require methodological rigor even for Christmas papers.
