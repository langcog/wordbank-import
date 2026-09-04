# Changelog

## 2026.1
### Updates
- [br] ASL, Dutch, and short form names are now rendered with numbers and spaces where appropriate (e.g., "FormOne" -> "Form 1")
- [br] German -> German (German) in anticipation of German (Swiss)
- [br] Import now makes stricter requirements on demographic matching for children with the same study_internal_id. 
  - When mismatches are due to NAs, they are filled (silent)
  - When mismatches are due to caregiver education, the max is taken (warning)
  - When mismatches are due to birth order, race, ethnicity, or sex, importers must now manually fix either the demographic variable or disambiguate the study_internal_id; with no other info this often means changing all values to NA (error)

### Bug fixes
- [br] Bergman -> Bergmann
- [br] Recombine JISH Arabic (Saudi) CDI WS and WS Other
- [br] Standardised format of contributors and citations
- [br] Many small tweaks to data bugs (Alroqi, Blume, Woll, Bergmann Swingley, Kapalkova)
- [br] Bergmann French (French) WG re-ingested as WS
