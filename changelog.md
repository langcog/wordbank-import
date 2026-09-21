# Changelog

## v3.3
### Updates
- Some standardisation to item definitions for disambiguation and unilemmas for consistency.

## v3.2
### Updates
- Date of test information now added for datasets which had them in the data but which had not imported it yet.

## v3.1
### Bug fixes
- Some items in Mandarin (Beijing) WS had incorrect item definitions that are now fixed.

## v3.0
### Updates
- ASL, Dutch, and short form names are now rendered with numbers and spaces where appropriate (e.g., "FormOne" -> "Form 1")
- German -> German (German) in anticipation of German (Swiss)
- Import now makes stricter requirements on demographic matching for children with the same study_internal_id
  - When mismatches are due to NAs, they are filled (silent)
  - When mismatches are due to caregiver education, the max is taken (warning)
  - When mismatches are due to birth order, race, ethnicity, or sex, importers must now manually fix either the demographic variable or disambiguate the study_internal_id; with no other info this often means changing all values to NA (error)

### Bug fixes
- Bergman -> Bergmann
- Recombine JISH Arabic (Saudi) WS and WS Other
- Standardised format of contributors and citations
- Many small tweaks to data bugs (Alroqi, Blume, Woll, Bergmann Swingley, Kapalkova)
- Bergmann French (French) WG re-ingested as WS
- Tsuji Labvanced French (French) WS had comprehension and production swapped
- Arabic (Saudi) WS and Oxford CDIs have their age ranges fixed
- Trudeau French (Quebecois) has combine fixed for children ≥24m
