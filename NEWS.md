
# Rilostat 2.5.2

### Improvements
- Added configurable remote and local data backends using `ILOSTAT_BACKEND` and `ILOSTAT_DATA_ROOT`.
- Optimized local RDS retrieval by filtering `best_source` before converting observations to tibbles.
- Enabled direct local access to dictionaries and tables of contents without additional caching.
- Preserved disk caching for remote API users.
- Fixed time conversion when reusing cached datasets.
- Removed obsolete Plotly mapping and associated examples.
- Removed unused internal session environment.

### Validation
- Verified local and remote data retrieval.
- Validated multilingual dictionaries and labels.
- Validated quarterly time conversion and remote cache reuse.
- R CMD check: 0 errors, 0 warnings, 0 notes.

# Rilostat 2.5.1

* restored `get_ilostat_sdmx()` for SDMX-oriented access to ILOSTAT data.
* Added SDMX structures, codelists and dataflows.
* Added SDMX-ML 3.1 and SDMX-JSON 2.1 support for structural resources.

# Rilostat 2.5.0

* best_source control 

# Rilostat 2.3.4

* minor fix

# Rilostat 2.3.3

* better header, better cache clean up

# Rilostat 2.3.0

* faster method to get data

# Rilostat 2.2.0

* add dataexplorer()

# Rilostat 2.1.0

* use ilostat API via https://rplumber.ilo.org/

# Rilostat 2.0.0

* faster method to get data

# Rilostat 1.2.0

* fixed ilo new path
* fixed sdmx new methods

# Rilostat 1.1.8

* add distribution feature

# Rilostat 1.1.7

* fixed dependency issues

# Rilostat 1.1.5

* update vignette


# Rilostat 1.1

* add data explorer app
* sdmx_ilostat new path
* test with R 4.0
* Rilostat replace ilostat 
* refresh doc

# Rilostat 1.0.1

* change ilostat path
* remove collection field

# Rilostat 1.0.0

* sdmx_ilostat new path
* use dplyr > 8
* add vignettes intro
* add pkgdown [website](https://ilostat.github.io/Rilostat/) 

# Rilostat 0.2.1

* add segment 'modelled_estimates' for new dta files
* sdmx_ilostat return threshold error

# Rilostat 0.2

* fixed .onload warnings
* reviewed import packages version
* fixed sdmx codelist annotation multi lang issues
* revised import package list and version
* fixed tutorial mapper error

# Rilostat 0.1

* first release
