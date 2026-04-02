# Read ilostat Table of Contents / workflow

Download one table of contents from ilostat <https://ilostat.ilo.org>
via bulk download facility <https://ilostat.ilo.org/data/bulk/>.

## Usage

``` r
get_ilostat_toc(
  segment = getOption("ilostat_segment", "indicator"),
  lang = getOption("ilostat_lang", "en"),
  search = getOption("ilostat_search", "none"),
  filters = getOption("ilostat_filter", "none"),
  fixed = getOption("ilostat_fixed", TRUE),
  cache_dir = getOption("ilostat_cache_dir", NULL),
  cache_update = getOption("ilostat_cache_update", TRUE),
  quiet = getOption("ilostat_quiet", TRUE)
)
```

## Arguments

- segment:

  A character, way to get datasets by: `"indicator"` (default) or
  `"ref_area"`, Can be set also with options(ilostat_segment =
  "ref_area"),

- lang:

  a character, code for language. Available are `"en"` (default), `"fr"`
  and `"es"`. Can be set also with options(ilostat_lang = "fr"),

- search:

  a character vector, "none" (default), datasets with this pattern in
  the description will be returned, characters vector will be use as
  AND, Character with "\|" as OR, see example,
  options(ilostat_time_format = "date"),

- filters:

  a list; `"none"` (default) to get a whole toc or a named list of
  filters to get just part of the table. Names of list objects are
  ilostat toc variable codes and values are vectors of observation
  codes. filters detect on variables.

- fixed:

  a logical, if `TRUE` (default), pattern is a string to be matched as
  is, Change to `FALSE` if more complex regex matching is needed.

- cache_dir:

  a path to a cache directory. The directory has to exist. The `NULL`
  (default) uses and creates 'ilostat' directory in the temporary
  directory from [`tempdir`](https://rdrr.io/r/base/tempfile.html). The
  directory can also be set with `option` ilostat_cache_dir,

- cache_update:

  a logical whether to update cache. Check cache update with last.update
  attribute store on the cache file name and the one from the table of
  contents. Can be set also with options(ilostat_cache_update = FALSE).
  Default is `TRUE`,

- quiet:

  a logical, if `FALSE` , return message from processing, `TRUE`
  (default). Can be set also with options(ilostat_quiet = TRUE),

## Value

A tibble with ten columns depending of the segment: indicator or
ref_area

- `id` : The codename of dataset of theme, will be used by the
  get_ilostat and get_ilostat_raw functions,

- `indicator or ref_area` : The indicator or ref_area code of dataset,

- `indicator.label or ref_area.label` : The indicator or ref_area name
  of dataset,

- `freq` : The frequency code of dataset,

- `freq.label` : Is freq name of dataset,

- `size` : Size of the csv.gz files,

- `data.start` : First time period of the dataset,

- `data.end` : Last time period of the dataset,

- `last.update` : Last update of the dataset,

- `...` : Others relevant information

## Details

The toc in English by indicator is downloaded from ilostat API
<https://rplumber.ilo.org/__docs__/#get-/metadata/toc/indicator/>. The
values in column "id" should be used to download a selected dataset.

The toc in English by ref_area is downloaded from ilostat API
<https://rplumber.ilo.org/__docs__/#get-/metadata/toc/ref_area/>. The
values in column "id" should be used to download a selected dataset.

## References

See citation("Rilostat") ilostat bulk download facility user guidelines
<https://ilostat.ilo.org/data/bulk/>

## See also

[`get_ilostat`](https://ilostat.github.io/Rilostat/reference/get_ilostat.md).

## Author

David Bescond <bescond@ilo.org>

## Examples

``` r
if (FALSE) { # \dontrun{
## default segment by indicator, default lang English
 toc <- get_ilostat_toc()
 head(toc)
 toc <- get_ilostat_toc(segment = "ref_area", lang = "fr")
 head(toc)
##
## search on toc
 toc <- get_ilostat_toc(search = "education")
 head(toc)
 toc <- get_ilostat_toc(lang = "fr", search = "age")
 head(toc)
 toc <- get_ilostat_toc(segment = "ref_area", lang = "fr", search = "Albanie")
 toc
 toc <- get_ilostat_toc(segment = "ref_area", lang = "es", search = "Trimestral")
 head(toc)
##
## search multi on toc
 toc <- get_ilostat_toc(segment = "ref_area", lang = "fr", 
             search = "Albanie|France", fixed = FALSE)
 head(toc)
 toc <- get_ilostat_toc(search = "youth|adult", fixed = FALSE)
 head(toc)
##
} # }
```
