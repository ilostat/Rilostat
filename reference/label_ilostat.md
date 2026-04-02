# Switch ilostat codes and labels

Gets definitions/labels for ilostat codes from ilostat dictionaries.

## Usage

``` r
label_ilostat(
  x,
  dic = NULL,
  code = NULL,
  lang = getOption("ilostat_lang", "en")
)
```

## Arguments

- x:

  A character or a factor vector or a data_frame to labelled.

- dic:

  A string (vector) naming ilostat dictionary or dictionaries. If `NULL`
  (default) dictionary names are taken from column names of the
  data_frame. A character or a factor vector or a data_frame to
  labelled,

- code:

  a vector of names of the column for which code columns should be
  retained. Set to `"all"`, keep all the code.

- lang:

  a character, code for language. Available are `"en"` (default), `"fr"`
  and `"es"`. Can be set also with options(ilostat_lang = 'fr'),

## Value

a vector or a data_frame. The suffix ".label" is added to code column
names.

## Details

A character or a factor vector of codes returns a corresponding vector
of definitions. `label_ilostat` labels also data_frames from
[`get_ilostat`](https://ilostat.github.io/Rilostat/reference/get_ilostat.md).
For vectors a dictionary "time" and "values" columns are returned as
they were, so you can supply data_frame from
[`get_ilostat`](https://ilostat.github.io/Rilostat/reference/get_ilostat.md)
and get data_frame with definitions instead of codes.

## References

See citation("Rilostat") ilostat bulk download facility user guidelines
<https://ilostat.ilo.org/data/bulk/>

## Author

David Bescond <bescond@ilo.org>

## Examples

``` r
if (FALSE) { # \dontrun{
 dat <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A", cache = FALSE)
 dat_lab <- label_ilostat(dat)
 head(dat_lab)


 # add just ref_area label
require(tidyverse)

dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A") %>% 
  mutate(ref_area.label = ref_area %>% 
  label_ilostat( "ref_area", code = "all"), .after = ref_area)



 clean_ilostat_cache() 
} # }
```
