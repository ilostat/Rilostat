# Switch ilostat to distribution

Get distribution for ilostat number of persons only.

## Usage

``` r
distribution_ilostat(
  x,
  var,
  .keep = FALSE,
  quiet = getOption("ilostat_quiet", TRUE)
)
```

## Arguments

- x:

  dataset to transform into distribution.

- var:

  String variable name use for the distribution default `"no"`, could be
  `"sex"`, `"classif1"`, `"classif2"`.

- .keep:

  if true return only new column call distribution default `FALSE`,

- quiet:

  a logical, if `TRUE` , don't return message from processing, `FALSE`
  (default). Can be set also with options(ilostat_quiet = TRUE).

## Value

a data_frame. obs_status will no longer be a number of persons but a
percentage.

## Details

this function use the max of the corresponding grouping so it is
important to not filter any subset of the corresponding variable
selected for the distribution at this level, ie. if you remove SEX_T,
the distribution by sex will only have SEX_F or SEX_M / max(SEX_M,
SEX_F) \* 100, which is no longer a distribution.

In addition, distribution is only applicable for indicators with Number
of persons (usually in thousands), So please do not distribute ratios,
earnings, hours of works, CPI, GDP etc ... no warning will prevent for
that if doubts use distribution from get_ilostat() instead of, warnings
will help you.

## References

See citation("Rilostat") ilostat bulk download facility user guidelines
<https://ilostat.ilo.org/data/bulk/>

## Author

David Bescond <bescond@ilo.org>

## Examples

``` r
if (FALSE) { # \dontrun{
 dat <- get_ilostat("EMP_TEMP_SEX_STE_GEO_NB_A", cache = FALSE)
 dat_dist <- distribution_ilostat(dat, "classif1")
 dat_plus_dist <- mutate(dat, dist = distribution_ilostat(dat,"classif1", .keep=TRUE))
 head(dat_dist)
 clean_ilostat_cache() 
} # }
```
