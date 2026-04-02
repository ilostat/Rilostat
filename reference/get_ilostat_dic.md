# Read ilostat Dictionary

Downloads one ilostat dictionary from ilostat <https://ilostat.ilo.org>
via bulk download facility <https://ilostat.ilo.org/data/bulk/>.

## Usage

``` r
get_ilostat_dic(
  dic,
  lang = getOption("ilostat_lang", "en"),
  cache_dir = getOption("ilostat_cache_dir", NULL),
  quiet = getOption("ilostat_quiet", TRUE)
)
```

## Arguments

- dic:

  A character, dictionary for the variable to be downloaded,

- lang:

  a character, code for language. Available are `"en"` (default), `"fr"`
  and `"es"`. Can be set also with options(ilostat_lang = 'fr'),

- cache_dir:

  a path to a cache directory. The directory has to exist. The `NULL`
  (default) uses and creates 'ilostat' directory in the temporary
  directory from [`tempdir`](https://rdrr.io/r/base/tempfile.html). The
  directory can also be set with `option` ilostat_cache_dir,

- quiet:

  a logical, if `FALSE` , return message from processing, `TRUE`
  (default). Can be set also with options(ilostat_quiet = TRUE),

## Value

tibble with two columns: code names and full names.

## Details

For a given coded variable from ilostat <https://ilostat.ilo.org/>. The
dictionaries link codes with human-readable labels. To translate codes
to labels, use
[`label_ilostat`](https://ilostat.github.io/Rilostat/reference/label_ilostat.md).

## References

See citation("Rilostat") ilostat bulk download facility user guidelines
<https://ilostat.ilo.org/data/bulk/>

## See also

[`label_ilostat`](https://ilostat.github.io/Rilostat/reference/label_ilostat.md),
[`get_ilostat`](https://ilostat.github.io/Rilostat/reference/get_ilostat.md).

## Author

David Bescond <bescond@ilo.org>

## Examples

``` r
if (FALSE) { # \dontrun{
 tmp <- get_ilostat_dic("indicator")
 head(tmp)
 tmp <- get_ilostat_dic("classif1", lang = "fr")
 head(tmp)
} # }
```
