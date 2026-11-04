# Clean ilostat Cache

Deletes all cache files from the your ilostat cache directory. See
[`get_ilostat`](https://ilostat.github.io/Rilostat/reference/get_ilostat.md)
for more on cache.

## Usage

``` r
clean_ilostat_cache(
  cache_dir = getOption("ilostat_cache_dir", file.path(tempdir(), "ilostat")),
  cache_update = getOption("ilostat_cache_update", FALSE),
  quiet = getOption("ilostat_quiet", TRUE)
)
```

## Arguments

- cache_dir:

  A character, path to a cache directory. The directory has to exist.
  The `NULL` (default) uses and creates 'ilostat' directory in the
  temporary directory from
  [`tempdir`](https://rdrr.io/r/base/tempfile.html). The directory can
  also be set with `option` ilostat_cache_dir,

- cache_update:

  a logical whether to delete only out of date cache files. Useful when
  `cache_dir` is set as keep only update datasets. Can be set also with
  options(ilostat_update = TRUE). Default is `FALSE`.

- quiet:

  a logical, if `TRUE` , don't return message from processing, `FALSE`
  (default). Can be set also with options(ilostat_quiet = TRUE),

## References

See citation("Rilostat") ilostat bulk download facility user guidelines
<https://ilostat.ilo.org/data/bulk/>

## Author

David Bescond <bescond@ilo.org>

## Examples

``` r
if (FALSE) { # \dontrun{
clean_ilostat_cache() 
} # }
```
