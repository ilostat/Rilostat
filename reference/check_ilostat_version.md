# Check Rilostat version against CRAN

Compare installed version with CRAN and warn if outdated.

## Usage

``` r
check_ilostat_version(pkg = "Rilostat", quiet = FALSE)
```

## Arguments

- pkg:

  Package name (default "Rilostat")

- quiet:

  Logical, suppress message if up-to-date

## Value

Invisible logical (TRUE = up-to-date, FALSE = outdated or unknown)
