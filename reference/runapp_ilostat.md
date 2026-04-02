# Play with ILOSTAT data apps

Open various data exploration and analysis applications hosted by the
ILO.

## Usage

``` r
runapp_ilostat(name = "dataexplorer")
```

## Arguments

- name:

  \`chr\` The name of the application to open.

  Available applications:

  - "dataexplorer": Quickly find, filter, pivot, and download ILOSTAT
    data.

  - "microquery": Explore data availability and create custom queries
    from the ILO Harmonized Microdata Collection. Users can select
    variables, filters, and indicators to generate tailored datasets.

  - "regionalaggregate": Generate custom regional estimates by defining
    or selecting country groupings. The app automatically aggregates
    country-level modelled data for selected indicators.

## References

See citation("Rilostat")

## See also

Additional documentation for microdata:

- [Quick guide on microdata
  processing](https://www.ilo.org/publications/ilostat-microdata-processing-quick-guide-principles-and-methods-underlying)

- [Adjustment procedure for ISCO- and ISIC-based
  estimates](https://rplumber.ilo.org/files/website/Ad_Hoc_Micro_ISIC_ISCO_Adjustment_procedure.pdf)

- [Methodology for ad hoc global and regional
  estimates](https://rplumber.ilo.org/files/website/Ad_Hoc_Micro_ISIC_ISCO_Global_Regional_Estimates.pdf)

- [Citations](https://rplumber.ilo.org/files/website/Ad_Hoc_Micro_Query_Citation.pdf)

## Author

David Bescond <bescond@ilo.org>

## Examples

``` r
if (FALSE) { # \dontrun{

  runapp_ilostat(name = "dataexplorer")

  # You can also run other apps like:
  runapp_ilostat(name = "microquery")
  runapp_ilostat(name = "regionalaggregate")


} # }
```
