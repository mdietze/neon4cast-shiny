
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neon4cast-shiny

## Overview

Source code for EFI-NEON Ecological Forecasting Challenge Dashboard
Shiny app at [shiny.ecoforecast.org](https://shiny.ecoforecast.org/)

## Testing Shiny app locally

The code in `index.Rmd` that runs the app assumes the forecasts and
target files are saved in local directories. Towards the top of
`index.Rmd`

  - `base.dir`: `"/efi_neon_challenge/forecasts/"`
  - `target.dir`: `"/efi_neon_challenge/targets/"`

You can change these directory paths during local development, but be
sure to revert them before merging to GitHub.

## Example using phenology forecasts

Here is an example on how to get the Shiny app running locally for the
phenology forecasts:

1.  Install the development version of the
    [`neon4cast`](https://github.com/eco4cast/neon4cast) R package from
    GitHub. In particular we need the
    [`download_forecast()`](https://github.com/eco4cast/neon4cast/blob/main/R/download_forecast.R)
    function.
    
    ``` r
    # install.packages("remotes")
    remotes::install_github("eco4cast/neon4cast")
    ```

2.  Download the phenology forecasts to the `base.dir`
    
    ``` r
    library(neon4cast)
    download_forecast(theme = "phenology", dir = "/efi_neon_challenge/forecasts/")
    ```

3.  Download the corresponding targets files on
    [data.ecoforecast.org/minio/targets/](https://data.ecoforecast.org/minio/targets/)
    to the `target.dir`. So for our phenology example:
    
    1.  Go to the [phenology targets
        folder](https://data.ecoforecast.org/minio/targets/phenology/)
    2.  Click on both `prov.json` and `phenology-targets.csv.gz` and
        download
    3.  Move these files to to `/efi_neon_challenge/targets/phenology/`

4.  Go to `index.Rmd` and click on “Run Document” to load the Shiny app
