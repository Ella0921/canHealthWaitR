# canHealthWaitR: 

### Exploring Healthcare Wait Times in Canada Using Web-Based Open Data

[![](https://github.com/Ella0921/canHealthWaitR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Ella0921/canHealthWaitR/actions/workflows/R-CMD-check.yaml)

------------------------------------------------------------------------

## Overview

**canHealthWaitR** is an R package that provides a structured and reproducible interface to selected Canadian health care wait time data published by **Statistics Canada** via its **Web Data Service (WDS) API**.

The package wraps raw REST API calls into a small set of consistent R functions, handling data retrieval, minimal wrangling, and standardization so that users can move directly from data access to analysis and visualization.

------------------------------------------------------------------------

## Motivation & background

Healthcare wait times are a persistent and widely discussed issue in Canada, frequently raised in online communities and public discourse.

Common concerns include long emergency room waits, difficulty accessing primary care, and extended delays for specialist referrals.

Despite the prominence of this issue, healthcare wait time data is often fragmented across multiple public sources, making systematic analysis difficult for non-experts.

------------------------------------------------------------------------

## What problem does this package solve?

Statistics Canada’s WDS API provides powerful access to official data, but:

-   API responses are returned in nested JSON structures
-   Column names, encodings, and dimensions vary across tables
-   Substantial preprocessing is required before analysis or plotting

`canHealthWaitR` abstracts these complexities by:

-   Maintaining a curated registry of relevant health wait time tables
-   Providing a standardized tabular output across products
-   Offering ready-to-use visualization helpers for common analytical questions

This allows users to focus on **analysis and interpretation**, rather than API mechanics.

------------------------------------------------------------------------

## Installation

You can install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("Ella0921/canHealthWaitR")
```

Then load the package:

``` r
library(canHealthWaitR)
```

------------------------------------------------------------------------

## Core workflow

The package is designed around a simple and consistent workflow:

1.  **Discover** curated Statistics Canada tables

2.  **Download** raw data via the WDS API

3.  **Standardize** the data into a common schema

4.  **Filter and visualize** results

------------------------------------------------------------------------

## Example

``` r
library(canHealthWaitR) 
library(dplyr)  

# List available curated tables 
mw_list_tables()  

# Resolve a table of interest 
tbl <- mw_resolve_table("wait_time")  

# Download raw data 
raw <- mw_read_data(tbl$product_id)  

# Standardize the dataset 
std <- mw_standardize(raw, table_id = tbl$product_id)  

# Plot number of people affected by wait times in 2024 
mw_plot_affected_province(std, year = 2024)
```

------------------------------------------------------------------------

## Relationship to Statistics Canada WDS

This package is built on top of Statistics Canada’s official\
**Web Data Service (WDS)** API:

[https://www.statcan.gc.ca/en/developers/wds](https://www.statcan.gc.ca/en/developers/wds?utm_source=chatgpt.com)

`canHealthWaitR` does **not** replicate or modify the source data.\
It provides a convenience layer for accessing, cleaning, and visualizing selected tables related to health care wait times.

------------------------------------------------------------------------

## Package status

This package was developed as part of the **DATA 534 – Collaborative Software Development**\
group project.

-   The API wrapper is functional and tested

-   Documentation and vignettes are provided

-   Continuous integration is enabled via GitHub Actions

The package is open to further extension as new data become available.

------------------------------------------------------------------------

## Authors

-   Yin-Wen Tsai

-   Sasivimol Sirijangkapattana

-   Bingzheng Jin

------------------------------------------------------------------------

## License

MIT License. See `LICENSE` and `LICENSE.md` for details.
