# canHealthWaitR
Tools for exploring Canadian healthcare wait time / access-to-care indicators using Statistics Canada’s Web Data Service (WDS).

## Motivation & background
Healthcare wait times are a persistent and widely discussed issue in Canada, frequently raised in online communities and public discourse.

Common concerns include long emergency room waits, difficulty accessing primary care, and extended delays for specialist referrals.

Despite the prominence of this issue, healthcare wait time data is often fragmented across multiple public sources, making systematic analysis difficult for non-experts.

## Data source / API
This package uses **Statistics Canada Web Data Service (WDS)**, an official public RESTful API.

Why WDS?
- Official government source (stable & well-documented)
- No login, no API key required
- Data structured into consistent statistical tables (“cubes”)
- Supports reproducible workflows: **query → tidy data → plot**

## Installation
Install the development version from GitHub:

```r
# Install development version from GitHub
# if (!require("devtools")) install.packages("devtools")
devtools::install_github("Ella0921/canHealthWaitR")

# Load the package
library(canHealthWaitR)
```
## Quick start
library(canHealthWaitR)

# 1) Discover tables
mw_list_tables()
mw_search_tables(query = "wait")

# 2) Inspect metadata (dimensions / members / vectors availability)
meta <- mw_table_metadata(product_id = "YOUR_PRODUCT_ID")

# 3) Retrieve data for a specific table (e.g., Specialized health services wait times)
df <- mw_get_data(
  product_id = "13100293", 
  geography  = "British Columbia",
  start_year = 2015
)

# 4) Standardize + filter
df_std <- mw_standardize(df)
df_bc  <- mw_filter(df_std, province = "British Columbia")

# 5) Plot
mw_plot_trend(df_bc, province = "British Columbia", indicator = "YOUR_INDICATOR")
mw_plot_compare(df_std, year = 2023, indicator = "YOUR_INDICATOR")

## Main functions
A) API discovery layer (find the right tables)
mw_list_tables()
list packaged “known” wait-time/access tables (curated shortlist)
mw_search_tables(query = "wait")
search WDS cube list by keyword (returns productId + title)
mw_table_metadata(product_id)
get cube metadata (dimensions, members, vectors availability)

B) Data retrieval layer (turn tables into data)
mw_get_data(product_id, geography = NULL, indicator = NULL, start_year = NULL, end_year = NULL)
returns a tidy tibble (minimum wrangling included)
mw_get_series(vector_id, start_year = NULL, end_year = NULL) (optional)
retrieve a specific series (vector) over time

C) Cleaning / standardization (make outputs consistent)
mw_standardize(df)
standardized column names (province, year, value, indicator, ...)
mw_filter(df, province = NULL, indicator = NULL, start_year = NULL, end_year = NULL)
convenience filtering

D) Plotting (required for vignette: zero → graph)
mw_plot_trend(df, province, indicator)
line plot of trends over time
mw_plot_compare(df, year, indicator)
bar chart comparing provinces (or categories)

## Notes
Data availability depends on the selected WDS table (cube) and its dimensions/members.
If you encounter issues, check mw_table_metadata() first to confirm valid values.

## License
MIT License. See `LICENSE` and `LICENSE.md` for details.


