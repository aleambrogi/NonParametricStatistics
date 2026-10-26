# Supplementary R analysis

These scripts preserve the joint coursework's data preparation, healthcare
accessibility indices, maps and functional analysis. They are supplementary to
the self-contained population notebooks; several source datasets are external.

## Run from the repository root

Each script loads `scripts/r/project_paths.R`. Optionally set:

```r
Sys.setenv(NPS_PROJECT_DATA_DIR = "/path/to/original/project/data")
Sys.setenv(NPS_SPATIAL_DATA_DIR = "/path/to/original/accessibility/data")
source("scripts/r/mappa_2021vs2040.R")
```

The default roots are `data/external/project` and `data/external/accessibility`.
The helper does not manufacture missing inputs. For the original input names and
subdirectories, inspect the corresponding script. Run the preparation steps only
after obtaining the required source data.

## Script groups

| Scripts | Purpose | Additional inputs |
| --- | --- | --- |
| `storico*.R`, `mergeoveryears.R` | Assemble historical population extracts | ISTAT population extracts and geographic lookup/boundary files |
| `index_computation*.R` | Compute accessibility and hospital-load indicators | Travel/accessibility table, hospital inputs, population and boundaries |
| `mappa_2021vs2040.R`, `mappe_popolation.R` | Map observed/projected indicators | Computed indicator tables, reconciled predictions and boundaries |
| `functional_creation*.R`, `spatial_function.R` | Functional summaries and spatial smoothing | Generated indicator tables and RDS objects |
| `index_dunntest.R` | Compare indicator distributions | Generated indicator tables and boundaries |

Libraries used across the scripts include `sf`, `dplyr`, `readr`, `tidyr`,
`purrr`, `stringr`, `data.table`, `ggplot2`, `viridis`, `classInt`, `FSA`,
`ggsignif`, `gganimate`, `scales`, `mgcv`, `plotly` and `rgl`.
Install only the packages used by the script you intend to run. `sf` may need
system geospatial libraries depending on the operating system.

Full R execution is not validated by the Python smoke checks. External files
retain their original names, and generated R files stay under the configured
project-data directory. Do not point that directory at irreplaceable source data
when rerunning scripts that write outputs.
