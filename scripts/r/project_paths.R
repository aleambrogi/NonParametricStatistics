# Run R scripts from the repository root.
if (!dir.exists(file.path("scripts", "r")) || !dir.exists("data")) {
  stop("Run this script from the repository root.")
}
project_data_dir <- Sys.getenv(
  "NPS_PROJECT_DATA_DIR", unset = file.path(getwd(), "data", "external", "project")
)
spatial_data_dir <- Sys.getenv(
  "NPS_SPATIAL_DATA_DIR", unset = file.path(getwd(), "data", "external", "accessibility")
)
project_file <- function(relative_path) file.path(project_data_dir, relative_path)
spatial_file <- function(relative_path) file.path(spatial_data_dir, relative_path)
