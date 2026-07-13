# update.packages(repos = "https://cran.rstudio.com/",
#                 ask = FALSE)

# Packages are provided by mt-climate-office/actions/setup-geospatial in CI;
# extras not in that environment are installed by a workflow step.
# For a bare local R installation, install manually:
# install.packages("pak", repos = "https://mac.r-project.org")
# pak::pak(
#   c(
#     "arrow",
#     "sf?source",
#     "tidyverse",
#     "png",
#     "ragg",
#     "rmapshaper",
#     "tigris",
#     "cols4all",
#     "curl",
#     "furrr",
#     "httr2",
#     "av",
#     "archive"
#   )
# )

library(magrittr)
library(tidyverse)
library(sf)
library(furrr)

## ---- S3 archive state --------------------------------------------------
## Published media live at s3://sustainable-fsa/usdm-viz/ (served at
## https://data.sustainable-fsa.com/usdm-viz/); the frame/render cache is
## internal pipeline state under usdm-viz/_cache/ (underscore keeps it out
## of manifests and dataset discovery).
source("R/s3-archive.R")
s3_preflight()
s3_bucket_name <- Sys.getenv("S3_BUCKET", unset = "sustainable-fsa")
s3_prefix      <- Sys.getenv("S3_PREFIX", unset = "usdm-viz")

## Pull the render cache so incremental frame guards see prior work
s3_pull(s3_bucket_name, paste0(s3_prefix, "/_cache"), "data")

## Viewer page shells live in git under docs/ (also the GitHub Pages
## publishing source); rendered media are staged alongside them below.
dir.create(file.path("docs", "usdm"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("docs", "usdm-counties"), recursive = TRUE, showWarnings = FALSE)

source("R/get_oconus.R")
source("R/get_usdm_dates.R")
source("R/usdm_layout.R")

source("R/update_usdm_video.R")
usdm_png <- update_usdm_video()
list.files(file.path("data","usdm"),
           full.names = TRUE,
           recursive = FALSE,
           include.dirs = FALSE) %>%
  file.copy(to = file.path("docs","usdm"),
            overwrite = TRUE)

source("R/update_usdm_counties_video.R")
usdm_counties_png <- update_usdm_counties_video()
list.files(file.path("data","usdm-counties"),
           full.names = TRUE,
           recursive = FALSE,
           include.dirs = FALSE) %>%
  file.copy(to = file.path("docs","usdm-counties"),
            overwrite = TRUE)

source("R/update_drought_disasters.R")
drought_disasters_png <- update_drought_disasters()
# list.files(file.path("data","disasters"),
#            full.names = TRUE,
#            recursive = FALSE,
#            include.dirs = FALSE) %>%
#   file.copy(to = file.path("docs","usdm-counties"),
#             overwrite = TRUE)

source("R/update_droughtlook.R")
usdm_droughtlook <- update_droughtlook()
# list.files(file.path("data","droughtlook"),
#            full.names = TRUE,
#            recursive = FALSE,
#            include.dirs = FALSE) %>%
#   file.copy(to = file.path("docs","usdm-counties"),
#             overwrite = TRUE)

# # source("R/update_usdm_change.R")
# # update_usdm_change()

## ---- Publish to S3 -----------------------------------------------------
## Media are append-only: dated files accumulate; latest.* are overwritten
## in place. Never --delete (a fresh runner's docs/ holds only this run's
## outputs, not the full media history).
s3_push(s3_bucket_name, s3_prefix, "docs", delete = FALSE)
s3_push(s3_bucket_name, paste0(s3_prefix, "/_cache"), "data", delete = FALSE)

s3_verify(s3_bucket_name, s3_prefix, "docs",
          allow_extra = character(0),
          expect_exact = FALSE)

s3_write_manifest(s3_bucket_name, s3_prefix)

cf_invalidate(c(
  paste0("/", s3_prefix, "/usdm/index.html"),
  paste0("/", s3_prefix, "/usdm/latest.mp4"),
  paste0("/", s3_prefix, "/usdm/latest.webm"),
  paste0("/", s3_prefix, "/usdm/latest.png"),
  paste0("/", s3_prefix, "/usdm-counties/index.html"),
  paste0("/", s3_prefix, "/usdm-counties/latest.mp4"),
  paste0("/", s3_prefix, "/usdm-counties/latest.webm"),
  paste0("/", s3_prefix, "/usdm-counties/latest.png"),
  paste0("/", s3_prefix, "/disasters/latest.png"),
  paste0("/", s3_prefix, "/droughtlook/latest*"),
  paste0("/", s3_prefix, "/_manifest.txt")
))
