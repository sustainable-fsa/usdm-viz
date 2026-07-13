# update.packages(repos = "https://cran.rstudio.com/",
#                 ask = FALSE)

# Packages are provided by mt-climate-office/actions/setup-geospatial in CI;
# extras beyond that environment are passed via its extra-r-packages input.
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

source("R/get_oconus.R")
source("R/get_usdm_dates.R")
source("R/usdm_layout.R")
source("R/update_usdm_video.R")
source("R/update_usdm_counties_video.R")
source("R/update_drought_disasters.R")
source("R/update_droughtlook.R")

## ---- Assess needed work from S3 state (before any pull) ----------------
## Each gate below reads only S3 listings/HEADs and small upstream
## endpoints, so a week with nothing new skips the 2.3 GiB cache pull, the
## renders, and the S3 sync entirely.

usdm_dates <- get_usdm_dates()

## A video needs a rebuild iff an expected frame is absent from the render
## cache AND its upstream parquet is posted, or a video artifact is missing
## outright. The in-function guards remain the local authority once the
## cache is pulled; this only decides whether to pull at all.
cache_video_work <- function(subprefix, upstream_url) {
  cache_prefix <- paste0(s3_prefix, "/_cache/", subprefix)
  keys <- s3_list_keys(s3_bucket_name, cache_prefix)$Key
  cached_frames <-
    keys %>%
    stringr::str_subset("/png/") %>%
    basename() %>%
    tools::file_path_sans_ext()
  missing <- usdm_dates[!(format(usdm_dates, "%Y-%m-%d") %in% cached_frames)]
  renderable <- missing[purrr::map_lgl(upstream_url(missing), url_exists)]
  videos_present <-
    all(file.path(cache_prefix,
                  c("latest.mp4", "latest.webm", "latest.png")) %in% keys)
  length(renderable) > 0 || !videos_present
}

videos_work <-
  cache_video_work(
    "usdm",
    \(d) paste0("https://data.sustainable-fsa.com/usdm/data/parquet/USDM_",
                d, ".parquet")) ||
  cache_video_work(
    "usdm-counties",
    \(d) paste0("https://data.sustainable-fsa.com/usdm-counties/data/usdm/USDM_",
                d, ".parquet"))

## The disasters map is current if it was published after the newest
## secretarial designation file upstream.
latest_secretarial <-
  jsonlite::fromJSON(
    "https://data.sustainable-fsa.com/fsa-disasters/manifest.json"
  ) %>%
  dplyr::filter(stringr::str_detect(path, "_SEC_")) %$%
  mtime %>%
  max() %>%
  lubridate::as_datetime()
disasters_published <-
  s3_object_mtime(s3_bucket_name, paste0(s3_prefix, "/disasters/latest.png"))
disasters_work <-
  is.na(disasters_published) || disasters_published < latest_secretarial

## Droughtlook is current if every outlook posted by CPC is already in the
## archive and the latest.* images exist.
droughtlook_remote <-
  s3_list_keys(s3_bucket_name, paste0(s3_prefix, "/droughtlook"))$Key
droughtlook_work <-
  length(setdiff(droughtlook_index(),
                 basename(stringr::str_subset(droughtlook_remote, "/raw/")))) > 0 ||
  !all(file.path(s3_prefix, "droughtlook",
                 c("latest_monthly.png", "latest_seasonal.png")) %in%
         droughtlook_remote)

## ---- USDM videos --------------------------------------------------------
if (videos_work) {
  ## Pull the render cache so incremental frame guards see prior work
  s3_pull(s3_bucket_name, paste0(s3_prefix, "/_cache"), "data")

  ## Viewer page shells live in git under docs/ (also the GitHub Pages
  ## publishing source); rendered media are staged alongside them below.
  dir.create(file.path("docs", "usdm"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("docs", "usdm-counties"), recursive = TRUE, showWarnings = FALSE)

  usdm_png <- update_usdm_video()
  list.files(file.path("data","usdm"),
             full.names = TRUE,
             recursive = FALSE,
             include.dirs = FALSE) %>%
    file.copy(to = file.path("docs","usdm"),
              overwrite = TRUE)

  usdm_counties_png <- update_usdm_counties_video()
  list.files(file.path("data","usdm-counties"),
             full.names = TRUE,
             recursive = FALSE,
             include.dirs = FALSE) %>%
    file.copy(to = file.path("docs","usdm-counties"),
              overwrite = TRUE)
} else {
  gate_skip("USDM frames and videos are current; skipping cache pull and video renders.")
}

## ---- FSA drought disasters ----------------------------------------------
if (disasters_work) {
  drought_disasters_png <- update_drought_disasters()
} else {
  gate_skip("FSA drought disasters map is current; skipping render.")
}

## ---- CPC drought outlooks -----------------------------------------------
if (droughtlook_work) {
  ## Restore the outlook archive first so the per-file guards only fetch
  ## and render NEW outlooks (docs/ was purged from git 2026-07; this cache
  ## now round-trips through the published prefix).
  s3_pull(s3_bucket_name, paste0(s3_prefix, "/droughtlook"),
          file.path("docs", "droughtlook"))
  usdm_droughtlook <- update_droughtlook()
} else {
  gate_skip("CPC drought outlooks are current; skipping downloads and renders.")
}

# # source("R/update_usdm_change.R")
# # update_usdm_change()

## ---- Publish to S3 -----------------------------------------------------
## Media are append-only: dated files accumulate; latest.* are overwritten
## in place. Never --delete (a fresh runner's docs/ holds only this run's
## outputs, not the full media history).
if (videos_work || disasters_work || droughtlook_work) {
  s3_push(s3_bucket_name, s3_prefix, "docs", delete = FALSE)
  if (videos_work)
    s3_push(s3_bucket_name, paste0(s3_prefix, "/_cache"), "data", delete = FALSE)

  s3_verify(s3_bucket_name, s3_prefix, "docs",
            allow_extra = character(0),
            expect_exact = FALSE)

  s3_write_manifest(s3_bucket_name, s3_prefix)

  cf_invalidate(c(
    if (videos_work)
      paste0("/", s3_prefix,
             c("/usdm/index.html", "/usdm/latest.mp4",
               "/usdm/latest.webm", "/usdm/latest.png",
               "/usdm-counties/index.html", "/usdm-counties/latest.mp4",
               "/usdm-counties/latest.webm", "/usdm-counties/latest.png")),
    if (disasters_work)
      paste0("/", s3_prefix, "/disasters/latest.png"),
    if (droughtlook_work)
      paste0("/", s3_prefix, "/droughtlook/latest*"),
    paste0("/", s3_prefix, "/_manifest.txt")
  ))
} else {
  gate_skip("All usdm-viz artifacts are current; nothing to sync to S3.")
}
