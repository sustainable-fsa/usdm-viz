# update.packages(repos = "https://cran.rstudio.com/",
#                 ask = FALSE)

install.packages("pak",
                 repos = "https://cran.rstudio.com/")

# installed.packages() |>
#   rownames() |>
#   pak::pkg_install(upgrade = TRUE,
#                    ask = FALSE)

pak::pak(
  c(
    "arrow?source",
    "sf?source",
    "tidyverse",
    "png",
    "ragg",
    "rmapshaper",
    "tigris",
    "cols4all",
    "curl",
    "furrr",
    "httr2",
    "av",
    "archive"
  )
)

library(magrittr)
library(tidyverse)
library(sf)
library(furrr)

source("R/get_oconus.R")
source("R/get_usdm_dates.R")
source("R/usdm_layout.R")

source("R/update_usdm_video.R")
usdm_png <- update_usdm_video()

source("R/update_usdm_counties_video.R")
usdm_counties_png <- update_usdm_counties_video()

source("R/update_drought_disasters.R")
drought_disasters_png <- update_drought_disasters()

source("R/update_droughtlook.R")
usdm_droughtlook <- update_droughtlook()

# # source("R/update_usdm_change.R")
# # update_usdm_change()
