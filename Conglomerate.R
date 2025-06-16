# --------------------------------
# loading in libraries
# --------------------------------
invisible(
  c(
    "lubridate",
    "readxl",
    "readr",
    "dplyr",
    "tidyr",
    "purrr",
    "stringr",
    "ggplot2",
    "cowplot",
    "rlang"
  ) |>
    lapply(function(x) {
      if (suppressMessages(!require(x, character.only = TRUE))) {
        install.packages(x)
        library(x, character.only = TRUE)
      }
    })
)

# --------------------------------
# setting paths and creating directories
# --------------------------------
common_path <- "~/Desktop/Soil_Work/"
output <- paste0(common_path, "cleaned_data/")
dir.create(output, showWarnings = FALSE)

# --------------------------------
# Importing Datasets
# --------------------------------
files <- list.files(
  path = paste0(common_path, "drive_data"), # directory to search
  pattern = "*.xlsx", # regex pattern to search for (in this case all xlsx files)
  recursive = TRUE, # match to all files, not just in the current directory
  full.names = TRUE # give full path, not just from common_path
)
# different formatting of files pre 2019, they are processed seperately below
older_files <- files[str_detect(files, "2017-2018")]
files <- files[!str_detect(files, "2017-2018")]

#regex patterns to remove specific files, written this way to make it easier to add/ remove
# 2019-present format
patterns <- paste(
  c("~\\$", "DataSheets_Blank", "Combined", "Durham"),
  collapse = "|"
)
files <- files[!str_detect(files, patterns)]
rm(patterns)
# before 2019 format
patterns <- paste(
  c("Frost", "SnowPits"),
  collapse = "|"
)
older_files <- older_files[!str_detect(older_files, "~\\$|Durham")]
older_files <- older_files[str_detect(older_files, patterns)]
rm(patterns)

# separate frost/pits/other datasets for processing
patterns <- paste(c("SnowPits", "Snow Pits", "SnowPit"), collapse = "|")
pits_files <- files[str_detect(files, patterns)]
frost_files <- files[str_detect(files, "SnowFrost")]
old_pits_files <- older_files[str_detect(older_files, patterns)]
old_frost_files <- older_files[str_detect(older_files, "Frost")]
#other <- files[!files %in% union(frost_files, pits_files)]
rm(files, older_files, patterns)

# --------------------------------
# Process Pits Dataset
# --------------------------------
source(paste0(common_path, "Soil_Analysis/pits.R"))
new_pits <- create_pits_new(pits_files)
old_pits <- create_pits_old(old_pits_files)
pits_data <- full_join(new_pits, old_pits) # this may be easier than bind_rows as that requires ordered rows
pits_data <- process_pits(pits_data)
rm(
  pits_files,
  old_pits_files,
  create_pits_new,
  create_pits_old,
  process_pits,
  old_pits,
  new_pits
)

# --------------------------------
# Process Frost Dataset
# --------------------------------
source(paste0(common_path, "Soil_Analysis/frost.R"))
new_frost <- create_frost_new(frost_files)
old_frost <- create_frost_old(old_frost_files)
frost_data <- full_join(new_frost, old_frost)
frost_data <- process_frost(frost_data)
rm(
  frost_files,
  old_frost_files,
  create_frost_new,
  create_frost_old,
  process_frost,
  old_frost,
  new_frost
)

# --------------------------------
# Combine + Analyze Full Dataset
# --------------------------------
source(paste0(common_path, "Soil_Analysis/analysis_helper.R"))
pits_explore <- full_explore(pits_data)
ggsave(
  filename = paste0(output, "pits_exploratory.png"),
  plot = pits_explore,
  width = 20,
  height = 20
)
rm(pits_explore)

frost_explore <- full_explore(frost_data)
ggsave(
  filename = paste0(output, "frost_exploratory.png"),
  plot = frost_explore,
  width = 20,
  height = 20
)
rm(frost_explore)
