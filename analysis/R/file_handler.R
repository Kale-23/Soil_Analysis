import_excel_files <- function(path) {
  files <- list.files(
    path = path, # directory to search
    pattern = "*.xlsx", # regex pattern to search for (in this case all xlsx files)
    recursive = TRUE, # match to all files, not just in the current directory
    full.names = TRUE # give full path, not just from common_path
  )

  # split into older/newer format
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

  # return both sets in a list
  list(older_files, files)
}

separate_datasets <- function(file_list) {
  # separate frost/pits/other datasets for processing

  # handles pits data
  patterns <- paste(c("SnowPits", "Snow Pits", "SnowPit"), collapse = "|")
  pits_files <- file_list[str_detect(file_list, patterns)]

  # handles frost data
  patterns <- paste(c("SnowFrost", "Frost"), collapse = "|")
  frost_files <- file_list[str_detect(file_list, patterns)]

  # return both datasets in a list
  list(pits_files, frost_files)

  #TODO: make sure this works before removing this
  #old_pits_files <- older_files[str_detect(older_files, patterns)]
  #old_frost_files <- older_files[str_detect(older_files, "Frost")]
}

oldest_files_handler <- function(path) {
  oldest_files <- list.files(
    path = path, # directory to search
    pattern = "*.xlsx", # regex pattern to search for (in this case all xlsx files)
    recursive = TRUE, # match to all files, not just in the current directory
    full.names = TRUE # give full path, not just from common_path
  )
  pattern <- paste(c("field", "canopy"), collapse = "|")
  oldest_files <- oldest_files[str_detect(oldest_files, pattern)] # only keep field and canopy files
  oldest_files <- oldest_files[!str_detect(oldest_files, "~\\$")] # remove microsoft temp files
  oldest_files <- oldest_files[!str_detect(oldest_files, "repeat")] # remove repeat measurements (2017-2022)

  oldest_files
}
