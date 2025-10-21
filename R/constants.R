# ---- Repo Config ----
GITHUB_OWNER  <- Sys.getenv("CHD_GITHUB_OWNER",  unset = "County-Health-Rankings-and-Roadmaps")
GITHUB_REPO   <- Sys.getenv("CHD_GITHUB_REPO",   unset = "chrr_measure_calcs")
GITHUB_BRANCH <- Sys.getenv("CHD_GITHUB_BRANCH", unset = "main")
DATA_DIR      <- Sys.getenv("CHD_DATA_DIR",      unset = "relational_data")
GITHUB_TOKEN  <- Sys.getenv("GITHUB_TOKEN", unset = NA)
