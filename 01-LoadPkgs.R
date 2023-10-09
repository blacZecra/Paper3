cat("Loading packages...\n")

# Install required packages from CRAN
for (i in c(tidyverse_pkgs,graphic_pkgs,stats_pkgs,datawrangling_pkgs)) {
  
  new.packages <- i[!(i %in% installed.packages()[,"Package"])]
  if (length(new.packages)) install.packages(new.packages)
}

# Install required packages from CRAN archive
for (i in archive_pkgs) {
  
  new.packages <- i[!(i %in% installed.packages()[,"Package"])]
  new.versions <- archive_pkgs_version[!(i %in% installed.packages()[,"Package"])]
  for (i in seq_along(new.packages)) {
    devtools::install_version(new.packages[i], new.versions[i], upgrade = "never")
  }
}

# Install required packages from github (multifunc)
multifunc_new <- multifunc_pkg[!(multifunc_pkg %in% installed.packages()[,"Package"])]
if (length(multifunc_new)) devtools::install_github("jebyrnes/multifunc", upgrade = "never") # Install required package from github


# Load necessary packages
for (i in c(tidyverse_pkgs,graphic_pkgs,stats_pkgs,datawrangling_pkgs,multifunc_pkg)) {
  shh <- lapply(i, library, character.only = TRUE)
}


# Purge obsolete variables
rm(new.packages, tidyverse_pkgs,graphic_pkgs,stats_pkgs,datawrangling_pkgs,multifunc_pkg,multifunc_new,i,shh)


cat("Packages loaded!\n")


