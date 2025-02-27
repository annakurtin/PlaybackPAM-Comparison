############ Check for packages, install those needed, and load libraries #################

# This is a function that reads in a list of packages, checks if they are installed, installs any needed, and then reads in the libraries for all of them.

############# Input ##############
# This function takes a list of the packages required
# ex:
# packages <- c("ROCR", "tidyverse", "ks", "mapview", "colorRamps","rgeos", "VGAM", "AICcmodavg", "MuMIn", "corrgram", "GGally","caret", "DescTools", "car", "sf", "terra", "stars", "tmap")

############# Code #################

package_function = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

## Now check each package, if it needs to be installed, install it, then load it
load_packages <- function(packages)
    {for(i in packages){
      package_function(i)
    }
}

# Original:
# package.check <- lapply(
#   packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )
