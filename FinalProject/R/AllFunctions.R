#pkgtest function checks to see if a package is install and if not, it installs and loads the package
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE)) {
    print(paste("installing",x))
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  } else{
    print(paste(x,"found"))
  }
}

packages <- c("aod", "dplyr", "forestmangr", "ggplot2")
lapply(packages, pkgTest)

#data <- read.csv("cbb.csv")

cleandata <- function(x){
  x <- na.omit(x) #remove all unknowns 
  x <- round_df(x, digits = 2) #round to nearest tenths
}

teamrating <- function(x, y, z){ #x=Offensive Efficiency, y=Defensive Efficieny, z=Power Rating
  w <- (x + y)/100 #theoretical team efficiency
  v <- w/z #team efficiency/power rating
  return(v)
}  

winrate <- function(x,y){ #x-games won, y=games played
  return(x/y)
}