
library("tiff")
library("png")
library("data.table")
library("stringr")


#finds the year based off of the name
.get_image_year <- function(imageName){
  splitName <- strsplit(imageName, "-")
  date <- splitName[[1]][length(splitName[[1]]) - 1]
  yearVector <- strsplit(date,"")[[1]][1:min(4, length(strsplit(date,"")[[1]]))]
  year <- paste0(yearVector[1], yearVector[2], yearVector[3], yearVector[4])
  return(year)
}


#creates the directory if not exist
dir_str <- function(imageName, pathToWorkingDir){
  year <- .get_image_year(imageName)
  if (dir.exists(paste0(pathToWorkingDir, "/", year, "/"))) {
    pwd <- paste0(pathToWorkingDir, "/", year, "/")
    return(pwd)
  }
  else {
    dir.create(paste0(pathToWorkingDir, "/", year, "/"))
    dir.create(paste0(pathToWorkingDir, "/", year, "/", "FailedToProcess/"))
    pwd <- paste0(pathToWorkingDir, "/", year, "/")
    return(pwd)
  }
}
isTiff <- function(imageName){
  splitMag <- strsplit(imageName, "-")
  lastStringInSplit <- strsplit(last(splitMag[[1]]), "")
  lastLetter <- last(lastStringInSplit[[1]])
  if (lastLetter == "f") {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
imageChecking <- function(imageName,locationToCheck){
  splitMag <- strsplit(imageName, "-")
  firstThing <- splitMag[[1]][locationToCheck]
  return(firstThing)
}




toDir <- "~/Magneto/creationShinyDir//"



# First part organizes the pngs from the conversion from .tiff on the server----

fromDir <- "~/Magneto/fromDir/"


# browser()
# for (i in list.files(fromDir)){#lists files in that dir, no subdirectorys please
#   found <- FALSE
#   pwd <- dir_str(imageName = i, pathToWorkingDir = toDir)
#   for (j in list.dirs(pwd)) {
#     if (length(list.files(path = j,
#                           pattern = str_split(i, pattern = ".p")[[1]][1])
#                != 0)){#i %in% list.files(path = list.dirs(path = pwd)[j])){
#       file.copy(from = paste0(fromDir, i), to = j)
#       found <- TRUE
#       print("found")
#       break
#     }
#   }
#   if (isFALSE(found)) { # put into main dir for people to process in the future
#     file.copy(from= paste0(fromDir, i), to = pwd)
#     print("not found")
#   }
#
# }

# Second part organizes the .tif's on the server -------------------------------

toDirtiff <- "~/Magneto/testingCreationTiffCreate/"
fromDirTiffOrigionals <-"~/Magneto/fromDirTiffs/"

#browser()
badOnes <- vector()
for (i in list.files(fromDirTiffOrigionals, recursive = TRUE)){ #recursive list of files
  found <- FALSE
  splitName <- strsplit(i, "/")
  imageName <- last(splitName[[1]])
  print(imageName)
  pwd <- dir_str(imageName = i, pathToWorkingDir = toDirtiff)

  for (j in list.dirs(pwd)) { #usually just the blank year and the failed to process inside
    if (length(list.files(path = j, pattern = imageName)) >= 1){
      if (length(list.files(path = j, pattern = paste0(imageName, ".png"))) != 0){
        file.copy(from = paste0(fromDirTiffOrigionals, i), to = j)
        found <- TRUE
      }
    }
    if (isTRUE(found)) {
      break
    }
  }
  if (isFALSE(found)){ #if no png is found for that image, it will create it
    for (j in list.dirs(pwd)) {
      if (length(list.files(path = j, pattern = imageName)) >=1){ #instead just look for the image name
        firstPartOfName <- imageChecking(imageName, 1)
        if (as.numeric(file.info(paste0(fromDirTiffOrigionals,i))[1]) != 0) {
          if (firstPartOfName == "AGC" || firstPartOfName == "TOR") {
            if(isTRUE(isTiff(i))){
              imageImport <- tryCatch(readTIFF(paste0(fromDirTiffOrigionals, i)), error = function(e) e) #catches corrupt files

              if (inherits(imageImport, "error")) {
                badOnes <- c(badOnes, paste0(i, "corrupt"))
                found <- TRUE
                break
              }

              writePNG(imageImport, target = paste0(j, "/", imageName,".png"))
              file.copy(from = paste0(fromDirTiffOrigionals, i), to = j)
              found <- TRUE

            }
          }
        }
      }
      if (isTRUE(found)) {
        break
      }
    }
    if (isFALSE(found)) {
      # not found anywhere, then no .png and no digitization yet
      firstPartOfName <- imageChecking(imageName, 1)
      if (as.numeric(file.info(paste0(fromDirTiffOrigionals,i))[1]) != 0) {
        if (firstPartOfName == "AGC" || firstPartOfName == "TOR") {
          if (isTRUE(isTiff(i))) {
            imageImport <- tryCatch(readTIFF(paste0(fromDirTiffOrigionals, i)),error = function(e) e) #catches corrupt files

            if (inherits(imageImport, "error")) {
              badOnes <- c(badOnes, paste0(i, "corrupt"))
              found <- TRUE
              next
            }

            writePNG(imageImport, target = paste0(pwd, imageName,".png"))
            file.copy(from = paste0(fromDirTiffOrigionals, i), to = pwd)
            found <- TRUE

          }
        }
      }
    }
  }
  if (isFALSE(found)) { # if still not found, then bad image name most likely
    badOnes <- c(badOnes, i)
  }
}

print(badOnes)
