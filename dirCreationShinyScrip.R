
#finds the year based off of the name
function(imageName){
  splitName <- strsplit(imageName, "-")
  date <- splitName[[1]][length(splitName[[1]]) - 1]
  yearVector <- strsplit(date,"")[[1]][1:4]
  year <- paste0(yearVector[1], yearVector[2], yearVector[3], yearVector[4])
  return(year)
}


#creates the directory if not exist
function(imageName, pathToWorkingDir){
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


