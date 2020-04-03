TworzenieFolderow <- function(id){
  path <- getwd()
  
  pathFolder <- paste0(path,'/Zajecia',id)
  if(!file.exists(pathFolder)){
    dir.create(pathFolder)
    for(folder in c('data','report','scripts')){
      dir.create(paste0(pathFolder,'/',folder))
    }
  }
  
}
TworzenieFolderow(3)
