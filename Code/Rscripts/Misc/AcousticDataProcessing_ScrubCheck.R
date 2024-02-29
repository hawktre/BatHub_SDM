##################################################
### Confirm that AHK_SonoBatScrubbingSB441_SUsNoNames_testConusGrtsClassifier.ahk script ran properly,
### and that all acoustic data was scrubbed. 
##################################################
state <- 'Oregon'
ST <- 'OR'
year <- '2023'
setwd(paste0("E:/NABat/", state, "/Raw/",year))

### First list all quad level directories
a <- list.dirs(full.names = F)

### Extract path to Station Location directories
quad.folders1 <- str_extract(a, '\\d+_[NS][EW]\\d')
quad.folders1 <- na.omit(unique(quad.folders1))

### Extract path to Noise Files directories
quad.folders2 <- str_extract(a, '\\d+_[NS][EW]\\d/Noise')
quad.folders2 <- na.omit(unique(quad.folders2))
quad.folders2 <- str_extract(quad.folders2,'\\d+_[NS][EW]\\d')

### Are there any Station Location directories without a 'Noise Files' subdirectory
quad.folders1[!quad.folders1 %in% quad.folders2]
