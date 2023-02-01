library(readr)
library(weathermetrics)
library(purrr)
library(RCurl)
library(dplyr)
library(stringr)
library(janitor)
library(plyr)

load_instrument <- function(file_name, file_path) {
  
  instrument = read.csv(file = paste0(file_path,file_name,".txt"), sep = '\t',header = TRUE,
                        row.names=NULL, na.string = c("","NA"), check.names=FALSE)
  
  #remove details line
  instrument=instrument[-1,]
  
  #drop columns introduced by NDA, they are not required in the instruments.
  instrument = instrument[,!(names(instrument) %in% c(paste0(file_name,"_id"), "collection_id", "collection_title", "promoted_subjectkey","subjectkey" ,"study_cohort_name", "dataset_id"))]
  
  #if visit was used instead of eventname, rename
  if ("visit" %in% names(instrument) ){
    ind = which(names(instrument) == "visit")
    names(instrument)[ind] = "eventname"
    print("eventname replaced visit")
  }
  
  #remove empty columns (and print their names)
  instrument = instrument[,colSums(is.na(instrument)) != nrow(instrument)]
  
  instrument = droplevels(instrument)
  
  
  #convert to numeric
  for (i in 1:ncol(instrument)) {
    
    tryCatch({
      if(typeof(instrument[,i]) == "character"){
        instrument[,i] = as.numeric(instrument[,i])
      }else if (typeof(instrument[,i]) == "factor"){
        instrument[,i] = as.numeric(as.character(instrument[,i]))
      }
    }, error = function(e) {
      print(colnames(instrument)[i])
      print(e)
    }, warning = function(e){
      print(colnames(instrument)[i])
      print(e)
    })
    
  }
  
  
  return(instrument)
}



get_models <- function(outcome, predictor, variables, var_added) {
  if (is.null(var_added)) {
    model <- as.formula(paste0(outcome, " ~", paste0(c(
      predictor, variables
    ), collapse = " + "), sep = ""))
  } else {
    model <- as.formula(paste0(outcome, " ~", paste0(
      c(predictor, variables, var_added), collapse = " + "
    ), sep = ""))
  }
  return(jtools::get_formula(model))
}



download_normal_climate <- function(stations, mainURL) {
  
  urls <- paste0(mainURL, stations, ".csv" )
  
  
  stations_contents <- list()
  
  for (i in 1:length(urls)) {
    station_data <-
      map(urls[i], ~ tryCatch(
        read.csv(text = getURL(urls[i])),
        error = function(e)
          NULL
      )) #%>% plyr::mutate(stations = stations[i])
    # stations_contents[[length(stations_contents) + 1]] <- station_data
    stations_contents[[stations[i]]] <- station_data
  }
  # names(stations_contents) <- stations
  
  stations_merge <-
    map(stations_contents, function(x) x[[1]]) %>%
    map(., data.frame) %>%
    # remove unneeded columns
    map(., function(x) {x %>% select(-contains(c("flag", "_ATTRIBUTES")))}) %>% 
    bind_rows() %>% 
    clean_names()
  
  return(stations_merge)
}

