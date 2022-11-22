library(readr)

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



models <- function(outcome, predictor, variables, var_added) {
  if (is.null(var_added)) {
    model <- as.formula(paste0(outcome, " ~", paste0(c(
      predictor, variables
    ), collapse = " + "), sep = ""))
  } else {
    model <- as.formula(paste0(outcome, " ~", paste0(
      c(predictor, variables, var_added), collapse = " + "
    ), sep = ""))
  }
  return(model)
}


download_normal_climate <- function(station, mainURL) {
  
  urls <- paste0(mainURL, station, ".csv" )
  
  
  station_contents <- list()
  
  for (i in 1:length(urls)) {
    list <-
      map(urls[i], ~ tryCatch(
        read.csv(text = getURL(urls[i])),
        error = function(e)
          NULL
      )) %>% plyr::mutate(station = station[i])
    station_contents[[length(station_contents) + 1]] <-
      list
  }
  names(station_contents) <- station
  
  if(str_detect(mainURL, "normals")) {
    dictionary_merge <-
      map(station_contents, function(x) x[[1]]) %>%
      map(., data.frame) %>%
      map(., function(x) x %>% 
            dplyr::select(-contains("_flag"))) %>% # not need those columns
      bind_rows() %>% 
      clean_names()
  } else {
    dictionary_merge <-
      map(station_contents, function(x) x[[1]]) %>%
      map(., data.frame) %>%
      map(., function(x) x %>% 
            # dplyr::select(-ends_with("_ATTRIBUTES"))) %>% # Those variables are not needed, and diffrent types --> cannot bind data
            dplyr::mutate(
              CDSD_ATTRIBUTES = as.integer(x$CDSD_ATTRIBUTES),
              HDSD_ATTRIBUTES = as.integer(x$HDSD_ATTRIBUTES))) %>%
      bind_rows() %>% 
      clean_names()
  }
  
  return(dictionary_merge)
}


create_ever_var <- function(data, search_term, new_col_name) {
  data <- data %>%
    mutate(!!new_col_name := apply(data[, grepl(search_term, colnames(data))], 1, function(x) {any(x == 1)*1}))
  data <- data %>%
    mutate(!!new_col_name := ifelse((is.na(get(new_col_name)) &
                                       (apply(data[, which(grepl(search_term, colnames(data)))], 1, function(x) {any(x == 0)}))), 0, get(new_col_name)))
  return(data)
}
