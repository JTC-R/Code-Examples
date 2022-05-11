### DESCRIPTION: 
###   In creating reports, a centralized pass for all required data helps with the flow of information, error handling, and ensuring
###   unified methods of data extraction. There are several streams of information that are tied together into one PDF report, 
###   there are: 
###     - Data relating to VoEx
###     - Data relating to SNAP Graphs
###     - Data relating to Delta, Gamma, and Vanna values, as calculated
###     - Shorting information
###     - Expected Price Range Table data
###     - Other Nominal Data
###
###     The process works by first looking to see if the appropriate data is on disk, and if not, a event ticket is created and stored.
###     Then, if the data was not there, the appropriate process caller is called in order for the data to be retireved, processed, and saved.
###     Once the data is present, the data is retrieved and returned to the caller function. 
###     If still after the data-processing callers have been invoked, the data is either not present on disk or corrupted, a fetal error is thrown
###     for upstream handling. 

#########################
## MAIN FUNCTION CALLER
#########################
get_report_data <- function(requested.stock, chosen.date, which.data){
  dds.format.chosen_date(chosen.date) -> chosen.date                          # This ensure that all dates entering this pipeline are formatted appropriately
  dds.format.requested_stock(requested.stock) -> requested.stock              # This ensures that all stock names entering this pipeline are formatted.
  get_report_data.translate(which.data) -> which.data                         # Introducing standardization of the data-type required
  get_report_data.create_path(requested.stock, which.data) -> file.path       # Creates the path where the data should be located
  if(dds.null_check(file.path)){                                              # If the data is not present:
    get_report_data.missing_data(requested.stock, chosen.date, which.data)       # Invokes the missing data portion of the function
    get_report_data.create_path(requested.stock, which.data) -> file.path        # Creates the new path for the new data
  }                                               
  get_report_data.import(file.path) -> data.report                            # The file located on the file.path is now retrieved
  if(!get_report_data.chosen_date(chosen.date, data.report)){                 # If the date requested is not in the file:
    get_report_data.missing_data(requested.stock, chosen.date, which.data)      # Invoke error logging and invoke process caller
    get_report_data.import(file.path) -> data.report                            # Re-imports the new data
    if(!get_report_data.chosen_date(chosen.date, data.report)){               # If the dat is still not present:
      get_report_data.failure(requested.stock, chosen.date, which.data)         # Invoke critical failure
    }
  }
  return(get_report_data.return(chosen.date, data.report))                    # Return the requested data
}
##
get_report_data.translate <- function(which.data){
  if(which.data == 'vex'){
    return('vex_values')
  } else {
    return(paste0(which.data, '_table'))
  }
}
get_report_data.create_path <- function(requested.stock, which.data){
  get_report_data.create_path.create_dir_path(which.data) -> dir.path
  get_report_data.create_path.create_file_name(requested.stock, dir.path) -> file.name
  if(length(file.name) == 0){                                                           
    return(NULL)                                                                    
  } else {
    return(paste0(dir.path, file.name))         
  }
}
get_report_data.create_path.create_dir_path <- function(which.data){
  return(paste0(report.data@location, which.data, '\\'))
}
get_report_data.create_path.create_file_name <- function(requested.stock, dir.path){
  list.files(dir.path) -> file.list
  return(file.list[which(stringr::str_detect(file.list, paste0("^", requested.stock, "(?=_)")))])
}
#
get_report_data.exists <- function(file.path){
  if(file.exists(file.path)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
get_report_data.import <- function(file.path){
  return(read.csv(file.path, stringsAsFactors = FALSE))
}
get_report_data.missing_data <- function(requested.stock, chosen.date, which.data){
  if(which.data != 'short_table'){
    get_report_data.missing_data.log(requested.stock, chosen.date, which.data) 
  }
  get_report_data.missing_data.call_processor(requested.stock, chosen.date, which.data)
}
get_report_data.missing_data.log <- function(requested.stock, chosen.date, which.data){
  log_event.new(type = 'event', requested.stock = requested.stock, chosen.date = chosen.date,
                condition.message = paste0('missing data for ', which.data), error.code = 3, 
                caller.function = 'get_report_data.missing_data.log', 
                process.numeric = 2, line.number = 54)
}
get_report_data.missing_data.call_processor <- function(requested.stock, chosen.date, which.data){
  get_report_data.missing_data.call_processor.caller(requested.stock, chosen.date, which.data)
}
get_report_data.missing_data.call_processor.caller <- function(requested.stock, chosen.date, which.data){
  if(which.data == 'option_direction_table'){
      directionalize_options(requested.stock, chosen.date)
  } else if (which.data == 'hedging_table' | which.data == 'vex_values'){
      hedging_matrix.process(requested.stock, chosen.date)
  } else {
      get_report_data.missing_data.call_processor.caller.name_args(requested.stock, chosen.date, which.data) -> arg.list
      do.call(what = paste0(which.data, '.process'), args = arg.list, envir = .GlobalEnv)
  }
}
get_report_data.missing_data.call_processor.caller.name_args <- function(requested.stock, chosen.date, which.data){
  if(which.data %in% c('delta_table', 'epr_table', 'nominal_table','voex_table')){
    list(NULL, requested.stock, chosen.date) -> arg.list
    c('data.stock', 'requested.stock', 'chosen.date') -> names(arg.list)
  } else {
    list(requested.stock, chosen.date) -> arg.list
    c('requested.stock', 'chosen.date') -> names(arg.list) 
  }
  return(arg.list)
}
#
get_report_data.chosen_date <- function(chosen.date, data.report){
  if(chosen.date %in% lubridate::ymd(data.report[, 'data.date'])){
    return(TRUE) 
  } else {
    return (FALSE)
  }
}
get_report_data.return <- function(chosen.date, data.report){
  return(data.report[which(lubridate::ymd(data.report[, 'data.date']) == chosen.date), ])
}
get_report_data.failure <- function(requested.stock, chosen.date, which.data){
  get_report_data.failure.log(requested.stock, chosen.date, which.data)
  stop('unable to retrieve or process data')
}
get_report_data.failure.log <- function(requested.stock, chosen.date, which.data){
  log_event.new(type = 'error', requested.stock = requested.stock, chosen.date = chosen.date,
                condition.message = paste0('irrecoverable error producing ', which.data),
                error.code = 1,
                caller.function = 'get_report_data.failure.log',
                process.numeric = 2, line.number = 103)
}






