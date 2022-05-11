### DESCRIPTION:
###   In producing 600+ PDF reports automatically every day utilizing parallel processing, occasionally there are
###   Corrupted PDF files that are created. Additionally, if a PDF file generation is cut short, then the PDF
###   may not be present but the component TEX files may be. 
### 
###   In this case, the following script reads the PDF file, verifies that it is the PDF file that is listed on the
###   file name, and if it is, proceeds to the next PDF. 
###
###   If it is not the ascribed PDF file, then the file name is logged as a missing report, and the file is remvoed.
###   The missing reports are then re-generated.
###
###   If the file is not a PDF but rather a component file from the PDF creation process, the files are removed.

##########################
## MAIN FUNCTION CALLER
##########################
verify_pdf <- function(chosen.date){
  dds.format.chosen_date(chosen.date) -> chose.date                                   # Ensuring all dates entering the pipeline are formatted.
  verify_pdf.find(chosen.date) -> unmatching.reports                                  # The main function caller that finds the corrupted PDFs
  if(!dds.null_check(unmatching.reports)){                                            # If there are corrupted files: log
    log_event.new(type = 'event', requested.stock = NA, chosen.date = chosen.date,
                  condition.message = paste0((nrow(unmatching.reports) - 1) , ' unmatching reports'), 
                  caller.function = 'verify_pdf',
                  error.code = 4,
                  process.numeric = 4, line.number = 29)
    verify_pdf.reproduce(chosen.date, unmatching.reports)                              # Ensure that the corrupted files are re-produced
  } else {
    log_event.new(type = 'status', requested.stock = NA, chosen.date = chosen.date,    # If no corrupted files: log
                  condition.message = 'no mismatched reports', error.code = 6,
                  caller.function = 'verify_pdf',
                  process.numeric = 4, line.number = 29)
  }
}

### VERIFY_PDF.FIND
#
#
verify_pdf.find <- function(chosen.date){
  dds.format.chosen_date(chosen.date) -> chosen.date
  verify_pdf.find.create_path(chosen.date) -> dir.path
  verify_pdf.find.get_file_list(chosen.date, dir.path) -> file.list
  if(dds.null_check(file.list)){
    log_event.new(type = 'error', requested.stock = NA, chosen.date = chosen.date,
                  condition.message = 'no reports found', error.code = 6,
                  process.numeric = 4, line.number = 29)    
    stop('no reports found for verification')
  } else {
    verify_pdf.find.perform(chosen.date, dir.path, file.list) -> unmatching.reports
    if(nrow(unmatching.reports) > 1){
      return(unmatching.reports) 
    } else {
      return(NULL)
    }
  }
}
#
verify_pdf.find.create_path <- function(chosen.date){
  return(paste0(db.reports@location, stringr::str_remove_all(chosen.date, '-'), '_reports\\'))
}
# verify_pdf.find.GET_FILE_LIST
#
verify_pdf.find.get_file_list <- function(chosen.date, dir.path){
  list.files(paste0(dir.path)) -> file.list
  if(length(file.list) == 0){
    log_event.new(type = 'error', requested.stck = NA, chosen.date = chosen.date,
                  condition.message = 'no reports found for verification', error.code = 2,
                  caller.function = 'verify_pdf.find.get_file_list',
                  process.numeric = 1, line.number = 24)
    return(NULL)
    
  } else {
    return(file.list)
  }
}
## verify_pdf.find.PERFORM
# This is the actual PDF checking mechanism. It works by reading the PDF and looking for the 
# title listed on the PDF page itself. If the PDF page does not display the same STOCK symbol
# as is in the file's name, it is considered a corrupted PDF.
#
verify_pdf.find.perform <- function(chosen.date, dir.path, file.list){
  verify_pdf.find.perform.create_output() -> unmatching.reports
  for(pdf in file.list){
    if(!verify_pdf.find.perform.is_pdf(dir.path, pdf)){
      next
    }
    message(paste0('Checking ', pdf))
    verify_pdf.find.perform.get_requested_name(pdf) -> requested.name
    verify_pdf.find.perform.get_actual_name(chosen.date, dir.path, pdf) -> actual.name
    if(dds.null_check(actual.name)){                                                            # Occasionally the PDF is corrupted and cannot be opened.
      message(paste0('Mismatch found: ', requested.name, ' is replaced by ', actual.name))    
      verify_pdf.find.perform.log(chosen.date, requested.name)
      verify_pdf.find.perform.add(unmatching.reports, requested.name) -> unmatching.reports
      verify_pdf.find.perform.remove(dir.path, pdf)
    } else if(requested.name != actual.name){                                                   # The names do not match
      message(paste0('Mismatch found: ', requested.name, ' is replaced by ', actual.name))    
      verify_pdf.find.perform.log(chosen.date, requested.name)
      verify_pdf.find.perform.add(unmatching.reports, requested.name) -> unmatching.reports
    }
  }
  return(unmatching.reports)
}
verify_pdf.find.perform.create_output <- function(){
  return(data.frame('missing.stock' = NA))
}
## verify_pdf.find.PERFORM.NOT_PDF
#
#
verify_pdf.find.perform.is_pdf <- function(dir.path, pdf){
  if(verify_pdf.find.perform.is_pdf.check(pdf)){
    return(TRUE)
  } else {
    message(paste0('removing non-pdf ', pdf))
    verify_pdf.find.perform.remove(dir.path, pdf)
    return(FALSE)
  }
}
verify_pdf.find.perform.is_pdf.check <- function(pdf){
  if(stringr::str_detect(pdf, 'pdf')){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# verify_pdf.find.perform.get_requested.name
#
verify_pdf.find.perform.get_requested_name <- function(pdf){
  return(stringr::str_extract(pdf, '[:alpha:]+(?=_)'))
}
# verify_pdf.find.PERFORM.GET_ACTUAL_NAME
#
verify_pdf.find.perform.get_actual_name <- function(chosen.date, dir.path, pdf){
  tryCatch(
    expr = {
      pdftools::pdf_text(paste0(dir.path, pdf)) -> pdf.text
      pdf.text[[1]][1] %>% 
        stringr::str_replace_all(., ' ', '') %>% 
        stringr::str_extract(., '(?<=\\$).+(?=on)') -> actual.name
      return(actual.name)
    }, error = function(e){
      log_event.new(type = 'error', requested.stock = 'NA', chosen.date = chosen.date,
                    condition.message = conditionMessage(e), error.code = 5,
                    caller.function = 'verify_pdf.find.perform.get_actual_name', 
                    process.numeric = 2, line = 133)
      return(NULL)              
    })
}
# verify_pdf.find.PERFORM.LOG
#
verify_pdf.find.perform.log <- function(chosen.date, which.name){
  log_event.new(type = 'event', requested.stock = which.name, chosen.date = chosen.date,
                condition.message = 'corrupted report', error.code = 5,
                caller.function = 'verify_pdf.find.perform.log', 
                process.numeric = 4, line = 80)
}
# verify_pdf.find.PERFORM.ADD
#
verify_pdf.find.perform.add <- function(unmatching.reports, which.name){
  which.name -> unmatching.reports[(nrow(unmatching.reports) + 1), 'missing.stock']
  return(unmatching.reports)  
}
## verify_pdf.find.PERFORM.REMOVE
#
#
verify_pdf.find.perform.remove <- function(dir.path, which.file){
  file.remove(paste0(dir.path, which.file))
  message(paste0('Removed ', which.file))
}
## VERIFY_PDF.REPRODUCE
#
#
verify_pdf.reproduce <- function(chosen.date, unmatching.reports){
  for(requested.stock in unmatching.reports[, 'missing.stock']){
    if(is.na(requested.stock)){
      next
    }
    log_event.new(type = 'event', requested.stock = requested.stock, chosen.date = chosen.date,
                  condition.message = 'recreating report', error.code = 6,
                  caller.function = 'verify_pdf.reprouce',
                  process.numeric = 4, line.number = 16)
    report_producer(requested.stock = requested.stock, chosen.date = chosen.date)
  }
}



