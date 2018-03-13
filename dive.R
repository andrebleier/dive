dive <- function(x, eval = "cat") {
  
  # isolate formula
  FUN <- gsub("(.*)\\(.*", "\\1", x)
  
  # get function arguments
  x.formal <- tryCatch({formals(FUN)}, error = function(e)
                        return(paste0("Cannot find function.")))
  
  # exit with ill-defined function
  if (!is.list(x.formal)) stop(x.formal)
  
  # trim stuff
  x <- gsub(" |\\n", "", x)
  
  x.args <- strsplit(x, ",")[[1]]
  
  # TODO implement purrr::map
  if (grepl("apply", FUN)) {
    
    if (grepl("FUN", x)) {
      FUN <- gsub(".*,FUN=([[:alnum:]\\.\\_]*)(,|\\)).*", "\\1", x)
    } else {
      FUN <- gsub(".*,([[:alnum:]\\.\\_]*)(,|\\)).*", "\\1", x) # FINISH THIS: NO FUN =
    }
    
    # get formals
    x.formal <- tryCatch({formals(FUN)}, error = function(e)
                        return(paste0("Cannot find function.")))
    
    # get rid of apply stuff
    x.args <- x.args[-grep(paste0("apply|", FUN), x.args)]
    
  } else {
    
    # extract function from first argument
    x.args[1] <- gsub(paste0(FUN,"\\("), "", x.args[1])
    
  }
    # get rid of ) at the end of the function call
    x.args[length(x.args)] <- gsub("\\)$", "", x.args[length(x.args)])
    
    # CASE 1: No '=' specification
    if (!any(grepl("=", x.args))) {
      x.argl <- as.list(x.args)
      names(x.argl) <- names(x.formal)[2:(length(x.argl)+1)]
    # CASE 3: mixing '=' specification correct CASE 1 (unlikely)
      if (any(grepl("=", x.argl))) {
        where <- grep("=", x.argl)
        names(x.argl)[where] <- gsub("(.*)=.*", "\\1", x.argl[where])
        x.argl[where] <- gsub(".*=(.*)", "\\1", x.argl[where])
      }
    # CASE 2: '=' specification
    } else {
      x.argl <- as.list(gsub(".*=(.*)", "\\1", x.args))
      names(x.argl) <- gsub("(.*)=.*", "\\1", x.args)
    }
    
    # add missing information
    mis <- which(!names(x.formal) %in% names(x.argl))
    if(length(mis)>0) {
    x.argl <- append(x.argl, x.formal[mis])
    }
    
    # reorder
    x.argl <- x.argl[names(x.formal)]
    
    # expand to output
    OUT <- paste(names(x.argl), x.argl, sep = " = ")
    return(cat(paste(OUT, collapse = "\n")))
    
 }

# SWITCH FÜR "CAT" "LIST" "ENV"


# testing functions
foo <- function(x, y = 1, z) {
  OUT <- z+y+x
  return(OUT)
}

x <- "foo(x = 2, y = 3, z = 2)"
x <- "foo(x = 2,
         y = 3)"
x <- "lapply(1:300, FUN = foo, z = 4, y = 3)"
x <- "lapply(1:300, FUN = foo, 4, 3)"
