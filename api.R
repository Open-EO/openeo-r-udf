library(stars)
library(abind)
library(lubridate)
library(xts)

api_version = "0.1.0"
r_udf_version = "0.3.1"

DEBUG = FALSE

#TODO define float maximum digits

source("data_transformation.R")

.measure_time = function(fun,message,envir=parent.frame()) {
  if (!DEBUG) return(eval(fun,envir = envir))
  tryCatch({
    start = Sys.time()
    return(eval(fun,envir=envir))
  },error=function(e){
    stop(e$message)
  },finally = {
    cat(message)
    cat("\n")
    print(Sys.time()-start)
  })
}

.read_data_requirement = function(code) {
  require_annotation = "@require"
  require_regex = paste0("[#]+\\s*",require_annotation,"\\s*(\\w+):(\\w+)\\s*\\n")
  selection = unlist(regmatches(code,regexec(require_regex,code,perl=TRUE)))

  if (length(selection) > 0) {
    return(list(
      variable_name = selection[2],
      target_class = selection[3]
    ))
  } else {
    return(list())
  }
}

#* Interprete JSON, divide code and data and assign classes
#* @filter check-data
check_data = function(req, res) {
  if (req$PATH_INFO != "/udf") return(plumber::forward())
  
  if (DEBUG) {
    cat("=== Started executing at endpoint /udf ===\n")
  }
  if (length(req$postBody) > 0) {
    if (DEBUG) {
      cat("Upload data:\n")
      print(Sys.time()-as_datetime(as.numeric(req$HEADERS["date"]),tz=Sys.timezone()))
    }

    if (grepl(x = req$postBody,pattern = "structured_data_list")) {
      json_in = .measure_time(quote(jsonlite::fromJSON(req$postBody,simplifyVector=FALSE)),"Read json. Runtime:")
    } else {
      json_in = .measure_time(quote(jsonlite::fromJSON(req$postBody)),"Read json. Runtime:")
    }
    
    

    req$postBody = NULL
    if (is.null(json_in$code$language) || !tolower(json_in$code$language)=="r") {
      res$status = 422
      return(list(error = "Cannot interprete code source, due to missing programming language."))
    }

    req$code = json_in$code
    req$data = json_in$data # data is the UDF data model
    
    # split user_context and server_context and append also to req
    req$user_context = if (length(json_in$data$user_context) == 0) list() else json_in$data$user_context
    json_in$data$user_context = NULL
    
    req$server_context = if (length(json_in$data$server_context) == 0) list() else json_in$data$server_context
    json_in$data$server_context = NULL
    
    if (length(req$data$structured_data_list) > 0) {
      class(req$data) = "StructuredData"
    } else if (length(req$data$data_collection$object_collections$data_cubes) > 0) {
      class(req$data) = "DataCube"
    } else {
      res$status = 422
      return(list(error = "Data other than 'StructuredData' and 'DataCube' as a special case of a DataCollection are not supported right now."))
    }
  }
  plumber::forward()
}

#* Interprete JSON, divide code and data and assign classes
#* @filter check-data-legacy
check_data_legacy = function(req, res) {
  if (req$PATH_INFO != "/udf_legacy") return(plumber::forward())
  # TODO check the endpoint called /udf -> ok, /udf_legacy -> another filter should be called
  
  if (DEBUG) {
    cat("=== Started executing at endpoint /udf ===\n")
  }
  if (length(req$postBody) > 0) {
    if (DEBUG) {
      cat("Upload data:\n")
      print(Sys.time()-as_datetime(as.numeric(req$HEADERS["date"]),tz=Sys.timezone()))
    }
    
    json_in = .measure_time(quote(jsonlite::fromJSON(req$postBody)),"Read json. Runtime:")
    
    req$postBody = NULL
    if (is.null(json_in$code$language) || !tolower(json_in$code$language)=="r") {
      res$status = 400 #maybe 422
      return(list(error = "Cannot interprete code source, due to missing programming language."))
    }
    
    req$code = json_in$code
    req$data = json_in$data
    
    if (length(req$data$raster_collection_tiles) > 0) {
      class(req$data) = "RasterCollectionTile"
    } else if (length(req$data$hypercubes) > 0) {
      class(req$data) = "HyperCube"
    } else if (length(req$data$structured_data) > 0) {
      class(req$data) = "StructuredDataLegacy"
    } else {
      res$status = 400
      return(list(error = "Data other than RasterCollectionTile, Hypercube and StructuredData are not supported yet."))
    }
  }
  plumber::forward()
}

#* @apiTitle R UDF API
#*
#* Takes a UDFRequest containing data and code and runs the code on the data
#*
#* @post /udf_legacy
post_udf_legacy.json = function(req,res, debug=FALSE) {
  if (!is.null(debug) && isTRUE(debug)) {
    DEBUG = debug
  }
  
  # prepare the executable code
  fun = .prepare_udf_function(code = req$code$source)
  
  # if data requirements states something else than stars we need to convert it
  data_requirement = .read_data_requirement(req$code$source)
  
  if (length(data_requirement) > 0) {
    if (length(data_requirement$variable_name) > 0) {
      #replace variable name in fun
      names(formals(fun)) = data_requirement$variable_name
      # TODO when we use the context this needs to be accounted for!
    }
  }
  
  # transform data into stars or simple data
  data_in = .translate_input_data_legacy(data = req$data, data_requirement)
  
  
  # run the UDF
  results = .measure_time(quote(lapply(data_in, fun)),"Executed script. Runtime:")
  
  
  # map to stars or keep simple data types
  results = lapply(1:length(results), function(index) {
    if (any(class(results[[index]]) %in% "stars")) {
      return(results[[index]])
    } else if (any(class(results[[index]]) %in% "xts")) { # TODO check later only xts to go in here (actual xts of results)
      return(st_as_stars(results[[index]]))
    } else if (any(class(results[[index]]) %in%
                   c("list","numeric","integer","character","factor","logical","matrix","data.frame"))) {
      return(results[[index]])
    } else {
      stop("UDF data return is not a simple type, xts or stars.")
    }
  })
  
  # transform stars into HyperCube, simple data types into StructuredData
  if (any("stars" %in% class(results[[1]]))) {
    json_out = .measure_time(quote(lapply(results,function(obj) as(obj,"HyperCube"))),"Translated from stars to Hypercube Runtime:")
  } else {
    json_out = .measure_time(quote(lapply(results,function(obj) as(obj,"StructuredDataLegacy"))),"Translated from simple data to StructuredData. Runtime:")
  }
  
  rm(results)
  
  # Merge multiple data chunks
  if (length(json_out) == 1) {
    json_out = json_out[[1]]
  } else {
    shell = json_out[[1]]
    shell$hypercubes = lapply(unname(json_out),function(obj) obj$hypercubes[[1]])
    shell$structured_data = lapply(unname(json_out),function(obj) obj$structured_data[[1]])
    json_out = shell
    rm(shell)
  }
  
  # Create the JSON structure
  json = .measure_time(quote(jsonlite::toJSON(json_out,auto_unbox = TRUE, na = "null", null = "null")),"Prepared JSON from list. Runtime:")
  
  res$setHeader(name = "CONTENT-TYPE",value = "application/json")
  res$setHeader(name = "date", value = Sys.time())
  res$body = json
  
  return(res)
}

#* @apiTitle R UDF API
#*
#* Takes a UDFRequest containing data and code and runs the code on the data
#*
#* @post /udf
post_udf.json = function(req,res, debug=FALSE) {
  tryCatch({
    
    if (!is.null(debug) && isTRUE(debug)) {
      DEBUG = debug
    }
  
    # prepare the executable code
    fun = .prepare_udf_function(code = req$code$source)
    
    # if data requirements states something else than stars we need to convert it
    data_requirement = .read_data_requirement(req$code$source)
    
    if (length(data_requirement) > 0) {
      if (length(data_requirement$variable_name) > 0) {
        #replace variable name in fun, but keep the variable "context" which describes the user context
        names(formals(fun)) = c(data_requirement$variable_name,"context")
      }
    }
  
    # transform data into stars or simple data
    data_in = .translate_input_data(data = req$data, data_requirement)
  
    # run the UDF
    if (length(data_in) == 0) {
      stop("error while reading the input data")
    } else if (class(data_in) == "stars" || (is.list(data_in) && class(data_in[[1]]) == "stars")) {
      results = list(.measure_time(quote(do.call(fun,args = list(data_in,req$user_context))),"Executed script. Runtime:"))
    } else {
      # mainly for a multitude of structured data (e.g. multiple timeseries)
      results = list(.measure_time(quote(lapply(data_in, fun, context = req$user_context)),"Executed script. Runtime:"))
    }
  
    # map to stars or keep simple data types
    results = lapply(1:length(results), function(index) {
      if (any(class(results[[index]]) %in% "stars")) {
        return(results[[index]])
      } else if (any(class(results[[index]]) %in% "xts")) { # TODO check later only xts to go in here (actual xts of results)
        return(st_as_stars(results[[index]]))
      } else if (any(class(results[[index]]) %in%
                     c("list","numeric","integer","character","factor","logical","matrix","data.frame"))) {
        return(results[[index]])
      } else {
        stop("UDF data return is not a simple type, xts or stars.")
      }
    })
  
    # transform stars into HyperCube, simple data types into StructuredData
    if ("stars" %in% class(results[[1]])) { # only if all results are stars objects
      if (! all(sapply(results,function(res)"stars"==class(res)))) {
        stop("All data outputs have to be of class 'stars' or any structured data. Mixed types not supported, yet.")
      }
      json_out = list(
        user_context = if (length(req$user_context) == 0) NA else length(req$user_context),
        server_context = if (length(req$server_context) == 0) NA else req$server_context,
        data_collection=.measure_time(quote(as(results,"DataCube")),"Translated from stars to DataCollection. Runtime:"))
        
    } else {
      json_out = .measure_time(quote(lapply(results,function(obj) as(obj,"StructuredData"))),"Translated from simple data to StructuredData. Runtime:")
      
      json_out = list(
        user_context = if (length(req$user_context) == 0) NA else length(req$user_context),
        server_context = if (length(req$server_context) == 0) NA else req$server_context,
        structured_data_list = json_out
      )
    }
  
    rm(results)
  
    # Create the JSON structure
    json = .measure_time(quote(jsonlite::toJSON(json_out,
                                                auto_unbox = TRUE,
                                                force=TRUE,
                                                na = "null",
                                                null = "null",
                                                digits = if (length(req$server_context$export_digits) == 0) 4 else req$server_context$export_digits)),
                         "Prepared JSON from list. Runtime:")
  }, error = function(e) {
    json = jsonlite::toJSON(e,auto_unbox = T,force=T)
    res$status = 500
  })
  
  res$setHeader(name = "Content-Type",value = "application/json")
  res$setHeader(name = "date", value = Sys.time())
  res$body = json

  return(res)
}

#* @get /
#* @serializer unboxedJSON
#* preempt check-data
udf_version = function(){
  return(list(
    runtime=list(
      language="R",
      service_name="r-udf-service",
      service_version = r_udf_version,
      api_version = api_version
    )
  ))
}

#* Gets the library configuration of this udf service
#* @get /packages
#* @serializer unboxedJSON
#* @preempt check-data
get_installed_libraries = function() {
  libs = as.data.frame(installed.packages()[,c("Package","Version")])
  rownames(libs) = NULL

  return(libs)
}

.prepare_udf_function = function(code) {
  fun = function() {}
  formals(fun) = alist(data=,context=) #TODO also metadata from run_udf (processes api)

  tryCatch({
    if (!startsWith(code,"{")) {
      # if a starting bracket is missing set opening and closing ones, otherwise we assume that the
      # provided code is clean
      code = paste0("{\n",code,"\n}")
    }

    body(fun) = parse(text=code)
  },
  error = function(e) {
    stop(paste0("Provided R code is not valid. Please check code syntax, parenthesis and spelling. Message: ",e$message))
  })

  return(fun)
}


.translate_input_data_legacy = function(data,data_requirement=NULL) {
  if ("HyperCube" %in% class(data)) {
    data_in = .measure_time(quote(as(data,"stars")),"Translated list into stars. Runtime:")
  } else if ("StructuredDataLegacy" %in% class(data)) {
    data_in = .measure_time(quote(as.StructuredDataLegacy.base(data)),"Translated into simple data. Runtime:")
  }
  
  if (length(data_requirement) > 0) {
    if (length(data_requirement$target_class) > 0) {
      switch(data_requirement$target_class,
             xts = {
               # coerce stars_in into the target class
               data_in = lapply(data_in, function(stars) {
                 if (! "stars" %in% class(stars)) stop("Coercion into xts failed. Input data is no stars object.")
                 
                 if (! "t" %in% names(st_dimensions(stars))) {
                   stop("No temporal dimension 't' found.")
                 }
                 as.xts(stars)
               })
             },
             list = {
               data_in = lapply(data_in,as.list)
             },
             data.frame = {
               data_in = lapply(data_in,as.data.frame)
             },
             matrix = {
               data_in = lapply(data_in,as.matrix)
             },
             tibble = {
               data_in = lapply(data_in,tibble::as_tibble)
             },
             numeric = {
               data_in = lapply(data_in,as.numeric)
             },
             character = {
               data_in = lapply(data_in,as.character)
             },
             integer = {
               data_in = lapply(data_in,as.integer)
             },
             logical = {
               data_in = lapply(data_in,as.logical)
             },
             factor = {
               data_in = lapply(data_in,as.factor)
             },
             {
               # default behavior
               if (!(length(data_requirement$target_class) > 0 &&
                     data_requirement$target_class == "stars")) {
                 stop("Not supported variable class. Use 'stars' or 'xts'")
               }
             }
      )
    }
  }
  
  return(data_in)
}

.translate_input_data = function(data,data_requirement=NULL) {
  
  if ("DataCube" == class(data)) {
    data_in = .measure_time(quote(as(data,"stars")),"Translated list into stars. Runtime:")
  } else if ("StructuredData" %in% class(data)) {
    data_in = .measure_time(quote(as.StructuredData.base(data)),"Translated into simple data. Runtime:")
  } 

  if (length(data_requirement) > 0) {
    if (length(data_requirement$target_class) > 0) {
      switch(data_requirement$target_class,
             xts = {
               # coerce stars_in into the target class
               data_in = lapply(data_in, function(stars) {
                 if (! "stars" %in% class(stars)) stop("Coercion into xts failed. Input data is no stars object.")

                 if (! "t" %in% names(st_dimensions(stars))) {
                   stop("No temporal dimension 't' found.")
                 }
                 as.xts(stars)
               })
             },
             list = {
               data_in = lapply(data_in,as.list)
             },
             data.frame = {
               data_in = lapply(data_in,as.data.frame)
             },
             matrix = {
               data_in = lapply(data_in,as.matrix)
             },
             tibble = {
               data_in = lapply(data_in,tibble::as_tibble)
             },
             numeric = {
               data_in = lapply(data_in,as.numeric)
             },
             character = {
               data_in = lapply(data_in,as.character)
             },
             integer = {
               data_in = lapply(data_in,as.integer)
             },
             logical = {
               data_in = lapply(data_in,as.logical)
             },
             factor = {
               data_in = lapply(data_in,as.factor)
             },
             {
               # default behavior
               if (!(length(data_requirement$target_class) > 0 &&
                     data_requirement$target_class == "stars")) {
                 stop("Not supported variable class. Use 'stars' or 'xts'")
               }
             }
         )
    }
  }

  return(data_in)
}
