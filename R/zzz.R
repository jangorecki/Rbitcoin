.onLoad <- function(libname, pkgname){
  
  ## Dependencies options
  
  # turn off sci notation of numbers, important for market API calls
  options(scipen=100)
  
  # override RCurl default for SSL
  O <- getOption("RCurlOptions")
  O$ssl.verifyhost <- as.integer(2)
  O$ssl.verifypeer <- TRUE
  O$cainfo <- system.file("CurlSSL","cacert.pem",package="RCurl")
  options("RCurlOptions" = O)
  
  ## Rbitcoin options
  
  options(Rbitcoin.verbose = 0)
  options(Rbitcoin.antiddos = TRUE)
  options(Rbitcoin.antiddos.sec = 10)
  options(Rbitcoin.antiddos.fun = antiddos_fun)
  options(Rbitcoin.antiddos.verbose = 0)
  # plot related
  options(Rbitcoin.plot.mask = FALSE)
  options(Rbitcoin.plot.limit_pct = Inf)
  
  # purely technical
  options(Rbitcoin.json.debug = FALSE)
  
  # will be changed soons
  options(Rbitcoin.archive_exchange_rate = NULL)
  
  ## Rbitcoin dictionaries (see dictionaries.R for details)
  
  # currency type dict
  options(Rbitcoin.ct.dict = ct_dict())
  # query.dict
  options(Rbitcoin.query.dict = query_dict())
  # api.dict
  options(Rbitcoin.api.dict = api_dict())
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage("You are currently using Rbitcoin dev version 0.9.3, be aware of the changes coming in the releases 0.9.3 (github) and 0.9.4 (cran). Do not auto update Rbitcoin to 0.9.3 (or later) without testing. For details see github.com/jangorecki/Rbitcoin. This message will be removed in 0.9.5+.")
}

# default antiddos_fun cache
Rbitcoin.last_api_call <- new.env()
