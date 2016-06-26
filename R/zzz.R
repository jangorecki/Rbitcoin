.onLoad <- function(libname, pkgname){

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
  options(Rbitcoin.cancel_order.order_not_found = NULL)
  # plot related
  options(Rbitcoin.plot.mask = FALSE)
  options(Rbitcoin.plot.limit_pct = Inf)
  
  # purely technical
  options(Rbitcoin.json.debug = FALSE)
  
  # wallet manager related
  options(Rbitcoin.wallet_manager.archive_path = "wallet_archive.rds")
  options(Rbitcoin.archive_exchange_rate = FALSE)
  
  ## Rbitcoin dictionaries (see dictionaries.R for details)
  
  # currency type dict
  options(Rbitcoin.ct.dict = ct_dict())
  # query.dict
  options(Rbitcoin.query.dict = query_dict())
  # api.dict
  options(Rbitcoin.api.dict = api_dict())
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage("You are using Rbitcoin 0.9.6, in case of migrating from CRAN release 0.9.2 see NEWS file for potentially breaking changes.")
}

# default antiddos_fun cache
Rbitcoin.last_api_call <- new.env()
