
# blockchain.api.process ---------------------------------------------------

#' @title Process blockchain.info API
#'
#' @description Query and process results from blockchain.info.
#'
#' @param \dots params passed to blockchain.info API, specific for particular method, example \code{'bitcoin_address'} or \code{'tx_index'}, for more read \code{\link{blockchain.api.query}}.
#' @param method character. For details see \code{blockchain.api.query}, currently supported \code{'Single Address'} and \code{'Single Transaction'}. If \code{method} missing the function will try to guess it based on first param in \dots.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @details By default it will perform antiddos check and wait if required, it can be turned off but in such case you should expect to be banned quite easily. Read \code{antiddos}.
#' @return post processed response from blockchain api, list or data.table.
#' @seealso \code{\link{blockchain.api.query}}, \code{\link{antiddos}}
#' @references \url{https://blockchain.info/api/blockchain_api}
#' @export
#' @examples
#' \dontrun{
#' # Rbitcoin donation address wallet
#' Rbitcoin_donation_wallet <- blockchain.api.process('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi')
#' # some transaction
#' tx <- blockchain.api.process('e5c4de1c70cb6d60db53410e871e9cab6a0ba75404360bf4cda1b993e58d45f8')
#' tx
#' }
blockchain.api.process <- function(... , method, 
                                   verbose = getOption("Rbitcoin.verbose",0)){
  input_list <- list(...)
  if(missing(method)){
    if(length(input_list) < 1) stop(paste0('missing method and missing ... param'))
    if(nchar(input_list[[1]]) == 34 | any(names(input_list[1]) == 'bitcoin_address')) method <- 'Single Address' #any used to handle NULL names
    else if(nchar(input_list[[1]]) == 64 | any(names(input_list[1]) == 'tx_index')) method <- 'Single Transaction'
    else stop(paste0('missing method and invalid first ... param'))
  }
  res <- switch(method,
                "Single Address" = {
                  r <- blockchain.api.query(..., method = method, limit = 0, verbose = verbose - 1)
                  data.table(location = 'blockchain', 
                             action = 'Single Address',
                             timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'), 
                             currency = 'BTC', 
                             hash = r[['hash160']], address = r[['address']], n_tx = r[['n_tx']], 
                             total_received = r[['total_received']]/1e8, 
                             total_sent = r[['total_sent']]/1e8, 
                             final_balance = r[['final_balance']]/1e8)
                },
                "Single Transaction" = {
                  r <- blockchain.api.query(..., method = method, verbose = verbose - 1)
                  list(location = 'blockchain', 
                       action = 'Single Transaction',
                       timestamp = as.POSIXct(r[['time']], origin = '1970-01-01', tz = 'UTC'),
                       currency = 'BTC',
                       double_spend = r[['double_spend']],
                       inputs = data.table(address = r[['inputs']][['prev_out']][['addr']], value = r[['inputs']][['prev_out']][['value']]/1e8, tx_index = r[['inputs']][['prev_out']][['tx_index']]),
                       hash = r[['hash']],
                       tx_index = r[['tx_index']],
                       out = data.table(address = r[['out']][['addr']], value = r[['out']][['value']]/1e8, tx_index = r[['out']][['tx_index']]),
                       size = r[['size']])
                })
  if(verbose > 0) cat(as.character(Sys.time()),': blockchain.api.process: call and post-processing finished','\n',sep='')
  res
}

# blockchain.api.query  -----------------------------------------------------

#' @title Query blockchain.info API
#'
#' @description Query bitcoin related data from blockchain.info.
#'
#' @param \dots params passed to blockchain.info API, specific for particular method, example \code{'bitcoin_address'} or \code{'tx_index'}, for more see references or examples.
#' @param method character. For details see references, currently supported \code{'Single Address'} and \code{'Single Transaction'}. If \code{method} missing the function will try to guess it based on first param in \dots.
#' @param antiddos logical default \code{TRUE}. Default 10s, read \code{\link{antiddos}}.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return result returned by \code{fromJSON} function applied on the blockchain result, most probably the list.
#' @seealso \code{\link{blockchain.api.process}}, \code{\link{antiddos}}
#' @references \url{https://blockchain.info/api/blockchain_api}
#' @export
#' @examples
#' \dontrun{
#' # query bitcoin address information - 'Single Address' method
#' # Rbitcoin donation address final balance in BTC
#' blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi',limit=0)[['final_balance']]/1e8
#' # Rbitcoin donation address full details
#' blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi',verbose=1)
#' # some first wallet final balance in BTC
#' blockchain.api.query('1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa',limit=0)[['final_balance']]/1e8
#' # some first wallet details (limit to 3 txs, skip two txs)
#' x <- blockchain.api.query(method = 'Single Address',
#'      bitcoin_address = '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa', limit=3, offset=2)
#' str(x)
#' # query bitcoin transaction information - 'Single Transaction' method
#' # Some recent transaction of some first wallet
#' blockchain.api.query('e5c4de1c70cb6d60db53410e871e9cab6a0ba75404360bf4cda1b993e58d45f8')
#' }
blockchain.api.query <- function(... , method,
                                 antiddos = getOption("Rbitcoin.antiddos",TRUE),
                                 verbose = getOption("Rbitcoin.verbose",0)){
  input_list <- list(...)
  if(missing(method)){ # MAINTAIN second version in .process fun
    if(length(input_list) < 1) stop(paste0('missing method and missing ... param'))
    if(nchar(input_list[[1]]) == 34 | any(names(input_list[1]) == 'bitcoin_address')) method <- 'Single Address' #any used to handle NULL names
    else if(nchar(input_list[[1]]) == 64 | any(names(input_list[1]) == 'tx_index')) method <- 'Single Transaction'
    else stop(paste0('missing method and invalid first ... param'))
  }
  url <- switch(method,
                'Single Address' = {
                  url <- 'https://blockchain.info/address/'
                  if(!is.null(input_list[['bitcoin_address']])) url <- paste0(url,input_list[['bitcoin_address']],'?format=json')
                  else {
                    if(length(input_list) < 1) stop(paste0('missing params in ... for ',method,' method'))
                    if(nchar(input_list[[1]]) == 34) url <- paste0(url,input_list[[1]],'?format=json')
                    else stop(paste0('invalid params in ... for ',method,' method, when providing unnamed params the first one must be bitcoin address with nchar = 34'))
                  }
                  if(!is.null(input_list[['limit']])) url <- paste0(url,'&limit=',input_list[['limit']])
                  if(!is.null(input_list[['offset']])) url <- paste0(url,'&offset=',input_list[['offset']])
                  url
                },
                'Single Transaction' = {
                  url <- 'https://blockchain.info/tx-index/'
                  if(!is.null(input_list[['tx_index']])) url <- paste0(url,input_list[['tx_index']],'?format=json')
                  else {
                    if(length(input_list) < 1) stop(paste0('missing params in ... for ',method,' method'))
                    if(nchar(input_list[[1]]) == 64) url <- paste0(url,input_list[[1]],'?format=json')
                    else stop(paste0('invalid params in ... for ',method,' method, when providing unnamed params the first one must be tx index with nchar = 64'))
                  }
                  if(!is.null(input_list[['scripts']])) url <- paste0(url,'&scripts=',tolower(as.character(input_list[['scripts']])))
                  if(!is.null(input_list[['format']])) stop(paste0('format param not supported, hardcoded for \'json\''))
                  url
                },
                stop(paste0('blockchain.api.query: unsupported method: ',method)))
  wait <- if(antiddos) getOption("Rbitcoin.antiddos.fun",antiddos_fun)(source_system = "blockchain", verbose = verbose - 1) else 0
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  res_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  res <- fromJSON(res_json)
  if(verbose > 0) cat(as.character(Sys.time()),': blockchain.api.query: call performed','\n',sep='')
  return(res)
}

# to-from BTC -------------------------------------------------------------

#' @title Fast convert to/from BTC
#'
#' @description Fast convert to/from BTC based on blockchain.info exchange rates API.
#'
#' @param value numeric.
#' @param currency character.
#' @param antiddos logical default \code{TRUE}. Default 10s, read \code{\link{antiddos}}.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return converted value.
#' @seealso \code{\link{fromBTC}}, \code{\link{blockchain.api.query}}, \code{\link{antiddos}}
#' @references \url{https://blockchain.info/api/exchange_rates_api}
#' @export
#' @examples
#' \dontrun{
#' # 1000 USD (default) to BTC
#' toBTC(1000)
#' toBTC(1000, "GBP")
#' # 1 BTC to USD (default)
#' fromBTC(1)
#' fromBTC(1, "EUR")
#' }
toBTC <- function(value = 1, currency = "USD",
                  antiddos = getOption("Rbitcoin.antiddos",TRUE),
                  verbose = getOption("Rbitcoin.verbose",0)){
  wait <- if(antiddos) getOption("Rbitcoin.antiddos.fun",antiddos_fun)(source_system = "blockchain", verbose = verbose - 1) else 0
  url <- paste0('https://blockchain.info/tobtc?currency=',currency,'&value=',format(value, scientific = FALSE))
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  charNum <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  r <- as.numeric(gsub(",","",charNum))
  if(verbose > 0) cat(as.character(Sys.time()),': toBTC: done','\n',sep='')
  r
}

#' @title Fast convert to/from BTC
#'
#' @description Fast convert to/from BTC based on blockchain.info exchange rates API.
#'
#' @param value numeric.
#' @param currency character.
#' @param antiddos logical default \code{TRUE}. Default 10s, read \code{\link{antiddos}}.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return converted value.
#' @seealso \code{\link{toBTC}}, \code{\link{blockchain.api.query}}, \code{\link{antiddos}}
#' @references \url{https://blockchain.info/api/exchange_rates_api}
#' @export
#' @examples
#' \dontrun{
#' # 1000 USD (default) to BTC
#' toBTC(1000)
#' toBTC(1000, "GBP")
#' # 1 BTC to USD (default)
#' fromBTC(1)
#' fromBTC(1, "EUR")
#' }
fromBTC <- function(value = 1, currency = "USD",
                    antiddos = getOption("Rbitcoin.antiddos",TRUE),
                    verbose = getOption("Rbitcoin.verbose",0)){
  wait <- if(antiddos) getOption("Rbitcoin.antiddos.fun",antiddos_fun)(source_system = "blockchain", verbose = verbose - 1) else 0
  url <- paste0('https://blockchain.info/tobtc?currency=',currency,'&value=1')
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  charNum <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  r <- value/as.numeric(gsub(",","",charNum))
  if(verbose > 0) cat(as.character(Sys.time()),': fromBTC: done','\n',sep='')
  r
}
