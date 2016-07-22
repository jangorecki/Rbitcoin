
# market.api.query.bitstamp -----------------------------------------------

#' @title Send request to bitstamp market API
#'
#' @description Send request to bitstamp market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param client_id character. Bitstamp market specific parameter used in private API call authorization (check reference for more information).
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return Character string a response from markets API call. Cancel order is an exception here, as bitstamp will not return json format for that method, it is additionally processed by toJSON.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://www.bitstamp.net/api/}
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.bitstamp(url = 'https://www.bitstamp.net/api/ticker/')
#' # wallet
#' market.api.query.bitstamp(url = 'https://www.bitstamp.net/api/balance/', 
#'                           client_id = '',
#'                           key = '', secret = '')
#' }
market.api.query.bitstamp <- function(url, client_id, key, secret, req = list(), 
                                      verbose = getOption("Rbitcoin.verbose",0)){
  if(length(req) > 0 | (!missing(key) & !missing(secret) & !missing(client_id))){
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    if(!missing(key) & !missing(secret)){
      sign <- toupper(hmac(key = secret, object = paste0(nonce,client_id,key), algo = 'sha256'))
    }
    post_data <- paste0('key=',key,'&signature=',sign,'&nonce=',nonce)
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
  }
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE, postfields = post_data))
  else stop(paste0('unhandled case on Rcurl functions calling in market.api.query.bitstamp'))
  if(verbose > 0) cat(as.character(Sys.time()),': market.api.query.bitstamp: api call performed on: ',url,'\n',sep='')
  return(query_result_json)
}

# bitstamp_api_dict ----------------------------------------------------------------

bitstamp_api_dict <- function(){
  
  # define global bitstamp
  bitstamp_api_dict_ticker <- function(market = "bitstamp", base, quote, action = "ticker"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'ticker', url = 'https://www.bitstamp.net/api/ticker/',
               pre_process = c(function(x) x),
               post_process = c(function(x) data.table(market = market, base = base, quote = quote,
                                                       timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                       market_timestamp = as.POSIXct(as.numeric(x[['timestamp']]), origin = '1970-01-01', tz = 'UTC'),
                                                       last = as.numeric(x[['last']]), 
                                                       vwap = as.numeric(x[['vwap']]), 
                                                       volume = as.numeric(x[['volume']]),
                                                       ask = as.numeric(x[['ask']]),
                                                       bid = as.numeric(x[['bid']]))),
               catch_market_error = c(function(x){
                 if(is.null(x[['last']])) stop(paste0("bitstamp ticker: error not handled by market: missing key object: x[['last']]"),call.=FALSE)
                 else x
               }))
  }
  bitstamp_api_dict_trades <- function(market = "bitstamp", base, quote, action = "trades"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'transactions', url = 'https://www.bitstamp.net/api/transactions/',
               pre_process = c(function(x){
                 if(!is.null(x[['tid']])) stop('bitstamp method=transactions does not handle tid/since/offset param, previously working \'offset\' param was obsolete in October 2013',call.=FALSE)
                 x
               }),
               post_process = c(function(x) list(market = market, base = base, quote = quote,
                                                 timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                 market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                                                 trades = as.data.table(x)[nrow(x):0
                                                                           ][,list(date = as.POSIXct(as.numeric(date), origin='1970-01-01', tz='UTC'), 
                                                                                   price = as.numeric(price), 
                                                                                   amount = as.numeric(amount),
                                                                                   tid = as.character(tid),
                                                                                   type = NA_character_)])),
               catch_market_error = c(function(x){
                 if(is.null(x)) stop(paste0("bitstamp trades: error not handled by market: missing key object"),call.=FALSE)
                 else if('error' %in% names(x)) stop(paste0('bitstamp trades: market error: ',x[['error']]),call.=FALSE)
                 else x
               }))
  }  
  bitstamp_api_dict_order_book <- function(market = "bitstamp", base, quote, action = "order_book"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'order_book', url = 'https://www.bitstamp.net/api/order_book/',
               pre_process = c(function(x) x),
               post_process = c(function(x) list(market = market, base = base, quote = quote,
                                                 timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                 market_timestamp = as.POSIXct(as.numeric(substr(x[['timestamp']],1,10)), origin = '1970-01-01', tz = 'UTC'),
                                                 asks = {
                                                   asks = as.data.table(x[["asks"]])
                                                   setnames(asks,c('price','amount'))
                                                   asks[,`:=`(price = as.numeric(price), amount = as.numeric(amount))][,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)]
                                                 },
                                                 bids = {
                                                   bids = as.data.table(x[["bids"]])
                                                   setnames(bids,c('price','amount'))
                                                   bids[,`:=`(price = as.numeric(price), amount = as.numeric(amount))][,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)]
                                                 })),
               catch_market_error = c(function(x){
                 if(is.null(x[['asks']]) | is.null(x[['bids']])) stop(paste0('bitstamp order_book: error not handled by market: missing key object (asks | bids)'),call.=FALSE)
                 else x
               }))
  }
  bitstamp_api_dict_wallet <- function(market = "bitstamp", base = NA_character_, quote = NA_character_, action = "wallet"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'balance', url = 'https://www.bitstamp.net/api/balance/',
               pre_process = c(function(x) x),
               post_process = c(function(x) list(market = market,
                                                 timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                 market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                                                 wallet = data.table(currency = c('BTC','USD'),
                                                                     amount = c(as.numeric(x[['btc_balance']]),as.numeric(x[['usd_balance']]))))),
               catch_market_error = c(function(x){
                 if('error' %in% names(x)) stop(paste0('bitstamp wallet: market error: ',x[['error']]),call.=FALSE)
                 else if(is.null(x[['btc_balance']])) stop(paste0("bitstamp wallet: error not handled by market: missing key object: x[['btc_balance']]"),call.=FALSE)
                 else x
               }))
  }
  bitstamp_api_dict_place_limit_order <- function(market = "bitstamp", base, quote, action = "place_limit_order"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'buy/sell', url = 'https://www.bitstamp.net/api/',
               pre_process = c(function(x){
                 assign('url', paste0(get('url',envir = parent.frame(1)),x[['type']],'/'),envir = parent.frame(1)) #overwrite url in market.api.process
                 x[['price']] <- trunc(x[['price']]*1e2)/1e2
                 x[['amount']] <- trunc(x[['amount']]*1e8)/1e8
                 x[names(x) %in% c('price','amount')]
               }),
               post_process = c(function(x) data.table(market = market, base = base, quote = quote,
                                                       timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                       market_timestamp = as.POSIXct(strptime(x[['datetime']], "%Y-%m-%d %H:%M:%S"), origin = '1970-01-01', tz = 'UTC'), 
                                                       oid = as.character(x[['id']]),
                                                       type = ifelse(x[['type']]==0,'buy',ifelse(x[['type']]==1,'sell',stop('bitstamp place_limit_order unknown type',call.=FALSE))),
                                                       price = as.numeric(x[['price']]),
                                                       amount = as.numeric(x[['amount']]))),
               catch_market_error = c(function(x){
                 if(is.null(x)) stop(paste0("bitstamp place_limit_order: error not handled by market: missing key object"),call.=FALSE)
                 else if('error' %in% names(x)) stop(paste0('bitstamp place_limit_order: market error: ',x[['error']]),call.=FALSE)
                 else x
               }))
  }
  bitstamp_api_dict_open_orders <- function(market = "bitstamp", base = NA_character_, quote = NA_character_, action = "open_orders"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'open_orders', url = 'https://www.bitstamp.net/api/open_orders/',
               pre_process = c(function(x) x),
               post_process = c(function(x){
                 if(length(x) == 0) list(market = market, 
                                         timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'), 
                                         market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                                         open_orders = data.table(base = as.character(NULL), 
                                                                  quote = as.character(NULL), 
                                                                  oid = as.character(NULL), 
                                                                  type = as.character(NULL), 
                                                                  price = as.numeric(NULL), 
                                                                  amount = as.numeric(NULL)))
                 else list(market = market, 
                           timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'), 
                           market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                           open_orders = data.table(base = 'BTC',
                                                    quote = 'USD',
                                                    oid = as.character(x[['id']]),
                                                    type = ifelse(x[['type']]==0,'buy',ifelse(x[['type']]==1,'sell',stop('bitstamp open_orders unknown type'))),
                                                    price = as.numeric(x[['price']]),
                                                    amount = as.numeric(x[['amount']])))
               }),
               catch_market_error = c(function(x){
                 if(is.null(x)) stop(paste0("bitstamp open_orders: error not handled by market: missing key object"),call.=FALSE)
                 else if('error' %in% names(x)) stop(paste0('bitstamp open_orders: market error: ',x[['error']]),call.=FALSE)
                 else x
               }))
  }
  bitstamp_api_dict_cancel_order <- function(market = "bitstamp", base = NA_character_, quote = NA_character_, action = "cancel_order"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'cancel_order', url = 'https://www.bitstamp.net/api/cancel_order/',
               pre_process = c(function(x) list(id = x[['oid']])),
               post_process = c(function(x){
                 if('error' %in% names(x) && x[['error']]=="Order not found") return(data.table(market = character(), base = character(), quote = character(),
                                                                                                timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC')[-1],
                                                                                                market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC')[-1], 
                                                                                                oid = character()))
                 data.table(market = market, base = base, quote = quote,
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                            market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                            oid = NA_character_)
               }),
               catch_market_error = c(function(x){
                 if(isTRUE(x)) return(x) # success
                 if('error' %in% names(x)){
                   if(x[['error']]=="Order not found"){
                     if(is.null(getOption("Rbitcoin.cancel_order.order_not_found"))) return(x) # will silently postprocessed to 0 row DT
                     if(getOption("Rbitcoin.cancel_order.order_not_found")=="warning"){
                       warning("bitstamp cancel_order was not performed, requested order not found",call. = FALSE)
                       return(x)
                     }
                     if(getOption("Rbitcoin.cancel_order.order_not_found")=="error"){
                       stop(paste0('bitstamp cancel_order was not performed: ',x[['error']]),call.=FALSE)
                     }
                   }
                   stop(paste0('bitstamp cancel_order: market error: ',x[['error']]),call.=FALSE)
                 }
                 stop(paste0("bitstamp cancel_order: error not handled by market, API did not return TRUE or valid error message"),call.=FALSE)
               }))
  }
  
  # generate dictionary
  api.dict.list <- list()
  
  # common for all currency pairs
  
  api.dict.list[[length(api.dict.list)+1]] <- bitstamp_api_dict_wallet()
  api.dict.list[[length(api.dict.list)+1]] <- bitstamp_api_dict_open_orders()
  api.dict.list[[length(api.dict.list)+1]] <- bitstamp_api_dict_cancel_order()
  
  # currency specific
  
  base = 'BTC'; quote = 'USD'
  api.dict.list[[length(api.dict.list)+1]] <- bitstamp_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitstamp_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitstamp_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitstamp_api_dict_place_limit_order(base = base, quote = quote)
  
  unique(setkeyv(rbindlist(api.dict.list),c("market","base","quote","action")), by=c("market","base","quote","action"))
}
