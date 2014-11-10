
# market.api.query.btce ---------------------------------------------------

#' @title Send request to btce market API
#'
#' @description Send request to btce market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc. See note.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return Character string a response from market's API call.
#' @note Market specific btce \code{method} param should be provided in \code{req} object.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://btc-e.com/api/documentation}
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.btce(url = 'https://btc-e.com/api/2/btc_usd/ticker')
#' # wallet
#' market.api.query.btce(url = 'https://btc-e.com/tapi', 
#'                       req = list(method = 'getInfo'), 
#'                       key = '', secret = '')
#' }
market.api.query.btce <- function(url, key, secret, req = list(),
                                  verbose = getOption("Rbitcoin.verbose",0)){
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    nonce <- as.character(trunc(as.numeric(Sys.time()))) # no multiply "* 1000000" due to btce handle nonce a little worse then other markets, we need smaller nonce values
    post_data <- paste0('nonce=',nonce)
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&') 
    if(!missing(key) & !missing(secret)){
      sign <- hmac(key = secret, object = post_data, algo = 'sha512')
      httpheader <- c('Key' = key, 'Sign' = sign)
    }
  }
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 | !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in market.api.query.btce'))
  if(verbose > 0) cat(as.character(Sys.time()),': market.api.query.btce: api call performed on: ',url,'\n',sep='')
  return(query_result_json)
}

# btce_api_dict ----------------------------------------------------------------

btce_api_dict <- function(){
  
  # define global btce
  btce_api_dict_wallet <- function(market = "btce", base = NA_character_, quote = NA_character_, action = "wallet"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'getInfo', url = 'https://btc-e.com/tapi',
               pre_process = c(function(x) {x[['method']] <- 'getInfo'; x}),
               post_process = c(function(x) list(market = market,
                                                 timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                 market_timestamp = as.POSIXct(as.numeric(x[['return']][['server_time']]), origin = '1970-01-01', tz = 'UTC'),
                                                 wallet = data.table(currency = toupper(names(x[['return']][['funds']])),
                                                                     amount = as.numeric(x[['return']][['funds']])))),
               catch_market_error = c(function(x){
                 if(is.null(x[['success']])) stop(paste0("btce wallet: error not handled by market: x[['result']] a NULL"),call.=FALSE)
                 else if(x[['success']] == 0) stop(paste0("btce wallet: market error: ",x[['error']]),call.=FALSE)
                 else if(x[['return']][['open_orders']] > 0){ #WARNING WHEN OPEN ORDERS, market specific method
                   warning("btce wallet: WARNING: action could not provide total account balance, btce API does not handle total balance, only available funds, therefore you should cancel open orders beforehand", call. = FALSE)
                   x
                 }
                 else x
               }))
  }
  btce_api_dict_open_orders <- function(market = "btce", base = NA_character_, quote = NA_character_, action = "open_orders"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'ActiveOrders', url = 'https://btc-e.com/tapi',
               pre_process = c(function(x) {x[['method']] <- 'ActiveOrders'; x}),
               post_process = c(function(x){
                 if(x[['success']] == 0){
                   if(x[['error']] == 'no orders') list(market = market,
                                                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                        market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                                                        open_orders = data.table(base = as.character(NULL), 
                                                                                 quote = as.character(NULL), 
                                                                                 oid = as.character(NULL), 
                                                                                 type = as.character(NULL), 
                                                                                 price = as.numeric(NULL), 
                                                                                 amount = as.numeric(NULL)))
                 }
                 else {
                   FUN = function(x, header_oid) data.table(base = toupper(substr(x[['pair']],1,3)), 
                                                            quote =  toupper(substr(x[['pair']],5,7)), 
                                                            oid = header_oid,
                                                            type = as.character(x[['type']]),
                                                            price = as.numeric(x[['rate']]),
                                                            amount = as.numeric(x[['amount']]))
                   orders <- lapply(seq_along(x[['return']]), function(i){
                     FUN(
                       x[['return']][[i]],
                       names(x[['return']])[i]
                     )
                   })
                   list(market = market,
                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                        market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                        open_orders = rbindlist(orders))
                 }
               }),
               catch_market_error = c(function(x){
                 if(is.null(x[['success']])) stop(paste0("btce open_orders: error not handled by market: x[['result']] a NULL"),call.=FALSE)
                 else if(x[['success']] == 0){
                   if(x[['error']] == 'no orders') x
                   else stop(paste0("btce open_orders: market error: ",x[['error']]),call.=FALSE)
                 }
                 else x
               }))
  }
  btce_api_dict_cancel_order <- function(market = "btce", base = NA_character_, quote = NA_character_, action = "cancel_order"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'CancelOrder', url = 'https://btc-e.com/tapi',
               pre_process = c(function(x) list(method = 'CancelOrder', order_id = x[['oid']])),
               post_process = c(function(x){
                 data.table(market = market, base = base, quote = quote,
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                            market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                            oid = as.character(x[['return']][['order_id']]))
               }),
               catch_market_error = c(function(x){
                 if(is.null(x[['success']])) stop(paste0("btce cancel_order: error not handled by market: x[['result']] a NULL"),call.=FALSE)
                 else if(x[['success']] == 0) stop(paste0("btce cancel_order: market error: ",x[['error']]),call.=FALSE)
                 else x
               }))
  }
  btce_api_dict_ticker <- function(market = "btce", base, quote, action = "ticker"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'Ticker', url = paste0('https://btc-e.com/api/3/ticker/',paste(tolower(c(base,quote)),collapse="_")),
               pre_process = c(function(x) x),
               post_process = c(function(x) data.table(market = market, base = base, quote = quote, 
                                                       timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                       market_timestamp = as.POSIXct(x[[paste(tolower(c(base,quote)),collapse="_")]][['updated']], origin = '1970-01-01', tz = 'UTC'), 
                                                       last = x[[paste(tolower(c(base,quote)),collapse="_")]][['last']], 
                                                       vwap = x[[paste(tolower(c(base,quote)),collapse="_")]][['avg']], 
                                                       volume = x[[paste(tolower(c(base,quote)),collapse="_")]][['vol_cur']],
                                                       ask = x[[paste(tolower(c(base,quote)),collapse="_")]][['buy']],
                                                       bid = x[[paste(tolower(c(base,quote)),collapse="_")]][['sell']])),
               catch_market_error = c(function(x){
                 if(is.null(x[[paste(tolower(c(base,quote)),collapse="_")]])) stop(paste0('btce ticker: error not handled by market: missing key object: ticker NULL'),call.=FALSE)
                 else x
               }))
  }
  btce_api_dict_order_book <- function(market = "btce", base, quote, action = "order_book"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'depth', url = paste0('https://btc-e.com/api/3/depth/',paste(tolower(c(base,quote)),collapse="_")),
               pre_process = c(function(x) x),
               post_process = c(function(x) list(market = market, base = base, quote = quote,
                                                 timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                 market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                                                 asks = {
                                                   asks = data.table(x[[paste(tolower(c(base,quote)),collapse="_")]][['asks']])
                                                   setnames(asks,c('price','amount'))
                                                   asks[,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)]
                                                 },
                                                 bids = {
                                                   bids = data.table(x[[paste(tolower(c(base,quote)),collapse="_")]][['bids']])
                                                   setnames(bids,c('price','amount'))
                                                   bids[,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)]
                                                 })),
               catch_market_error = c(function(x){
                 if(is.null(x[[paste(tolower(c(base,quote)),collapse="_")]][['asks']]) | is.null(x[[paste(tolower(c(base,quote)),collapse="_")]][['bids']])) stop(paste0('btce order_book: error not handled by market: missing key object: (asks | bids) NULL'),call.=FALSE)
                 else x
               }))
  }
  btce_api_dict_place_limit_order <- function(market = "btce", base, quote, action = "place_limit_order"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'Trade', url = 'https://btc-e.com/tapi',
               pre_process = c(function(x) list(method = 'Trade', 
                                                pair = paste(tolower(c(base,quote)),collapse="_"), 
                                                type = x[['type']], 
                                                rate = trunc(x[['price']]*1e6)/1e6, 
                                                amount = trunc(x[['amount']]*1e8)/1e8)),
               post_process = c(function(x){
                 data.table(market = market, base = base, quote = quote,
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                            market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                            oid = as.character(x[['return']][['order_id']]),
                            type = NA_character_,
                            price = NA_real_,
                            amount = NA_real_)
               }),
               catch_market_error = c(function(x){
                 if(is.null(x[['success']])) stop(paste0("btce place_limit_order: error not handled by market: x[['result']] a NULL"),call.=FALSE)
                 else if(x[['success']] == 0) stop(paste0("btce place_limit_order: market error: ",x[['error']]),call.=FALSE)
                 else x
               }))
  }
  btce_api_dict_trades <- function(market = "btce", base, quote, action = "trades"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'trades', url = paste0('https://btc-e.com/api/3/trades/',paste(tolower(c(base,quote)),collapse="_")),
               pre_process = c(function(x){
                 if(!is.null(x[['tid']])) stop('btce trades does not handle tid param',call.=FALSE)
                 assign('url', 
                        paste0(get('url', envir = parent.frame(1)),
                               '?limit=2000'),
                        envir = parent.frame(1))
                 x
               }),
               post_process = c(function(x) list(market = market, base = base, quote = quote,
                                                 timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                 market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                                                 trades = {
                                                   trades <- as.data.table(x[[paste(tolower(c(base,quote)),collapse="_")]])
                                                   trades[nrow(trades):0
                                                          ][,list(date = as.POSIXct(timestamp, origin='1970-01-01', tz='UTC'), 
                                                                  price = as.numeric(price), 
                                                                  amount = as.numeric(amount),
                                                                  tid = as.character(tid),
                                                                  type = type)]
                                                 })),
               catch_market_error = c(function(x){
                 if(is.null(x)) stop(paste0('btce trades: error not handled by market: NULL'),call.=FALSE)
                 else x
               }))
  }  
  
  # generate dictionary
  api.dict.list <- list()
  
  # common for all currency pairs
  
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_wallet()
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_open_orders()
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_cancel_order()
  
  # currency specific
  
  base = 'BTC'; quote = 'USD'
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'LTC'; quote = 'USD'
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'LTC'; quote = 'BTC'
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'NMC'; quote = 'BTC'
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- btce_api_dict_place_limit_order(base = base, quote = quote)
  
  unique(setkeyv(rbindlist(api.dict.list),c("market","base","quote","action")))
}
