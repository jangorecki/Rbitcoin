
# market.api.query.bitmarket ----------------------------------------------

#' @title Send request to bitmarket market API
#'
#' @description Send request to bitmarket market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return Character string a response from markets API call.
#' @note Market specific bitmarket \code{method} param should be provided in \code{req} object.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://www.bitmarket.pl/docs.php?file=api_private.html}
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.bitmarket(url = 'https://www.bitmarket.pl/json/LTCPLN/ticker.json')
#' # wallet
#' market.api.query.bitmarket(url = 'https://www.bitmarket.pl/api2/',
#'                            req = list(method = 'info'),
#'                            key = '', secret = '')
#' }
market.api.query.bitmarket <- function(url, key, secret, req = list(), 
                                       verbose = getOption("Rbitcoin.verbose",0)){ 
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    tonce <- as.character(as.integer(Sys.time()))
    post_data <- paste0('tonce=',tonce)
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(!missing(key) & !missing(secret)){
      sign <- hmac(key = secret, object = post_data, algo = 'sha512')
      httpheader <- c('API-Key' = key, 'API-Hash' = sign)      
    }
  }
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on RCurl functions calling in market.api.query.bitmarket'), call. = FALSE)
  if(verbose > 0) cat(as.character(Sys.time()),': market.api.query.bitmarket: api call performed on: ',url,'\n',sep='')
  return(query_result_json)
}

# bitmarket_api_dict ----------------------------------------------------------------

bitmarket_api_dict <- function(){
  
  # define global bitmarket
  bitmarket_api_dict_ticker <- function(market = "bitmarket", base, quote, action = "ticker"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'ticker', url = paste0('https://www.bitmarket.pl/json/',paste(c(base,quote),collapse=""),'/ticker.json'),
               pre_process = c(function(x) x),
               post_process = c(function(x){
                 data.table(market = market, base = base, quote = quote, 
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'), 
                            market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                            last = x[['last']], 
                            vwap = x[['vwap']],
                            volume = x[['volume']],
                            ask = x[['ask']],
                            bid = x[['bid']])
               }),
               catch_market_error = c(function(x){
                 if(is.null(x[['last']])) stop(paste0("bitmarket ticker error not handled by market: key field is NULL"),call.=FALSE)
                 else x
               }))
  }
  bitmarket_api_dict_trades <- function(market = "bitmarket", base, quote, action = "trades"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'trades_public', url = paste0('https://www.bitmarket.pl/json/',paste(c(base,quote),collapse=""),'/trades.json'),
               pre_process = c(function(x){
                 if(!is.null(x[['tid']])){
                   assign('url', paste0(get('url',envir = parent.frame(1)),'?since=',as.character(x[['tid']])),envir = parent.frame(1))
                   x[['tid']] <- NULL
                 }
                 x
               }),
               post_process = c(function(x){
                 list(market = market, base = base, quote = quote,
                      timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                      market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                      trades = {
                        if(length(x)==0) data.table(date = as.POSIXct(NA,origin='1970-01-01',tz='UTC')[-1], 
                                                    price = numeric(), 
                                                    amount = numeric(), 
                                                    tid = character(), 
                                                    type = character())
                        else as.data.table(x)[,`:=`(date = as.POSIXct(date, origin='1970-01-01', tz='UTC'),
                                                    tid = as.character(tid),
                                                    type = as.character(type))
                                              ][,list(date,price,amount,tid,type)
                                                ][nrow(x):0]
                      })
               }),
               catch_market_error = c(function(x){
                 if(is.null(x)) stop(paste0('bitmarket trades error not handled by market: key field is NULL'),call.=FALSE)
                 else x
               }))
  }  
  bitmarket_api_dict_order_book <- function(market = "bitmarket", base, quote, action = "order_book"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'orderbook', url = paste0('https://www.bitmarket.pl/json/',paste(c(base,quote),collapse=""),'/orderbook.json'),
               pre_process = c(function(x) x),
               post_process = c(function(x){
                 list(market = market, base = base, quote = quote, 
                      timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                      market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                      asks = setnames(as.data.table(x[["asks"]]),c("price","amount"))[,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)],
                      bids = setnames(as.data.table(x[["bids"]]),c("price","amount"))[,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)])
               }),
               catch_market_error = c(function(x){
                 if(is.null(x[['asks']]) & is.null(x[['bids']])) stop(paste0("bitmarket order_book error not handled by market: key field is NULL"),call.=FALSE)
                 else x
               }))
  }
  bitmarket_api_dict_wallet <- function(market = "bitmarket", base = NA_character_, quote = NA_character_, action = "wallet"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'info', url = 'https://www.bitmarket.pl/api2/',
               pre_process = c(function(x) {x[['method']] <- 'info'; x}),
               post_process = c(function(x){
                 av <- x[['data']][['balances']][['available']]
                 bl <- x[['data']][['balances']][['blocked']]
                 av_dt <- data.table(currency = names(av), amount = unname(unlist(av)), key = 'currency')
                 bl_dt <- data.table(currency = names(bl), amount = unname(unlist(bl)), key = 'currency')
                 total_dt <- merge(av_dt,bl_dt, by ='currency', all = TRUE)[,list(amount = sum(amount.x,amount.y,na.rm=TRUE)), by=currency]
                 list(market = market,
                      timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                      market_timestamp = as.POSIXct(x[['time']], origin = '1970-01-01', tz = 'UTC'), 
                      wallet = total_dt)
               }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0) stop(paste0('bitmarket wallet market error: ',x[['errorMsg']]),call.=FALSE)
                 else if(is.null(x[['data']])) stop(paste0("bitmarket wallet error not handled by market: key field is NULL"),call.=FALSE)
                 else x
               }))
  }
  bitmarket_api_dict_place_limit_order <- function(market = "bitmarket", base, quote, action = "place_limit_order"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'trade', url = 'https://www.bitmarket.pl/api2/',
               pre_process = c(function(x){
                 list(method = 'trade',
                      market = paste(c(base,quote),collapse=""),
                      type = x[['type']], 
                      amount = trunc(x[['amount']]*1e6)/1e6,
                      rate = trunc(x[['price']]*1e4)/1e4)
               }),
               post_process = c(function(x){
                 data.table(market = market, base = base, quote = quote,
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                            market_timestamp = as.POSIXct(x[['time']], origin = '1970-01-01', tz = 'UTC'), 
                            oid = NA_character_,
                            type = NA_character_,
                            price = NA_real_,
                            amount = NA_real_)
               }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0) stop(paste0('bitmarket place_limit_order market error: ',x[['errorMsg']]),call.=FALSE)
                 else if(is.null(x[['data']])) stop(paste0("bitmarket place_limit_order error not handled by market: key field is NULL"),call.=FALSE)
                 else x
               }))
  }
  bitmarket_api_dict_open_orders <- function(market = "bitmarket", base = NA_character_, quote = NA_character_, action = "open_orders"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'orders', url = 'https://www.bitmarket.pl/api2/',
               pre_process = c(function(x) {x[['method']] <- 'orders'; x}),
               post_process = c(function(x){
                 dt <- rbindlist(lapply(x[['data']], function(currency_pair){
                   rbindlist(list(
                     as.data.table(currency_pair[['sell']]),
                     as.data.table(currency_pair[['buy']])
                   ))
                 }))
                 if(nrow(dt) > 0){
                   setnames(dt,'market','currency_pair') # proper name for BTCPLN
                   list(market = market,
                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                        market_timestamp = as.POSIXct(x[['time']], origin = '1970-01-01', tz = 'UTC'),
                        open_orders = dt[,`:=`(base = toupper(substr(currency_pair,1,3)),
                                               quote = toupper(substr(currency_pair,4,6)),
                                               oid = as.character(id),
                                               type = type,
                                               price = as.numeric(rate),
                                               amount = as.numeric(amount))
                                         ][,list(base, quote, oid, type, price, amount)])
                 }
                 else if(nrow(dt) == 0){
                   list(market = market,
                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'), 
                        market_timestamp = as.POSIXct(x[['time']], origin = '1970-01-01', tz = 'UTC'),
                        open_orders = data.table(base = character(), quote = character(), 
                                                 oid = character(), type = character(), price = numeric(), amount = numeric()))
                 }
               }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0) stop(paste0('bitmarket open_orders market error: ',x[['errorMsg']]),call.=FALSE)
                 else if(is.null(x[['data']])) stop(paste0("bitmarket open_orders error not handled by market: key field is NULL"),call.=FALSE)
                 else x
               }))
  }
  bitmarket_api_dict_cancel_order <- function(market = "bitmarket", base = NA_character_, quote = NA_character_, action = "cancel_order"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'cancel', url = 'https://www.bitmarket.pl/api2/',
               pre_process = c(function(x) list(method = 'cancel', id = x[['oid']])),
               post_process = c(function(x){
                 if(length(x[['error']]) > 0 && x[['error']]==406) return(data.table(market = character(), base = character(), quote = character(),
                                                                                     timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC')[-1],
                                                                                     market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC')[-1], 
                                                                                     oid = character()))
                 data.table(market = market, base = NA_character_, quote = NA_character_,
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                            market_timestamp = as.POSIXct(x[['time']], origin = '1970-01-01', tz = 'UTC'), 
                            oid = NA_character_)
               }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0){
                   if(x[['error']]==406){
                     if(is.null(getOption("Rbitcoin.cancel_order.order_not_found"))) return(x) # will silently postprocessed to 0 row DT
                     if(getOption("Rbitcoin.cancel_order.order_not_found")=="warning"){
                       warning("bitmarket cancel_order was not performed, requested order not found",call. = FALSE)
                       return(x)
                     }
                     if(getOption("Rbitcoin.cancel_order.order_not_found")=="error"){
                       stop(paste0('bitmarket cancel_order was not performed: ',x[['error']],': ',x[['errorMsg']]),call.=FALSE)
                     }
                   }
                   stop(paste0('bitmarket cancel_order market error: ',x[['error']],': ',x[['errorMsg']]),call.=FALSE)
                 }
                 if(is.null(x[['data']])) stop(paste0("bitmarket cancel_order error not handled by market: key element 'data' is NULL"),call.=FALSE)
                 x
               }))
  }
  
  # generate dictionary
  api.dict.list <- list()
  
  # common for all currency pairs
  
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_wallet()
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_open_orders()
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_cancel_order()
  
  # currency specific
  
  base = 'BTC'; quote = 'EUR'
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'BTC'; quote = 'PLN'
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'LTC'; quote = 'PLN'
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'LTC'; quote = 'BTC'
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- bitmarket_api_dict_place_limit_order(base = base, quote = quote)
  
  unique(setkeyv(rbindlist(api.dict.list),c("market","base","quote","action")))
}
