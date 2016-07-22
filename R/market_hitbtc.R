
# market.api.query.hitbtc ----------------------------------------------

#' @title Send request to hitbtc market API
#'
#' @description Send request to hitbtc market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return Character string a response from markets API call.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://github.com/hitbtc-com/hitbtc-api}
#' @keywords internal 
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.hitbtc(url = "https://api.hitbtc.com/api/1/public/BTCUSD/ticker")
#' 
#' # wallet
#' market.api.query.hitbtc(url = "https://api.hitbtc.com/api/1/trading/balance",
#'                         key = '', secret = '')
#' }
market.api.query.hitbtc <- function(url, key, secret, req = list(), 
                                    verbose = getOption("Rbitcoin.verbose",0)){
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    post_data <- NULL
    nonce <- as.character(as.integer(Sys.time()) * 1e3)
    if(length(req) > 0) post_data <- paste(paste(names(req),req,sep='='),collapse='&')
    if(!missing(key) & !missing(secret)){
      method_url = substr(url, nchar('https://api.hitbtc.com')+1, nchar(url))
      req_url = paste0("?",paste(paste("nonce",nonce,sep="="),paste("apikey",key,sep="="),sep="&"))
      url = paste0(url,req_url)
      sign <- digest::hmac(key = secret, object = paste0(method_url,req_url,post_data), algo = 'sha512')
      httpheader <- paste0("X-Signature: ",sign) 
    }
  }
  curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) == 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                         httpheader = httpheader))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                        postfields = post_data, 
                                                                                                                        httpheader = httpheader))
  else stop(paste0('unhandled case on RCurl functions calling in market.api.query.hitbtc'), call. = FALSE)
  if(verbose > 0) cat(as.character(Sys.time()),': market.api.query.hitbtc: api call performed on: ',url,'\n',sep='')
  return(query_result_json)
}

# hitbtc_api_dict ----------------------------------------------------------------

hitbtc_api_dict <- function(){
  
  # define global hitbtc technical
  hitbtc_lot_dict <- function(i){
    # lot.dict.generator - use for update lot.dict
    lot.dict = data.table(
      symbol = c("BCNBTC", "BTCEUR", "BTCUSD", "DOGEBTC", "EURGBP", "EURUSD", "FCNBTC", "GBPUSD", "LTCBTC", "LTCEUR", "LTCUSD", "NXTBTC", "QCNBTC", "XDNBTC", "XMRBTC"),
      step = c(0.000000001, 0.01, 0.01, 0.000000001, 0.0001, 0.0001, 0.000001, 0.0001, 0.00001, 0.001, 0.001, 0.00000001, 0.000001, 0.000000001, 0.000001),
      lot = c(100, 0.01, 0.01, 1000, 1, 1, 0.01, 1, 0.1, 0.1, 0.1, 1, 0.01, 100, 0.01),
      currency = c("BTC", "EUR", "USD", "BTC", "GBP", "USD", "BTC", "USD", "BTC", "EUR", "USD", "BTC", "BTC", "BTC", "BTC"),
      commodity = c("BCN", "BTC", "BTC", "DOGE", "EUR", "EUR", "FCN", "GBP", "LTC", "LTC", "LTC", "NXT", "QCN", "XDN", "XMR"), 
      takeLiquidityRate = c(0.001, 0.001, 0.001, 0.001, 0.0002, 0.0002, 0.001, 0.0002, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001),
      provideLiquidityRate = c(-0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001, -0.0001)
      , key = "symbol"
    )
    lot.dict.filtered = lot.dict[i,nomatch=NA]
    if(any(lot.dict.filtered[,is.na(lot)])) stop("hitbtc lot dictionary is outdated, update hitbtc_lot_dict function in market_hitbtc.R, report and/or pull request fix to github repo")
    # would be simple if and when FR solved: https://github.com/Rdatatable/data.table/issues/940
    lot.dict.filtered
  }
  
  # define global hitbtc
  hitbtc_api_dict_ticker <- function(market = "hitbtc", base, quote, action = "ticker"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'ticker', url = paste0("https://api.hitbtc.com/api/1/public/",paste(c(base,quote),collapse=""),"/ticker"),
               pre_process = c(function(x) x),
               post_process = c(function(x){
                 rbindlist(list(x)
                 )[,lapply(.SD, as.numeric)
                   ][,list(market = market, base = base, quote = quote,
                           timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                           market_timestamp = as.POSIXct(timestamp*1e-3, origin = '1970-01-01', tz = 'UTC'),
                           last, vwap = NA_real_, volume, ask, bid)
                     ]
               }),
               catch_market_error = c(function(x){
                 if(identical(c("code","message"),names(x))){
                   stop(paste0('hitbtc ticker: market error: ',paste(x,collapse=": ")),call.=FALSE)
                 }
                 if(is.null(x[['last']])) stop(paste0("hitbtc ticker error not handled by market: key field is NULL"),call.=FALSE)
                 else x
               }))
  }
  hitbtc_api_dict_trades <- function(market = "hitbtc", base, quote, action = "trades"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'trades', url = paste0("https://api.hitbtc.com/api/1/public/",paste(c(base,quote),collapse=""),"/trades"),
               pre_process = c(function(x){
                 if(is.null(x[['tid']])){
                   url_add = "/recent?max_results=1000&format_item=object&side=true"
                 } # recent trades
                 else {
                   # this is required due to: https://github.com/hitbtc-com/hitbtc-api/issues/6
                   url_add = paste0("?from=",x[['tid']],"&by=trade_id&sort=asc&start_index=0&max_results=1000&format_item=object&format_price=number&format_amount=number&format_amount_unit=currency&format_tid=string&format_timestamp=millisecond&format_wrap=true&side=true")
                   x['tid'] <- NULL
                 } # since tid
                 assign('url', paste0(get('url',envir = parent.frame(1)),url_add),envir = parent.frame(1))
                 x
               }),
               post_process = c(function(x){
                 list(market = market, base = base, quote = quote,
                      timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                      market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                      trades = {
                        if(length(x[["trades"]])==0) data.table(date = as.POSIXct(NA,origin='1970-01-01',tz='UTC')[-1], price = numeric(), amount = numeric(), tid = character(), type = character())
                        else{
                          xx = setDT(x[["trades"]])
                          # api issue: https://github.com/hitbtc-com/hitbtc-api/issues/6
                          if(nrow(xx) >= 2){ # check ordering
                            tid12 = xx[1:2,as.numeric(tid)]
                            iorder = if(tid12[1] < tid12[2]) 1:nrow(xx) else if(tid12[1] > tid12[2]) nrow(xx):1 else stop("hitbtc trades postprocess, review API workaround in market_hitbtc for issue https://github.com/hitbtc-com/hitbtc-api/issues/6")
                          }
                          else if(nrow(xx) < 2){ # check ordering
                            iorder = nrow(xx)
                          }
                          if(!"side" %in% names(xx)) xx[,side:=NA_character_] # hitbtc bug: https://github.com/hitbtc-com/hitbtc-api/issues/4
                          xx[,`:=`(date = as.POSIXct(date*1e-3, origin='1970-01-01', tz='UTC'), price=as.numeric(price), amount=as.numeric(amount), tid = as.character(tid),
                                   type = side)][type=="buy",type:="bid"][type=="sell",type:="ask"][,list(date,price,amount,tid,type)][iorder]
                        }
                      })
               }),
               catch_market_error = c(function(x){
                 if(identical(c("code","message"),names(x))){
                   stop(paste0('hitbtc trades: market error: ',paste(x,collapse=": ")),call.=FALSE)
                 }
                 if(is.null(x)) stop(paste0('hitbtc trades error not handled by market: key field is NULL'),call.=FALSE)
                 else x
               }))
  }
  hitbtc_api_dict_order_book <- function(market = "hitbtc", base, quote, action = "order_book"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'orderbook', url = paste0("https://api.hitbtc.com/api/1/public/",paste(c(base,quote),collapse=""),"/orderbook"),
               pre_process = c(function(x){
                 url_add = "?format_price=number&format_amount=number&format_amount_unit=currency"
                 assign('url', paste0(get('url',envir = parent.frame(1)),url_add),envir = parent.frame(1))
                 x
               }),
               post_process = c(function(x){
                 list(market = market, base = base, quote = quote, 
                      timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                      market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                      asks = setnames(as.data.table(x[["asks"]]),c("price","amount"))[,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)],
                      bids = setnames(as.data.table(x[["bids"]]),c("price","amount"))[,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)])
               }),
               catch_market_error = c(function(x){
                 if(identical(c("code","message"),names(x))){
                   stop(paste0('hitbtc order_book: market error: ',paste(x,collapse=": ")),call.=FALSE)
                 }
                 if(is.null(x[['asks']]) & is.null(x[['bids']])) stop(paste0("hitbtc order_book error not handled by market: key field is NULL"),call.=FALSE)
                 else x
               }))
  }
  hitbtc_api_dict_wallet <- function(market = "hitbtc", base = NA_character_, quote = NA_character_, action = "wallet"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'balance', url = 'https://api.hitbtc.com/api/1/trading/balance',
               pre_process = c(function(x) x),
               post_process = c(function(x) list(market = market,
                                                 timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                 market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                                                 wallet = setDT(x[["balance"]])[,list(currency=currency_code,amount=cash+reserved)])),
               catch_market_error = c(function(x){
                 if(identical(c("code","message"),names(x))){
                   stop(paste0('hitbtc wallet: market error: ',paste(x,collapse=": ")),call.=FALSE)
                 }
                 if(is.null(x)) stop(paste0('hitbtc wallet error not handled by market: key field is NULL'),call.=FALSE)
                 else x
               }))
  }
  hitbtc_api_dict_place_limit_order <- function(market = "hitbtc", base, quote, action = "place_limit_order"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'trading/new_order', url = 'https://api.hitbtc.com/api/1/trading/new_order',
               pre_process = c(function(x){                 
                 list(clientOrderId = as.character(as.integer(Sys.time())),
                      symbol = symbol <- paste(c(base,quote),collapse=""), # symbol reused in quantity
                      side = x[["type"]],
                      price = trunc(x[["price"]]*hitbtc_lot_dict(symbol)[,1/step])/hitbtc_lot_dict(symbol)[,1/step],
                      quantity = trunc(x[["amount"]]/(hitbtc_lot_dict(symbol)[,lot])),
                      type = "limit",
                      timeInForce = "GTC")
               }),
               post_process = c(function(x){
                 setDT(x[["ExecutionReport"]])[,list(market = market, base = base, quote = quote,
                                                     timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                     market_timestamp = as.POSIXct(timestamp*1e-3, origin = '1970-01-01', tz = 'UTC'), 
                                                     oid = as.character(clientOrderId),
                                                     type = side,
                                                     price = price,
                                                     amount = quantity*(hitbtc_lot_dict(symbol)[,lot]))]
               }),
               catch_market_error = c(function(x){
                 if(is.null(x)) stop(paste0('hitbtc place_limit_order error not handled by market: key field is NULL'),call.=FALSE)
                 else if(x$ExecutionReport$orderStatus=="rejected"){
                   stop(paste0('hitbtc place_limit_order: market error: order rejected, reject reason: ',x$ExecutionReport$orderRejectReason),call.=FALSE)
                 }
                 else if(identical(c("code","message"),names(x))){
                   stop(paste0('hitbtc place_limit_order: market error: ',paste(x,collapse=": ")),call.=FALSE)
                 }
                 else x
               }))
  }
  hitbtc_api_dict_open_orders <- function(market = "hitbtc", base = NA_character_, quote = NA_character_, action = "open_orders"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'orders/active', url = 'https://api.hitbtc.com/api/1/trading/orders/active',
               pre_process = c(function(x) x),
               post_process = c(function(x){
                 if(length(x[["orders"]])==0){
                   list(market = market,
                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                        market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                        open_orders = data.table(base = as.character(NULL), 
                                                 quote = as.character(NULL), 
                                                 oid = as.character(NULL), 
                                                 type = as.character(NULL), 
                                                 price = as.numeric(NULL), 
                                                 amount = as.numeric(NULL)))
                 }
                 else{
                   oo_lot <- setDT(x[["orders"]])[,list(base=toupper(substr(symbol,1,3)),quote=toupper(substr(symbol,4,6)), oid=clientOrderId, type=side, 
                                                        price=orderPrice, 
                                                        amount_lot=quantityLeaves),
                                                  keyby=symbol]
                   list(market = market,
                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                        market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                        open_orders = hitbtc_lot_dict(oo_lot)[, list(base,quote,oid,type,price,amount=amount_lot*lot)])
                 }
               }),
               catch_market_error = c(function(x){
                 if(identical(c("code","message"),names(x))){
                   stop(paste0('hitbtc open_orders: market error: ',paste(x,collapse=": ")),call.=FALSE)
                 }
                 if(is.null(x)) stop(paste0('hitbtc open_orders error not handled by market: key field is NULL'),call.=FALSE)
                 else x
               }))
  }
  hitbtc_api_dict_cancel_order <- function(market = "hitbtc", base = NA_character_, quote = NA_character_, action = "cancel_order"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'trading/cancel_order', url = 'https://api.hitbtc.com/api/1/trading/cancel_order',
               pre_process = c(function(x){
                 if(all(c("symbol","side","oid") %in% names(x))){
                   # workaround for https://github.com/hitbtc-com/hitbtc-api/issues/3
                   list(clientOrderId = x[['oid']],
                        cancelRequestClientOrderId = as.character(as.integer(Sys.time())),
                        symbol = x[['symbol']],
                        side = x[['side']])
                 }
                 else{
                   list(clientOrderId = x[['oid']], cancelRequestClientOrderId = as.character(as.integer(Sys.time())))
                 }
               }),
               post_process = c(function(x){
                 if('CancelReject' %in% names(x) && any(x[['CancelReject']][['rejectReasonCode']]=="orderNotFound")) return(data.table(market = character(), base = character(), quote = character(),
                                                                                                                                       timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC')[-1],
                                                                                                                                       market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC')[-1], 
                                                                                                                                       oid = character()))
                 setDT(x[["ExecutionReport"]])[,list(market = market, base=toupper(substr(symbol,1,3)),quote=toupper(substr(symbol,4,6)),
                                                     timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                                     market_timestamp = as.POSIXct(timestamp*1e-3, origin = '1970-01-01', tz = 'UTC'), 
                                                     oid = as.character(clientOrderId))]
               }),
               catch_market_error = c(function(x){
                 if('CancelReject' %in% names(x)){
                   if(x[['CancelReject']][['rejectReasonCode']]=="orderNotFound"){
                     if(is.null(getOption("Rbitcoin.cancel_order.order_not_found"))) return(x) # will silently postprocessed to 0 row DT
                     if(getOption("Rbitcoin.cancel_order.order_not_found")=="warning"){
                       warning("hitbtc cancel_order was not performed, requested order not found",call. = FALSE)
                       return(x)
                     }
                     if(getOption("Rbitcoin.cancel_order.order_not_found")=="error"){
                       stop(paste0('hitbtc cancel_order was not performed: ',x[['CancelReject']][['rejectReasonCode']]),call.=FALSE)
                     }
                   }
                   stop(paste0('hitbtc cancel_order: market error: ',x[['CancelReject']][['rejectReasonCode']]),call.=FALSE)
                 }

                 if(is.null(x[["ExecutionReport"]])) stop(paste0("hitbtc cancel_order error not handled by market: 'ExecutionReport' NULL"),call.=FALSE)
                 x
               }))
  }
  
  # generate dictionary
  api.dict.list <- list()
  
  # common for all currency pairs
  
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_wallet()
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_open_orders()
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_cancel_order()
  
  # currency specific
  
  base = 'BTC'; quote = 'EUR'
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'BTC'; quote = 'USD'
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'LTC'; quote = 'BTC'
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'LTC'; quote = 'USD'
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- hitbtc_api_dict_place_limit_order(base = base, quote = quote)
  
  unique(setkeyv(rbindlist(api.dict.list),c("market","base","quote","action")), by=c("market","base","quote","action"))
}