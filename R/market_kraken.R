
# query.dict --------------------------------------------------------------------

#' @title Send request to kraken market API
#'
#' @description Send request to kraken market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return Character string a response from markets API call.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://www.kraken.com/help/api}
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.kraken(url = 'https://api.kraken.com/0/public/Ticker?pair=XBTCZEUR')
#' # wallet
#' market.api.query.kraken(url = 'https://api.kraken.com/0/private/Balance', 
#'                         key = '', secret = '')
#' }
market.api.query.kraken <- function(url, key, secret, req = list(),
                                    verbose = getOption("Rbitcoin.verbose",0)){
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    post_data <- paste0('nonce=',nonce)
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(!missing(key) & !missing(secret)){
      method_path <- gsub("^.*?kraken.com","",url)
      sign <- hmac(
        key = base64Decode(secret,mode='raw'),
        object = c(
          charToRaw(method_path),
          digest(object = paste0(nonce,post_data), 
                 algo = 'sha256', serialize = FALSE,
                 raw = TRUE)
        ), 
        algo = 'sha512', 
        raw = TRUE)
      httpheader <- c('API-Key' = key, 'API-Sign' = base64Encode(sign))
    }
  }
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 | !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in market.api.query.kraken'))
  if(verbose > 0) cat(as.character(Sys.time()),': market.api.query.kraken: api call performed on: ',url,'\n',sep='')
  return(query_result_json)
}

# kraken_api_dict ----------------------------------------------------------------

kraken_api_dict <- function(){
  
  # define global kraken technical
  kraken_xbt_code <- function(currency){
    ifelse(currency!="XBT",
           ifelse(currency!="BTC",
                  currency,
                  "XBT"),
           "BTC")
  }
  ct_prefix <- function(currency, ct.dict = getOption("Rbitcoin.ct.dict",stop("Rbitcoin.ct.dict options mandatory, see zzz.R source file for default value"))){
    ifelse(!(currency %in% unlist(ct.dict)),
           stop(paste0("Expected currency: ",currency," do not exists in currency type dictionary, see getOption('Rbitcoin.ct.dict'), read ?ct.dict")),
           ifelse(currency %in% ct.dict[["crypto"]],
                  paste0("X",kraken_xbt_code(currency)),
                  ifelse(currency %in% ct.dict[["fiat"]],
                         paste0("Z",kraken_xbt_code(currency)),
                         stop(paste0("Expected currency: ",currency," does not exists in currency type dictionary, see getOption('Rbitcoin.ct.dict'), read ?ct.dict")))))
  }
  
  # define global kraken

  kraken_api_dict_ticker <- function(market = "kraken", base, quote, action = "ticker"){
    pair = paste0(ct_prefix(base),ct_prefix(quote))
    data.table(market = market, base = base, quote = quote, action = action, 
               method = 'Ticker', url = paste0('https://api.kraken.com/0/public/Ticker?pair=',pair),
               pre_process = c(function(x) x),
               post_process = c(function(x) data.table(market = market, base = base, quote = quote, 
                                                       timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'), 
                                                       market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                                                       last = as.numeric(x[['result']][[pair]][['c']][1]), 
                                                       vwap = as.numeric(x[['result']][[pair]][['p']][2]),
                                                       volume = as.numeric(x[['result']][[pair]][['v']][2]),
                                                       ask = as.numeric(x[['result']][[pair]][['a']][1]),
                                                       bid = as.numeric(x[['result']][[pair]][['b']][1]))),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0) stop(paste0(market,' ticker: market error: ',x[['error']]),call.=FALSE)
                 else if(is.null(x[['result']])) stop(paste0(market," ticker: error not handled by market: `x[['result']]` is NULL object"),call.=FALSE)
                 else x
               }))
  }
  kraken_api_dict_trades <- function(market = "kraken", base, quote, action = "trades"){
    pair = paste0(ct_prefix(base),ct_prefix(quote))
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'Trades', url = paste0('https://api.kraken.com/0/public/Trades?pair=',pair),
               pre_process = c(function(x){
                 if(!is.null(x[['tid']])){
                   assign('url', 
                          paste0(get('url', envir = parent.frame(1)),
                                 '&since=',
                                 as.character(x[['tid']])),
                          envir = parent.frame(1))
                   x['tid'] <- NULL
                 }
                 x
               }),
               post_process = c(function(x){
                 if(length(x[["result"]][[pair]]) == 0){ #exception when no new trades since last visit
                   trades <- data.table(
                     date = as.POSIXct(NA,origin='1970-01-01',tz='UTC')[-1], 
                     price = numeric(), 
                     amount = numeric(), 
                     tid = character(), 
                     type = character()
                   )
                 }
                 else{
                   trades <- as.data.table(x[["result"]][[pair]][,1:4,drop=FALSE])
                   setnames(trades,c("price","amount","date","type"))
                   trades[,`:=`(date = as.POSIXct(as.numeric(date),origin='1970-01-01',tz='UTC'),
                                price = as.numeric(price),
                                amount = as.numeric(amount),
                                tid = NA_character_,
                                type = ifelse(type=='b','bid',ifelse(type=='s','ask',NA_character_)))]
                   setcolorder(trades,c("date","price","amount","tid","type"))
                   trades[nrow(trades), tid := x[['result']][['last']]]
                 }
                 list(market = market, base = base, quote = quote,
                      timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                      market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                      trades = trades)
               }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0) stop(paste0('kraken trades: market error: ',x[['error']]),call.=FALSE)
                 else if(is.null(x[['result']][[pair]])) stop(paste0("kraken trades: error not handled by market: x[['result']][[pair]] a NULL object"),call.=FALSE)
                 else x
               }))
  }
  kraken_api_dict_order_book <- function(market = "kraken", base, quote, action = "order_book"){
    pair = paste0(ct_prefix(base),ct_prefix(quote))
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'Depth', url = paste0('https://api.kraken.com/0/public/Depth?pair=',pair),
               pre_process = c(function(x) x),
               post_process = c(function(x){
                 if(length(x[['result']][[pair]][['asks']]) == 0){ #exception when no new trades since last visit
                   asks <- data.table(
                     price = numeric(), 
                     amount = numeric()
                   )
                 }
                 else{
                   asks <- as.data.table(x[["result"]][[pair]][["asks"]][,1:2])
                   setnames(asks,c("price","amount"))
                   asks[,`:=`(price = as.numeric(price),
                              amount = as.numeric(amount))]
                 }
                 if(length(x[['result']][[pair]][['bids']]) == 0){ #exception when no new trades since last visit
                   asks <- data.table(
                     price = numeric(), 
                     amount = numeric()
                   )
                 }
                 else{
                   bids <- as.data.table(x[["result"]][[pair]][["bids"]][,1:2])
                   setnames(bids,c("price","amount"))
                   bids[,`:=`(price = as.numeric(price),
                              amount = as.numeric(amount))]
                 }
                 list(market = market, base = base, quote = quote,
                      timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                      market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                      asks = asks[,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)],
                      bids = bids[,`:=`(value = price * amount,cum_amount= cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)])
                 }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0) stop(paste0('kraken wallet: market error: ',x[['error']]),call.=FALSE)
                 else if(is.null(x[['result']])) stop(paste0("kraken wallet: error not handled by market: x[['result']] a NULL object"),call.=FALSE)
                 else x
               }))
  }
  kraken_api_dict_wallet <- function(market = "kraken", base = NA_character_, quote = NA_character_, action = "wallet"){
    data.table(market = market, base = base, quote = quote, action = action, 
               method = 'Balance', url = 'https://api.kraken.com/0/private/Balance',
               pre_process = c(function(x) x),
               post_process = c(function(x){
                 if(length(x[['result']]) == 0){
                   list(market = market, 
                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                        market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                        wallet = data.table(currency = character(), amount = numeric()))
                 }
                 else {
                   list(market = market,
                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                        market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                        wallet = data.table(currency = kraken_xbt_code(substring(names(x[['result']]), 2, 4)),
                                            amount = as.numeric(x[['result']])))
                 }
               }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0) stop(paste0(market,' wallet: market error: ',x[['error']]),call.=FALSE)
                 else if(is.null(x[['result']])) stop(paste0(market," wallet: error not handled by market: x[['result']] is NULL object"),call.=FALSE)
                 else x
               }))
  }
  kraken_api_dict_place_limit_order <- function(market = "kraken", base, quote, action = "place_limit_order"){
    pair = paste0(ct_prefix(base),ct_prefix(quote))
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'AddOrder', url = 'https://api.kraken.com/0/private/AddOrder',
               pre_process = c(function(x){
                 list(pair = pair, 
                      type = x[['type']],
                      ordertype = 'limit',
                      price = trunc(x[['price']]*1e5)/1e5, 
                      volume = trunc(x[['amount']]*1e8)/1e8)
               }),
               post_process = c(function(x){
                 data.table(market = market, base = base, quote = quote,
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                            market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                            oid = x[['result']][['txid']],
                            type = as.character(NA),
                            price = as.numeric(NA),
                            amount = as.numeric(NA))
               }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0) stop(paste0('kraken place limit order: market error: ',x[['error']]),call.=FALSE)
                 else if(is.null(x[['result']])) stop(paste0("kraken place limit order: error not handled by market: x[['result']] a NULL object"),call.=FALSE)
                 else x
               }))
  }
  kraken_api_dict_open_orders <- function(market = "kraken", base = NA_character_, quote = NA_character_, action = "open_orders"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'OpenOrders', url = 'https://api.kraken.com/0/private/OpenOrders',
               pre_process = c(function(x) x),
               post_process = c(function(x){
                 if(length(x[['result']][['open']]) == 0){
                   list(market = market,
                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                        market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                        open_orders = data.table(base = as.character(NULL), quote = as.character(NULL), oid = as.character(NULL), type = as.character(NULL), price = as.numeric(NULL), amount = as.numeric(NULL)))
                 }
                 else {
                   orders <- mapply(FUN = function(x, header_oid) data.table(base = kraken_xbt_code(toupper(substr(x[['descr']][['pair']],1,3))),
                                                                             quote = kraken_xbt_code(toupper(substr(x[['descr']][['pair']],4,6))),
                                                                             oid = header_oid,
                                                                             type = as.character(x[['descr']][['type']]),
                                                                             price = as.numeric(x[['descr']][['price']]),
                                                                             amount = as.numeric(x[['vol']])),
                                    x[['result']][['open']],
                                    names(x[['result']][['open']]),
                                    SIMPLIFY = FALSE)
                   list(market = market,
                        timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                        market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                        open_orders = rbindlist(orders))
                 }
               }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0) stop(paste0('kraken open_orders: market error: ',x[['error']]),call.=FALSE)
                 else if(is.null(x[['result']])) stop(paste0("kraken open_orders: error not handled by market: x[['result']] a NULL object"),call.=FALSE)
                 else x
               }))
  }
  kraken_api_dict_cancel_order <- function(market = "kraken", base = NA_character_, quote = NA_character_, action = "cancel_order"){
    data.table(market = market, base = base, quote = quote, action = action,
               method = 'CancelOrder', url = 'https://api.kraken.com/0/private/CancelOrder',
               pre_process = c(function(x) list(txid = x[['oid']])),
               post_process = c(function(x){
                 dt <- data.table(market = market, base = base, quote = quote,
                                  timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                  market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                                  oid = NA_character_)
                 if(length(x[['error']]) > 0 && x[['error']]=="EOrder:Unknown order"){
                   return(dt[0])
                 }
                 if(!(x[['result']][['count']] > 0)) return(dt[0])
                 else dt
               }),
               catch_market_error = c(function(x){
                 if(length(x[['error']]) > 0){
                   if(x[['error']]=="EOrder:Unknown order"){
                     if(is.null(getOption("Rbitcoin.cancel_order.order_not_found"))) return(x) # will silently postprocessed to 0 row DT
                     if(getOption("Rbitcoin.cancel_order.order_not_found")=="warning"){
                       warning("kraken cancel_order was not performed, requested order not found",call. = FALSE)
                       return(x)
                     }
                     if(getOption("Rbitcoin.cancel_order.order_not_found")=="error"){
                       stop(paste0('kraken cancel_order was not performed: ',x[['error']]),call.=FALSE)
                     }
                   }
                   stop(paste0('kraken cancel_order: market error: ',x[['error']]),call.=FALSE)
                 }
                 else if(is.null(x[['result']][['count']])) stop(paste0("kraken cancel_order: error not handled by market: missing result$count object"),call.=FALSE)
                 else x
               }))
  }
  
  # generate dictionary
  api.dict.list <- list()
  
  # common for all currency pairs
  
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_wallet()
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_open_orders()
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_cancel_order()
  
  # currency specific
  
  base = 'BTC'; quote = 'EUR'
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'LTC'; quote = 'EUR'
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_place_limit_order(base = base, quote = quote)
  
  base = 'BTC'; quote = 'LTC'
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_ticker(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_trades(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_order_book(base = base, quote = quote)
  api.dict.list[[length(api.dict.list)+1]] <- kraken_api_dict_place_limit_order(base = base, quote = quote)
  
  unique(setkeyv(rbindlist(api.dict.list),c("market","base","quote","action")))
}
