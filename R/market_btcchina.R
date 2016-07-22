
# market.api.query.btcchina ----------------------------------------------

#' @title Send request to btcchina market API
#'
#' @description Send request to btcchina market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return Character string a response from markets API call.
#' @note Market specific btcchina \code{method} param should be provided in \code{req} object.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://www.btcc.com/apidocs/spot-exchange-market-data-rest-api} \url{https://www.btcc.com/apidocs/spot-exchange-trade-json-rpc-api}
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.btcchina(url = 'https://data.btcchina.com/data/ticker?market=btccny')
#' # wallet
#' market.api.query.btcchina(url = 'https://api.btcc.com/api_trade_v1.php',
#'                           req = list(method = 'getAccountInfo', id=1),
#'                           key = '', secret = '')
#' }
market.api.query.btcchina <- function(url, key, secret, req = list(), 
                                      verbose = getOption("Rbitcoin.verbose",0)){ 
    if(length(req) > 0 | (!missing(key) & !missing(secret))){
        if (is.null(req[['id']])) req[['id']] = 1
        tonce <- as.character(as.numeric(Sys.time()) * 1000000)
        to_post <- list("method"=req[['method']],
                        "params"=I(unname(req[['params']])),
                        "id"=req[["id"]])
        post_data <- toJSON(to_post, auto_unbox = TRUE)
        if(!missing(key) & !missing(secret)){
            sign <- sprintf('tonce=%s&accesskey=%s&requestmethod=%s&id=%s&method=%s&params=%s',
                            tonce,
                            key,
                            "post",
                            req[['id']],
                            req[['method']],
                            zz<-paste(lapply(req[['params']], function(x) if(is.logical(x)) tolower(x) else x), collapse=','))
            sign_hash <- hmac(key = secret, object = sign, algo = 'sha1')
            httpheader <- c("Authorization" = paste("Basic", base64Encode(paste(key,sign_hash,sep=":"))),
                            "Json-Rpc-Tonce" = tonce)
        }
        message(post_data)
        message(zz)
    }
    curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
    if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
    else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                   postfields = post_data, httpheader = httpheader))
    else stop(paste0('unhandled case on RCurl functions calling in market.api.query.btcchina'), call. = FALSE)
    if(verbose > 0) cat(as.character(Sys.time()),': market.api.query.btcchina: api call performed on: ',url,'\n',sep='')
    return(query_result_json)
}

# btcchina_api_dict ----------------------------------------------------------------

btcchina_api_dict <- function(){
    
    btcchina_lot_dict <- function(i){
        # lot.dict.generator - use for update lot.dict
        lot.dict = data.table(
            symbol = c("BTCCNY", "LTCCNY", "LTCBTC"),
            step = c(0.01, 0.01, 0.0001)
            , key = "symbol"
        )
        lot.dict.filtered = lot.dict[i,nomatch=NA]
        if(any(lot.dict.filtered[,is.na(step)])) stop("btcchina lot dictionary is outdated, update btcchina_lot_dict function in market_btcchina.R, report and/or pull request fix to git repo")
        lot.dict.filtered
    }
    
    # define global btcchina
    btcchina_api_dict_ticker <- function(market = "btcchina", base, quote, action = "ticker"){
        data.table(market = market, base = base, quote = quote, action = action,
                   method = 'ticker', url = sprintf('https://data.btcchina.com/data/ticker?market=%s',tolower(paste(c(base,quote),collapse=""))),
                   pre_process = c(function(x) x),
                   post_process = c(function(x){
                       data.table(market = market, base = base, quote = quote, 
                                  timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'), 
                                  market_timestamp = as.POSIXct(x[['ticker']][["date"]], origin = '1970-01-01', tz = 'UTC'), 
                                  last = x[['ticker']][['last']], 
                                  vwap = x[['ticker']][['vwap']],
                                  volume = x[['ticker']][['vol']],
                                  ask = x[['ticker']][['sell']],
                                  bid = x[['ticker']][['buy']])
                   }),
                   catch_market_error = c(function(x){
                       if(is.null(x[['ticker']][['last']])) stop(paste0("btcchina ticker error not handled by market: key field is NULL"),call.=FALSE)
                       else x
                   }))
    }
    btcchina_api_dict_trades <- function(market = "btcchina", base, quote, action = "trades"){
        data.table(market = market, base = base, quote = quote, action = action,
                   method = 'trade_history', url = sprintf('https://data.btcchina.com/data/historydata?market=%s',tolower(paste(c(base,quote),collapse=""))),
                   pre_process = c(function(x){
                       if(!is.null(x[['tid']])){
                           if (!is.character(x[['tid']])) x[['tid']] = as.character(x[['tid']])
                           if (identical(x[['tid']], "0")) x[['tid']] = "1" # btcchina dont like 0 here, starts from 2 on btccny anyway
                           assign('url', sprintf("%s&since=%s&limit=1000&sincetype=id", get('url',envir=parent.frame(1)), as.character(x[['tid']])), envir=parent.frame(1))
                           x['tid'] <- NULL
                       }
                       x
                   }),
                   post_process = c(function(x){
                       list(market = market, base = base, quote = quote,
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                            market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'),
                            trades = {
                                if(nrow(x)==0) data.table(date = as.POSIXct(NA,origin='1970-01-01',tz='UTC')[-1], 
                                                            price = numeric(), 
                                                            amount = numeric(), 
                                                            tid = character(), 
                                                            type = character())
                                else setDT(x)[,`:=`(date = as.POSIXct(as.numeric(date), origin='1970-01-01', tz='UTC'),
                                                            tid = as.character(tid),
                                                            type = ifelse(type=="buy", "bid", "ask"))
                                                      ][,list(date,price,amount,tid,type)]
                            })
                   }),
                   catch_market_error = c(function(x){
                       if(is.null(x)) stop(paste0('btcchina trades error not handled by market: key field is NULL'),call.=FALSE)
                       else x
                   }))
    }
    btcchina_api_dict_order_book <- function(market = "btcchina", base, quote, action = "order_book"){
        data.table(market = market, base = base, quote = quote, action = action,
                   method = 'order_book', url = sprintf('https://data.btcchina.com/data/orderbook?market=%s',tolower(paste(c(base,quote),collapse=""))),
                   pre_process = c(function(x) x),
                   post_process = c(function(x){
                       list(market = market, base = base, quote = quote, 
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                            market_timestamp = as.POSIXct(x[["date"]], origin = '1970-01-01', tz = 'UTC'),
                            asks = setnames(as.data.table(x[["asks"]]),c("price","amount"))[.N:0][,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)],
                            bids = setnames(as.data.table(x[["bids"]]),c("price","amount"))[,`:=`(value = price * amount,cum_amount = cumsum(amount))][,`:=`(cum_value = cumsum(value),avg_price = cumsum(value) / cum_amount)])
                   }),
                   catch_market_error = c(function(x){
                       if(is.null(x[['asks']]) & is.null(x[['bids']])) stop(paste0("btcchina order_book error not handled by market: key field is NULL"),call.=FALSE)
                       else x
                   }))
    }
    btcchina_api_dict_wallet <- function(market = "btcchina", base = NA_character_, quote = NA_character_, action = "wallet"){
        data.table(market = market, base = base, quote = quote, action = action,
                   method = 'balance', url = 'https://api.btcc.com/api_trade_v1.php',
                   pre_process = c(function(x) {
                       x[['method']] <- 'getAccountInfo'
                       if (is.null(x[["id"]])) x[["id"]] = as.character(as.integer(Sys.time()))
                       x
                   }),
                   post_process = c(function(x) {
                       list(market = market,
                            timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                            market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                            wallet = rbind(
                                rbindlist(lapply(x[["result"]][["balance"]], `[`, c("currency","amount"))),
                                rbindlist(lapply(x[["result"]][["frozen"]], `[`, c("currency","amount")))
                            )[, .(amount=sum(as.numeric(amount))), currency])
                   }),
                   catch_market_error = c(function(x){
                        if(is.null(x[['result']])) stop(paste0("btcchina wallet error not handled by market: key field is NULL"),call.=FALSE)
                       else x
                   }))
    }
    btcchina_api_dict_place_limit_order <- function(market = "btcchina", base, quote, action = "place_limit_order"){
        data.table(market = market, base = base, quote = quote, action = action,
                   method = 'buyOrder2_sellOrder2', url = 'https://api.btcc.com/api_trade_v1.php',
                   pre_process = c(function(x){
                       pair = toupper(paste(c(base,quote),collapse=""))
                       price = trunc(x[['price']]*btcchina_lot_dict(pair)[,1/step])/btcchina_lot_dict(pair)[,1/step]
                       amount = trunc(x[['amount']]*1e3)/1e3
                       if (is.null(x[["id"]])) x[["id"]] = as.character(as.integer(Sys.time()))
                       if (! amount > 0) stop(sprintf("Cannot place order for 0 amount, btcchina amount rounded to 3 decimal places, attempted to trade for %s amount.", x[['amount']]))
                       list(method = if (x[['type']]=="buy") 'buyOrder2' else if (x[['type']]=="sell") 'sellOrder2' else stop("Unknown type of order, must be 'buy' or 'sell'"),
                            params = list(price, amount, pair),
                            id = x[["id"]])
                   }),
                   post_process = c(function(x){
                       data.table(market = market, base = base, quote = quote,
                                  timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                  market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                                  oid = as.character(x[['result']]),
                                  type = NA_character_,
                                  price = NA_real_,
                                  amount = NA_real_)
                   }),
                   catch_market_error = c(function(x){
                       if(is.null(x[['result']])) stop(paste0("btcchina place_limit_order error not handled by market: key field is NULL"),call.=FALSE)
                       else x
                   }))
    }
    btcchina_api_dict_open_orders <- function(market = "btcchina", base = NA_character_, quote = NA_character_, action = "open_orders"){
        data.table(market = market, base = base, quote = quote, action = action,
                   method = 'getOrders', url = 'https://api.btcc.com/api_trade_v1.php',
                   pre_process = c(function(x) {
                       if (is.null(x[["id"]])) x[["id"]] = as.character(as.integer(Sys.time()))
                       list(method = "getOrders",
                            params = list(TRUE, "ALL"),
                            id = x[["id"]])
                   }),
                   post_process = c(function(x){
                       market.date = x[["result"]][["date"]]
                       x[["result"]]["date"] = NULL
                       dt = rbindlist(x[["result"]], idcol = "currency_pair")
                       if(nrow(dt) == 0){
                           list(market = market,
                                timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'), 
                                market_timestamp = as.POSIXct(market.date, origin = '1970-01-01', tz = 'UTC'),
                                open_orders = data.table(base = character(), quote = character(), 
                                                         oid = character(), type = character(), price = numeric(), amount = numeric()))
                       } else {
                           list(market = market,
                                timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                market_timestamp = as.POSIXct(market.date, origin = '1970-01-01', tz = 'UTC'),
                                open_orders = dt[, .(base=toupper(substr(currency_pair, 7, 9)), quote=toupper(substr(currency_pair, 10, 12)),
                                                     oid=as.character(id), type=as.character(type),
                                                     price=as.numeric(price), amount=as.numeric(amount))])
                       }
                   }),
                   catch_market_error = c(function(x){
                       if(is.null(x[['result']])) stop(paste0("btcchina open_orders error not handled by market: key field is NULL"),call.=FALSE)
                       else x
                   }))
    }
    btcchina_api_dict_cancel_order <- function(market = "btcchina", base = NA_character_, quote = NA_character_, action = "cancel_order"){
        data.table(market = market, base = base, quote = quote, action = action,
                   method = 'cancelOrder', url = 'https://api.btcc.com/api_trade_v1.php',
                   pre_process = c(function(x) {
                       if (is.null(x[["id"]])) x[["id"]] = as.character(as.integer(Sys.time()))
                       if (!"oid" %in% names(x)) stop("No order id 'oid' provided to 'req' argument in cancel_order for btcchina")
                       if (is.character(x[["oid"]])) x[["oid"]] = as.numeric(x[["oid"]])
                       list(method = "cancelOrder",
                            params = unname(x[intersect(c('oid','market'), names(x))]),
                            id = x[["id"]])
                   }),
                   post_process = c(function(x){
                       data.table(market = market, base = NA_character_, quote = NA_character_,
                                  timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'),
                                  market_timestamp = as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'), 
                                  oid = NA_character_)
                   }),
                   catch_market_error = c(function(x){
                       if(!isTRUE(x[['result']])) stop(sprintf("btcchina cancel_order did not return true, error message: %s", toString(x[["error"]][["message"]])),call.=FALSE)
                       x
                   }))
    }
    
    # generate dictionary
    api.dict.list <- list()
    
    # common for all currency pairs
    
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_wallet()
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_open_orders()
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_cancel_order()
    
    # currency specific
    
    base = 'BTC'; quote = 'CNY'
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_ticker(base = base, quote = quote)
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_trades(base = base, quote = quote)
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_order_book(base = base, quote = quote)
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_place_limit_order(base = base, quote = quote)
    
    base = 'LTC'; quote = 'CNY'
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_ticker(base = base, quote = quote)
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_trades(base = base, quote = quote)
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_order_book(base = base, quote = quote)
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_place_limit_order(base = base, quote = quote)
    
    base = 'LTC'; quote = 'BTC'
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_ticker(base = base, quote = quote)
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_trades(base = base, quote = quote)
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_order_book(base = base, quote = quote)
    api.dict.list[[length(api.dict.list)+1]] <- btcchina_api_dict_place_limit_order(base = base, quote = quote)
    
    unique(setkeyv(rbindlist(api.dict.list),c("market","base","quote","action")), by=c("market","base","quote","action"))
}
