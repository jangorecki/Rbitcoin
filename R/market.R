
# market.api.process -----------------------------------------------------------

#' @title Process market API
#'
#' @description Unified processing of API call according to API dictionary \code{api.dict}. Limited to markets and currency processing defined in \code{api.dict}, in case of currency pairs and methods not availble in dictionary use \code{market.api.query} directly. This function perform pre processing of request and post processing of API call response to unified structure across markets.
#' 
#' @param market character, example: \code{'kraken'}.
#' @param currency_pair character vector of length 2, e.g. \code{c(base = 'BTC', quote = 'EUR')}, names are not mandatory but order does matter. It will also handle the \code{action} param provided in case of \code{market.api.process("kraken","wallet")}.
#' @param action character, defined process/method to get organized data.
#' \itemize{
#' \item \code{'ticker'} returns \code{data.table} ticker information.
#' \item \code{'wallet'} returns \code{list} wallet information like \code{currency}, \code{amount}.
#' \item \code{'order_book'} returns \code{list} with API call level attributes and sub elements \code{[['asks']]} and \code{[['bids']]} as \code{data.table} objects with order book including already calculated cumulative \code{amount}, \code{price} and \code{value}.
#' \item \code{'open_orders'} returns \code{list} open orders information like \code{oid}, \code{type}, \code{price}, \code{amount}.
#' \item \code{'place_limit_order'} returns \code{data.table} with fields: \code{oid}, \code{type}, \code{price}, \code{amount}.
#' \item \code{'cancel_order'} returns \code{data.table} with fields like \code{oid}.
#' \item \code{'trades'} returns \code{list} with API call level attributes and sub element \code{[['trades']]} as \code{data.table} (ASC order) with fields: \code{date}, \code{price}, \code{amount}, \code{tid}, \code{type}.
#' }
#' @param req list of action details (price, amount, tid, oid, etc.) unified across the markets specific per action, see examples.
#' @param \dots objects to be passed to \code{market.api.query} and farther to particular market query (read \code{query.dict}).
#' \itemize{
#' \item auth params: \code{key}, \code{secret}
#' \item auth param on bitstamp: \code{client_id}
#' \item any other args in case of own custom market function
#' }
#' @param skip_post_process logical skip post-processing and return results only after \code{fromJSON} processing. Useful in case of change response structure from market API. It can always be manually post-processed on user side as a workaround till the Rbitcoin api dict update.
#' @param api.dict data.table user custom API dictionary definition, if not provided function will use default Rbitcoin \code{getOption("Rbitcoin.api.dict")}.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @details By default it will perform antiddos check and wait if required, it can be turned off but in such case you should expect to be banned quite easily. Read \code{?antiddos}.
#' @return
#' Unless \code{skip_post_process==TRUE} the returned value depends on the \code{action} param but does not depend on \code{market} anymore.
#' It returns a list or data.table.
#' It will also result truncation of most (not common across the markets) attributes returned. If you need the full set of data returned by market's API you might use \code{skip_post_process=TRUE}.
#' All actions will return API call response but also metadata about API call itself, in a common structure across different markets.
#' Follow Rbitcoin introduction vignette or examples.
#' @note The api dictionary was not fully tested, if you find any bugs please report. Use only api dictionaries from trusted source or review them before using!
#' @seealso \code{\link{market.api.query}}, \code{\link{api.dict}}, \code{\link{antiddos}}, \code{\link{query.dict}}
#' @export
#' @examples
#' \dontrun{
#' # get ticker from market
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action='ticker')
#' 
#' # get ticker from few markets and combine
#' op <- options("Rbitcoin.antiddos.verbose" = 1) # will print antiddos waiting time
#' ticker_all <- rbindlist(list(
#'   market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action='ticker'),
#'   market.api.process(market = 'btce', currency_pair = c('LTC', 'USD'), action='ticker'),
#'   market.api.process(market = 'btce', currency_pair = c('LTC', 'BTC'), action='ticker'),
#'   market.api.process(market = 'kraken', currency_pair = c('BTC','EUR'), action='ticker'),
#'   market.api.process(market = 'kraken', currency_pair = c('LTC','EUR'), action='ticker'),
#'   market.api.process(market = 'kraken', currency_pair = c('BTC','LTC'), action='ticker')
#' ))
#' options(op)
#' print(ticker_all)
#' 
#' # get wallet from market
#' market.api.process(market = 'kraken', action = 'wallet', key = '', secret = '')
#' 
#' # get wallet from all markets and combine
#' wallet_all <- rbindlist(list(
#'   market.api.process(market = 'bitstamp', action = 'wallet',
#'                      client_id = '', key = '', secret = ''),
#'   market.api.process(market = 'btce', action = 'wallet',
#'                      method = '', key = '', secret = ''),
#'   market.api.process(market = 'kraken', action = 'wallet',
#'                      key = '', secret = '')
#' ))
#' print(wallet_all)
#' 
#' # get order book from market
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'order_book')
#' 
#' # get open orders from market
#' market.api.process(market = 'kraken', action = 'open_orders', key = '', secret = '')
#' 
#' # place limit order
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'place_limit_order',
#'                    req = list(type = 'sell', amount = 1, price = 8000), # sell 1 btc for 8000 eur
#'                    key = '', secret = '')
#' 
#' # cancel order
#' market.api.process(market = 'kraken', action = 'cancel_order', 
#'                    req = list(oid = 'oid_from_open_orders'),
#'                    key = '', secret = '')
#' 
#' # get trades, since arg allowed: `req = list(tid = "123456")`
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'trades')
#' }
market.api.process <- function(market, currency_pair, action, req = list(), ..., 
                               skip_post_process = FALSE,
                               api.dict = getOption("Rbitcoin.api.dict",stop("no api.dict in options! options('Rbitcoin.api.dict')")),
                               verbose = getOption("Rbitcoin.verbose",0)){
  # format input
  if(!all.equal(key(api.dict),c("market","base","quote","action"))) setkeyv(api.dict,c("market","base","quote","action"))
  if(missing(currency_pair)) currency_pair <- c(NA_character_,NA_character_)
  if(is.null(currency_pair)) currency_pair <- c(NA_character_,NA_character_)
  if(length(currency_pair)==1){
    # remap `action` provided as second arg (currency pair) - in case if anybody use market.api.process("kraken","wallet")
    if(currency_pair %in% c("wallet","open_orders","cancel_order") & missing(action)){
      action <- currency_pair
      currency_pair <- c(NA_character_,NA_character_)
    }
    else stop("Invalid currency_pair arg, read ?market.api.process")
  }
  # process
  api.dict.filter <- bquote(J(.(market),.(currency_pair[[1]]),.(currency_pair[[2]]),.(action)))
  api.dict.local <- api.dict[eval(api.dict.filter),nomatch=0]
  if(nrow(api.dict.local)<1) stop(paste0('Missing api.dict data for particular set: ',market,', ',currency_pair[[1]],', ',currency_pair[[2]],', ',action,". Extend api.dict."))
  if(nrow(api.dict.local)>1) stop(paste0('Multiple api.dict data for particular set: ',market,', ',currency_pair[[1]],', ',currency_pair[[2]],', ',action,". Clean api.dict."))
  url = api.dict.local$url # can be later updated by pre_process function
  # pre-process req for market
  req <- api.dict.local$pre_process[[1]](req)
  res <- market.api.query(market = market, url = url, req = req, ...,  verbose = verbose - 1)
  if(skip_post_process){
    if(verbose > 0) cat(as.character(Sys.time()),': market.api.process: skip_post_process=TRUE, returning raw object just after fromJSON for ',market,' ',action,'\n',sep='')
    return(res)
  }
  # catch market error
  res <- api.dict.local$catch_market_error[[1]](res)
  # post-process res from market
  res <- api.dict.local$post_process[[1]](res)
  if(verbose > 0) cat(as.character(Sys.time()),': market.api.process: api call processed finished for ',market,' ',action,'\n',sep='')
  return(res)
}

# market.api.query -----------------------------------------------------

#' @title Send request to market API
#'
#' @description Route a request to particular market function according to dictionary \code{query.dict}.
#'
#' @param market character which identifies market on which we want to send request, e.g. kraken.
#' @param url character url on which launch api call.
#' @param \dots objects to be passed to api: \code{key}, \code{secret}, \code{req}, \code{client_id} (used on bitstamp).
#' @param antiddos logical default \code{TRUE}. Default to 10s. To customize read \code{antiddos}.
#' @param query.dict data.table default built-in dictionary with market query functions, read \code{query.dict}.
#' @param json.debug logical default \code{FALSE}. Purely technical, if set to \code{TRUE} it will allow to debug case of unknown non-json format returned by market, by saving market response in working directory as timestamped txt file in case of \code{fromJSON} error.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return R object created by \code{fromJSON} decoded response from market's API call.
#' @seealso \code{\link{market.api.process}}, \code{\link{antiddos}}, \code{\link{query.dict}}
#' @references API documentation: \url{https://www.bitstamp.net/api/}, \url{https://btc-e.com/api/documentation}, \url{https://www.kraken.com/help/api}, \url{https://www.bitmarket.pl/docs.php?file=api_private.html}, \url{https://github.com/hitbtc-com/hitbtc-api}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query(market = 'bitstamp', 
#'                  url = 'https://www.bitstamp.net/api/ticker/')
#' market.api.query(market = 'btce', 
#'                  url = 'https://btc-e.com/api/3/ticker/ltc_usd')
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/public/Ticker?pair=XXBTZEUR')
#' market.api.query(market = 'bitmarket',
#'                  url = 'https://www.bitmarket.pl/json/LTCPLN/ticker.json')
#' market.api.query(market = 'hitbtc',
#'                  url = 'https://api.hitbtc.com/api/1/public/BTCUSD/ticker')
#' # wallet
#' market.api.query(market = 'bitstamp', 
#'                  url = 'https://www.bitstamp.net/api/balance/', 
#'                  client_id = '', # bitstamp specific
#'                  key = '', secret = '')
#' market.api.query(market = 'btce', 
#'                  url = 'https://btc-e.com/tapi', 
#'                  req = list(method = 'getInfo'), 
#'                  key = '', secret = '')
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/private/Balance', 
#'                  key = '', secret = '')
#' market.api.query(market = 'bitmarket',
#'                  url = 'https://www.bitmarket.pl/api2/',
#'                  req = list(method = 'info'),
#'                  key = '', secret = '')
#' market.api.query(market = 'hitbtc',
#'                  url = "https://api.hitbtc.com/api/1/trading/balance",
#'                  key = '', secret = '')
#' # order book
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/public/Depth?pair=XXBTZEUR')
#' # open orders
#' market.api.query(market = 'kraken',
#'                  url = 'https://api.kraken.com/0/private/OpenOrders', 
#'                  key = '', secret = '')
#' # place order
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/private/AddOrder', 
#'                  key = '', secret = '',
#'                  req = list(pair = 'XXBTZEUR',
#'                             type = 'sell',
#'                             ordertype = 'limit',
#'                             price = 1200, # 1200 eur
#'                             volume = 0.1)) # 0.1 btc
#' # cancel order
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/private/CancelOrder', 
#'                  key = '', secret = '',
#'                  req = list(txid = 'id_from_open_orders'))
#' # trades
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/public/Trades?pair=XXBTZEUR')
#' }
market.api.query <- function(market, url, ...,
                             antiddos = getOption("Rbitcoin.antiddos",TRUE),
                             query.dict = getOption("Rbitcoin.query.dict",stop("query.dict is mandatory for redirection to market query function")),
                             json.debug = getOption("Rbitcoin.json.debug",FALSE),
                             verbose = getOption("Rbitcoin.verbose",0)){
  if(length(url) > 1) stop("market.api.query: `url` argument should be length one character vector")
  # antiddos
  wait <- if(antiddos) getOption("Rbitcoin.antiddos.fun",antiddos_fun)(source_system = market, verbose = verbose - 1) else 0
  # redirection to market function
  json_res <- query.dict[market,query][[1]](url = url, ..., verbose = verbose - 1)
  # decode from json
  if(json.debug){
    tryCatch(query_res <- fromJSON(json_res),
             error = function(e){
               invisible(writeLines(json_res, paste0("Rbitcoin_json_debug_",market,"_",as.character(Sys.time(),"%Y%m%d%H%M%S"),".txt")))
               stop(e[["message"]], call. = FALSE)
             })
  }
  else{
    query_res <- fromJSON(json_res)
  }
  if(verbose > 0) cat(as.character(Sys.time()),': market.api.query: query completed for ',market,', antiddos wait was ',round(wait,2),'s','\n',sep='')
  return(query_res)
}
