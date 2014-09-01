
# Rbitcoin - package level data ------------------------------------------------------------------

#' @title R & bitcoin integration
#'
#' @description Utilities related to Bitcoin and other cryptocurrencies. Core functionalities are:
#' \itemize{
#' \item \code{market.api.query} - launch query on market's API (\code{bitstamp}, \code{btce}, \code{kraken}, \code{bitmarket}). Both public and private API calls supported. All currency pairs supported.
#' \item \code{market.api.process} - integration of market's processing structures: pre-process of API request, post-process API results, market error catching. Input and output unified structure. Requires API dictionary definition, for details of package built-in dictionary see \code{\link{api.dict}}.
#' \item \code{blockchain.api.query} - launch query on blockchain.info API json interface.
#' \item \code{blockchain.api.process} - postprocess blockchain api result, transform to \code{data.table}.
#' \item \code{Rbitcoin.plot} - illustrate the data returned by some Rbitcoin functions.
#' \item \code{wallet_manager} - track the assets amounts and values in multiple wallet sources.
#' }
#' You need to note that imported \code{digest} package docs states: \emph{Please note that this package is not meant to be deployed for cryptographic purposes for which more comprehensive (and widely tested) libraries such as OpenSSL should be used}. Still \code{digest} is one of the top downloaded package from CRAN.\cr
#' To do not get banned by market's API anti-DDoS protection user should use: \code{Sys.sleep(10)} between the API calls or \code{\link{antiddos}} function.\cr
#' It is advised to maintain your API keys security level as tight as possible, if you do not need withdraw api method be sure to disable it for api keys.\cr
#' You can print debug messages of \code{Rbitcoin} to console using verbose argument in FUNs or \code{options("Rbitcoin.verbose" = 1)}.\cr
#' Two params \code{ssl.verify} and \code{curl.verbose} have been deprecated since \code{0.8.5}. They can and should be controlled using \code{options("RCurlOptions")}. SSL verify is by default active.\cr
#' At the time of writing the most recent market's API version were used:
#' \itemize{
#' \item bitstamp v2 (public) / ? (private)
#' \item btce v2 (public) / "tapi" (private)
#' \item kraken v0
#' \item bitmarket v2
#' \item mtgox v2 (market already closed)
#' }
#' 
#' SSL is by default active, to disable SSL set \code{RCurlOptions} to \code{ssl.verify* = FALSE} and \code{cainfo = NULL}, see examples.
#' In case of SSL error try update certificate CA file (\code{cacert.pem} in location mentioned below as \code{cainfo}), see references for CA file source. Alternatively you can always disable SSL.
#' 
#' For others package-level options see examples.
#' 
#' BTC donation: \url{bitcoin:15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi}
#' 
#' @seealso \code{\link{market.api.process}}, \code{\link{blockchain.api.process}}, \code{\link{wallet_manager}}, \code{\link{Rbitcoin.plot}}, \code{\link{api.dict}}, \code{\link{available_wallet}}
#' @references Package discussion thread: \url{https://bitcointalk.org/index.php?topic=343504}\cr Example SSL CA file source: \url{http://curl.haxx.se/docs/caextract.html}
#' @docType package
#' @import RJSONIO RCurl digest data.table
#' @name Rbitcoin
#' @aliases bitcoin btc BTC
#' @examples
#' \dontrun{
#' # default options used by Rbitcoin
#' 
#' # print Rbitcoin processing to console set "Rbitcoin.verbose" to 1 (or more)
#' options(Rbitcoin.verbose=0)
#' 
#' # print Rcurl processing to console set RCurlOptions[["verbose"]] to TRUE
#' options(RCurlOptions=list(ssl.verifypeer = TRUE,
#'                           ssl.verifyhost = TRUE,
#'                           cainfo = system.file("CurlSSL","cacert.pem",package="RCurl"),
#'                           verbose = FALSE))
#' 
#' # currency type dictionary used by wallet_manager
#' options(Rbitcoin.ct.dict = list(
#'   crypto = c('BTC','LTC','NMC', ...),
#'   fiat = c('USD','EUR','GBP', ...)
#' ))
#' }
NULL

# market.api.process -----------------------------------------------------------

#' @name api.dict
#' @title API dictionary
#' @description This data set contains dictionary (\code{\link{data.table}} object) for \code{\link{market.api.process}} function which perform pre-process API call request, post-process API call results and catch market level errors. Still there is function \code{\link{market.api.query}} that do not require any dictionary and can operate on any currency pairs. Run \code{data(api.dict); api.dict} to print built-in dictionary. Granularity of data is \code{c(market, base, quote, action)}. This dictionary can be edited/extended by user for new currency pairs.\cr Currently supported currency pairs:
#' \itemize{
#' \item \code{bitstamp: BTCUSD}
#' \item \code{btce: BTCUSD, LTCUSD, LTCBTC, NMCBTC}
#' \item \code{kraken: BTCEUR, LTCEUR, BTCLTC}
#' \item \code{bitmarket: BTCPLN, LTCPLN}
#' \item \code{mtgox: BTCUSD}
#' }
#' @usage data(api.dict)
#' @note Do not use \code{api.dict} from untrusted source or read whole it's code to ensure it is safe! The api dictionary was not fully tested, please follow the examples, if you find any bugs please report.
#' @docType data
#' @author Jan Gorecki, 2014-08-13
#' @keywords datasets
NULL

#' @title Process market API
#'
#' @description Unified processing of API call according to API dictionary \code{\link{api.dict}}. Limited to markets and currency processing defined in \code{api.dict}, in case of currency pairs and methods not availble in dictionary use \code{\link{market.api.query}} directly. This function perform pre processing of request and post processing of API call results to unified structure across markets. It will result truncation of most (not common across the markets) attributes returned. If you need the full set of data returned by market's API you should use \code{\link{market.api.query}}.
#' 
#' @param market character, example: \code{'kraken'}.
#' @param currency_pair character vector of length 2, ex. \code{c(base = 'BTC', quote = 'EUR')}. Order does matter.
#' @param action character, defined process to get organized data.
#' @param req list with action details (price, amount, tid, oid, etc.) unified across the markets specific per action, see examples.
#' @param \dots objects to be passed to \code{\link{market.api.query}}
#' \itemize{
#' \item auth params: \code{key}, \code{secret}, \code{client_id} (last one used on bitstamp),
#' }
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @param on.market.error expression to be evaluated on market level error. Rules specified in \code{\link{api.dict}}.
#' @param on.error expression to be evaluated on R level error related to \code{market.api.query}. For details read \code{\link{market.api.query}}.
#' @param api.dict data.table user custom API dictionary definition, if not provided function will use default Rbitcoin \code{\link{api.dict}}.
#' @param raw.query.res logical skip post-processing are return results only after \code{fromJSON} processing. Useful in case of change results structure from market API. It can always be manually post-processed as a workaround till the Rbitcoin update.
#' @details To do not spam market's API, use \code{Sys.sleep(10)} between API calls.
#' @return Returned value depends on the \code{action} param. All actions will return market, currency pair (except \code{wallet} and \code{open_orders} which returns all currencies), R timestamp, market timestamp and below data (in case if market not provide particular data, it will result \code{NA} value):
#' \itemize{
#' \item \code{'ticker'} returns \code{data.table} with fields: \code{last}, \code{vwap}, \code{volume}, \code{ask}, \code{bid}.
#' \item \code{'wallet'} returns \code{data.table} with fields: \code{currency}, \code{amount}, \code{fee}.
#' \item \code{'order_book'} returns \code{list} with API call level attributes and sub elements \code{[['asks']]} and \code{[['bids']]} as \code{data.table} objects with order book including already calculated cumulative \code{amount}, \code{price} and \code{value}.
#' \item \code{'open_orders'} returns \code{data.table} with fields: \code{oid}, \code{type}, \code{price}, \code{amount}.
#' \item \code{'place_limit_order'} returns \code{data.table} with fields: \code{oid}, \code{type}, \code{price}, \code{amount}.
#' \item \code{'cancel_order'} returns \code{data.table} with fields: \code{oid}.
#' \item \code{'trades'} returns \code{list} with API call level attributes and sub element \code{[['trades']]} as \code{data.table} (ASC order) with fields: \code{date}, \code{price}, \code{amount}, \code{tid}, \code{type}.
#' }
#' @note The api dictionary was not fully tested, please follow the examples, if you find any bugs please report. Use only api dictionary \code{\link{api.dict}} from trusted source, in case if you use other \code{api.dict} it is advised to review pre-process, post-process and catch_market_error functions for markets and currency pairs you are going to use. Market level error handling might not fully work as not all markets returns API call status information.
#' @seealso \code{\link{market.api.query}}
#' @export
#' @examples
#' \dontrun{
#' # get ticker from market
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action='ticker')
#' # get ticker from all markets and combine
#' ticker_all <- rbindlist(list(
#'   market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action='ticker')
#'   ,market.api.process(market = 'btce', currency_pair = c('LTC', 'USD'), action='ticker')
#'   ,{Sys.sleep(10);
#'     market.api.process(market = 'btce', currency_pair = c('LTC', 'BTC'), action='ticker')}
#'   ,{Sys.sleep(10);
#'     market.api.process(market = 'btce', currency_pair = c('NMC', 'BTC'), action='ticker')}
#'   ,market.api.process(market = 'kraken', currency_pair = c('BTC','EUR'), action='ticker')
#'   ,{Sys.sleep(10);
#'     market.api.process(market = 'kraken', currency_pair = c('LTC','EUR'), action='ticker')}
#'   ,{Sys.sleep(10);
#'     market.api.process(market = 'kraken', currency_pair = c('BTC','LTC'), action='ticker')}
#' ))
#' print(ticker_all)
#' 
#' # get wallet from market
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'wallet', 
#'                    key = '', secret = '')
#' # get wallet from all markets and combine
#' wallet_all <- rbindlist(list(
#'   market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action = 'wallet',
#'                      client_id = '', key = '', secret = ''),
#'   market.api.process(market = 'btce', currency_pair = c('LTC', 'USD'), action = 'wallet',
#'                      method = '', key = '', secret = ''),
#'   market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'wallet',
#'                      key = '', secret = '')
#' ))
#' print(wallet_all)
#' 
#' # get order book from market
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'order_book')
#' 
#' # get open orders from market
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'open_orders', 
#'                    key = '', secret = '')
#' 
#' # place limit order
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'place_limit_order',
#'                    req = list(type = 'sell', amount = 1, price = 8000), # sell 1 btc for 8000 eur
#'                    key = '', secret = '')
#' 
#' # cancel order
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'cancel_order, 
#'                    req = list(oid = 'oid_from_open_orders'),
#'                    key = '', secret = '')
#' # get trades
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'trades')
#' }
market.api.process <- function(market, currency_pair, action, req = list(), ..., verbose = getOption("Rbitcoin.verbose",0), on.market.error = expression(stop(e[['message']], call. = FALSE)), on.error = expression(stop(e[['message']], call. = FALSE)), api.dict = NULL, raw.query.res = FALSE){
  fun_name <- 'market.api.process'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing started for ',market,' ',action,sep='')
  #R package check warning NOTE prevention:
  pre_process <- NULL; post_process <- NULL; catch_market_error <- NULL; base <- NULL; quote <- NULL
  if(is.null(api.dict)) data("api.dict", package = "Rbitcoin", envir = environment())
  v_market <- market; rm(market) #conflict data.table vars
  v_action <- action; rm(action)
  # bug fixed in 1.9.3 on CRAN should be released as 1.9.4
  if(packageVersion('data.table') < "1.9.4") api.dict <- api.dict[order(market,base,quote,action)]
  # ordering will be removed after dt bug fix
  setkeyv(api.dict,c("market","base","quote","action"))
  api.dict.filter <- expression(J(v_market,currency_pair[[1]],currency_pair[[2]],v_action))
  v_url <- api.dict[eval(api.dict.filter)][,url]
  if(is.null(v_url) | is.na(v_url)) stop(paste0('missing api.dict data for particular set: ',v_market,', ',currency_pair[[1]],', ',currency_pair[[2]],', ',v_action))
  #preprocess req for market
  v_req <- api.dict[eval(api.dict.filter)][,pre_process][[1]](req)  #possible overwrite 'v_url' variable for GET or bitstamp place limit order
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': API call request pre-processed',sep='')
  res <- market.api.query(market = v_market, 
                          url = v_url, 
                          req = v_req,
                          ..., 
                          verbose = verbose - 1,
                          on.error = on.error)
  if(raw.query.res){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': raw.query.res=TRUE, returning raw object fromJSON',sep='')
    return(res)
  }
  #catch market's internal errors
  res <- tryCatch(
    expr = {
      res <- api.dict[eval(api.dict.filter)][,catch_market_error][[1]](res) #transcode kind of " x[['error']] " to stop()
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': market level errors checked, no errors occurred',sep='')
      res
    },
    error = function(e){
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': market level error catched, will result eval(on.market.error) param',sep='')
      eval(on.market.error)
    })
  #postprocess res from market
  res <- api.dict[eval(api.dict.filter)][,post_process][[1]](res)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': API call result post-processed',sep='')
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing finished for ',v_market,' ',v_action,sep='')
  return(res)
}

# market.api.query -----------------------------------------------------

#' @title Send request to market API
#'
#' @description Route a request to particular market function.
#'
#' @param market character which identifies market on which we want to send request: bitstamp, btce, kraken, bitmarket.
#' @param \dots objects to be passed to API: \code{url}, \code{key}, \code{secret}, \code{req}, \code{client_id} (used on bitstamp).
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @param on.error expression to be evaluated on R level error of market specific function. It does not catch internal market's error returned as valid object.
#' @return R object created by \code{fromJSON} decoded result from market's API call.
#' @details To do not spam market's API, use \code{Sys.sleep(10)} between API calls.
#' @note It is advised to use this function instead of calling market's function directly. If calling directly one should ensure to send any numeric values in non-exponential notation: \code{options(scipen=100)}. 
#' @seealso \code{\link{market.api.process}}, \code{\link{market.api.query.bitstamp}}, \code{\link{market.api.query.btce}}, \code{\link{market.api.query.kraken}}, \code{\link{market.api.query.bitmarket}}, \code{\link{market.api.query.mtgox}}
#' @references API documentation: \url{https://bitbucket.org/nitrous/mtgox-api}, \url{https://www.bitstamp.net/api/}, \url{https://btc-e.com/api/documentation}, \url{https://www.kraken.com/help/api}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query(market = 'bitstamp', 
#'                  url = 'https://www.bitstamp.net/api/ticker/')
#' market.api.query(market = 'btce', 
#'                  url = 'https://btc-e.com/api/2/btc_usd/ticker')
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/public/Ticker?pair=XXBTZEUR')
#' market.api.query(market = 'bitmarket',
#'                  url = 'https://www.bitmarket.pl/json/LTCPLN/ticker.json')
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
market.api.query <- function(market, ..., verbose = getOption("Rbitcoin.verbose",0), on.error = expression(stop(e[['message']], call. = FALSE))){
  fun_name <- 'market.api.query'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': switch query for particular market: ',market,sep='')
  old.scipen <- options(scipen=100) #it happend that R send a exponential sci notation which is not supported by markets, setting this localy, will revert after function call to previous value
  query_result <- tryCatch(expr = {
    query_result <- switch(market,
                           'mtgox' = market.api.query.mtgox(..., verbose = verbose - 1),
                           'bitstamp' = market.api.query.bitstamp(..., verbose = verbose - 1),
                           'btce' = market.api.query.btce(..., verbose = verbose - 1),
                           'kraken' = market.api.query.kraken(..., verbose = verbose - 1),
                           'bitmarket' = market.api.query.bitmarket(..., verbose = verbose - 1),
                           stop(paste0(fun_name,': unsupported market: ',market)))
    options(old.scipen)
    query_result
  },
  error = function(e){
    options(old.scipen) #revert previous option scipen value on error
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': R level error catched, will result eval(on.error) param',sep='')
    eval(on.error) #error handling
  })
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': query completed',sep='')
  return(query_result)
}

#' @title Send request to mtgox market API
#'
#' @description Send request to mtgox market API. MtGox is already closed but public API calls are working. Also it's code/dictionary can be reused in future.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return R object created by \code{fromJSON} decoded result from market's API call.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://bitbucket.org/nitrous/mtgox-api}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.mtgox(url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast')
#' # wallet
#' market.api.query.mtgox(url = 'https://data.mtgox.com/api/2/BTCUSD/money/info', 
#'                        key = '', secret = '')
#' }
market.api.query.mtgox <- function(url, key, secret, req = list(), verbose = getOption("Rbitcoin.verbose",0)){ 
  fun_name <- 'market.api.query.mtgox'
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    tonce <- as.character(as.numeric(Sys.time()) * 1000000)
    post_data <- paste0('tonce=',tonce)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': tonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
    if(!missing(key) & !missing(secret)){
      method_path <- substr(url,30,nchar(url))
      sign <- hmac(key = base64Decode(secret,mode='raw'), object = c(charToRaw(method_path),as.raw(0),charToRaw(post_data)), algo = 'sha512', raw = TRUE)
      httpheader <- c('Rest-Key' = key, 'Rest-Sign' = base64Encode(sign))
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call on url=\'',url,'\'',sep='')
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name), call. = FALSE)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}

#' @title Send request to bitstamp market API
#'
#' @description Send request to bitstamp market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param client_id character. Bitstamp market specific parameter used in private API call authorization (check reference for more information).
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc..
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return R object created by \code{fromJSON} decoded result from market's API call. Cancel order is an exception handled by hardcode, as bitstamp will not return json format for that method.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://www.bitstamp.net/api/}
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
market.api.query.bitstamp <- function(url, client_id, key, secret, req = list(), verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'market.api.query.bitstamp'
  if(length(req) > 0 | (!missing(key) & !missing(secret) & !missing(client_id))){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    if(!missing(key) & !missing(secret)){
      sign <- toupper(hmac(key = secret, object = paste0(nonce,client_id,key), algo = 'sha256'))
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha256',sep='')
    }
    post_data <- paste0('key=',key,'&signature=',sign,'&nonce=',nonce)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call on url=\'',url,'\'',sep='')
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- if(url == 'https://www.bitstamp.net/api/cancel_order/') query_result_json else fromJSON(query_result_json) #bitstamp exception, cancel order does not return json object
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}

#' @title Send request to btce market API
#'
#' @description Send request to btce market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc. See note.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return R object created by \code{fromJSON} decoded result from market's API call.
#' @note Market specific btce \code{method} param should be provided in \code{req} object.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://btc-e.com/api/documentation}
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
market.api.query.btce <- function(url, key, secret, req = list(), verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'market.api.query.btce'
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(trunc(as.numeric(Sys.time()))) # no multiply "* 1000000" due to btce handle nonce a little worse then other markets, we need smaller nonce values
    post_data <- paste0('nonce=',nonce)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&') 
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
    if(!missing(key) & !missing(secret)){
      sign <- hmac(key = secret, object = post_data, algo = 'sha512')
      httpheader <- c('Key' = key, 'Sign' = sign)
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call on url=\'',url,'\'',sep='')
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 | !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}

#' @title Send request to kraken market API
#'
#' @description Send request to kraken market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return R object created by \code{fromJSON} decoded result from market's API call.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://www.kraken.com/help/api}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.kraken(url = 'https://api.kraken.com/0/public/Ticker?pair=XBTCZEUR')
#' # wallet
#' market.api.query.kraken(url = 'https://api.kraken.com/0/private/Balance', 
#'                         key = '', secret = '')
#' }
market.api.query.kraken <- function(url, key, secret, req = list(), verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'market.api.query.kraken'
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    post_data <- paste0('nonce=',nonce)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
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
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call on url=\'',url,'\'',sep='')
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 | !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}

#' @title Send request to bitmarket market API
#'
#' @description Send request to bitmarket market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return R object created by \code{fromJSON} decoded result from market's API call.
#' @note Market specific bitmarket \code{method} param should be provided in \code{req} object.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://www.bitmarket.pl/docs.php?file=api_private.html}
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
market.api.query.bitmarket <- function(url, key, secret, req = list(), verbose = getOption("Rbitcoin.verbose",0)){ 
  fun_name <- 'market.api.query.bitmarket'
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    tonce <- as.character(as.integer(Sys.time()))
    post_data <- paste0('tonce=',tonce)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': tonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
    if(!missing(key) & !missing(secret)){
      sign <- hmac(key = secret, object = post_data, algo = 'sha512')
      httpheader <- c('API-Key' = key, 'API-Hash' = sign)
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call on url=\'',url,'\'',sep='')
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name), call. = FALSE)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}

# blockchain.api.process ---------------------------------------------------

#' @title Process blockchain.info API
#'
#' @description Query and process results from blockchain.info.
#'
#' @param \dots params passed to blockchain.info API, specific for particular method, example \code{'bitcoin_address'} or \code{'tx_index'}, for more read \code{\link{blockchain.api.query}}.
#' @param method character. For details see \code{blockchain.api.query}, currently supported \code{'Single Address'} and \code{'Single Transaction'}. If \code{method} missing the function will try to guess it based on first param in \dots.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return data.table object, blockchain api data transformed to table.
#' @seealso \code{\link{blockchain.api.query}}
#' @export
#' @examples
#' \dontrun{
#' # Rbitcoin donation address wallet
#' Rbitcoin_donation_wallet <- blockchain.api.process('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi')
#' # some transaction
#' tx <- blockchain.api.process('e5c4de1c70cb6d60db53410e871e9cab6a0ba75404360bf4cda1b993e58d45f8')
#' }
blockchain.api.process <- function(... , method, verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'blockchain.api.process'
  input_list <- list(...)
  if(missing(method)){ # MAINTAIN second version in .process fun
    if(length(input_list) < 1) stop(paste0('missing method and missing ... param'))
    if(nchar(input_list[[1]]) == 34 | any(names(input_list[1]) == 'bitcoin_address')) method <- 'Single Address' #any used to handle NULL names
    else if(nchar(input_list[[1]]) == 64 | any(names(input_list[1]) == 'tx_index')) method <- 'Single Transaction'
    else stop(paste0('missing method and invalid first ... param'))
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': missing method set as \'',method,'\' based on first param in ...',sep='')
  }
  res <- switch(method,
                "Single Address" = {
                  r <- blockchain.api.query(..., method = method, limit = 0, verbose = verbose - 1)
                  data.table(location = 'blockchain', 
                             action = 'Single Address',
                             timestamp = as.POSIXct(Sys.time(), origin = '1970-01-01', tz = 'UTC'), 
                             currency = 'BTC', 
                             hash = r[['hash160']], address = r[['address']], n_tx = r[['n_tx']], 
                             total_received = r[['total_received']]/100000000, 
                             total_sent = r[['total_sent']]/100000000, 
                             final_balance = r[['final_balance']]/100000000)
                },
                "Single Transaction" = {
                  r <- blockchain.api.query(..., method = method, verbose = verbose - 1)
                  list(location = 'blockchain', 
                       action = 'Single Transaction',
                       timestamp = as.POSIXct(r[['time']], origin = '1970-01-01', tz = 'UTC'),
                       currency = 'BTC',
                       double_spend = r[['double_spend']],
                       inputs = rbindlist(lapply(r[['inputs']], function(x) data.table(address = x[['prev_out']][['addr']], value = x[['prev_out']][['value']]/100000000, tx_index = x[['prev_out']][['tx_index']]))),
                       hash = r[['hash']],
                       tx_index = r[['tx_index']],
                       out = rbindlist(lapply(r[['out']], function(x) data.table(address = x[['addr']], value = x[['value']]/100000000, tx_index = x[['tx_index']]))),
                       size = r[['size']])
                })
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': call and post-processing finished',sep='')
  res
}

# blockchain.api.query  -----------------------------------------------------

#' @title Query blockchain.info API
#'
#' @description Query bitcoin related data from blockchain.info.
#'
#' @param \dots params passed to blockchain.info API, specific for particular method, example \code{'bitcoin_address'} or \code{'tx_index'}, for more see references or examples.
#' @param method character. For details see references, currently supported \code{'Single Address'} and \code{'Single Transaction'}. If \code{method} missing the function will try to guess it based on first param in \dots.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return result returned by \code{fromJSON} function applied on the blockchain result, most probably the list.
#' @seealso \code{\link{market.api.query}}
#' @references \url{https://blockchain.info/api/blockchain_api}
#' @export
#' @examples
#' \dontrun{
#' # query bitcoin address information - 'Single Address' method
#' # Rbitcoin donation address final balance in BTC
#' blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi',limit=0)[['final_balance']]/100000000
#' # Rbitcoin donation address full details
#' blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi',verbose=1)
#' # some first wallet final balance in BTC
#' blockchain.api.query('1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa',limit=0)[['final_balance']]/100000000
#' # some first wallet details (limit to 3 txs, skip two txs)
#' blockchain.api.query(method = 'Single Address',
#'                      bitcoin_address = '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa', limit=3, offset=2)
#' # query bitcoin transaction information - 'Single Transaction' method
#' # Some recent transaction of some first wallet
#' blockchain.api.query('e5c4de1c70cb6d60db53410e871e9cab6a0ba75404360bf4cda1b993e58d45f8')
#' }
blockchain.api.query <- function(... , method, verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'blockchain.api.query'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing started',sep='')
  input_list <- list(...)
  if(missing(method)){ # MAINTAIN second version in .process fun
    if(length(input_list) < 1) stop(paste0('missing method and missing ... param'))
    if(nchar(input_list[[1]]) == 34 | any(names(input_list[1]) == 'bitcoin_address')) method <- 'Single Address' #any used to handle NULL names
    else if(nchar(input_list[[1]]) == 64 | any(names(input_list[1]) == 'tx_index')) method <- 'Single Transaction'
    else stop(paste0('missing method and invalid first ... param'))
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': missing method set as \'',method,'\' based on first param in ...',sep='')
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
                stop(paste0(fun_name,': unsupported method: ',method)))
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': url prepared: ',url,sep='')
  curl <- getCurlHandle(useragent = paste("Rbitcoin",packageVersion("Rbitcoin")))
  res_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': API call executed',sep='')
  res <- fromJSON(res_json)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': call result processed using fromJSON',sep='')
  return(res)
}

# available wallet --------------------------------------------------------

#' @title Available wallet
#'
#' @description Calculates assets available to trade, not on hold by current open orders.
#'
#' @param wallet data.table object returned by \code{market.api.process} with \code{action="wallet"} param.
#' @param open_orders data.table object returned by \code{market.api.process} with \code{action="open_orders"} param.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return data.table object, the same as wallet but with the appropriate amounts after subtracting the open orders amounts.
#' @seealso \code{\link{market.api.process}}
#' @export
#' @examples
#' \dontrun{
#' wallet <- market.api.process('kraken',c('BTC','EUR'),'wallet', key = '', secret = '')
#' Sys.sleep(10)
#' open_orders <- market.api.process('kraken',c('BTC','EUR'),'open_orders', key = '', secret = '')
#' aw <- available_wallet(wallet, open_orders, verbose = 1)
#' print(aw)
#' }
available_wallet <- function(wallet, open_orders, verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'available_wallet'
  # input validation
  if(nrow(wallet)==0){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': Wallet object is empty, returning provided wallet',sep='')
    warning(paste0("Wallet object provided to ",fun_name," is empty, returning provided wallet"))
    return(wallet)
  }
  if(nrow(open_orders)==0){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': no open orders, returning provided wallet',sep='')
    return(wallet)
  }
  # processing
  if(open_orders[,any(unlist(lapply(lapply(.SD,is.na),any))), .SDcols = c('type','price','amount')])
    stop('available_wallet requires open_orders with non NA type, price, amount. Not every market returned those data in open_orders method')
  setkey(wallet,currency)
  available <- rbindlist(list(
    open_orders[type=='buy',list(amount = price * amount),by='quote'][,list(currency = quote, amount)],
    open_orders[type=='sell',list(amount = amount),by='base'][,list(currency = base, amount)]
  ))[,list(amount_in_orders = sum(amount)),keyby='currency'][wallet,list(amount_available = sum(amount,-amount_in_orders,na.rm=TRUE))]
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': available wallet calculated',sep='')
  available
}

# wallet manager ----------------------------------------------------------

#' @title Wallet Manager
#'
#' @description Downloads wallet balance from multiple sources and calculate value in chosen currency based on actual exchange rates. Function is limited to dictionary \code{\link{api.dict}} plus fiat-fiat exchange rates.
#'
#' @param market.sources list of market sources definition, see examples. Mandatory fields: \code{market, currency_pair, key, secret} (for bitstamp also \code{client_id}).
#' @param blockchain.sources list of blockchain sources definition, see examples. Mandatory field: \code{address}.
#' @param manual.sources list of manually provided amounts, see examples. Mandatory fields: \code{currency, amount}, optional field: \code{location, location_type}.
#' @param min_amount numeric used to filter out near zero amounts of source currency, default \code{0.0001}.
#' @param antispam_interval numeric time in seconds between API calls on one source system, defeault \code{10s}.
#' @param api.dict data.table required when using custom API dictionary, read \code{\link{market.api.process}} for details.
#' @param verbose integer Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @param value_calc logical calculate value, by default \code{TRUE}, can be turned off by setting to \code{FALSE}. Process will be slightly faster due to no API calls for exchange rates.
#' @param value_currency character default \code{"USD"}, target currency in which measure the current value.
#' @param value_currency_type character, optional for most currencies, if \code{value_currency} is an exotic currency you need to define its currency type ('crypto' or 'fiat') in this param or update \code{getOption("Rbitcoin.ct.dict")} param.
#' @param rate_priority character vector of market and priorioties for sourcing exchange rates, this param needs to be maintained by user, read Exchange rates note below. Example param value \code{rate_priority = c('bitstamp','kraken','bitmarket','btce')}.
#' @param transfer_currency_pair vector length of 2 of named character, default \code{c(crypto = "BTC", fiat = "USD")}, read Exchange rates note below.
#' @param archive_write logical, default \code{FALSE}, recommended \code{TRUE}. If \code{TRUE} wallet manager result will be archived to \code{"wallet_archive.rds"} file in the working directory, read Wallet archive note below.
#' @param archive_read logical, default \code{FALSE}, recommended \code{FALSE}. If \code{TRUE} it return full archive of wallets data over time grouped by \code{wallet_id}. To be used when passing results to \code{\link{Rbitcoin.plot}} function or performing other analysis over time, read notes below.
#' @return data.table object with wallet information in denormilized structure. Number of columns depends on \code{value_calc} param, when \code{FALSE} then columns related to the value will not be returned. When launch with \code{wallet_read=TRUE} then all historical archived wallet statuses will be returned. Field \code{wallet_id} is a processing batch id and also the timestamp of single wallet manager processing as integer in Unix time format.
#' @section Wallet archive:
#' To be able to track wallet assets value over time user needs to use \code{archive_write=TRUE}. It will archive wallet manager result \code{data.table} to \code{wallet_archive.rds} file in not encrypted format (not a plain text also), sensitive data like amount and value will be available from R by \code{readRDS("wallet_archive.rds")}. This can be used to correct/manipulate archived data or union the results of the wallet manager performed on different machines by \code{readRDS(); rbindlist(); saveRDS()}. Setting \code{archive_write=FALSE} and \code{archive_read=TRUE} will skip processing and just load the archive, same as \code{readRDS()}.
#' You should be aware the archive file will be growing over time, unless you have tons of sources defined or you scheduled \code{wallet_manager} every hour or less you should not experience any issues because of that. In case of the big size of archived rds file you can move data to database, wrap function into database archiver function and query full archive from database only for for plotting.
#' @section Exchange rates:
#' Exchange rates will be downloaded from different sources. Fiat-fiat rates will be sourced from yahoo finance, if yahoo would not be available then also fiat-fiat rate cannot be calculated. Rates for cryptocurrencies will be downloaded from market's tickers according to \code{rate_priority} and currency pairs available in \code{api.dict}. Currency type (crypto or fiat) is already defined in \code{getOption("Rbitcoin.ct.dict")}, can be edited for support other/new currency.\cr
#' Markets used for crypto rates are defined by \code{rate_priority} as vector of market names in order of its priority from highest to lowest. User need to chose own trusted exchange rates providers and keep in mind to update \code{rate_priority} parameter when necessary. As we recently seen the mtgox after death was still spreading the public API data and any system which sources data from them would be affected, so the control over the source for exchange rates needs to be maintained by user.
#' In case of calculation crypto rate for a currency pair which is not available in \code{\link{api.dict}} then \code{transfer_currency_pair} will be used to get indirect exchange rate. Example: exchange rate for NMC-GBP will be computed as NMC-BTC-USD-GBP using the default \code{transfer_currency_pair} and current \code{api.dict}.
#' The process was not exhaustively tested, you can track all the exchange rates used by setting \code{options(Rbitcoin.archive_exchange_rate=0)} for \code{saveRDS()},  \code{options(Rbitcoin.archive_exchange_rate=1)} for \code{write.table(sep=",", dec=".")} or \code{options(Rbitcoin.archive_exchange_rate=2)} for \code{write.table(sep=";", dec=",")}. This option will append the data to \code{exchange_rate_archive} rds/csv file in working directory.
#' @section NA measures:
#' In case of missing exchange path (direct and indirect through \code{transfer_currency_pair} based on \code{\link{api.dict}}) between the currency in the wallet and the \code{value_currency} the \code{NA} will be provided to \code{value} for that currency. Any errors while downloading wallet data or exchange rates data will also result \code{NA} measure.
#' Be sure to avoid \code{NA} measures: for unavailable sources you can provide amounts as manual source, for not supported alt cryptocurrencies precalculate its value to supported currency and provide as manual source.
#' While plotting \code{wallet_manager} data any wallet batches which contain at least one \code{NA} measure will be omitted from plot.
#' @section Schedule wallet tracking:
#' User may consider to schedule execution of the function with \code{archive_write=TRUE} for better wallet assets tracking over time. Schedule can be setup on OS by run prepared R script with \code{wallet_manager} function execution. In case of scheduling also plot of wallet manager use \code{archive_read=TRUE} and add \code{Rbitcoin.plot} function execution.
#' @section Troubleshooting:
#' In case of the issues with this function verify if all of the sources are returning correct data, use \code{blockchain.api.process} and \code{market.api.process} functions. Possible sources for wallet data: market api, blockchain api, manually provided. Possible sources for exchange rate data: market tickers, yahoo (see references). If all sources works and issue still occurs please report.
#' Additionally you can always use \code{verbose} argument to print processing informations.
#' @seealso \code{\link{Rbitcoin.plot}}, \code{\link{blockchain.api.process}}, \code{\link{market.api.process}}, \code{\link{antiddos}}
#' @references \url{https://code.google.com/p/yahoo-finance-managed/wiki/csvQuotesDownload}
#' @export
#' @examples
#' \dontrun{
#' ## define source
#' # define wallets on markets
#' market.sources <- list(
#'   list(market = 'bitstamp', currency_pair = c('BTC', 'USD'),
#'        client_id = '', key = '', secret = ''),
#'   list(market = 'btce', currency_pair = c('LTC', 'USD'),
#'        key = '', secret = ''),
#'   list(market = 'btce', currency_pair = c('LTC', 'USD'),
#'        key = '', secret = ''), #multiple accounts on same market possible
#'   list(market = 'kraken', currency_pair = c('BTC', 'EUR'),
#'        key = '', secret = '')
#' )
#' # define wallets on blockchain
#' blockchain.sources <- list(
#'   list(address = ''),
#'   list(address = '')
#' )
#' # define wallets manually
#' manual.sources <- list(
#'   list(location = 'while transferring',
#'        currency = c('BTC','LTC'),
#'        amount = c(0.08, 0)),
#'   # manually provided value as workaround for bitstamp api unavailability captcha bug
#'   list(location = 'bitstamp',
#'        location_type = 'market' 
#'        currency = c('USD','BTC'),
#'        amount = c(50,0.012))
#' )
#' 
#' ## launch wallet manager with no value calculation
#' wallet_dt <- wallet_manager(market.sources,
#'                             blockchain.sources,
#'                             manual.sources,
#'                             value_calc = FALSE)
#' print(wallet_dt)
#' 
#' ## launch wallet manager
#' wallet_dt <- wallet_manager(
#'   market.sources = market.sources,
#'   blockchain.sources = blockchain.sources, 
#'   manual.sources = manual.sources, 
#'   value_currency = 'GBP', 
#'   rate_priority = c('bitstamp','kraken','bitmarket','btce')
#'   archive_write = TRUE
#' )
#' print(wallet_dt)
#' 
#' # export to excel/google spreadsheet
#' setkey(wallet_dt,wallet_id,currency) #sort
#' write.table(wallet_dt, "clipboard", sep="\t", row.names=FALSE, na = "")
#' # now go to excel or google spreadsheet and use "paste" from clipboard
#' 
#' # aggregate measures by currency and type
#' wallet_dt[,list(amount = sum(amount, na.rm=T),
#'                 value = sum(value, na.rm=T)),
#'            by = c('wallet_id','currency','value_currency')
#'            ][order(wallet_id,currency,value_currency)]
#' # aggregate value by location and type
#' wallet_dt[,list(value = sum(value, na.rm=T)),
#'            by = c('wallet_id','location_type','location')
#'            ][order(wallet_id,location_type,location)]
#' 
#' # send to plot
#' wallet_dt <- wallet_manager(archive_write=F, archive_read=T)
#' Rbitcoin.plot(wallet_dt)
#' 
#' # discard processing batch, by id, from wallet archive (will omit on plot)
#' dt <- readRDS("wallet_archive.rds")
#' dt[wallet_id==1390000000,`:=`(amount = NA_real_, value = NA_real_)]
#' saveRDS(dt, "wallet_archive.rds")
#' 
#' # To track exchange rates used set option Rbitcoin.archive_exchange_rate
#' options(Rbitcoin.archive_exchange_rate=0)
#' wallet_dt <- wallet_manager(market.sources,
#'                             blockchain.sources,
#'                             manual.sources = manual.sources,
#'                             rate_priority = c('bitstamp','kraken','bitmarket','btce')
#'                             archive_write = TRUE)
#' # all exchange rate data as dt
#' dt <- readRDS("exchange_rate_archive.rds")
#' # last exchange rate table as dt
#' dt <- readRDS("exchange_rate_archive.rds")[value_rate_id==max(value_rate_id)]
#' # save to csv
#' write.table(dt, "exchange_rate_archive.csv",
#'             row.names=FALSE,quote=FALSE,append=FALSE,col.names=TRUE,
#'             sep=";", dec=",")
#' }
wallet_manager <- function(market.sources = NULL, 
                           blockchain.sources = NULL, 
                           manual.sources = NULL,
                           min_amount = 0.0001, 
                           antispam_interval = 10, 
                           api.dict = NULL, 
                           verbose = getOption("Rbitcoin.verbose",0),
                           value_calc = TRUE, 
                           value_currency = 'USD', 
                           value_currency_type = NULL,
                           rate_priority, 
                           transfer_currency_pair = c(crypto = "BTC", fiat = "USD"),
                           archive_write = FALSE, 
                           archive_read = FALSE){
  fun_name <- 'wallet_manager'
  
  # read archive only, skip processing
  if(archive_read & !archive_write){
    wallet_dt.archive <- if(file.exists('wallet_archive.rds')) readRDS('wallet_archive.rds') else stop('Wallet archive file wallet_archive.rds does not exists, run function with archive_write=TRUE')
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing started, wallet manager processing skipped, returning wallet archive only',sep='')
    if(value_calc) return(wallet_dt.archive) 
    else return(wallet_dt.archive[,list(wallet_id,currency,currency_type, timestamp, location, location_type, amount)])
  }
  # convert list() to NULL
  if(!is.null(manual.sources)){ if(length(manual.sources)==0) manual.sources <- NULL}
  if(!is.null(blockchain.sources)){ if(length(blockchain.sources)==0) blockchain.sources <- NULL}
  if(!is.null(market.sources)){ if(length(market.sources)==0) market.sources <- NULL}
  
  # common missing param - throw error without processing wallets
  ct.dict <- getOption("Rbitcoin.ct.dict")
  ct_dt <- rbindlist(lapply(1:length(ct.dict), function(i) data.table(currency_type = names(ct.dict[i]), currency = ct.dict[[i]])))[,.SD,,keyby='currency']
  if(value_calc){
    if(missing(rate_priority)) stop('rate_priority argument missing in wallet_manager function, provide the vector of market names which should be used in exchange rate lookup for value calculation, read ?wallet_manager', call.=FALSE)
    
    # value_currency_type from ct.dict
    if(is.null(value_currency_type)){ #value_currency_type is optional if value_currency available in ct.dict
      value_currency_type <- ct_dt[value_currency][,currency_type]
      if(is.na(value_currency_type)) stop(paste0('No currency type defined for provided value_currency "',value_currency,'", provide value_currency_type ("fiat" or "crypto") or update getOption("Rbitcoin.ct.dict") for ',value_currency))
    } # get from ct.dict
  }
  
  
  # START PROCESSING
  wallet_id <- as.integer(Sys.time())
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing started for wallet_id ',wallet_id,sep='')
  
  # local processing funs
  wallet_source.market <- function(x, verbose){
    fun_name <- 'wallet_source.market'
    wait <- antiddos(market = x[['market']], antispam_interval = antispam_interval, verbose = verbose - 1)
    # call api
    x[['action']] <- 'wallet'
    x[['api.dict']] <- api.dict
    x[['verbose']] <- verbose - 2 # market.api.process debugging to background
    #wal <- do.call(what = market.api.process, args = x)[,list(timestamp = timestamp, location = x[['market']], location_type = 'market', currency, amount)] #do.call to handle bitstamp client_id param
    wal <- tryCatch(
      expr = do.call(what = market.api.process, args = x)[,list(timestamp = timestamp, location = x[['market']], location_type = 'market', currency, amount)], #do.call to handle bitstamp client_id param
      error = function(e){
        msg <- paste0('error on downloading wallet from ',x[['market']])
        if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': ',msg,': ',e[["message"]],sep='')
        warning(paste0(msg,': ',e[["message"]]), call. = FALSE)
        data.table(timestamp = Sys.time(), location = x[['market']], location_type = 'market', currency = NA_character_, amount = NA_real_)
      }
    )
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': source processed for ',x[['market']],sep='')
    wal
  }
  wallet_source.blockchain <- function(x, verbose){
    fun_name <- 'wallet_source.blockchain'
    wait <- antiddos(market = 'blockchain', antispam_interval = antispam_interval, verbose = verbose - 1) 
    x[['method']] <- 'Single Address'
    x[['verbose']] <- verbose - 2 # blockchain.api.process debugging to background
    #wal <- do.call(what = blockchain.api.process, args = x)[,list(timestamp = timestamp, location = address, location_type = 'blockchain', currency, amount = final_balance)]
    wal <- tryCatch( #decode location to location_type, address to location
      expr = do.call(what = blockchain.api.process, args = x)[,list(timestamp = timestamp, location = address, location_type = 'blockchain', currency, amount = final_balance)],
      error = function(e){
        msg <- paste0('error on downloading wallet from ','blockchain')
        if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': ',msg,': ',e[["message"]],sep='')
        warning(paste0(msg,': ',e[["message"]]), call. = FALSE)
        data.table(timestamp = Sys.time(), location = x[['address']], location_type = "blockchain", currency = NA_character_, amount = NA_real_)
      }
    )
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': source processed for ','blockchain',sep='')
    wal
  }
  wallet_source.manual <- function(x, verbose){
    fun_name <- 'wallet_source.manual'
    wal <- data.table(timestamp = Sys.time(), 
                      location = if(is.null(x[['location']])) NA_character_ else x[['location']], 
                      location_type = if(is.null(x[['location_type']])) 'manual' else x[['location_type']],
                      currency = as.character(x[['currency']]),
                      amount = as.numeric(x[['amount']]))
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': source processed for ',if(is.null(x[['location']])) NA_character_ else paste0('\'',x[['location']],'\''),sep='')
    wal
  }
  
  all.wallet <- list()
  all.wallet[['market']] <- if(length(market.sources) > 0){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing market wallets',sep='')
    lapply(market.sources, wallet_source.market, verbose = verbose - 1)
  }
  all.wallet[['blockchain']] <- if(length(blockchain.sources) > 0){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing blockchain wallets',sep='')
    lapply(blockchain.sources, wallet_source.blockchain, verbose = verbose - 1)
  }
  all.wallet[['manual']] <- if(length(manual.sources) > 0){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing manual wallets',sep='')
    lapply(manual.sources, wallet_source.manual, verbose = verbose - 1)
  }
  
  # combine cleaned raw results
  if(length(all.wallet) == 0){
    wallet_dt <- data.table(wallet_id = integer(), currency = character(), currency_type = character(), timestamp = as.POSIXct(NA, origin='1970-01-01', tz='UTC')[-1], location = character(), location_type = character(), amount = numeric())
  }
  else if(length(all.wallet) > 0){
    all.wallet_dt <- rbindlist(lapply(all.wallet, function(x) rbindlist(x)))[amount >= min_amount | is.na(amount),.SD,,keyby='currency']
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': all wallets combined',sep='')
    
    # add currency_type dimensions
    wallet_dt <- ct_dt[all.wallet_dt
                       ][,if(.N > 0){
                         data.table(wallet_id = wallet_id, currency, currency_type, timestamp, location, location_type, amount)
                       } else data.table(wallet_id = integer(), currency = character(), currency_type = character(), timestamp = as.POSIXct(NA,origin='1970-01-01',tz='UTC')[-1], location = character(), location_type = character(), amount = numeric())
                       ]
  }
  if(nrow(wallet_dt) == 0) warning('Zero rows wallet table, review sources definition and min_amount', call.=FALSE)
  
  # calc value
  if(value_calc & nrow(wallet_dt) == 0){
    wallet_dt[,`:=`(value_currency = character(), value_rate = numeric(), value = numeric())]
  }
  
  else if(value_calc & nrow(wallet_dt) > 0){
    if(!identical(rate_priority,unique(rate_priority))){
      stop('rate_priority must contain a vector of unique market names')
    } # stop, vector needs to be unique
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': calculates currency value in ',value_currency,sep='')
    wallet_dt <- wallet_value(wallet_dt = wallet_dt, 
                              value_currency = value_currency, 
                              value_currency_type = value_currency_type,
                              rate_priority = rate_priority, 
                              transfer_currency_pair = transfer_currency_pair,
                              antispam_interval = antispam_interval,
                              api.dict = api.dict, 
                              verbose = verbose - 1)
  }
  
  # archive_write
  if(archive_write){
    wallet_dt.archive <- if(file.exists('wallet_archive.rds')) readRDS('wallet_archive.rds') else NULL
    if(nrow(wallet_dt) > 0){
      wallet_dt.archive <- rbindlist(list(
        wallet_dt.archive, 
        if(value_calc) wallet_dt else wallet_dt[,list(wallet_id,currency,currency_type, timestamp, location, location_type, amount,
                                                      value_currency = NA_character_, value_rate = NA_real_, value = NA_real_)]
      ))
      saveRDS(wallet_dt.archive,'wallet_archive.rds')
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': wallet data archived to wallet_archive.rds',sep='')
    }
  }
  
  # archive_read
  if(archive_read){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': wallet manager processing finished, returning full wallet archive',sep='')
    if(value_calc) return(wallet_dt.archive) 
    else return(wallet_dt.archive[,list(wallet_id,currency,currency_type, timestamp, location, location_type, amount)])
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': wallet manager processing finished',sep='')
  return(wallet_dt)
}

# Rbitcoin.plot ------------------------------------------------------

#' @title Plot Rbitcoin objects
#'
#' @description Generic function to plot different objects returned by some Rbitcoin functions. The plot produce basic visualization of the data. The plots will likely be subject to change in future versions.
#'
#' @param x object to be plotted, result of Rbitcoin function, currently supported: \code{market.api.process} with \code{action} in \code{c("trades","order_book")}, \code{wallet_manager} with \code{archive_read = TRUE}.
#' @param mask logical, default \code{FALSE}, setting \code{TRUE} will mask values on wallet manager plot with the ratio of value to the initial value. Use this when you want to share the plot. See examples to mask the bitcoin address.
#' @param export logical default \code{FALSE}, if \code{TRUE} the plot will be exported to file instead of plot to ploting device.
#' @param export.args list of arguments to be passed to target graphic device function, ex. \code{svg()} or \code{png()}, list gives the control over width and height which by default for png are quite small. Element \code{export.args[['format']]} will be mapped to the function name, by default \code{svg()}, any others as its \code{args}.
#' @param \dots additional params to be passed to plot function.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return TRUE
#' @section Export:
#' Element \code{format} in the \code{export.args} list defines the export format, default \code{"svg"}, tested and supported formats are \code{"svg"} and \code{"png"}, other might work also.
#' To use custom export filename just pass the filename arg to export.args list. By default \code{NULL} results timestamped by last \code{wallet_id} filename. Use custom \code{export.args[['filename']]} with no file extension while declaring.
#' You may notice the legend is different on exported files. The same legend was not scalling well between export to file and plot to interactive device.
#' @section input trades, order_book:
#' The plot function for \code{trades}, \code{order_book} do not process the data, it plot the data as is, so it may result poor visibility due to the data itself (ex. \code{order_book} containing asks with enormously high price). See examples how this can be handled.
#' @section input wallet manager:
#' To be able to track wallet assets value over time user needs to use \code{archive_write=TRUE} at least twice in wallet manager processing (with non-NA measures). Using the cryptocurrency which do not have any exchange path to \code{transfer_currency_pair} and/or \code{value_currency} will result \code{NA} as \code{value}. Error on data downloading from external sources (wallets or exchange rates) will also result \code{NA}. Any wallet processing batch which will contain at least one \code{NA} measure will be omitted from plot. If you have some crypto not currenctly supported you may extend dictionary for more currencies or provide its value as manual source to \code{wallet_manager} already calculated in common value currency, remember to comment out the previous source which returns the \code{NA} measure.\cr To plot wallet manager data load wallet archive data, see examples.\cr Plotting function will produce dashboard panel to track different measures of your assets and its value. Use \code{mask} if you want to share the results to somebody, it will overwrite value with value ratio.
#' Target value currency is taken from the last execution of \code{wallet_manager}.
#' @seealso \code{\link{market.api.process}}, \code{\link{wallet_manager}}
#' @export
#' @examples
#' \dontrun{
#' # plot trades data from kraken's api
#' trades <- market.api.process('kraken',c('BTC','EUR'),'trades')
#' Rbitcoin.plot(trades)
#' Rbitcoin.plot(trades,export=TRUE,col='blue') #export to file, plot trades line in blue
#' 
#' # plot order book data from kraken's api
#' order_book <- market.api.process('kraken',c('BTC','EUR'),'order_book')
#' Rbitcoin.plot(order_book)
#' 
#' # plot order book with filtering margins based on order price
#' order_book <- market.api.process('bitmarket',c('BTC','PLN'),'order_book')
#' pct <- 0.75
#' mid <- ((order_book[["asks"]][1,price] + order_book[["bids"]][1,price]) / 2)
#' order_book[["asks"]] <- order_book[["asks"]][price <= mid * (1+pct)]
#' order_book[["bids"]] <- order_book[["bids"]][price >= mid * (1-pct)]
#' Rbitcoin.plot(order_book)
#' 
#' # plot wallet manager data (from local archive) - for details read ?waller_manager
#' wallet_dt <- wallet_manager(archive_write=F, archive_read=T) #readRDS("wallet_archive.rds")
#' Rbitcoin.plot(wallet_dt) # plot in R
#' Rbitcoin.plot(wallet_dt[value>=100 | is.na(value)]) # filter out low value from plot
#' Rbitcoin.plot(wallet_dt, export=T) # export to svg
#' # mask value with ratio value and save to png
#' Rbitcoin.plot(wallet_dt,mask=T,export=T,
#'               export.args=list(format="png",
#'                                width = 2*480,
#'                                height = 2*480, 
#'                                units = "px", 
#'                                pointsize = 18))
#' # mask value with ratio and mask bitcoin addresses
#' Rbitcoin.plot(wallet_dt[,.SD][location_type=="blockchain",location := "*address*"],
#'               mask=T, export=T)
#' }
Rbitcoin.plot <- function(x, mask = FALSE, ..., 
                          export = FALSE, export.args = list(format = 'svg', filename = NULL), 
                          verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'Rbitcoin.plot'
  # recognize input
  if(is.list(x) & !is.data.table(x)){
    stopifnot(length(x) > 0)
    if(!is.null(x[['asks']]) | !is.null(x[['bids']])){
      action <- 'order_book'
    } else if(!is.null(x[['trades']])){
      action <- 'trades'
    } else{
      stop(paste0("unknown list object provided to ",fun_name,", list should contain asks/bids/trades element"))
    }
  } else if(is.data.table(x)){
    stopifnot(nrow(x) > 0)
    if(all(c('wallet_id','currency','amount','value_currency','value_rate','value') %in% names(x))){
      if(length(x[,unique(wallet_id)])==1) stop(paste0("Plotting wallet manager possible for wallet archive data, load wallet_manager archive using archive_read=TRUE param, also ensure you did run wallet_manager at least twice using archive_write=TRUE"))
      else if(length(x[,unique(wallet_id)]) > 1) action <- 'wallet_manager'
    } else if(all(c('wallet_id','currency','amount') %in% names(x))){
      stop(paste0("Plotting wallet manager possible only for results including value, use wallet_manager with value_calc=TRUE param"))
    } else {
      stop(paste0("unknown data.table object provided to ",fun_name,", read manual for supported objects"))
    }
  } else {
    stop(paste0("unknown object provided to ",fun_name,", read manual for supported objects"))
  }
  
  if(export){
    #if(!all(export.args[['format']] %in% c('svg','png'))) stop("export format supports currently svg and png format")
    if(is.null(export.args[['filename']])){
      export.args[['filename']] <- switch(
        action,
        'order_book' = paste(as.character(x[['timestamp']],format='%Y%m%d%H%M%S'),x[['market']],paste0(x[['base']],x[['quote']]),action,sep='_'),
        'trades' = paste(as.character(x[['timestamp']],format='%Y%m%d%H%M%S'),x[['market']],paste0(x[['base']],x[['quote']]),action,sep='_'),
        'wallet_manager' = paste(as.character(as.POSIXct(x[,max(wallet_id)], origin='1970-01-01', tz='UTC'),format='%Y%m%d%H%M%S'),action,sep='_')
      )
    }
    export.args[['filename']] <- paste(export.args[['filename']],export.args[['format']],sep='.')
    FUN <- export.args[['format']]
    export.args['format']  <- NULL
    do.call(FUN, export.args)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': setting plot export to ',export.args[['filename']],sep='')    
    on.exit(invisible(dev.off()), add = FALSE)
  }
  # launch plot
  switch(action,
         'order_book' = Rbitcoin.plot.order_book(x, ..., verbose = verbose - 1),
         'trades' = Rbitcoin.plot.trades(x, ..., verbose = verbose - 1),
         'wallet_manager' = Rbitcoin.plot.wallet(x, mask = mask, ..., verbose = verbose - 1))
  if(export){
    invisible(dev.off())
    on.exit(NULL)
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': plotting finished',sep='')
  invisible(TRUE)
}

Rbitcoin.plot.wallet <- function(x, mask = FALSE, ..., verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'Rbitcoin.plot.wallet'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': preparing metadata to plot',sep='')
  x[,`:=`(
    na_group = any(c(
      namount = any(is.na(amount)), 
      nvalue_rate = any(is.na(value_rate)),
      nvalue = any(is.na(value))
    ))
  ),
  by=c('wallet_id')] #NA WILL BE OMITED
  
  # choose last
  v.value_currency <- x[which.max(wallet_id),value_currency]
  
  setkeyv(x,c('na_group','value_currency'))
  
  # chosoe most frequently used value currency # WE USE LAST - above
  #v.value_currency <- x[J(FALSE),.N, by="value_currency"][order(-N)][1,value_currency]
  
  # verify time dimension length > 1
  if(x[J(FALSE,v.value_currency), nrow(unique(.SD)), .SDcols=c('wallet_id')]$V1 <= 1){ # data.table 1.9.3: .SDcols=c('wallet_id')] <= 1){
    stop(paste0("Cannot plot time on x axis: provided combined wallet data consists only one (non-NA measure) observation for (recent used) value currency: ",v.value_currency,", be sure to load wallet archive using wallet_dt <- wallet_manager(archive_write=FALSE, archive_read=TRUE) or readRDS(), by default wallet_manager function return only recent wallet data. See examples"), call.=FALSE)
  }
  
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': plotting',sep='')
  
  # plot window for matrix of plots
  #op <- par(mfrow=c(3,2),mar=c(5.1,4.1,4.1,2.1)) #default margins
  op <- par(mfrow=c(3,2),mar=c(4.6,4.1,3.6,2.1))
  on.exit(par(op), add = TRUE)
  # legend settings
  case.legend <- function(legend_, col_, verbose=getOption("Rbitcoin.verbose",0)){
    fun_name <- 'case.legend'
    if(!dev.interactive()){ 
      legend(x = 'topleft', legend = legend_, col = col_, lty = 1, 
             cex = 0.7, y.intersp = 0.8, seg.len = 1)
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': plotting legend for export graphics device',sep='')
    } else if(dev.interactive()){
      legend(x = 'topleft', legend = legend_, col = col_, lty = 1, 
             cex = 0.7, y.intersp = 0.8, seg.len = 1, bty = "n")
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': plotting legend for interactive graphics device',sep='')
    } else {
      msg <- paste0("some legend was not plotted, unknow result of dev.cur() case: ",names(dev.cur()),' ',dev.cur())
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': ', msg,sep='')
      warning(msg)
    }
    NULL
  }
  
  # total value over time
  x[J(FALSE,v.value_currency),#na_group==FALSE & value_currency==v.value_currency,
    list(value = sum(value,na.rm=T)),
    by=c('wallet_id')
    ][,
      `:=`(value_init = sum(.SD[wallet_id==min(wallet_id),value]))
      ][,
        `:=`(value_mask = value / value_init, value_init = NULL)
        ][order(wallet_id)
          ][,{
            plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
                 y = if(mask) value_mask else value,
                 type = 'l', xlab = 'time', ylab = 'value', col = 1,
                 main = paste('Wallet value over time in',v.value_currency,sep=' '),
                 #sub = as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z'),
                 ...)
          }]
  
  # last wallet
  x[J(FALSE,v.value_currency) # na_group==FALSE & value_currency==v.value_currency,
    ][wallet_id==max(wallet_id)
      ][,
        list(value = sum(value,na.rm=T)),
        by=c('wallet_id','currency')
        ][,
          `:=`(value_init = sum(value))
          ][,
            `:=`(value_mask = value / value_init, value_init = NULL)
            ][order(-value)][,{
              barplot(if(mask) value_mask else value,
                      names.arg=currency,
                      xlab = paste("Total value:",if(mask) as.character(round(sum(value_mask),2)) else paste(as.character(round(sum(value),2)),v.value_currency)), #sub title functionality here
                      ylab = 'value', col = 2:(.N+1),
                      main = paste('Last wallet currency value in',v.value_currency,sep=' '),
                      #sub = as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z'),
                      ...)
            }]
  
  # currency value over time
  x[J(FALSE,v.value_currency),#na_group==FALSE & value_currency==v.value_currency,
    list(value = sum(value,na.rm=T)),
    by=c('wallet_id','currency')
    ][,
      `:=`(value_init = sum(.SD[wallet_id==min(wallet_id),value]))
      ][,
        `:=`(value_mask = value / value_init, value_init = NULL)
        ][order(wallet_id,currency)
          ][,{
      plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
           y = if(mask) value_mask else value,
           type = 'n', xlab = 'time', ylab = 'value',
           main = paste('Currency value over time in',v.value_currency,sep=' '),
           #sub = as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z'),
           ...)
      cl <- 1
      lg <- data.table()
      #all currencies
      for(v.currency in unique(currency)){
        cl <- cl+1
        lines(x = as.POSIXct(.SD[currency==v.currency,wallet_id], origin='1970-01-01', tz='UTC'),
              y = .SD[currency==v.currency,if(mask) value_mask else value],
              col = cl)
        #add to legend
        lg <- rbindlist(list(lg, data.table(legend_ = v.currency, col_ = cl)))
      }
      lg[,{
        case.legend(legend_, col_, verbose=verbose-1) # different legend for interarctive graphics device and for export due to scaling issue
      }]
      NULL
    }]
  
  # currency_type
  x[J(FALSE,v.value_currency),#na_group==FALSE & value_currency==v.value_currency,
    list(value = sum(value,na.rm=T)),
    by=c('wallet_id','currency_type')
    ][,
      `:=`(value_init = sum(.SD[wallet_id==min(wallet_id),value]))
      ][,
        `:=`(value_mask = value / value_init, value_init = NULL)
        ][order(wallet_id,currency_type)
          ][,{
            plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
                 y = if(mask) value_mask else value,
                 type = 'n', xlab = 'time', ylab = 'value',
                 main = paste('Currency type value over time in',v.value_currency,sep=' '),
                 #sub = as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z'),
                 ...)
            cl <- 1
            lg <- data.table()
            #all currencies
            for(v.currency_type in unique(currency_type)){
              cl <- cl+1
              lines(x = as.POSIXct(.SD[currency_type==v.currency_type,wallet_id], origin='1970-01-01', tz='UTC'),
                    y = .SD[currency_type==v.currency_type,if(mask) value_mask else value],
                    col = cl)
              #add to legend
              lg <- rbindlist(list(lg, data.table(legend_ = v.currency_type, col_ = cl)))
            }
            lg[,{
              case.legend(legend_, col_, verbose=verbose-1) # different legend for interarctive graphics device and for export due to scaling issue
            }]
            NULL
          }]
  
  # location over time
  x[J(FALSE,v.value_currency),#na_group==FALSE & value_currency==v.value_currency,
    list(value = sum(value,na.rm=T)),
    by=c('wallet_id','location')
    ][,
      `:=`(value_init = sum(.SD[wallet_id==min(wallet_id),value]))
      ][,
        `:=`(value_mask = value / value_init, value_init = NULL)
        ][order(wallet_id,location)
          ][,{
            plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
                 y = if(mask) value_mask else value,
                 type = 'n', xlab = 'time', ylab = 'value',
                 main = paste('Location value over time in',v.value_currency,sep=' '),
                 #sub = as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z'),
                 ...)
            cl <- 1
            lg <- data.table()
            #all currencies
            for(v.location in unique(location)){
              cl <- cl+1
              lines(x = as.POSIXct(.SD[location==v.location,wallet_id], origin='1970-01-01', tz='UTC'),
                    y = .SD[location==v.location,if(mask) value_mask else value],
                    col = cl)
              #add to legend
              lg <- rbindlist(list(lg, data.table(legend_ = if(nchar(v.location) > 10) paste0(substr(v.location,1,10),"...") else v.location, col_ = cl)))
            }
            lg[,{
              case.legend(legend_, col_, verbose=verbose-1) # different legend for interarctive graphics device and for export due to scaling issue
            }]
            NULL
          }]
  
  # location_type
  x[J(FALSE,v.value_currency),
    list(value = sum(value,na.rm=T)),
    by=c('wallet_id','location_type')
    ][,
      `:=`(value_init = sum(.SD[wallet_id==min(wallet_id),value]))
      ][,
        `:=`(value_mask = value / value_init, value_init = NULL)
        ][order(wallet_id,location_type)
          ][,{
            plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
                 y = if(mask) value_mask else value,
                 type = 'n', xlab = 'time', ylab = 'value',
                 main = paste('Location type value over time in',v.value_currency,sep=' '),
                 #sub = as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z'),
                 ...)
            cl <- 1
            lg <- data.table()
            #all currencies
            for(v.location_type in unique(location_type)){
              cl <- cl+1
              lines(x = as.POSIXct(.SD[location_type==v.location_type,wallet_id], origin='1970-01-01', tz='UTC'),
                    y = .SD[location_type==v.location_type,if(mask) value_mask else value],
                    col = cl)
              #add to legend
              lg <- rbindlist(list(lg, data.table(legend_ = v.location_type, col_ = cl)))
            }
            lg[,{
              case.legend(legend_, col_, verbose=verbose-1) # different legend for interarctive graphics device and for export due to scaling issue
            }]
            NULL
          }]
  
  # END OF plot window for matrix of plots
  
  # sign
  par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))
  par(new=TRUE)
  title(sub = x[J(FALSE,v.value_currency),list(sub = as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z'))][,sub], cex.sub = 0.7)
  mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
  par(new=FALSE)
  invisible(TRUE)
}

Rbitcoin.plot.trades <- function(x, ..., verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'Rbitcoin.plot.trades'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': performing plot',sep='')
  plot(x = x[['trades']][['date']], y = x[['trades']][['price']],
       type = 'l', xlab = 'time', ylab = 'price',
       main = paste(x[['market']],paste0(x[['base']],x[['quote']]),'trades',sep=' '),
       sub = as.character(x[['timestamp']], format = '%Y-%m-%d %H:%M:%S %Z'),
       ...)
  mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
  grid()
  invisible()
}

Rbitcoin.plot.order_book <- function(x, ..., verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'Rbitcoin.plot.order_book'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': performing plot',sep='')
  v_price <- c(x[['asks']][['price']],x[['bids']][['price']])
  v_amount <- c(x[['asks']][['cum_amount']],x[['bids']][['cum_amount']])
  plot(x = v_price, y = v_amount,
       type = 'n', xlab = 'price', ylab = 'cum amount',
       main = paste(x[['market']],paste0(x[['base']],x[['quote']]),'order book',sep=' '),
       sub = as.character(x[['timestamp']], format = '%Y-%m-%d %H:%M:%S %Z'),
       ...)
  lines(x = x[['asks']][['price']], y = x[['asks']][['cum_amount']])
  lines(x = x[['bids']][['price']], y = x[['bids']][['cum_amount']])
  mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
  grid()
  invisible()
}

# antiddos ----------------------------------------------------------

#' @title Anti DDoS
#'
#' @description Wait if you should before next API call to market (or any other source system) to do not get banned.
#'
#' @param market character, a unique name of source system, could be any name \code{c('kraken','bitstamp','blockchain','alt_bitstamp')} 
#' @param antispam_interval numeric time in seconds between API calls on the particular source system, defeault \code{10s}.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used, by default \code{0}.
#' @return numeric time of wait in seconds.
#' @section Side effect:
#' Environment of name \code{Rbitcoin.last_api_call} in \code{.GlobalEnv} which holds the timestamps of last api call per \code{market} during the R session.
#' @seealso \code{\link{market.api.process}}, \code{\link{wallet_manager}}
#' @export
#' @examples
#' \dontrun{
#' # run below code in a batch
#' wait <- antiddos(market = 'kraken', antispam_interval = 5, verbose = 1)
#' market.api.process('kraken',c('BTC','EUR'),'ticker')
#' wait2 <- antiddos(market = 'kraken', antispam_interval = 5, verbose = 1)
#' market.api.process('kraken',c('BTC','EUR'),'ticker')
#' }
antiddos <- function(market, antispam_interval = 10, verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'antiddos'
  wait <- 0
  if(!is.null(Rbitcoin.last_api_call[[market]])){
    wait <- as.numeric(get(market,Rbitcoin.last_api_call)) + antispam_interval - as.numeric(Sys.time())
    if(wait>0){
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': waiting for ',round(wait,2),'s as anti-ddos for api: ',market,sep='')
      Sys.sleep(wait)
    }
    else wait <- 0
  }
  assign(market, Sys.time(), envir = Rbitcoin.last_api_call)
  return(wait)
}

# antiddos cache
Rbitcoin.last_api_call <- new.env()

# wallet manager value calculation ------------------------------------------------

#' @title Get rate
#' @description Download ask-bid data for provided market and currency pair, supports bitcoin markets and yahoo.
#' @keywords internal
get_rate <- function(v_market, v_base, v_quote, antispam_interval, api.dict, verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'get_rate'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': downloading exchange rate ',v_base,v_quote,' from ',v_market,sep='')
  if(v_market == 'yahoo'){
    ab_dt <- tryCatch(
      expr = {
        raw_char <- getURL(url = paste0('https://download.finance.yahoo.com/d/quotes.csv?s=',paste0(v_base,v_quote),'=X&f=sl1d1t1ab&e=.csv'),
                           .opts = list(useragent = paste(paste("R",packageVersion("base")),paste("Rbitcoin",packageVersion("Rbitcoin")), sep="::")))
        rate_dt <- fread(raw_char)
        setnames(rate_dt,c('currency_pair','last','date','time','ask','bid'))
        rate_dt[,list(ask, bid)]
      },
      error = function(e){
        msg <- paste0('error on downloading ',v_base,v_quote,' exchange rate from ',v_market)
        if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': ',msg,': ',e[["message"]],sep='')
        warning(paste0(msg,': ',e[["message"]]), call. = FALSE)
        data.table(ask = NA_real_, bid = NA_real_)
      }
    )
  }
  else{
    ab_dt <- tryCatch( #intentional verbose - 2
      expr = market.api.process(v_market,c(v_base,v_quote), 'ticker', api.dict = api.dict, verbose = verbose - 1)[,list(ask, bid)],
      error = function(e){
        msg <- paste0('error on downloading ',v_base,v_quote,' exchange rate from ',v_market)
        if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': ',msg,': ',e[["message"]],sep='')
        warning(paste0(msg,': ',e[["message"]]), call. = FALSE)
        data.table(ask = NA_real_, bid = NA_real_)
      }
    )
  }
  ab_dt
}

#' @title Wallet value
#' @description Defines the currency pairs required to cover needed exchange rates and chose the source according to rate priority, also utilize the transfer_currency_pair when no direct currency pair available.
#' @note Setting option "Rbitcoin.archive_exchange_rate" to numeric 0, 1 o 2 will lead to archive exchange rates used during the processing. Value 0 for rds file, 1 for csv (dec='.', sep=','), 2 for csv (dec=',', sep=';'). Read \code{\link{wallet_manager}} manual.
#' @keywords internal
wallet_value <- function(wallet_dt, 
                         ct.dict = getOption("Rbitcoin.ct.dict"),
                         value_currency, 
                         value_currency_type = NULL,
                         rate_priority,
                         transfer_currency_pair,
                         antispam_interval,
                         api.dict = NULL, 
                         verbose = getOption("Rbitcoin.verbose",0)){
  fun_name <- 'wallet_value'
  
  # currency type dict
  ct_dt <- rbindlist(lapply(1:length(ct.dict), function(i) data.table(currency_type = names(ct.dict[i]), currency = ct.dict[[i]])))[,.SD,,keyby='currency']
  
  if(is.null(api.dict)) data("api.dict", package = "Rbitcoin", envir = environment())
  # data.table 1.9.2 presort workaround for bug
  api.dict <- api.dict[order(market,base,quote,action)]
  setkey(api.dict,market,base,quote,action)
  
  # api.dict filtering to ticker and only top priority (from rate_priority) currency_pair order independent: sort(c(base,quote))
  rate_priority_dt <- data.table(market = rate_priority, priority = 1:length(rate_priority), key = 'market')
  
  priority.ticker.api.dict <- 
    rate_priority_dt[api.dict[action=='ticker'
                              ],list(market,priority,base,quote),nomatch=0
                     ][,list(currency_pair = paste(sort(c(base,quote)),collapse='')), by=c('market','base','quote','priority')
                       ][order(currency_pair,priority)
                         ][,head(.SD,1),keyby=c('currency_pair'),.SDcols=c('market','base','quote')
                           ]
  currency_to_rate <- 
    wallet_dt[,unique(data.table(currency, currency_type))
              ]
  
  direct_pair <- 
    priority.ticker.api.dict[currency_to_rate[,list(currency_pair = paste(sort(c(currency,value_currency)),collapse='')), by=c('currency','currency_type')
                                              ][,list(currency, currency_type),keyby='currency_pair'
                                                ],list(currency, currency_type, market, base, quote) # data.table 1.9.3: ],list(currency_pair, currency, currency_type, market, base, quote)
                             ][value_currency_type=='fiat' & currency_type=='fiat',`:=`(market = 'yahoo', base = currency, quote = value_currency)
                               ]
  to_transfer_pair <- {
    direct_to_indirect <- direct_pair[is.na(quote) & !is.na(currency_type)]
    if(nrow(direct_to_indirect) > 0){
      ct_vec <- ct_dt[,currency_type]
      names(ct_vec) <- ct_dt[,currency]
      res <- priority.ticker.api.dict[
        direct_to_indirect[,
                           list(currency_pair = paste(sort(c(currency,transfer_currency_pair[[currency_type]])),collapse='')), 
                           by=c('currency','currency_type')
                           ][,
                             list(currency, currency_type),
                             keyby='currency_pair'
                             ],
        ][intersect(
          which(ct_vec[substr(currency_pair,1,3)]=='fiat'),
          which(ct_vec[substr(currency_pair,4,6)]=='fiat')
          ),
          `:=`(market = 'yahoo', base = transfer_currency_pair[['fiat']], quote = currency)
          ][,list(currency_pair, currency, currency_type, market, base, quote)
            ]
    } 
    if(nrow(direct_to_indirect) == 0) 
      res <- data.table(currency_pair = character(), currency = character(), currency_type = character(), market = character(), base = character(), quote = character())
    res
  }
  transfer_pair <-
    priority.ticker.api.dict[currency_pair==paste(sort(transfer_currency_pair),collapse=''),
                             list(currency_pair, currency = transfer_currency_pair[[ifelse(value_currency_type=='fiat','crypto','fiat')]], currency_type = ifelse(value_currency_type=='fiat','crypto','fiat'), market, base, quote)
                             ]
  
  from_transfer_pair <- {
    if(value_currency_type == 'fiat'){
      res <- data.table(currency_pair = paste(sort(c(value_currency,transfer_currency_pair[['fiat']])),collapse=''),
                        currency = transfer_currency_pair[['fiat']],
                        currency_type = 'fiat',
                        market = 'yahoo', 
                        base = transfer_currency_pair[['fiat']], quote = value_currency)
    } else if(value_currency_type == 'crypto'){
      dt <- data.table(currency_pair = paste(sort(c(value_currency,transfer_currency_pair[['crypto']])),collapse=''),
                       currency = transfer_currency_pair[['crypto']],
                       currency_type = 'crypto', 
                       key = 'currency_pair')
      res <- priority.ticker.api.dict[dt, list(currency, currency_type, market, base, quote)]
    }
    res
  }
  
  all_pair <- rbindlist(list(
    direct_pair,
    to_transfer_pair,
    transfer_pair,
    from_transfer_pair
  ))[,head(.SD,1),keyby='currency_pair'
     ]
  
  # download exchange rates all pairs used for direct and indirect
  for(i in 1:nrow(all_pair)){
    # exceptions
    if(all_pair[i,identical(substr(currency_pair,1,3),substr(currency_pair,4,6))]){# 1:1 rate
      ab <- all_pair[i,list(
        market = NA_character_, 
        base = substr(currency_pair,1,3), 
        quote = substr(currency_pair,4,6), 
        ask = 1, 
        bid = 1
         )]
    }
    else if(all_pair[i,!is.na(market)]){ #regular rate - download from ticker/yahoo
      wait <- antiddos(market = all_pair[i,market], antispam_interval = antispam_interval, verbose = verbose - 1)
      ab <- all_pair[i,get_rate(v_market = market, v_base = base, v_quote = quote, antispam_interval = antispam_interval, api.dict = api.dict, verbose = verbose - 1)]
      ab <- all_pair[i,data.table(market, base, quote, ab)]
      if(!ab[,is.numeric(ask) & is.numeric(bid)]) stop(all_pair[i,paste0("Incorrect ask-bid data returned for ",base,quote," pair from ",market,sep="")],call.=FALSE)
    }
    else {
      #if(all_pair[i,is.na(market)]){
      ab <- data.table(market = NA_character_, base = NA_character_, quote = NA_character_, ask = NA_real_, bid = NA_real_) # postponed to second loop for indirect rates
    }
    set(all_pair, i, "market", ab[,market])
    set(all_pair, i, "base", ab[,base])
    set(all_pair, i, "quote", ab[,quote])
    set(all_pair, i, "ask", ab[,ask])
    set(all_pair, i, "bid", ab[,bid])
  }
  
  # calc indirect COMPOUND rate calculation based on rate table
  for(i in 1:nrow(all_pair)){
    if(all_pair[i,!is.na(market) | (!is.na(ask) & !is.na(bid))]) next # pair already (should be) calculated, when error occur it may not be calculated
    if(all_pair[i,is.na(currency_type)]) next # cannot calculate
    
    # transfer in
    if(all_pair[i,currency == transfer_currency_pair[[currency_type]]]){ # currency already in the transfer pair
      transfer_in_ab <- all_pair[FALSE]
    } else{
      v.currency_pair <- all_pair[i,paste(sort(c(currency,transfer_currency_pair[[currency_type]])),collapse='')]
      transfer_in_ab <- all_pair[J(v.currency_pair)]
    }
    
    # transfer through
    if(all_pair[i,currency_type!=value_currency_type]){
      v.currency_pair <- all_pair[i,paste(sort(c(transfer_currency_pair)),collapse='')]
      transfer_ab <- all_pair[J(v.currency_pair)]
    } else{
      transfer_ab <- all_pair[FALSE]
    }
    
    # transfer out
    if(all_pair[i,value_currency == transfer_currency_pair[[value_currency_type]]]){ # currency already in the transfer pair
      transfer_out_ab <- all_pair[FALSE]
    } else{
      v.currency_pair <- all_pair[i,paste(sort(c(value_currency,transfer_currency_pair[[value_currency_type]])),collapse='')]
      transfer_out_ab <- all_pair[J(v.currency_pair)]
    }
    
    # combine
    indirect_pair <- rbindlist(
      list(transfer_in_ab,transfer_ab,transfer_out_ab)
    )
    #if(indirect_pair[,any(is.na(market))]) next
    #else{
      set(all_pair, i, "market", paste(
        c(transfer_in_ab[,market],transfer_ab[,market],transfer_out_ab[,market]),
        collapse=','))
      set(all_pair, i, "base", paste(
        c(transfer_in_ab[,ifelse(currency==base, base, quote)],transfer_ab[,ifelse(currency==base, base, quote)],transfer_out_ab[,ifelse(currency==base, base, quote)]),
        collapse=','))
      set(all_pair, i, "quote", paste(
        c(transfer_in_ab[,ifelse(currency==quote, base, quote)],transfer_ab[,ifelse(currency==quote, base, quote)],transfer_out_ab[,ifelse(currency==quote, base, quote)]),
        collapse=','))
      set(all_pair, i, "ask", prod(c(transfer_in_ab[,ifelse(currency==base, ask, 1/bid)],transfer_ab[,ifelse(currency==base, ask, 1/bid)],transfer_out_ab[,ifelse(currency==base, ask, 1/bid)]),na.rm=F))
      set(all_pair, i, "bid", prod(c(transfer_in_ab[,ifelse(currency==base, bid, 1/ask)],transfer_ab[,ifelse(currency==base, bid, 1/ask)],transfer_out_ab[,ifelse(currency==base, bid, 1/ask)]),na.rm=F))
    #}
  } # end of indirect rate calculation
  
  # add rate and value
  wallet_dt[,rowid:=1:.N][,currency_pair := paste(sort(c(currency,value_currency)),collapse=''), by='rowid'][,rowid:=NULL]
  setkey(wallet_dt,currency_pair)
  # DUMP RATE TABLE - option
  if(!is.null(getOption("Rbitcoin.archive_exchange_rate"))){
    if(getOption("Rbitcoin.archive_exchange_rate")==0){
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': extract rate table to exchange_rate_archive.rds',sep='')
      exchange_rate_dt.archive <- if(file.exists('exchange_rate_archive.rds')) readRDS('exchange_rate_archive.rds') else NULL
      if(nrow(wallet_dt) > 0){
        exchange_rate_dt.archive <- rbindlist(list(
          exchange_rate_dt.archive, 
          data.table(value_rate_id = as.integer(Sys.time()), all_pair)
        ))
        saveRDS(exchange_rate_dt.archive,'exchange_rate_archive.rds')
      }
    }
    else if(!(getOption("Rbitcoin.archive_exchange_rate") %in% c(1,2))) stop("getOption('Rbitcoin.archive_exchange_rate') should be in 0:2 for saveRDS, write.csv, write.csv2. See examples in manual")
    else{
      sep <- switch(getOption("Rbitcoin.archive_exchange_rate"),
                    "1" = ",",
                    "2" = ";")
      dec <- switch(getOption("Rbitcoin.archive_exchange_rate"),
                    "1" = ".",
                    "2" = ",")
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': extract rate table to exchange_rate_archive.csv',sep='')
      if(file.exists('exchange_rate_archive.csv')){
        tryCatch(expr = write.table(data.table(value_rate_id = as.integer(Sys.time()), all_pair),
                                    "exchange_rate_archive.csv",
                                    sep=sep,dec=dec,row.names=FALSE,quote=FALSE,append=TRUE,col.names=FALSE),
                 error = function(e){
                   warning(paste0("Not possible to write exchange rates data to exchange_rate_archive.csv File may be locked by other application. Error returned: ",e[['message']]), call.=FALSE)
                   NULL
                 })
      } else {
        write.table(data.table(value_rate_id = as.integer(Sys.time()), all_pair),
                    "exchange_rate_archive.csv",
                    sep=sep,dec=dec,row.names=FALSE,quote=FALSE,append=FALSE,col.names=TRUE)
      }
    }
  }
  
  v.value_currency <- value_currency #avoid eval(value_currency) in dt below to avoid names collision
  # askbid swap for reversed currency pairs
  f.askbid_value <- function(currency, value_currency, base, quote, ask, bid){
    ifelse(
      (value_currency==substr(base,1,3) & currency==substr(quote,nchar(quote)-2,nchar(quote))),
      1/ask,
      ifelse(
        currency==substr(base,1,3) & value_currency==substr(quote,nchar(quote)-2,nchar(quote)),
        bid,
        NA_real_))
  }
  # calc value on wallet data
  all_pair[,list(base,quote,ask,bid),keyby='currency_pair'
           ][wallet_dt,list(
             wallet_id,currency,currency_type, timestamp, location, location_type, amount, 
             value_currency = v.value_currency,
             base, quote, ask, bid
           )][(!is.na(ask) & !is.na(bid)),
              value_rate := f.askbid_value(currency, value_currency, base, quote, ask, bid)
              ][,
                list( # just a clean return list of columns
                  wallet_id, currency, currency_type, timestamp, location, location_type, amount, 
                  value_currency, value_rate, 
                  value = amount * value_rate
                )]
}

# CRAN check NOTE prevention
if(getRversion() >= "2.15.1") utils::globalVariables(c('action','amount','amount_in_orders','ask','base','bid','col_','currency','currency_pair','currency_type','final_balance','J','legend_','location','location_type','market','price','priority','rowid','type','value','value_currency','value_init','value_mask','value_rate','wallet_id'))
