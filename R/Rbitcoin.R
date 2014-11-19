
# Rbitcoin - package level data ------------------------------------------------------------------

#' @title R & bitcoin integration
#'
#' @description Utilities related to Bitcoin and other cryptocurrencies. Core functionalities are:
#' \itemize{
#' \item \code{fromBTC}, \code{toBTC} - fast conversion based on blockchain.info rates.
#' \item \code{market.api.query} - launch query on markets API. Both public and private API calls supported. All methods and all currency pairs supported.
#' \item \code{market.api.process} - launch query in a common way (for all markets), receive results in a common structure (for all markets). It will perform pre-process of API request, post-process of API results, and catch market error. Requires API dictionary definition, for built-in dictionary see \code{\link{api.dict}}.
#' \item \code{blockchain.api.query} - launch query on blockchain.info API json interface.
#' \item \code{blockchain.api.process} - launch query and postprocess results.
#' \item \code{rbtc.plot} - visualize the data returned by some Rbitcoin functions.
#' \item \code{wallet_manager} - track the assets amounts and values in multiple source systems.
#' \item \code{antiddos} - built-in antiddos procedure to prevent to be banned.
#' }
#' You need to note that imported \strong{digest} package docs states: \emph{Please note that this package is not meant to be deployed for cryptographic purposes for which more comprehensive (and widely tested) libraries such as OpenSSL should be used}. Still \strong{digest} is one of the top downloaded package from CRAN.\cr
#' It is advised to maintain your API keys security level as tight as possible, if you do not need withdraw api method be sure to disable it for api keys.\cr
#' SSL will be used by default. It can be customized using \code{options("RCurlOptions")}, see examples or read \strong{RCurl} docs.\cr
#' In case of SSL error try update certificate CA file (\code{cacert.pem} in \code{cainfo = system.file("CurlSSL","cacert.pem",package="RCurl")}), see references for CA file source. Alternatively you can always disable SSL.\cr
#' At the time of writing the most recent markets API version were used:
#' \itemize{
#' \item kraken v0
#' \item bitstamp v2 (public) / ? (private)
#' \item btce v3 (public) / v1 (private)
#' \item bitmarket v2
#' \item hitbtc v1
#' }
#' 
#' For package-level options see examples below.
#' 
#' If you find this package useful or you made profit from using it please consider donation to Rbitcoin: \url{bitcoin:15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi}.\cr
#' Part of the received donations will likely be transferred to \strong{Rbitcoin} dependencies if and when they will setup BTC donation address: \strong{data.table}, \strong{RCurl}, \strong{jsonlite}, \strong{digest}, \strong{R Foundation}.
#' 
#' There is also simple GUI app for \strong{Rbitcoin} package \strong{shinyBTC}: \url{https://github.com/jangorecki/shinyBTC}
#' 
#' @seealso \code{\link{market.api.process}}, \code{\link{blockchain.api.process}}, \code{\link{antiddos}}, \code{\link{fromBTC}}, \code{\link{wallet_manager}}, \code{\link{rbtc.plot}}, \code{\link{api.dict}}
#' @references Issues report: \url{https://github.com/jangorecki/Rbitcoin/issues}\cr Example SSL CA file source: \url{http://curl.haxx.se/docs/caextract.html}
#' @docType package
#' @import RCurl digest jsonlite data.table
#' @name Rbitcoin
#' @aliases btc rbtc bitcoin BTC
#' @examples
#' \dontrun{
#' # default options used by Rbitcoin
#' 
#' # turn off sci notation of numbers, important for market API calls
#' options(scipen=100)
#' 
#' # RCurl
#' O <- getOption("RCurlOptions")
#' O$ssl.verifyhost <- as.integer(2)
#' O$ssl.verifypeer <- TRUE
#' O$cainfo <- system.file("CurlSSL","cacert.pem",package="RCurl")
#' options("RCurlOptions" = O)
#' 
#' # Rbitcoin
#' options(Rbitcoin.verbose = 0) # 1+ will invoke Rbitcoin processing messages
#' options(Rbitcoin.antiddos = TRUE) # ?antiddos
#' options(Rbitcoin.antiddos.sec = 10) # ?antiddos
#' options(Rbitcoin.antiddos.fun = antiddos_fun) # ?antiddos
#' options(Rbitcoin.antiddos.verbose = 0) # ?antiddos
#' options(Rbitcoin.cancel_order.order_not_found = NULL) # ?api.dict
#' options(Rbitcoin.json.debug = FALSE) # ?market.api.query
#' options(Rbitcoin.plot.mask = FALSE) # ?rbtc.plot
#' options(Rbitcoin.plot.limit_pct = Inf) # ?rbtc.plot
#' options(Rbitcoin.archive_exchange_rate = FALSE) # ?wallet_manager
#' options(Rbitcoin.wallet_manager.archive_path = "wallet_archive.rds") # ?wallet_manager
#' 
#' # Rbitcoin dictionaries
#' options(Rbitcoin.query.dict = query_dict()) # ?query.dict
#' options(Rbitcoin.api.dict = api_dict()) # ?api.dict
#' options(Rbitcoin.ct.dict = ct_dict()) # ?ct.dict
#' 
#' # data.table 1.9.4 bug #858, will be removed 1.9.6+
#' options(datatable.auto.index=FALSE)
#' }
NULL

# CRAN check NOTE prevention ----------------------------------------------

if(getRversion() >= "2.15.1") utils::globalVariables(c('action','amount','amount_in_orders','ask','base','bid','col_','currency','currency_pair','currency_type','final_balance','J','legend_','location','location_type','market','price','priority','rowid','type','value','value_currency','value_init','value_mask','value_rate','wallet_id','amount.x','amount.y','id','rate','oid','cum_amount','tid','query','NA_group','auth','new_address','new_auth','.','label','lot','volume','side','currency_code','cash','reserved','clientOrderId','quantity','symbol','orderPrice','quantityLeaves','amount_lot'))
