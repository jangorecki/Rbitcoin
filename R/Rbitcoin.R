
# Rbitcoin - package level data ------------------------------------------------------------------

#' @title R & bitcoin integration
#'
#' @description Utilities related to Bitcoin and other cryptocurrencies. Core functionalities are:
#' \itemize{
#' \item \code{fromBTC} \code{toBTC} - fast conversion based on blockchain.info.
#' \item \code{market.api.query} - launch query on market's API (\code{bitstamp}, \code{btce}, \code{kraken}, \code{bitmarket}). Both public and private API calls supported. All methods and all currency pairs supported.
#' \item \code{market.api.process} - integration of market's processing structures: pre-process of API request, post-process API results, market error catching. Input and output common structure across markets. Requires API dictionary definition, for details of package built-in dictionary see \code{\link{api.dict}}.
#' \item \code{blockchain.api.query} - launch query on blockchain.info API json interface.
#' \item \code{blockchain.api.process} - postprocess blockchain api result, transform to \code{data.table}.
#' \item \code{plot.btc} - illustrate the data returned by some Rbitcoin functions.
#' \item \code{wallet_manager} - track the assets amounts and values in multiple wallet sources.
#' \item \code{antiddos} - built-in antiddos procedure to prevent to be banned.
#' }
#' You need to note that imported \code{digest} package docs states: \emph{Please note that this package is not meant to be deployed for cryptographic purposes for which more comprehensive (and widely tested) libraries such as OpenSSL should be used}. Still \code{digest} is one of the top downloaded package from CRAN.\cr
#' It is advised to maintain your API keys security level as tight as possible, if you do not need withdraw api method be sure to disable it for api keys.\cr
#' You can print debug messages of \code{Rbitcoin} to console using verbose argument in FUNs or \code{options("Rbitcoin.verbose" = 1)}.\cr
#' SSL will be used by default. It can be customized using \code{options("RCurlOptions")}, see examples or read \code{RCurl} docs.\cr
#' In case of SSL error try update certificate CA file (\code{cacert.pem} in \code{cainfo = system.file("CurlSSL","cacert.pem",package="RCurl")}), see references for CA file source. Alternatively you can always disable SSL.\cr
#' At the time of writing the most recent market's API version were used:
#' \itemize{
#' \item bitstamp v2 (public) / ? (private)
#' \item btce v3 (public) / v1 (private)
#' \item kraken v0
#' \item bitmarket v2
#' }
#' 
#' For package-level options see examples.
#' 
#' BTC donation: \url{bitcoin:15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi}
#' 
#' @seealso \code{\link{market.api.process}}, \code{\link{blockchain.api.process}}, \code{\link{wallet_manager}}, \code{\link{plot.btc}}, \code{\link{api.dict}}, \code{\link{fromBTC}}
#' @references Issues report: \url{https://github.com/jangorecki/Rbitcoin/issues}\cr Example SSL CA file source: \url{http://curl.haxx.se/docs/caextract.html}
#' @docType package
#' @import RCurl digest jsonlite data.table
#' @name Rbitcoin
#' @aliases btc bitcoin BTC
#' @examples
#' \dontrun{
#' # default options used by Rbitcoin
#' 
#' # print Rbitcoin processing to console set "Rbitcoin.verbose" to 1 (or more)
#' options(Rbitcoin.verbose = 0)
#' 
#' # turn off sci notation of numbers, important for market API calls
#' options(scipen=100)
#' 
#' # print RCurl processing to console set O$verbose to TRUE
#' O <- getOption("RCurlOptions")
#' O$ssl.verifyhost <- as.integer(2)
#' O$ssl.verifypeer <- TRUE
#' O$cainfo <- system.file("CurlSSL","cacert.pem",package="RCurl")
#' options("RCurlOptions" = O)
#' 
#' # some other options
#' options(Rbitcoin.antiddos = TRUE) # ?antiddos
#' options(Rbitcoin.antiddos.sec = 10) # ?antiddos
#' options(Rbitcoin.antiddos.fun = antiddos_fun) # ?antiddos
#' options(Rbitcoin.antiddos.verbose = 0) # ?antiddos
#' options(Rbitcoin.json.debug = FALSE) # ?market.api.query
#' 
#' # currency type dict
#' options(Rbitcoin.ct.dict = ct_dict()) # ?ct.dict
#' # query dict
#' options(Rbitcoin.query.dict = query_dict()) # ?query.dict
#' # api dict
#' options(Rbitcoin.api.dict = api_dict()) # ?api.dict
#' }
NULL

# CRAN check NOTE prevention ----------------------------------------------

if(getRversion() >= "2.15.1") utils::globalVariables(c('action','amount','amount_in_orders','ask','base','bid','col_','currency','currency_pair','currency_type','final_balance','J','legend_','location','location_type','market','price','priority','rowid','type','value','value_currency','value_init','value_mask','value_rate','wallet_id','amount.x','amount.y','id','rate','oid','cum_amount','tid','query'))
