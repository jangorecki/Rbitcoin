
# api_dict ----------------------------------------------------------------

#' @name api_dict
#' @title API dictionary
#' @description By default provided to \code{getOption("Rbitcoin.api.dict")}. This function returns built-in Rbitcoin data set contains API dictionary for \code{\link{market.api.process}} function to perform pre-process API call arguments, post-process API call results and catch market level errors. Still there is function \code{\link{market.api.query}} that do not require any dictionary and can operate on any currency pair. Granularity of api dictionary data is \code{c("market", "base", "quote", "action")}. This dictionary can be edited/extended by user for new markets, currency pairs and actions.\cr Currently supported markets, currency pairs, actions use: \code{api_dict()[!is.na(base), .(market, currency_pair = paste0(base,quote))][,unique(.SD)]}
#' @details Default dictionary post-process function returns list or data.table. The data.table is returned in case of (always) one row response from API (ticker, place_limit_order, etc.). The list is returned in case of (possible) multiple rows response from API (trades, wallet, etc.).
#' @note Do not use \code{api.dict} from untrusted source or read whole it's code to ensure it is safe! The api dictionary was not fully tested, please follow the examples, if you find any bugs please report.
#' @section Interface in/out exceptions:
#' \itemize{
#' \item Only 3 letters currency codes are supported (USD, GBP, BTC, etc.), others (e.g. DOGE) were not tested and might not work.
#' \item bitstamp private api calls requires additional param \code{client_id}, see bitstamp api docs in references.
#' \item hitbtc \code{cancel_order} action requires extended \code{req}, see examples below. See repo \code{hitbtc-api} issue #3.
#' \item hitbtc \code{trades} action for recent trades (no \code{tid} param) will include content of returned \code{type} field, but in case of method for trades since \code{tid} param then its field is empty. Open issue in repo \code{hitbtc-com/hitbtc-api} issue #4.
#' \item hitbtc \code{wallet} action will return balance of the hitbtc trading subaccount, see examples below for hitbtc payment (main account) balance query. Also see \code{?wallet_manager} examples for hitbtc main balance in wallet manager.
#' \item following markets - kraken, bitmarket, hitbtc - supports \code{tid} (aka \code{since}) parameter to trades action. See examples for full history trades downloading.
#' }
#' @references API documentation: \url{https://www.bitstamp.net/api/}, \url{https://btc-e.com/api/documentation}, \url{https://www.kraken.com/help/api}, \url{https://www.bitmarket.pl/docs.php?file=api_private.html}, \url{https://github.com/hitbtc-com/hitbtc-api}
#' @export
#' @aliases api.dict
#' @examples
#' \dontrun{
#' api_dict()[]
#' 
#' # hitbtc cancel order exception
#' #req = list(oid = "") # as it is for other markets
#' req = list(oid = "",
#'            symbol = "BTCUSD",
#'            side = "sell") # issue open: https://github.com/hitbtc-com/hitbtc-api/issues/3
#' cancel_order <- market.api.process(market = 'hitbtc', action = 'cancel_order',
#'                                    req = req, key = '', secret = '')
#' 
#' # hitbtc payment (main account) balance
#' r <- market.api.query(market="hitbtc", url="https://api.hitbtc.com/api/1/payment/balance",
#'                       key="", secret="")
#' 
#' # historical data FAST: bitcoincharts full archive
#' browseURL("http://api.bitcoincharts.com/v1/csv/")
#' # download particular market data dump, extract and load using fread
#' trades <- fread(".hitbtcEUR.csv")
#' trades[,`:=`(date = as.POSIXct(V1,origin="1970-01-01", tz="UTC"),
#'              price = V2, amount = V3,
#'              tid = NA_character_, type = NA_character_)
#'        ][,c("V1","V2","V3"):=NULL][]
#' 
#' # historical data SLOW: loop using `tid` param - works only on kraken, hitbtc, bitmarket
#' batch_size <- 1000 # kraken 1000, hitbtc 1000, bitmarket 500
#' last_tid <- 0 # from the beginning
#' trades <- data.table()
#' repeat{
#'   trades_batch = tryCatch(
#'     market.api.process(market="kraken",currency_pair=c("BTC","LTC"), action="trades",
#'                        req=list(tid = last_tid))[["trades"]],
#'     error = function(e){
#'       message(e[["message"]])
#'       invisible(NULL)
#'     }
#'   )
#'   if(is.null(trades_batch)) next # error, skip
#'   if(nrow(trades_batch)==0) break # last batch empty
#'   last_tid <- trades_batch[length(tid),tid]
#'   trades <- rbindlist(list(trades, trades_batch))
#'   cat("nrow(trades): ",nrow(trades),"\n",sep="")
#'   if(nrow(trades_batch) < batch_size) break # last batch
#' }
#' saveRDS(trades,"trades_full_history.rds") # backup
#' # also it do not scale well
#' # instead of rbindlist use write.csv(append=TRUE) or dbWriteTable(append=TRUE)
#' print(trades)
#' }
api_dict <- function(){
  api.dict.list <- list(
    kraken_api_dict(),
    bitstamp_api_dict(),
    btce_api_dict(),
    bitmarket_api_dict(),
    hitbtc_api_dict()
    )
  api.dict <- rbindlist(api.dict.list)
  setkeyv(api.dict,c("market","base","quote","action"))
}

# query_dict --------------------------------------------------------------

#' @name query_dict
#' @title Market's API query function dictionary
#' @description By default provided to \code{getOption("Rbitcoin.query.dict")}. This function returns built-in Rbitcoin supported markets dictionary. Granularity of query dictionary data is \code{c("market")}. This dictionary can be edited/extended by user for new markets.\cr Currently supported:
#' \itemize{
#' \item \code{kraken}
#' \item \code{bitstamp}
#' \item \code{btce}
#' \item \code{bitmarket}
#' \item \code{hitbtc}
#' }
#' @note Do not use \code{query.dict} from untrusted source or read whole it's code to ensure it is safe! The api dictionary was not fully tested, please follow the examples, if you find any bugs please report.
#' @export
#' @aliases query.dict
#' @examples
#' \dontrun{
#' query_dict()[]
#' }
query_dict <- function(){
  query.dict.list <- list(
    data.table(market = "kraken", query = c(market.api.query.kraken)),
    data.table(market = "bitstamp", query = c(market.api.query.bitstamp)),
    data.table(market = "btce", query = c(market.api.query.btce)),
    data.table(market = "bitmarket", query = c(market.api.query.bitmarket)),
    data.table(market = "hitbtc", query = c(market.api.query.hitbtc))
  )
  query.dict <- rbindlist(query.dict.list)
  setkeyv(query.dict,"market")
}

# ct_dict -----------------------------------------------------------------

#' @name ct_dict
#' @title Currency type (fiat or crypti) dictionary
#' @description By default provided to \code{getOption("Rbitcoin.ct.dict")}. Simple crypto/fiat indicator, used in few places, can be extended here.
#' @export
#' @aliases ct.dict
ct_dict <- function(){
  list(
    crypto = c('BTC','LTC','NMC','FTC','NVC','PPC','TRC','XPM','XDG','XRP','XVN'),
    fiat = c('USD','EUR','GBP','KRW','PLN','RUR','JPY','CHF','CAD','AUD','NZD','CNY','INR',
             'TRY','SYP','GEL','AZN','IRR','KZT','NOK','SEK','ISK','MYR','DKK','BGN','HRK',
             'CZK','HUF','LTL','RON','UAH','IDR','IQD','MNT','BRL','ARS','VEF','MXN')
  )
}
