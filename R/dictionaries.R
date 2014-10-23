
# api_dict ----------------------------------------------------------------

#' @name api_dict
#' @title API dictionary
#' @description By default provided to \code{getOption("Rbitcoin.api.dict")}. This function returns built-in Rbitcoin data set contains API dictionary for \code{\link{market.api.process}} function to perform pre-process API call arguments, post-process API call results and catch market level errors. Still there is function \code{\link{market.api.query}} that do not require any dictionary and can operate on any currency pair. Granularity of api dictionary data is \code{c("market", "base", "quote", "action")}. This dictionary can be edited/extended by user for new markets, currency pairs and actions.\cr Currently supported currency pairs:
#' \itemize{
#' \item \code{bitstamp: BTCUSD}
#' \item \code{btce: BTCUSD, LTCUSD, LTCBTC, NMCBTC}
#' \item \code{kraken: BTCEUR, LTCEUR, BTCLTC}
#' \item \code{bitmarket: BTCPLN, LTCPLN}
#' }
#' @details Default dictionary post-process function returns list or data.table. The data.table is returned in case of (always) one row response from API (ticker, place_limit_order, etc.). The list is returned in case of (possible) multiple rows response from API (trades, wallet, etc.).
#' @note Do not use \code{api.dict} from untrusted source or read whole it's code to ensure it is safe! The api dictionary was not fully tested, please follow the examples, if you find any bugs please report.
#' @export
#' @aliases api.dict
#' @examples
#' \dontrun{
#' api_dict()[]
#' }
api_dict <- function(){
  api.dict.list <- list(
    kraken_api_dict(),
    bitstamp_api_dict(),
    btce_api_dict(),
    bitmarket_api_dict()
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
    data.table(market = "bitmarket", query = c(market.api.query.bitmarket))
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
