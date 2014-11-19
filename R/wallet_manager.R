
# wallet manager ----------------------------------------------------------

#' @title Wallet Manager
#'
#' @description Downloads wallet balance from multiple sources and calculate value in chosen currency based on actual exchange rates. Function is limited to dictionary \code{\link{api.dict}} plus fiat-fiat exchange rates.
#'
#' @param market.sources list of market sources definition, see examples. Mandatory fields: \code{market, key, secret} (for bitstamp also \code{client_id}).
#' @param blockchain.sources list of blockchain sources definition, see examples. Mandatory field: \code{address}.
#' @param manual.sources list of manually provided amounts, see examples. Mandatory fields: \code{currency, amount}, optional field: \code{location, location_type}.
#' @param min_amount numeric used to filter out near zero amounts of source currency, default \code{0.0001}.
#' @param value_calc logical calculate value, by default \code{TRUE}, can be turned off by setting to \code{FALSE}. Process will be slightly faster due to no API calls for exchange rates.
#' @param value_currency character default \code{"USD"}, target currency in which measure the current value.
#' @param value_currency_type character, optional for most currencies, if \code{value_currency} is an exotic currency you need to define its currency type ('crypto' or 'fiat') in this param or update \code{getOption("Rbitcoin.ct.dict")} param.
#' @param rate_priority character vector of market and priorioties for sourcing exchange rates, this param needs to be maintained by user, read Exchange rates note below. Example param value \code{rate_priority = c('bitstamp','kraken','hitbtc','bitmarket','btce')}.
#' @param transfer_currency_pair vector length of 2 of named character, default \code{c(crypto = "BTC", fiat = "USD")}, read Exchange rates note below.
#' @param archive_write logical, default \code{FALSE}, recommended \code{TRUE}. If \code{TRUE} wallet manager result will be archived to \code{"wallet_archive.rds"} file in the working directory, read Wallet archive note below.
#' @param archive_read logical, default \code{FALSE}, recommended \code{FALSE}. If \code{TRUE} it return full archive of wallets data over time grouped by \code{wallet_id}. To be used when passing results to \code{\link{rbtc.plot}} function or performing other analysis over time, read notes below.
#' @param archive_path character, default \code{"wallet_archive.rds"}, a wallet archive location.
#' @param api.dict data.table required when using custom API dictionary, read \code{\link{market.api.process}} for details.
#' @param verbose integer Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return data.table object with wallet information in denormilized structure. When launch with \code{wallet_read=TRUE} then all historical archived wallet statuses will be returned. Field \code{wallet_id} is a processing batch id and also the timestamp of single wallet manager processing as integer in Unix time format. Since 0.9.3 new column has been added \code{auth} which corresponds to the \code{*.sources} args lists elements names, see examples.
#' @section Wallet archive:
#' To be able to track wallet assets value over time you need to use \code{archive_write=TRUE}. It will archive wallet manager result \code{data.table} to \code{wallet_archive.rds} file (or filepath as \code{archive_path} arg) in not encrypted format (not a plain text also), sensitive data like amount and value will be available from R by \code{readRDS("wallet_archive.rds")}. This can be used to correct/manipulate archived data or union the results of the wallet manager performed on different machines by \code{readRDS(); rbindlist(); saveRDS()}, see examples. Setting \code{archive_write==FALSE & archive_read==TRUE} will skip processing and just load the archive, same as \code{readRDS()}.
#' You should be aware the archive file will be growing over time, unless you have tons of sources defined or you scheduled \code{wallet_manager} every hour or less you should not experience any issues because of that. In case of the big size of archived rds file you can move data to database, use \code{archive_write=FALSE} and wrap function into database archiver function. For later analysis simply query full archive from database, be sure to match original data types.
#' @section Exchange rates:
#' Exchange rates will be downloaded from different sources. Fiat-fiat rates will be sourced from yahoo finance, if yahoo would not be available then also fiat-fiat rate cannot be calculated. Rates for cryptocurrencies will be downloaded from markets tickers according to \code{rate_priority} and currency pairs available in \code{api.dict}. Currency type (crypto or fiat) is already defined in \code{getOption("Rbitcoin.ct.dict")}, can be edited for support other/new currency.\cr
#' Markets used for crypto rates are defined by \code{rate_priority} as vector of market names in order of its priority from highest to lowest. User need to chose own trusted exchange rates providers and keep in mind to update \code{rate_priority} parameter when necessary. As we recently seen the mtgox after death was still spreading the public API data and any system which sources data from them would be affected, so the control over the source for exchange rates needs to be maintained by user.
#' In case of calculation crypto rate for a currency pair which is not available in \code{\link{api.dict}} then \code{transfer_currency_pair} will be used to get indirect exchange rate. Example: exchange rate for NMC-GBP will be computed as NMC-BTC-USD-GBP using the default \code{transfer_currency_pair} and current \code{api.dict}.
#' The process was not exhaustively tested, you can track all the exchange rates used in the processing by setting \code{options(Rbitcoin.archive_exchange_rate=TRUE)}. This option will append the data to \code{exchange_rate_archive.rds} file in working directory.
#' @section NA measures:
#' In case of missing exchange path (direct and indirect through \code{transfer_currency_pair} based on \code{\link{api.dict}}) between the currency in the wallet and the \code{value_currency} the \code{NA} will be provided to \code{value} for that currency. Any errors while downloading wallet data or exchange rates data will also result \code{NA} measure.
#' Be sure to avoid \code{NA} measures: for unavailable sources you can provide amounts as manual source, for not supported alt cryptocurrencies precalculate its value to supported currency and provide as manual source.
#' While plotting \code{wallet_manager} data any wallet batches which contain at least one \code{NA} measure will be omitted from plot.
#' @section Schedule wallet tracking:
#' User may consider to schedule execution of the function with \code{archive_write=TRUE} for better wallet assets tracking over time. Schedule can be setup on OS by run prepared R script with \code{wallet_manager} function execution. In case of scheduling also plot of wallet manager data use \code{archive_read=TRUE} and pass results to \code{rbtc.plot}, more on \code{\link{rbtc.plot.wallet_manager}}.
#' @section Troubleshooting:
#' In case of the issues with this function try first to verify if all of the sources are returning correct data, use \code{blockchain.api.process} and \code{market.api.process} functions. Possible sources for wallet data: market api, blockchain api, manually provided. Possible sources for exchange rate data: market tickers (taken from \code{api.dict} according to \code{rate_priority}), yahoo (for fiat-fiat, see references). If all sources works and issue still occurs please report. Fiat to fiat conversion using yahoo may not be available for all possible fiat currency pairs.
#' @seealso \code{\link{rbtc.plot}}, \code{\link{blockchain.api.process}}, \code{\link{market.api.process}}, \code{\link{antiddos}}
#' @references \url{https://code.google.com/p/yahoo-finance-managed/wiki/csvQuotesDownload}
#' @export
#' @examples
#' \dontrun{
#' # example market.sources
#' market.sources <- list(
#'   "john smith" = list(market='kraken', key='', secret=''),
#'   "jane smith" = list(market='kraken', key='', secret=''),
#'   "john smith" = list(market='btce', key='', secret=''),
#'   "jane smith" = list(market='btce', key='', secret='')
#' )
#' # example blockchain.sources
#' blockchain.sources <- list(
#'   "john smith" = list(address='')
#' )
#' # example manual.sources
#' manual.sources <- list(
#'   "john smith" = list(location='bitfinex', location_type='market',
#'                       currency=c('BTC','USD'), amount=c(0.4,0)),
#'   "john smith" = list(location='fidor', location_type='bank',
#'                       currency=c('EUR','USD'), amount=c(20,0)),
#'   "jane smith" = list(location='fidor', location_type='bank',
#'                       currency=c('EUR','GBP'), amount=c(10,105))
#' )
#' # execute
#' wallet_dt <- wallet_manager(
#'   market.sources = market.sources,
#'   blockchain.sources = blockchain.sources,
#'   manual.sources = manual.sources,
#'   value_currency = 'USD', # your target currency
#'   rate_priority = c('bitstamp','kraken','hitbtc','btce','bitmarket'), # rates source priority
#'   archive_write = TRUE # by default FALSE, read ?wallet_manager
#' )
#' print(wallet_dt)
#' 
#' # plot recent wallet balances
#' rbtc.plot(wallet_dt, type="recent")
#' 
#' # load archive
#' wallet_dt <- wallet_manager(archive_write=FALSE, archive_read=TRUE)
#' 
#' # aggregate measures by time and currency
#' wallet_dt[,list(amount = sum(amount), value = sum(value)),
#'            keyby = c('wallet_id','currency','value_currency')]
#' # aggregate value by time and location
#' wallet_dt[,list(value = sum(value)),
#'            keyby = c('wallet_id','location','value_currency')]
#' 
#' # plot value over time
#' rbtc.plot(wallet_dt)
#' 
#' # remove processing batch from archive, by id
#' setkey(wallet_dt,wallet_id)
#' wallet_id_to_remove <- 1390000000
#' saveRDS(wallet_dt[!.(wallet_id_to_remove)], "wallet_archive.rds")
#' 
#' # To track exchange rates used set option Rbitcoin.archive_exchange_rate
#' options(Rbitcoin.archive_exchange_rate=TRUE)
#' wallet_dt <- wallet_manager(market.sources,
#'                             blockchain.sources,
#'                             manual.sources = manual.sources,
#'                             rate_priority = c('bitstamp','kraken','hitbtc','bitmarket','btce')
#'                             archive_write = TRUE)
#' exchange_rates_dt <- readRDS("exchange_rate_archive.rds")
#' 
#' # track hitbtc main account balance using dynamically created manual source element
#' manual.sources <- list(
#'   "john smith" = tryCatch(
#'     expr = {
#'       r <- market.api.query(market="hitbtc", url="https://api.hitbtc.com/api/1/payment/balance",
#'                             key="", secret="")
#'       list(location="hitbtc", location_type="market",
#'            currency=r[["balance"]][["currency_code"]], amount=r[["balance"]][["balance"]])
#'     },
#'     error = function(e) list(location="hitbtc", location_type="market",
#'                              currency=NA_character_, amount=NA_real_)
#'   )
#' )
#' }
wallet_manager <- function(market.sources = NULL, 
                           blockchain.sources = NULL, 
                           manual.sources = NULL,
                           min_amount = 1e-4, 
                           value_calc = TRUE, 
                           value_currency = 'USD', 
                           value_currency_type = NULL,
                           rate_priority, 
                           transfer_currency_pair = c(crypto = "BTC", fiat = "USD"),
                           api.dict = getOption("Rbitcoin.api.dict",stop("no api.dict in options and not provided to wallet_manager")), 
                           archive_write = getOption("Rbitcoin.wallet_manager.archive_write",FALSE), 
                           archive_read = getOption("Rbitcoin.wallet_manager.archive_read",FALSE),
                           archive_path = getOption("Rbitcoin.wallet_manager.archive_path","wallet_archive.rds"),
                           verbose = getOption("Rbitcoin.verbose",0)){
  # read archive only, skip processing
  if(archive_read & !archive_write){
    wallet_dt.archive <- if(file.exists(archive_path)) readRDS(archive_path) else stop(paste0('Wallet archive file ',archive_path,' does not exists, define wallet sources and run function using archive_write=TRUE'))
    if(verbose > 0) cat(as.character(Sys.time()),': wallet_manager: wallet manager processing skipped, returning wallet archive only','\n',sep='')
    return(setkeyv(wallet_dt.archive,"wallet_id"))
  }
  # convert list() to NULL and check if data are not dummy
  if(!is.null(manual.sources)){
    if(!is.list(manual.sources)) stop("manual.sources must be a list")
    if(length(manual.sources)==0) manual.sources <- NULL
  }
  if(!is.null(blockchain.sources)){
    if(!is.list(blockchain.sources)) stop("blockchain.sources must be a list")
    if(length(blockchain.sources)==0) blockchain.sources <- NULL
    else if(any(sapply(blockchain.sources, function(x) nchar(x[["address"]])==0))) stop("blockchain.sources must contain non empty 'address' fields")
  }
  if(!is.null(market.sources)){
    if(!is.list(market.sources)) stop("market.sources must be a list")
    if(length(market.sources)==0) market.sources <- NULL
    else if(any(sapply(market.sources, function(x) nchar(x[["key"]])==0 | nchar(x[["secret"]])==0))) stop("market.sources must contain non empty 'key' and 'secret' fields")
  }
  if(value_calc){
    if(!identical(rate_priority,unique(rate_priority))){
      stop('rate_priority must contain a vector of unique market names')
    } # stop, vector needs to be unique
  }
  
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
  
  # NULL and "" names to NA_character_ processing funs
  namesNA <- function(x){
    nm <- names(x)
    if(is.null(nm)) return(rep(NA_character_, length(x)))
    ifelse(nchar(nm)==0,NA_character_,nm)
  }
  
  wallet_source.market <- function(i, verbose){
    # call api
    market.sources[[i]][['action']] <- 'wallet'
    market.sources[[i]][['api.dict']] <- api.dict
    market.sources[[i]][['verbose']] <- verbose - 1 # market.api.process debugging to background
    wal <- tryCatch(
      expr = {
        wallet <- do.call(what = market.api.process, args = market.sources[[i]]) #do.call to handle bitstamp client_id param
        data.table(auth = namesNA(market.sources[i]), timestamp = wallet$timestamp, location = market.sources[[i]][['market']], location_type = 'market', currency = wallet$wallet$currency, amount = wallet$wallet$amount) 
      },
      error = function(e){
        msg <- paste0('error on downloading wallet from ',market.sources[[i]][['market']])
        warning(paste0(msg,': ',e[["message"]]), call. = FALSE)
        data.table(auth = namesNA(market.sources[i]), timestamp = Sys.time(), location = market.sources[[i]][['market']], location_type = 'market', currency = NA_character_, amount = NA_real_)
      }
    )
    if(verbose > 0) cat(as.character(Sys.time()),': wallet_source.market: source processed for ',namesNA(market.sources[i]),' @ ',market.sources[[i]][['market']],'\n',sep='')
    wal
  }
  wallet_source.blockchain <- function(i, verbose){
    blockchain.sources[[i]][['method']] <- 'Single Address'
    blockchain.sources[[i]][['verbose']] <- verbose - 1 # blockchain.api.process debugging to background
    wal <- tryCatch( #decode location to location_type, address to location
      expr = do.call(what = blockchain.api.process, args = blockchain.sources[[i]])[,list(auth = namesNA(blockchain.sources[i]), timestamp = timestamp, location = address, location_type = 'blockchain', currency, amount = final_balance)],
      error = function(e){
        msg <- paste0('error on downloading wallet from ','blockchain')
        warning(paste0(msg,': ',e[["message"]]), call. = FALSE)
        data.table(auth = namesNA(blockchain.sources[i]), timestamp = Sys.time(), location = blockchain.sources[[i]][['address']],  location_type = "blockchain", currency = NA_character_, amount = NA_real_)
      }
    )
    if(verbose > 0) cat(as.character(Sys.time()),': wallet_source.blockchain: source processed for ',namesNA(blockchain.sources[i]),' @ ','blockchain','\n',sep='')
    wal
  }
  wallet_source.manual <- function(i, verbose){
    wal <- data.table(auth = namesNA(manual.sources[i]),
                      timestamp = Sys.time(), 
                      location = if(is.null(manual.sources[[i]][['location']])) NA_character_ else manual.sources[[i]][['location']], 
                      location_type = if(is.null(manual.sources[[i]][['location_type']])) NA_character_ else manual.sources[[i]][['location_type']],
                      currency = as.character(manual.sources[[i]][['currency']]),
                      amount = as.numeric(manual.sources[[i]][['amount']]))
    if(verbose > 0) cat(as.character(Sys.time()),': wallet_source.manual: source processed for ',namesNA(manual.sources[i]),' @ ',if(is.null(manual.sources[[i]][['location']])) NA_character_ else manual.sources[[i]][['location']],'\n',sep='')
    wal
  }

  all.wallet <- list()
  all.wallet[['market']] <- if(length(market.sources) > 0){
    lapply(1:length(market.sources), wallet_source.market, verbose = verbose - 1)
  }
  all.wallet[['blockchain']] <- if(length(blockchain.sources) > 0){
    lapply(1:length(blockchain.sources), wallet_source.blockchain, verbose = verbose - 1)
  }
  all.wallet[['manual']] <- if(length(manual.sources) > 0){
    lapply(1:length(manual.sources), wallet_source.manual, verbose = verbose - 1)
  }
  
  # combine cleaned raw results
  if(length(all.wallet) == 0){
    wallet_dt <- data.table(wallet_id = integer(), currency = character(), currency_type = character(), timestamp = as.POSIXct(NA, origin='1970-01-01', tz='UTC')[-1], location = character(), location_type = character(), amount = numeric())
  }
  else if(length(all.wallet) > 0){
    all.wallet_dt <- rbindlist(lapply(all.wallet, function(x) rbindlist(x)))[amount >= min_amount | is.na(amount),.SD,,keyby='currency']
    # add currency_type dimensions
    wallet_dt <- ct_dt[all.wallet_dt]
    # add wallet_id field
    if(nrow(wallet_dt)==0){
      wallet_dt <- wallet_dt[,list(wallet_id = integer(), currency = character(), currency_type = character(), auth = character(), timestamp = as.POSIXct(NA,origin='1970-01-01',tz='UTC')[-1], location = character(), location_type = character(), amount = numeric())]
    }
    else{
      wallet_dt <- wallet_dt[,list(wallet_id = wallet_id, currency, currency_type, auth, timestamp, location, location_type, amount)]
    }
  }
  if(nrow(wallet_dt) == 0) warning('Zero rows wallet table, review sources definition and min_amount', call.=FALSE)
  
  # calc value
  if(value_calc){
    if(nrow(wallet_dt) == 0) wallet_dt[,`:=`(value_currency = character(), value_rate = numeric(), value = numeric())]
    else if(nrow(wallet_dt) > 0){
      wallet_dt <- wallet_value(wallet_dt = wallet_dt, 
                                value_currency = value_currency, 
                                value_currency_type = value_currency_type,
                                rate_priority = rate_priority, 
                                transfer_currency_pair = transfer_currency_pair,
                                api.dict = api.dict, 
                                verbose = verbose - 1)
    }
  }
  else if(!value_calc){
    if(nrow(wallet_dt) == 0) wallet_dt[,`:=`(value_currency = character(), value_rate = numeric(), value = numeric())]
    else if(nrow(wallet_dt) > 0) wallet_dt[,`:=`(value_currency = NA_character_, value_rate = NA_real_, value = NA_real_)]
  }
  
  # archive_write
  if(archive_write){
    wallet_dt.archive <- if(file.exists(archive_path)) readRDS(archive_path) else NULL
    if(!is.null(wallet_dt.archive) & !("auth" %in% names(wallet_dt.archive))){ # 0.9.3 update
      backup_092 <- paste0(archive_path,'_0.9.2_',as.character(Sys.time(),"%Y%m%d_%H%M%S"),'.rds')
      stopifnot(file.rename(archive_path, backup_092))
      wallet_dt.archive[,auth := NA_character_]
      setcolorder(wallet_dt.archive,c("wallet_id","currency","currency_type","auth","timestamp","location","location_type","amount","value_currency","value_rate","value"))
      saveRDS(wallet_dt.archive,archive_path)
      message(paste0("Your current archive of wallet data stored in file ",archive_path," has been renamed to '",backup_092,"'. Since 0.9.3+ there is addtional field stored in archive. Your wallet archive has been extended for that column and resaved. To use new 'auth' column just name the lists within *.sources args. Unnamed will result NA, column can be useful on grouping data."))
    }
    if(nrow(wallet_dt) > 0){
      wallet_dt.archive <- rbindlist(list(
        wallet_dt.archive, 
        wallet_dt
      ))
      saveRDS(wallet_dt.archive,archive_path)
    }
  }
  
  # archive_read
  if(archive_read){
    if(verbose > 0) cat(as.character(Sys.time()),': wallet_manager: wallet manager processing finished','\n',sep='')
    return(setkeyv(wallet_dt.archive,"wallet_id"))
  }
  
  # no archive - current batch
  if(verbose > 0) cat(as.character(Sys.time()),': wallet_manager: wallet manager processing finished','\n',sep='')
  return(setkeyv(wallet_dt,"wallet_id"))
}

# get_rate ------------------------------------------------

#' @title Get rate
#' @description Download ask-bid data for provided market and currency pair, supports bitcoin markets and yahoo.
#' @keywords internal
get_rate <- function(v_market, v_base, v_quote, 
                     api.dict, 
                     antiddos = getOption("Rbitcoin.antiddos",TRUE),
                     verbose = getOption("Rbitcoin.verbose",0)){
  if(v_market == 'yahoo'){
    ab_dt <- tryCatch(
      expr = {
        wait <- if(antiddos) getOption("Rbitcoin.antiddos.fun",antiddos_fun)(source_system = v_market, verbose = verbose - 1) else 0
        raw_char <- getURL(url = paste0('https://download.finance.yahoo.com/d/quotes.csv?s=',paste0(v_base,v_quote),'=X&f=sl1d1t1ab&e=.csv'),
                           .opts = list(useragent = paste("Rbitcoin",packageVersion("Rbitcoin"))))
        rate_dt <- fread(raw_char)
        setnames(rate_dt,c('currency_pair','last','date','time','ask','bid'))
        rate_dt[,list(ask, bid)]
      },
      error = function(e){
        msg <- paste0('error on downloading ',v_base,v_quote,' exchange rate from ',v_market)
        warning(paste0(msg,': ',e[["message"]]), call. = FALSE)
        data.table(ask = NA_real_, bid = NA_real_)
      }
    )
  }
  else{
    ab_dt <- tryCatch(
      expr = market.api.process(v_market,c(v_base,v_quote), 'ticker', api.dict = api.dict, verbose = verbose - 1)[,list(ask, bid)],
      error = function(e){
        msg <- paste0('error on downloading ',v_base,v_quote,' exchange rate from ',v_market)
        warning(paste0(msg,': ',e[["message"]]), call. = FALSE)
        data.table(ask = NA_real_, bid = NA_real_)
      }
    )
  }
  if(verbose > 0) cat(as.character(Sys.time()),': get_rate: downloading exchange rate ',v_base,v_quote,' from ',v_market,'\n',sep='')
  ab_dt
}

# wallet_value ------------------------------------------------------------

#' @title Wallet value
#' @description Defines the currency pairs required to cover needed exchange rates and chose the source according to rate priority, also utilize the transfer_currency_pair when no direct currency pair available.
#' @note Setting option "Rbitcoin.archive_exchange_rate" to \code{TRUE} will lead to archive exchange rates used during the processing. Read \code{\link{wallet_manager}} manual.
#' @keywords internal
wallet_value <- function(wallet_dt, 
                         ct.dict = getOption("Rbitcoin.ct.dict"),
                         value_currency, 
                         value_currency_type = NULL,
                         rate_priority,
                         transfer_currency_pair,
                         api.dict = getOption("Rbitcoin.api.dict",stop("api.dict is mandatory")), 
                         archive_exchange_rate = getOption("Rbitcoin.archive_exchange_rate",FALSE),
                         verbose = getOption("Rbitcoin.verbose",0)){
  # currency type dict
  ct_dt <- rbindlist(lapply(1:length(ct.dict), function(i) data.table(currency_type = names(ct.dict[i]), currency = ct.dict[[i]])))[,.SD,,keyby='currency']
  
  # api.dict filtering to ticker and only top priority (from rate_priority) currency_pair order independent: sort(c(base,quote))
  rate_priority_dt <- data.table(market = rate_priority, priority = 1:length(rate_priority), key = 'market')
  
  priority.ticker.api.dict <- {
    rate_priority_dt[api.dict[action=='ticker'
                              ],list(market,priority,base,quote),nomatch=0
                     ][,list(currency_pair = paste(sort(c(base,quote)),collapse='')), by=c('market','base','quote','priority')
                       ][order(currency_pair,priority)
                         ][,head(.SD,1),keyby=c('currency_pair'),.SDcols=c('market','base','quote')
                           ]
  } # currency_pair is sorted here
    
  currency_to_rate <- {
    wallet_dt[,unique(data.table(currency, currency_type))]
  }
  
  direct_pair <- {
    priority.ticker.api.dict[currency_to_rate[,list(currency_pair = paste(sort(c(currency,value_currency)),collapse='')), by=c('currency','currency_type')
                                              ][,list(currency, currency_type),keyby='currency_pair'
                                                ],list(currency_pair, currency, currency_type, market, base, quote)
                             ][value_currency_type=='fiat' & currency_type=='fiat',`:=`(market = 'yahoo', base = currency, quote = value_currency)
                               ]
  }
  
  to_transfer_pair <- if(!all(currency_to_rate$currency %in% direct_pair$base)){ # only if there are some indirect lefts
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
    else if(nrow(direct_to_indirect) == 0){
      res <- data.table(currency_pair = character(), currency = character(), currency_type = character(), market = character(), base = character(), quote = character())
    }
    res
  } else if(all(currency_to_rate$currency %in% direct_pair$base)){ # if all direct available then skill
    data.table(currency_pair = character(), currency = character(), currency_type = character(), market = character(), base = character(), quote = character())
  }
  
  transfer_pair <- if(!all(currency_to_rate$currency %in% direct_pair$base)){ # only if there are some indirect lefts
    priority.ticker.api.dict[currency_pair==paste(sort(transfer_currency_pair),collapse=''),
                             list(currency_pair, currency = transfer_currency_pair[[ifelse(value_currency_type=='fiat','crypto','fiat')]], currency_type = ifelse(value_currency_type=='fiat','crypto','fiat'), market, base, quote)
                             ]
  } else if(all(currency_to_rate$currency %in% direct_pair$base)){ # if all direct available then skill
    data.table(currency_pair = character(), currency = character(), currency_type = character(), market = character(), base = character(), quote = character())
  }
  
  from_transfer_pair <- if(!all(currency_to_rate$currency %in% direct_pair$base)){ # only if there are some indirect lefts
    if(value_currency_type == 'fiat'){
      res <- data.table(currency_pair = paste(sort(c(value_currency,transfer_currency_pair[['fiat']])),collapse=''),
                        currency = transfer_currency_pair[['fiat']],
                        currency_type = 'fiat',
                        market = 'yahoo', 
                        base = transfer_currency_pair[['fiat']], quote = value_currency)
    }
    else if(value_currency_type == 'crypto'){
      dt <- data.table(currency_pair = paste(sort(c(value_currency,transfer_currency_pair[['crypto']])),collapse=''),
                       currency = transfer_currency_pair[['crypto']],
                       currency_type = 'crypto', 
                       key = 'currency_pair')
      res <- priority.ticker.api.dict[dt, list(currency_pair, currency, currency_type, market, base, quote)]
    }
    res
  } else if(all(currency_to_rate$currency %in% direct_pair$base)){ # if all direct available then skill
    data.table(currency_pair = character(), currency = character(), currency_type = character(), market = character(), base = character(), quote = character())
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
    if(all_pair[i,identical(substr(currency_pair,1,3),substr(currency_pair,4,6))]){
      ab <- all_pair[i,list(
        market = NA_character_, 
        base = substr(currency_pair,1,3), 
        quote = substr(currency_pair,4,6), 
        ask = 1, 
        bid = 1
      )]
    } # 1:1 rate
    else if(all_pair[i,!is.na(market)]){
      ab <- all_pair[i,get_rate(v_market = market, v_base = base, v_quote = quote, api.dict = api.dict, verbose = verbose - 1)]
      ab <- data.table(all_pair[i,.(market, base, quote)],ab)
      if(!ab[,is.numeric(ask) & is.numeric(bid)]){
        warning(all_pair[i,invisible({
          paste0("Incorrect ask-bid data returned for ",base,quote," pair from ",market,". ",market," do not support such currency pair conversion. It results NA. Use min_amount arg to wallet_manager to filter out 'empty' wallets.",sep="")
        })] ,call.=FALSE)
        ab <- data.table(all_pair[i,.(market, base, quote)],ask=NA_real_,bid=NA_real_)
      }
    } # regular rate - download from market ticker/yahoo
    else {
      ab <- data.table(market = NA_character_, base = NA_character_, quote = NA_character_, ask = NA_real_, bid = NA_real_)
    } # postponed to second loop for indirect rates
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
    } 
    else{
      v.currency_pair <- all_pair[i,paste(sort(c(currency,transfer_currency_pair[[currency_type]])),collapse='')]
      transfer_in_ab <- all_pair[J(v.currency_pair)]
    }
    
    # transfer through
    if(all_pair[i,currency_type!=value_currency_type]){
      v.currency_pair <- all_pair[i,paste(sort(c(transfer_currency_pair)),collapse='')]
      transfer_ab <- all_pair[J(v.currency_pair)]
    }
    else{
      transfer_ab <- all_pair[FALSE]
    }
    
    # transfer out
    if(all_pair[i,value_currency == transfer_currency_pair[[value_currency_type]]]){ # currency already in the transfer pair
      transfer_out_ab <- all_pair[FALSE]
    }
    else{
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
  
  # archive exchange rates
  if(archive_exchange_rate){
    exchange_rate_dt.archive <- if(file.exists('exchange_rate_archive.rds')) readRDS('exchange_rate_archive.rds') else NULL
    if(nrow(wallet_dt) > 0){
      exchange_rate_dt.archive <- rbindlist(list(
        exchange_rate_dt.archive, 
        data.table(value_rate_id = as.integer(Sys.time()), all_pair)
      ))
      saveRDS(exchange_rate_dt.archive,'exchange_rate_archive.rds')
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
             wallet_id,currency,currency_type, auth, timestamp, location, location_type, amount, 
             value_currency = v.value_currency,
             base, quote, ask, bid
           )][(!is.na(ask) & !is.na(bid)),
              value_rate := f.askbid_value(currency, value_currency, base, quote, ask, bid)
              ][,
                list( # just a clean return list of columns
                  wallet_id, currency, currency_type, auth, timestamp, location, location_type, amount, 
                  value_currency, value_rate, 
                  value = amount * value_rate
                )]
}
