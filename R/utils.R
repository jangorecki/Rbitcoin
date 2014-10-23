
# available_wallet --------------------------------------------------------

#' @title Available wallet
#'
#' @description Calculates assets available to trade, not on hold by current open orders.
#'
#' @param wallet list (or it's nested data.table object) returned by \code{market.api.process} with \code{action="wallet"} param.
#' @param open_orders list (or it's nested data.table object) returned by \code{market.api.process} with \code{action="open_orders"} param.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return data.table object, the same as wallet but with the appropriate amounts after subtracting the open orders amounts.
#' @seealso \code{\link{market.api.process}}
#' @export
#' @examples
#' \dontrun{
#' wallet <- market.api.process('kraken', action='wallet', key='', secret='')
#' open_orders <- market.api.process('kraken', action='open_orders', key='', secret='')
#' aw <- available_wallet(wallet, open_orders, verbose = 1)
#' print(aw)
#' }
available_wallet <- function(wallet, open_orders, 
                             verbose = getOption("Rbitcoin.verbose",0)){
  # input conversion
  # wallet
  if(all(c("market","timestamp","market_timestamp","wallet") == names(wallet))){
    wallet <- wallet[["wallet"]]
  }
  else if(all(c("currency","amount") == names(wallet))){
    invisible()
  }
  else stop("unknown input `wallet` argument, should be wallet action result")
  
  if(all(c("market","timestamp","market_timestamp","open_orders") == names(open_orders))){
    open_orders <- open_orders[["open_orders"]]
  }
  else if(all(c("base","quote","oid","type","price","amount") == names(open_orders))){
    invisible()
  }
  else stop("unknown input `open_orders` argument, should be open_orders action result")
  
  # input validation
  if(nrow(wallet)==0){
    if(verbose > 0) cat(as.character(Sys.time()),': available_wallet: wallet object is empty, returning provided wallet','\n',sep='')
    warning(paste0("Wallet object provided to available_wallet is empty, returning provided wallet"))
    return(wallet)
  }
  if(nrow(open_orders)==0){
    if(verbose > 0) cat(as.character(Sys.time()),': available_wallet: no open orders, returning provided wallet','\n',sep='')
    return(wallet)
  }
  # processing
  if(open_orders[,any(unlist(lapply(lapply(.SD,is.na),any))), .SDcols = c('type','price','amount')]){
    stop('available_wallet requires open_orders with non NA type, price, amount. Not every market returned those data in open_orders method')
  }
  setkey(wallet,currency)
  available <- rbindlist(list(
    open_orders[type=='buy',list(amount = price * amount),by='quote'][,list(currency = quote, amount)],
    open_orders[type=='sell',list(amount = amount),by='base'][,list(currency = base, amount)]
  ))[,list(amount_in_orders = sum(amount)),keyby='currency'][wallet][,list(amount_available = sum(amount,-amount_in_orders,na.rm=TRUE)),by="currency"]
  if(verbose > 0) cat(as.character(Sys.time()),': available_wallet: available wallet calculated','\n',sep='')
  available
}

# antiddos_fun ----------------------------------------------------------

#' @title Anti DDoS
#'
#' @description Wait if necessary before next API call to market (or any other source system) to do not get banned.
#'
#' @param source_system character, a unique name of source system, could be any name \code{c('kraken','bitstamp','blockchain','alt_bitstamp')} 
#' @param antiddos.sec numeric time in seconds between API calls on the particular source system, defeault \code{getOption("Rbitcoin.antiddos.sec",10)}.
#' @param antiddos.verbose integer, Rbitcoin antiddos verbose message. Should be used when user prefer \code{verbose=0} but still wants to see antiddos waiting time.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return numeric time of wait in seconds.
#' @details
#' The following options can control antiddos process: 
#' \itemize{
#' \item \code{getOption("Rbitcoin.antiddos",TRUE)}
#' \item \code{getOption("Rbitcoin.antiddos.sec",10)} seconds to wait between calls to the same system
#' \item \code{getOption("Rbitcoin.antiddos.fun",antiddos_fun)} - custom antiddos function. Default the one documentated on this site.
#' }
#' @note 
#' Rbitcoin built-in antiddos function handle only api calls launched within the current R session.
#' By default it will wait \code{getOption("Rbitcoin.antiddos.sec",10)} seconds between calls to the same system.
#' It cache the api calls in \code{Rbitcoin.last_api_call} environment object in current workspace.
#' User can provide own custom antiddos function using \code{getOption("Rbitcoin.antiddos.fun",antiddos_fun)}. 
#' This way it is possible to cache last api call in own data storage (like database), this can allow concurrent R sessions + queueing to be handled by the antiddos process. 
#' Custom antiddos should return numeric value of time spent on antiddos waiting in seconds.
#
#' @section Side effect:
#' Environment of name \code{Rbitcoin.last_api_call} which holds the timestamps of last api call per \code{source_system} during the R session.
#' @seealso \code{\link{market.api.process}}, \code{\link{wallet_manager}}
#' @export
#' @aliases antiddos
#' @examples
#' \dontrun{
#' # run below code in a batch
#' options("Rbitcoin.antiddos.verbose"=1) # alternatively: options("Rbitcoin.verbose" = 3)
#' market.api.process('kraken',c('BTC','EUR'),'ticker')
#' market.api.process('kraken',c('BTC','LTC'),'ticker')
#' options("Rbitcoin.antiddos.verbose"=0)
#' }
antiddos_fun <- function(source_system, 
                         antiddos.sec = getOption("Rbitcoin.antiddos.sec",10),
                         antiddos.verbose = getOption("Rbitcoin.antiddos.verbose",0),
                         verbose = getOption("Rbitcoin.verbose",0)){
  wait <- 0
  if(!is.null(Rbitcoin.last_api_call[[source_system]])){
    wait <- as.numeric(get(source_system,Rbitcoin.last_api_call)) + antiddos.sec - as.numeric(Sys.time())
    if(wait>0){
      if(verbose > 0 | antiddos.verbose > 0) cat(as.character(Sys.time()),': antiddos_fun: waiting for ',round(wait,2),'s as anti-ddos for api: ',source_system,'\n',sep='')
      Sys.sleep(wait)
    }
    else wait <- 0
  }
  assign(source_system, Sys.time(), envir = Rbitcoin.last_api_call)
  return(wait)
}
