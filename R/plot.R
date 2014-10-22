
# rbtc.plot ------------------------------------------------------

#' @title Plot Rbitcoin objects
#' @description Generic function to plot different objects returned by some Rbitcoin functions.
#' @param x object to be plotted, result of Rbitcoin function, currently supported: \code{market.api.process(action=c("trades","order_book"))}, \code{wallet_managere}.
#' @param \dots additional params to be passed to particular plot function.
#' @note Legend may not scale well in interactive viewer, for better scaling save plot to file, see examples.
#' @return by side effect a base plot.
#' @seealso \code{\link{rbtc.plot.trades}}, \code{\link{rbtc.plot.order_book}}, \code{\link{rbtc.plot.wallet_manager}}
#' @aliases Rbitcoin.plot
#' @export
#' @examples
#' \dontrun{
#' # fetch trades
#' trades <- market.api.process('kraken',c('BTC','EUR'),'trades')
#' # plot trades
#' rbtc.plot(trades)
#' # plot trades to svg
#' svg("trades.svg")
#' rbtc.plot(trades)
#' dev.off()
#' # plot wallet_manager archive to svg
#' wallet_dt <- wallet_manager(archive_write=FALSE, archive_read=TRUE)
#' svg("wallet_value.svg")
#' rbtc.plot(wallet_dt)
#' dev.off()
#' }
rbtc.plot <- function(x, ...,
                      verbose = getOption("Rbitcoin.verbose",0)){
  # recognize input
  if(is.list(x) & !is.data.table(x)){
    stopifnot(length(x) > 0)
    if(!is.null(x[['asks']]) | !is.null(x[['bids']])) action <- 'order_book'
    else if(!is.null(x[['trades']])) action <- 'trades'
    else stop(paste0("unknown list object provided to rbtc.plot, list to plot should contain asks/bids/trades element"))
  } 
  else if(is.data.table(x)){
    stopifnot(nrow(x) > 0)
    if(all(c('wallet_id','currency',"auth",'amount','value_currency','value_rate','value') %in% names(x))){
      action <- 'wallet_manager'
    }
    else {
      stop(paste0("unknown data.table object provided to rbtc.plot, read manual for supported objects"))
    }
  } 
  else{
    stop(paste0("unknown object provided to rbtc.plot, read manual for supported objects"))
  }
  
  # launch plot
  switch(action,
         'order_book' = rbtc.plot.order_book(x, ..., verbose = verbose - 1),
         'trades' = rbtc.plot.trades(x, ..., verbose = verbose - 1),
         'wallet_manager' = rbtc.plot.wallet_manager(wallet_dt= x, ..., verbose = verbose - 1))
  if(verbose > 0) cat(as.character(Sys.time()),': rbtc.plot: plotting finished','\n',sep='')
  invisible(NULL)
}

# rbtc.plot.trades ---------------------------------------------------------

#' @title Plot trades data
#' @param x list, a result from \code{market.api.process(action="trades")}.
#' @seealso \code{\link{market.api.process}}, \code{\link{rbtc.plot}}
#' @export
#' @examples
#' \dontrun{
#' trades <- market.api.process('kraken',c('BTC','EUR'),'trades')
#' rbtc.plot(trades)
#' }
rbtc.plot.trades <- function(x,
                             verbose = getOption("Rbitcoin.verbose",0)){
  plot(x = x[['trades']][['date']], y = x[['trades']][['price']],
       type = 'l', xlab = 'time', ylab = 'price',
       main = paste(x[['market']],paste0(x[['base']],x[['quote']]),'trades',sep=' '),
       sub = as.character(x[['timestamp']], format = '%Y-%m-%d %H:%M:%S %Z'))
  mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
  grid()
  if(verbose > 0) cat(as.character(Sys.time()),': rbtc.plot.trades: performing plot','\n',sep='')
  invisible(NULL)
}

# rbtc.plot.order_book -----------------------------------------------------

#' @title Plot order book data
#' @param x list, a result from \code{market.api.process(action="order_book")}.
#' @param limit_pct numeric, percentage of limit from middle price. It acts like a zoom-in to the middle of order book plot.
#' @seealso \code{\link{market.api.process}}, \code{\link{rbtc.plot}}
#' @export
#' @examples
#' \dontrun{
#' order_book <- market.api.process('bitmarket',c('BTC','PLN'),'order_book')
#' rbtc.plot(order_book)
#' rbtc.plot(order_book, limit_pct = 0.5)
#' }
rbtc.plot.order_book <- function(x, 
                                 limit_pct = getOption("Rbitcoin.plot.limit_pct",Inf),
                                 verbose = getOption("Rbitcoin.verbose",0)){
  if(is.finite(limit_pct)){
    x <- copy(x)
    mid_price <- ((x[["asks"]][1,price] + x[["bids"]][1,price]) / 2)
    x[["asks"]] <- x[["asks"]][price <= mid_price * (1+limit_pct)]
    x[["bids"]] <- x[["bids"]][price >= mid_price * (1-limit_pct)]
  }
  v_price <- c(x[['asks']][['price']],x[['bids']][['price']])
  v_amount <- c(x[['asks']][['cum_amount']],x[['bids']][['cum_amount']])
  plot(x = v_price, y = v_amount,
       type = 'n', xlab = 'price', ylab = 'cum amount',
       main = paste(x[['market']],paste0(x[['base']],x[['quote']]),'order book',sep=' '),
       sub = as.character(x[['timestamp']], format = '%Y-%m-%d %H:%M:%S %Z'))
  lines(x = x[['asks']][['price']], y = x[['asks']][['cum_amount']])
  lines(x = x[['bids']][['price']], y = x[['bids']][['cum_amount']])
  mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
  grid()
  if(verbose > 0) cat(as.character(Sys.time()),': rbtc.plot.order_book: performing plot','\n',sep='')
  invisible(NULL)
}
