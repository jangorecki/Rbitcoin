
# plot.btc ------------------------------------------------------

#' @title Plot Rbitcoin objects
#' @description Generic function to plot different objects returned by some Rbitcoin functions. The function is just a wrapper to class specific plots and also placeholder for Rbitcoin plot doc.
#' @param x object to be plotted, result of Rbitcoin function, currently supported: \code{market.api.process(action=c("trades","order_book"))}, \code{wallet_manager(archive_read=TRUE)}.
#' @param \dots additional params to be passed to particular plot function.
#' @note Legend may not scale well in interactive viewer, for better scaling save plot to file, see examples.
#' @return by side effect a plot.
#' @seealso \code{\link{plot.btc.trades}}, \code{\link{plot.btc.order_book}}, \code{\link{plot.btc.wallet_manager}}
#' @aliases Rbitcoin.plot
#' @export
#' @examples
#' \dontrun{
#' # plot trades
#' trades <- market.api.process('kraken',c('BTC','EUR'),'trades')
#' svg()
#' plot(trades)
#' dev.off()
#' # plot wallet_manager archive to svg
#' wallet_dt <- wallet_manager(archive_write = FALSE, archive_read = TRUE)
#' svg()
#' plot(wallet_dt)
#' dev.off()
#' }
plot.btc <- function(x,  ...){
  return(plot(x, ...))
}

# plot.btc.wallet_manager -------------------------------------------------------------

#' @title Plot wallet manager results
#' @description Plot wallet manager results archive
#' @param x data.table, result from \code{wallet_manager(archive_read=TRUE)}
#' @param y.type character, measure to be used on Y axis, it will affect a global type of plot. Only "value" currently supported.
#' @param mask logical default \code{FALSE}. If set to \code{TRUE} it will mask any amounts and blockchain addresses.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return \code{TRUE} invisibily. Side effect a base plot - dashboard panel to track different measures of your assets and its value. Use \code{mask} if you want to share the results to somebody, it will overwrite value with value ratio and also mask blockchain addresses.
#' @section wallet manager archive:
#' To be able to track wallet assets value over time user needs to use \code{archive_write=TRUE} at least twice in \code{wallet_manager} processing (with non-NA measures). Using the cryptocurrency which do not have any exchange path to \code{transfer_currency_pair} and/or \code{value_currency} will result \code{NA} as \code{value}. Error on data downloading from external sources (wallets or exchange rates) will also result \code{NA}. Any wallet processing batch which will contains at least one \code{NA} measure will be omitted from plot. If you have some crypto not currenctly supported you may extend dictionary for more currencies or provide its value as manual source to \code{wallet_manager} already calculated in common value currency, remember to comment out the previous source which returns the \code{NA} measure.\cr To plot wallet manager data load wallet archive data, see examples.
#' Target value currency is taken from the last execution of \code{wallet_manager} archive.
#' @seealso \code{\link{wallet_manager}}, \code{\link{plot.btc}}
#' @export
#' @examples
#' \dontrun{
#' wallet_dt <- wallet_manager(archive_write = FALSE, archive_read = TRUE)
#' plot(wallet_dt)
#' plot(wallet_dt, mask=TRUE)
#' }
plot.btc.wallet_manager <- function(x, y.type = c("value","amount"), 
                                    mask = getOption("Rbitcoin.plot.mask",FALSE),
                                    verbose = getOption("Rbitcoin.verbose",0)){
  x <- copy(x)
  # NA wallet_id groups! will be omitted
  x[,NA_group := any(is.na(value) | is.na(amount)), by=wallet_id]
  
  # choose last value_currency
  v.value_currency <- x[which.max(wallet_id),value_currency]
  
  x <- x[NA_group==FALSE & value_currency==v.value_currency][,NA_group:=NULL]
  
  # verify time dimension length > 1
  if(length(x[,unique(wallet_id)]) <= 1){
    stop(paste0("Cannot plot time on x axis: provided combined wallet data consists only one (non-NA measure) observation for (recent used) value currency: ",v.value_currency,", be sure to load wallet archive using `wallet_dt <- wallet_manager(archive_write=FALSE, archive_read=TRUE)`, by default wallet_manager function return only recent wallet data. See examples"), call.=FALSE)
  }
  
  # legend settings
  case.legend <- function(legend_, col_){
    if(!dev.interactive()){ 
      legend(x = 'topleft', legend = legend_, col = col_, lty = 1, 
             cex = 0.7, y.intersp = 0.8, seg.len = 1)
    } else if(dev.interactive()){
      legend(x = 'topleft', legend = legend_, col = col_, lty = 1, 
             cex = 0.7, y.intersp = 0.8, seg.len = 1, bty = "n")
    } else {
      warning(paste0("some legend was not plotted, unknow result of dev.cur() case: ",names(dev.cur()),' ',dev.cur()))
    }
    invisible(TRUE)
  }
  
  y.type.value <- function(){
    # plot window for matrix of plots
    par(mfrow=c(3,2),mar=c(4.6,4.1,3.6,2.1))
    
    # total value over time
    x[,list(value = sum(value)),by=wallet_id
      ][order(wallet_id)
        ][,list(wallet_id, value, value_init = value[1])
          ][,list(wallet_id, value, value_mask = value / value_init)
            ][,plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
                    y = if(mask) value_mask else value,
                    type = 'l', xlab = 'time', ylab = 'value', col = 1,
                    main = paste('Wallet value over time in',v.value_currency))]
    
    # last wallet
    # dt 1.9.4 issue: https://github.com/Rdatatable/data.table/issues/858
    x[wallet_id==max(x$wallet_id)
      ][,list(value = sum(value)),by=currency
        ][,list(currency, value, value_init = sum(value))
          ][,list(currency, value, value_mask = value / value_init)
            ][order(-value)
              ][,barplot(if(mask) value_mask else value,
                         names.arg=currency,
                         xlab = paste("Total value:",if(mask) as.character(round(sum(value_mask),2)) else paste(as.character(round(sum(value),2)),v.value_currency)), # xlab used as 'sub'
                         ylab = 'value', col = 2:(.N+1),
                         main = paste('Last wallet currency value in',v.value_currency))]
    
    # currency value over time
    x[,list(value = sum(value)),by=list(currency,wallet_id)
      ][,value_init := .SD[wallet_id==min(x$wallet_id),sum(value)]
        ][,value_mask := value / value_init
          ][order(wallet_id,currency)
            ][,{
              plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
                   y = if(mask) value_mask else value,
                   type = 'n', xlab = 'time', ylab = 'value',
                   main = paste('Currency value over time in',v.value_currency))
              cl <- 1
              lg <- data.table()
              # currencies series
              for(v.currency in unique(currency)){
                cl <- cl+1
                lines(x = as.POSIXct(.SD[currency==v.currency,wallet_id], origin='1970-01-01', tz='UTC'),
                      y = .SD[currency==v.currency,if(mask) value_mask else value],
                      col = cl)
                # build legend
                lg <- rbindlist(list(lg, data.table(legend_ = v.currency, col_ = cl)))
              }
              lg[,case.legend(legend_, col_)]
              invisible(TRUE)
            }]
    
    # currency_type
    x[,list(value = sum(value)), by=list(currency_type,wallet_id)
      ][,value_init := .SD[wallet_id==min(x$wallet_id),sum(value)]
        ][,value_mask := value / value_init
          ][order(wallet_id,currency_type)
            ][,{
              plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
                   y = if(mask) value_mask else value,
                   type = 'n', xlab = 'time', ylab = 'value',
                   main = paste('Currency type value over time in',v.value_currency))
              cl <- 1
              lg <- data.table()
              # currency types series
              for(v.currency_type in unique(currency_type)){
                cl <- cl+1
                lines(x = as.POSIXct(.SD[currency_type==v.currency_type,wallet_id], origin='1970-01-01', tz='UTC'),
                      y = .SD[currency_type==v.currency_type,if(mask) value_mask else value],
                      col = cl)
                # build legend
                lg <- rbindlist(list(lg, data.table(legend_ = v.currency_type, col_ = cl)))
              }
              lg[,case.legend(legend_, col_)]
              invisible(TRUE)
            }]
    
    # location over time
    # mask blockchain address
    if(mask){
      map_address <- x[location_type=="blockchain",list(location_type = head(location_type,1), location = unique(location))
                         ][,list(new_address = paste("address",.I,sep="_")), keyby=list(location_type,location)
                           ]
      if(nrow(map_address) > 0){
        setkeyv(x,c("location_type","location"))
        x <- map_address[x
                         ][location_type=="blockchain", location:=new_address
                           ][,list(wallet_id, currency, currency_type, auth, timestamp, location, location_type, amount, value_currency, value_rate, value)
                             ]
      }
    }
    x[,list(value = sum(value)),by=list(location,wallet_id)
      ][,value_init := .SD[wallet_id==min(x$wallet_id),sum(value)]
        ][,value_mask := value / value_init
          ][order(wallet_id,location)
            ][,{
              plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
                   y = if(mask) value_mask else value,
                   type = 'n', xlab = 'time', ylab = 'value',
                   main = paste('Location value over time in',v.value_currency))
              cl <- 1
              lg <- data.table()
              # location series
              for(v.location in unique(location)){
                cl <- cl+1
                lines(x = as.POSIXct(.SD[location==v.location,wallet_id], origin='1970-01-01', tz='UTC'),
                      y = .SD[location==v.location,if(mask) value_mask else value],
                      col = cl)
                # build legend
                lg <- rbindlist(list(lg, data.table(legend_ = if(nchar(v.location) > 10) paste0(substr(v.location,1,7),"...") else v.location,
                                                    col_ = cl)))
              }
              lg[,case.legend(legend_, col_)]
              invisible(TRUE)
            }]
    
    # location_type
    x[,list(value = sum(value)), by=list(location_type,wallet_id)
      ][,value_init := .SD[wallet_id==min(x$wallet_id),sum(value)]
        ][,value_mask := value / value_init
          ][order(wallet_id,location_type)
            ][,{
              plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
                   y = if(mask) value_mask else value,
                   type = 'n', xlab = 'time', ylab = 'value',
                   main = paste('Location type value over time in',v.value_currency))
              cl <- 1
              lg <- data.table()
              # currency types series
              for(v.location_type in unique(location_type)){
                cl <- cl+1
                lines(x = as.POSIXct(.SD[location_type==v.location_type,wallet_id], origin='1970-01-01', tz='UTC'),
                      y = .SD[location_type==v.location_type,if(mask) value_mask else value],
                      col = cl)
                # build legend
                lg <- rbindlist(list(lg, data.table(legend_ = v.location_type, col_ = cl)))
              }
              lg[,case.legend(legend_, col_)]
              invisible(TRUE)
            }]
    
    # sign plot
    
    par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))
    par(new=TRUE)
    title(sub = x[,as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z')], cex.sub = 0.7)
    mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
    par(new=FALSE)
    if(verbose > 0) cat(as.character(Sys.time()),': plot.btc.wallet_manager: plot DONE','\n',sep='')
    invisible(TRUE)
  } # end of y.type.value
  
  y.type.value()
}

# plot.btc.trades ---------------------------------------------------------

#' @title Plot trades data
#' @param x list, a result from \code{market.api.process(action="trades")}.
#' @seealso \code{\link{market.api.process}}, \code{\link{plot.btc}}
#' @export
#' @examples
#' \dontrun{
#' trades <- market.api.process('kraken',c('BTC','EUR'),'trades')
#' plot(trades)
#' }
plot.btc.trades <- function(x,
                            verbose = getOption("Rbitcoin.verbose",0)){
  plot(x = x[['trades']][['date']], y = x[['trades']][['price']],
       type = 'l', xlab = 'time', ylab = 'price',
       main = paste(x[['market']],paste0(x[['base']],x[['quote']]),'trades',sep=' '),
       sub = as.character(x[['timestamp']], format = '%Y-%m-%d %H:%M:%S %Z'))
  mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
  grid()
  if(verbose > 0) cat(as.character(Sys.time()),': plot.btc.trades: performing plot','\n',sep='')
  invisible(TRUE)
}

# plot.btc.order_book -----------------------------------------------------

#' @title Plot order book data
#' @param x list, a result from \code{market.api.process(action="order_book")}.
#' @param limit_pct numeric, percentage of limit from middle price. It acts like a zoom-in to the middle of order book plot.
#' @seealso \code{\link{market.api.process}}, \code{\link{plot.btc}}
#' @export
#' @examples
#' \dontrun{
#' order_book <- market.api.process('bitmarket',c('BTC','PLN'),'order_book')
#' plot(order_book)
#' plot(order_book, limit_pct = 0.5)
#' }
plot.btc.order_book <- function(x, 
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
  if(verbose > 0) cat(as.character(Sys.time()),': plot.btc.order_book: performing plot','\n',sep='')
  invisible(TRUE)
}
