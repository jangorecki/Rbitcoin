
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
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
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
#' @aliases plot.btc
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
  # recognize input
  if(is.list(x) & !is.data.table(x)){
    stopifnot(length(x) > 0)
    if(!is.null(x[['asks']]) | !is.null(x[['bids']])){
      action <- 'order_book'
    } else if(!is.null(x[['trades']])){
      action <- 'trades'
    } else{
      stop(paste0("unknown list object provided to Rbitcoin.plot, list should contain asks/bids/trades element"))
    }
  } else if(is.data.table(x)){
    stopifnot(nrow(x) > 0)
    if(all(c('wallet_id','currency','amount','value_currency','value_rate','value') %in% names(x))){
      if(length(x[,unique(wallet_id)])==1) stop(paste0("Plotting wallet manager possible for wallet archive data, load wallet_manager archive using archive_read=TRUE param, also ensure you did run wallet_manager at least twice using archive_write=TRUE"))
      else if(length(x[,unique(wallet_id)]) > 1) action <- 'wallet_manager'
    } else if(all(c('wallet_id','currency','amount') %in% names(x))){
      stop(paste0("Plotting wallet manager possible only for results including value, use wallet_manager with value_calc=TRUE param"))
    } else {
      stop(paste0("unknown data.table object provided to Rbitcoin.plot, read manual for supported objects"))
    }
  } else {
    stop(paste0("unknown object provided to Rbitcoin.plot, read manual for supported objects"))
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
  if(verbose > 0) cat(as.character(Sys.time()),': Rbitcoin.plot: plotting finished','\n',sep='')
  invisible(TRUE)
}

Rbitcoin.plot.wallet <- function(x, mask = FALSE, ...,
                                 verbose = getOption("Rbitcoin.verbose",0)){
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
  if(x[J(FALSE,v.value_currency), nrow(unique(.SD)), .SDcols=c('wallet_id')] <= 1){
    stop(paste0("Cannot plot time on x axis: provided combined wallet data consists only one (non-NA measure) observation for (recent used) value currency: ",v.value_currency,", be sure to load wallet archive using wallet_dt <- wallet_manager(archive_write=FALSE, archive_read=TRUE) or readRDS(), by default wallet_manager function return only recent wallet data. See examples"), call.=FALSE)
  }
   
  # plot window for matrix of plots
  #op <- par(mfrow=c(3,2),mar=c(5.1,4.1,4.1,2.1)) #default margins
  op <- par(mfrow=c(3,2),mar=c(4.6,4.1,3.6,2.1))
  on.exit(par(op), add = TRUE)
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
    invisible(NULL)
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
              case.legend(legend_, col_) # different legend for interarctive graphics device and for export due to scaling issue
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
              case.legend(legend_, col_) # different legend for interarctive graphics device and for export due to scaling issue
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
              case.legend(legend_, col_) # different legend for interarctive graphics device and for export due to scaling issue
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
              case.legend(legend_, col_) # different legend for interarctive graphics device and for export due to scaling issue
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
  if(verbose > 0) cat(as.character(Sys.time()),': Rbitcoin.plot.wallet: plotting','\n',sep='')
  invisible(TRUE)
}

Rbitcoin.plot.trades <- function(x, ..., verbose = getOption("Rbitcoin.verbose",0)){
  plot(x = x[['trades']][['date']], y = x[['trades']][['price']],
       type = 'l', xlab = 'time', ylab = 'price',
       main = paste(x[['market']],paste0(x[['base']],x[['quote']]),'trades',sep=' '),
       sub = as.character(x[['timestamp']], format = '%Y-%m-%d %H:%M:%S %Z'),
       ...)
  mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
  grid()
  if(verbose > 0) cat(as.character(Sys.time()),': Rbitcoin.plot.trades: performing plot','\n',sep='')
  invisible()
}

Rbitcoin.plot.order_book <- function(x, ..., verbose = getOption("Rbitcoin.verbose",0)){
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
  if(verbose > 0) cat(as.character(Sys.time()),': Rbitcoin.plot.order_book: performing plot','\n',sep='')
  invisible()
}
