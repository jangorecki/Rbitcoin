
# rbtc.plot.wallet_manager ---------------------------------------------------

#' @title Plot wallet manager results
#' @description Plot wallet manager results archive
#' @param wallet_dt data.table, result from \code{wallet_manager}
#' @param type character, type of plot: \code{"value"} - wallet value in time, \code{"amount"} - wallet amount in time (not yet supported), \code{"recent"} - recent wallet.
#' @param mask logical default \code{FALSE}. If set to \code{TRUE} it will mask sensitive informations such as: amounts, values, blockchain addresses, auth accounts.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose",0)} is used.
#' @return invisibily \code{NULL}. Side effect a base plot, for type \code{"value"} or \code{"amount"} the dashboard panel to track different measures of your assets over time. For type \code{"recent"} a base plot of the most recent non-NA measures wallet data.
#' @section wallet manager archive:
#' To be able to track wallet assets value over time user needs to use \code{archive_write=TRUE} at least twice in \code{wallet_manager} processing (with non-NA measures). Using the cryptocurrency which do not have any exchange path to \code{transfer_currency_pair} and/or \code{value_currency} will result \code{NA} as \code{value}. Error on data downloading from external sources (wallets or exchange rates) will also result \code{NA}. Any wallet processing batch which will contains at least one \code{NA} measure will be omitted from plot. If you have some crypto not currenctly supported you may extend dictionary for more currencies or provide its value as manual source to \code{wallet_manager} already calculated in common value currency, remember to comment out the previous source which returns the \code{NA} measure.\cr To plot wallet manager data load wallet archive data, see examples.
#' Target value currency is taken from the last execution of \code{wallet_manager} archive.
#' @seealso \code{\link{wallet_manager}}, \code{\link{rbtc.plot}}
#' @export
#' @examples
#' \dontrun{
#' wallet_dt <- wallet_manager(archive_write=FALSE, archive_read=TRUE)
#' # plot wallet value over time
#' rbtc.plot(wallet_dt)
#' # the same but with masked sensitive data
#' rbtc.plot(wallet_dt, mask=TRUE)
#' # legend lookS better when plot to file
#' svg("wallet_value.svg")
#' rbtc.plot(wallet_dt, type="value", mask=TRUE)
#' dev.off()
#' # plot recent wallet value
#' rbtc.plot(wallet_dt, type="recent")
#' }
rbtc.plot.wallet_manager <- function(wallet_dt, type = c("value","recent"), 
                                     mask = getOption("Rbitcoin.plot.mask",FALSE),
                                     verbose = getOption("Rbitcoin.verbose",0)){
  x <- copy(wallet_dt)
  
  # NA wallet_id groups! will be omitted
  x[,NA_group := any(is.na(value) | is.na(amount)), by=wallet_id]
  
  # choose last process meta
  last_wallet_meta <- x[!evalq(NA_group)][which.max(wallet_id),list(wallet_id, value_currency)]
  
  # types validation and match
  unq_wallet_id <- x[!evalq(NA_group) & value_currency == last_wallet_meta$value_currency,unique(wallet_id)]
  if(length(unq_wallet_id)==0){
    stop("In all wallet manager observations there are NA measures, fix the source definition all needs to working without NA results")
  }
  else if(length(unq_wallet_id)==1){
    if(missing(type)){
      type="recent"
    }
    else if(type %in% c("value","amount")){
      warning("Provided wallet data contains only one non-NA measure observation, plot will be performed as `type='recent'`. read ?wallet_manager")
      type="recent"
    }
  }
  else if(length(unq_wallet_id)>1){
    if(missing(type)){
      type="value"
    }
  }
  
  # notification if type="recent" AND last NA_group TRUE
  if( type == "recent" & x[which.max(wallet_id),NA_group] == TRUE ){
    last_valid_wallet_id <- x[!evalq(NA_group) & value_currency == last_wallet_meta$value_currency][which.max(wallet_id),wallet_id]
    if(length(last_valid_wallet_id) & interactive()) warning(paste0("wallet manager plot will be performed for ",as.character(as.POSIXct(last_valid_wallet_id, origin="1970-01-01", tz="UTC"))," wallet data due to NA measures in the last wallet manager process. read ?wallet_manager"))
  }
  
  # if recent cut to last
  if(type=="recent") x <- x[wallet_id==max(wallet_id)]
  
  # aggregate to unq set - if 2 accounts missing auth they will be aggregated
  x <- x[!evalq(NA_group) & value_currency == last_wallet_meta$value_currency
         ][,NA_group:=NULL
           ][,list(amount = sum(amount), value_rate = sum(value_rate*amount)/sum(amount), value = sum(value)), by=c("wallet_id","currency","currency_type","auth","location","location_type","value_currency")
             ]
  if(nrow(x)==0) stop(paste0("Provided wallet archive contains 0 (non-NA measure) observation for (recently used) value currency ",last_wallet_meta$value_currency,". One of the defined sources in wallet_manager might not work. Read NA measure section in ?wallet_manager"))
  
  
  if(mask){
    address_dt <- x[location_type=="blockchain",list(new_address = paste("address",.GRP,sep="_")), keyby=list(location_type,location)]
    auth_dt <- x[,list(new_auth = paste("auth",.GRP,sep="_")), keyby=list(auth)]
    if(nrow(address_dt) > 0){
      setkeyv(x,c("location_type","location"))
      x <- address_dt[x][location_type=="blockchain", location:=new_address
                         ][,list(wallet_id, currency, currency_type, auth, location, location_type, amount, value_currency, value_rate, value)
                           ]
    }
    if(nrow(auth_dt) > 0){
      setkeyv(x,c("auth"))
      x <- auth_dt[x][,auth:=new_auth
                      ][,list(wallet_id, currency, currency_type, auth, location, location_type, amount, value_currency, value_rate, value)
                        ]
    }
  } #  mask address in location_type=="blockchain" and auth
  
  # trunc address nchar > 10
  x[nchar(location)>10, location:=paste0(substr(location,1,7),"...")]
  
  if(mask){
    if(type %in% c("value","amount")){
      first_wallet <- x[wallet_id==min(wallet_id)]
      x[,value:=value/first_wallet[,sum(value)], by="wallet_id"]
    }
    else if(type=="recent"){
      x[,value:=value/sum(value), by="wallet_id"]
    }
  } # mask values
  
  # route plot type
  if(type=="value"){
    r <- rbtc.plot.wallet_manager.value(x, mask=mask, verbose=verbose-1)
  }
  else if(type=="amount"){
    r <- rbtc.plot.wallet_manager.amount(x, mask=mask, verbose=verbose-1)
  }
  else if(type=="recent"){
    r <- rbtc.plot.wallet_manager.recent(x = x, mask=mask, verbose=verbose-1)
  }
  invisible(r)
}

# rbtc.plot.wallet_manager.value -----------------------------------------------------------------

rbtc.plot.wallet_manager.value <- function(x, mask, verbose=0){
  # plot window for matrix of plots
  par(mfrow=c(3,2),mar=c(3.1,2.6,2.1,1.1),oma=c(1.1,0,2.6,0))
  
  # by .
  x[,list(value = sum(value)),keyby=c("wallet_id","value_currency")
    ][,{
      plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
           y = value, type = 'l', col = 1, xlab="", ylab="",
           main = paste('total'))
      legend_("topleft", legend = "value", fill = 1)
      invisible(NULL)
    }]
  # by auth
  x[,list(value = sum(value)),keyby=c("auth","wallet_id","value_currency")
    ][,{
      plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
           y = value, type = 'n', xlab="", ylab="",
           main = paste('by auth'))
      unq_auth <- unique(auth)
      lg <- data.table(auth = unq_auth, label = na.fill(unq_auth,"NA"), col = as.integer(as.factor(na.fill(unq_auth,"NA")))+1, key = "auth")
      for(v.auth in lg$auth) lines(x = as.POSIXct(.SD[.(v.auth), wallet_id], origin='1970-01-01', tz='UTC'),
                                   y = .SD[.(v.auth), value],
                                   col = lg[v.auth, col])
      lg[, legend_("topleft", legend = label, fill = col)]
      invisible(NULL)
    }]
  # by currency value
  x[,list(value = sum(value)),keyby=c("currency","wallet_id","value_currency")
    ][,{
      plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
           y = value, type = 'n', xlab="", ylab="",
           main = paste('by currency'))
      unq_currency <- unique(currency)
      lg <- data.table(currency = unq_currency, label = na.fill(unq_currency,"NA"), col = as.integer(as.factor(na.fill(unq_currency,"NA")))+1, key = "currency")
      for(v.currency in lg$currency) lines(x = as.POSIXct(.SD[.(v.currency), wallet_id], origin='1970-01-01', tz='UTC'),
                                           y = .SD[.(v.currency), value],
                                           col = lg[v.currency, col])
      lg[, legend_("topleft", legend = label, fill = col)]
      invisible(NULL)
    }]
  # by currency_type
  x[,list(value = sum(value)),keyby=c("currency_type","wallet_id","value_currency")
    ][,{
      plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
           y = value, type = 'n', xlab="", ylab="",
           main = paste('by currency type'))
      unq_currency_type <- unique(currency_type)
      lg <- data.table(currency_type = unq_currency_type, label = na.fill(unq_currency_type,"NA"), col = as.integer(as.factor(na.fill(unq_currency_type,"NA")))+1, key = "currency_type")
      for(v.currency_type in lg$currency_type) lines(x = as.POSIXct(.SD[.(v.currency_type), wallet_id], origin='1970-01-01', tz='UTC'),
                                                     y = .SD[.(v.currency_type), value],
                                                     col = lg[v.currency_type, col])
      lg[, legend_("topleft", legend = label, fill = col)]
      invisible(NULL)
    }]
  # by location
  x[,list(value = sum(value)),keyby=c("location","wallet_id","value_currency")
    ][,{
      plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
           y = value, type = 'n', xlab="", ylab="",
           main = paste('by location'))
      unq_location <- unique(location)
      lg <- data.table(location = unq_location, label = na.fill(unq_location,"NA"), col = as.integer(as.factor(na.fill(unq_location,"NA")))+1, key = "location")
      for(v.location in lg$location) lines(x = as.POSIXct(.SD[.(v.location), wallet_id], origin='1970-01-01', tz='UTC'),
                                           y = .SD[.(v.location), value],
                                           col = lg[v.location, col])
      lg[, legend_("topleft", legend = label, fill = col)]
      invisible(NULL)
    }]
  # by location_type
  x[,list(value = sum(value)),keyby=c("location_type","wallet_id","value_currency")
    ][,{
      plot(x = as.POSIXct(wallet_id, origin='1970-01-01', tz='UTC'),
           y = value, type = 'n', xlab="", ylab="",
           main = paste('by location type'))
      unq_location_type <- unique(location_type)
      lg <- data.table(location_type = unq_location_type, label = na.fill(unq_location_type,"NA"), col = as.integer(as.factor(na.fill(unq_location_type,"NA")))+1, key = "location_type")
      for(v.location_type in lg$location_type) lines(x = as.POSIXct(.SD[.(v.location_type), wallet_id], origin='1970-01-01', tz='UTC'),
                                                     y = .SD[.(v.location_type), value],
                                                     col = lg[v.location_type, col])
      lg[, legend_("topleft", legend = label, fill = col)]
      invisible(NULL)
    }]
  
  # labels etc
  par(mfrow=c(1,1),mar=c(5.1,4.1,2.1,2.1), oma=rep(0,4))
  par(new=TRUE)
  x[,{
    title(main = paste(c("Wallet value over time in",value_currency[1],if(mask) "ratio"), collapse=" "),
          sub = as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z'),
          cex.sub = 0.7)
  }] # main and sub
  mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
  par(new=FALSE)
  par(mar=c(5.1,4.1,4.1,2.1))
  if(verbose > 0) cat(as.character(Sys.time()),': rbtc.plot.wallet_manager.value: plot finished','\n',sep='')
  invisible(NULL)
}

# rbtc.plot.wallet_manager.recent ------------------------------------------------------

rbtc.plot.wallet_manager.recent <- function(x, mask, verbose=0){
  value_currency <- x[,value_currency[1]]
  # plot window for matrix of plots
  par(mfrow=c(2,2),mar=c(3.1,2.6,2.1,1.1),oma=c(1.1,0,2.6,0))
  jj_formula <- c(as.formula(. ~ currency),
                  as.formula(auth ~ currency),
                  as.formula(location ~ currency),
                  as.formula(location_type ~ currency))
  f <- function(j_formula, x){ # j_formula=jj_formula[[1]]
    column <- j_formula[[2]]
    if(as.character(column)=="."){
      row.names = "value"
      dt <- x[,list(value=sum(value)),by="currency"][, setattr(as.list(value), 'names', currency)]
      col <- "grey"
      fill = col
      main = paste('total')
    } # exception
    else{
      dt <- dcast.data.table(x, j_formula, fun = sum)
      row.names <- dt[,eval(column)]
      dt[,as.character(column):=NULL]
      col = 2:(length(row.names)+1)
      fill = col
      main = paste('by ',gsub("_"," ",as.character(column)))
    }
    setcolorder(dt,names(dt)[order(-dt[,lapply(.SD,sum)])])
    mx <- dt[,as.matrix(setattr(setDF(.SD),"row.names",row.names))]
    barplot(mx, col = col, main = main)
    legend_("topright",legend=row.names, fill=fill)
    invisible(NULL)
  }
  r <- lapply(jj_formula, f, x)
  
  # labels etc
  par(mfrow=c(1,1),mar=c(5.1,4.1,2.1,2.1), oma=rep(0,4))
  par(new=TRUE)
  x[,{
    title(main = paste(c("Recent wallet currency value in",value_currency[1],if(mask) "ratio"), collapse=" "),
          sub = as.character(as.POSIXct(max(wallet_id), origin='1970-01-01', tz='UTC'), format = '%Y-%m-%d %H:%M:%S %Z'),
          cex.sub = 0.7)
  }] # main and sub
  mtext(side=1, line=-1, text="plot by Rbitcoin", adj=0, outer=TRUE, col = "darkgrey", cex = 0.7)
  par(new=FALSE)
  par(mar=c(5.1,4.1,4.1,2.1))
  if(verbose > 0) cat(as.character(Sys.time()),': rbtc.plot.wallet_manager.recent: plot DONE','\n',sep='')
  invisible(NULL)
}

# rbtc.plot.wallet_manager.amount -------------------------------------------------------------

rbtc.plot.wallet_manager.amount <- function(x, mask, verbose=0){
  stop("rbtc.plot.wallet_manager type='amount' not yet supported")
}

# helpers -----------------------------------------------------------------

legend_ <- function(..., cex = 0.7, y.intersp = 0.8, seg.len = 1, bty = if(!dev.interactive()) "o" else "n"){
  graphics::legend(..., cex=cex, y.intersp=y.intersp, seg.len=seg.len, bty=bty)
}

na.fill <- function(x, fill){
  x[is.na(x)] <- fill
  x
}
