.onLoad <- function(libname, pkgname){
  options(Rbitcoin.verbose=0)
  #options(Rbitcoin.archive_exchange_rate=0) #no default, read ?wallet_manager
  options(RCurlOptions=list(ssl.verifypeer = TRUE, 
                            ssl.verifyhost = TRUE, 
                            cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
                            verbose = FALSE))
  options(Rbitcoin.ct.dict = list(
    crypto = c('BTC','LTC','NMC','FTC','NVC','PPC','TRC','XPM','XDG','XRP','XVN'),
    fiat = c('USD','EUR','GBP','KRW','PLN','RUR','JPY','CHF','CAD','AUD','NZD','CNY','INR',
             'TRY','SYP','GEL','AZN','IRR','KZT','NOK','SEK','ISK','MYR','DKK','BGN','HRK',
             'CZK','HUF','LTL','RON','UAH','IDR','IQD','MNT','BRL','ARS','VEF','MXN')
  ))
}