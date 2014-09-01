.onLoad <- function(libname, pkgname){
  options(Rbitcoin.verbose=0)
  #options(Rbitcoin.archive_exchange_rate=0) #no default, read ?wallet_manager
  options(RCurlOptions=list(ssl.verifypeer = TRUE, 
                            ssl.verifyhost = as.integer(2), 
                            cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
                            verbose = FALSE))
  options(Rbitcoin.ct.dict = list(
    crypto = c('BTC','LTC','NMC','FTC','NVC','PPC','TRC','XPM','XDG','XRP','XVN'),
    fiat = c('USD','EUR','GBP','KRW','PLN','RUR','JPY','CHF','CAD','AUD','NZD','CNY','INR',
             'TRY','SYP','GEL','AZN','IRR','KZT','NOK','SEK','ISK','MYR','DKK','BGN','HRK',
             'CZK','HUF','LTL','RON','UAH','IDR','IQD','MNT','BRL','ARS','VEF','MXN')
  ))
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage("You are currently using Rbitcoin 0.9.2, be aware of the changes coming in the next releases (0.9.3 - github, 0.9.4 - cran). Do not auto update Rbitcoin to 0.9.3 (or later) without testing. For details see github.com/jangorecki/Rbitcoin. This message will be removed in 0.9.5 (or later).")
}