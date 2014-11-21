# Rbitcoin

Utilities related to Bitcoin. Unified markets API interface (bitstamp, kraken, btce, hitbtc, bitmarket). Both public and private API calls. Other utils related to cryptocurrencies.

See [introduction vignette](http://cran.r-project.org/web/packages/Rbitcoin/vignettes/introduction.html) and [shinyBTC](https://github.com/jangorecki/shinyBTC) a simple GUI for Rbitcoin.

**Current version**: 0.9.3.9 [NEWS](https://github.com/jangorecki/Rbitcoin/blob/master/NEWS)

Biggest changes are:
* built-in antiddos
* hitbtc market support
* toBTC/fromBTC funs
* improved wallet manager and its plot
* simplified new market integration

## Installation

```R
# Rbitcoin 0.9.2 from CRAN
install.packages("Rbitcoin")
# Rbitcoin 0.9.3 from github
devtools::install_github("jangorecki/Rbitcoin", build_vignettes=TRUE) # vignettes optional, takes a minute
devtools::install_github("jangorecki/Rbitcoin") # no vignettes, much faster
```

## Usage

```R
library(Rbitcoin)
vignette("introduction", package="Rbitcoin")
?btc
```

## License

[MIT license](http://opensource.org/licenses/MIT)

## Contact

`J.Gorecki@wit.edu.pl`
