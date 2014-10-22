# Rbitcoin

Utilities related to Bitcoin. Unified markets API interface (bitstamp, kraken, btce, bitmarket). Both public and private API calls. Other utils related to cryptocurrencies.

See [introduction](https://github.com/jangorecki/Rbitcoin/blob/master/vignettes/introduction.Rmd) vignette and [shinyBTC](https://github.com/jangorecki/shinyBTC) a simple GUI for Rbitcoin.

**Current version**: 0.9.3 [NEWS](https://github.com/jangorecki/Rbitcoin/blob/master/NEWS)

## Significant changes post 0.9.2

* Requires latest data.table `1.9.4`.
* Each function that query over http(s) by default will invoke antiddos procedure behind the scene. Read `?antiddos` for customization.
* Now it is possible to add new markets and currency pairs just by setting `options()`.
* wallet_manager archive contains additional column.
* Renamed plotting functions.
* Read NEWS file for more details.

## Installation

```R
# Rbitcoin 0.9.2 from CRAN
install.packages("Rbitcoin")
# Rbitcoin 0.9.3 from github
devtools::install_github("jangorecki/Rbitcoin")
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

`j.gorecki@wit.edu.pl`
