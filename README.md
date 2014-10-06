# Rbitcoin

Utilities related to Bitcoin. Unified markets API interface (bitstamp, kraken, btce, bitmarket). Both public and private API calls.

**Current version**: 0.9.3 [NEWS](https://github.com/jangorecki/Rbitcoin/blob/master/NEWS)

## Significant changes post 0.9.2

* Requires latest data.table `1.9.4`.
* Each function that query over http(s) by default will invoke antiddos procedure behind the scene. Read `?antiddos` for customization.
* Now it is possible to add new markets and currency pairs just by setting `options()`.
* wallet_manager archive contains additional column.
* Read NEWS file for more details.

temporary: plot not working yet due to the changes in api.dict

## Installation

```R
# Rbitcoin 0.9.2 from CRAN
install.packages("Rbitcoin")
# Rbitcoin 0.9.3 from github
devtools::install_github("Rbitcoin", "jangorecki")
```

## Usage

```R
library(Rbitcoin)
?btc
```

## License

[MIT license](https://github.com/jangorecki/Rbitcoin/blob/master/LICENSE)

## Contact

`j.gorecki@wit.edu.pl`
