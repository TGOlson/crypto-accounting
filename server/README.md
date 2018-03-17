# crypto-accounting

curl https://min-api.cryptocompare.com/data/all/coinlist | jq '.Data[] | [.CoinName, .Symbol]'
