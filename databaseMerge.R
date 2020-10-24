#################################################################################

        ##########################################################
        #Deveolped by Pablo Martinez Angerosa - Vanessa Alcalde  #
        ##########################################################

#################################################################################


############
# librarys #
############

library(tidyverse)

#############
# databases #
#############

# Bitcoin precio diario desde 2020/9/9 
btc_price = read.csv("Coinbase_BTCUSD_d.csv")

# principales criptomonedas diarias desde 2020/9/9 
eth_price = read.csv("Coinbase_ETHUSD_d.csv")
ltc_price = read.csv("Coinbase_LTCUSD_d.csv")
bch_price = read.csv("Coinbase_BCHUSD_d.csv")
etc_price = read.csv("Coinbase_ETCUSD_d.csv")
link_price = read.csv("Coinbase_LINKUSD_d.csv")
rep_price = read.csv("Coinbase_REPUSD_d.csv")
xlm_price = read.csv("Coinbase_XLMUSD_d.csv")
xrp_price = read.csv("Coinbase_XRPUSD_d.csv")

# Google trends de busqueda "Bitcoin" desde 2020/9/9
google_trends_bitcoin = read.csv("GoogleTrends_Bitcoin_d.csv")
google_trends_bitcoin = data.frame("index"= c(1:233), "bitcoin"= google_trends_bitcoin$bitcoin[1:233] , "day" = google_trends_bitcoin$Day[1:233])
google_trends_bitcoin = google_trends_bitcoin %>% arrange(desc(index))


#######################
# Mergeamos los datos #
#######################

# se toma los "precios del open" en el momento n
# la "cantidad de volumen" en el momento n-1
# la variable explicada "precio del close de bitcoin" esta en el momento n.
# se agregan los lags de 1 a 5 del precio del bitcion y volumen de bitcoin. 

DATABASE_COINS = data.frame(
  "btc_trend" = google_trends_bitcoin$bitcoin[2:233], 
  "btc_close" = btc_price$Close[1:232], 
  "eth_open"  = eth_price$Open[1:232], 
  "eth_vol"   = eth_price$Volume.ETH[2:233],
  "ltc_open"  = ltc_price$Open[1:232], 
  "ltc_vol"   = ltc_price$Volume.LTC[2:233],
  "bch_open"  = bch_price$open[1:232], 
  "bch_vol"   = bch_price$Volume.BCH[2:233],
  "etc_open"  = etc_price$open[1:232], 
  "etc_vol"   = etc_price$Volume.ETC[2:233],
  "link_open" = link_price$open[1:232], 
  "link_vol"  = link_price$Volume.LIN[2:233],
  "rep_open"  = rep_price$open[1:232], 
  "rep_vol"   = rep_price$Volume.REP[2:233],
  "btc_close_lag1" = btc_price$Close[2:233],
  "btc_close_lag2" = btc_price$Close[3:234],
  "btc_close_lag3" = btc_price$Close[4:235],
  "btc_close_lag4" = btc_price$Close[5:236],
  "btc_close_lag5" = btc_price$Close[6:237],
  "btc_vol_lag1" = btc_price$Volume.BTC[2:233],
  "btc_vol_lag2" = btc_price$Volume.BTC[3:234],
  "btc_vol_lag3" = btc_price$Volume.BTC[4:235],
  "btc_vol_lag4" = btc_price$Volume.BTC[5:236],
  "btc_vol_lag5" = btc_price$Volume.BTC[6:237]
)


############
# Renko DB #
############






