### Schätzung eines Simultangleichungsmodells mit 'systemfit'

load ("data/work-sem1.RData") # Der Datensatz
library(systemfit) # Das R-Zusatzpaket
sfit.Linke <- systemfit(
  list( # Mehrere Strukturgleichungen kommen in eine Liste
    logHHeink   ~ OstWest, # Kommas nicht vergessen!
    lr.self     ~ logHHeink + OstWest,
    scalo.Linke ~ lr.self + sqdist.Linke + OstWest # Hier kein Komma!
  ), # Komma nicht vergessen!
  data=work.sem1)
summary(sfit.Linke)

### Schätzung eines Simultangleichungsmodells mit 'lavaan

load ("work-sem1.RData") # Der Datensatz
library(lavaan)
lavaan.Linke <- sem(
  ' # Mehrere Strukturgleichungen kommen in eine Zeichenkette!
    logHHeink   ~ OstWest # keine Kommas!
    lr.self     ~ logHHeink + OstWest
    scalo.Linke ~ lr.self + sqdist.Linke + OstWest 
  ', # Komma nicht vergessen!
  data=work.sem1)
summary(lavaan.Linke)
'
