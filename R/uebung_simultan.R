### Schätzung eines Simultangleichungsmodells mit 'lavaan

load ("data/work-sem1.RData") # Der Datensatz

library(lavaan)
lavaan.Linke <- sem(
  ' # Mehrere Strukturgleichungen kommen in eine Zeichenkette!
  logHHeink   ~ OstWest 
  religatt    ~ OstWest 
  lr.self     ~ logHHeink + religatt + OstWest
  scalo.CDU ~ lr.self +  OstWest 
  ', # Komma nicht vergessen!
  data=work.sem1)
summary(lavaan.Linke)
