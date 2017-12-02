#### Beispiel Faktorenanalyse

## Explorative Faktorenanalyse
lijphart <- read.csv("lijphart.csv")
fa1.lijphart <- factanal(lijphart[, -(1:3)], factors=1)
fa2.lijphart <- factanal(lijphart[, -(1:3)], factors=2)
# oder:
fa1.lijphart <- factanal(~effnumpa
                         +minwin
                         +exdom
                         +disprop
                         +pluralis
                         +federali
                         +bicamera
                         +constrig
                         +judrevie
                         +centralb,
                         data=lijphart, factors=1)
                         
fa2.lijphart <- factanal(~effnumpa
                         +minwin
                         +exdom
                         +disprop
                         +pluralis
                         +federali
                         +bicamera
                         +constrig
                         +judrevie
                         +centralb,
                         data=lijphart, factors=2)
fa1.lijphart 
fa2.lijphart


## Konfirmatorische Faktorenanalyse

library(lavaan)
# Unstandardised indicators
cfa.lijphart <- lavaan(
  '
  F1 =~ effnumpa + minwin + exdom + disprop + pluralis
  F2 =~ federali + bicamera + constrig + judrevie + centralb
  ',
  auto.var=TRUE,       # Set of unique factors variances free
  auto.fix.first=TRUE, # Fix first loadings to 1
  auto.cov.lv.x=TRUE,  # Set covariance among factors free
  model.type="cfa",
  data=lijphart)
summary(cfa.lijphart)
# Standardised indicators
cfas.lijphart <- lavaan(
  '
  F1 =~ effnumpa + minwin + exdom + disprop + pluralis
  F2 =~ federali + bicamera + constrig + judrevie + centralb
  ',
  auto.var=TRUE,       # Set of unique factors variances free
  auto.fix.first=TRUE, # Fix first loadings to 1
  auto.cov.lv.x=TRUE,  # Set covariance among factors free
  std.ov=TRUE,         # Standardise observed variables
  model.type="cfa",
  data=lijphart)
summary(cfas.lijphart)

# Same model, different way to specify restrictions
cfas.lijphart <- lavaan(
  '
  F1 =~ 1*effnumpa + minwin + exdom + disprop + pluralis
  F2 =~ 1*federali + bicamera + constrig + judrevie + centralb

  F1~~F2
  ',
  auto.var=TRUE,       # Set of unique factors variances free
  std.ov=TRUE,         # Standardise observed variables
  model.type="cfa",
  data=lijphart)

# Fully standardised model
cfast.lijphart <- lavaan(
  '
  F1 =~ effnumpa + minwin + exdom + disprop + pluralis
  F2 =~ federali + bicamera + constrig + judrevie + centralb

  F1~~F2
  ',
  auto.var=TRUE,       # Set of unique factors variances free
  std.ov=TRUE,         # Standardise observed variables
  std.lv=TRUE,         # Variances of Lat.Vars set to One
  model.type="cfa",
  data=lijphart)
summary(cfast.lijphart)


### Strukturgleichungsmodell mit latenten Variablen:
### Beispiel Political Action Studie
load("polact-work.RData") 
PolactWork.Germany <- subset(PolactWork,country=="GERMANY")
 
model1 <- '
  matpmat2 =~ matpmat4.2 + matpmat10.2
  protest2 =~ petition.2
            + boycotts.2
            + demo.2
            + refusetaxes.2
            + wildstrikes.2
            + sitins.2
            + blocktraffic.2
            + damagethings.2
            + persviolance.2
  repress2 =~ policeforce.2
            + severesentence.2
            + forbidemo.2
            + troopsbkstrikes.2
  repress2 ~ matpmat2
  protest2 ~ matpmat2
  repress2 ~~ protest2
'

mod1.Germany <- sem(model1,data=PolactWork.Germany)  
summary(mod1.Germany)

model2 <- '
  matpmat1 =~ matpmat4.1 + matpmat10.1
  matpmat2 =~ matpmat4.2 + matpmat10.2
  formsec =~ father.educ + father.prestige + mother.educ + self.educ
  matpmat1 ~ formsec
  matpmat2 ~ matpmat1
'

mod2.Germany <- sem(model2,data=PolactWork.Germany)
summary(mod2.Germany)


### Tests von Strukturgleichungsmodell mit latenten Variablen:
### Beispiel GLES 2009

library(lavaan)
load(file="GLES2009-pca.RData")

## Ausgangsmodell
repres.mod <- '
  socecon =~ reprInt.Unions + reprInt.Employers + reprInt.EnvirGroups
           + reprInt.AntiGlob
  farmchurch =~ reprInt.FarmAssoc + reprInt.CathChurch 
           + reprInt.ProtChurch
'
cfa.repres <- lavaan(repres.mod,
                     model.type="cfa",
                     auto.var=TRUE,
                     auto.fix.first=TRUE,
                     auto.cov.lv.x=TRUE,
                     std.ov=TRUE,
                     data=GLES2009.pca
)
anova(cfa.repres)

## Modifikations-Indices
modificationIndices(cfa.repres)

# Die Modifikations-Indices legen nahe
# eine weitere Ladung auf die zweite latente
# Variable hinzuzufügen:
# farmchurch =~ reprInt.Employers
repres.mod.ex <- '
  socecon =~ reprInt.Unions + reprInt.Employers + reprInt.EnvirGroups
           + reprInt.AntiGlob
  farmchurch =~ reprInt.FarmAssoc + reprInt.CathChurch 
              + reprInt.ProtChurch
              + reprInt.Employers
'

cfa.repres.ex <- lavaan(repres.mod.ex,
                        model.type="cfa",
                        auto.var=TRUE,
                        auto.fix.first=TRUE,
                        auto.cov.lv.x=TRUE,
                        std.ov=TRUE,
                        data=GLES2009.pca
)

# Der LR-Test zeigt eine signifikante
# Verbesserung der Modellanpassung an:
anova(
  cfa.repres,
  cfa.repres.ex)
  
# Die GOF-Indices führen zu einem ähnlichen Ergebnis
sapply(
  list(
      M1=cfa.repres,
      M2=cfa.repres.ex),
  fitMeasures,
  fit.measures=c("gfi","agfi","rmsea","cfi","tli")
  )

###

load("polact-work.RData")

library(lavaan)

PolactWork.Germany <- subset(PolactWork,
                             country=="GERMANY")

fa.mod <- '
repress1 =~ policeforce.1
+ severesentence.1
+ forbidemo.1
+ troopsbkstrikes.1
'

categ.fa.Germany <- cfa(fa.mod,data=PolactWork.Germany,
                        # 'ordered' ist alles was
                        # nötig ist
                        ordered=c(
                          "policeforce.1",
                          "severesentence.1",
                          "forbidemo.1",
                          "troopsbkstrikes.1"
                        ))
summary(categ.fa.Germany)
