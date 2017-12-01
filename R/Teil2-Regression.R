### Eine künstliche Regression

x <- rep(1:5,each=20)
y <- 0.3 + 0.5*x + rnorm(n=length(x),sd=.3)
plot(x,y)
abline(a=.3,b=.5,col="red")

x <- rnorm(n=100,mean=3)
y <- 0.3 + 0.5*x + rnorm(n=length(x),sd=.3)
plot(x,y)
abline(a=.3,b=.5,col="red")

## Schätzung einer künstlichen Regression
# Wir erzeugen künstliche Daten
set.seed(2)
x <- rep(1:5,each=20)
y <- 0.3 + 0.5*x + rnorm(n=length(x),sd=.3)
DataFrame1 <- data.frame(x,y)
x <- rnorm(n=100,mean=3)
y <- 0.3 + 0.5*x + rnorm(n=length(x),sd=.3)
DataFrame2 <- data.frame(x,y)
# Der Arbeitsbereich wird aufgeräumt
rm(x,y)

# Wir holen uns die Daten vom Data Frame
lm1 <- lm(y~x,data=DataFrame1)
# Das "lm"-Object
print(lm1)
# Die Koeffizienten
coef(lm1)
# Die Zusammenfassung des Modells
summary(lm1)

### Ein reales Beispiel: Berufsprestige, Einkommen und Bildung
library(car)
# Bildungsdauer als unabh. Variable
lm.prestige_education <- lm(prestige~education,
                            data=Prestige)
summary(lm.prestige_education)

plot(prestige~education,data=Prestige)
abline(lm.prestige_education,col="red")

# Einkommen als unabh. Variable
lm.prestige_income <- lm(prestige~income,
                            data=Prestige)
summary(lm.prestige_income)

plot(prestige~income,data=Prestige)
abline(lm.prestige_income,col="red")
# Offensichtlich keine besonders gute Passung
# Logarithmus des Einkommens als unabh. Variable
lm.prestige_logincome <- lm(prestige~log(income),
                            data=Prestige)
summary(lm.prestige_logincome)

lm.prestige_biv <- lm(prestige~education+log(income),
                            data=Prestige)
summary(lm.prestige_biv)
