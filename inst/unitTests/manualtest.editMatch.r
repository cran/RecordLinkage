# Testfälle für editMatch

# zuletzt erfolgreich getestet: 1.3.2010

# wird manuell getestet, weil interaktive Bedienung nötig


library(RecordLinkage)
data(RLdata500)
pairs=compare.dedup(RLdata500)
train <- getMinimalTrain(pairs)

ntrain <- nrow(train$pairs)

# erster Fall: keine Matche vorhanden, müssen eingetragen werden
# verursacht Warnung, ohne Fehler zu verursachen
message("einige Matche eintragen")
result <- editMatch(train)
summary(result)

# zweiter Fall: Nur Matche vorhanden, Non-Matche werden eingetragen
# verursacht Warnung, ohne Fehler zu verursachen
train$pairs$is_match=rep(1,ntrain)
message("einige Non-Matche eintragen")
result <- editMatch(train)
summary(result)

# dritter Fall: gemischte Eingabe, gemischte Ausgabe
train$pairs$is_match=rep(1,ntrain)
train$pairs$is_match[sample(ntrain, ntrain/2)]=0
summary(train)
message("einige Felder ändern")
result <- editMatch(train)
summary(result)

# vierter Fall: gemischte Eingabe, nur Matche kommen raus
train$pairs$is_match=rep(1,ntrain)
train$pairs$is_match[1] <- 0
summary(train)
message("alle Non-Matche entfernen")
result <- editMatch(train)
summary(result)

# fünfter Fall: gemischte Eingabe, NAs eintragen
train$pairs$is_match=rep(1,ntrain)
message("NAs eintragen")
result <- editMatch(train)
summary(result)

# Testfall für "linkage"-Projekt

data(RLdata10000)
pairs=compare.linkage(RLdata500, RLdata10000, blockfld=c(1,3))
train <- getMinimalTrain(pairs)
message("einige Matche eintragen")
result <- editMatch(train)
summary(result)
