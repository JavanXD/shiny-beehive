x<-matrix(c(8,2,5,5,1,4),nrow=2)
chisq.test(x)
#Ergebnis:
#Pearson's Chi-squared test
#data: x
#x-squared = 5,1136, df = 2, p-value = 0.07755

#Praxis R - Beispielcode für Korrelationsberechnungen
#View(mtcars)
#cor(mtcars$mpg,mtcars$hp)
#-0.7761684 (pearson ist default)
#cor(mtcars$mpg,mtcars$wt)
#-0.8676594

#cor(mtcars$mpg,mtcars$hp,method = "spearman")
#-0,8946646

#install.packages("corrgram")
#require(corrgram)

#corrgram(mtcars)
#blau Korrelation
#rot keine Korrleation
#Größe gibt an, ob stärker Korrelation oder nicht
#Interessant für eine kurze Übersicht der vorhandenen Korrelationen
#Bspw. disp und wt aus mtcars
#ggplot(mtcars)+geom_point(aes(x=disp,y=wt))
#Multivariate Analyse
#ggplot(mtcars)+geom_point(aes(x=disp,y=wt,size=cyl))
#ggplot(mtcars)+geom_point(aes(x=disp,y=wt,col=cyl))
#ggplot(mtcars)+geom_point(aes(x=disp,y=wt,col=as.factor(cyl)))
#letztere ggf. besser Darstellung, da davor mehr Zylinder suggeriert werden können (bspw. 7, etc.)

#ggplot(mtcars)+geom_point(aes(x=disp,y=wt,col=as.factor(cyl),size=mpg))
#Ausreißer oben rechts ersichtlich.


#Lineare Regression = lm()
#lm(data=mtcars,mpg~hp) data legt den Datensatz fest, $spaltenname nicht mehr notwendig
#fit<-lm(data=mtcars,mpg~hp)
#summary(fit) für mehr Informationen / auch interessant für Lineares Modell
#Multiple R-squared [0,1], 1=gut/passt 0=nicht (Bestimmtheitsmaß)

#plot(data=mtcars,mpg~hp)
#abline(fit) "häßlicher Plot"
#Daher über ggplot
#ggplot(mtcars, aes(x=hp,y=mpg))+geom_point()
#ggplot(mtcars, aes(x=hp,y=mpg))+geom_point()+geom_smooth()
#ggplot(mtcars, aes(x=hp,y=mpg))+geom_point()+geom_smooth(method="lm)
#Eher nächster Plot, da sonst "unangenehme" Fragen bzw. gefährliches Halbwissen
#ggplot(mtcars, aes(x=hp,y=mpg))+geom_point()+geom_smooth(method="lm", se = FALSE)

#Splitten der Linie sinnvoll?
#ggplot(mtcars)+geom_point(aes(x=hp,y=mpg,col=factor(cyl)))
#Ja aufgrund des Einflussfaktor cyl
#ggplot(mtcars)+geom_point(aes(x=hp,y=mpg,col=factor(cyl)))+geom_smooth(aes(x=hp,y=mpg,col=factor(cyl)),method = "lm", se=FALSE)
#Auch neue Aussagefähigkeit
#Einschränken möglich:
#ggplot()+geom_point(aes(x=mtcars$hp,y=mtcars$mpg,col=factor(mtcars$cyl)))+geom_smooth(aes(x=subset(mtcars,cyl==4)$hp,y=subset(mtcars,cyl==4)$mpg),method = "lm", se=FALSE)
#Farben freidefinierbar
#ggplot()+geom_point(aes(x=mtcars$hp,y=mtcars$mpg,col=factor(mtcars$cyl)))+geom_smooth(aes(x=subset(mtcars,cyl==4)$hp,y=subset(mtcars,cyl==4)$mpg),method = "lm", se=FALSE,col="red")
#

#Einbinden SQLite-DB für Bestimmtheitsmaß & Co.
#con<-dbConnect(RSQLite::SQLite(),file.choose())
#Downloads: 2019_06_02_hamster_big.sqlite
#dbListTables(con)
#daten<-dbReadTable(con,"rteu_logger_data")
#corrgram(daten)
#ggplot(daten)+geom_point(aes(x=signal_bw,y=max_signal))
#lm(data=daten,signal_bw~max_signal)
#fit<-lm(data=daten,signal_bw~max_signal)
#summary(fit)
#ggplot(daten, aes(x=signal_bw,y=max_signal))+geom_point()+geom_smooth(method="lm", se = FALSE)
#
#
#con<-dbConnect(RSQLite::SQLite(),file.choose())
#Downloads: 2019_06_02_hamster_big.sqlite
#dbListTables(con)
#daten<-dbReadTable(con,"rteu_logger_data")
#
#str(daten) für einen ersten Überblick.
#ggplot(daten)+geom_histrogram(aes(timestamp)) Durchsicht der einzelnen Spalten per univariate Analyse // Überlicherweise per Barplot!
#tmp<-as.POSIXct(daten$timestamp,origin="1970-01-01",tz="UTC")
#head(tmp)
#ggplot(daten)+geom_histrogram(aes(duration)) Aha, Diskrete Werte
#ggplot(daten)+geom_histrogram(aes(signal_freq))
#ggplot(daten)+geom_histrogram(aes(Name),stat="count")
#ggplot(daten)+geom_bar(aes(Name)) Kein stat="count" notwendig
#ggplot(daten)+geom_bar(aes(receiver))
#ggplot(daten)+geom_histrogram(aes(max_signal))
#ggplot(daten)+geom_histrogram(aes(signal_bw))
#daten$timestamp<-as.POSIXct(daten$timestamp,origin="1970-01-01") für hübschere Timestamp-Ansicht
#ggplot(daten)+geom_point(aes(x=timestamp,y=max_signal))
#ggplot(daten)+geom_point(aes(x=timestamp,y=max_signal,col=receiver))

#Zusammenhänge suchen
#cor(daten) Fehlermeldung x muss numerisch sein
#is.numeric(daten) = FALSE
#cor(daten[,c("duration","signal_freq","max_signal","signal_bw")]) Korrelationsmatrix
#Großer Zusammenhang zwischen max_signal und signal_bw
#ggplot(daten)+geom_point(aes(x=signal_bw,y=max_signal,col=as.factor(duration))
#ggplot(daten)+geom_point(aes(x=as.factor(duration),y=max_signal,col=signal_bw)) Variablen durchtauschen, um ggf. neue Erkenntnisse rauszuziehen
#td<-diff(daten$timestamp)
#ggplot()+geom_histogram(aes(td)) Zeitabstand geht gegen Null, nicht geordnet
#td<-diff(daten$timestamp[order(daten$timestamp)])
#ggplot()+geom_histogram(aes(td))
#ggplot()+geom_point(aes(x=daten$timestamp[-1],y=td)) Interpretation der Zeitabstände // [-1] zur Anpassung der Dimensionen
#Daten schreiben / speichern
#daten$td<-c(0,td) 0 für den ersten Eintrag, td für die restlichen Werte
