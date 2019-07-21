require(DBI)
con<-dbConnect(RSQLite::SQLite(),file.choose())
dbListTables(con)
daten<-dbReadTable(con, "rteu_logger_data")


# zeichen
ggplot(daten)+geom_point(aes(x=max_signal,y=signal_bw))

# lineare regression
ggplot(daten, aes(x=max_signal,y=signal_bw))+geom_point()+geom_smooth(method="lm", se=FALSE) 

lm(data=daten,max_signal~signal_bw)
fit2<-lm(data=daten,max_signal~signal_bw)
summary(fit2)
plot(data=daten,max_signal~signal_bw)
abline(fit2)
