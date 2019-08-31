require(ggplot2)
require(corrgram)

View(mtcars)
str(cor(mtcars))
cor(mtcars$mpg, mtcars$hp, method = "spearman")
corrgram(mtcars)

ggplot(mtcars)+geom_point(aes(x=disp,y=wt))
ggplot(mtcars)+geom_point(aes(x=disp,y=wt, col=as.factor(cyl)))
ggplot(mtcars)+geom_point(aes(x=disp,y=wt, col=cyl))
ggplot(mtcars)+geom_point(aes(x=disp,y=wt, col=as.factor(cyl), size=mpg))

# lineare regression
lm(data=mtcars,mpg~hp)
fit<-lm(data=mtcars,mpg~hp)
summary(fit)
# zeichnen
plot(data=mtcars,mpg~hp)
abline(fit)
ggplot(mtcars, aes(x=hp,y=mpg))+geom_point()
# abschätzung wie streuen meine punkte um meine gerade heraus
ggplot(mtcars, aes(x=hp,y=mpg))+geom_point()+geom_smooth(method="lm", se=FALSE) 
