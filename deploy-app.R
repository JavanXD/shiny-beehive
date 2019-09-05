install.packages('rsconnect')

library(rsconnect)
rsconnect::setAccountInfo(name='honeypi', token='token', secret='secret')

rsconnect::deployApp('./')
