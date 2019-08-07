install.packages('rsconnect')

library(rsconnect)
rsconnect::setAccountInfo(name='honeypi', token='3BD7EBD41FB8196D8E683C0D5649A599', secret='QjA3Mmlfo1DcJrHWuAlneVQx2yAxiW9z1XVxpikx')

rsconnect::deployApp('./')