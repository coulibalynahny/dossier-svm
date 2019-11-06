library(rsconnect)

setAccountInfo(name   = Sys.getenv("coulibalynahny"),
               token  = Sys.getenv("31EB9850B4552BA87AAE54C242E30F89"),
               secret = Sys.getenv("<SECRET>"))

deployApp()

