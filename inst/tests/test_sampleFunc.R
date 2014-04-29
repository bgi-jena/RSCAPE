require(testthat)
library("wmtsa")
context("RSCAPE")

test_that("predictR calculates correct Respiration prediction",{
		Rb <- 1:5
		S  <- 2
		tau<- 11:15
		expect_that( predictR(Rb,S,tau), equals(Rb*exp(S*tau)) )
    #Introduce lag
		expect_that( predictR(Rb,S,tau,1)[2:5], equals(Rb[2:5]*exp(S*tau[1:4])) )
	})

test_that("SCAPE gives reasonable results for a very simple example",{
  t   <- seq(0,365,by=0.25)
  Temp<- 10 + 3*sin(2*pi*t)+10*sin(2*pi*t/365)
  Rb  <- 1+0.9*sin(2*pi*t/365) # test base respiration
  q10 <- 2
  Ea  <- 8000
  Ea2 <- 0.01
  Tref<- 15
  T0  <- -46.02
  gam <- 10
  R   <- Rb*q10^((Temp-Tref)/gam)
  methods   <- c("Fourier","Spline","MA","wavMODWT")
  
  for (m in methods) {
    res_q10 <- getQ10(Temp,R,4,method=m,M=30)
    print(res_q10$SCAPE_Q10)
    ev_q10  <- evalSens(res_q10,Rb)
    print(ev_q10$SCAPE$MEF)
    expect_that( (res_q10$SCAPE_Q10<2.03) && (res_q10$SCAPE_Q10>1.97), is_true())
    expect_that( ev_q10$SCAPE$MEF>0.99, is_true())
    expect_that( ev_q10$SCAPE$Rb$MEF>0.99, is_true())
  }
  
  R   <- Rb*exp(-Ea/(8.3144621*(Temp+273.15)))
  for (m in methods) {
    res_Ea <- getArrhenius(Temp,R,4,method=m,M=30)
    ev_Ea  <- evalSens(res_Ea,Rb)
    print(ev_Ea$SCAPE$Rb$MEF)
    expect_that( (res_Ea$SCAPE_Ea<9000) && (res_Ea$SCAPE_Ea>7000), is_true())
    expect_that( ev_Ea$SCAPE$MEF>0.9, is_true())
    expect_that( ev_Ea$SCAPE$Rb$MEF>0.6, is_true())
  }
  
  Ea2<-8000
  R   <- Rb*exp(Ea2/8.3144621*(1/(Tref-T0)-1/(Temp-T0)))
  for (m in methods) {
    res_q10 <- getLloydTaylor(Temp,R,4,method=m,M=30)
    print(res_q10$SCAPE_Ea)
    ev_q10  <- evalSens(res_q10,Rb)
    print(ev_q10$SCAPE$MEF)
    expect_that( ev_q10$SCAPE$MEF>0.9, is_true())
    expect_that( ev_q10$SCAPE$Rb$MEF>0.6, is_true())
  }
  
})
