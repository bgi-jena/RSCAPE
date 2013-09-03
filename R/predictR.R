#     R-Code to calculate Q10-value based on SCAPE
#     Copyright (C) 2013  Fabian Gans, Miguel Mahecha
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

predictR<-function(
  ##title<< Predict respiration
  ##description<< Calculate respiration for given basal respiration and Q10 value
  Rb, ##<< numeric vector: basal respiration
  S, ##<< numeric: Sensitivity
  tau, ##<< numeric vector: converted temperature time series
  lag=0
) {
  ##details<< This function can be useful to calculate predicted respiration for surrogate and time-lagged SCAPE output data
  ## 
  
  ##author<<
  ##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
  l<-length(tau)
  
  if (lag>0) {
    tau[(lag+1):l]<-tau[1:(l-lag)]
    tau[1:lag]<-NA
  } else if (lag<0) {
    tau[1:(l+lag)]<-tau[(1-lag):l]
    tau[(l+lag+1):l]<-NA
  }
  ##value<< time series of predicted respiration
  return(Rb*exp(S*tau))
}
