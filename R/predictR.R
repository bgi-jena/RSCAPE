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
  Q10, ##<< numeric: Q10
  temperature, ##<< numeric vector: temperature time series
  lag=0, 
  Tref=15, ##<< numeric: reference tempreature
  gam=10  ##<< numeric: gamma value in Q10 equation
) {
  ##details<< This function can be useful to calculate predicted respiration for surrogate and time-lagged SCAPE output data
  ## 
  
  ##author<<
  ##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
  l<-length(temperature)
  if (lag>0) {
    temperature[lag+1:l]<-temperature[1:l-lag]
    temperature[1:lag]<-NA
  } else if (lag<0) {
    temperature[1:l+lag]<-temperature[1-lag:l]
    temperature[l+lag+1:l]<-NA
  }
  ##value<< time series of predicted respiration
  return(Rb*Q10^((temperature-Tref)/gam))
}
