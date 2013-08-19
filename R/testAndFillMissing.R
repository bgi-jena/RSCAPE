
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

testAndFillMissing <-function(
  ##title<< determine if one or both of the time series have gaps and fill them 
  ##description<< Function that tests the respiration and temperature time series for gaps. 
  DAT, ##<< data frame: contains temperature and respiration time series
  sf  ##<< numeric: sampling frequency of the time series
  ) {
  ##details<<
  ## Function that tests the respiration and temperature time series for gaps. 
  ## If gaps are detected, they are filled using SSA. A weight vector will be created which sets the weight for the filled values to 0
  
  ##author<<
  ##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
  library(zoo)
  DAT               <- na.trim(DAT, sides = c("both"), is.na = c("any"))
  DAT$weights       <- 1
  
  if (any(is.na(DAT$te))) {
    if (TRUE) {
      print("Temperature data has gaps! Trying Gapfilling....")
    } else {
      error("Temperature data has gaps! Please provide a dataset without gaps or enable gapfilling.")
    }
    
  }
  
  if (any(is.na(DAT$temperature))) {
    obj.fill                     <- gapfillSSA(series = DAT$te, M = (sf*3*30), remove.infinite = TRUE)
    DAT$temperature_raw          <- DAT$temperature
    DAT$temperature              <- obj.fill$filled.series
    DAT$weights[is.na(DAT$temperature_raw)]<-0
    print("Done!")
  }
  
  
  if (any(is.na(DAT$re))) {
    if (TRUE) {
      print("Respiration data has gaps! Trying Gapfilling....")
    } else {
      error("Respiration data has gaps! Please provide a dataset without gaps or enable gapfilling.")
    }
  }
  if (any(is.na(DAT$re))) {
    library("Rssa")
    obj.fill              <- gapfillSSA(series = DAT$re, M = (sf*3*30), remove.infinite = TRUE)
    DAT$respiration_raw   <- DAT$respiration
    DAT$respiration       <- obj.fill$filled.series
    DAT$weights[is.na(DAT$respiration_raw)]<-0
    print("Done!")
  }
  return(DAT)
}
