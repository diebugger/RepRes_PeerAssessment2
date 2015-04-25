GetCorrectAmount <- function (val, mag) {
  for (i in 1:length(mag)) {
    if (mag[i] == "K" | mag[i] == "k")
      val[i] <- val[i] * 1000
    if (mag[i] == "M" | mag[i] == "m")
      val[i] <- val[i] * 1000000
    if (mag[i] == "B" | mag[i] == "b")
      val[i] <- val[i] * 1000000000
  }
  return(val)
}
