#' Smalians volume function
#'
#' This function compliments the Kozak Volume Function.
#' Function provides volume of a cylindrical object.
#'
#'@keywords internal
#'
#'@param r1 diameter on one end of cylinder
#'@param r2 diameter on second end of cylinder
#'@param len length of cylinder
#'
#'
#'
#'@export

Smalians <- function(r1, r2, len) {
  L <- (r1 / 2)^2 * pi
  S <- (r2 / 2)^2 * pi
  vol <- ((L + S) / 2) * len
  return(round(vol, 4))
}
