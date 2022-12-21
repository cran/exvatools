#' Generate test IOT
#'
#' Extracts example matrix and prepares it for
#' processing with `make_wio`
#' @param is_icio Logical, `TRUE` to make a ICIO-type IOT, with
#' disaggregated countries. If `FALSE`, it will be WIOD-type.
#' @param quiet Boolean, if `TRUE`, suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
generate_test_iot <- function(is_icio = TRUE, quiet = FALSE) {

  year <- 2022

  # Dimensions
  G <- 6
  GX <- if (is_icio) 10 else G
  N <- 3
  FD <- 2
  GN <- G * N
  GXN <- GX * N
  GFD <- G * FD

  # Names
  g_names <- c("ESP", "FRA", "MEX", "USA", "CHN", "ROW")
  if (is_icio){
    gx_names <- c("ESP", "FRA", "MEX", "USA", "CHN", "ROW",
                  "MX1", "MX2", "CN1", "CN2")
  } else{
    gx_names <- g_names
  }
  n_names <- c("D01T09", "D10T39", "D41T98")
  fd_names <- c("CONS", "INVST")
  gn_names <- paste0(rep(g_names, each = N), gsub("D", "_", n_names))
  gxn_names <- paste0(rep(gx_names, each = N), gsub("D", "_", n_names))
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)

  # Basic matrices
  if (!quiet) {cli::cli_alert_info("Getting matrices Z, Y, X")}

  if (is_icio) {
    df <- get("iciotest_data")
    Z <- as.matrix(df[1:GXN, 1:GXN])
    Yfd <- as.matrix(df[1:GXN , (GXN + 1):(GXN + GFD)])
  } else {
    df <- get("wiodtest_data")
    Z <- as.matrix(df[1:GN, 1:GN])
    Yfd <- as.matrix(df[1:GN , (GN + 1):(GN + GFD)])
  }


  rownames(Z) <- colnames(Z) <- gxn_names
  rownames(Yfd) <- gxn_names
  colnames(Yfd) <- gfd_names

  # Aggregation of Y
  Y <- matrix(0, GXN, G)
  for(r in 1:G) {
    p <- (r - 1) * FD + 1
    q <- (r - 1) * FD + FD
    Y[,r] <- rowSums(Yfd[, p:q])
  }
  rownames(Y) <- gxn_names
  colnames(Y) <- g_names

  X <- rowSums(Z) + rowSums(Y)
  VA <- X - colSums(Z)
  names(VA) <- names(X) <- gxn_names


  # Create io object
  io <- list(Z, Yfd, Y, VA, X)
  names(io) <- c("Z", "Yfd", "Y", "VA", "X")

  # Metadata (dimensions)
  io$dims <- list(G, N, FD, GX, GN, GXN, GFD)
  names(io$dims) <- c("G","N","FD", "GX","GN","GXN", "GFD")

  # Metadata (names)
  io$names <- list(g_names, n_names, fd_names, gx_names,
                    gn_names, gxn_names, gfd_names)
  names(io$names) <- c("g_names","n_names","fd_names",
                       "gx_names", "gn_names", "gxn_names",
                       "gfd_names")

  if (is_icio) {
    io$type <- "iciotest"
  } else {
    io$type <- "wiodtest"
  }

  io$year <- year

  return(io)

}
