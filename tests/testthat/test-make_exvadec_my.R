# Make test icio
wio <- make_wio("iciotest")

# Individual
exvadec <- make_exvadec(wio, exporter = "ESP", method = "my",
                        output = "standard")

test_that(
  "Country exvadec: check standard country ESP",
  {
    expect_equal(round(sum(exvadec$EXGR), 2), 4666.96)
    expect_equal(round(sum(exvadec$DC),2), 2165.17)
    expect_equal(round(sum(exvadec$DVA),2), 1880.60)
    expect_equal(round(sum(exvadec$DDC),2), 284.58)
    expect_equal(round(sum(exvadec$FC),2), 2501.78)
    expect_equal(round(sum(exvadec$FVA),2), 2176.21)
    expect_equal(round(sum(exvadec$FDC),2), 325.58)
  }
)

exvadec <- make_exvadec(wio, exporter = "CHN", method = "my",
                        output = "standard")

test_that(
  "Country exvadec: check aggregated country CHN",
  {
    expect_equal(round(sum(exvadec$EXGR), 2), 8575.51)
    expect_equal(round(sum(exvadec$DC),2), 4409.66)
    expect_equal(round(sum(exvadec$DVA),2), 3469.40)
    expect_equal(round(sum(exvadec$DDC),2), 940.27)
    expect_equal(round(sum(exvadec$FC),2), 4165.85)
    expect_equal(round(sum(exvadec$FVA),2), 3273.60)
    expect_equal(round(sum(exvadec$FDC),2), 892.25)
  }
)

exvadec <- make_exvadec(wio, exporter = "NAFTA", method = "my",
                        output = "standard")

test_that(
  "Country exvadec: check country group NAFTA",
  {
    expect_equal(round(sum(exvadec$EXGR), 2), 10485.40)
    expect_equal(round(sum(exvadec$DC),2), 6127.84)
    expect_equal(round(sum(exvadec$DVA),2), 4613.26)
    expect_equal(round(sum(exvadec$DDC),2), 1514.58)
    expect_equal(round(sum(exvadec$FC),2), 4357.56)
    expect_equal(round(sum(exvadec$FVA),2), 3276.30)
    expect_equal(round(sum(exvadec$FDC),2), 1081.26)
  }
)

# Now checking global exvadec

exvadec <- make_exvadec(wio, exporter = "all", method = "my",
                        output = "standard")

pgn_exp <- grep("ESP", exvadec$names$gn_names)


test_that(
  "Global exvadec: check standard country ESP",
  {
    expect_equal(round(sum(exvadec$EXGR[pgn_exp, ]), 2), 4666.96)
    expect_equal(round(sum(exvadec$DC[pgn_exp, ]),2), 2165.17)
    expect_equal(round(sum(exvadec$DVA[pgn_exp, ]),2), 1880.60)
    expect_equal(round(sum(exvadec$DDC[pgn_exp, ]),2), 284.58)
    expect_equal(round(sum(exvadec$FC[pgn_exp, ]),2), 2501.78)
    expect_equal(round(sum(exvadec$FVA[pgn_exp, ]),2), 2176.21)
    expect_equal(round(sum(exvadec$FDC[pgn_exp, ]),2), 325.58)
  }
)

pgn_exp <- grep("CHN", exvadec$names$gn_names)

test_that(
  "Global exvadec: check aggregated country CHN",
  {
    expect_equal(round(sum(exvadec$EXGR[pgn_exp, ]), 2), 8575.51)
    expect_equal(round(sum(exvadec$DC[pgn_exp, ]),2), 4409.66)
    expect_equal(round(sum(exvadec$DVA[pgn_exp, ]),2), 3469.40)
    expect_equal(round(sum(exvadec$DDC[pgn_exp, ]),2), 940.27)
    expect_equal(round(sum(exvadec$FC[pgn_exp, ]),2), 4165.85)
    expect_equal(round(sum(exvadec$FVA[pgn_exp, ]),2), 3273.60)
    expect_equal(round(sum(exvadec$FDC[pgn_exp, ]),2), 892.25)
  }
)


pgn_exp <- grep("MEX|USA", exvadec$names$gn_names)

test_that(
  "Global exvadec: check country group NAFTA",
  {
    expect_equal(round(sum(exvadec$EXGR[pgn_exp, ]), 2), 13087.74)
    expect_equal(round(sum(exvadec$DC[pgn_exp, ]),2), 6149.93)
    expect_equal(round(sum(exvadec$DVA[pgn_exp, ]),2), 5070.13)
    expect_equal(round(sum(exvadec$DDC[pgn_exp, ]),2), 1079.80)
    expect_equal(round(sum(exvadec$FC[pgn_exp, ]),2), 6937.81)
    expect_equal(round(sum(exvadec$FVA[pgn_exp, ]),2), 5732.88)
    expect_equal(round(sum(exvadec$FDC[pgn_exp, ]),2), 1204.93)
  }
)


exvadec <- make_exvadec(wio, exporter = "ESP", method = "my", perim = "WLD",
                        output = "standard")

test_that(
  "Country exvadec: ESP with perspective world",
  {
    expect_equal(round(sum(exvadec$EXGR), 2), 4666.96)
    expect_equal(round(sum(exvadec$DC),2), 2165.17)
    expect_equal(round(sum(exvadec$DVA),2), 1880.60)
    expect_equal(round(sum(exvadec$DDC),2), 284.58)
    expect_equal(round(sum(exvadec$FC),2), 2501.78)
    expect_equal(round(sum(exvadec$FVA),2), 0.00)
    expect_equal(round(sum(exvadec$FDC),2), 2501.78)
  }
)

exvadec <- make_exvadec(wio, exporter = "ESP", method = "my", perim = "WLD",
                        output = "terms")

test_that(
  "Country exvadec: ESP with perspective world and additional terms",
  {
    expect_equal(round(sum(exvadec$EXGR), 2), 4666.96)
    expect_equal(round(sum(exvadec$DVA),2), 1880.60)
    expect_equal(round(sum(exvadec$DDC),2), 284.58)
    expect_equal(round(sum(exvadec$FVA),2), 0.00)
    expect_equal(round(sum(exvadec$FDC1),2), 1015.98)
    expect_equal(round(sum(exvadec$FDC2),2), 1485.80)
  }
)


exvadec <- make_exvadec(wio, exporter = "EU27", method = "my", perim = "WLD",
                        output = "terms")

test_that(
  "Country exvadec: EU27 with perspective world and additional terms",
  {
    expect_equal(round(sum(exvadec$EXGR), 2), 7577.09)
    expect_equal(round(sum(exvadec$DVA),2), 3114.35)
    expect_equal(round(sum(exvadec$DDC),2), 807.03)
    expect_equal(round(sum(exvadec$FVA),2), 0.00)
    expect_equal(round(sum(exvadec$FDC1),2), 1648.26)
    expect_equal(round(sum(exvadec$FDC2),2), 2007.45)
  }
)
