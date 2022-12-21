# Make test icio
wio <- make_wio("iciotest")

# Individual
exvadec <- make_exvadec(wio, exporter = "ESP", method = "oecd",
                        output = "standard")

test_that(
  "Country exvadec: check standard country ESP",
  {
    expect_equal(round(sum(exvadec$EXGR), 2), 4666.96)
    expect_equal(round(sum(exvadec$DC),2), 2165.17)
    expect_equal(round(sum(exvadec$DVA),2), 1880.60)
    expect_equal(round(sum(exvadec$VAX),2), 1624.18)
    expect_equal(round(sum(exvadec$REF),2), 256.41)
    expect_equal(round(sum(exvadec$DDC),2), 284.58)
    expect_equal(round(sum(exvadec$FC),2), 2501.78)
    expect_equal(round(sum(exvadec$GVC),2), 5549.66)
    expect_equal(round(sum(exvadec$GVCB),2), 2501.78)
    expect_equal(round(sum(exvadec$GVCF),2), 3047.87)
  }
)

exvadec <- make_exvadec(wio, exporter = "CHN", method = "oecd",
                        output = "standard")

test_that(
  "Country exvadec: check aggregated country CHN",
  {
    expect_equal(round(sum(exvadec$EXGR), 2), 8575.51)
    expect_equal(round(sum(exvadec$DC),2), 4409.66)
    expect_equal(round(sum(exvadec$DVA),2), 3469.40)
    expect_equal(round(sum(exvadec$VAX),2), 3117.58)
    expect_equal(round(sum(exvadec$REF),2), 351.82)
    expect_equal(round(sum(exvadec$DDC),2), 940.27)
    expect_equal(round(sum(exvadec$FC),2), 4165.85)
    expect_equal(round(sum(exvadec$GVC),2), 9004.13)
    expect_equal(round(sum(exvadec$GVCB),2), 4165.85)
    expect_equal(round(sum(exvadec$GVCF),2), 4838.28)
  }
)

exvadec <- make_exvadec(wio, exporter = "NAFTA", method = "oecd",
                        output = "standard")

test_that(
  "Country exvadec: check country group NAFTA",
  {
    expect_equal(round(sum(exvadec$EXGR), 2), 10485.40)
    expect_equal(round(sum(exvadec$DC),2), 6127.84)
    expect_equal(round(sum(exvadec$DVA),2), 4613.26)
    expect_equal(round(sum(exvadec$VAX),2), 3639.16)
    expect_equal(round(sum(exvadec$REF),2), 974.09)
    expect_equal(round(sum(exvadec$DDC),2), 1514.58)
    expect_equal(round(sum(exvadec$FC),2), 4357.56)
    expect_equal(round(sum(exvadec$GVC),2), 10028.76)
    expect_equal(round(sum(exvadec$GVCB),2), 4357.56)
    expect_equal(round(sum(exvadec$GVCF),2), 5671.20)
  }
)

# Now checking global exvadec

exvadec <- make_exvadec(wio, exporter = "all", method = "oecd",
                        output = "standard")

pgn_exp <- grep("ESP", exvadec$names$gn_names)


test_that(
  "Global exvadec: check standard country ESP",
  {
    expect_equal(round(sum(exvadec$EXGR[pgn_exp, ]), 2), 4666.96)
    expect_equal(round(sum(exvadec$DC[pgn_exp, ]),2), 2165.17)
    expect_equal(round(sum(exvadec$DVA[pgn_exp, ]),2), 1880.60)
    expect_equal(round(sum(exvadec$VAX[pgn_exp, ]),2), 1624.18)
    expect_equal(round(sum(exvadec$REF[pgn_exp, ]),2), 256.41)
    expect_equal(round(sum(exvadec$DDC[pgn_exp, ]),2), 284.58)
    expect_equal(round(sum(exvadec$FC[pgn_exp, ]),2), 2501.78)
    expect_equal(round(sum(exvadec$GVC[pgn_exp, ]),2), 5549.66)
    expect_equal(round(sum(exvadec$GVCB[pgn_exp, ]),2), 2501.78)
    expect_equal(round(sum(exvadec$GVCF[pgn_exp, ]),2), 3047.87)
  }
)

pgn_exp <- grep("CHN", exvadec$names$gn_names)

test_that(
  "Global exvadec: check aggregated country CHN",
  {
    expect_equal(round(sum(exvadec$EXGR[pgn_exp, ]), 2), 8575.51)
    expect_equal(round(sum(exvadec$DC[pgn_exp, ]),2), 4409.66)
    expect_equal(round(sum(exvadec$DVA[pgn_exp, ]),2), 3469.40)
    expect_equal(round(sum(exvadec$VAX[pgn_exp, ]),2), 3117.58)
    expect_equal(round(sum(exvadec$REF[pgn_exp, ]),2), 351.82)
    expect_equal(round(sum(exvadec$DDC[pgn_exp, ]),2), 940.27)
    expect_equal(round(sum(exvadec$FC[pgn_exp, ]),2), 4165.85)
    expect_equal(round(sum(exvadec$GVC[pgn_exp, ]),2), 9004.13)
    expect_equal(round(sum(exvadec$GVCB[pgn_exp, ]),2), 4165.85)
    expect_equal(round(sum(exvadec$GVCF[pgn_exp, ]),2), 4838.28)
  }
)


pgn_exp <- grep("MEX|USA", exvadec$names$gn_names)

test_that(
  "Global exvadec: check country group NAFTA",
  {
    expect_equal(round(sum(exvadec$EXGR[pgn_exp, ]), 2), 13087.74)
    expect_equal(round(sum(exvadec$DC[pgn_exp, ]),2), 6149.93)
    expect_equal(round(sum(exvadec$DVA[pgn_exp, ]),2), 5070.13)
    expect_equal(round(sum(exvadec$VAX[pgn_exp, ]),2), 4547.65)
    expect_equal(round(sum(exvadec$REF[pgn_exp, ]),2), 522.48)
    expect_equal(round(sum(exvadec$DDC[pgn_exp, ]),2), 1079.80)
    expect_equal(round(sum(exvadec$FC[pgn_exp, ]),2), 6937.81)
    expect_equal(round(sum(exvadec$GVC[pgn_exp, ]),2), 14096.49)
    expect_equal(round(sum(exvadec$GVCB[pgn_exp, ]),2), 6937.81)
    expect_equal(round(sum(exvadec$GVCF[pgn_exp, ]),2), 7158.68)
  }
)

