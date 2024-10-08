# exvatools 0.9.0 (2024-09-16)

* Added partial 2024 edition of ADB Multi-Regional Input-Output Tables
  (ADB-MRIO), with data up to 2023, for the 62-country version in current  
  prices (`"mrio2024"`) and for the 72-country version (`"mrio2024x"`) only.
* Added new edition of FIGARO EU Input-Output Tables (2024 edition, 2010-2022) 
* Added parameter `bkdown` for the sector breakdown of the Borin and Mancini
  decomposition, with default `"exporting"` sector and possibility of
  showing sector of `"origin"`.
* Solved issues with `set_zero()` command when using country or sector codes. 

# exvatools 0.8.0 (2024-04-05)

* Added support for ADB Multi-Regional Input-Output Tables (ADB-MRIO), with 
  data up to 2022, for the 62-country version in current prices 
  (`"mrio2023"`) and in constant 2010 prices (`"mrio2023k"`), and for the
  72-country version (`"mrio2023x"`).
* Corrected bugs in 2023 edition of FIGARO EU Input-Output Tables and other
  minor errors in sector classification.

# exvatools 0.7.0 (2024-03-21)

* Updated code to make it compatible with changes in the structure of
  zip files containing raw data for the OECD ICIO tables 2023 edition 
  (`"icio2023"` and `"icio2023s"`) after the February 2024 update.
* Added new edition of FIGARO EU Input-Output Tables 
  (2023 edition, 2010-2021) 

# exvatools 0.6.0 (2024-01-25)

* Updated code to make it compatible with changes in the structure of
  zip files containing raw data for the OECD ICIO tables 2023 edition 
  (`"icio2023"` and `"icio2023s"`) after the December 2023 update.

# exvatools 0.5.0 (2023-12-18)

* Added OECD ICIO tables 2023 edition, with normal extended data (`"icio2023"`) 
  plus a reduced version ("small") with data for `CHN` and `MEX` consolidated 
  (`"icio2023s"`).
* Updated documentation.
* Corrected bugs.


# exvatools 0.4.0 (2023-06-12)

* Added new database FIGARO EU Input-Output Tables (EU IC-SUIOTs), 2022 
  edition (2010-2020), with data by industry (`"figaro2022i"`) and by product 
  (`"figaro2022p"`).
* Simplified code for extraction of matrices.
* Corrected bugs.

# exvatools 0.3.0 (2023-03-09)

* Added `make_exvadec()` custom perspective (`sector`, `partner` or both) for
  `"bm_src"` (Borin-Mancini, source) method.
* Improved geographic groups for `"icio2021"`.
* Improved description.
* Corrected bugs.

# exvatools 0.2.0 (2023-01-20)

* New function `make_custom_wio()` that allows the possibility of creating
  a `wio` from custom data (and not only from standard databases).
* Corrected minor bugs, especially printing issues.
* Improved documentation and package description.

# exvatools 0.1.0 (2022-12-22)

* Initial version.
