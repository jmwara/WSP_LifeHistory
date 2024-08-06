READ ME file for Waraniak JM, Rogowski D, Stockwell CA ms. Life history tradeoffs mediated by digene paraistes in the protected White Sands pupfish (Cyprinodon tularosa)

This READ ME file describes the datasets and R code necessary to run all analyses described in the above manuscript.

- - - - - - - - - - - - - - - - - 

Rogowski_PhD_data2.xls
Format: Microsoft Excel Workbook
Sheet: life history - Data sheet with measurements of life history traits and parasite counts for 827 White Sands pupfish. 
	* Column A: Entry Number
	* Column B: Individual ID Code (Site Abbreviation + 4-digit sample ID)
	* Column C: Site ID Number
	* Column D: copy of Site ID Number
	* Column E: Site Abbreviation (System Abbreviation + u - upper, l - lower, m - middle, or s - spring)
	* Column F: System Abbreviation (s - Salt Creek, a - Malpais, l - Lost Creek, m - Mound)
	* Column G: Sample Year
	* Column H: Sample Month (6 - June, 7 - July, 8 - August)
	* Column I: Sample Date (1-31)
	* Column J: Initials of Dissector who tallied parasite counts
	* Column K: Fish Sex (f - Female, m - Male)
	* Column L: Fish Age (years)
	* Column M: fish length (mm)
	* Column N: fish wet weight (g)
	* Column O: left eye parasite count
	* Column P: right eye parasite count
	* Column Q: right first gill arch parasite count
	* Column R: right second gill arch parasite count
	* Column S: right third gill arch parasite count
	* Column T: right fourth gill arch parasite count
	* Column U: left first gill arch parasite count
	* Column V: left second gill arch parasite count
	* Column W: left third gill arch parasite count
	* Column X: left fourth gill arch parasite count 
	* Column Y: total gill parasite count
	* Column Z: liver parasite count
	* Column AA: mesentery parasite count
	* Column AB: total parasite count 
	* Column AC: presence of a tapeworm
	* Column AD: Mass of empty vial used for drying (g)
	* Column AE: Mass of dried fish and vial combined (g)
	* Column AF: Mass of dried fish alone (Column AE - Column AD)
	* Column AG: Mass of vial with dried fish with lipids removed (g)
	* Column AH: Mass of lipids (Column AE - Column AG)
	* Column AI: Mass of water from fish tissue (Column N - Column AF)
	* Column AJ: Mass of empty vial used for reproductive tissue (g) 
	* Column AK: Mass of vial with reproductive tissue (g)
	* Column AL: Mass of reproductive tissue dry weight (Column AK - Column AJ)
	* Column AM: Mass of vial with reproductive tissue after lipid removal (g)
	* Column AN: Mass of lipids from reproductive tissue (Column AK - Column AM)
	* Column AO: presence of yolked eggs (yes/no; . for N/A)
	* Column AP: Mass of empty vial for measuring eggs (g)
	* Column AQ: Mass of vial with eggs (g)
	* Column AR: Mass of egg dry wight (Column AQ - Column AP)
	* Column AS: Egg Count
	* Column AT: Average egg mass (Column AR / Column AS)
	* Column AU: total eye parasite count
	* Column AV: Miscellaneous notes
	* Column AW: Ascocotyl spp. mesentery parasite count
	* Column AX: Posthodiplostomum minimum mesentery parasite count
Sheet: pf-temp - Data sheet that contains temperature data from loggers placed in each sample site
	* Column A: short form date of temperature record (mm/dd/yy)
	* Column B: time of temperature record
	* Column C: copy of Column A
	* Column D: Temperature recorded in upper Salt Creek site (ºC)
	* Column E: Temperature recorded in middle Salt Creek site (ºC)
	* Column F: Temperature recorded in lower Salt Creek site (ºC)
	* Column G: Temperature recorded in upper Lost River site (ºC)
	* Column H: Temperature recorded in middle Lost River site (ºC)
	* Column I: Temperature recorded in lower Lost River site (ºC)
	* Column J: Temperature recorded in Malpais spring site (ºC)
	* Column K: Temperauter recorded in Malpais spring marsh site (ºC)
	* Column L: Temperature recorded in Mound lower site (ºC)

WSP_LifeHistory_Analyses.R
Format: R script
Contains R code to complete all analyses and produce all figures used in the manuscript.
