
### ------ Fix offset columns and flag missing values ------ ###
# In this example the dataset (see bottom for dummy set) has 5 columns, the first two of which are fine but
# Column 3: varying length string representing a species name and author(s) - sometimes missing some text that has been split into col(s) 4 (and sometimes 5) - it looks like left over commas at some point split the text but are no longer in the data.
# Column 4: validity of name as a single word - should be either "Accepted", "Synonym", or "NULL" but in some cases is empty and in worst cases has text that should be in col 3.
# Column 5: Accepted name - Should contain valid binomial and author (similar to column 3). The contents of column 5 should be: the same as column 3 if column 4 = "Accepted"; different if "Synonym"; and "NULL" if "Unknown") but sometimes has fragments of text from column 3, and is sometimes empty (and cannot be inferred from other columns here).


# To fix this need to identify cells in "Status" with a valid entry, and then add a blank cell to "Status" for rows without a valid value.
# These cells were then populated with a "FLAG" flag for further scrutiny.

library(data.table)

#convert dataframe to data.table for easier manipulation
setDT(df.corrupt)

# Define existing VALID values for column "Status"
valid_status <- c("Accepted", "Synonym", "Unknown")

# Join invalid status to NameFull column
df.corrupt[!Status %in% valid_status, NameFull := paste(NameFull, Status, sep = ", ")]
# then add "flag" to cells with invalid status
df.corrupt[!Status %in% valid_status, Status := "FLAG"]

# Combine the contents of two or more cells conditional upon third cell value. ie join AcceptedName to NameFull if Status == FLAG (and Accepted Name is not NULL)
df.corrupt[Status == "FLAG" & AcceptedName != "NULL", NameFull := paste(NameFull, AcceptedName, sep = ", ")]

# Populate column ("Status") contingent on the value of other cells:
df.corrupt[Status == "Accepted", AcceptedName := NameFull]
df.corrupt[Status == "Synonym" & AcceptedName == "", AcceptedName := "FLAG"] # FLAG is to denote records where synonym is missing and cannot be inferred
df.corrupt[Status == "Unknown", AcceptedName := "NULL"]
df.corrupt[Status == "FLAG" & !AcceptedName %in% c("FLAG", "NULL"), AcceptedName := "FLAG"] # FLAG is to denote records where Accepted Name is missing

# return data table to dataframe format
setDF(df.corrupt) #return data table to dataframe format


### ------ Toy corrupt dataset ------ ###

#df.corrupt <- data.frame(
  'NameId' = c('350-8D6A','BC2-85E2','426-C0FA','615-8E09','651-8D6F','DE8-3D0F','2B6-D039','5E9-EE00','38F-75E4','B02-FBBC','B7A-821E','95A-B349','A8C-4A7B','3F6-90A1'),
  'Tribe' = c('Heliantheae','Heliantheae','Cichorieae','Cichorieae','Cichorieae','Senecioneae','Vernonieae','Tageteae','Vernonieae','Vernonieae','Millerieae','Inuleae','Astereae','Cardueae'),
  'NameFull' = c('Wedelia mexicana (Sch.Bip.) McVaugh','Wedelia modesta Baker','Youngia multiflora (Thunb.) DC.','Youngia napifera DC. ex Wight','Scorzonera mucida Rech.f. Aellen & Esfand.','Senecio hualtaranensis Petenatti Ariza & Del Vitto','Baccharoides tolypophora (Mattf.) Isawumi El-Ghazaly & B.Nord.','Bajacalia crassifolia (S.Watson) Loockerman B.L.Turner & R.K.Jansen','Vernonia westermanii Ekman & Dusen','Vernonia westiniana Less.','Oteiza scandens Panero Villaseeor & Medina','Pulicaria hesperia Maire Weiller & Wilczek','Aster chusanensis Y.S.Lim','Cheirolophus mansanetianus Stubing'),
  'Status' = c('Accepted','Accepted','Synonym','Synonym','Accepted','Accepted','Synonym','Synonym','Unknown','Unknown','Unknown','Unknown','Hyun','J.B.Peris'),
  'AcceptedName' = c('Wedelia mexicana (Sch.Bip.) McVaugh','Wedelia modesta Baker','Youngia japonica (L.) DC.','Youngia japonica (L.) DC.','','','','','NULL','NULL','','','Y.D.Kim & H.C.Shin','Olivares & J.Marten')
)
