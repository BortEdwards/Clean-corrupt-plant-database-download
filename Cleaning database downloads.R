library(stringr)
library(data.table)
library(stringr)
library(dplyr)
library(data.table)

# read in data. Using fread allows much faster loading of large datasets and can define subset of at load to reduce memory load.
df.corrupt <- fread("input.txt", 
              select = c("scientificName", "scientificNameAuthorship", "namePublishedIn", "taxonomicStatus"))


### ------ Fix offset columns and flag missing values ------ ###

# Set data.bad as data.table
setDT(df.corrupt)
# Define valid values for column "Status"
valid_status <- c("Accepted", "Synonym", "Unknown")

# Join invalid status to NameFull column
df.corrupt[!Status %in% valid_status, NameFull := paste(NameFull, Status, sep = ", ")]
# then flag invalid status
df.corrupt[!Status %in% valid_status, Status := "FLAG"]

# Combine contents of two or more cells conditional upon third cell value. ie Join AcceptedName to NameFull if Status == FLAG (and Accepted Name is not NULL)
df.corrupt[Status == "FLAG" & AcceptedName != "NULL", NameFull := paste(NameFull, AcceptedName, sep = ", ")]

# Populate column ("Status") contingent on the value of other cells:
df.corrupt[Status == "Accepted", AcceptedName := NameFull]
df.corrupt[Status == "Synonym" & AcceptedName == "", AcceptedName := "FLAG"]
df.corrupt[Status == "Unknown", AcceptedName := "NULL"]
df.corrupt[Status == "FLAG" & !AcceptedName %in% c("FLAG", "NULL"), AcceptedName := "FLAG"]

# return data table to dataframe format
setDF(df.corrupt) #return data table to dataframe format


### ------ Identify and replace corrupt text ------ ### 

# replace multiple spaces with single spaces
df.corrupt[] <- lapply(df.corrupt, function(x) (gsub('  +', ' ', x)))

# Identify potentially corrupt characters
exotic.chr.check <- setDT(df.corrupt)[, .(NameFull=unlist(str_extract_all(NameFull, "\\w+([^ -~]|\\?)\\w+"))), by = NameFull] #pull out all words with non-ASCII characters (^ to ~) and question marks https://stackoverflow.com/questions/43708888/regex-matching-utf-8-pattern-in-r
exotic.chr.check <- select(exotic.chr.check,-c(1)) # Remove unwanted first column
exotic.chr.check <- as.data.frame(unique(exotic.chr.check$NameFull)) # Reduce to unique instances of each word with undesirable character(s)

# Identify question marks in species binomials - NB this does not identify "?" in subspecies or names outside the binomial
matches <- grepl("\\?", df.corrupt$NameFull) # creates a logical vector for each row: TRUE = match, FALSE = no match
df.hits <- df.corrupt[matches,] # lists all cells with "?"

# For plant names a leading "?" in a binomial typically indicates a missing/corrupt "×" designating hybrid status. "×" is a multiplication sign (U+2715) NOT a clear-text "x"
# To substitute "these"?" for "×" in binomials:
df.corrupt$NameFull <- str_replace_all(df.corrupt$NameFull, "^\\?|(?<=^(\\?)?\\b\\w{1,100}\\b\\s)\\?", "×") # replaces "?" in binomial (first two words) with "×" as hybrid designation
df.corrupt$AcceptedName <- str_replace_all(df.corrupt$AcceptedName, "^\\?|(?<=^(\\?)?\\b\\w{1,100}\\b\\s)\\?", "×") # replaces "?" in binomial (first two words) with "×" as hybrid designation

matches <- grepl("\\?", df.corrupt$AcceptedName) # creates a logical vector for each row: TRUE = match, FALSE = no match
df.author.hits <- df.corrupt[matches,] # lists all remaining cells with "?". This may be hybrid designation outside the binomial or other corruption in the authority names
df.author.hits
write.csv(df.author.hits,'corrupt.authority.entries.txt', fileEncoding = "UTF-16") # output corrupt records


# To fix corrupted authority names you can either provide a list of each individual corrupted word/name and it's corrected version, each in quotes separated by a comma. NB special characters (eg "?" need to be escaped: "\?")
# An alternative if you are confident each corrupt character corresponds to a single "correct" character is to parse a list like above but just for each character.
list_gsub <- read.csv("conversion.list.txt", fileEncoding = "UTF-16",sep=",", header = TRUE) #this is the comma delimited look-up table - first column "corrupted" is the corrupt name (in quotes) and a corresponding entry in second column "clean" is the clean version. 

# For unknown reasons my corrupt dataset has replaced "á" with non-breaking spaces. Replace using  unicode designation for non-breakingspace.                                                      
df.clean <- df.corrupt
df.clean$NameFull <- (gsub("\u00A0", "á", df.clean$NameFull, fixed = TRUE))
df.clean$AcceptedName <- (gsub("\u00A0", "á", df.clean$AcceptedName, fixed = TRUE))

# loop through each lookup table entry and fix for column "NameFull https://stackoverflow.com/questions/24058700/find-and-replace-words
for(x in 1:nrow(list_gsub))
  df.clean$NameFull <- gsub(list_gsub[x,"corrupted"],list_gsub[x,"clean"], df.clean$NameFull)

# repeat for column "AcceptedName"
for(x in 1:nrow(list_gsub))
  df.clean$AcceptedName <- gsub(list_gsub[x,"corrupted"],list_gsub[x,"clean"], df.clean$AcceptedName)

#It is worth running the "clean" dataframe back through the above "df.corrupt <- df.clean" to check errors or overlooked characters.


### ------ Options ------ ### 


## To create lookup table in-script rather than importing a list:
list_gsub2 <- read.csv(text="
corrupted,clean
,ü
¢,ó
‰,ë
¤,ñ
‡,ç
†,å
•,ò
”,ö
„,ä
Œ,î
ƒ,â
ˆ,ê")

##This is an example of corrupt characters from a dump of the Global Compositae Database:


##BEWARE! Not all characters are a one-to-one substitution. In the example of the GCD "?" substitutes for a host of special characters:
#? = ×,ı,Á,ă,ą,ã,ě,ę,ł,ý,ř,š,ş,Š,Ś,ø,Ó,Ø,ń,ğ,Č,ć,č,ž,'U

## Also, characters that may be valid in one instance can represent a corrupt character elsewhere, eg:
#š = Ü
#‚ = é
#Š = è

##In these cases I see no good solution other than writing a case-by-case list to parse through the script. The first steps of this script can be used to identify these cases.


### ------ Toy corrupt dataset ------ ###

df.corrupt <- data.frame(
  'NameId' = c('350-8D6A','BC2-85E2','426-C0FA','615-8E09','651-8D6F','DE8-3D0F','2B6-D039','5E9-EE00','38F-75E4','B02-FBBC','B7A-821E','95A-B349','A8C-4A7B','3F6-90A1'),
  'Tribe' = c('Heliantheae','Heliantheae','Cichorieae','Cichorieae','Cichorieae','Senecioneae','Vernonieae','Tageteae','Vernonieae','Vernonieae','Millerieae','Inuleae','Astereae','Cardueae'),
  'NameFull' = c('Wedelia mexicana (Sch.Bip.) McVaugh','Wedelia modesta Baker','Youngia multiflora (Thunb.) DC.','Youngia napifera DC. ex Wight','Scorzonera mucida Rech.f. Aellen & Esfand.','Senecio hualtaranensis Petenatti Ariza & Del Vitto','Baccharoides tolypophora (Mattf.) Isawumi El-Ghazaly & B.Nord.','Bajacalia crassifolia (S.Watson) Loockerman B.L.Turner & R.K.Jansen','Vernonia westermanii Ekman & Dusen','Vernonia westiniana Less.','Oteiza scandens Panero Villaseeor & Medina','Pulicaria hesperia Maire Weiller & Wilczek','Aster chusanensis Y.S.Lim','Cheirolophus mansanetianus Stubing'),
  'Status' = c('Accepted','Accepted','Synonym','Synonym','Accepted','Accepted','Synonym','Synonym','Unknown','Unknown','Unknown','Unknown','Hyun','J.B.Peris'),
  'AcceptedName' = c('Wedelia mexicana (Sch.Bip.) McVaugh','Wedelia modesta Baker','Youngia japonica (L.) DC.','Youngia japonica (L.) DC.','','','','','NULL','NULL','','','Y.D.Kim & H.C.Shin','Olivares & J.Marten')
)
