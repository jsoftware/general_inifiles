NB. =========================================================
NB. reading ini structures, strings and files

NB.*parseIni v Parse string contents of an INI file
NB. returns: 5-column boxed table
NB.       (lc section name;lc key name;section name;key name;key string & comments)
NB. y is: string contents of an Ini file
parseIni=: 3 :0
  ini=. }.(patsection&rxmatches rxcut ]) y    NB. cut on section names & drop first
  'snmes secs'=. <"1 |: (] $~ 2 ,~ -:@#) ini  NB. reshape to 2-column table
  snmes=. (<'[]')-.~each snmes
  secs=. parseIniSection each secs
  nkys=. 1 >. #&> secs
  ini=. (nkys#snmes),. ;secs
  (([: boxtolower 2&{."1) ,. ]) ini
)

NB.*parseIniSection v Parse content of INI file section
parseIniSection=: 3 : 0
  keys=. }.<;._2 (],LF -. {:) y    NB. box each line (using LF) and drop first
  msk=. 0< #@> keys                NB. lines of non-zero length
  keys=. msk#keys
  >(_2{. [: <;._1 '==',]) &.> keys NB. box on '='
)

NB. ---------------------------------------------------------
NB.*getIniAllSections v Gets all the keynames and values in an INI file
NB. form: [IniFileContent] getIniAllSections IniFileName
NB. returns: 5-column boxed table,
NB.       0{"1 lowercase sectionnames, 1{"1 lowercase keynames,
NB.       2{"1 sectionnames, 3{"1 keynames, 4{"1 keyvalues and comments
NB. y is: literal filename of Ini file to read. (can be 0-length if x is given)
NB. x is: optional. Either string contents of Ini file, 
NB.       or 5-column boxed table result of parseIni
NB. Where both x & y are given, x is used in preference.
getIniAllSections=: 3 :0
  '' getIniAllSections y
  :
  fln=. 0{:: ,boxopen y
  ini=. x
  if. -.*#ini do.             NB. read Ini from file
    if. -.fexist fln do. '' return. end.  NB. file not found or given
    ini=. freads fln
  end.
  if. (L.=0:) ini do.         NB. parse string contents of Ini file
    ini=. parseIni ini
  else.                       NB. x was already parsed
    ini
  end.
)

NB. ---------------------------------------------------------
NB.*getIniSectionNames v Gets section names from INI file
NB. form: [IniFileContent] getIniSectionNames IniFileName
NB. returns: list of boxed section names from the INI string
NB. y is: literal filename of Ini file to read. Can be 0-length if x given.
NB. x is: optional. String contents of Ini file (LF delimited)
getIniSectionNames=: 3 : 0
  '' getIniSectionNames y
  :
  fln=. 0{:: ,boxopen y
  ini=. x
  if. -.*#ini do. NB. read Ini from file
    if. -.fexist fln do. '' return. end. NB. file not found or given
    ini=. freads fln
  end.
  (<'[]')-.~each patsection rxall ini
)

NB. ---------------------------------------------------------
NB.*getIniIndex v Gets row index of INI key in parsed INI file
NB. form: [IniFileContent] getIniIndex KeyName[;SectionName[;IniFileName]]
NB. returns: 2-item boxed list
NB.       0{:: numeric list of row indicies of INI keys in parsed INI file
NB.       1{:: parsed INI file if not given in x
NB. y is: one of: literal ; 1,2 or 3-item boxed list ; 1,2 or 3-column boxed table.
NB.       literal, 0{:: or 0{"1 is key name(s) to look up
NB.       1{:: or 1{"1 is optional section name(s) of Ini to look for key in
NB.       2{:: or 2{"1 is optional file name of Ini file to read.
NB. x is: optional. Either string contents of Ini file,
NB.       or 5-column table result of parsing Ini file using parseIni
NB. Keyname lookup is case-insensitive.
getIniIndex=: 3 :0
  '' getIniIndex y
  :
  'keyn secn fln'=. <"1|: 3{."1 mktbl boxopen y
  ini=. x
  'All keys must be from same file.' assert 1=#~.fln
  ini=. ini getIniAllSections 0{::fln
  if. -.*#ini do. '' return. end.  NB. error (reading Ini from file)
  parsed=. (L.=0:) x
  NB. look up keyn in 5-column table ini
  if. *./a: = secn do.             NB. no section names given so look up keyn ignoring section
    tbl=. 1{"1 ini
    kys=. keyn
  else.                            NB. look up keyn within section
    tbl=. 2{."1 ini
    kys=. secn,.keyn
  end.
  i=. tbl i. boxtolower kys
  i=. (#ini) (I.keyn=a:)}i         NB. empty keynames never match
  i;< parsed#ini                   NB. return parsed ini if not given in x
)

NB. ---------------------------------------------------------
NB.*getIniStrings v Gets INI key value string(s) from INI array
NB. form: [IniFileContent] getIniStrings KeyName[;SectionName[;IniFileName[;CommentDelimiter]]]
NB. returns: one or more INI key values as list of boxed strings
NB. y is: one of: literal; 1,2,3 or 4-item boxed list; 1,2,3 or 4-column boxed table.
NB.       literal, 0{:: or 0{"1 is key name(s) to look up
NB.       1{:: or 1{"1 is optional section name(s) of Ini to look for key in
NB.       2{:: or 2{"1 is optional file name of Ini file to read.
NB.       3{:: or 3{"1 is optional comment delimiter (defaults to '#')
NB. x is: optional. Either string contents of Ini file,
NB.       or 5-column table result of parsing Ini file using parseIni
NB. Keyname lookup is case-insensitive.
getIniStrings=: 3 : 0
  '' getIniStrings y
  :
  'i ini'=. 2{.!.a: x getIniIndex y
  delim=. 3{:: 4{.{.mktbl boxopen y
  if. 0=#delim do. delim=. '#' end.  NB. default comment delimiter is #
  if. -.*#ini do. ini=. x end.       NB. x was parsed Ini
  dtb@(delim&taketo) each (<i;4){ ini,a:
)

NB. ---------------------------------------------------------
NB.*getIniString v Gets single, unboxed key value string from INI array
NB. form: [IniFileContent] getIniString KeyName[;SectionName[;IniFileName[;CommentDelimiter]]]
getIniString=: 3 : 0
  '' getIniString y
  :
  0{:: x getIniStrings {.mktbl boxopen y
)

NB. ---------------------------------------------------------
NB.*getIniValues v Gets INI key value(s) from an INI array
NB. form: [IniFileContent] getIniValues KeyName[;SectionName[;IniFileName[;CommentDelimiter]]]
NB. returns: one or more INI key values as boxed list of 
NB.       interpreted strings (literal, numeric list or boxed list of literals)
NB. y is: one of: literal; 1,2,3 or 4-item boxed list; 1,2,3 or 4-column boxed table.
NB.       literal, 0{:: or 0{"1 is key name(s) to look up
NB.       1{:: or 1{"1 is optional section name(s) of Ini to look for key in
NB.       2{:: or 2{"1 is optional file name of Ini file to read.
NB.       3{:: or 3{"1 is optional comment delimiter (defaults to '#')
NB. x is: optional. Either string contents of Ini file,
NB.       or 5-column table result of parsing Ini file using parseIni
NB. Keyname lookup is case-insensitive.
getIniValues=: [: makeVals&.> getIniStrings

NB. ---------------------------------------------------------
NB.*getIniValue v Gets single, unboxed key value from INI array
NB. form: [IniFileContent] getIniValue KeyName[;SectionName[;IniFileName[;CommentDelimiter]]]
getIniValue=: [: makeVals getIniString
