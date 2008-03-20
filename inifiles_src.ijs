NB. verbs for reading from & writing to INI files

NB.*getIniAllSections v Gets all the keynames and values in an INI file
NB. returns 5-column boxed table,
NB.      0{"1 lowercase sectionnames, 1{"1 lowercase keynames,
NB.      2{"1 sectionnames, 3{"1 keynames, 4{"1 keyvalues and comments
NB. y is literal filename of Ini file to read. (Empty if x is given)
NB. x is optional. Either string contents of Ini file,
NB.      or 5-column boxed table result of parseIni
getIniAllSections=: 3 :0
  '' getIniAllSections y
  :
  fln=. 0{:: ,boxopen y
  ini=. x
  if. -.*#ini do. NB. read Ini from file
    if. -.fexist fln do. '' return. end. NB. file not found or given
    ini=. freads fln
  end.
  if. (L.=0:) ini do. NB. parse string contents of Ini file
    ini=. parseIni ini
  else. NB. x was already parsed
    ini
  end.
)

NB.*getIniSectionNames v Gets section names from INI file
NB. returns list of boxed section names from the INI string
NB. y is literal filename of Ini file to read. Empty if x given.
NB. x is optional. String contents of Ini file (LF delimited)
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

NB.*getIniIndex v returns row index of INI key in parsed INI file
NB. returns 2-item boxed list
NB.     0{ contains numeric list of row indicies of INI keys in parsed INI file
NB.     1{ contains parsed INI file if not given in x
NB. y is literal, or 1,2 or 3-item boxed list.
NB.      literal or 0{:: y is key name to look up
NB.      1{:: y is optional section name of Ini to look for key in
NB.      2{:: is optional file name of Ini file to read.
NB. x is optional. Either string contents of Ini file,
NB.      or 5-column table result of parsing Ini file using parseIni
NB. keyname lookup is case-insensitive
getIniIndex=: 3 :0
  '' getIniIndex y
  :
  'keyn secn fln'=. <"1|: 3{."1 mktbl boxopen y
  ini=. x
  'All keys must be from same file.' assert 1=#~.fln
  ini=. ini getIniAllSections 0{::fln
  if. -.*#ini do. '' return. end. NB. error (reading Ini from file)
  parsed=. (L.=0:) x
  NB. look up keyn in 5-column table ini
  if. *./a: = secn do. NB. no section names given so look up keyn ignoring section
    tbl=. 1{"1 ini
    kys=. keyn
  else. NB. look up keyn within section
    tbl=. 2{."1 ini
    kys=. secn,.keyn
  end.
  i=. tbl i. boxtolower kys
  i;< parsed#ini  NB. return parsed ini if not given in x
)

NB.*getIniStrings v returns INI key value string from INI array
NB. returns INI key values as list of boxed strings
NB. y is literal, or 1,2,3 or 4-item boxed list.
NB.      literal or 0{:: y is key name to look up
NB.      1{:: y is optional section name of Ini to look for key in
NB.      2{:: is optional file name of Ini file to read.
NB.      3{:: is optional comment delimiter (defaults to '#')
NB. x is optional. Either string contents of Ini file,
NB.      or 5-column table result of parsing Ini file using parseIni
NB. keyname lookup is case-insensitive
getIniStrings=: 3 : 0
  '' getIniStrings y
  :
  'i ini'=. 2{.!.a: x getIniIndex y
  delim=. 3{:: 4{.{.mktbl boxopen y
  if. 0=#delim do. delim=. '#' end. NB. default comment delimiter is #
  if. -.*#ini do. ini=. x end. NB. x was parsed Ini
  dtb@(delim&taketo) each (<i;4){ ini,a:
)

getIniString=: 3 : 0
  '' getIniString y
  :
  0{:: x getIniStrings {.mktbl boxopen y
)

NB.*getIniValues v returns INI key value(s) from an INI array
NB. returns INI key string as boxed list of values
NB.      (numeric list or boxed list of literals)
NB. y is literal, or 1,2,3 or 4-item boxed list.
NB.      literal or 0{:: y is key name to look up
NB.      1{:: y is optional section name of Ini to look for key in
NB.      2{:: is optional file name of Ini file to read.
NB.      3{:: is optional comment delimiter (defaults to '#')
NB. x is optional. Either string contents of Ini file,
NB.      or 5-column table result of parsing Ini file using parseIni
NB. keyname lookup is case-insensitive
getIniValues=: [: makeVals&.> getIniStrings

getIniValue=: [: makeVals getIniString

NB.*makeVals v Interprets a string as numeric or list of boxed literals
NB. if y can be interpreted as all numeric, returns numeric list
NB. elseif y contains spaces or commas, returns list of boxed literals
NB. else returns original string
NB. y is a string
NB. x is optional numeric value used to signify non-numeric.
NB.   Choose value for x that will not be valid numeric in your data.
makeVals=: 3 : 0
  _999999 makeVals y
  :
  err=. x
  if. L.y do. y return. end. NB. already boxed
  val=. ', ' charsub y  NB. values delimited by commas and/or spaces
  if. -.+./err= nums=. err&". val do. val=. nums end.
  if. ' ' e. val do. val=. <;._1 ' ',deb val end.
  val
)

NB.*updateIniStrings v Writes key and key value to an INI file
NB. returns updated 5-column boxed table.
NB. y is 2,3,4 or 5-item boxed list.
NB.      0{:: y is key value to write
NB.      1{:: y is key name to write
NB.      2{:: y is section name of Ini to look for key in
NB.      3{:: y is optional file name of Ini file.
NB.      4{:: y is optional comment delimiter (defaults to '#')
NB. x is optional. Either string contents of Ini file,
NB.      or 5-column table result of parsing Ini file using parseIni
NB. keyname lookup is case-insensitive
updateIniStrings=: 3 : 0
  '' updateIniStrings y
  :
  'val knme snme fnme delim'=. <"1|: 5{."1 y=. mktbl boxopen y
  val=. makeString each val
  'i ini'=. 2{.!.a: x getIniIndex }."1 y
  if. -.*#ini do. ini=. x end. NB. x was parsed Ini
  msk=. i<#ini NB. existing keys to amend
  ini=. (msk#val) (<(msk#i);4) } ini
  msk=. -.msk NB. new key names to append
  ini=. ini, (boxtolower@(2&{."1) ,. ]) msk#snme,.knme,.val
)

NB.*writeIniAllSections v Writes Ini string or table as Ini file.
NB. returns bytes written if success, else _1
NB. y is literal filename to write to.
NB. x is ini file in literal or boxed table form.
writeIniAllSections=: 4 : 0
  fln=. 0{:: ,boxopen y
  ini=. x
  if. (L.=1:) ini do. NB. reformat to string
    ini=. makeIni ini
  end.
  ini fwrites fln
)

NB.*writeIniStrings v Writes key and key value to an INI file
NB. returns Boolean, 1 if wrote OK, otherwise 0.
NB. y is 2,3,4 or 5-item boxed list.
NB.      0{:: y is key value to write
NB.      1{:: y is key name to write
NB.      2{:: y is section name of Ini to look for key in
NB.      3{:: y is file name of Ini file to (read and) write.
NB.      4{:: y is optional comment delimiter (defaults to '#')
NB. x is optional. Either string contents of Ini file,
NB.      or 5-column table result of parsing Ini file using parseIni
NB. keyname lookup is case-insensitive
writeIniStrings=: 3 : 0
  '' writeIniStrings y
  :
  ini=. x updateIniStrings y
  'All keys must be from same file.' assert 1=# fln=. ~. 3{"1 mktbl y
  ini writeIniAllSections 0{::fln NB. write ini boxed table to file
)

NB. =========================================================
NB. Verbs local to rgsini locale

boxtolower=: 13 : '($y) $ <;._2 tolower ; y ,each {:a.'

mktbl=: ,:^:(0:>.2:-#@:$)

NB.*join v Unbox and delimit a list of boxed items y with x
NB. from forum post
NB. http://www.jsoftware.com/pipermail/programming/2007-June/007077.html
NB. eg. '","' join 'item1';'item2'
NB. eg. LF join 'item1';'item2'
NB. eg. 99 join <&> i.8
join=: ' '&$. : (4 : '(;@(#^:_1!.(<x))~  1 0$~_1 2 p.#) y')  NB. ignore $.

NB.*makeIni v Create string with INI format from 5-column boxed table
NB. returns literal in format of INI file.
NB. y is 5-column boxed table (result of parseIni)
makeIni=: 3 : 0
  ini=. _3{."1 y NB. drop lowercase columns
  'snmes keys'=. split |: ini
  secs=. snmes </. |: keys
  ini=. (~.snmes) makeIniSection each secs
  (],LF -. {:) LF join ini NB. join with LFs and ensure trailing LF
)

NB.*makeIniSection v creates string with INI format
NB. returns literal representation of an Ini section
NB. y is boxed list of boxed table representations of Ini sections
NB. x is boxed list of section names corresponding to items of y
makeIniSection=: 4 : 0
  'snme sec'=. x;<y
  snme=. '[',snme,']'
  msk=. -.*#&>{."1 sec NB. where no keyname
  sec=. <@('='&join)"1 sec
  sec=. msk }.&.> sec  NB. drop first ('=') where no keyname
  LF join snme;sec  NB. join lines with LFs
)

NB.*makeString v Creates space-delimited string from numeric list, list of boxed literals or numbers
NB. returns space-delimited string or unchanged y, if y is literal
NB. y is string, numeric list, list of boxed literals and/or numbers
makeString=: [: ' '&join 8!:0

NB.*parseIni v Parse string contents of an INI file
NB. returns 5-column boxed table
NB. (lc section name;lc key name;section name;key name;key string & comments)
NB. y is string contents of an Ini file
parseIni=: 3 :0
  ini=. }.(patsection&rxmatches rxcut ]) y NB. cut on section names & drop first
  'snmes secs'=. <"1 |: (] $~ 2 ,~ -:@#) ini NB. reshape to 2-column table
  snmes=. (<'[]')-.~each snmes
  secs=. parseIniSection each secs
  nkys=. 1 >. #&> secs
  ini=. (nkys#snmes),. ;secs
  (([: boxtolower 2&{."1) ,. ]) ini
)

NB.*parseIniSection v parse content of INI file section
parseIniSection=: 3 : 0
  keys=. }.<;._2 (],LF -. {:) y NB. box each line (using LF) and drop first
  msk=. 0< #@> keys NB. lines of non-zero length
  keys=. msk#keys
  >(_2{. [: <;._1 '==',]) &.> keys NB. box on '='
)

patsection=: rxcomp '\[[[:alnum:]]+\]' NB. compile pattern
NB. patsection rxmatches inistr
NB. rxfree patsection  NB. frees compiled pattern resources


NB. =========================================================
NB. Declarations in z locale

getIniAllSections_z_=: getIniAllSections_rgsini_
getIniString_z_=: getIniString_rgsini_
getIniStrings_z_=: getIniStrings_rgsini_
getIniValue_z_=: getIniValue_rgsini_
getIniValues_z_=: getIniValues_rgsini_
getIniIndex_z_=: getIniIndex_rgsini_
updateIniStrings_z_=: updateIniStrings_rgsini_
writeIniAllSections_z_=: writeIniAllSections_rgsini_
writeIniStrings_z_=: writeIniStrings_rgsini_
makeVals_z_=: makeVals_rgsini_
