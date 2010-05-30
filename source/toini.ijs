NB. =========================================================
NB. writing ini structures, strings and files

NB.*makeIni v Create string with INI format from 5-column boxed table
NB. form: makeIni IniTable
NB. returns: literal in format of INI file.
NB. y is: 5-column boxed table (result of parseIni)
makeIni=: 3 : 0
  ini=. _3{."1 y            NB. drop lowercase columns
  'snmes keys'=. split |: ini
  secs=. snmes </. |: keys
  ini=. (~.snmes) makeIniSection each secs
  (],LF -. {:) LF join ini  NB. join with LFs and ensure trailing LF
)

NB.*makeIniSection v Creates INI formated string of an INI section
NB. form: SectionName makeIniSection TableOfKeyValuePairs
NB. returns: literal representation of an Ini section
NB. y is: list of boxed Key;Value pairs for an Ini section
NB. x is: literal section name for all items in y
makeIniSection=: 4 : 0
  'snme sec'=. x;<y
  snme=. '[',snme,']'
  msk=. -.*#&>{."1 sec  NB. where no keyname
  sec=. <@('='&join)"1 sec
  sec=. msk }.&.> sec   NB. drop first ('=') where no keyname
  LF join snme;sec      NB. join lines with LFs
)

NB. ---------------------------------------------------------
NB.*updateIniStrings v Updates key and key value in an INI file
NB. form: [IniFileContent] updateIniStrings KeyValue;KeyName[;SectionName[;IniFileName[;CommentDelimiter]]]
NB. returns: updated 5-column boxed table.
NB. y is: 2,3,4 or 5-item boxed list OR 2,3,4 or 5-column boxed table.
NB.       0{:: or 0{"1 is key value(s) to update
NB.       1{:: or 1{"1 is key name(s) to update
NB.       2{:: or 2{"1 is section name(s) of Ini to look for key in
NB.       3{:: or 3{"1 is optional file name of Ini file.
NB.       4{:: or 4{"1 is optional comment delimiter (defaults to '#')
NB. x is: optional. Either string contents of Ini file,
NB.       or 5-column table result of parsing Ini file using parseIni
NB. Keyname lookup is case-insensitive.
NB. Sections and keys not found in current Ini are appended.
updateIniStrings=: 3 : 0
  '' updateIniStrings y
  :
  'val knme snme fnme delim'=. <"1|: 5{."1 y=. mktbl boxopen y
  val=. makeString each val
  'i ini'=. 2{.!.a: x getIniIndex }."1 y
  if. -.*#ini do. ini=. x end.  NB. x was parsed Ini
  msk=. i<#ini                  NB. existing keys to amend
  ini=. (msk#val) (<(msk#i);4) } ini
  msk=. (-.msk) *. a:~:knme     NB. new, non-empty key names to append
  ini=. ini, (boxtolower@(2&{."1) ,. ]) msk#snme,.knme,.val
)

NB. ---------------------------------------------------------
NB.*writeIniAllSections v Writes Ini string or table to an Ini file.
NB. form: IniFileContent writeIniAllSections TargetIniFileName
NB. returns: bytes written if success, else _1
NB. y is: literal filename to write to.
NB. x is: ini file in literal or boxed table form.
writeIniAllSections=: 4 : 0
  fln=. 0{:: ,boxopen y
  ini=. x
  if. (L.=1:) ini do.  NB. reformat to string
    ini=. makeIni ini
  end.
  ini fwrites fln
)

NB. ---------------------------------------------------------
NB.*writeIniStrings v Writes updated key and key value to an INI file
NB. form: [IniFileContent] writeIniStrings KeyValue;KeyName[;SectionName[;IniFileName[;CommentDelimiter]]]
NB. returns: Boolean, 1 if wrote OK, otherwise 0.
NB. y is: 2,3,4 or 5-item boxed list OR 2,3,4 or 5-column boxed table.
NB.       0{:: or 0{"1 is key value(s) to write
NB.       1{:: or 1{"1 is key name(s) to write
NB.       2{:: or 2{"1 is section name(s) of Ini to look for key in
NB.       3{:: or 3{"1 is file name of Ini file to write to.
NB.       4{:: or 4{"1 is optional comment delimiter (defaults to '#')
NB. x is: optional. Either string contents of Ini file,
NB.       or 5-column table result of parsing Ini file using parseIni
NB. Keyname lookup is case-insensitive.
NB. If x is not given, the Ini file will be read from and written to the file name in y.
NB. Sections and keys not found in current Ini are appended.
writeIniStrings=: 3 : 0
  '' writeIniStrings y
  :
  ini=. x updateIniStrings y
  'All keys must be from same file.' assert 1=# fln=. ~. 3{"1 mktbl y
  ini writeIniAllSections 0{::fln  NB. write ini boxed table to file
)
