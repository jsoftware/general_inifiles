NB. =========================================================
NB. utils for rgsini

boxtolower=: 13 : '($y) $ <;._2 tolower ; y ,each {:a.'

NB.*join v Unbox and delimit a list of boxed items y with x
NB. form: Delimiter join BoxedList
NB. eg: '","' join 'item1';'item2'
NB. eg: LF join 'item1';'item2'
NB. eg: 99 join <"0 i.8
NB. based on forum post
NB. http://www.jsoftware.com/pipermail/programming/2007-June/007077.html
NB. see also joinstring in strings script
join=: ' '&$: : (4 : '(;@(#^:_1!.(<x))~  1 0$~0 >. _1 2 p.#) y')

mktbl=: ,:^:(0:>.2:-#@:$)

NB.*makeString v Creates space-delimited string from numeric list, list of boxed literals or numbers
NB. returns: space-delimited string or unchanged y, if y is literal
NB. y is: string, numeric list, list of boxed literals and/or numbers
makeString=: ' ' join 8!:0

NB. ---------------------------------------------------------
NB.*makeVals v Interprets a string as numeric or list of boxed literals
NB. form: [ErrorValue] makeVals String
NB. returns: if y can be interpreted as all numeric, returns numeric list
NB.       elseif y contains spaces or commas, returns list of boxed literals
NB.       else returns original string
NB. y is: a string to interpret
NB. x is: optional numeric value used to signify non-numeric.
NB.       Choose value for x that will not be valid numeric in your data.
makeVals=: 3 : 0
  _999999 makeVals y
  :
  err=. x
  if. L.y do. y return. end.  NB. already boxed
  val=. ', ' charsub y        NB. values delimited by commas and/or spaces
  if. -.+./err= nums=. err&". val do. val=. nums end.
  if. ' ' e. val do. val=. <;._1 ' ',deb val end.
  val
)

patsection=: rxcomp '^\[[[:alnum:]]+\]'  NB. compile pattern
NB. patsection rxmatches inistr
NB. rxfree patsection  NB. frees compiled pattern resources
