_PALETTECOLOR 1, _RGB32(255, 255, 255)
COLOR 0, 1
RANDOMIZE TIMER

TSAO = 1

TSAO = 0

gtm = 1.25: gtm = 1
DIM upp$(1000)
DIM sector$(100), pas$(100)
DIM val$(13)
DIM text$(100)
DIM lox(6)

DIM world(20000), sector(20000), result$(2000)

val$(1) = "NO"
val$(2) = "Negligible amounts (-7)"
val$(3) = "Tiny amounts (-5)"
val$(4) = "Very small amounts (-4)"
val$(5) = "Small amounts (-3)"
val$(6) = "Mediocre amounts (-2)"
val$(7) = "Average amounts"
val$(8) = "Above average amounts (+1)"
val$(9) = "Supple amounts (+3)"
val$(10) = "Big amounts (+4)"
val$(11) = "Huge amounts (+6)"
val$(12) = "Enormous amounts (+8)"
val$(13) = "Unlimited amounts (+9)"
DIM lat(20, 20)
DIM loca(100, 100)
DIM au(19), orf(19)
DIM mass(45, 6), lumi(45, 6)
DIM nhz(10, 4), hz(10, 4)
' LOCATE 24, 1

citx = 7
DIM city$(citx), city(citx), citynum(citx)
c = 0: city$(c) = "giant Metroplexes with over a Billion Inhabitants": city(c) = 1E+9
c = 1: city$(c) = "huge Metroplexes with hundreds of Millions of Inhabitants": city(c) = 1E+8
c = 2: city$(c) = "Metroplexes with tens of Millions of Inhabitants": city(c) = 1E+7
c = 3: city$(c) = "huge Cities with Millions of Inhabitants": city(c) = 1E+6
c = 4: city$(c) = "large Cities with 100K+ Inhabitants": city(c) = 1E+5
c = 5: city$(c) = "Cities with 10K+ Inhabitants": city(c) = 1E+4
c = 6: city$(c) = "small Cities with thousands of Inhabitants": city(c) = 1E+3
c = 7: city$(c) = "Outposts with hundreds of Inhabitants": city(c) = 1E+2

carx = 10
DIM cargoship(carx), cargo(carx), trader$(carx)
s = 0: trader$(s) = "Fornorb Class Heavy Merchant (100kt)": cargo(s) = 80000: pasngr(s) = 36
s = 1: trader$(s) = "Rigel Class Bulk Carrier (50kt)": cargo(s) = 25000: pasngr(s) = 0
s = 2: trader$(s) = "Maru Class bulk freighter (20kt)": cargo(2) = 13042: pasngr(s) = 0
s = 3: trader$(s) = "Hercules Class cargo transport (5000t)": cargo(3) = 2827: pasngr(s) = 0
s = 4: trader$(s) = "Tukera freighter (3000t)": cargo(s) = 1312.5: pasngr(s) = 0
s = 5: trader$(s) = "Imperialines TI Transport (2000t)": cargo(s) = 605.5: pasngr(s) = 0
s = 6: trader$(s) = "Tukera Long Liner (1000t)": cargo(s) = 130: psngr(s) = 36
s = 7: trader$(s) = "Bloodwell Class Merchant (1000t)": cargo(s) = 301.5: pasngr(s) = 8
s = 8: trader$(s) = "Ad Astra Class Liner (600t)": cargo(s) = 129: pasngr(s) = 21
s = 9: trader$(s) = "Far Trader (400t)": cargo(s) = 210: pasngr(s) = 17
s = 10: trader$(s) = "Free Trader (200t)": cargo(s) = 82: pasngr(s) = 26

CLS
DATA "0-TEST.DAT",""
DATA "TSAO.DAT",""
DATA "Aldebaran.DAT",""
DATA "Alpha Crucis.DAT",""
DATA "Antares.DAT","Antares"
DATA "Canopus.DAT",""
DATA "Core.DAT","Capital"
DATA "Corridor.DAT","Kaasu"
DATA "Crucis Margin.DAT",""
DATA "Dagudashaag.DAT","Medurma"
DATA "Dark Nebula.DAT","Kuzu"
DATA "Delphi.DAT","Anaxias"
DATA "Deneb.DAT","Vincennes"
DATA "Diaspora.DAT","Libert"
DATA "Empty Quarter.DAT","Nulinad"
DATA "Far Frontiers.DAT","Alzenei"
DATA "Fornast.DAT","Shumduur"
DATA "Gateway.DAT","Gateway"
DATA "Glimmerdrift.DAT","Lamente"
DATA "Gushemege.DAT","Usdiki"
DATA "Gvurrdon.DAT",""
DATA "Hinterworld.DAT",""
DATA "Ilelish.DAT","Dlan"
DATA "Langere.DAT",""
DATA "Ley.DAT","Dukh"
DATA "Lishun.DAT","Tephany"
DATA "Massilia.DAT","Keum"
DATA "Magyar.DAT",""
DATA "Newworld.DAT",""
DATA "Old Expanses.DAT","Terzin"
DATA "Reavers Deep.DAT","Marlheim"
DATA "Reft Sector.DAT",""
DATA "Riftspan Reaches.DAT",""
DATA "Interstellar Wars.DAT","Terra"
DATA "Solomani Rim.DAT","Terra"
DATA "Spica.DAT",""
DATA "Spinward Marches.DAT","Rhylanor"
'DATA "Stars End.DAT",""
DATA "Trojan Reach.DAT","Empire"
DATA "Verge.DAT","Tripolis"
DATA "Vland.DAT","Vland"
DATA "Ustral Quadrant.DAT",""
DATA "Zarushagar.DAT","Liasdi"


DATA "XXX","XXX"
GOTO 1

50000 'SEARCH ENGINE
code$ = siz$: GOSUB 3000: siz = code: IF siz$ = "S" THEN siz = .4
code$ = atm$: GOSUB 3000: atm = code
code$ = hyd$: GOSUB 3000: hyd = code
code$ = pop$: GOSUB 3000: pop = code
code$ = gov$: GOSUB 3000: gov = code
code$ = law$: GOSUB 3000: law = code
code$ = tec$: GOSUB 3000: tec = code
IF stp$ <> stx$ AND stx$ <> "" THEN GOTO 50010
IF siz < sizLL OR siz > sizUL THEN GOTO 50010
IF atm < atmLL OR atm > atmUL THEN GOTO 50010
IF hyd < hydLL OR hyd > hydUL THEN GOTO 50010
IF pop < popLL OR pop > popUL THEN GOTO 50010
IF law < lawLL OR law > lawUL THEN GOTO 50010
IF gov < govLL OR gov > govUL THEN GOTO 50010
IF tec < tecLL OR tec > tecUL THEN GOTO 50010
IF TSAO = 1 THEN name$ = LEFT$(upp$, 24)
xc = xc + 1: PRINT USING "#####, "; xc;: PRINT name$; LEFT$(sector$, LEN(sector$) - 4)
world(xc) = i: sector(xc) = q: result$(xc) = LEFT$(sector$, LEN(sector$) - 4) + " " + upp$: XXC = xc
IF xc / 20 = INT(xc / 20) THEN INPUT "    ", choice: IF choice > 0 GOTO 159 ELSE CLS: PRINT: PRINT




50010 RETURN

50100 'SEARCH Parameters
sizLL = 7: sizUL = 8
atmLL = 5: atmUL = 9
hydLL = 3: hydUL = 5
popLL = 3: popUL = 10
govLL = 0: govUL = 3
lawLL = 0: lawUL = 5
tecLL = 0: tecUL = 5

PRINT " If you hit ENTER without a number on both values, the system will set the full  available range, e.g. 0-10 for planetary size, or A to X for starports."
PRINT
INPUT " Starport Type ", stx$:
stx$ = UCASE$(stx$)
PRINT

INPUT " Minimal Size ", sizLL
INPUT " Maximal Size ", sizUL: IF sizUL < sizLL THEN sizUL = sizLL
IF sizUL = 0 AND sizLL = 0 THEN sizLL = 0: sizUL = 10
PRINT


INPUT " Minimal Atmosphere ", atmLL
INPUT " Maximal Atmoshere ", atmUL: IF atmUL < atmLL THEN atmUL = atmLL
IF atmUL = 0 AND atmLL = 0 THEN atmLL = 0: atmUL = 14


PRINT
INPUT " Minimal Hydrographics ", hydLL
INPUT " Maximal Hydrographics ", hydUL: IF hydUL < hydLL THEN hydUL = hydLL
IF hydUL = 0 AND hydLL = 0 THEN hydLL = 0: hydUL = 10


PRINT
INPUT " Minimal Population ", popLL
INPUT " Maximal Population ", popUL: IF popUL < popLL THEN popUL = popLL
IF popUL = 0 AND popLL = 0 THEN popLL = 0: popUL = 10

PRINT
INPUT " Minimal Government ", govLL
INPUT " Maximal Government ", govUL: IF govUL < govLL THEN govUL = govLL
IF govUL = 0 AND govLL = 0 THEN govLL = 0: govUL = 14

PRINT
INPUT " Minimal Law Level ", lawLL
INPUT " Maximal Law Level ", lawUL: IF lawUL < lawLL THEN lawUL = lawLL
IF lawUL = 0 AND lawLL = 0 THEN lawLL = 0: lawUL = 25

PRINT
INPUT " Minimal Tech Level ", tecLL
INPUT " Maximal Tech Level ", tecUL: IF tecUL < tecLL THEN tecUL = tecLL
IF tecUL = 0 AND tecLL = 0 THEN tecLL = 0: tecUL = 20

RETURN

RETURN
1
search = 0
PRINT " Enter the Worlds name (partial names are OK, capitalization is not needed)": PRINT
PRINT " By hitting just ENTER you will access the Search Modus.": PRINT


IF search = 0 THEN INPUT " ", world$: IF world$ = "" THEN search = 1: GOSUB 50100
'IF world$ = "SEARCH" OR world$ = "search" THEN search = 1: GOSUB 50100
secend = 100:

FOR i = 0 TO secend
    READ sector$(i)
    READ pas$(i)
    IF sector$(i) = "XXX" THEN xx = i - 1: i = 100
NEXT: PRINT
IF TSAO = 1 THEN xx = 1
xc = 0
FOR q = 0 TO xx

    sector$ = sector$(q): ix = i: 'PRINT sector$
    pas$ = pas$(q)
    OPEN sector$ FOR INPUT AS #1
    INPUT #1, systems
    FOR i = 1 TO systems: INPUT #1, upp$(i): upp$ = upp$(i): GOSUB 3900 'reading UPP
        IF search = 1 THEN GOSUB 50000: GOTO 154

        'PRINT i; upp$(i)
        'PRINT MID$(upp$(i), 25, 4)
        'GOSUB 1000
        'name$ = MID$(upp$, 6, 31)
        IF TSAO = 1 THEN name$ = LEFT$(upp$(i), 24)
        tl = VAL(MID$(upp$(i), 35, 1))
        'IF tl > 6 OR tl = 0 THEN GOTO 154


        IF LCASE$(world$) <> LCASE$(LEFT$(name$, LEN(world$))) THEN GOTO 154
        xc = xc + 1: PRINT USING "########, "; xc;: PRINT name$; LEFT$(sector$, LEN(sector$) - 4)
        world(xc) = i: sector(xc) = q
        IF xc / 20 = INT(xc / 20) THEN GOSUB 1000: CLS: PRINT: PRINT

        'PRINT world(xc); sector(xc)
        REM world = i: sector = q: q = xx: i = systems
        154
    NEXT
    CLOSE #1
NEXT
PRINT

IF xc = 1 THEN choice = 1
IF choice = 0 THEN INPUT " Enter the number in front of the world ", choice
IF choice < 1 THEN RUN
159
sector = sector(choice)
sector$ = sector$(sector): ix = sector


pas$ = pas$(sector)
CLOSE #1
OPEN sector$ FOR INPUT AS #1
INPUT #1, systems
FOR i = 1 TO systems: INPUT #1, upp$(i):
    ' PRINT upp$(i);
NEXT
CLOSE #1

upp$ = upp$(world(choice))


name$ = MID$(upp$, 6, 31)
IF TSAO = 1 THEN name$ = LEFT$(upp$, 25)
' name$: GOSUB 1000
xxx = 1
'wq = .25
PRINT
PRINT " DATA files for the "; LEFT$(sector$(ix), LEN(sector$(ix)) - 4); " have been loaded."



'name$ = MID$(upp$(world(choice)), 6, 7)
'IF TSAO = 1 THEN name$ = LEFT$(upp$(world(choice)), 7)



funf = 6
ffx = (funf * 2 + 1) ^ 2
33 FOR i = 1 TO systems: upp$ = upp$(i)

    IF TSAO = 1 GOTO 34

    lo1 = VAL(LEFT$(upp$, 2))
    lo2 = VAL(MID$(upp$, 3, 2))
    IF LCASE$(name$) = LCASE$(MID$(upp$, 6, LEN(name$))) THEN lox = VAL(LEFT$(upp$, 4)): horiz = lo1: verti = lo2: i = systems: GOSUB 17000
    GOTO 35


    34
    lo1 = VAL(MID$(upp$, 25, 2))
    lo2 = VAL(MID$(upp$, 27, 2))
    IF LCASE$(name$) = LCASE$(LEFT$(upp$, LEN(name$))) THEN lox = VAL(MID$(upp$, 25, 4)): horiz = lo1: verti = lo2: i = systems: GOSUB 17000

35 NEXT
IF horiz = 0 AND LEN(name$) > 0 THEN name$ = LEFT$(name$, LEN(name$) - 1): GOTO 33
IF horiz = 0 THEN RUN
hmin = horiz - funf
hplu = horiz + funf
vmin = verti - funf
vplu = verti + funf


FOR i = 1 TO systems
    upp$ = upp$(i)
    lo1 = VAL(LEFT$(upp$, 2))
    lo2 = VAL(MID$(upp$, 3, 2))
    IF TSAO = 1 THEN lo1 = VAL(MID$(upp$, 25, 2)): lo2 = VAL(MID$(upp$, 27, 2))


    dx = 0
    IF lo1 >= hmin AND lo1 < hplu THEN dx = 1
    IF lo2 >= vmin AND lo2 < vplu THEN dx = dx + 1
    IF dx = 2 THEN sx = sx + 1: upp$(sx) = upp$
NEXT: systems = sx


888 RESTORE 888
DATA 24,21,21,18,18,15,15,12,12,9
DATA 16,14,14,12,12,10,10,8,8,6
DATA 8,7,7,6,6,5,5,4,4,3
DATA 0,0,0,0,0,0,0,0,0,0
DATA -8,-7,-7,-6,-6,-5,-5,-4,-4,-3
DATA -16,-14,-14,-12,-12,-10,-10,-8,-8,-6
DATA -24,-21,-21,-18,-18,-15,-15,-12,-12,-9
DATA -32,-28,-28,-24,-24,-20,-20,-16,-16,-12
DATA -40,-35,-35,-30,-30,-25,-25,-20,-20,-15
DATA -48,-42,-42,-36,-36,-30,-30,-24,-24,-18
DATA -56,-49,-49,-42,-42,-35,-35,-28,-28,-21
FOR i = 1 TO 11: FOR j = 1 TO 10: READ lat(i, j): NEXT: NEXT
GOSUB 4000
'print UCASE$(LEFT$(upp$, 4)); " ";

FOR i = 1 TO 6
    FOR j = 0 TO 45
        IF j < 5 THEN j0 = 0: j1 = 5: GOTO 9
        IF j < 10 THEN j0 = 5: j1 = 10: GOTO 9
        IF j < 15 THEN j0 = 10: j1 = 15: GOTO 9
        IF j < 20 THEN j0 = 15: j1 = 20: GOTO 9
        IF j < 25 THEN j0 = 20: j1 = 25: GOTO 9
        IF j < 30 THEN j0 = 25: j1 = 30: GOTO 9
        IF j < 35 THEN j0 = 30: j1 = 35: GOTO 9
        IF j < 40 THEN j0 = 35: j1 = 40: GOTO 9
        IF j < 45 THEN j0 = 40: j1 = 45
        9 '
        'PRINT USING "##"; i; : PRINT USING "## "; j;
        m1 = mass(j1, i): m0 = mass(j0, i)
        l1 = lumi(j1, i): l0 = lumi(j0, i)
        mx = (m1 - m0) / 5 * (j - j0): lx = (l1 - l0) / 5 * (j - j0)
        IF mass(j, i) = 0 THEN mass(j, i) = mass(j0, i) + mx
        IF lumi(j, i) = 0 THEN lumi(j, i) = lumi(j0, i) + lx
NEXT: NEXT
GOSUB 5000

FOR i = 1 TO systems
    'PRINT UCASE$(LEFT$(upp$(i), 4)); " ";
    lo1 = VAL(MID$(upp$(i), 1, 2)): lo2 = VAL(MID$(upp$(i), 3, 2))
    IF TSAO = 1 THEN lo1 = VAL(MID$(upp$(i), 25, 2)): lo2 = VAL(MID$(upp$(i), 27, 2))

    10 loca(lo1, lo2) = i
NEXT



FOR i = 1 TO ffx
    IF TSAO = 0 AND LCASE$(MID$(upp$(i), 6, LEN(name$))) = LCASE$(name$) THEN qq = i: i = ffx
    IF TSAO = 1 AND LCASE$(name$) = LCASE$(LEFT$(upp$(i), LEN(name$))) THEN qq = i: i = ffx: 'GOSUB 1000

NEXT


FOR j = 1 TO 32
    FOR z = 1 TO 40
        IF loca(j, z) = qq THEN horiz = j: vertik = z
    NEXT
NEXT
PRINT
PRINT " "; LEFT$(upp$(qq), 38)
PRINT zon$
'GOTO 3050
PRINT " Do you want to change randomized values? Y/N"
GOSUB 1000

IF a$ = "y" OR a$ = "Y" THEN GOTO 3051
GOTO 3050
3051
PRINT
PRINT "Example: Earth has a base temperature of 14ø": PRINT
INPUT "Base temperature in Celsius? ", bsy: PRINT
PRINT "Example: Earth has an axial tilt of 27ø": PRINT
INPUT "Axial tilt? ", axtilt: PRINT
PRINT "Example: Earth's days have a length of 24 hours": PRINT
INPUT "Length of day in hours? ", rota: PRINT
PRINT "Example: Earth's gravity is 1.0":: PRINT
INPUT "Gravity? ", grav


3050
mod1 = horiz - 5: mod2 = vertik - 5
IF INT(horiz / 2) <> horiz / 2 THEN mod1 = horiz - 6

IF mod1 < 0 THEN mod1 = 0
IF mod2 < 0 THEN mod2 = 0



'****************************
'                           *
GOSUB 3100: ' START WORKOUT *
'                           *
'****************************
' GOTO 3360

100 ' Subsector Description

tvz = 0: tpz = 0: CLS




LOCATE 1, 1: PRINT CHR$(201);: FOR w = 1 TO 34: PRINT CHR$(205);: NEXT
PRINT CHR$(187): FOR u = 2 TO 22: LOCATE u, 1: PRINT CHR$(186); TAB(36); CHR$(186);: NEXT
LOCATE 23, 1: PRINT CHR$(200);: FOR w = 1 TO 34: PRINT CHR$(205);: NEXT: PRINT CHR$(188);
tfm = 0
FOR i = 1 TO 8: FOR j = 1 TO 10
        bb = 1: IF INT(i / 2) = i / 2 THEN bb = 0
        sa = loca(i + mod1, j + mod2)
        hz = j * 2 + bb + 1: ve = i * 4
        IF sa <= 0 THEN LOCATE hz, ve: PRINT " . ": GOTO 77
        upp$ = upp$(sa): rtx$ = " ": loy = VAL(LEFT$(upp$, 4)): GOSUB 15000
        l1 = VAL(MID$(upp$, 1, 2)): l2 = VAL(MID$(upp$, 3, 2))
        IF a$ = "t" THEN othw = 1: GOSUB 2001: GOSUB 9000: tbase = bsp ELSE GOSUB 3900
        IF a$ = "b" OR a$ = "r" THEN othw = 1: GOSUB 2001: GOSUB 9100 ELSE GOSUB 3900


        LOCATE hz - 1, ve: PRINT LEFT$(namx$, 3)
        LOCATE hz, ve
        IF a$ = "P" OR a$ = "p" THEN PRINT pol$; bas$: GOTO 77
        IF a$ = "s" THEN PRINT sox$: GOTO 77
        IF a$ = "n" THEN PRINT MID$(namx$, 4, 3): GOTO 77
        IF a$ = "t" THEN PRINT USING "###"; tbase / 100: GOTO 77
        IF bas$ = " " THEN bas$ = "-"
        IF a$ = "r" THEN GOSUB 20000: vl = btn * 2: GOSUB 16000: PRINT stp$; wwx$; vl$: GOTO 77
        IF a$ = "b" THEN PRINT USING "#.#"; btn: GOTO 77
        PRINT stp$; pop$; tec$

        77
        IF a$ = "r" THEN tvz = tvz + tvx: tpz = tpz + tvp: tvx = 0: tvp = 0
    NEXT
NEXT

' Planeten darstellung


upp$ = LEFT$(upp$(qq), 76):
ups$ = LEFT$(upp$, 60)
othw = 1: GOSUB 3100
traffic = traffic + tfm: facility = facility + tfm: tfm = 0

sg = 1: GOSUB 14000

LOCATE 1 + sg, 39: PRINT LEFT$(ups$, 42)
wt0 = btn
LOCATE 3 + sg, 39: PRINT "Port : "; stp$;
IF traffic >= 0 THEN PRINT "-";: vl = traffic: GOSUB 16000: PRINT vl$; "-";: vl = facility: GOSUB 16000: PRINT vl$;
PRINT " ";
IF tv0 > 0 THEN tvz = tv0
IF tp0 > 0 THEN tpz = tp0
IF tvz > 0 THEN value = tvz: GOSUB 13001
IF tpz > 0 THEN value = tpz / 365: PRINT " t/y ";: GOSUB 13001: PRINT " P/d"
PRINT
LOCATE 5 + sg, 39: PRINT "Route:";


IF minor > 0 THEN PRINT USING "### Minor,"; minor;
IF feeder > 0 THEN PRINT USING "### Feeder,"; feeder;
IF major > 0 THEN PRINT USING "### Major,"; major;
IF minor > 0 THEN PRINT USING "##%"; (minor + feeder + major) / systems * 100;
PRINT
wt0 = btn

LOCATE 7 + sg, 39:
IF siz = 0 THEN PRINT "Size : Asteroid Belt"
IF siz > 0 THEN PRINT USING "Size :######, km, #.## g"; siz * 1609; grav

LOCATE 9 + sg, 39: PRINT "Atm. : "; atmx$;: IF press > 0 THEN PRINT USING "###.## bar####.#ø"; press; bse ELSE PRINT
LOCATE 11 + sg, 39:
IF hyd > 0 THEN PRINT USING "Hyd. :###, %"; hyz * 10
IF hyd = 0 THEN PRINT "Hyd. : No free water"

LOCATE 13 + sg, 39: PRINT "Pop. : ";: value = 10 ^ pop * mul: GOSUB 13001
LOCATE 15 + sg, 39: PRINT "Gov. : "; LEFT$(gov$, 32)
LOCATE 17 + sg, 39: PRINT USING "Law #: "; cr;: PRINT LEFT$(law$, 32)



LOCATE 19 + sg, 39: PRINT USING "Tech :## "; tec;
PRINT USING "S##"; SciTL * gtm;
PRINT USING "M##"; MatTL * gtm;
PRINT USING "E##"; EneTL * gtm;
PRINT USING "I##"; COMTL * gtm;
PRINT USING "T##"; TraTL * gtm;
PRINT USING "W##"; WeaTL * gtm;
PRINT USING " - B##"; BioTL * gtm;
PRINT USING "M##"; MedTL * gtm;
PRINT USING "E##"; EnvTL * gtm

'  PRINT LEFT$(tecx$, 32)
LOCATE 21 + sg, 39: PRINT "Bases: "; LEFT$(bax$, 32)
' LOCATE 23 + sg, 39:     PRINT "Trade: "; stp$; "-"; tec$; " "; trade$; " Cr"; tbase; pol$
PRINT
101
IF xxx = 1 THEN xxx = 0: a$ = "x": GOTO 199

a$ = "d"
' GOSUB 1000
199 a$ = LCASE$(a$)
IF a$ = "r" OR a$ = "b" OR a$ = "p" OR a$ = "1" OR a$ = "n" OR a$ = "s" OR a$ = "t" THEN GOTO 100
IF a$ = "x" AND tv0 > 0 THEN a$ = "b": GOTO 100
IF a$ = "x" AND tv0 = 0 THEN GOSUB 30000: a$ = "1": GOTO 100
IF a$ = "D" OR a$ = "d" THEN GOTO 3300: ' Temperatur
' IF a$ = "3" THEN GOTO 3310: ' Worlddata

' IF a$ = "5" THEN GOTO 3360: ' Resources
' IF a$ = "7" THEN GOTO 3381: ' Tech Level

IF a$ = "h" OR a$ = "H" THEN GOSUB 120: GOTO 101
IF a$ = " " THEN RUN
GOTO 101

1000 a$ = INKEY$: IF a$ = "" GOTO 1000
IF a$ = "q" THEN END
IF a$ = "r" THEN RUN

RETURN

120 ' HELP
CLS: PRINT " "; ups$: PRINT
PRINT
PRINT " 1 = Starmap, Starport, Pop, Tech AND REROLL WORLD"
PRINT ""
PRINT " D = System Data as text file"
PRINT
PRINT " P = Politics"
PRINT " N = Names"
PRINT " S = Suns"
PRINT " T = Trade Values"
PRINT " R = Port, BTN (5 to 12), Facilities (Traveller Chronicle 4)"
PRINT " B = WTN (Gurps Far Trader)"
PRINT
PRINT " SPACE = Find world"
RETURN


3000
code = 0
IF VAL(code$) > 0 THEN code = VAL(code$): RETURN
IF code$ = "A" THEN code = 10
IF code$ = "B" THEN code = 11
IF code$ = "C" THEN code = 12
IF code$ = "D" THEN code = 13
IF code$ = "E" THEN code = 14
IF code$ = "F" THEN code = 15
IF code$ = "G" THEN code = 16
IF code$ = "H" THEN code = 17
IF code$ = "J" THEN code = 18
IF code$ = "K" THEN code = 19
IF code$ = "L" THEN code = 20
IF code$ = "M" THEN code = 21
IF code$ = "N" THEN code = 22
RETURN


















3100 'Ausarbeitung


' 'Planetary Data
namx$ = MID$(upp$(qq), 6, 20): IF TSAO = 1 THEN namx$ = LEFT$(upp$(qq), 20)
30101 IF RIGHT$(namx$, 1) = " " THEN namx$ = LEFT$(namx$, LEN(namx$) - 1): GOTO 30101
upp$ = MID$(upp$(qq), 27, 40)
upp$ = upp$(qq)
' IF upp$ = "" THEN stop
2001
GOSUB 3900
code$ = siz$: GOSUB 3000: siz = code: IF siz$ = "S" THEN siz = .4
code$ = atm$: GOSUB 3000: atm = code
code$ = hyd$: GOSUB 3000: hyd = code
code$ = pop$: GOSUB 3000: pop = code
code$ = gov$: GOSUB 3000: gov = code
code$ = law$: GOSUB 3000: law = code
code$ = tec$: GOSUB 3000: tec = code


code$ = MUL$: GOSUB 3000: mul = code: mul = mul + RND
IF mul < 0 THEN mul = 1
IF TSAO = 1 THEN pop = pop + RND
POPULATION = INT(10 ^ pop * mul): POP0 = POPULATION

code$ = asg$: GOSUB 3000: asg = code
code$ = gas$: GOSUB 3000: gas = code: IF TSAO = 1 AND gas$ = "G" THEN gas = 1

capital = 0: FOR u = 1 TO LEN(tra$)
    IF UCASE$(MID$(tra$, u, 2)) = "CP" THEN capital = .5
    IF UCASE$(MID$(tra$, u, 2)) = "CX" THEN capital = 1
NEXT
traffic = INT(RND * 6) + 1 + INT(RND * 6) + 1 - 2: REM traffic = 5
facility = INT(RND * 6) + 1 + INT(RND * 6) + 1 - 6: REM facility = 1
tbase = 4000
tt = 0
IF atm = 0 THEN atmx$ = "No atmosphere"
IF atm = 1 THEN atmx$ = "Trace"
IF atm = 2 THEN atmx$ = "Very thin, tainted": tt = 1
IF atm = 3 THEN atmx$ = "Very thin"
IF atm = 4 THEN atmx$ = "Thin, tainted": tt = 1
IF atm = 5 THEN atmx$ = "Thin"
IF atm = 6 THEN atmx$ = "Standard"
IF atm = 7 THEN atmx$ = "Standard, tainted": tt = 1
IF atm = 8 THEN atmx$ = "Dense"
IF atm = 9 THEN atmx$ = "Dense, tainted": tt = 1
IF atm = 10 THEN atmx$ = "Exotic"
IF atm = 11 THEN atmx$ = "Corrosive"
IF atm = 12 THEN atmx$ = "Insidious"
IF atm = 13 THEN atmx$ = "Dense, high"
IF atm = 14 THEN atmx$ = "Elipsoid"
IF atm = 15 THEN atmx$ = "Thin, low"
tt$ = ""
IF tt = 0 GOTO 3200
ww = INT((RND * 6) + (RND * 6)) + 2
IF pox$ <> " " THEN code$ = pox$: GOSUB 3000: ww = code
IF ww < 3 OR ww = 12 THEN atmx$ = "Disease": tt$ = "The atmosphere contains highly contagious components. "
IF ww = 3 OR ww = 10 THEN atmx$ = "Gas Mix": tt$ = "The gas mixture is unhealthy. "
IF ww = 4 THEN atmx$ = "High Oxygen": tt$ = "The oxygen percentage is too high. "
IF ww = 5 OR ww = 9 THEN atmx$ = "Pollutants": tt$ = "The atmosphere is highly polluted. "
IF ww = 7 THEN atmx$ = "Pollutants": tt$ = "The atmosphere is polluted by industrial waste. "
IF ww = 6 OR ww = 8 THEN atmx$ = "Sulfur Compounds": tt$ = "The atmosphere is polluted by sulfur compounds. "
IF ww = 11 THEN atmx$ = "Low Oxygen": tt$ = "The oxygen percentage is too low. "
IF ww = 13 THEN atmx$ = "Radioactive": tt$ = "The atmosphere is irradiated. "
IF ww = 14 THEN atmx$ = "Dust": tt$ = "The atmosphere is polluted by dust particles. "
IF ww = 15 THEN atmx$ = "Toxic": tt$ = "The atmosphere contains higly poisonous compounds. "
IF ww = 16 THEN atmx$ = "Allergens": tt$ = "The atmosphere contains hihgly allergic compounds. "

3200 '

IF atm < 4 THEN cloud$ = " 00 00 10 10 20 20 20 20 20 20 20": ovc$ = "0001020304040404040404": prec$ = "0001020405050505050505"
IF atm > 3 AND atm < 10 THEN cloud$ = " 00 00 10 10 20 30 40 50 60 70 70": ovc$ = "0001020304050607080910": prec$ = "0001020405060708091011"
IF atm > 9 AND atm < 14 OR atm = 15 THEN cloud$ = " 40 40 50 50 60 70 80 90100110120": ovc$ = "0405060708091011121314": prec$ = "0405060809101112131415"
IF atm = 14 THEN cloud$ = " 00 00 05 05 10 15 20 25 30 35 35": ovc$ = "0000010102020303040405": prec$ = "0000010202030304040505"
cloud = VAL(MID$(cloud$, hyd * 3 + 1, 3))
ovc = VAL(MID$(ovc$, hyd * 2 + 1, 2))
prec = VAL(MID$(prec$, hyd * 2 + 1, 2))
IF bse < -50 THEN cloud = 0: ovc = 0: prec = 0
pxl$ = "is a non aligned world."
ImpTL = 12
IF pol$ = "Wi" OR pol$ = "WI" THEN pxl$ = "belongs to the Wilds.": ImpTL = 0
IF pol$ = "Re" OR pol$ = "RE" THEN pxl$ = "is a member of the Regency."
IF pol$ = "As" OR pol$ = "AS" THEN pxl$ = "is a member of the Aslan Hierate.": ImpTL = 10
IF pol$ = "Af" OR pol$ = "AF" THEN pxl$ = "is an Aslan Frontier World.": ImpTL = 12
IF pol$ = "Cs" OR pol$ = "CS" THEN pxl$ = "is a Client State.": traffic = traffic - 1
IF pol$ = "Da" OR pol$ = "DA" THEN pxl$ = "is a member of the Darrian confederation.": ImpTL = 14
IF pol$ = "Dd" OR pol$ = "DD" THEN pxl$ = "is a member of the Domain of Deneb.": ImpTL = 15
IF pol$ = "Dr" OR pol$ = "DR" THEN pxl$ = "is a Droyne world."
IF pol$ = "Fa" OR pol$ = "FA" THEN pxl$ = "is a member of the Federation of Arden"
IF pol$ = "Na" OR pol$ = "NA" THEN pxl$ = "is a non alligned world.": traffic = traffic - 2
IF pol$ = "Rf" OR pol$ = "RF" THEN pxl$ = "belongs to the Regency Frontier."
IF pol$ = "Sf" OR pol$ = "SF" THEN pxl$ = "is a member of the Senlis Federation."
IF pol$ = "Sw" OR pol$ = "SW" THEN pxl$ = "is a member of the Sword Worlds.": ImpTL = 11
IF sector$ = "Gateway" AND pol$ = "Sw" THEN pxl$ = "belongs to the Swanfei Free Worlds."
IF pol$ = "Zh" OR pol$ = "ZH" THEN pxl$ = "is a member of the Zhodani consulate.": ImpTL = 14
IF pol$ = "Vg" OR pol$ = "VG" THEN pxl$ = "is a Vargr world.": ImpTL = 12
IF pol$ = "Im" OR pol$ = "IM" THEN pxl$ = "is a member of the 3rd Imperium.": ImpTL = 15
IF pol$ = "Ho" OR pol$ = "HO" THEN pxl$ = "is a member of the Hochiken People's Assembly."
IF pol$ = "Ga" OR pol$ = "GA" THEN pxl$ = "belongs to the Galian Federation."
IF pol$ = "Pl" OR pol$ = "PL" THEN pxl$ = "is a member of the Plaviam League."
IF pol$ = "Tr" OR pol$ = "TR" THEN pxl$ = "is a member of the Trindel Confederacy."
IF pol$ = "Vi" OR pol$ = "VI" THEN pxl$ = "is a member of the Viyard Concourse."
IF pol$ = "Ak" OR pol$ = "AK" THEN pxl$ = "is a member of the Akeena Union."
IF pol$ = "Me" OR pol$ = "ME" THEN pxl$ = "is a member of the Megusard Corporate."
IF pol$ = "Re" OR pol$ = "RE" THEN pxl$ = "is a member of the Renkard Union."

GOTO 189

188 'wilds
IF gov = 0 THEN gov$ = "No government"
IF gov = 1 THEN gov$ = "Tribal Government"
IF gov = 2 THEN gov$ = "Participating democracy"
IF gov = 3 THEN gov$ = "Representative democracy"
IF gov = 4 THEN gov$ = "Charismatic dictator"
IF gov = 5 THEN gov$ = "Charismatic oligarchy"
IF gov = 6 THEN gov$ = "TED"
IF gov = 7 THEN gov$ = "Mystic dictatorship"
IF gov = 8 THEN gov$ = "Totalitaran oligarchy"
IF gov = 9 THEN gov$ = "Mystic autocracy"
IF gov = 10 THEN gov$ = "Civil service bureaucracy"
IF gov = 11 THEN gov$ = "Self perpetuating Oligarchy"
IF gov = 12 THEN gov$ = "Impersonal beauraucracy"

189 '

IF law = 0 THEN law$ = "No prohibitions": cr = 0
IF law = 1 THEN law$ = "Body pistols, explosives and poison gas probibited": cr = 1
IF law = 2 THEN law$ = "Portable energy weapons prohibited": cr = 1
IF law = 3 THEN law$ = "Machineguns, automatic rifles prohibited": cr = 2
IF law = 4 THEN law$ = "Light assault weapons prohibited": cr = 2
IF law = 5 THEN law$ = "Personal concealable weapons prohibited": cr = 3
IF law = 6 THEN law$ = "All firearms exept shotguns prohibited": cr = 4
IF law = 7 THEN law$ = "Shotguns prohibited": cr = 4
IF law = 8 THEN law$ = "Long blade weapons controlled": cr = 5
IF law = 9 THEN law$ = "Possesion of weapons outside home prohibited": cr = 6
IF law > 9 THEN law$ = "Extreme Law. All weapons prohibited.": cr = 6
'IF LEN(law$) > 32 THEN law$ = LEFT$(law$, 32)

gtl = 0
IF tec = 0 THEN tecx$ = "Stone Age": gtl = 3
IF tec = 1 THEN tecx$ = "Bronze/Iron Age": gtl = 4
IF tec = 2 THEN tecx$ = "1400 - 1700": gtl = 5
IF tec = 3 THEN tecx$ = "1700 - 1860": gtl = 5.5
IF tec = 4 THEN tecx$ = "1860 - 1900": gtl = 5.9
IF tec = 5 THEN tecx$ = "1900 - 1939": gtl = 6
IF tec = 6 THEN tecx$ = "1940 - 1979": gtl = 6
IF tec = 7 THEN tecx$ = "1980 - 2009": gtl = 7
IF tec = 8 THEN tecx$ = "2010 - 2049": gtl = 8
IF tec = 9 THEN tecx$ = "Near Future": gtl = 9
IF tec = 10 THEN tecx$ = "Interstellar community": gtl = 9
IF tec = 11 THEN tecx$ = "Lower average Imperial": gtl = 9
IF tec = 12 THEN tecx$ = "Average Imperial": gtl = 10
IF tec = 13 THEN tecx$ = "Above average Imperial": gtl = 10
IF tec = 14 THEN tecx$ = "Above average Imperial": gtl = 11
IF tec = 15 THEN tecx$ = "Imperial Maximum": gtl = 12
IF tec = 16 THEN tecx$ = "Occasional Imperial": gtl = 13
IF tec < 4 THEN facility = facility - 6
IF tec > 3 AND tec < 6 THEN facility = facility - 4
IF tec > 5 AND tec < 9 THEN facility = facility - 2
IF tec > 8 AND tec < 11 THEN facility = facility - 1
IF tec > 12 AND tec < 17 THEN facility = facility + 1
IF tec > 16 THEN facility = facility + 2
gtl = tec

tbase = tbase + tec * 100
bax$ = ""
IF bas$ = "NS" THEN bax$ = " hosts an Imperial Naval as well as a Scout Base.": facility = facility + 3
IF bas$ = "S " THEN bax$ = " hosts a Scout Base.": facility = facility + 2
IF bas$ = "N " THEN bax$ = " hosts an Imperial Naval Base.": facility = facility + 2
IF bas$ = "Z " THEN bax$ = " hosts a Zhodani Naval Base.": facility = facility + 2
IF bas$ = "D " THEN bax$ = " has an Imperial Depot spanning the whole system.": facility = facility + 4
IF bas$ = "NW" THEN bax$ = " hosts an Imperial Naval Base and a Scout Way Station.": facility = facility + 3
IF bas$ = "W " THEN bax$ = " hosts a X-Boat Way Station.": facility = facility + 3
IF bas$ = "M " THEN bax$ = " hosts a Military Base.": facility = facility + 2
IF bas$ = "C " THEN bax$ = " hosts a Vargr Corsair Base."
IF bas$ = "I " THEN bax$ = " is an Interface World.": facility = facility + 2
IF bas$ = "T " THEN bax$ = " is a Terminus World.": facility = facility + 2
IF bax$ <> "" THEN bax$ = namx$ + bax$


IF zon$ = "A" THEN traffic = traffic - 2: facility = facility - 1
IF zon$ = "R" THEN traffic = traffic - 4: facility = facility - 2

IF pop < 4 THEN traffic = traffic - 4: GOTO 667
IF pop < 6 THEN traffic = traffic - 2: GOTO 667
IF pop < 7 THEN traffic = traffic - 1: GOTO 667
IF pop = 8 THEN traffic = traffic + 1: GOTO 667
IF pop = 9 THEN traffic = traffic + 3: GOTO 667
IF pop > 9 THEN traffic = traffic + 4: GOTO 667

667 'TRADE CODES
trade$ = ""
tdsc$ = ""

agr = 0: IF (atm > 3 AND atm < 10) AND (hyd > 3 AND hyd < 9) AND (pop > 4 AND pop < 8) THEN agr = 1: trade$ = trade$ + "Ag ": tdsc$ = tdsc$ + "agricultural, ": tbase = tbase - 1000: traffic = traffic + 3
ast = 0: IF siz = 0 AND atm = 0 AND hyd = 0 THEN ast = 1: trade$ = trade$ + "As ": tdsc$ = tdsc$ + "asteroid, ": tbase = tbase - 1000: traffic = traffic + 3
bar = 0: IF pop = 0 AND gov = 0 AND law = 0 THEN bar = 1: trade$ = trade$ + "Ba ": tdsc$ = tdsc$ + "barren, ": tbase = tbase + 1000: traffic = traffic - 1
des = 0: IF atm > 1 AND hyd = 0 THEN des = 1: trade$ = trade$ + "Ds ": tdsc$ = tdsc$ + "desert, ": tbase = tbase + 1000: traffic = traffic + 1
flu = 0: IF atm > 9 AND hyd > 0 THEN flu = 1: trade$ = trade$ + "Fl ": tdsc$ = tdsc$ + "fluid, ": tbase = tbase + 1000: traffic = traffic - 1
hig = 0: IF pop > 8 THEN hig = 1: trade$ = trade$ + "Hi ": tdsc$ = tdsc$ + "high population, ": tbase = tbase - 1000: traffic = traffic + 2
ice = 0: IF atm < 2 AND hyd > 0 THEN ice = 1: trade$ = trade$ + "Ic ": tdsc$ = tdsc$ + "ice, ": tbase = tbase - 0: traffic = traffic - 1
ind = 0: IF (atm < 2 OR atm = 4 OR atm = 7 OR atm = 9) AND pop > 8 THEN ind = 1: trade$ = trade$ + "In ": tdsc$ = tdsc$ + "industrial, ": tbase = tbase - 1000: traffic = traffic + 4
low = 0: IF pop < 4 THEN low = 1: trade$ = trade$ + "Lo ": tdsc$ = tdsc$ + "low population, ": tbase = tbase + 1000: traffic = traffic - 1
nag = 0: IF atm < 4 AND hyd < 4 AND pop > 5 THEN nag = 1: trade$ = trade$ + "Na ": tdsc$ = tdsc$ + "non-agricultural, ": tbase = tbase + 1000
nin = 0: IF pop < 7 THEN nin = 1: trade$ = trade$ + "Ni ": tdsc$ = tdsc$ + "non-industrial, ": tbase = tbase + 1000: traffic = traffic - 2
por = 0: IF atm > 1 AND atm < 6 AND hyd < 4 THEN por = 1: trade$ = trade$ + "Po ": tdsc$ = tdsc$ + "poor, ": tbase = tbase - 1000: traffic = traffic - 2
ric = 0: IF (atm = 6 OR atm = 8) AND (pop > 5 AND pop < 9) AND (gov > 3 AND gov < 10) THEN ric = 1: tdsc$ = tdsc$ + "rich, "
vac = 0: IF atm = 0 THEN vac = 1: trade$ = trade$ + "Va ": tdsc$ = tdsc$ + "": tdsc$ = tdsc$ + "vacuum, ": IF ast = 0 THEN tbase = tbase + 1000
wat = 0: IF hyd = 10 THEN wat = 1: trade$ = trade$ + "Wa ": tdsc$ = tdsc$ + "": tbase = tbase - 0: traffic = traffic + 1: tdsc$ = tdsc$ + "water, "
tmi = 1: IF agr = 1 AND atm < 8 THEN tmi = .6
exx = 0: IF siz < 1 OR atm < 3 OR atm > 9 OR ast = 1 OR des = 1 OR flu = 1 OR ice = 1 OR vac = 1 THEN exx = 1
GOSUB 9100
IF tdsc$ <> "" THEN tdsc$ = LEFT$(tdsc$, (LEN(tdsc$) - 2))
facility = facility + traffic

IF othw = 1 THEN othw = 0: RETURN

agr0 = agr: ast0 = ast: bar0 = bar: des0 = des: flu0 = flu: hig0 = hig: ice0 = ice: ind0 = ind
low0 = low: nag0 = nag: nin0 = nin: por0 = por: ric0 = ric: vac0 = vac: wat0 = wat
tec0 = tec: exx0 = exx
po0$ = pol$

'Sonne
s1$ = LEFT$(sox$, 1)
mag$ = RIGHT$(sox$, 1)
s2 = VAL(MID$(sox$, 2, 1))
'IF s2 < 5 THEN s2 = 0 ELSE s2 = 5
mag = 0
mag = VAL(mag$)
IF mag = 0 AND mag$ = "V" THEN mag = 5
IF mag = 0 AND mag$ = "D" THEN mag = 6
IF mag = 0 AND mag$ = "III" THEN mag = 3
IF mag = 0 AND mag$ = "IV" THEN mag = 4
IF mag = 0 AND mag$ = "II" THEN mag = 2
mag = mag - 1
IF mag < 1 THEN mag = 5
IF mag < 0 THEN mag = 0
IF s1$ = "A" THEN solz = 0: weather = weather + 4: col$ = "bluewhite": son$ = "blue"
IF s1$ = "F" THEN solz = 10: weather = weather + 2: col$ = "white": son$ = "white"
IF s1$ = "G" THEN solz = 20: col$ = "Yellow": son$ = "yellow"
IF s1$ = "K" THEN solz = 30: weather = weather - 1: col$ = "orange": son$ = "orange"
IF s1$ = "M" THEN solz = 40: weather = weather - 2: col$ = "red": son$ = "red"
solz = solz + s2
IF solz > 40 THEN solz = 40
PRINT sox$; solz; s2; mag
131 ' MAG+
IF mag > 5 THEN mag = 4
lumi = lumi(solz, mag)
smass = mass(solz, mag)
IF mag = 1 THEN stiz$ = " bright Giant"
IF mag = 2 THEN stiz$ = " Giant"
IF mag = 3 THEN stiz$ = " Subgiant"
IF mag = 4 THEN stiz$ = " Star"
IF mag = 5 THEN stiz$ = " Dwarf"



bse = 0: bsx = 0: IF bsy <> 0 THEN bse = bsy: bsx = bsy
IF datx$ = "" GOTO 2900
'  024h025C025ø
'rota = VAL(datx$, 3))
'axtilt = VAL(MID$(datx$, 9, 3))
'bse = VAL(MID$(datx$, 5, 3)): bsx = bse
2900
IF rota = 0 THEN rota = ((INT(RND * 6) + 1 + INT(RND * 6) + 1 - 2) + (INT(RND * 6) + 1 + INT(RND * 6) + 1 - 2) + (INT(RND * 6) + 1 + INT(RND * 6) + 1 - 2) + (INT(RND * 6) + 1 + INT(RND * 6) + 1 - 2) + 5 + (smass / au(tempzone))) * tmi
'IF press < 1 AND press > 0 THEN rota = rota * press * 3
hyz = hyd + ((INT(RND * 10) + 1) - 5) / 10
IF hyz < 0 THEN hyz = 0
IF hyz > 10 THEN hyz = 10




hvycor = 0
moltcor = 0
rockbod = 0
icebod = 0

wtyp = INT(RND * 6) + 1 + INT(RND * 6) + 1
IF siz < 5 THEN wtyp = wtyp + 1
IF siz > 5 THEN wtyp = wtyp - 2
IF atm < 4 THEN wtyp = wtyp + 1
IF atm > 5 THEN wtyp = wtyp - 1: ' -2
IF wtyp < 2 THEN hvycor = 1: wtyp$ = "Heavy Core"
IF wtyp > 1 THEN moltcor = 1: wtyp$ = "Molten Core"
IF wtyp > 10 THEN rockbod = 1: wtyp$ = "Rocky Body"
IF wtyp > 14 THEN icebod = 1: wtyp$ = "Ice Body"
' IF hvycor = 1 THEN density = 1.1 + (INT(RND * 115) + 1) / 100
IF hvycor = 1 THEN density = 1.1 + (INT(RND * 55) + 1) / 100
IF moltcor = 1 THEN density = .76 + (INT(RND * 6) + 1 + INT(RND * 6) + 1 + INT(RND * 6) + 1) * .02
IF rockbod = 1 THEN density = .46 + (INT(RND * 6) + 1 + INT(RND * 6) + 1 + INT(RND * 6) + 1) * .02
IF icebod = 1 THEN density = .6 + (INT(RND * 6) + 1 + INT(RND * 6) + 1 + INT(RND * 6) + 1) * .02
IF siz = 0 THEN siz = .0001
mass = density * (siz / 8) ^ 3

IF grav = 0 THEN grav = mass * (64 / siz ^ 2)
'Atmosphere
press = 0
ge = 1
IF atm = 1 THEN press = (INT(RND * 9) + 1) * .01: atr = 1
IF atm = 2 OR atm = 3 THEN press = .2 + (INT(RND * 20) + 1) / 100: atr = 1
IF atm = 4 OR atm = 5 THEN press = .42 + (INT(RND * 33) + 1) / 100: atr = 2: ge = 1.05
IF atm = 6 OR atm = 7 THEN press = .75 + (INT(RND * 65) + 1) / 100: atr = 2: ge = 1.1
IF atm = 8 OR atm = 9 THEN press = 1.5 + (INT(RND * 90) + 1) / 100: atr = 2: ge = 1.15
' Exotic
IF atm <> 10 GOTO 2910
atmx$ = "Exotic"
ww = INT(RND * 6) + 1 + INT(RND * 6) + 1
IF ww < 4 THEN press = .1 + RND * (30) / 100: atr = 1
IF ww = 4 OR ww = 5 THEN press = .42 + (INT(RND * 33) + 1) / 100: atr = 2: ge = 1.05
IF ww = 6 OR ww = 7 THEN press = .75 + (INT(RND * 65) + 1) / 100: atr = 2: ge = 1.1
IF ww = 8 OR ww = 9 THEN press = 1.5 + (INT(RND * 90) + 1) / 100: atr = 2: ge = 1.15
IF ww = 10 OR ww = 11 THEN press = 2.5 + (INT(RND * 700) + 1): atr = 2: ge = 1.4
2905
IF ww = 2 OR ww = 5 OR ww = 7 OR ww = 9 OR ww = 1 THEN atmx$ = "Irritant"
IF ww = 12 THEN atmx$ = "Occasional Corrosive"

2910 ' Corrosive
IF atm <> 11 GOTO 2920
atmx$ = "Corrosive"
ww = INT(RND * 6) + 1 + INT(RND * 6) + 1
IF ww < 4 THEN press = .1 + RND * (30) / 100: atr = 1
IF ww = 4 OR ww = 5 THEN press = .42 + RND * (33) / 100: atr = 2: ge = 1.05
IF ww = 6 OR ww = 7 THEN press = .75 + RND * (65) / 100: atr = 2: ge = 1.1
IF ww = 8 OR ww = 9 THEN press = 1.5 + RND * (90) / 100: atr = 2: ge = 1.15
IF ww = 10 OR ww = 11 THEN press = 2.5 + RND * (700): atr = 2: ge = 1.4

2920 ' Insidious
IF atm <> 12 GOTO 2930
atmx$ = "Insidious"
press = 200 + RND * (700): atr = 2: ge = 2.2
2930

IF atm = 13 OR atm = 14 THEN press = 2 + RND * (750): ge = 1.15
GOSUB 9800
IF atm > 9 THEN atr = 3
IF atm = 15 THEN atr = 4
IF hyd > 4 THEN weather = weather + 1
bse = 0

txx = -273: tempzone = 0

minte = 0: maxte = 20: GOSUB 12000: IF txx > -273 GOTO 3290

minte = -10: maxte = 40: GOSUB 12000: IF txx > -273 GOTO 3290

minte = -20: maxte = 60: GOSUB 12000: IF txx > -273 GOTO 3290

minte = -30: maxte = 70: GOSUB 12000: IF txx > -273 GOTO 3290

minte = -35: maxte = 80: GOSUB 12000: IF txx > -273 GOTO 3290

IF txx = -273 THEN temp = lumi * orf(0) * hz(hyd, atr) * ge - 273: tempzone = 0: bse = temp: ' IF mag > 1 THEN mag = mag - 1: GOTO 131

3290 period = SQR(au(tempzone) * au(tempzone) * au(tempzone) / smass)

IF bsx > 0 THEN bse = bsx
liv = 0: IF atm = 0 THEN liv = -3
IF atm > 3 AND atm < 10 THEN liv = 4
IF hyd = 0 THEN liv = liv - 2
IF hyd > 1 AND hyd < 9 THEN liv = liv + 1
IF bse < -20 THEN liv = liv - 1
IF bse > 30 THEN liv = liv - 1
IF s1$ = "G" OR s1$ = "K" THEN liv = liv + 1
IF s1$ = "F" OR s1$ = "A" OR s1$ = "B" THEN liv = liv - 1
IF agr = 1 THEN liv = liv + 3
life = 0: IF INT(RND * 6) + 1 + INT(RND * 6) + 1 + liv > 10 THEN life = 1

'IF atm < 10 THEN RETURN

seismic = RND * (3)
IF hvco = 1 THEN seismic = seismic + RND * (2)
IF moltcor = 1 THEN seismic = seismic + RND * (3)
seismic = seismic + smass / au(tempzone)




'axial Tilt

IF axtilt > 0 GOTO 3400
ww = INT(RND * 6) + INT(RND * 6) + 2
IF ww < 4 THEN axtilt = INT(RND * 6) + INT(RND * 6): GOTO 3400
IF ww < 6 THEN axtilt = INT(RND * 6) + INT(RND * 6) + 10: GOTO 3400
IF ww < 8 THEN axtilt = INT(RND * 6) + INT(RND * 6) + 20: GOTO 3400
IF ww < 11 THEN axtilt = INT(RND * 6) + INT(RND * 6) + 30: GOTO 3400
IF ww < 12 THEN axtilt = INT(RND * 6) + INT(RND * 6) + 40: GOTO 3400
IF ww = 12 THEN axtilt = 40 + RND * (50): GOTO 3400





3400 'Season
IF axtilt = 0 THEN S00 = 0: S75 = 0: S50 = 0: S25 = 11: weather = weather - 1: GOTO 3500
IF axtilt = 1 THEN S00 = 0: S75 = 0: S50 = 11: S25 = 10: weather = weather - 1: GOTO 3500
IF axtilt < 4 THEN S00 = 0: S75 = 11: S50 = 10: S25 = 9: weather = weather - 1: GOTO 3500
IF axtilt < 6 THEN S00 = 11: S75 = 10: S50 = 9: S25 = 8: weather = weather - 1: GOTO 3500
IF axtilt < 9 THEN S00 = 10: S75 = 9: S50 = 8: S25 = 7: weather = weather - 1: GOTO 3500
IF axtilt < 13 THEN S00 = 9: S75 = 8: S50 = 7: S25 = 6: GOTO 3500
IF axtilt < 17 THEN S00 = 8: S75 = 7: S50 = 6: S25 = 5: GOTO 3500
IF axtilt < 23 THEN S00 = 7: S75 = 6: S50 = 5: S25 = 4: GOTO 3500
IF axtilt < 29 THEN S00 = 6: S75 = 5: S50 = 4: S25 = 3: weather = weather + 1: GOTO 3500
IF axtilt < 35 THEN S00 = 5: S75 = 4: S50 = 3: S25 = 2: weather = weather + 1: GOTO 3500
IF axtilt < 45 THEN S00 = 4: S75 = 3: S50 = 2: S25 = 1: weather = weather + 2: GOTO 3500
IF axtilt < 60 THEN S00 = 3: S75 = 2: S50 = 1: S25 = 0: weather = weather + 3: GOTO 3500
IF axtilt < 85 THEN S00 = 2: S75 = 1: S50 = 0: S25 = 0: weather = weather + 4: GOTO 3500
S00 = 1: S75 = 0: S50 = 0: S25 = 0: GOTO 3500

3500 'day & night
IF press < .01 THEN inc = 1: pluslim = (bse + 273) * .1: dec = 20: minlim = (bse + 273) * .8: GOTO 3600
IF press < .1 THEN inc = .9: pluslim = (bse + 273) * .3: dec = 15: minlim = (bse + 273) * .7: GOTO 3600
IF press < .41 THEN inc = .8: pluslim = (bse + 273) * .8: dec = 8: minlim = (bse + 273) * .5: GOTO 3600
IF press < .76 THEN inc = .6: pluslim = (bse + 273) * 1.5: dec = 3: minlim = (bse + 273) * .3: GOTO 3600
IF press < 1.4 THEN inc = .5: pluslim = (bse + 273) * 2.5: dec = 1: minlim = (bse + 273) * .15: GOTO 3600
IF press < 2.4 THEN inc = .4: pluslim = (bse + 273) * 4!: dec = .5: minlim = (bse + 273) * .1: GOTO 3600
inc = .2: pluslim = (bse + 273) * 5!: dec = .2: minlim = (bse + 273) * .05: GOTO 3600


3600 'weather

minlim = minlim / 4


IF bse > 30 THEN weather = weather + 3: GOTO 3601
IF bse > 25 THEN weather = weather + 2: GOTO 3601
IF bse > 20 THEN weather = weather + 1: GOTO 3601
IF bse < 10 THEN weather = weather - 1: GOTO 3601
IF bse < -10 THEN weather = weather - 4: GOTO 3601
IF bse < -5 THEN weather = weather - 3: GOTO 3601
IF bse < 0 THEN weather = weather - 2: GOTO 3601
IF bse < 5 THEN weather = weather - 1: GOTO 3601


3601 'day
tmult = 24 / rota
jz = period * 365.25 / 4 * tmult
day = rota / 2 * inc: IF day > pluslim THEN day = pluslim
night = rota / 2 * dec: IF night > minlim THEN night = minlim

IF atm = 0 THEN weather = 0
RETURN

3300 ' START OF TEXT GENERATION


' START OF TEXT GENERATION
' START OF TEXT GENERATION
' START OF TEXT GENERATION
' START OF TEXT GENERATION
' START OF TEXT GENERATION
' START OF TEXT GENERATION



output$ = LEFT$(upp$, 35) + ".rtf"

nnn = 1: OPEN output$ FOR OUTPUT AS #1

ups1$ = RIGHT$(ups$, LEN(ups$) - 5)
PRINT #nnn, ups1$: PRINT #nnn,

PRINT #nnn, LEFT$(upp$, 4); " "; LEFT$(sector$, LEN(sector$) - 4)
PRINT #nnn,
PRINT #nnn, "http://wiki.travellerrpg.com/" + namx$ + "_(world)"
PRINT #nnn,


IF bse < -50 THEN GOTO 3309
jz = period * 365.25 / 4 * tmult


3309 ' Textausgabe Planet und Efficiency
PRINT #nnn,

PRINT #nnn, "FIRST IMPRESSIONS WHEN YOU OPEN THE AIRLOCK:"
PRINT #nnn,

9700 ' Beschreibung
subgr = smass / au(tempzone)
IF subgr < .25 THEN subg$ = "A tiny "
IF subgr >= .25 AND subgr < .5 THEN subg$ = "A very small "
IF subgr >= .5 AND subgr < .75 THEN subg$ = "A small "
IF subgr >= .75 AND subgr < 1.1 THEN subg$ = "A "
IF subgr >= 1.1 AND subgr < 1.3 THEN subg$ = "A big "
IF subgr >= 1.3 AND subgr < 1.7 THEN subg$ = "A huge "
IF subgr >= 1.7 THEN subg$ = "A gigantic "
'IF siz = 0 OR atm = 0 THEN subg$ = " "
sonne$ = subg$ + son$
IF subgr > 10 THEN planet$ = "The whole sky is filled by a " + col$ + " Sun. ": GOTO 9702


wolk$ = "shines from a cloudless Sky. "
IF cloud >= 10 THEN wolk$ = "shines from a nearly cloudless sky. "
IF cloud >= 20 THEN wolk$ = "shines from a sky with no significant clouds. "
IF cloud >= 30 THEN wolk$ = "shines from a sky dotted with a few clouds. "
IF cloud >= 40 THEN wolk$ = "shines from amidst scattered clouds. "
IF cloud >= 50 THEN wolk$ = "shines through a broken cloudcover. "
IF cloud >= 60 THEN wolk$ = "is partially concealed by heavy clouds. "
IF cloud >= 70 THEN wolk$ = "is mostly concealed by overcast. "
IF cloud >= 80 THEN wolk$ = "is nearly completely concealed by overcast. "
IF cloud >= 90 THEN wolk$ = "is completely concealed by overcast. "
planet$ = planet$ + " " + wolk$

planet$ = sonne$ + " Sun " + wolk$

wet = ((weather + 5) / 14 * 15): IF atm < 2 THEN wet = 0
IF wet < 5 THEN wind$ = "There is no wind."
IF wet > 4 AND wet < 7 THEN wind$ = "There is a gentle breeze."
IF wet > 6 AND wet < 9 THEN wind$ = "There is a stiff breeze."
IF wet > 8 AND wet < 11 THEN wind$ = "The wind is really strong."
IF wet > 10 AND wet < 13 THEN wind$ = "A gale is blowing."
IF wet > 12 THEN wind$ = "A strong gale is blowing."
IF wet > 14 THEN wind$ = "As usual, there is a monstrous storm raging."
planet$ = planet$ + wind$
9702

IF siz = 0 OR atm < 2 GOTO 9711
bsx = bse: GOSUB 7100: tep0$ = tep$
planet$ = planet$ + " The mornings are " + tep$
bsx = bse - night: GOSUB 7100: tep1$ = tep$
bsx = bse + day: GOSUB 7100
IF tep$ = tep0$ AND tep$ = tep1$ THEN planet$ = planet$ + ". ": GOTO 9711
IF tep$ = tep0$ THEN planet$ = planet$ + ", the nights are " + tep1$ + ". ": GOTO 9711
IF tep$ <> tep0$ THEN planet$ = planet$ + ", the nights are " + tep1$ + ", during the day it is " + tep$ + ". "
9711

IF height > 12000 THEN luft$ = " There is no atmosphere to be mentioned.": GOTO 9710

IF height > 1000 THEN luft$ = "The atmospheric pressure is comparable to earth conditions in an altitude of" + STR$(INT(height / 100) / 10) + " km."
IF height > 6500 THEN luft$ = luft$ + " A respirator is needed to survive in this atmosphere.": GOTO 9710
IF height > 1000 GOTO 9710

IF height > -250 THEN luft$ = "The atmospheric pressure is comparable to earth norm.": GOTO 9710
IF height > -3000 THEN luft$ = "The atmospheric pressure is comparable to earth conditions in a depth of" + STR$(INT(-height)) + " m.": GOTO 9710
IF height > -5000 THEN luft$ = "The atmospheric pressure is very high.": GOTO 9710
luft$ = "The atmospheric pressure is murderous."

9710
ww = INT(RND * 9) + 1
IF ww = 1 THEN compo$ = "Carbon Dioxide (CO2)"
IF ww = 2 THEN compo$ = "Sulphur Dioxide (SO2)"
IF ww = 3 THEN compo$ = "Methane, Ammonia, Hydrogen"
IF ww = 4 THEN compo$ = "Nitrogen Trichloride (NCl3)"
IF ww = 5 THEN compo$ = "Carbonyl Fluoride (COF2)"
IF ww = 6 THEN compo$ = "Hydrogen (H)"
IF ww = 7 THEN compo$ = "Chlorofluorocarbon (CFC)"
IF ww = 8 THEN compo$ = "Disulfur dichloride (S2Cl2)"
IF ww = 9 THEN compo$ = "Nitrogen trifluoride (NF3)"



IF atm = 10 THEN luft$ = "The atmosphere contains exotic components (dangerous amounts of " + compo$ + "), protective equipment is mandatory. " + luft$
IF atm = 11 THEN luft$ = "The atmosphere contains corrosive components (" + compo$ + "), the use of sealed protective suits is mandatory. " + luft$
IF atm = 12 THEN luft$ = "The atmosphere is extremly corrosive (it contains a high percentage of " + compo$ + "), that can defeat any protection over time. " + luft$

planet$ = planet$ + luft$ + " "

IF tt$ <> "" THEN planet$ = planet$ + tt$
regen$ = "The humidity is extremely high."
IF prec < 11 THEN regen$ = "The humidity is very high."
IF prec < 9 THEN regen$ = "The humidity is normal."
IF prec < 7 THEN regen$ = "The air is quite dry."
IF prec < 4 THEN regen$ = "The air is very dry."
IF prec < 3 THEN regen$ = "The air is as dry as in a desert."
IF prec < 1 THEN regen$ = "There is no humidity at all."
IF siz > 0 AND atm > 0 THEN planet$ = planet$ + regen$ + " "

grav$ = "The gravity is overpowering"
IF grav < 2 THEN grav$ = "Gravity is uncomfortably high"
IF grav < 1.5 THEN grav$ = "Gravity is high"
IF grav < 1.2 THEN grav$ = "Gravity is close to earth values"
IF grav < .8 THEN grav$ = "Gravity is slightly lower than earth standard"
IF grav < .66 THEN grav$ = "Gravity is about half of earth standard"
IF grav < .4 THEN grav$ = "Gravity is comparable to that of Mars"
IF grav < .1 THEN grav$ = "Gravity is comparable to that of the Moon"
grav$ = grav$ + ". A standard sized human (80kg) will weigh about" + STR$(CINT(80 * grav)) + " kg"
planet$ = planet$ + grav$ + "."


text$ = planet$: GOSUB 19999
PRINT #nnn,
PRINT #nnn,
PRINT #nnn, "THE SOLAR SYSTEM, PLANETARY DATA:"
'PRINT #nnn,

planet$ = namx$ + "'s "

IF LEN(sol$) > 6 THEN planet$ = planet$ + "primary "

IF sunname$ = "" THEN planet$ = planet$ + "sun is a " + col$ + stiz$ ELSE planet$ = sunname$ + " is a " + col$ + stiz$

planet$ = planet$ + " (" + sol$ + ")"

planet$ = planet$ + ". It has a Mass of" + STR$(smass) + " and a Luminosity of" + STR$(lumi) + "."
planet$ = planet$ + " It's perceived size is" + STR$(INT(smass / au(tempzone) * 100)) + "% SOL. "
IF TSAO = 1 THEN worlds = INT(10 * RND)
planet$ = planet$ + "The system has" + STR$(worlds) + " worlds"
IF gas = 0 THEN planet$ = planet$ + ", but no gas giant"
IF gas = 1 THEN planet$ = planet$ + ", one of them is a gas giant"
IF gas > 1 THEN planet$ = planet$ + "," + STR$(gas) + " of which are gas giants"

IF asg > 0 THEN planet$ = planet$ + " and" + STR$(asg) + " asteroid belt": IF asg > 1 THEN planet$ = planet$ + "s"

planet$ = planet$ + ". "
tmult = 24 / rota
yearprint = INT(period * 365.25 * tmult * 100) / 100

IF siz < 1 THEN GOTO 9790
planet$ = planet$ + namx$ + " circles its sun in Orbit" + STR$(tempzone) + ", at a distance of" + STR$((INT(au(tempzone) * 149.6 * 100) / 100)) + " Million kilometers,"



planet$ = planet$ + " one year has a duration of" + STR$(yearprint) + " planetary days of" + STR$(INT(rota * 100) / 100) + " hours, each."

IF axtilt > 0 THEN planet$ = planet$ + " The planetary axis is tilted" + STR$(axtilt) + "°."



9790
GOSUB 15100: planet$ = planet$ + world$: 'surface composition



IF life < 1 THEN planest$ = planet$ + " There is no native life."

IF zon$ = "A" THEN: planet$ = planet$ + " The system is flagged as an Amber Zone."
IF zon$ = "R" THEN: planet$ = planet$ + " The system is flagged as a Red Zone."



text$ = planet$: GOSUB 19999




PRINT #nnn,
PRINT #nnn, USING "Density            :#####.## "; density;: PRINT #nnn, wtyp$
PRINT #nnn, USING "Gravity            :#####.##"; grav
PRINT #nnn, USING "Seismic Factor     :#####.##"; seismic
IF siz <> 0 AND atm <> 0 THEN PRINT #nnn, USING "Air pressure       :#####.## Atm,"; press;: PRINT #nnn, height; "m, ";: PRINT #nnn, atmx$
IF atm > 0 THEN PRINT #nnn, USING "Weather factor     :#####%##.#"; ((weather + 5) / 14 * 100); weather
PRINT #nnn, USING "Energy absorption  :#####%"; hz(hyd, atr) * 100
IF life > 0 THEN PRINT #nnn, USING "Native life        :#####"; liv
PRINT #nnn,

' FOR zzz = 1 TO 10
















' Government
IF POPULATION < 1 OR pop < 1 THEN text$ = "": GOTO 3330



PRINT #nnn,
PRINT #nnn, "GOVERNMENT, LAW, ECONOMIC AND CULTURAL INFORMATION:"
PRINT #nnn,


namz$ = namx$
IF pop < 6 THEN namz$ = namz$ + " has a population of" + STR$(POPULATION) + " sentients. The world"
IF pop > 5 AND pop < 9 THEN namz$ = namz$ + " has a population of" + STR$(INT(POPULATION / 1E+3) / 1000) + " Million sentients. The world"
IF pop > 8 THEN namz$ = namz$ + " has a population of" + STR$(INT(POPULATION / 1E+6) / 1000) + " Billion sentients. The world"


wars = 1
' IF pol$ = "Wi" OR pol$ = "WI" GOTO 188
IF gov = 0 THEN gov$ = " has no Government Structure. Family bonds predominate.": peas = .5: wars = 1.5
IF gov = 1 THEN gov$ = " is owned by a Corporation. It is ruled by a managerial elite.": peas = .8: wars = 1.4: GOTO 3202
IF gov = 2 THEN gov$ = "'s government is a participating Democracy. The people rule by popular vote.": peas = 1: wars = 1.5: ZW6 = 2: GOTO 32021
IF gov = 3 THEN gov$ = " is governed by a self-perpetuating Oligarchy, a minority isolated from the population.": peas = .9: wars = 1.2: EW6 = INT(RND * 6) + 1: IF EW6 < 5 THEN ZW6 = 3: GOTO 32021 ELSE ZW6 = 8: GOTO 32021
IF gov = 4 THEN gov$ = "'s government is a representative Democracy. The world is governed by elected politicians.": peas = .85: wars = 1.45:
IF gov = 5 THEN gov$ = " is governed by a feudal Technocracy. Governmental relationships are based on  mutually beneficial technical activities.": peas = .95: wars = 1.4
IF gov = 6 THEN gov$ = " has a Captive Government. It is ruled by an externally imposed leadership.": peas = 1.5: wars = 1.5
IF gov = 7 THEN gov$ = " is balkanized. Rival governments compete for control.": peas = 1: wars = 1.5: GOTO 3220
IF gov = 8 THEN gov$ = " is governed by a Civil Service Beaureaucracy. The rulers are agencies, employing individuals selected by merit.": peas = 1.1: wars = 1.2: ZW6 = 8: GOTO 32021
IF gov = 9 THEN gov$ = " is governed by an impersonal Beaureaucracy. The rulers are agencies, isolated from the governed populations.": peas = 1.15: wars = 1.2: ZW6 = 8: GOTO 32021
IF gov = 10 THEN gov$ = " is ruled by a charismatic Dictatorship. The Ruler is a single leader enjoying the confidence of the citizens.": peas = 1.2: wars = 1.5: EW6 = INT(RND * 6) + 1: IF EW6 < 6 THEN ZW6 = 6: GOTO 32021 ELSE ZW6 = 3: GOTO 32021
IF gov = 11 THEN gov$ = " is ruled by a non-charismatic Dictatorship. The Ruler is the successor to a charismatic dictator.": peas = 1.1: wars = 1.2: EW6 = INT(RND * 6) + 1: IF EW6 < 6 THEN ZW6 = 6: GOTO 32021 ELSE ZW6 = 3: GOTO 32021
IF gov = 12 THEN gov$ = " is ruled by a charismatic Oligarchy. The world is governed by a select religious, mystic, or psionic group, organization, or class enjoying the overwhelming confidence of the citizenry.": peas = 1.2: wars = 1.5
IF gov = 13 THEN gov$ = " is ruled by a religious Dictatorship. The world is ruled by prophets.": peas = .75: wars = 1.5
IF gov = 14 THEN gov$ = " is ruled by a religious Autocracy. Government by a single religious, mystic, or psionic leader wielding absolute power.": peas = .75: wars = 1.5
IF gov = 15 THEN gov$ = " is ruled by a totalitarian Oligarchy. They are an all-powerful minority maintaining absolute control through coercion and oppression.": peas = 1.1: wars = 1.2: EW6 = INT(RND * 6) + 1: IF EW6 < 5 THEN ZW6 = 3: GOTO 32021 ELSE ZW6 = 8: GOTO 32021
IF gov = 16 THEN gov$ = "Small Aslan facility": peas = .5: wars = 1.5
IF gov = 17 THEN gov$ = "Split Clan Control": peas = .5: wars = 1.5
IF gov = 18 THEN gov$ = "On world Clan Control": peas = .5: wars = 1.5
IF gov = 19 THEN gov$ = "Multi world Clan Control": peas = .5: wars = 1.5
IF gov = 20 THEN gov$ = "Major Clan Control": peas = .5: wars = 1.5
IF gov = 21 THEN gov$ = "Vassal Clan Control": peas = .5: wars = 1.5
IF gov = 22 THEN gov$ = "Major vassal Clan Control": peas = .5: wars = 1.5



3201 ' balkanized
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
IF gov = 7 GOTO 3220

3202 ' Representative authority
ZW6 = INT(RND * 6) + INT(RND * 6) + 2: IF (gov = 13 OR gov = 14) AND (ZW6 = 2 OR ZW6 = 12) THEN GOTO 3202
32021

IF ZW6 = 2 OR ZW6 = 12 THEN REPA$ = "The entire population decides the acts of Government. In many cases, various qualifications will be established to lessen the participation of some classes of citizen.": GOTO 3203
IF ZW6 = 3 OR ZW6 = 4 OR ZW6 = 5 THEN REPA$ = "One group, with" + STR$(INT(RND * 997) + 3) + " members, confers and decides upon the acts of government."
IF ZW6 = 6 OR ZW6 = 7 THEN REPA$ = "One to three powerful individuals control the functions of the government."
IF ZW6 > 7 THEN REPA$ = "Several councils, wether equal in power or ranked according to system, each exercise influence over the government.": GOTO 3203


3203 ' division of authority
EW6 = INT(RND * 6) + 1
IF EW6 < 3 THEN REPA$ = REPA$ + " The authority is divided in 3-ways. ": GOTO 3204
IF EW6 = 3 THEN REPA$ = REPA$ + " The authority is divided in 2-ways. ": GOTO 3206
IF EW6 = 4 THEN REPA$ = REPA$ + " The authority is divided in 2-ways. ": GOTO 3206
IF EW6 > 4 THEN REPA$ = REPA$ + " The authority is undivided. ": GOTO 3208

3204 ' 3-way division
EW6 = INT(RND * 6) + 1
IF EW6 = 1 OR EW6 = 2 THEN REPA$ = REPA$ + "The Exexutive branch is the representative authority, Legislative and Judicial are secondary.": GOTO 3208
IF EW6 = 3 OR EW6 = 4 THEN REPA$ = REPA$ + "The Legislative branch is the representative authority, Executive and Judicial are secondary.": GOTO 3208
IF EW6 = 5 OR EW6 = 6 THEN REPA$ = REPA$ + "The Judicial branch is the representative authority, Legislative and Executive are secondary.": GOTO 3208


GOTO 3220
3205 ' 3-way Organization of other authorities
ZW6 = INT(RND * 6) + INT(RND * 6) + 2





3206 ' 2-way division
EW6 = INT(RND * 6) + 1
IF EW6 = 1 OR EW6 = 2 THEN REPA$ = REPA$ + "The Executive and Judicial branches are the representative authorities, Legislative is secondary.": GOTO 3208
IF EW6 = 3 OR EW6 = 4 THEN REPA$ = REPA$ + "The Executive and Legislative branches are the representative authorities, Judicial is secondary.": GOTO 3208
IF EW6 = 5 OR EW6 = 6 THEN REPA$ = REPA$ + "The Judicial and Legislative branches are the representative authorities, Executive is secondary.": GOTO 3208
GOTO 3220

3207 ' 2-way Organization of other authorities
ZW6 = INT(RND * 6) + INT(RND * 6) + 2

3208 ' religous profile
ZW6 = INT(RND * 6) + INT(RND * 6) + 2


3220
text$ = namz$ + " " + gov$ + " " + REPA$

GOSUB 19999


'PRINT #nnn,
'IF POPULATION < 1 THEN GOTO 3241

PRINT #nnn,
GOSUB 34000 'Law Level

PRINT #nnn,: text$ = "": GOSUB 14600: 'city

3241
text$ = namx$ + " is a ": IF tdsc$ <> "" THEN text$ = text$ + tdsc$: IF TSAO = 0 THEN text$ = text$ + ", "
IF TSAO = 1 OR ix$ = "  " THEN text$ = text$ + " world.": PRINT #nnn,: GOSUB 19999: GOTO 3330
3372 ' New Data


' PRINT #nnn, " Importance   d   "; ix$:
ix = VAL(ix$)
IF ix < -4 THEN ix1$ = "completely unimportant world,"
IF ix = -3 THEN ix1$ = "totally unimportant world,"
IF ix = -2 THEN ix1$ = "very unimportant world,"
IF ix = -1 THEN ix1$ = "unimportant world,"
IF ix = 0 THEN ix1$ = "world of average importance,"
IF ix = 1 THEN ix1$ = "world of above average importance,"
IF ix = 2 THEN ix1$ = "highly important world,"
IF ix = 3 THEN ix1$ = "very important world,"
IF ix > 3 THEN ix1$ = "extremely important world,"
text$ = text$ + ix1$

ix = VAL(ix$)
IF ix < -4 THEN ix1$ = " is a completely unimportant world,"
IF ix = -3 THEN ix1$ = " is a totally unimportant world,"
IF ix = -2 THEN ix1$ = " is a very unimportant world,"
IF ix = -1 THEN ix1$ = " is an unimportant world,"
IF ix = 0 THEN ix1$ = " is a world of average importance,"
IF ix = 1 THEN ix1$ = " is a world of above average importance,"
IF ix = 2 THEN ix1$ = " is a highly important world,"
IF ix = 3 THEN ix1$ = " is a very important world,"
IF ix > 3 THEN ix1$ = " is an extremely important world,"
REM text$ = text$ + ix1$











':print #nnn, "Ex:"; ex$:print #nnn,
code$ = LEFT$(ex$, 1): GOSUB 3000: resources = code
IF resources < 3 THEN resource$ = " with negligible resources": GOTO 3373
IF resources < 5 THEN resource$ = " with very small resources": GOTO 3373
IF resources < 6 THEN resource$ = " with small resources": GOTO 3373
IF resources < 7 THEN resource$ = " with below average resources": GOTO 3373
IF resources < 9 THEN resource$ = " with average resources": GOTO 3373
IF resources < 10 THEN resource$ = " with above average resources": GOTO 3373
IF resources < 11 THEN resource$ = " with high resources": GOTO 3373
IF resources < 12 THEN resource$ = " with huge resources": GOTO 3373
IF resources < 13 THEN resource$ = " with very huge resources": GOTO 3373
IF resources > 12 THEN resource$ = " with abundant resources": GOTO 3373
3373 text$ = text$ + resource$
code$ = MID$(ex$, 2, 1): GOSUB 3000: labour = code
IF labour < 3 THEN labour$ = " and a nonexistant labour force": GOTO 3374
IF labour < 5 THEN labour$ = " and a very small labour force": GOTO 3374
IF labour < 6 THEN labour$ = " and a small labour force": GOTO 3374
IF labour < 7 THEN labour$ = " and a below average labour force": GOTO 3374
IF labour < 9 THEN labour$ = " and an average labour force": GOTO 3374
IF labour < 10 THEN labour$ = " and an above average labour force": GOTO 3374
IF labour < 11 THEN labour$ = " and a big labour force": GOTO 3374
IF labour < 12 THEN labour$ = " and a huge labour force": GOTO 3374
IF labour < 13 THEN labour$ = " and a very huge labour force": GOTO 3374
IF labour > 12 THEN labour$ = " and an abundant labour force": GOTO 3374
3374 text$ = text$ + labour$
code$ = MID$(ex$, 3, 1): GOSUB 3000: infrastructure = code
IF infrastructure < 3 THEN infrastructure$ = " as well as a nonexisting infrastructure.": GOTO 3375
IF infrastructure < 5 THEN infrastructure$ = " as well as a very weak infrastructure.": GOTO 3375
IF infrastructure < 6 THEN infrastructure$ = " as well as a weak infrastructure.": GOTO 3375
IF infrastructure < 8 THEN infrastructure$ = " as well as an average infrastructure.": GOTO 3375
IF infrastructure < 9 THEN infrastructure$ = " as well as a robust infrastructure.": GOTO 3375
IF infrastructure < 10 THEN infrastructure$ = " as well as a strong infrastructure.": GOTO 3375
IF infrastructure < 11 THEN infrastructure$ = " as well as a very strong infrastructure.": GOTO 3375
IF infrastructure < 13 THEN infrastructure$ = " as well as an extremely strong infrastructure.": GOTO 3375
IF infrastructure > 12 THEN infrastructure$ = " as well as a fantastic infrastructure.": GOTO 3375
3375 text$ = text$ + infrastructure$
code$ = RIGHT$(ex$, 2): GOSUB 3000: Efficiency = code
IF Efficiency = -5 THEN Efficiency$ = " The overall efficiency is abyssmal."
IF Efficiency = -4 THEN Efficiency$ = " The overall efficiency is extremely low."
IF Efficiency = -3 THEN Efficiency$ = " The overall efficiency is very low."
IF Efficiency = -2 THEN Efficiency$ = " The overall efficiency is low."
IF Efficiency = -1 THEN Efficiency$ = " The overall efficiency is below average."
IF Efficiency = -0 THEN Efficiency$ = " The overall efficiency is mediocre."
IF Efficiency = 1 THEN Efficiency$ = " The overall efficiency is above average."
IF Efficiency = 2 THEN Efficiency$ = " The overall efficiency is high."
IF Efficiency = 3 THEN Efficiency$ = " The overall efficiency is very high."
IF Efficiency = 4 THEN Efficiency$ = " The overall efficiency is extremely high."
IF Efficiency = 5 THEN Efficiency$ = " The overall efficiency is marvellous."
text$ = text$ + Efficiency$
effmult = Efficiency: IF effmult = 0 THEN effmult = 1
RU = resources * labour * infrastructure * effmult


text$ = text$ + " The world generates" + STR$(RU) + " Resource units, which puts it in the" + STR$(CINT(RU / 12150 * 100)) + "% Range."
' GOSUB 19999
' RETURN
' print #nnn, "Cx:"; cx$
PRINT #nnn,
homog$ = LEFT$(cx$, 1): code$ = homog$: GOSUB 3000: homog = code
accep$ = MID$(cx$, 2, 1): code$ = accep$: GOSUB 3000: accep = code
stran$ = MID$(cx$, 3, 1): code$ = stran$: GOSUB 3000: stran = code
symbo$ = RIGHT$(cx$, 1): code$ = symbo$: GOSUB 3000: symbo = code
' text$ = ""

' flux = +5 to -5 = 1W6 - 1W6

text$ = text$ + " The Homogeneity Rating is" + STR$((homog + 5) * 5) + "%, "
text$ = text$ + "Acceptance reaches" + STR$(CINT(accep / 14 * 100)) + "%, "
text$ = text$ + "Strangeness is" + STR$(stran * 10) + "%, "
text$ = text$ + "Symbols =" + STR$(symbo) + "."

IF pxl$ <> "" THEN text$ = text$ + " " + namx$ + " " + pxl$
GOSUB 19999












3330 ' Starport


GOSUB 14000
Starport$ = ""
IF stp$ = "A" THEN tbase = tbase - 1000: valmo = 1: traffic = traffic + 2: port = 5
IF stp$ = "B" THEN valmo = .95: port = 4
IF stp$ = "C" THEN tbase = tbase + 1000: valmo = .9: traffic = traffic - 1: port = 3
IF stp$ = "D" THEN tbase = tbase + 2000: valmo = .85: traffic = traffic - 2: port = 2
IF stp$ = "E" THEN tbase = tbase + 3000: valmo = .8: traffic = traffic - 3: port = 1
IF stp$ = "X" THEN tbase = tbase + 5000: valmo = .75: traffic = traffic - 4: GOTO 3360

pbudget = 500 * peas * (10 ^ pop * mul)
wbudget = 500 * wars * (10 ^ pop * mul)
valmo = valmo - ((15 - tec) * .05)
IF valmo < 0 THEN valmo = .1

'PRINT valmo; "valmo"
yard = wars * (10 ^ pop * mul) / 6666 * valmo / ((7 - STP) / 6): ' PRINT "yard"; stp; yard 7 - stp
IF tec < 7 AND yard > 600 THEN yard = 600
IF tec < 9 AND yard > 1000 THEN yard = 1000
IF tec < 10 AND yard > 4000 THEN yard = 4000
IF tec < 11 AND yard > 10000 THEN yard = 10000
IF tec < 12 AND yard > 50000 THEN yard = 50000
IF tec < 13 AND yard > 100000 THEN yard = 100000
IF tec < 14 AND yard > 1000000 THEN yard = 1E+6



IF yard > 500000 THEN yard = 500000
IF yard < 10 THEN yard = INT(yard): GOTO 123
IF yard < 100 THEN yard = INT(yard / 10) * 10: GOTO 123
IF yard < 1000 THEN yard = INT(yard / 100) * 100: GOTO 123
IF yard < 10000 THEN yard = INT(yard / 1000) * 1000: GOTO 123
IF yard < 100000 THEN yard = INT(yard / 10000) * 10000: GOTO 123
IF yard < 1000000 THEN yard = INT(yard / 100000) * 100000: GOTO 123
yard = INT(yard / 1000000!) * 1000000!
123 ' IF tec < 8 THEN yard = 0







1230 'Starport "A"
IF stp$ <> "A" GOTO 1240

IF yard < 1000 THEN yard = 1000
stpx$ = "Refined Fuel is available":
IF gas = 0 THEN stpx$ = stpx$ + ", be advised, this is the only source of fuel in this system": IF hyd > 0 THEN stpx$ = stpx$ + ". Scooping fuel from the planetary oceans is seen as a direct infringment of planetary law"
stpx$ = stpx$ + ". "
IF yard <= 1000 THEN stpx$ = stpx$ + "Starships up to" + STR$(yard) + "t can be constructed.":
IF yard > 1000 THEN stpx$ = stpx$ + "Starships up to" + STR$(yard / 1000) + "Kt can be constructed.":
stpx$ = stpx$ + " This port is one of the "
pass = 200
pass$ = "crossroads of the spaceways. Dozens"
IF tpz / 52 > pass * 10 THEN pass$ = "big crossroads of the spaceways. Hundreds"
IF tpz / 52 > pass * 100 THEN pass$ = "huge crossroads of the spaceways. Thousands"
IF tpz / 52 > pass * 1000 THEN pass$ = "massive crossroads of the spaceways. Tenthousands"
IF tpz / 52 > pass * 5000 THEN pass$ = "monumental crossroads of the spaceways. Hundredthousands"
stpx$ = stpx$ + pass$ + " of Travellers find a temporary home here, either waiting for an interstellar connection, a shipboard job, or just a working passage to Somewhere Else."
stpx$ = stpx$ + " The transient accommodations are more varied than at any smaller port, plenty of luxury hotel rooms, but also more cheap hostel space."
stpx$ = stpx$ + " The same is true of shopping; there are of course more high-end retailers, but also more, and more varied, goods at the budget shops."






1240 'Starport "B"
IF stp$ <> "B" GOTO 1250

IF yard < 600 THEN yard = 600
stpx$ = "Refined Fuel is available":
IF gas = 0 THEN stpx$ = stpx$ + ", be advised, this is the only source of fuel in this system": IF hyd > 0 THEN stpx$ = stpx$ + ". Scooping fuel from the planetary oceans is seen as a direct infringment of planetary law"
stpx$ = stpx$ + ". The port offers a full suite of services to the average small starship or passenger."
stpx$ = stpx$ + " A variety of shopping, overnight accommodations, entertainment, and food are available onsite."
stpx$ = stpx$ + " Full ship maintenance and cargohandling is offered, the shipyard can build non-starships of up to" + STR$(yard) + "t size and handle all but the largest structural repairs or rebuilds."






1250 'Starport "C"
IF stp$ <> "C" GOTO 1260

IF yard < 100 THEN yard = 100
pad = INT(traffic * 20): IF pad < 10 THEN pad = 10
stpx$ = "The port has" + STR$(pad) + " landing pads, most of them equipped with a proper berth"
IF press > .8 THEN stpx$ = stpx$ + " and one runway"
stpx$ = stpx$ + ". Its traffic control can handle multiple takeoffs and landings at once."
stpx$ = stpx$ + " Unrefined fuel and all conventional ship’s stores are available, refined fuel is also offerd, but tankage is limited."
stpx$ = stpx$ + " All minor repairs and maintenance of ships up to " + STR$(yard) + "t can be performed without undue delays."
stpx$ = stpx$ + " Handling of ordinary container cargo is efficient, though goods requiring very special handling will slow the routine."
stpx$ = stpx$ + " Modest but adequate shopping and restaurants, and usually some kind of entertainment, will be available for passengers and crews."




1260 'Starport "D"
IF stp$ <> "D" GOTO 1270

stpx$ = "The port has" + STR$(traffic * 8) + " surfaced landing pads, and a landing strip for small craft incapable of vertical takeoff and landing."
stpx$ = stpx$ + " A few of the landing areas feature covered hangars. Landing guidance is more sophisticated, and incorporates a minimal satellite network."
stpx$ = stpx$ + " Mechanical service is available, but limited to minor repairs, standard spare parts, and essential ship’s stores. Unrefined fuel is for sale."
stpx$ = stpx$ + " There is usually no more than one service crewman on duty at any time, so ship crews in a hurry may need to buy parts and perform their own labor."


1270 'Starport "E"
IF stp$ <> "E" GOTO 1280
stpx$ = "The port consists of spaces for several small starships to set down,"
stpx$ = stpx$ + " with one area large enough for a starship of up to 1,000 dtons, to put down without undue risk."
stpx$ = stpx$ + " The ports headquarters building is an office module from a modular cutter."
stpx$ = stpx$ + " The office operates the landing lights and approach beacon, handles the standard paperwork on transient vessels,"
stpx$ = stpx$ + " stores cargo of Imperial concern (mail, mostly), and provides a place for passengers to wait out of the rain or vacuum."
stpx$ = stpx$ + " There are no facilities for fueling or routine servicing of ships."
'stpx$ = stpx$ + " Some emergency parts and supplies are stocked."
'stpx$ = stpx$ + " The port has no surfaced or covered berths or hangars; sometimes it must temporarily shut down operations after a hard rain."
'stpx$ = stpx$ + " Local residents may lease longterm berthing space, but must prepare and maintain any improvements themselves."
'stpx$ = stpx$ + " There are no retail stores or food service, though the general manager (the person staffing the office) may share his coffee."
stpx$ = stpx$ + " Directions will be provided to the nearest habitation, along with advice on local customs, law levels, and anything else likely to keep the visitor out of trouble."
stpx$ = stpx$ + ""

1280 'Planetary Navy
Navy = wbudget * valmo / 666666: ' price Tigress per ton

IF yard = 0 OR stp$ = "D" OR stp$ = "E" OR stp$ = "X" OR Navy < 20 THEN Navy = 0
IF Navy < 10 THEN navy$ = " There is no planetary Navy.": GOTO 104

IF Navy > 500000 AND yard >= 500000 THEN flagship$ = "Dreadnought of 500.000 t": GOTO 102
IF Navy > 200000 AND yard >= 200000 THEN flagship$ = "Battleship of 200.000 t": GOTO 102
IF Navy > 100000 AND yard >= 100000 THEN flagship$ = "Light Battleship of 100.000 t": GOTO 102
IF Navy > 75000 AND yard >= 75000 THEN flagship$ = "Heavy Cruiser of 75.000 t": GOTO 102
IF Navy > 3000 AND yard >= 30000 THEN flagship$ = "Light Cruiser of 30.000 t": GOTO 102
IF Navy > 15000 AND yard >= 15000 THEN flagship$ = "Destroyer of 15.000 t": GOTO 102
IF Navy > 2000 AND yard >= 2000 THEN flagship$ = "FleetEscort of 2.000 t": GOTO 102
IF Navy > 1000 AND yard >= 1000 THEN flagship$ = "Corvette of 1000 t": GOTO 102
IF Navy > 600 AND yard >= 600 THEN flagship$ = "SDB of 600 t": GOTO 102
IF Navy > 400 AND yard >= 400 THEN flagship$ = "SDB of 400 t": GOTO 102
IF Navy > 200 AND yard >= 200 THEN flagship$ = "SDB of 200 t": GOTO 102
IF Navy > 50 AND yard > 50 THEN flagship$ = "Revenue Cutter of 50t": GOTO 102
IF Navy > 20 AND yard > 20 THEN flagship$ = "Fighter of 20t": GOTO 102
102

IF Navy > 10 THEN navy$ = navy$ + " The planetary Navy has a total tonnage of"

IF Navy > 1E+9 THEN navy$ = navy$ + STR$(INT(Navy / 1E+9 * 10) / 10) + " Billion tons": GOTO 103
IF Navy > 1E+6 THEN navy$ = navy$ + STR$(INT(Navy / 1E+6 * 10) / 10) + " Million Tons": GOTO 103
IF Navy > 1000 THEN navy$ = navy$ + STR$(INT(Navy / 1000 * 10) / 10) + " kilotons": GOTO 103
navy$ = navy$ + STR$(INT(Navy)) + " tons": GOTO 103

103
IF Navy > 0 THEN navy$ = navy$ + ", the flagship is a " + flagship$ + "."
104


' PRINT yard; Navy: GOSUB 1000

' PRINT #nnn, USING " EXCHANGE : #.## Credits"; 1 / valmo
' PRINT #nnn,
' PRINT #nnn, " MIL. BUDGET : ";: value = pbudget * valmo: GOSUB 13000: PRINT #nnn, "(Peace)   ";: value = wbudget * valmo: GOSUB 13000: PRINT #nnn, "(War)": PRINT #nnn,

' Starport part 2
PRINT #nnn,
PRINT #nnn,
' text$ = "STARPORT TYPE " + stp$: GOSUB 19999
PRINT #nnn, "STARPORT TYPE "; stp$: PRINT #nnn,
text$ = ""
'text$ = stpx$

vl = facility: GOSUB 16000
text$ = text$ + facility$ + " " + stpx$
Starport$ = text$
'text$ = Starport$



tpzm = INT(tpz / 1E+06 * 100) / 100
tvzm = INT(tvz / 1E+06 * 100) / 100
IF tpzm >= .5 THEN text$ = text$ + " Each year" + STR$(tpzm) + " Million passengers are handled"
IF tpzm < .5 THEN text$ = text$ + " Each year" + STR$(INT(tpz)) + " passengers are handled"
IF tvzm >= .5 THEN text$ = text$ + " and" + STR$(tvzm) + " Million tons of cargo shipped. "
IF tvzm < .5 THEN text$ = text$ + " and" + STR$(INT(tvz)) + " tons of cargo shipped. "


' text$ = text$ + vl$

text$ = text$ + traffic$
'
IF travel / 365 / 82 < 10 THEN text$ = text$ + " There are about" + STR$(CINT(tday)) + " " + trader$ + " arrivals each " + tday$ + ". "

GOSUB 19999: PRINT #nnn,

text$ = baseport$ + extrality$


' PRINT #nnn,
' PRINT #nnn, " PASSENGERS  : ca. ";: value = tpz / 365: GOSUB 13000: PRINT #nnn, " per day";
ATL = INT(tpz / 278000 * 100) / 100
sqm = INT(SQR(ATL * 63) / 5) * 5: IF sqm < .5 THEN sqm = .5
sqm$ = STR$(sqm)
' NT #nnn, ATL; "% ATL. The spaceport measures"; sqm; "by"; sqm; "km."
text$ = text$ + " The whole port measures" + sqm$ + " by" + sqm$ + " km."
IF bax$ <> "" THEN text$ = text$ + " " + bax$
IF navy$ <> "" THEN text$ = text$ + navy$


GOSUB 19999
PRINT #nnn,
text$ = ""

GOSUB 14200 'traffic control
GOSUB 14400 'cargoship


IF admin$ <> "" THEN text$ = admin$: GOSUB 19999: PRINT #nnn,


IF minor > 0 THEN PRINT #nnn,: PRINT #nnn, "DAILY freight and passenger amounts:": PRINT #nnn,
IF major > 0 THEN text$ = "MAIN ROUTE:" + major$: GOSUB 19999: PRINT #nnn,
IF feeder > 0 THEN text$ = "FEEDER ROUTE:" + feeder$: GOSUB 19999: PRINT #nnn,
IF minor > 0 THEN text$ = "MINOR ROUTE:" + minor$: GOSUB 19999: PRINT #nnn,
vl = traffic: GOSUB 16000



3360 ' Resources

GOSUB 9900
' agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
' parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 1: artfo = artfo + 0: softw = softw + 1: docum = docum + 2
GOSUB 8000
PRINT #nnn,





3381 ' Tech Level output
'TECH LEVEL TEXT
IF tec < 1 THEN GOTO 3310
GOSUB 35000
PRINT #nnn,
PRINT #nnn, "TECH LEVEL ("; tec; ") :"
PRINT #nnn,

text$ = SciTl$: GOSUB 19999: PRINT #nnn,
text$ = MatTL$: GOSUB 19999: PRINT #nnn,
text$ = EneTl$: GOSUB 19999: PRINT #nnn,

IF COMRO$ <> "" THEN text$ = COMRO$: GOSUB 19999: PRINT #nnn,
IF COMTL$ <> "" THEN text$ = COMTL$: GOSUB 19999: PRINT #nnn,

text$ = Trans$: GOSUB 19999: PRINT #nnn,

text$ = WeaTl$: GOSUB 19999: PRINT #nnn,
IF hmtl$ <> "" THEN text$ = hmtl$: GOSUB 19999: PRINT #nnn,

text$ = BioTL$: GOSUB 19999: PRINT #nnn,
text$ = MedTL$: GOSUB 19999: PRINT #nnn,
text$ = EnvTL$: GOSUB 19999: PRINT #nnn,

3310 ' (2) Worlddata


GOSUB 3390: ' Seismic

IF datx$ <> "" THEN PRINT #nnn, datx$



' PRINT text$: END

IF atm = 0 THEN GOTO 7777


PRINT #nnn,
PRINT #nnn, "TEMPERATURES PER HEXROW (HR) in °C:"
PRINT #nnn,


tmult = 24 / rota

IF siz <> 0 AND atm <> 0 THEN PRINT #nnn, USING " Year length        :#####.## Terra =##### planetary days"; period; period * 365.25 * tmult
PRINT #nnn,
' PRINT #nnn, "ÚÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
PRINT #nnn, USING " HR  Spring ######   Summer######   Autumn######   Winter######  "; jz; jz; jz; jz
' PRINT #nnn, "ÃÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄ´";
' print #nnn, "ÃÄÄÄÄÒÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÒÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÒÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÒÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄ´"
FOR i = 1 TO 11
    latmod = lat(i, 11 - siz): PRINT #nnn,
    summer = .6 * axtilt: winter = -1 * axtilt

    perz = 0
    IF i >= S00 THEN perz = 1: GOTO 3700
    IF i >= S75 THEN perz = .75: GOTO 3700
    IF i >= S50 THEN perz = .5: GOTO 3700
    IF i >= S25 THEN perz = .25
    3700

    sutemp = latmod + bse + summer * perz
    witemp = latmod + bse + winter * perz

    day = rota / 2 * inc: IF day > pluslim THEN day = pluslim
    night = rota / 2 * dec: IF night > minlim THEN night = minlim

    IF witemp - night < -273 THEN witemp = -273 + night


    PRINT #nnn, USING " ## ####°####°####°"; i; latmod + bse + day; latmod + bse; latmod + bse - night;
    PRINT #nnn, USING "####°####°####°"; sutemp + day; sutemp; sutemp - night;
    PRINT #nnn, USING "####°####°####°"; latmod + bse + day; latmod + bse; latmod + bse - night;
    PRINT #nnn, USING "####°####°####°"; witemp + day; witemp; witemp - night
    '     IF I < 11 THEN PRINT #nnn, "ÃÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄ´";
NEXT
' PRINT #nnn, "ÀÄÄÄÄÁÄÄÂÄÄÁÄÄÂÄÄÁÄÄÂÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÙ"
PRINT #nnn, "       :    :    :"
PRINT #nnn, USING "       :    :    :      Length of day         ###:##"; INT(rota); (rota - INT(rota)) * 60
PRINT #nnn, "       :    :    :"
PRINT #nnn, USING "       :    :    L......Deepest Night temp at  ##:00"; 5 / 24 * rota
PRINT #nnn, USING "       :    L............Medium values at       ##:00 and ##:00"; 10 / 24 * rota; 20 / 24 * rota
PRINT #nnn, USING "       L..................Highest Noon temp at   ##:00"; 15 / 24 * rota


' GOTO 101


' GOSUB 3370: text$ = resource$: IF pop > 0 THEN GOSUb 19999

7777 CLS:
PRINT "The world has been saved with the following filename:"
PRINT
PRINT output$: PRINT
CLOSE #1
'NEXT welt
OPEN "Worldlist.txt" FOR OUTPUT AS #1
FOR i = 1 TO XXC: PRINT #1, result$(i): NEXT
CLOSE #1
RUN 'END






3370
resource$ = "Warenangebot: "
axa = INT(RND * 6) + 1 + INT(RND * 6) + 1
IF agric >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "Getreide, Frchte " + val$(agric) + ", ": GOSUB 3371
IF ores >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "Roherze " + val$(ores) + ", ": GOSUB 3371
IF radio >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "radioaktive Stoffe " + val$(radio) + ", ": GOSUB 3371
IF cryst >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "Kristalle " + val$(cryst) + ", ": GOSUB 3371
IF agrop >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "verarbeitete Nahrungsmittel " + val$(agrop) + ", ": GOSUB 3371
IF metal >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "legierte Metalle, Stahl " + val$(metal) + ", ": GOSUB 3371
IF nonme >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "Kunststoffprodukte " + val$(nonme) + ", ": GOSUB 3371
IF parts >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "fertige Einzelteile " + val$(parts) + ", ": GOSUB 3371
IF durab >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "fertige Dauerwaren " + val$(durab) + ", ": GOSUB 3371
IF consu >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "fertige Konsumgter " + val$(consu) + ", ": GOSUB 3371
IF weapo >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "Waffen " + val$(weapo) + ", ": GOSUB 3371
IF recor >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "Bild- und Tonaufnahmen " + val$(recor) + ", ": GOSUB 3371
IF artfo >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "Kunstgegenst„nde " + val$(artfo) + ", ": GOSUB 3371
IF softw >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "Software " + val$(softw) + ", ": GOSUB 3371
IF docum >= INT(RND * 6) + 1 + INT(RND * 6) + 1 THEN res$ = "Dokumente " + val$(docum) + ", ": GOSUB 3371
resource$ = LEFT$(resource$, LEN(resource$) - 2) + "."
RETURN
3371 resource$ = resource$ + res$: RETURN

' Tech Level

PRINT #nnn,
PRINT #nnn, USING " Sci##"; SciTL * gtm;
PRINT #nnn, USING ", Mat##"; MatTL * gtm;
PRINT #nnn, USING ", Eng##"; EneTL * gtm;
PRINT #nnn, USING ", Inf##"; InfTL * gtm;
PRINT #nnn, USING ", Tra##"; TraTL * gtm;
PRINT #nnn, USING ", Wea##"; WeaTL * gtm;
PRINT #nnn, USING " - Bio##"; BioTL * gtm;
PRINT #nnn, USING ", Med##"; MedTL * gtm;
PRINT #nnn, USING ", Env##"; EnvTL * gtm
PRINT #nnn,



RETURN



4000 'Solar
'CLS

RESTORE 4000
FOR i = 0 TO 9
    FOR j = 1 TO 5
        READ mass(i * 5, j)

    NEXT
    READ q$
    FOR j = 1 TO 5
        READ lumi(i * 5, j)
    NEXT
    READ q$
NEXT

DATA 14,12,6,3.2,0.36,'A0 Mass
DATA 6.85,4.09,4.53,3.08,0.27,'A0 Lum

DATA 11,9,4,2.1,0.39,'A5 Mass
DATA 5.4,3.08,2.47,2,0.20,'A5 Lum

DATA 10,8,2.5,1.7,0.42,'F0 Mass
DATA 4.95,2.7,2.09,1.69,0.13,'F0 Lum

DATA 8.1,5,2,1.3,0.51,'F5 Mass
DATA 4.75,2.56,1.86,1.37,0.11,'F5 Lum

DATA 8.1,2.5,1.75,1.04,0.63,'G0 Mass
DATA 4.86,2.66,1.6,1.05,0.09,'G0 Lum

DATA 10,3.2,2,.94,0.73,'G5 Mass
DATA 5.22,2.94,1.49,.9,0.09,'G5 Lum

DATA 11,4,2.3,.825,.83,'K0 Mass
DATA 5.46,3.12,1.47,.81,.08,'K0 Lum

DATA 14,5,2.6,.57,.97,'K5 Mass
DATA 7.04,4.23,1.45,.53,.08,'K5 Lum

DATA 16,6.3,0,.489,1.11,'M0 Mass
DATA 8.24,4.65,0,.45,.07,'M0 Lum

DATA 16,7.4,0,.331,1.20,'M5 Mass
DATA 11.05,6.91,0,.29,0.05,'M5 Lum


DATA .2,.4,.7,1,1.6,2.8,5.2,10,19.6,38.8,77.2,154,307.6,614.8,1229.2,2548,4915,9830.8,19661.2,39322
DATA 836.345,591.385,447.045,374.025,295.693,223.523,164.021,118.277,84.484,60.046,42.569,30.140,21.326,15.085,10.668,7.554,5.335,3.772,2.667,1.886
FOR i = 0 TO 19: READ au(i): NEXT
FOR i = 0 TO 19: READ orf(i): NEXT
RETURN
5000



RESTORE 5000
FOR i = 0 TO 10
    FOR j = 1 TO 4
        READ nhz(i, j), hz(i, j)
        nhz(i, j) = nhz(i, j) / 1000:
        hz(i, j) = hz(i, j) / 1000
    NEXT
NEXT

DATA 800,900,800,900,680,740,800,900
DATA 744,829,811,900,646,697,811,900
DATA 736,803,789,860,635,672,807,882
DATA 752,811,799,860,644,676,817,883
DATA 738,782,774,820,625,648,813,866
DATA 753,789,747,780,599,613,809,850
DATA 767,795,718,740,570,577,805,836
DATA 782,802,687,700,537,539,800,821
DATA 796,808,654,660,500,500,794,807
DATA 810,814,619,620,500,500,787,793
DATA 818,818,619,619,500,500,773,773
RETURN

3390 ' seismic

RETURN

'  024h025C025ø







9000 ' Trade & Commerce
modi = 0
bsp = 5000
IF agr0 = 1 AND agr = 1 THEN modi = modi + 1
IF agr0 = 1 AND ast = 1 THEN modi = modi + 1
IF agr0 = 1 AND des = 1 THEN modi = modi + 1
IF agr0 = 1 AND hig = 1 THEN modi = modi + 1
IF agr0 = 1 AND ind = 1 THEN modi = modi + 1
IF agr0 = 1 AND low = 1 THEN modi = modi + 1
IF agr0 = 1 AND nag = 1 THEN modi = modi + 1
IF agr0 = 1 AND ric = 1 THEN modi = modi + 1

IF ast0 = 1 AND ast = 1 THEN modi = modi + 1
IF ast0 = 1 AND ind = 1 THEN modi = modi + 1
IF ast0 = 1 AND nag = 1 THEN modi = modi + 1
IF ast0 = 1 AND ric = 1 THEN modi = modi + 1
IF ast0 = 1 AND vac = 1 THEN modi = modi + 1

IF bar0 = 1 AND agr = 1 THEN modi = modi + 1
IF bar0 = 1 AND ind = 1 THEN modi = modi + 1

IF des0 = 1 AND des = 1 THEN modi = modi + 1
IF des0 = 1 AND nag = 1 THEN modi = modi + 1

IF flu0 = 1 AND flu = 1 THEN modi = modi + 1
IF flu0 = 1 AND ind = 1 THEN modi = modi + 1

IF hig0 = 1 AND hig = 1 THEN modi = modi + 1
IF hig0 = 1 AND low = 1 THEN modi = modi + 1
IF hig0 = 1 AND ric = 1 THEN modi = modi + 1

IF ice0 = 1 AND ind = 1 THEN modi = modi + 1

IF ind0 = 1 AND agr = 1 THEN modi = modi + 1
IF ind0 = 1 AND ast = 1 THEN modi = modi + 1
IF ind0 = 1 AND des = 1 THEN modi = modi + 1
IF ind0 = 1 AND flu = 1 THEN modi = modi + 1
IF ind0 = 1 AND hig = 1 THEN modi = modi + 1
IF ind0 = 1 AND ind = 1 THEN modi = modi + 1
IF ind0 = 1 AND nin = 1 THEN modi = modi + 1
IF ind0 = 1 AND por = 1 THEN modi = modi + 1
IF ind0 = 1 AND ric = 1 THEN modi = modi + 1
IF ind0 = 1 AND vac = 1 THEN modi = modi + 1
IF ind0 = 1 AND wat = 1 THEN modi = modi + 1

IF low0 = 1 AND ind = 1 THEN modi = modi + 1
IF low0 = 1 AND ric = 1 THEN modi = modi + 1

IF nag0 = 1 AND ast = 1 THEN modi = modi + 1
IF nag0 = 1 AND des = 1 THEN modi = modi + 1
IF nag0 = 1 AND vac = 1 THEN modi = modi + 1

IF nin0 = 1 AND ind = 1 THEN modi = modi + 1
IF nin0 = 1 AND nin = 1 THEN modi = modi - 1

IF por0 = 1 AND por = 1 THEN modi = modi - 1

IF ric0 = 1 AND agr = 1 THEN modi = modi + 1
IF ric0 = 1 AND des = 1 THEN modi = modi + 1
IF ric0 = 1 AND hig = 1 THEN modi = modi + 1
IF ric0 = 1 AND ind = 1 THEN modi = modi + 1
IF ric0 = 1 AND nag = 1 THEN modi = modi + 1
IF ric0 = 1 AND ric = 1 THEN modi = modi + 1

IF vac0 = 1 AND ast = 1 THEN modi = modi + 1
IF vac0 = 1 AND ind = 1 THEN modi = modi + 1
IF vac0 = 1 AND vac = 1 THEN modi = modi + 1

IF wat0 = 1 AND ind = 1 THEN modi = modi + 1
IF wat0 = 1 AND ric = 1 THEN modi = modi + 1
IF wat0 = 1 AND wat = 1 THEN modi = modi + 1




bsp = 5000 + modi * 1000
bsp = bsp + bsp * ((tec0 - tec) * .1)
RETURN














9100 ' Gurps Far Trader

IF tec < 2 THEN TLM = -.5: GOTO 9101
IF tec < 6 THEN TLM = 0: GOTO 9101
IF tec < 9 THEN TLM = .5: GOTO 9101
IF tec < 12 THEN TLM = 1: GOTO 9101
IF tec < 16 THEN TLM = 1.5: GOTO 9101
IF tec > 15 THEN TLM = 2: GOTO 9101

9101 uwtn = TLM + (pop + mul / 10) / 2

IF uwtn > 6.5 AND stp$ = "A" THEN pm = 0: GOTO 9102
IF uwtn > 6.5 AND stp$ = "B" THEN pm = -1: GOTO 9102
IF uwtn > 6.5 AND stp$ = "C" THEN pm = -1.5: GOTO 9102
IF uwtn > 6.5 AND stp$ = "D" THEN pm = -2: GOTO 9102
IF uwtn > 6.5 AND stp$ = "E" THEN pm = -2.5: GOTO 9102
IF uwtn > 6.5 AND stp$ = "X" THEN pm = -5: GOTO 9102

IF uwtn > 5.5 AND stp$ = "A" THEN pm = 0: GOTO 9102
IF uwtn > 5.5 AND stp$ = "B" THEN pm = -.5: GOTO 9102
IF uwtn > 5.5 AND stp$ = "C" THEN pm = -1: GOTO 9102
IF uwtn > 5.5 AND stp$ = "D" THEN pm = -1.5: GOTO 9102
IF uwtn > 5.5 AND stp$ = "E" THEN pm = -2: GOTO 9102
IF uwtn > 5.5 AND stp$ = "X" THEN pm = -4.5: GOTO 9102

IF uwtn > 4.5 AND stp$ = "A" THEN pm = 0: GOTO 9102
IF uwtn > 4.5 AND stp$ = "B" THEN pm = 0: GOTO 9102
IF uwtn > 4.5 AND stp$ = "C" THEN pm = -.5: GOTO 9102
IF uwtn > 4.5 AND stp$ = "D" THEN pm = -1: GOTO 9102
IF uwtn > 4.5 AND stp$ = "E" THEN pm = -1.5: GOTO 9102
IF uwtn > 4.5 AND stp$ = "X" THEN pm = -3.5: GOTO 9102

IF uwtn > 3.5 AND stp$ = "A" THEN pm = .5: GOTO 9102
IF uwtn > 3.5 AND stp$ = "B" THEN pm = 0: GOTO 9102
IF uwtn > 3.5 AND stp$ = "C" THEN pm = 0: GOTO 9102
IF uwtn > 3.5 AND stp$ = "D" THEN pm = -.5: GOTO 9102
IF uwtn > 3.5 AND stp$ = "E" THEN pm = -1: GOTO 9102
IF uwtn > 3.5 AND stp$ = "X" THEN pm = -2.5: GOTO 9102

IF uwtn > 2.5 AND stp$ = "A" THEN pm = .5: GOTO 9102
IF uwtn > 2.5 AND stp$ = "B" THEN pm = .5: GOTO 9102
IF uwtn > 2.5 AND stp$ = "C" THEN pm = 0: GOTO 9102
IF uwtn > 2.5 AND stp$ = "D" THEN pm = 0: GOTO 9102
IF uwtn > 2.5 AND stp$ = "E" THEN pm = -.5: GOTO 9102
IF uwtn > 2.5 AND stp$ = "X" THEN pm = -3: GOTO 9102

IF uwtn > 1.5 AND stp$ = "A" THEN pm = 1: GOTO 9102
IF uwtn > 1.5 AND stp$ = "B" THEN pm = .5: GOTO 9102
IF uwtn > 1.5 AND stp$ = "C" THEN pm = .5: GOTO 9102
IF uwtn > 1.5 AND stp$ = "D" THEN pm = 0: GOTO 9102
IF uwtn > 1.5 AND stp$ = "E" THEN pm = 0: GOTO 9102
IF uwtn > 1.5 AND stp$ = "X" THEN pm = -2.5: GOTO 9102

IF uwtn > .5 AND stp$ = "A" THEN pm = 1: GOTO 9102
IF uwtn > .5 AND stp$ = "B" THEN pm = 1: GOTO 9102
IF uwtn > .5 AND stp$ = "C" THEN pm = .5: GOTO 9102
IF uwtn > .5 AND stp$ = "D" THEN pm = .5: GOTO 9102
IF uwtn > .5 AND stp$ = "E" THEN pm = 0: GOTO 9102
IF uwtn > .5 AND stp$ = "X" THEN pm = 0: GOTO 9102



IF stp$ = "A" THEN pm = 1.5: GOTO 9102
IF stp$ = "B" THEN pm = 1: GOTO 9102
IF stp$ = "C" THEN pm = 1: GOTO 9102
IF stp$ = "D" THEN pm = .5: GOTO 9102
IF stp$ = "E" THEN pm = .5: GOTO 9102
IF stp$ = "X" THEN pm = 0: GOTO 9102

9102
wtn = uwtn + pm
btn = wtn
RETURN







9900 ' Resources

agric = 0: ores = 0: radio = 0: cryst = 0: compo = 0: agrop = 0: metal = 0: nonme = 0: parts = 0: durab = 0: consu = 0: weapo = 0: recor = 0: artfo = 0: softw = 0: docum = 0


IF hvycor = 1 THEN agric = agric + 1: ores = ores + 8: radio = radio + 7: cryst = cryst + 6: compo = compo + 5: agrop = agrop + 0: metal = metal + 2: nonme = nonme + 1
IF hvycor = 1 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF moltcor = 1 THEN agric = agric + 4: ores = ores + 7: radio = radio + 5: cryst = cryst + 5: compo = compo + 6: agrop = agrop + 1: metal = metal + 0: nonme = nonme + 0
IF moltcor = 1 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF rockbod = 1 THEN agric = agric + 4: ores = ores + 3: radio = radio + 3: cryst = cryst + 2: compo = compo + 1: agrop = agrop + 1: metal = metal + 0: nonme = nonme + 0
IF rockbod = 1 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF icebod = 1 THEN agric = agric - 4: ores = ores + 7: radio = radio + 5: cryst = cryst + 5: compo = compo + 6: agrop = agrop + 1: metal = metal + 0: nonme = nonme + 0
IF icebod = 1 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF atm > 3 AND atm < 10 THEN agric = agric + 1: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 2: metal = metal + 0: nonme = nonme + 1
IF atm > 3 AND atm < 10 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + -1

IF atm < 4 OR atm > 9 THEN agric = agric - 3: ores = ores + 1: radio = radio + 1: cryst = cryst + 0: compo = compo + 1: agrop = agrop + 0: metal = metal + 1: nonme = nonme + 1
IF atm < 4 OR atm > 9 THEN parts = parts + 1: durab = durab + 1: consu = consu + 1: weapo = weapo + 1: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + -1

IF pop < 5 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 1: metal = metal - 1: nonme = nonme + 0
IF pop < 5 THEN parts = parts - 1: durab = durab - 1: consu = consu - 1: weapo = weapo - 1: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + -1

IF pop > 4 AND pop < 9 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 2: metal = metal + 1: nonme = nonme + 1
IF pop > 4 AND pop < 9 THEN parts = parts + 1: durab = durab + 2: consu = consu + 1: weapo = weapo + 0: recor = recor + 1: artfo = artfo + 2: softw = softw + 1: docum = docum + 0

IF pop > 9 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 2: metal = metal + 1: nonme = nonme + 1
IF pop > 9 THEN parts = parts + 2: durab = durab + 3: consu = consu + 4: weapo = weapo + 1: recor = recor + 2: artfo = artfo + 3: softw = softw + 4: docum = docum + 1

IF gov < 2 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF gov < 2 THEN parts = parts + -1: durab = durab + -1: consu = consu + -1: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF gov > 1 AND gov < 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF gov > 1 AND gov < 7 THEN parts = parts + 1: durab = durab + 1: consu = consu + 1: weapo = weapo + 1: recor = recor + 1: artfo = artfo + 1: softw = softw + 1: docum = docum + 1

IF gov = 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF gov = 7 THEN parts = parts + 2: durab = durab + 2: consu = consu + 2: weapo = weapo + 3: recor = recor + 1: artfo = artfo + 2: softw = softw + 1: docum = docum + 0

IF gov > 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF gov > 7 THEN parts = parts + 0: durab = durab + 0: consu = consu + 1: weapo = weapo + 1: recor = recor + 2: artfo = artfo + 0: softw = softw + 1: docum = docum + 4

IF law < 3 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF law < 3 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF law > 2 AND law < 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF law > 2 AND law < 7 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 1: artfo = artfo + 0: softw = softw + 1: docum = docum + 2

IF law > 6 AND law < 10 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF law > 6 AND law < 10 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 2: artfo = artfo + 0: softw = softw + 2: docum = docum + 4

IF law > 9 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF law > 9 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 3: artfo = artfo + 0: softw = softw + 3: docum = docum + 6

IF tec < 4 THEN agric = agric + 1: ores = ores + 1: radio = radio + 1: cryst = cryst + 1: compo = compo + 1: agrop = agrop + 1: metal = metal + -1: nonme = nonme + 0
IF tec < 4 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + -3: artfo = artfo + 2: softw = softw - 9: docum = docum + 0

IF tec > 3 AND tec < 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 2: metal = metal + 2: nonme = nonme + 2
IF tec > 3 AND tec < 7 THEN parts = parts + 0: durab = durab + 1: consu = consu + 1: weapo = weapo + 1: recor = recor + 1: artfo = artfo + 1: softw = softw + 0: docum = docum + 1

IF tec > 6 AND tec < 12 THEN agric = agric - 1: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 1: metal = metal + 4: nonme = nonme + 4
IF tec > 6 AND tec < 12 THEN parts = parts + 2: durab = durab + 2: consu = consu + 2: weapo = weapo + 1: recor = recor + 2: artfo = artfo + 1: softw = softw + 1: docum = docum + 3

IF tec > 11 THEN agric = agric - 2: ores = ores + 1: radio = radio + 1: cryst = cryst + 1: compo = compo + 1: agrop = agrop + 1: metal = metal + 5: nonme = nonme + 6
IF tec > 11 THEN parts = parts + 4: durab = durab + 3: consu = consu + 4: weapo = weapo + 2: recor = recor + 3: artfo = artfo + 1: softw = softw + 4: docum = docum + 1

IF life = 1 THEN agric = agric + 5: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 1: agrop = agrop + 5: metal = metal + 0: nonme = nonme + 3
IF life = 1 THEN parts = parts + 1: durab = durab + 1: consu = consu + 1: weapo = weapo + 1: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF life = 0 THEN compo = compo - 1

IF bse < -5 THEN agric = agric - 5: agrop = agrop - 5


RETURN

9800 ' H”henwerte
IF press > 1 THEN height = (press - 1) * -2500
IF press > .95 AND press < 1 THEN height = 0
IF press < .95 AND press >= .5 THEN height = 1000 + (.95 - press) * 10000
IF press < .5 AND press > .2 THEN height = 5500 + (.5 - press) * 22000
IF press <= .2 THEN height = 12000 + (.2 - press) * 50000
height = INT(height)

RETURN

7100
tep$ = "unbearably hot (Vacc Suit mandatory)"
IF bsx < 80 THEN tep$ = "extremely hot"
IF bsx < 60 THEN tep$ = "very hot"
IF bsx < 45 THEN tep$ = "hot"
IF bsx < 30 THEN tep$ = "warm"
IF bsx < 20 THEN tep$ = "cool"
IF bsx < 10 THEN tep$ = "chilly"
IF bsx < 5 THEN tep$ = "cold"
IF bsx < -5 THEN tep$ = "very cold"
IF bsx < -15 THEN tep$ = "very, very cold"
IF bsx < -30 THEN tep$ = "extremely cold"
IF bsx < -50 THEN tep$ = "unbearably cold (Vacc Suit mandatory)"

RETURN
 
8000
x4 = 5: nz = 21

text$ = ""
FOR axa = 16 TO 1 STEP -1: cco = 0: vx$ = ""

    val1 = axa * (traffic / 8)
    IF val1 < 2 THEN val1 = 2: GOTO 8001
    IF val1 > 13 THEN val1 = 13
    vx$ = val$(val1) + " of "

    value = agric: value$ = "Natural Agricultural Resources"
    value$ = "Wood, Meat, Spices, Fruit, "
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = agrop: value$ = "Processed Agroproducts"
    value$ = "Liquor, Grain, Canned food, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = ores: value$ = "Natural Ores, "
    value$ = "Copper, Tin, Silver, Aluminium, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = radio: value$ = "Radioactives, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = cryst: value$ = "Crystals, Gems, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = metal: value$ = "Processed Metals, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value$ = "Steel, Special Alloys, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = nonme: value$ = "Processed Non-Metals":
    value$ = "Textiles, Polymers, Pharmaceuticals, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = parts: value$ = "Manufactured parts":
    value$ = "Tools, mechanical parts, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = durab: value$ = "Manufactured Durables"
    value$ = "Aircraft, ATV, Machine tools, Farm machinery, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = consu: value$ = "Manufactured Consumables"
    value$ = "Clothing, Computers, Entertainment, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = weapo: value$ = "Weapons, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = recor: value$ = "Recordings, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = artfo: value$ = "Artforms, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = softw: value$ = "Software, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    value = docum: value$ = "Documents, ":
    IF value = axa THEN vx$ = vx$ + value$: cco = 1

    IF cco = 1 THEN vx$ = LEFT$(vx$, LEN(vx$) - 2): vx$ = vx$ + ". "

    IF cco = 1 THEN text$ = text$ + vx$

8001 NEXT

IF text$ <> "" THEN text$ = "Available trade goods : " + text$: GOSUB 19999: PRINT #nnn,
RETURN
8100
IF value <> axa THEN RETURN

value = value * (traffic / 8)
IF value < 2 THEN RETURN
IF value > 12 THEN value = 12
vx$ = val$(value) + " of " + value$


PRINT #nnn, " "; vx$; SPC(67 - LEN(vx$));
PRINT #nnn, USING "+## "; 10.5 - (nz - value * 1.5): cco = cco + 1
RETURN










19999 ' Druckausgabe
FOR i2 = 0 TO 10: text$(i2) = "": NEXT

tabu0 = 1000: tabu = tabu0: tx = 0:
text$(0) = text$: IF LEN(text$(0)) <= tabu THEN GOTO 10004

10001 IF tabu > 1 AND MID$(text$(tx), tabu, 1) <> " " THEN tabu = tabu - 1: GOTO 10001
leng = LEN(text$(tx)): l1 = leng - tabu
text$(tx + 1) = RIGHT$(text$(tx), l1)
text$(tx) = LEFT$(text$(tx), tabu)
tx = tx + 1
tabu = tabu0
IF LEN(text$(tx)) > tabu THEN GOTO 10001
' IF tx < 2 THEN GOTO 10004

' GOTO 10004
10002 FOR i2 = 0 TO tx - 1: space0 = tabu0 - LEN(text$(i2)): work1$ = ""

    10003 work$ = text$(i2): work1$ = "":
    FOR zz = 1 TO LEN(work$): space0$ = MID$(work$, zz, 1)
        IF space0$ = " " AND space0 > 0 THEN space0$ = "  ": space0 = space0 - 1
        work1$ = work1$ + space0$
    NEXT
    text$(i2) = work1$
    IF space0 > 0 AND tx <> i2 THEN GOTO 10003

NEXT

10004 FOR i2 = 0 TO tx: IF i2 = 0 THEN PRINT #nnn, " ";
    PRINT #nnn, text$(i2)
NEXT
RETURN
10005
IF LEFT$(text$, 1) = " " THEN text$ = RIGHT$(text$, LEN(text$) - 1): GOTO 10005
RETURN















12000 ' System - Placing of planetary Orbit

FOR z = 0 TO 19 STEP .5
    orfa = orf(INT(z))
    IF INT(z) <> z THEN orfa = (orfa + orf(INT(z) + 1)) / 2
    temp = lumi * orfa * hz(hyd, atr) * ge - 273
    IF LEFT$(temp$, 7) = "Cryonic" THEN temp = -200 - RND * 73
    IF LEFT$(temp$, 5) = "Frozen" THEN temp = -50 - RND * 150
    IF LEFT$(temp$, 4) = "Cold" THEN temp = -50 * RND
    IF LEFT$(temp$, 3) = "Hot" THEN temp = 30 + 50 * RND
    IF LEFT$(temp$, 7) = "Boiling" THEN temp = 80 + 120 * RND
    IF LEFT$(temp$, 6) = "Torrid" THEN temp = 200 + 500 * RND
    IF temp > minte AND temp < maxte AND bse = 0 THEN tempzone = z: bse = CINT(temp): txx = temp
NEXT: RETURN

13000 ' Mio Ausgabe NNN
IF value < 100 THEN PRINT #nnn, USING "##"; value;: RETURN
IF value < 1000 THEN PRINT #nnn, USING "###"; value;: RETURN

IF value < 10000 THEN PRINT #nnn, USING "####,"; value;: RETURN

IF value < 100000 THEN PRINT #nnn, USING "#####,"; value;: RETURN
IF value < 1000000! THEN PRINT #nnn, USING "######,"; value;: RETURN
IF value < 1E+09 THEN PRINT #nnn, USING "###.## Mio"; value / 1000000!;: RETURN
IF value < 1E+12 THEN PRINT #nnn, USING "###.## Mia"; value / 1E+09;: RETURN
IF value < 1E+15 THEN PRINT #nnn, USING "###.## Bio"; value / 1E+12;: RETURN
PRINT #nnn, USING "###,.## Bia"; value / 1E+15;: RETURN

13001 ' Mio Ausgabe
IF value < 100 THEN PRINT USING "##"; value;: RETURN
IF value < 1000 THEN PRINT USING "###"; value;: RETURN

IF value < 10000 THEN PRINT USING "####,"; value;: RETURN

IF value < 100000 THEN PRINT USING "#####,"; value;: RETURN
IF value < 1000000! THEN PRINT USING "######,"; value;: RETURN
IF value < 1E+09 THEN PRINT USING "###.## Mio"; value / 1000000!;: RETURN
IF value < 1E+12 THEN PRINT USING "###.## Mia"; value / 1E+09;: RETURN
IF value < 1E+15 THEN PRINT USING "###.## Bio"; value / 1E+12;: RETURN
PRINT USING "###,.## Bia"; value / 1E+15;: RETURN













































14000 ' Starport details
baseport$ = "The port "
sens = 0
IF cr = 0 THEN extrality$ = "is secured with a single fence. The gate is always open."
IF cr = 1 THEN extrality$ = "is secured by a single fence with a gatehouse."
IF cr = 2 THEN extrality$ = "has a single fence and a cleared buffer zone with patrolling guards."
IF cr = 3 THEN extrality$ = "has an Anti-intrusion fence and a cleared buffer zone with patrolling guards, the patrols check it hourly."
IF cr = 4 THEN extrality$ = "has a double fence, with motion sensors and a cleared buffer zone with patrolling guards, they are patrolling 24/7.": sens = 1
IF cr = 5 THEN extrality$ = "has a double fence, with motion sensors, and a cleared buffer zone with nonlethal traps. Guards are patrolling 24/7.": sens = 1
IF cr = 6 THEN extrality$ = "has a triple fence with motion sensors, and a cleared buffer zone with lethal traps installed between all three fences. Guards are patrolling 24/7. Everyone will be searched on leaving. And BTW they have the air covered, too...": sens = 1
IF sens = 0 GOTO 54
IF tec < 5 THEN extrality$ = extrality$ + ""
IF tec = 5 THEN extrality$ = extrality$ + " Additional security measures: Searchlight."
IF tec = 6 THEN extrality$ = extrality$ + " Additional security measures, Radar, metal detector."
IF tec = 7 THEN extrality$ = extrality$ + " Additional security measures, Radar, LLTV."
IF tec = 8 THEN extrality$ = extrality$ + " Additional security measures, PESA AESA, Chem."
IF tec = 9 OR tec = 10 THEN extrality$ = extrality$ + " Additional security measures, PESA AESA, Chem, Radscanner."
IF tec = 11 OR tec = 12 THEN extrality$ = extrality$ + " Additional security measures, PESA AESA, Chem, Radscanner, Densitometer, Biosniffer."
IF tec > 12 THEN extrality$ = extrality$ + " Additional security measures, PESA AESA, Chem, Radscanner, Densitometer, Biosniffer, Neural sensor."
54

IF traffic < 0 THEN travel = 100
IF traffic = 0 THEN travel = 1000
IF traffic = 1 THEN travel = 10000
IF traffic = 2 THEN travel = 50000
IF traffic = 3 THEN travel = 100000
IF traffic = 4 THEN travel = 500000
IF traffic = 5 THEN travel = 1000000
IF traffic = 6 THEN travel = 2500000
IF traffic = 7 THEN travel = 5000000
IF traffic = 8 THEN travel = 7500000
IF traffic = 9 THEN travel = 1E+07
IF traffic = 10 THEN travel = 5E+07
IF traffic = 11 THEN travel = 1E+08
IF traffic = 12 THEN travel = 5E+08
IF traffic = 13 THEN travel = 1E+09
IF traffic = 14 THEN travel = 2.5E+09
IF traffic > 14 THEN travel = 5E+09
IF tr = 0 THEN tr = travel
IF tv0 > 0 THEN tvz = tv0
IF tp0 > 0 THEN tpz = tp0
IF tvz > 0 THEN tr = travel: travel = tvz

IF tvz < 1000 THEN traffic = -1: GOTO 14001
IF tvz < 10000 THEN traffic = 0: GOTO 14001
IF tvz < 50000 THEN traffic = 1: GOTO 14001
IF tvz < 100000 THEN traffic = 2: GOTO 14001
IF tvz < 500000 THEN traffic = 3: GOTO 14001
IF tvz < 1000000! THEN traffic = 4: GOTO 14001
IF tvz < 2500000! THEN traffic = 5: GOTO 14001
IF tvz < 5000000! THEN traffic = 6: GOTO 14001
IF tvz < 7500000! THEN traffic = 7: GOTO 14001
IF tvz < 1E+07 THEN traffic = 8: GOTO 14001
IF tvz < 3.33E+07 THEN traffic = 9: GOTO 14001
IF tvz < 6.66E+07 THEN traffic = 10: GOTO 14001
IF tvz < 1E+08 THEN traffic = 11: GOTO 14001
IF tvz < 3.333E+08 THEN traffic = 12: GOTO 14001
IF tvz < 6.666E+08 THEN traffic = 13: GOTO 14001
traffic = 14

14001
IF traffic < 0 THEN traffic$ = "There is usually no insystem Traffic. ": tf$ = "X"
IF traffic > -1 AND traffic < 3 THEN traffic$ = "A typical Backwater. Meeting another ship is a lucky event. ": tf$ = "E"
IF traffic > 2 AND traffic < 6 THEN traffic$ = "The system has a low traffic volume. ": tf$ = "D"
IF traffic > 5 AND traffic < 9 THEN traffic$ = "The system has an average traffic volume. ": tf$ = "C"
IF traffic > 8 AND traffic < 11 THEN traffic$ = "The system has a high traffic volume.": tf$ = "B"
IF traffic > 10 AND traffic < 14 THEN traffic$ = "The system has a very high traffic volume.": tf$ = "A"
IF traffic > 13 THEN traffic$ = "The system has an extreme traffic volume, a virtual interstellar highway.": tf$ = "A+"
'traffic$ = STR$(tvz) + STR$(traffic) + traffic$
facility = CINT(wtn * 2)

IF facility < 0 THEN facility$ = "None, (abandoned, converted or destroyed).": fc$ = "X": STP = 6
IF facility > -1 AND facility < 3 THEN facility$ = "inadequate ": fc$ = "E": STP = 5
IF facility > 2 AND facility < 6 THEN facility$ = "cramped but adequate": fc$ = "D": STP = 4
IF facility > 5 AND facility < 9 THEN facility$ = "average": fc$ = "C": STP = 3
IF facility > 8 AND facility < 12 THEN facility$ = "above average": fc$ = "B": STP = 2
IF facility > 11 AND facility < 15 THEN facility$ = "excellent": fc$ = "A": STP = 1
IF facility > 14 THEN fc$ = "A+": STP = 0
IF facility = 0 THEN facility$ = "The port does not actually meet all requirements for its Starport class"
IF facility = 1 THEN facility$ = "The port has inadequate facilities in at least one respect."
IF facility = 2 THEN facility$ = "The port has the bare minimum to meet all requirements for ist Starport class"
IF facility = 3 THEN facility$ = "The port has cramped, low capacitiy facilities."
IF facility = 4 THEN facility$ = "The port has substandard facilities and services."
IF facility = 5 THEN facility$ = "The port is small, crowded, expect delays for all facilities and services."
IF facility = 6 THEN facility$ = "The port is small but not overcrowded."
IF facility = 7 THEN facility$ = "The port facilities are about average for its class."
IF facility = 8 THEN facility$ = "The port has better than average facilities or services."
IF facility = 9 THEN facility$ = "The port has spacious facilities and efficient, skilled services."
IF facility = 10 THEN facility$ = "This port is capable of almost anything."
IF facility = 11 THEN facility$ = "The port has large, cavernous facilities with plenty of room for expansion."
IF facility = 12 THEN facility$ = "The port has excellent facilities, it can meet any requirement within its class."
IF facility > 12 THEN facility$ = "This port is outstanding, large and efficient, they don`t come any better.": IF stp$ <> "A" THEN facility$ = facility$ + " An Upgrade is imminent."
'travel = travel + travel * ((facility - 7) / 15)

admin$ = ""
IF cr > 0 THEN admin$ = "To be cleared for landing, you need a bill of health for crew and passengers and you need to contact traffic control with your intentions"
IF cr > 1 THEN admin$ = admin$ + " as well as a customs declaration"
IF cr = 3 THEN admin$ = admin$ + ", also Landing and departure clearances, flight plan approval and a portside manifest or physical spot inspections"
IF cr = 4 THEN admin$ = admin$ + ". You also have to undergo an inspection by a boarding party, an inspection of your cargo and of passenger/crew manifests with full physical spot checks"
IF cr = 5 THEN admin$ = admin$ + ". Your flight plans require approved arrival and departure block times, there will be a full physical inspection of passengers, crew and cargo"
IF cr = 6 THEN admin$ = admin$ + ". Finally, you have to take a port guide on board, who will do a comparison of your logbook and the flight recorder. In addition you will have to apply for an exit visa, to leave the planet"
IF admin$ <> "" THEN admin$ = admin$ + "."


IF traffic > 10 THEN trader$ = "bulk carrier (50kt)": cargo = 25000: GOTO 55
IF traffic > 9 THEN trader$ = "Bulk freighter (20kt)": cargo = 13042: GOTO 55
IF traffic > 8 THEN trader$ = "Type AH (5000t)": cargo = 2827: GOTO 55
IF traffic > 7 THEN trader$ = "Type TF (3000t)": cargo = 1312.5: GOTO 55
IF traffic > 6 THEN trader$ = "Type TI (2000t)": cargo = 605.5: GOTO 55
IF traffic > 5 THEN trader$ = "Type OF (1000t)": cargo = 301.5: GOTO 55
IF traffic > 3 THEN trader$ = "Type R (400t)": cargo = 210: GOTO 55
trader$ = "Type A1 (200t)": cargo = 82

55
'FOR tztz = 1 TO 20
IF pop > 5 OR LEFT$(bas$, 1) = "N" OR stp$ = "A" THEN GOSUB 14100
IF ops$ <> "" THEN ops$ = " " + namx$ + ops$
facility$ = facility$ + ops$
'PRINT ops; "W:"; ZW6; opm; LEFT$(ops$, 60): NEXT: END

tday = travel / 365 / cargo: tday$ = "day"

IF tday < 0 THEN tday = 0
IF tday > 3000 THEN tday = tday / 24 / 60: tday$ = "minute": RETURN
IF tday > 50 THEN tday = tday / 24: tday$ = "hour": RETURN
IF tday < .03 THEN tday = tday * 365: tday$ = "year": RETURN
IF tday < .3 THEN tday = tday * 30: tday$ = "month": RETURN
IF tday < 1 THEN tday = tday * 7: tday$ = "week": RETURN
RETURN

14100 'Orbital port
REM ops = INT(4 * RND) + 1: 'INT(6 * RND) + 2:
ops = 0
ZW6 = ops

IF stp$ = "A" THEN DM = 3
IF stp$ = "B" THEN DM = 2
IF stp$ = "C" THEN DM = 1

POM = INT((pop - 6))
TEM = INT(tec / 5)
TRA = INT(traffic / 2)
opm = DM + POM + TEM + TRA
ops = INT(ops + opm)


ops$ = ""
IF ops < 2 THEN ops$ = "": RETURN
baseport$ = "The planetbound part of the port "
IF ops = 2 THEN ops$ = " has a small orbital station equipped with a freight terminal. Passengers are handled in the downport.": RETURN
IF ops = 3 THEN ops$ = " has an orbital station, with a passenger and a freight terminal.": RETURN
IF ops = 4 THEN ops$ = " has an orbital port, with a passenger terminal, and a freight terminal with a dispersed logistic base.": RETURN
IF ops = 5 THEN ops$ = " has a big orbital port, with a passenger terminal as well as a freight terminal with a dispersed logistic base.": RETURN
IF ops = 6 THEN ops$ = " has a huge orbital port, with a combined passenger terminal, shopping concourse, and a freight terminal with a dispersed logistic base. Containers are shuttled to surface destinations.": RETURN
IF ops = 7 THEN ops$ = " has a huge orbital port with a combined passenger terminal, hotel, concourse, and a freight terminal with a dispersed logistic base. All freight traffic is handled in orbit, containers are shuttled to surface destinations.": RETURN
IF ops > 7 THEN ops$ = " has an immense orbital port, containing passenger terminals, several hotels and a grand concourse, as well as a freight terminal with a dispersed logistic base. All freight traffic is handled in orbit, containers are shuttled to surface destinations.": RETURN
RETURN

14200 'traffic control
DIAM = siz * 1600

port$ = "There is no traffic control. This is uncontrolled space."

'IF port < 3 THEN text$ = port$: GOSUB 19999: PRINT #nnn,: RETURN

IF port = 1 THEN GOTO 14300

port$ = "TRAFFIC CONTROL HAS" + STR$(port - 1) + " CONTROLLED ZONE": IF port > 2 THEN port$ = port$ + "S"

PRINT #nnn,: text$ = port$: GOSUB 19999: PRINT #nnn,

'text$ = "ADVISORY Zone. It extends from the planets surface to the outer limit of controlled space. The Advisory Zone is uncontrolled space.":
'GOSUB 19999

IF port > 1 THEN text$ = "STARPORT CONTROL Zone. It covers the physical extent of the starport (on the surface or in orbit), a 15 km radius around it and up to 15 km above it."
IF port > 1 THEN GOSUB 19999: PRINT #nnn,

IF port > 2 THEN text$ = "AEROPSPACE Zone. It extends from the planets surface to" + STR$(DIAM / 10) + " km above the surface."
IF port > 2 THEN GOSUB 19999: PRINT #nnn,

IF port > 3 THEN text$ = "ORBITAL Zone. It extends from the upper limit of the Aerospace Zone to a distance of" + STR$(DIAM * 10 / 1000) + "K kilometers from the the planet."

IF port > 3 THEN GOSUB 19999: PRINT #nnn,

IF port > 4 THEN text$ = "TRANSITION Zone. It extends from the upper limit of the ORBITAL Zone to a distance of" + STR$(DIAM * 10 / 100000) + " Million kilometers. It controls ships up to the event of jump."
IF port > 4 THEN GOSUB 19999: PRINT #nnn,


14300
PRINT #nnn,
'tcar
PRINT #nnn, "TRAFFIC CONTROL, ENCOUNTERS AND REQUIREMENTS"
PRINT #nnn,
IF port = 1 THEN TCARS$ = "System Advisory. A single common channel that all ships in the system (outside controlled space) must use to exchange intentions and information."
IF port = 2 THEN TCARS$ = "Starport Tower / Starport Ground Control. Traffic control that relies on positive identification, tracking and direction of all craft within the Starport Control Zone. Starport Ground controls craft from after landing to just prior to takeoff. The Starport Tower controls all other movements, including takeoff."
IF port = 3 THEN TCARS$ = "Approach/Departure Control. Provides traffic control and separation that relies on craft adhering to published procedures within the planets AEROSPACE Zone. Also provides weather advisories and separation along flight path."
IF port = 4 THEN TCARS$ = "Orbital Control. Traffic control relies on craft adhering to published procedures of craft moving to or from the 100-diameter jump limit using advisories and information provided by Orbital Control."
IF port = 5 THEN TCARS$ = "Transitional Control. Traffic control and traffic separation rely on positive identification, tracking and direction of all craft up to the transition into jump space."

text$ = TCARS$: GOSUB 19999
PRINT #nnn,
RETURN













14400 'transponder echo

FOR car = 0 TO carx: cargoship(car) = 0: NEXT
cargoyear = tvzm * 1000000:
passyear = tpzm * 1E+6

traveltime = 5.5

cargotrav = cargoyear / 365 / 24 * traveltime
passtrav = passyear / 365 / 24 * traveltime: pass0 = passtrav



PRINT cargoyear; cargotrav; passtrav

14410
FOR car = 0 TO carx
    IF cargotrav > cargo(car) * 10 AND cargo(car) > 0 THEN cargoship(car) = cargoship(car) + 1: cargotrav = cargotrav - cargo(car): passtrav = passtrav - pasngr(car)
NEXT
'FOR qi = qi TO carx: PRINT cargoship(qi); trader$(qi),: NEXT
IF cargotrav > 1000 THEN GOTO 14410
FOR car = carx TO 0 STEP -1
    IF cargotrav > cargo(car) AND cargo(car) > 0 THEN cargoship(car) = cargoship(car) + 1: cargotrav = cargotrav - cargo(car): passtrav = passtrav - pasngr(car)
NEXT

FOR car = carx TO 0 STEP -1
    IF cargotrav > cargo(car) AND cargo(car) > 0 THEN cargoship(car) = cargoship(car) + 1: cargotrav = cargotrav - cargo(car): passtrav = passtrav - pasngr(car)
NEXT



14420
FOR qi = 0 TO carx: IF cargoship(qi) > 0 THEN PRINT cargoship(qi); trader$(qi),:
NEXT
PRINT pass0; passtrav, passtrav / pass0 * 100; "%"
'GOSUB 1000
cargo$ = "": count = 0
FOR car = 0 TO carx
    IF cargoship(car) > 0 THEN cargo$ = cargo$ + STR$(cargoship(car)) + " x " + trader$(car): IF car < carx THEN cargo$ = cargo$ + ", "
    count = count + cargoship(car)
NEXT
IF cargo$ = "" THEN cargo$ = " no other merchant ships"
cargo$ = "During the flight from the jump point to " + namx$ + " you encounter" + cargo$: IF count > 0 THEN cargo$ = cargo$ + ", altogether" + STR$(count) + " vessels." ELSE cargo$ = cargo$ + "."
text$ = cargo$: GOSUB 19999: PRINT #nnn,

RETURN










14600 'cities



3232 '  POP < 1E6
IF pop < 2 THEN city$ = " only one settlement.": GOTO 3240

citypop = POP0: FOR qc = 0 TO citx: citynum(qc) = 0: NEXT
cix = 0
14610
ZW6 = INT(RND * 6) + INT(RND * 6) + 2

FOR qc = 0 TO citx
    citynum = 11 - qc - INT(tl / 5): PRINT citynum
    IF citypop > city(qc) AND ZW6 > citynum AND tec > 8 - qc THEN citypop = citypop - city(qc): citynum(qc) = citynum(qc) + 1: cix = 1
NEXT
IF citypop > 1000 OR cix = 0 THEN GOTO 14610

city$ = ""
FOR qc = 0 TO citx
    IF citynum(qc) > 0 THEN city$ = city$ + STR$(citynum(qc)) + " " + city$(qc): IF qc < citx THEN city$ = city$ + ","
NEXT

3240 text$ = text$ + namx$ + " has" + city$ + ". ": GOSUB 19999: 'PRINT #nnn,

'PRINT city$; citypop: GOSUB 1000



15000 'RETURN
FOR t = 1 TO 6
    IF loy <> lox(t) GOTO 15001
    rtx$ = "*"
    stz$ = MID$(upp$, 20, 1)
    IF stz$ = "A" OR stz$ = "B" THEN tfm = tfm + 2: rtx$ = "+2"
    IF stz$ = "C" OR stz$ = "D" OR stz$ = "E" THEN tfm = tfm + 1: rtx$ = "+1"
15001 NEXT

RETURN

























15100 'Continents and water
world$ = ""

IF hyd < 6 GOTO 15200
world$ = " The majority of " + namx$ + "'s surface is covered by water,"
EW6 = INT(RND * 6) + 1
conti = EW6 + hyd * 3


IF conti = 16 THEN MAJ = 2: MAJm = 1: MINC = 1: MINCm = -1: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 17 THEN MAJ = 2: MAJm = 1: MINC = 2: MINCm = -2: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 18 THEN MAJ = 2: MAJm = 1: MINC = 3: MINCm = -3: MAJI = 3: MAJIm = -3: ARCHIP = 2

IF conti = 19 THEN MAJ = 2: MAJm = 0: MINC = 1: MINCm = -1: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 20 THEN MAJ = 2: MAJm = 1: MINC = 2: MINCm = -2: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 21 THEN MAJ = 2: MAJm = 1: MINC = 3: MINCm = -3: MAJI = 3: MAJIm = -3: ARCHIP = 2

IF conti = 22 THEN MAJ = 1: MAJm = 0: MINC = 1: MINCm = -1: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 23 THEN MAJ = 1: MAJm = 0: MINC = 2: MINCm = -2: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 24 THEN MAJ = 1: MAJm = 0: MINC = 3: MINCm = -3: MAJI = 3: MAJIm = -3: ARCHIP = 2

IF conti = 25 THEN MAJ = 1: MAJm = -1: MINC = 1: MINCm = 0: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 26 THEN MAJ = 1: MAJm = -1: MINC = 2: MINCm = 0: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 27 THEN MAJ = 1: MAJm = -1: MINC = 3: MINCm = 0: MAJI = 3: MAJIm = -3: ARCHIP = 2

IF conti = 28 THEN MAJ = 1: MAJm = -2: MINC = 1: MINCm = -1: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 29 THEN MAJ = 1: MAJm = -3: MINC = 1: MINCm = -2: MAJI = 3: MAJIm = -3: ARCHIP = 2
IF conti = 30 THEN MAJ = 1: MAJm = -4: MINC = 1: MINCm = -3: MAJI = 2: MAJIm = 0: ARCHIP = 2

IF conti = 31 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 1: MAJIm = -3: ARCHIP = 2
IF conti = 32 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 1
IF conti = 33 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 1

IF conti = 34 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 1
IF conti = 35 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 0: world$ = " there is no significant surface area.": GOTO 15210
IF conti = 36 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 0: world$ = " there is no significant surface area.": GOTO 15210

Majorcon = MAJ * (INT(RND * 6) + 1) + MAJm: IF Majorcon < 0 THEN Majorcon = 0
Minorcon = MINC * (INT(RND * 6) + 1) + MINCm: IF Minorcon < 0 THEN Minorcon = 0
MajIsl = MAJI * (INT(RND * 6) + 1) + MAJIm: IF MajIsl < 0 THEN MajIsl = 0
Archipe = ARCHIP * (INT(RND * 6) + 1): IF Archipe < 0 THEN Archipe = 0

world$ = world$ + " with the exception of"

IF Majorcon = 1 THEN world$ = world$ + " one major Continent,"
IF Majorcon > 1 THEN world$ = world$ + STR$(Majorcon) + " major Continents,"


IF Minorcon = 1 THEN world$ = world$ + " one minor Continent, "
IF Minorcon > 1 THEN world$ = world$ + STR$(Minorcon) + " minor Continents,"


IF MajIsl = 1 THEN world$ = world$ + " one major Isle,"
IF MajIsl > 1 THEN world$ = world$ + STR$(MajIsl) + " major Isles,"


IF Archipe > 0 THEN world$ = world$ + STR$(Archipe) + " Archipelagos."

GOTO 15210

15200 ' HYD < 6
world$ = " The majority of " + namx$ + "'s surface is covered by land,"
EW6 = INT(RND * 6) + 1
conti = EW6 + hyd * 3: PRINT conti,


IF conti = 1 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 0: world$ = world$ + " there is no significant body of water.": GOTO 15210
IF conti = 2 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 0: world$ = world$ + " there is no significant body of water.": GOTO 15210
IF conti = 3 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 0: world$ = world$ + " there are a few scatterd lakes, but there is no significant body of surface water.": GOTO 15210

IF conti = 4 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 0: world$ = world$ + " there are a few scatterd lakes, but there is no significant body of surface water.": GOTO 15210
IF conti = 5 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: MAJI = 0: MAJIm = 0: ARCHIP = 0: world$ = world$ + " there are a few scatterd lakes, but there is no significant body of surface water.": GOTO 15210
IF conti = 6 THEN MAJ = 0: MAJm = 0: MINC = 0: MINCm = 0: SMAS = 1: SMASM = -3: SCATL = 2

IF conti = 7 THEN MAJ = 1: MAJm = -4: MINC = 1: MINCm = -3: SMAS = 2: SMASM = -3: SCATL = 2
IF conti = 8 THEN MAJ = 1: MAJm = -4: MINC = 1: MINCm = -2: SMAS = 3: SMASM = -3: SCATL = 2
IF conti = 9 THEN MAJ = 1: MAJm = -3: MINC = 1: MINCm = -1: SMAS = 3: SMASM = -3: SCATL = 2

IF conti = 10 THEN MAJ = 1: MAJm = -3: MINC = 1: MINCm = -1: SMAS = 3: SMASM = -3: SCATL = 2
IF conti = 11 THEN MAJ = 1: MAJm = -2: MINC = 1: MINCm = -1: SMAS = 3: SMASM = -3: SCATL = 2
IF conti = 12 THEN MAJ = 1: MAJm = -2: MINC = 2: MINCm = -2: SMAS = 3: SMASM = -3: SCATL = 2

IF conti = 13 THEN MAJ = 1: MAJm = -1: MINC = 1: MINCm = -1: SMAS = 3: SMASM = -3: SCATL = 2
IF conti = 14 THEN MAJ = 1: MAJm = -1: MINC = 2: MINCm = -2: SMAS = 3: SMASM = -3: SCATL = 2
IF conti = 15 THEN MAJ = 1: MAJm = -1: MINC = 3: MINCm = -3: SMAS = 3: SMASM = -3: SCATL = 2

IF conti = 16 THEN MAJ = 1: MAJm = 0: MINC = 1: MINCm = -1: SMAS = 3: SMASM = -3: SCATL = 2
IF conti = 17 THEN MAJ = 1: MAJm = 0: MINC = 2: MINCm = -2: SMAS = 3: SMASM = -3: SCATL = 2
IF conti = 18 THEN MAJ = 1: MAJm = 0: MINC = 3: MINCm = -3: SMAS = 3: SMASM = -3: SCATL = 2

IF conti = 19 THEN MAJ = 1: MAJm = 6: MINC = 1: MINCm = -1: SMAS = 3: SMASM = -3: SCATL = 2
IF conti = 20 THEN MAJ = 1: MAJm = 6: MINC = 2: MINCm = -2: SMAS = 3: SMASM = -3: SCATL = 2
IF conti = 21 THEN MAJ = 1: MAJm = 6: MINC = 3: MINCm = -3: SMAS = 3: SMASM = -3: SCATL = 2

Majoroce = MAJ * (INT(RND * 6) + 1) + MAJm: IF Majoroce < 0 THEN Majoroce = 0
IF conti > 18 THEN Majoroce = 1
Minoroce = MINC * (INT(RND * 6) + 1) + MINCm: IF Minoroce < 0 THEN Minoroce = 0
SmaSea = SMAS * (INT(RND * 6) + 1) + SMASM: IF SmaSea < 0 THEN SmaSea = 0
SmaLak = SCATL * (INT(RND * 6) + 1)

world$ = world$ + " with the exception of"

IF Majoroce = 1 THEN world$ = world$ + " one major Ocean,"
IF Majoroce > 1 THEN world$ = world$ + STR$(Majoroce) + " major Oceans,"

IF Minoroce = 1 THEN world$ = world$ + "  one minor Ocean,"
IF Minoroce > 1 THEN world$ = world$ + STR$(Minoroce) + " minor Oceans,"

IF SmaSea = 1 THEN world$ = world$ + " one small Sea,"
IF SmaSea > 1 THEN world$ = world$ + STR$(SmaSea) + " small Seas,"

world$ = world$ + STR$(SmaLak) + " scattered Lakes."

15210
'PRINT world$: GOSUB 1000

PRINT #nnn,
'world$ = " " + namx$ + "'s surface has the following features: " + world$: RETURN
RETURN
GOSUB 19999: RETURN




























17000 lox(0) = lox
IF INT(horiz / 2) = horiz / 2 GOTO 17001
lox(1) = lox - 1
lox(2) = lox + 1
lox(3) = lox - 101
lox(4) = lox - 100
lox(5) = lox + 99
lox(6) = lox + 100
RETURN
17001
lox(1) = lox - 1
lox(2) = lox + 1
lox(3) = lox - 100
lox(4) = lox - 99
lox(5) = lox + 100
lox(6) = lox + 101
RETURN
16000
vl = INT(vl)
IF vl < 0 THEN vl$ = "X"
IF vl = 0 THEN vl$ = "0"
IF vl = 1 THEN vl$ = "1"
IF vl = 2 THEN vl$ = "2"
IF vl = 3 THEN vl$ = "3"
IF vl = 4 THEN vl$ = "4"
IF vl = 5 THEN vl$ = "5"
IF vl = 6 THEN vl$ = "6"
IF vl = 7 THEN vl$ = "7"
IF vl = 8 THEN vl$ = "8"
IF vl = 9 THEN vl$ = "9"
IF vl = 10 THEN vl$ = "A"
IF vl = 11 THEN vl$ = "B"
IF vl = 12 THEN vl$ = "C"
IF vl = 13 THEN vl$ = "D"
IF vl = 14 THEN vl$ = "E"
IF vl = 15 THEN vl$ = "F"
IF vl > 15 THEN vl$ = "G"
RETURN
19000
IF traffic < 1 AND facility < 1 THEN PRINT " ? ";: RETURN
PRINT stp$;
vl = traffic: GOSUB 16000: PRINT vl$;
vl = facility: GOSUB 16000: PRINT vl$
RETURN



20000 ' GURPS Far Trader BTN calculation

wwx = btn + wt0
    
   
kg = btn: IF wt0 < kg THEN kg = wt0
   

IF agr0 = 1 AND exx = 1 THEN wwx = wwx + .5
IF agr0 = 1 AND nag = 1 THEN wwx = wwx + .5

IF agr = 1 AND exx0 = 1 THEN wwx = wwx + .5
IF agr = 1 AND nag0 = 1 THEN wwx = wwx + .5


IF ind0 = 1 AND nin = 1 THEN wwx = wwx + .5
IF nin0 = 1 AND ind = 1 THEN wwx = wwx + .5

IF pol$ <> po0$ THEN wwx = wwx - .5

hz = ABS(l1 - horiz): vz = ABS(l2 - vertik)
entf = INT(SQR(hz ^ 2 + vz ^ 2))

dmo = 0
IF entf > 1 THEN dmo = .5
IF entf > 2 THEN dmo = 1
IF entf > 5 THEN dmo = 1.5
IF entf > 9 THEN dmo = 2
IF entf > 19 THEN dmo = 2.5
IF entf > 29 THEN dmo = 3

wwx = wwx - dmo
IF wwx > kg + 5 THEN wwx = kg + 5

tvx = 0
IF wwx > 3.5 THEN tvx = 5
IF wwx > 4 THEN tvx = 10
IF wwx > 4.5 THEN tvx = 50
IF wwx > 5 THEN tvx = 100
IF wwx > 5.5 THEN tvx = 500
IF wwx > 6 THEN tvx = 1000
IF wwx > 6.5 THEN tvx = 5000
IF wwx > 7 THEN tvx = 10000
IF wwx > 7.5 THEN tvx = 50000
IF wwx > 8 THEN tvx = 100000
IF wwx > 8.5 THEN tvx = 500000
IF wwx > 9 THEN tvx = 1000000!
IF wwx > 9.5 THEN tvx = 5000000!
IF wwx > 10 THEN tvx = 1E+07
IF wwx > 10.5 THEN tvx = 5E+07
IF wwx > 11 THEN tvx = 1E+08
IF wwx > 11.5 THEN tvx = 5E+08
tvx = tvx * (RND + .1) ' Wurfel

wwp = wwx + capital
IF ric0 = 1 OR ric = 1 THEN wwp = wwp + .5

tvp = 0
IF wwp > 5 THEN tvp = 5
IF wwp > 5.5 THEN tvp = 10
IF wwp > 6 THEN tvp = 50
IF wwp > 6.5 THEN tvp = 100
IF wwp > 7 THEN tvp = 500
IF wwp > 7.5 THEN tvp = 1000
IF wwp > 8 THEN tvp = 5000
IF wwp > 8.5 THEN tvp = 10000
IF wwp > 9 THEN tvp = 50000!
IF wwp > 9.5 THEN tvp = 100000!
IF wwp > 10 THEN tvp = 500000
IF wwp > 10.5 THEN tvp = 1000000!
IF wwp > 11 THEN tvp = 5000000!
IF wwp > 11.5 THEN tvp = 1E+07
tvp = tvp * (RND + .1): ' Wurfel

IF entf < 1 THEN tvx = 0: tvp = 0
tvq1 = tvz / 1000000!
tvq = tvx / 1000000!

'LOCATE 24, 48: print #nnn, USING "####,.##"; tvq1; tvq; : print namx$;
IF a$ = "x" THEN wwx$ = STR$(INT(wwp * 10) / 10): RETURN
IF entf = 0 THEN wwx$ = "!": tvx = 0: RETURN
wwx$ = " ":
IF wwx >= 8 + wq - capital THEN wwx$ = "°"
IF wwx >= 9 + wq - capital THEN wwx$ = "²"
IF wwx >= 10 + wq - capital THEN wwx$ = "Û"
IF wwx >= 11 + wq - capital THEN wwx$ = "Û"
IF wwx >= 12 + wq - capital THEN wwx$ = "Û"
wwx$ = "-"
IF wwx >= 4 + wq - capital THEN wwx$ = "4"
IF wwx >= 5 + wq - capital THEN wwx$ = "5"
IF wwx >= 6 + wq - capital THEN wwx$ = "6"
IF wwx >= 7 + wq - capital THEN wwx$ = "7"
IF wwx >= 8 + wq - capital THEN wwx$ = "8"
IF wwx >= 9 + wq - capital THEN wwx$ = "9"
IF wwx >= 10 + wq - capital THEN wwx$ = "A"
IF wwx >= 11 + wq - capital THEN wwx$ = "B"
IF wwx >= 12 + wq - capital THEN wwx$ = "C"

RETURN



30000 ' Trade Route calculation


tv0 = 0:
tvz = 0:
minor$ = ""
feeder$ = ""
major$ = ""

tp0 = 0: tpz = 0
FOR i = 1 TO systems
    upp$ = upp$(i)
    namx$ = MID$(upp$(i), 6, 20)
    IF TSAO = 1 THEN namx$ = LEFT$(upp$(i), 20)
    30102 IF RIGHT$(namx$, 1) = " " THEN namx$ = LEFT$(namx$, LEN(namx$) - 1): GOTO 30102


    ' l1 = VAL(MID$(upp$, 15, 2)): l2 = VAL(MID$(upp$, 17, 2))
    l1 = VAL(MID$(upp$, 1, 2)): l2 = VAL(MID$(upp$, 3, 2))
    IF TSAO = 1 THEN l1 = VAL(MID$(upp$, 25, 2)): l2 = VAL(MID$(upp$, 27, 2))



    othw = 1: GOSUB 2001: GOSUB 9100: GOSUB 20000
    IF entf = 0 GOTO 30001
    IF INTWARS = 1 AND entf > 3 GOTO 30001
    tvz = tvz + tvx: tpz = tpz + tvp
    prz = i / systems * 99

    tvq = tvz / 1000000!
    'vq = tvx / 1000
    LOCATE 24, 68: PRINT "####,.## ##%"; tvq; prz;
    dsf = 365
    STARP$ = " " + namx$: ' + " -" + wwx$
    vl = entf: GOSUB 16000: STARP$ = STARP$ + " (J-" + vl$ + ")"

    STARPC = tvx / 1000 / dsf: pc1$ = "Kt,": IF STARPC < 10 THEN STARPC = STARPC * 1000: pc1$ = "t,"
    STARPP = tvp / 1000 / dsf: pp1$ = "K": IF STARPP < 10 THEN STARPP = STARPP * 1000: pp1$ = ""
    STARP$ = STARP$ + STR$(INT(STARPC)) + pc1$
    STARP$ = STARP$ + STR$(INT(STARPP)) + pp1$ + "P."
    IF wwx >= 10 + wq - capital THEN major = major + 1: LOCATE 24, 2: PRINT LEFT$(upp$, 18); entf;: major$ = major$ + STARP$: GOTO 30001
    IF wwx >= 9 + wq - capital THEN feeder = feeder + 1: LOCATE 24, 24: PRINT LEFT$(upp$, 18); entf;: feeder$ = feeder$ + STARP$: GOTO 30001
    IF wwx >= 8 + wq - capital THEN minor = minor + 1: LOCATE 24, 48: PRINT LEFT$(upp$, 18); entf;: minor$ = minor$ + STARP$
30001 NEXT: tv0 = tvz: tp0 = tpz
'PLAY musik$

RETURN

34000 'LAW LEVEL

'2-5 Personal
'6-7 Territorial
'8-12 Undivided

'-1 law level A+
'+2 Extensivness Monolithic
govmod = INT((law + gov) / 2)
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
Overalllaw = law
IF law > 20 THEN law$ = "The world is a Full-fledged police state, the government is not afraid to use mind control technologies or slavery.": GOTO 3410
IF law = 20 THEN law$ = "The world is a Full-fledged police state, totally oppressive and restrictive.": GOTO 3410
IF law = 19 THEN law$ = "The world is a Full-fledged police state, excessively oppressive and restrictive.": GOTO 3410
IF law = 18 THEN law$ = "The world is a Full-fledged police state, routinely oppressive and restrictive.": GOTO 3410
IF law = 17 THEN law$ = "The world is a Full-fledged police state, oppressive practices are legalized.": GOTO 3410
IF law = 16 THEN law$ = "The world is a Full-fledged police state, there will be severe punishment for even petty infractions.": GOTO 3410
IF law = 15 THEN law$ = "The world is a Full-fledged police state, all facets of daily life rigidly controlled.": GOTO 3410
IF law = 14 THEN law$ = "The world is a Full-fledged police state, personal freedom does not exist.": GOTO 3410
IF law = 13 THEN law$ = "The general law level is extreme. Paramilitary forces work in addition to law enforcement.": GOTO 3410
IF law = 12 THEN law$ = "The general law level is extreme. Unrestricted invasion of privacy is common practice.": GOTO 3410
IF law = 11 THEN law$ = "The general law level is extreme. Rigid control of civilian movement.": GOTO 3410
IF law = 10 THEN law$ = "The general law level is stringent. Most infringements will be punished severely.": GOTO 3410
IF law = 9 THEN law$ = "The general law level is very strict. Many infringements will be punished severely.": GOTO 3410
IF law = 8 THEN law$ = "The general law level is strict. Infringements will generally be punished.": GOTO 3410
IF law = 7 THEN law$ = "The general law level is average. Most infringements will be punished.": GOTO 3410
IF law = 6 THEN law$ = "The general law level is average. Many infringements will be punished.": GOTO 3410
IF law = 5 THEN law$ = "The general law level is average to low. Many infringements will not be punished.": GOTO 3410
IF law = 4 THEN law$ = "The general law level is low. Most infringements will not be punished.": GOTO 3410
IF law = 3 THEN law$ = "The general law level is very low. Enforcement is scarce.": GOTO 3410
IF law = 2 THEN law$ = "The general law level is extremly low, Enforcement is nearly unheard of.": GOTO 3410
IF law = 1 THEN law$ = "A general law level is actually non existant. If at all, there are a few regulations.": GOTO 3410
law$ = "No laws at all, there is an effective state of anarchy.": GOTO 3410


3410
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
Weaponlaw = ZW6 - 7 + govmod
IF Weaponlaw >= 14 THEN Weaponlaw$ = "Weapon laws are draconic. No posession of weapons is allowed. Carrying is a capital crime.": GOTO 3420
IF Weaponlaw >= 12 THEN Weaponlaw$ = "The Weapons law level is extreme. No posession of weapons is allowed. Carrying will be punished severely.": GOTO 3420
IF Weaponlaw >= 10 THEN Weaponlaw$ = "The Weapons law level is stringent. No posession of weapons is allowed. Carrying will be punished.": GOTO 3420
IF Weaponlaw = 9 THEN Weaponlaw$ = "The Weapons law level is very restricting. Posession of weapons outside home is prohibited. Carrying will be punished.": GOTO 3420
IF Weaponlaw = 8 THEN Weaponlaw$ = "The Weapons law level is restricting. No ranged weapons are allowed, but concealed carrying of blade weapons is allowed.": GOTO 3420
IF Weaponlaw = 7 THEN Weaponlaw$ = "The Weapons law level is moderate. No ranged weapons are allowed, but open carrying of blade weapons is allowed.": GOTO 3420
IF Weaponlaw = 6 THEN Weaponlaw$ = "The Weapons law level is moderate. Open carrying of sidearms is allowed.": GOTO 3420
IF Weaponlaw = 5 THEN Weaponlaw$ = "The Weapons law level is moderate. Allowed are Automatic weapons, Pistols, as well as open carrying.": GOTO 3420
IF Weaponlaw = 4 THEN Weaponlaw$ = "The Weapons law level is low. Allowed are Shock, EMP, Rad, Mag, Grav, Automatic weapons, Pistols, as well as open carrying.": GOTO 3420
IF Weaponlaw = 3 THEN Weaponlaw$ = "The Weapons law level is low. Allowed are Energy, Shock, EMP, Rad, Mag, Grav, Automatic weapons, Pistols, as well as open carrying.": GOTO 3420
IF Weaponlaw = 2 THEN Weaponlaw$ = "The Weapons law level is very low. Allowed are Acid, Fire, Gas, Energy, Shock, EMP, Rad, Mag, Grav, Automatic weapons, Pistols, as well as open carrying.": GOTO 3420
IF Weaponlaw = 1 THEN Weaponlaw$ = "The Weapons law level is extremly low. Allowed are Portable Heavy, Acid, Fire, Gas, Energy, Shock, EMP, Rad, Mag, Grav, Automatic weapons, Pistols, as well as open carrying.": GOTO 3420
Weaponlaw$ = "The Weapons law level is actually non existing, there are no prohibitions on ownership and carrying.": GOTO 3420


3420
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
Tradelaw = ZW6 - 7 + govmod
IF Tradelaw >= 14 THEN Tradelaw$ = "You need triplicate permits for each type of good you want to buy or sell, as well as a license from the government to trade at all. In addition you have to deposit safeties for the duration of the business talks.": GOTO 3430
IF Tradelaw >= 12 THEN Tradelaw$ = "You need triplicate permits for each type of good you want to buy or sell, as well as a license from the government to trade at all.": GOTO 3430
IF Tradelaw >= 10 THEN Tradelaw$ = "You need triplicate permits if you want to buy or sell, as well as a license from the government to trade at all.": GOTO 3430
IF Tradelaw >= 9 THEN Tradelaw$ = "You need a permit if you want to buy or sell, as well as a license from the government to trade at all.": GOTO 3430
IF Tradelaw >= 8 THEN Tradelaw$ = "You need a permit if you want to sell, as well as a license from the government to trade at all.": GOTO 3430
IF Tradelaw >= 7 THEN Tradelaw$ = "You need a license from the government to trade.": GOTO 3430
IF Tradelaw >= 6 THEN Tradelaw$ = "Buyer beware, many laws favour the seller.": GOTO 3430
IF Tradelaw >= 5 THEN Tradelaw$ = "Buyer beware, most laws favour the seller.": GOTO 3430
IF Tradelaw >= 4 THEN Tradelaw$ = "Buyer beware, the laws favour the seller.": GOTO 3430
IF Tradelaw >= 3 THEN Tradelaw$ = "Buyer beware, only in extreme cases is there any hope of recompensation.": GOTO 3430
IF Tradelaw >= 2 THEN Tradelaw$ = "Buyer beware, you bought it, you own it.": GOTO 3430
Tradelaw$ = "No laws regulating trade are in existence.": GOTO 3430



3430
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
Criminallaw = ZW6 - 7 + govmod
IF Criminallaw > 14 THEN CriminalLaw$ = "The Criminal law level is draconic. Even the smallest infringements will be punished severely.": GOTO 3440
IF Criminallaw > 10 THEN CriminalLaw$ = "The Criminal law level is extreme. Even the smallest infringements will be punished severely.": GOTO 3440
IF Criminallaw = 10 THEN CriminalLaw$ = "The Criminal Law level is stringent. Even the smallest infringement will be punished severely.": GOTO 3440
IF Criminallaw = 9 THEN CriminalLaw$ = "The Criminal Law level is very restricting. Even the smallest infringements will be punished.": GOTO 3440
IF Criminallaw = 8 THEN CriminalLaw$ = "The Criminal Law level is restricting. Most infringements will be punished.": GOTO 3440
IF Criminallaw = 7 THEN CriminalLaw$ = "The Criminal Law level is average, all Crimes will be prosecuted.": GOTO 3440
IF Criminallaw = 6 THEN CriminalLaw$ = "The Criminal Law level is average, most crimes will be prosecuted.": GOTO 3440
IF Criminallaw = 5 THEN CriminalLaw$ = "The Criminal Law level is below average, only capital crimes, assault and theft will be prosecuted.": GOTO 3440
IF Criminallaw = 4 THEN CriminalLaw$ = "The Criminal Law level is low, only capital crimes, assault and grand theft will be prosecuted.": GOTO 3440
IF Criminallaw = 3 THEN CriminalLaw$ = "The Criminal Law level is very low, only capital crimes and assault and grand theft will be prosecuted.": GOTO 3440
IF Criminallaw = 2 THEN CriminalLaw$ = "The Criminal Law level is extrmely low, only capital crimes and assault will be prosecuted.": GOTO 3440
IF Criminallaw = 1 THEN CriminalLaw$ = "The Criminal Law level is nearly non-existing, only capital crimes will be prosecuted.": GOTO 3440
CriminalLaw$ = "There is no criminal Law. Everyone has to take care of himself.": GOTO 3440


3440
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
Civillaw = ZW6 - 7 + govmod
IF Civillaw >= 14 THEN Civillaw$ = "BWAP paradise, the bureaucracy is truly overwhelming. Quadruple documentation is needed for everything. Bribery is a capital crime.": GOTO 3450
IF Civillaw >= 12 THEN Civillaw$ = "The Civil law level is extreme, the bureaucracy overwhelming. Triplicate documentation is needed for about everything. Bribery is a capital crime.": GOTO 3450
IF Civillaw >= 10 THEN Civillaw$ = "The Civil law level is stringent, the bureaucracy overwhelming. Duplicate documentation is needed for about everything. Bribery is a capital crime.": GOTO 3450
IF Civillaw >= 9 THEN Civillaw$ = "The Civil law level is very restricting, the bureaucracy controls most branches. Duplicate documentation is needed for about everything. Bribery is very dangerous.": GOTO 3450
IF Civillaw >= 8 THEN Civillaw$ = "The Civil law level is restricting, the bureaucracy powerful. Documentation is needed for about everything. Bribery is dangerous.": GOTO 3450
IF Civillaw >= 6 THEN Civillaw$ = "The Civil law level is average. Documentation is needed for most transactions, Bribery is risky.": GOTO 3450
IF Civillaw >= 5 THEN Civillaw$ = "The Civil law level is linient. A well placed bribe can be helpful.": GOTO 3450
IF Civillaw >= 4 THEN Civillaw$ = "The Civil law level is low. A well placed bribe can smooth things out.": GOTO 3450
IF Civillaw >= 3 THEN Civillaw$ = "The Civil law level is very low. A well placed bribe can be very helpful.": GOTO 3450
IF Civillaw >= 1 THEN Civillaw$ = "The Civil law level is actually non existing. While bureaucracy is nearly nonexistent, a bribe at the right place can speed things up.": GOTO 3450
Civillaw$ = "There is no civil law level that governs the conduct of the citzens with one another. To get things going, a bribe at the right place can be helpful.": GOTO 3450

3450
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
Personallaw = ZW6 - 7 + govmod
IF Personallaw >= 12 >= 14 THEN Personallaw$ = "There is no personal freedom, the state controls everything. Surveillance is everywhere.": GOTO 3460
IF Personallaw >= 12 THEN Personallaw$ = "There is almost no personal freedom, the state controls most interactions.": GOTO 3460
IF Personallaw >= 10 THEN Personallaw$ = "There is not very much personal freedom, the state controls a lot of areas.": GOTO 3460
IF Personallaw >= 9 THEN Personallaw$ = "Personal freedom is impeded, the state controls many areas.": GOTO 3460
IF Personallaw >= 8 THEN Personallaw$ = "Personal freedom is partially restricted. The state controls some areas.": GOTO 3460
IF Personallaw >= 6 THEN Personallaw$ = "Personal freedom is usually not restricted.": GOTO 3460
IF Personallaw >= 5 THEN Personallaw$ = "Personal freedom is generally not restricted.": GOTO 3460
IF Personallaw >= 4 THEN Personallaw$ = "Personal freedom is generally unrestricted.": GOTO 3460
IF Personallaw >= 3 THEN Personallaw$ = "Beyond emergencies, the personal freedom is completely unrestricted.": GOTO 3460
Personallaw$ = "The government does nothing to impact the personal freedom of its citizens.": GOTO 3460

3460

'PRINT #nnn,
'PRINT #nnn, USING "LAW LEVEL (##)"; law
'PRINT #nnn,

text$ = law$ + " " + Weaponlaw$ + " " + CriminalLaw$: GOSUB 19999:
PRINT #nnn,
text$ = Tradelaw$ + " " + Civillaw$ + " " + Personallaw$: GOSUB 19999:

'text$ = law$: GOSUB 19999: PRINT #nnn,
'text$ = Weaponlaw$: GOSUB 19999: PRINT #nnn,
'text$ = Tradelaw$: GOSUB 19999: PRINT #nnn,
'text$ = CriminalLaw$: GOSUB 19999: PRINT #nnn,
'text$ = Civillaw$: GOSUB 19999: PRINT #nnn,
'text$ = Personallaw$: GOSUB 19999: PRINT #nnn,

RETURN
'oVERALL
'

'Weapons
'Trade
'Criminal
'Civil
'Personal freedom

'2D-7+GOV
'
'12 draconic
'11 extreme
'10 strict
'9 stringent
'8 restricting
'7 average
'6 linient
'5 weak
'4 soft
'3 squishy
'2 actually non existing















35000 'TECH LEVEL

' Digest WBH Tech Level

HCTL = tec
LCTM = INT(HCTL / 2)
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 4 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)
PLM = 0
IF pop < 6 THEN PLM = 1
IF pop > 8 THEN PLM = -1
TLG = 0
IF gov = 7 THEN TLG = -2
LCTL = HCTL + TLM + PLM + TLG
IF LCTL > HCTL THEN LCTL = HCTL
IF LCTL < LCTM THEN LCTL = LCTM

SciTL = HCTL

IF zon$ <> "R" AND stp$ <> "X" AND SciTL < 9 THEN SciTL = 9

IF SciTL = 0 THEN SciTl$ = "Animistic explanations for physical processes"
IF SciTL = 1 THEN SciTl$ = "Engineering and architecturee by rule-of-thumb techniques. Astrology appears due to the need to time seasonal activities (planting and harvest, for example)."
IF SciTL = 2 THEN SciTl$ = "Geocentric theory of celestial motions. First empirical rules of mechanics."
IF SciTL = 3 THEN SciTl$ = "Observational astronomy matures, with very precise naked-eye observations of planetary and stellar motions."
IF SciTL = 4 THEN SciTl$ = "The telescope leads to a revolution in astronomical observation. Old theories of mechanics and celestial motions are rejected, usually in favor of a heliocentric theory."
IF SciTL = 5 THEN SciTl$ = "The first synthesis of celestial and local laws of motion, usually along the lines of Newtonian mechanics. Engineering begins to rely on rigorous mathematics and empirical investigation of materials."
IF SciTL = 6 THEN SciTl$ = "Flaws in the Newtonian synthesis become obvious. New theories involve elements of quantum mechanics and Einsteinian relativity. First approximate measurements of galactic distances."
IF SciTL = 7 THEN SciTl$ = "High-energy physics elaborates on the basic structure of quantum and relativistic theory. First full classification of elementary particles and forces. Cosmology appears as a viable science."
IF SciTL = 8 THEN SciTl$ = "Quantum physics and relativity theory are reconciled in some form of unified field theory. One consequence of the new theory is gravity-manipulation technology. A few cultures discover jump space at about this point."
IF SciTL = 9 THEN SciTl$ = "Empirical observations allow full access to jump space. The exact relationship of jump space to normal space remains a mystery. Gravitic manipulation continues to advance."
IF SciTL = 10 THEN SciTl$ = "Empirical observations allow full access to jump space. The exact relationship of jump space to normal space remains a mystery. Gravitic manipulation continues to advance."
IF SciTL = 11 THEN SciTl$ = "Empirical observations allow full access to jump space. The exact relationship of jump space to normal space remains a mystery. Gravitic manipulation continues to advance."
IF SciTL = 12 THEN SciTl$ = "Continued elaboration on unified field theory leads to practical methods for manipulating the strong and weak nuclear forces."
IF SciTL = 13 THEN SciTl$ = "Continued elaboration on unified field theory leads to practical methods for manipulating the strong and weak nuclear forces."
IF SciTL = 14 THEN SciTl$ = "Advanced control of gravity, including the creation of artificial gravitic fields at a distance."
IF SciTL > 14 THEN SciTl$ = "Tentative theory unifying the known physics of normal space and the observed physics of jump space."

SciTl$ = "SCIENCE" + STR$(SciTL) + " : " + SciTl$

PRINT HCTL; LCTL,

MatTL = HCTL

IF MatTL = 0 THEN MatTL$ = "Use of found materials in construction (wood, stone, bone and hides). "
IF MatTL = 1 THEN MatTL$ = "Bronze working, hard woods, quarried stone."
IF MatTL = 2 THEN MatTL$ = "Iron working, fine carving of quarried stone, cement and concrete."
IF MatTL = 3 THEN MatTL$ = "Improved iron working, soft steels. Reinforced concrete becomes possible, although it is more likely to appear once hard steels are available (at TL4-5)."
IF MatTL = 4 THEN MatTL$ = "Hard steels, advanced masonry techniques."
IF MatTL = 5 THEN MatTL$ = "Large-scale production of hard steels, allowing their use as structural members."
IF MatTL = 6 THEN MatTL$ = "Versatile metal alloys, plastics and other synthetic materials."
IF MatTL = 7 THEN MatTL$ = "Light composite materials, advanced ceramics."
IF MatTL = 8 THEN MatTL$ = "Composite laminate materials. Earliest experiments with nanotechnology."
IF MatTL = 9 THEN MatTL$ = "Crystalline iron and other super-strong allotropes of industrial metals."
IF MatTL = 10 THEN MatTL$ = "Crystalline iron and other super-strong allotropes of industrial metals."
IF MatTL = 11 THEN MatTL$ = "Crystalline iron and other super-strong allotropes of industrial metals."
IF MatTL = 12 THEN MatTL$ = "Gravitic manipulation allows the creation of superdense materials. Nanotechnology finally becomes practical for industrial applications, although these are very limited."
IF MatTL = 13 THEN MatTL$ = "Techniques for reinforcing the electron bonds in superdense material, increasing its strength further. Industrial nanotechnology spreads into new applications, reducing production costs for many items and producing new materials."
IF MatTL > 13 THEN MatTL$ = "Bonded superdense, Incremental improvements in structural materials and nanotechnology."


MatTL$ = "MATERIAL" + STR$(MatTL) + " : " + MatTL$




'Energy tech Level
ENUL = HCTL + INT(HCTL / 5)
ENLL = LCTM
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)

ENTL = HCTL + TLM

IF ENTL > ENUL THEN ENTL = ENUL
IF ENTL < ENLL THEN ENTL = ENLL

PRINT ENTL;
EneTL = ENTL

IF EneTL = 0 THEN EneTl$ = "All work is driven by muscle power, including the energy of domesticated animals and slave labor."
IF EneTL = 1 THEN EneTl$ = "Early water wheels used for irrigation."
IF EneTL = 2 THEN EneTl$ = "Advanced water wheels, including complex mechanisms to drive mills or other equipment."
IF EneTL = 3 THEN EneTl$ = "Advanced water wheels, including complex mechanisms to drive mills or other equipment. Windmills work on the same principle in areas with no running water."
IF EneTL = 4 THEN EneTl$ = "Advanced water wheels, Windmills, and mechanical means for energy storage (clockwork)."
IF EneTL = 5 THEN EneTl$ = "Steam engines, followed by the exploitation of wood and coal for fuel. Electrical energy appears late in the period, transmitted through local power grids and stored using simple chemical batteries."
IF EneTL = 6 THEN EneTl$ = "Internal combustion engines, large-scale exploitation of oil and natural gas. Electric power grids cover large urban areas and begin to reach the countryside."
IF EneTL = 7 THEN EneTl$ = "Fission power plants and radiothermal generators. Electric power grids on continental scale. Energy storage includes advanced chemical batteries and fuel cells."
IF EneTL = 8 THEN EneTl$ = "Advanced radiothermal generators (NPUs). Solar power, especially in space. Microwave beamed power. Superconducting materials make power transmission much more efficient and lead to the development of advanced power cells."
IF EneTL = 9 THEN EneTl$ = "First fusion power plants. Energy grids have become global in scope."
IF EneTL = 10 THEN EneTl$ = "Fusion power plants. Energy grids are global in scope."
IF EneTL = 11 THEN EneTl$ = "Fusion power plants. Energy grids are global in scope."
IF EneTL = 12 THEN EneTl$ = "Advanced fusion power plants are the backbone of the planetary energy economy."
IF EneTL = 13 THEN EneTl$ = "Advanced fusion power plants are the backbone of the planetary energy economy."
IF EneTL = 14 THEN EneTl$ = "Incremental improvements in fusion power technology. "
IF EneTL > 14 THEN EneTl$ = "First large-scale production of antimatter. The new material has industrial uses, but is very expensive and strictly controlled. Antimatter-based power is not yet practical."

EneTl$ = "ENERGY" + STR$(EneTL) + " : " + EneTl$




'Computer / Robotics
CRUL = ENUL
CRLL = INT(CRUL / 3)
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)
PLM = 0
IF pop < 6 THEN PLM = 1
IF pop > 8 THEN PLM = -1
CRTL = CRUL + TLM + PLM

IF CRTL > CRUL THEN CRTL = CRUL
IF CRTL < CRLL THEN CRTL = CRLL

PRINT CRTL;

COMRO = CRTL

IF COMRO = 0 THEN COMRO$ = "Memorized messages and oral tradition. Whole-number mathematics. "
IF COMRO = 1 THEN COMRO$ = "Rational-number arithmetic."
IF COMRO = 2 THEN COMRO$ = "Incremental improvements in mathematics, often including a systematic study of geometry and formal logic."
IF COMRO = 3 THEN COMRO$ = "Algebra and trigonometry."
IF COMRO = 4 THEN COMRO$ = "Movable-type printing, making printed material cheap aplace. Development of calculus."
IF COMRO = 5 THEN COMRO$ = "Personal printing devices (typewriters). Mechanical calculating devices."
IF COMRO = 6 THEN COMRO$ = "Massive electrical calculating devices for specialized applications."
IF COMRO = 7 THEN COMRO$ = "Programmable electronic computers, rapidly becoming small and cheap enough to allow personal use on the desktop."
IF COMRO = 8 THEN COMRO$ = "Almost every device has a small, specialized computer in it (ubiquitous computing). Computers can be programmed in high-level languages that approach the ease of natural language. Meanwhile, computers can be used (not programmed) in natural language via smooth voice recognition and real-time linguistic transcription."
IF COMRO = 9 THEN COMRO$ = "Linguistic interfaces improve, allowing computers to be programmed exclusively in natural language. Computers are capable of reasonable linguistic interpretation."
IF COMRO = 10 THEN COMRO$ = "Linguistic interfaces improve, allowing computers to be programmed exclusively in natural language. Computers are capable of reasonable linguistic interpretation."
IF COMRO = 11 THEN COMRO$ = "Linguistic interfaces improve, allowing computers to be programmed exclusively in natural language. Computers are capable of reasonable linguistic interpretation."
IF COMRO = 12 THEN COMRO$ = "Artificial intelligence makes computers capable of some of the same self-programming capability and flexibility as biological intelligence."
IF COMRO = 13 THEN COMRO$ = "Artificial intelligence makes computers capable of some of the same self-programming capability and flexibility as biological intelligence."
IF COMRO = 14 THEN COMRO$ = "Real-time linguistic analysis and translation. Small computer brains of roughly human intelligence, allowing the construction of convincing humanoid robots."
IF COMRO > 14 THEN COMRO$ = "Pseudo-reality computers allow the perfect simulation of physical reality and living personalities. First experiments with true machine consciousness."

COMRO$ = "COMPUTER / ROBOTICS" + STR$(COMRO) + " : " + COMRO$


'Communications
COUL = ENTL
COLL = INT(COUL / 3)
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)
COTL = CRTL + TLM
IF COTL > COUL THEN COTL = COUL
IF COTL < COLL THEN COTL = COLL

COMTL = COTL

IF COMTL = 0 THEN COMTL$ = "Communication by runner, with memorized messages and oral tradition."
IF COMTL = 1 THEN COMTL$ = "Communication by runner, writing is used almost exclusively by social elites for record-keeping."
IF COMTL = 2 THEN COMTL$ = "Communication by runner or rider, true literacy appears, as writing is used for many purposes other than simple record-keeping."
IF COMTL = 3 THEN COMTL$ = "Communication by runner or rider, block printing, too cumbersome to have broad impact."
IF COMTL = 4 THEN COMTL$ = "Long-distance communication by runner or rider is supplemented by organized semaphore networks."
IF COMTL = 5 THEN COMTL$ = "Longdistance communications via electric signals (telegraph and voice telephone)."
IF COMTL = 6 THEN COMTL$ = "Radio telegraph and voice transmission."
IF COMTL = 7 THEN COMTL$ = "Advanced radio communications include radio telephony, image transmission (television), satellite relays, and digital signaling."
IF COMTL = 8 THEN COMTL$ = "Fiber-optic networks and digital signals are applied to a global information network. Personal image transmission (videophone, flatscreen TV)."
IF COMTL = 9 THEN COMTL$ = "Instant planetary communication. Holographic (three-V) telephones and media."
IF COMTL = 10 THEN COMTL$ = "Instant planetary communication. Holographic (three-V) telephones and media."
IF COMTL = 11 THEN COMTL$ = "Instant planetary communication. Holographic (three-V) telephones and media."

Commolaw = INT(15 - tec + law + gov) / 3: IF Commolaw < 8 GOTO 35050
IF Commolaw > 8 THEN COMTL$ = COMTL$ + " Personal comunicators are not allowed. Only official communication systems can be used.": GOTO 35050



35050
commorange = tec * 5

IF zon$ <> "R" AND stp$ <> "X" AND tec < 8 AND Commolaw < 9 THEN COMTL$ = COMTL$ + " Not regarding the lacking technology, small, imported, communication devices are the norm. Due to the lack of a satellite network, their range outside settlements is usually only" + STR$(commorange) + " km."




35100

IF COMTL < 12 THEN COMTL$ = "COMMUNICATIONS" + STR$(COMTL) + " : " + COMTL$



PRINT COTL;
'Medical
MEUL = ENTL
MELL = 0
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)
METL = CRTL + TLM
IF METL > MEUL THEN METL = MEUL
IF METL < MELL THEN METL = MELL

PRINT METL;
MedTL = METL

IF MedTL = 0 THEN MedTL$ = "Shamanistic medicine, herbalism."
IF MedTL = 1 THEN MedTL$ = "Advanced herbalism, crude surgery."
IF MedTL = 2 THEN MedTL$ = "First formal description of medical diagnosis. Medical knowledge is recorded and expanded through empirical observation."
IF MedTL = 3 THEN MedTL$ = "Formal description of medical diagnosis. Medical knowledge is recorded and expanded through empirical observation. Incremental improvements in medical diagnosis and treatment."
IF MedTL = 4 THEN MedTL$ = "Formal description of medical diagnosis. Medical knowledge is recorded and expanded through empirical observation. Incremental improvements in medical diagnosis and treatment."
IF MedTL = 4 THEN MedTL$ = "Further incremental advances in medicine."
IF MedTL = 5 THEN MedTL$ = "Experimental vaccination techniques. Reliable anesthetics and antiseptics make radical surgery practical for the first time."
IF MedTL = 6 THEN MedTL$ = "Mass vaccination techniques. Development of antibiotics lead to the first reliable cures for disease. Rapid development of surgical techniques. Workable prosthetic limbs."
IF MedTL = 7 THEN MedTL$ = "Advanced antibiotics. Eradication of specific infectious diseases through universal vaccination. Invasive surgery reaches its highest level of development. Major organ transplants, surgical reattachment of limbs. Experimental genetic therapies."
IF MedTL = 8 THEN MedTL$ = "Lasers and microprobes make surgery minimally invasive, allowing greater efficiency and very rapid recovery. Full genetic screening and complex genetic therapies. Previously unknown infectious diseases can be analyzed and cured quickly."
IF MedTL = 9 THEN MedTL$ = "Experimental nanosurgical techniques. Mechanical implants can be integrated with peripheral or sensory nerves, allowing a variety of bionic replacement organs. Clone transplants are available."
IF MedTL = 10 THEN MedTL$ = "Experimental nanosurgical techniques. Mechanical implants can be integrated with peripheral or sensory nerves, allowing a variety of bionic replacement organs. Clone transplants are available."
IF MedTL = 11 THEN MedTL$ = "Experimental nanosurgical techniques. Mechanical implants can be integrated with peripheral or sensory nerves, allowing a variety of bionic replacement organs. Clone transplants are available."
IF MedTL = 12 THEN MedTL$ = "Practical nanosurgery (still too expensive for widespread use). Partial panimmunity. Neural implants can interface directly with the brain, treating certain kinds of mental dysfunction. Cheap, reliable clone transplants."
IF MedTL = 13 THEN MedTL$ = "Practical nanosurgery (still too expensive for widespread use). Partial panimmunity. Neural implants can interface directly with the brain, treating certain kinds of mental dysfunction. Cheap, reliable clone transplants."
IF MedTL = 14 THEN MedTL$ = "Practical nanosurgery. Brain implants can augment natural brain functions or modify an existing personality. Memories can be erased, implanted or modified. Crude antiagathic treatments"
IF MedTL > 14 THEN MedTL$ = "Total panimmunity, reliable antiagathic treatments. "

MedTL$ = "MEDICINE" + STR$(MedTL) + " : " + MedTL$


BIUL = METL
BILL = 0
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)
BITL = METL + TLM
IF BITL > BIUL THEN BITL = BIUL
IF BITL < BILL THEN BITL = BILL

PRINT bIETL;
BioTL = BITL


IF BioTL = 0 THEN BioTL$ = "Useful animals and plants are domesticated. Basic principles of animal husbandry and/or agriculture."
IF BioTL = 1 THEN BioTL$ = "Selective breeding of domesticated species for desired characteristics, probably with uncontrolled side effects."
IF BioTL = 2 THEN BioTL$ = "Basic understanding of anatomy, possibly more detailed for animal than for sentient species. First formal description of the art of medical diagnosis."
IF BioTL = 3 THEN BioTL$ = "Incremental improvements in anatomy and physiology. Crude sciences of chemistry (alchemy) and pharmacology."
IF BioTL = 4 THEN BioTL$ = "Further incremental advances in biological understanding. Discovery of microorganisms, and of the cellular structure of living tissue."
IF BioTL = 5 THEN BioTL$ = "Theories of heredity and evolution put biology on a rigorous footing. Early cell biology. Germ theory of infectious disease."
IF BioTL = 6 THEN BioTL$ = "Biochemistry. Discovery of viruses. Growing awareness of the nature of ecosystems."
IF BioTL = 7 THEN BioTL$ = "Discovery of DNA (or the local equivalent), allowing crude genome mapping. Experimental genetic engineering."
IF BioTL = 8 THEN BioTL$ = "Genome mapping extends even to complex organisms. Complex genetic therapies, experimental germ-line engineering. Systems ecology(detailed understanding of specific ecosystems)."
IF BioTL = 9 THEN BioTL$ = "Full theory of genetic morphology allows radical modification of species. First integration of mechanical implants with peripheral or sensory nerves. Experimental nanosurgery."
IF BioTL = 10 THEN BioTL$ = "Full theory of genetic morphology allows radical modification of species. Integration of mechanical implants with peripheral or sensory nerves. Nanosurgery."
IF BioTL = 11 THEN BioTL$ = "Full theory of genetic morphology allows radical modification of species. Total integration of mechanical implants with peripheral or sensory nerves. Standard nanosurgery."
IF BioTL = 12 THEN BioTL$ = "Practical nanosurgery. First brain implants. Partial theory of aging."
IF BioTL = 13 THEN BioTL$ = "Nanosurgery is standard. Brain implants. Theory of aging."
IF BioTL = 14 THEN BioTL$ = "Nanosurgery is standard. Brain implants. Theory of aging, theory of memory. Full understanding of the brain's mechanisms."
IF BioTL > 14 THEN BioTL$ = "Nanosurgery is standard. Brain implants. Theory of aging. Full understanding of the body's immune systems and aging process"

BioTL$ = "BIOLOGY" + STR$(BioTL) + " : " + BioTL$




'Environment
EVUL = ENTL
EVLL = EVUL - 5
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)
ATMM = 0
IF atm <> 5 AND atm <> 6 AND atm <> 8 THEN ATMM = 1
IF hyd = 0 OR hyd = 10 THEN ATMM = ATMM + 1
EVTL = HCTL + TLM + ATMM
IF EVTL > EVUL THEN EVTL = EVUL
IF EVTL < EVLL THEN EVTL = EVLL

PRINT EVTL,

EnvTL = EVTL

IF EnvTL = 0 THEN EnvTL$ = "Hunting cultures may overhunt certain game animals to extinction. Primitive agriculture. Villages and towns (PR 3)."
IF EnvTL = 1 THEN EnvTL$ = "Cities (PR 4) and large-scale irrigation projects. "
IF EnvTL = 2 THEN EnvTL$ = "Large cities (PR 5), aqueducts and canals. Deliberate deforestation and swamp drainage to increase agricultural land."
IF EnvTL = 3 THEN EnvTL$ = "Large cities (PR 5), aqueducts and canals. Advanced agricultural techniques (crop rotation, crude fertilization). "
IF EnvTL = 4 THEN EnvTL$ = "Large cities (PR 5), aqueducts and canals. Advanced agricultural techniques (crop rotation, crude fertilization)."
IF EnvTL = 5 THEN EnvTL$ = "Improved sanitation allows very large cities (PR 6)."
IF EnvTL = 6 THEN EnvTL$ = "Skyscrapers. Crude weather prediction. Conservationist ethic."
IF EnvTL = 7 THEN EnvTL$ = "Megalopolitan areas appear, as mass transit allows rapid expansion of suburbs. First space stations. Advanced weather prediction. Environmentalism."
IF EnvTL = 8 THEN EnvTL$ = "Arcologies. Large orbital settlements, with near-complete recycling efficiency. Crude weather control, long-term climate prediction. Experiments in reconstructing damaged ecosystems."
IF EnvTL = 9 THEN EnvTL$ = "Orbital and deep-space settlements with complete recycling efficiency (microworlds). Similar techniques make arcologies common in crowded planetary urban centers. Practical weather control. Terraforming can cause gradual change in planetary environments."
IF EnvTL = 10 THEN EnvTL$ = "Orbital and deep-space settlements with complete recycling efficiency (microworlds). Similar techniques make arcologies common in crowded planetary urban centers. Practical weather control. Terraforming can cause gradual change in planetary environments."
IF EnvTL = 11 THEN EnvTL$ = "Orbital and deep-space settlements with complete recycling efficiency (microworlds). Similar techniques make arcologies common in crowded planetary urban centers. Practical weather control. Terraforming can cause gradual change in planetary environments."
IF EnvTL = 12 THEN EnvTL$ = "Gravitic support of large buildings. Grav technology allows stationary floating cities late in the period. Advanced weather control. Incremental advances in terraforming techniques, advanced ecosystem reconstruction."
IF EnvTL = 13 THEN EnvTL$ = "Gravitic support of large buildings. Grav technology allows stationary floating cities late in the period. Advanced weather control. Incremental advances in terraforming techniques, advanced ecosystem reconstruction."
IF EnvTL = 14 THEN EnvTL$ = "Advances in grav technology allow mobile floating cities. Further incremental advances in terraforming. Whole ecosystems can now be created from scratch (ecopoiesis)."
IF EnvTL > 14 THEN EnvTL$ = "Terraforming can produce radical and (relatively) fast change in planetary environments."



EnvTL$ = "ENVIRONMENTAL" + STR$(EnvTL) + " : " + EnvTL$





'Land Transport
TraTL$ = ""

LTUL = ENUL
LTLL = LTUL - 5
ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)

HTMM = 0
IF hyd > 9 THEN HTMM = -1
LTTL = ENTL + TLM + HTMM
IF LTTL > LTUL THEN LTTL = LTUL
IF LTTL < LTLL THEN LTTL = LTLL

PRINT LTTL;
TraTL = LTTL

IF TraTL = 0 THEN TraTL$ = "All land travel is on foot, possibly with the help of draft animals."
IF TraTL = 1 THEN TraTL$ = "The wheel allows draft animals to pull carts or chariots."
IF TraTL = 2 THEN TraTL$ = "Road networks and bridges."
IF TraTL = 3 THEN TraTL$ = "Road networks and bridges with improved animal harness, animal shoeing, and so on."
IF TraTL = 4 THEN TraTL$ = "Road networks and bridges. Fast long-distance travel by coach."
IF TraTL = 5 THEN TraTL$ = "Railroad lines are supplemented by fast stagecoach service in frontier areas."
IF TraTL = 6 THEN TraTL$ = "Fast railways and electric trains. Personal automobiles."
IF TraTL = 7 THEN TraTL$ = "Fast railways and electric trains. Advanced automobiles."
IF TraTL = 8 THEN TraTL$ = "Fast railways and electric trains. Electric automobiles."
IF TraTL = 9 THEN TraTL$ = "Ultra-fast rail lines, comparable in speed to hypersonic aircraft but restricted to underground tunnels. Personal grav cars."
IF TraTL = 10 THEN TraTL$ = "Ultra-fast rail lines, comparable in speed to hypersonic aircraft but restricted to underground tunnels. Personal grav cars."
IF TraTL = 11 THEN TraTL$ = "Ultra-fast rail lines, comparable in speed to hypersonic aircraft but restricted to underground tunnels. Personal grav cars."
IF TraTL = 12 THEN TraTL$ = "Grav vehicles are the basis for all forms of transportation (land, sea, air and space). Personal grav belts are available."
IF TraTL > 12 THEN TraTL$ = "Grav vehicles are the basis for all forms of transportation (land, sea, air and space). Personal grav belts are the preferred medium of transport."

LTTL$ = "TRANSPORTATION" + STR$(LTTL) + " : " + TraTL$




'Water Transport

TraTL$ = ""
WTUL = LTTL
WTLL = WTUL - 5

ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)

WTMM = 0
IF hyd < 1 THEN WTMM = -1

WTTL = LTTL + TLM + WTMM
IF WTTL < WTUL THEN WTTL = WTUL
IF WTTL < WTLL THEN WTTL = WTLL
PRINT WTTL;

TraTL = WTTL

IF TraTL = 0 AND hyd > 0 THEN TraTL$ = "Water travel is by raft or canoe."
IF TraTL = 1 AND hyd > 0 THEN TraTL$ = "Small rowed ships and single-masted sailing ships can cross short stretches of open water."
IF TraTL = 2 AND hyd > 1 THEN TraTL$ = " Large, specialized rowed ships (triremes, galleys) and double-masted sailing ships."
IF TraTL = 3 AND hyd > 1 THEN TraTL$ = " Sailing vessels now strike out across open ocean, although navigational methods are still rudimentary."
IF TraTL = 4 AND hyd > 1 THEN TraTL$ = " Sailing vessels use complex square-rigging techniques. Dead-reckoning navigation leads to long-distance voyages of discovery."
IF TraTL = 5 AND hyd > 2 THEN TraTL$ = " Steam-powered ships replace sailing vessels for short-distance applications. Precise navigation makes planet-wide sea transport routine. "
IF TraTL = 6 AND hyd > 3 THEN TraTL$ = " Massive steam-powered sea vessels, supplemented by the first practical submersibles."
IF TraTL = 7 AND hyd > 2 THEN TraTL$ = " Ocean-going vessels and submersibles use nuclear power. Hovercraft and hydrofoils for specialized purposes."
IF TraTL = 8 AND hyd > 0 THEN TraTL$ = " Incremental improvements in water-transport technology, including triphibian vehicles."

IF hyd > 0 AND TraTL < 9 THEN WTTL$ = "WATER" + STR$(WTTL) + " : " + TraTL$




'Air transport
TraTL$ = ""
ATUL = LTUL
ATLL = ATUL
IF atm < 1 AND ATUL < 9 THEN ATLL = 0

ATLL = ATUL - 5: IF ATLL < 2 THEN ATLL = 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)

ATTL = ENTL + TLM
IF ATTeL > ATUL THEN ATTL = ATUL
IF ATTL < ATLL THEN ATTL = ATLL

IF LTTL > 9 THEN ATTL = LTTL

PRINT ATTL;
TraTL = ATTL
IF TraTL = 3 AND atm > 3 THEN TraTL$ = " Experimenting with rockets and kites."
IF TraTL = 4 AND atm > 3 THEN TraTL$ = " Theoretical designs for simple aircraft."
IF TraTL = 5 AND atm > 3 THEN TraTL$ = " Hot-air balloons and experimental propeller aircraft."
IF TraTL = 6 AND atm > 3 THEN TraTL$ = " Advanced balloons and dirigibles become practical for slow, long-range applications. Advanced propeller-driven aircraft, supplemented by early jet aircraft."
IF TraTL = 7 AND atm > 3 THEN TraTL$ = " Jet aircraft dominate air transport, with some models attaining supersonic speeds."
IF TraTL = 8 AND atm > 3 THEN TraTL$ = " Hypersonic and semi-ballistic aircraft are in use."

IF atm > 3 AND TraTL < 9 AND TraTL > 2 THEN ATTL$ = "AIR" + STR$(ATTL) + " : " + TraTL$





'Space transport
TraTL$ = ""
STUL = ENUL
STLL = STUL - 3

ZW6 = INT(RND * 6) + INT(RND * 6) + 2
TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)

XTL = ENTL: IF CRTL < XTL THEN XTL = CRTL

STM = 0
IF stp$ = "A" OR atp$ = "B" THEN STM = 1

STTL = XTL + TLM + STM

IF STTL < STUL THEN STTL = STUL
IF STTL < STLL THEN STTL = STLL

PRINT STTL;
TraTL = STTL
IF TraTL = 6 THEN TraTL$ = "First suborbital rockets."
IF TraTL = 7 THEN TraTL$ = "Manned orbital rockets allow exploration of near-planetary space. Nuclear-powered spacecraft (the Orion concept)."
IF TraTL = 8 THEN TraTL$ = "Lightsails and practical fission and ion drives allow manned interplanetary missions. New advances in physical science lead to experimental contragrav vehicles and reactionless thrusters. "
IF TraTL = 9 THEN TraTL$ = "Fusion rockets become practical, although the new reactionless drives are much faster and more efficient. Starships are barely possible but only with Jump-1 drives."
IF TraTL = 10 THEN TraTL$ = "Fusion rockets become practical, although the new reactionless drives are much faster and more efficient. Starships can be build with Jump-1 drives."
IF TraTL = 11 THEN TraTL$ = "Fusion rockets become practical, although the new reactionless drives are much faster and more efficient. Starships can be equipped with Jump-2 drives."
IF TraTL = 12 THEN TraTL$ = "Jump-drive 3 is available."
IF TraTL = 13 THEN TraTL$ = "Jump-drive 3 is available."
IF TraTL = 14 THEN TraTL$ = "Antimatter rockets appear but are almost never used, being more inefficient (and more dangerous) than reactionless drives. Jump-4 and jump-5 drives are available."
IF TraTL > 14 THEN TraTL$ = "Incremental improvements in maneuver drive technology. Appearance of jump-6 drive."

IF TraTL > 5 THEN STTL$ = "SPACE" + STR$(STTL) + " : " + TraTL$

'Personal military
PMUL = ENUL
PMLL = 0

TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)

PMTL = INT(ENTL + TLM + acceptance / 4 + strangeness / 4)

IF PMTL > PMUL THEN PMTL = PMUL
IF PMTL < PMLL THEN PMTL = PMLL

PRINT PMTL;
WeaTL = PMTL

IF WeaTL = 0 THEN WeaTl$ = "Stone knives, cudgels, stone-pointed spears and arrows."
IF WeaTL = 1 THEN WeaTl$ = "Bronze stabbing swords and spear points, Daggers and Pikes are in use."
IF WeaTL = 2 THEN WeaTl$ = "Halberds, Broadswords and early matchlock guns are standard."
IF WeaTL = 3 THEN WeaTl$ = "Foil, Cutlass, Blade, Bayonet, Flintlock and Matchlocks are in personal use."
IF WeaTL = 4 THEN WeaTl$ = "Revolvers and Shotguns are in personal use."
IF WeaTL = 5 THEN WeaTl$ = "Carbines, Rifles, Pistols, and Submachineguns are in personal use."
IF WeaTL = 6 THEN WeaTl$ = "Auto rifles, Missiles, Light machine guns, Missile launchers are in personal use."
IF WeaTL = 7 THEN WeaTl$ = "Body pistols, Assault rifles and Grenade launcher are in personal use."
IF WeaTL = 8 THEN WeaTl$ = "Laser carbines and Snub pistol are the Standard. Rifles now use caseless ammunition."
IF WeaTL = 9 THEN WeaTl$ = "Laser rifle and Accelerator rifles are available."
IF WeaTL = 10 THEN WeaTl$ = "ACR, the firearm attains its full maturity with advanced caseless-ammunition weapons."
IF WeaTL = 11 THEN WeaTl$ = "ACR, the firearm attains its full maturity with advanced caseless-ammunition weapons."
IF WeaTL = 12 THEN WeaTl$ = "Laser and gauss rifles are the standard infantry weapons, while plasma rifles (PGMP) enter use for squad support."
IF WeaTL = 13 THEN WeaTl$ = "Laser and gauss rifles are the standard infantry weapon, plasma rifles (PGMP) are used for squad support."
IF WeaTL = 14 THEN WeaTl$ = "Laser and gauss rifles are the standard infantry weapon, Fusion guns like the FGMP-14 appear in the squad-support role."
IF WeaTL > 14 THEN WeaTl$ = "Laser and gauss rifles are the standard infantry weapons, Fusion guns, FGMP-15 is the standard squad-support weapon. Experimental neural weapons and stunners are available but not generally used in military applications."

WeaTl$ = "PERSONAL WEAPONS" + STR$(WeaTL) + " : " + WeaTl$


'    TL$ = "PERSONAL" + STR$(PweTL) + " : " + PweTL$



HMUL = ENUL
HMLL = 0

TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)

HMTL = INT(ENTL + TLM + acceptance / 4 + strangeness / 4)

IF HMTL > HMUL THEN HMTL = HMUL
IF HMTL < HMLL THEN HMTL = HMLL

PRINT HMTL
HMTL = HMTL

IF HMTL = 1 THEN hmtl$ = "Springpowered stone-and bolt-throwers."
IF HMTL = 2 THEN hmtl$ = "Torsionpowered catapults. Mass tactics (phalanx and legion) evolve. The composite bow becomes the primary battlefield weapon, possibly wielded by chariotry."
IF HMTL = 3 THEN hmtl$ = "Iron replaces bronze, leading to a revolution in military styles as more people can afford weapons. Counterweight-powered engines (trebuchets). A few crude gunpowder cannon (bombards) appear late in the period."
IF HMTL = 4 THEN hmtl$ = "Mass tactics involve techniques for managing volley fire). Cannons are now standard heavy artillery."
IF HMTL = 5 THEN hmtl$ = "Machine guns and heavy cannon. Abandonment of the old volley-fire techniques of mass combat, forcing a more open order and greater reliance on mobility."
IF HMTL = 6 THEN hmtl$ = "Advanced indirect-fire techniques. Self-propelled guns (tanks). Large air-dropped explosives, including (late in the period) fission bombs."
IF HMTL = 7 THEN hmtl$ = "Advanced AFVs dominate the battlefield, with air and infantry support. Fusion and enhanced-radiation bombs."
IF HMTL = 8 THEN hmtl$ = "Energy weapons appear in the heavy-support role (laser and particle cannon). Experimental laser small-arms are produced late in the period. "
IF HMTL = 9 THEN hmtl$ = "Laser weapons become more common on the smallunit scale. Exoskeletons produce powered infantry units. Plasma cannon."
IF HMTL = 10 THEN hmtl$ = "Laser weapons become more common on the smallunit scale. Exoskeletons produce powered infantry units. Plasma cannon."
IF HMTL = 11 THEN hmtl$ = "Laser weapons become more common on the smallunit scale. Combat armor becomes standard. Exoskeletons produce powered infantry units. Plasma cannon."
IF HMTL = 12 THEN hmtl$ = "Advanced (X-ray) lasers. Full-coverage powered armor. Heavy artillery includes fusion guns. Nuclear weapons suddenly go into decline with the appearance of the battlefield nuclear damper."
IF HMTL = 13 THEN hmtl$ = "Advanced (X-ray) lasers. Full-coverage powered armor. Heavy artillery includes fusion guns. Nuclear weapons suddenly go into decline with the appearance of the battlefield nuclear damper."
IF HMTL = 14 THEN hmtl$ = "Powered armor continues to improve. Antimatter warheads replace the near-obsolete fission or fusion bombs."
IF HMTL > 14 THEN hmtl$ = "Blackglobe technology is sometimes applied in deep-space combat."

IF HMTL > 0 THEN hmtl$ = "HEAVY WEAPONS" + STR$(HMTL) + " : " + hmtl$


PRINT


' Tech Level Descriptions
pwex = 0




BioTL = METL
BIUL = METL
BILL = METL / 2


TLM = 0
IF ZW6 = 2 THEN TLM = (INT(RND * 6) + 1) * -1
IF ZW6 = 3 THEN TLM = -2
IF ZW6 = 3 THEN TLM = -1
IF ZW6 = 10 THEN TLM = 1
IF ZW6 = 11 THEN TLM = 2
IF ZW6 = 12 THEN TLM = (INT(RND * 6) + 1)

BioTL = METL + TLM

IF BioTL > BIUL THEN BioTL = BIUL
IF BioTL < BILL THEN BioTL = BILL

IF BioTL = 0 THEN BioTL$ = "Useful animals and plants are domesticated. Basic principles of animal husbandry and/or agriculture."
IF BioTL = 1 THEN BioTL$ = "Selective breeding of domesticated species for desired characteristics, probably with uncontrolled side effects."
IF BioTL = 2 THEN BioTL$ = "Basic understanding of anatomy, possibly more detailed for animal than for sentient species. First formal description of the art of medical diagnosis."
IF BioTL = 3 THEN BioTL$ = "Incremental improvements in anatomy and physiology. Crude sciences of chemistry (alchemy) and pharmacology."
IF BioTL = 4 THEN BioTL$ = "Further incremental advances in biological understanding. Discovery of microorganisms, and of the cellular structure of living tissue."
IF BioTL = 5 THEN BioTL$ = "Theories of heredity and evolution put biology on a rigorous footing. Early cell biology. Germ theory of infectious disease."
IF BioTL = 6 THEN BioTL$ = "Biochemistry. Discovery of viruses. Growing awareness of the nature of ecosystems."
IF BioTL = 7 THEN BioTL$ = "Discovery of DNA (or the local equivalent), allowing crude genome mapping. Experimental genetic engineering."
IF BioTL = 8 THEN BioTL$ = "Genome mapping extends even to complex organisms. Complex genetic therapies, experimental germ-line engineering. Systems ecology(detailed understanding of specific ecosystems)."
IF BioTL = 9 THEN BioTL$ = "Full theory of genetic morphology allows radical modification of species. First integration of mechanical implants with peripheral or sensory nerves. Experimental nanosurgery."
IF BioTL = 10 THEN BioTL$ = "Full theory of genetic morphology allows radical modification of species. First integration of mechanical implants with peripheral or sensory nerves. Experimental nanosurgery."
IF BioTL = 11 THEN BioTL$ = "Full theory of genetic morphology allows radical modification of species. First integration of mechanical implants with peripheral or sensory nerves. Experimental nanosurgery."
IF BioTL = 12 THEN BioTL$ = "Practical nanosurgery. First brain implants. Partial theory of aging."
IF BioTL = 13 THEN BioTL$ = "Practical nanosurgery. First brain implants. Partial theory of aging."
IF BioTL = 14 THEN BioTL$ = "Theory of memory. Full understanding of the brain's mechanisms."
IF BioTL > 14 THEN BioTL$ = "Full understanding of the body's immune systems and aging process"

BioTL$ = "BIOLOGY" + STR$(BioTL) + " : " + BioTL$

Tech$ = SciTl$ + " " + MatTL$ + " " + EneTl$ + " " + InfTl$ + " " + COMRO$
Trans$ = LTTL$ + " " + WTTL$ + " " + ATTL$ + " " + STTL$
Wtech$ = WeaTl$ + " " + HTML$
Btech$ = BioTL$ + " " + MedTL$ + " " + EnvTL$

RETURN

40000 'SEARCH





RETURN


3900 IF TSAO = 1 THEN GOTO 4900
' Reading the UPP String
DENEB = 0: deneb2 = 0: deneb3 = 0: deneb4 = 0
IF sector$ = "Deneb.DAT" THEN DENEB = -8: deneb2 = -1
IF sector$ = "Core.DAT" THEN DENEB = -19: deneb2 = 0
IF sector$ = "Old Expanses.DAT" THEN DENEB = -19: deneb2 = -1
IF sector$ = "Far Frontiers.DAT" THEN DENEB = -13: deneb2 = -4
IF sector$ = "Solomani Rim.DAT" THEN DENEB = -9: deneb2 = -1
IF sector$ = "Gateway.DAT" THEN DENEB = -18: deneb2 = -4
IF sector$ = "Ley.DAT" THEN DENEB = -7: deneb2 = 0
IF sector$ = "Trojan Reach.DAT" THEN DENEB = -14: deneb2 = 0
IF sector$ = "Fornast.DAT" THEN DENEB = -15: deneb2 = 0
IF sector$ = "Corridor.DAT" THEN DENEB = -14: deneb2 = -1
IF sector$ = "Dark Nebula.DAT" THEN DENEB = -8: deneb2 = -4
IF sector$ = "Reavers Deep.DAT" THEN DENEB = -13: deneb2 = -1
IF sector$ = "Vland.DAT" THEN DENEB = -14: deneb2 = -1
IF sector$ = "Gushemege.DAT" THEN DENEB = -14: deneb2 = -1
IF sector$ = "Dagudashaag.DAT" THEN DENEB = -2: deneb2 = -1
IF sector$ = "Lishun.DAT" THEN DENEB = -16: deneb2 = -1
IF sector$ = "Antares.DAT" THEN DENEB = -15: deneb2 = -1
IF sector$ = "Empty Quarter.DAT" THEN DENEB = -8: deneb2 = -1
IF sector$ = "Crucis Margin.DAT" THEN DENEB = -20: deneb2 = -4
IF sector$ = "Alpha Crucis.DAT" THEN DENEB = -12: deneb2 = -1
IF sector$ = "Gvurrdon.DAT" THEN DENEB = -14: deneb2 = -4: deneb3 = 5
IF sector$ = "Riftspan Reaches.DAT" THEN DENEB = -20: deneb2 = -5
IF sector$ = "Verge.DAT" THEN DENEB = -20: deneb2 = -1
IF sector$ = "Ilelish.DAT" THEN DENEB = -17: deneb2 = 0
IF sector$ = "Zarushagar.DAT" THEN DENEB = -14: deneb2 = 0
IF sector$ = "Massilia.DAT" THEN DENEB = -13: deneb2 = -1
IF sector$ = "Delphi.DAT" THEN DENEB = -20: deneb2 = -1
IF sector$ = "Interstellar Wars.DAT" THEN DENEB = -32: deneb2 = 0: deneb4 = -1
IF sector$ = "Diaspora.DAT" THEN DENEB = -13: deneb2 = 0: deneb4 = 0
IF sector$ = "Newworld.DAT" THEN DENEB = -12: deneb2 = -4
IF sector$ = "Aldebaran.DAT" THEN DENEB = -17: deneb2 = -4
IF sector$ = "Canopus.DAT" THEN DENEB = -19: deneb2 = -4
IF sector$ = "Magyar.DAT" THEN DENEB = -17: deneb2 = -1
IF sector$ = "Glimmerdrift.DAT" THEN DENEB = -20: deneb2 = -1
IF sector$ = "Hinterworld.DAT" THEN DENEB = -16: deneb2 = -4
IF sector$ = "Spica.DAT" THEN DENEB = -17: deneb2 = -4
IF sector$ = "Langere.DAT" THEN DENEB = -11: deneb2 = -4
IF sector$ = "Ustral Quadrant.DAT" THEN DENEB = -14: deneb2 = -4
IF sector$ = "Reft Sector.DAT" THEN DENEB = -14: deneb2 = -2



' Hex  Name                 UWP       Remarks                                  {Ix}   (Ex)    [Cx]   N     B  Z PBG W  A    Stellar
' ---- -------------------- --------- ---------------------------------------- ------ ------- ------ ----- -- - --- -- ---- --------------
' 0101 Zeycude              C430698-9 De Na Ni Po                              { -1 } (C53-1) [6559] -     -  - 613 8  ZhCo K9 V
'          1         2         3         4         5         6         7         8         9         0         1         2
' 0102 Cauldron             D988222-4 Lo                                       { -3 } (411-5) [1111] B     -  - 403 7  ImDd M0 V
' 0102 Cauldron             D988222-4 Lo                         { -3 } (411-5) [1111] B   -  - 403 7  ImDd M0 V



t$ = MID$(upp$, 1, 4)
name$ = MID$(upp$, 6, 31)

stp$ = MID$(upp$, 27 + deneb3, 1)
siz$ = MID$(upp$, 28 + deneb3, 1)
atm$ = MID$(upp$, 29 + deneb3, 1)
hyd$ = MID$(upp$, 30 + deneb3, 1)
pop$ = MID$(upp$, 31 + deneb3, 1)
gov$ = MID$(upp$, 32 + deneb3, 1)
law$ = MID$(upp$, 33 + deneb3, 1)

tec$ = MID$(upp$, 35 + deneb3, 1)

tra$ = MID$(upp$, 37 + deneb3, 47)

ix$ = MID$(upp$, 80 + DENEB, 2)
ex$ = MID$(upp$, 86 + DENEB, 5)
cx$ = MID$(upp$, 94 + DENEB, 4)

zon$ = MID$(upp$, 109 + DENEB + deneb2, 1):
bas$ = MID$(upp$, 106 + DENEB, 2)
pox$ = MID$(upp$, 50, 1)
MUL$ = MID$(upp$, 111 + DENEB + deneb2, 1)
asg$ = MID$(upp$, 112 + DENEB + deneb2, 1)
gas$ = MID$(upp$, 113 + DENEB + deneb2, 1)
worlds = VAL(MID$(upp$, 115 + DENEB + deneb2, 2))

REM PRINT zon$, bax$; pox$, MUL$; asg4; gas$, "XXXXX"; MID$(upp$, 109 + DENEB, 10): GOSUB 1000


pol$ = MID$(upp$, 118 + DENEB + deneb2, 4)
pol$ = LEFT$(pol$, 2)
sol$ = MID$(upp$, 123 + DENEB + deneb2 + deneb4, 14)
sox$ = LEFT$(sol$, 2)
so1$ = ""
FOR r = 4 TO 6
    m$ = MID$(sol$, r, 1)
    IF m$ <> " " THEN so1$ = so1$ + m$ ELSE r = 7
NEXT
IF so1$ = "II" THEN mag$ = "2"
IF so1$ = "III" THEN mag$ = "3"
IF so1$ = "V" THEN mag$ = "5"
IF so1$ = "VI" OR so1$ = "D" THEN mag$ = "6": mag$ = "5"
IF so1$ = "IV" THEN mag$ = "4"
' sox$ = sox$ + so1$
sox$ = sox$ + mag$
'PRINT sol$, sox$; so1$, mag$, solz; s2; mag: GOSUB 1000
' PRINT sol$;
' IF RIGHT$(upp$, 1) = "ø" THEN datx$ = RIGHT$(upp$, 12): upp$ = LEFT$(upp$, LEN(upp$) - LEN(datx$)) ELSE datx$ = ""
RETURN








4900 'Cepheus
' Hex  Name                   UWP         BaseG   Remarks             Temp
'          1         2         3         4         5         6         7         8         9         0         1         2
' ----    --------------------    ---------   --- -   ----------------    ------- --  ---         ---
' Adlet                   0603 C310442-B          Ni                Frozen      Tr   Wolf 424                     M6V M6V

t$ = MID$(upp$, 25, 4)
stp$ = MID$(upp$, 30, 1)
siz$ = MID$(upp$, 31, 1)
atm$ = MID$(upp$, 32, 1)
hyd$ = MID$(upp$, 33, 1)
pop$ = MID$(upp$, 34, 1)
gov$ = MID$(upp$, 35, 1)
law$ = MID$(upp$, 36, 1)

tec$ = MID$(upp$, 38, 1)

tra$ = MID$(upp$, 49, 19)

'ix$ = MID$(upp$, 80 + DENEB, 2)
'ex$ = MID$(upp$, 86 + DENEB, 5)
'cx$ = MID$(upp$, 94 + DENEB, 4)

'zon$ = MID$(upp$, 109 + DENEB + deneb2, 1):
bas$ = MID$(upp$, 40, 3)
'pox$ = MID$(upp$, 50, 1)
'MUL$ = MID$(upp$, 111 + DENEB + deneb2, 1)
'asg$ = MID$(upp$, 112 + DENEB + deneb2, 1)
gas$ = MID$(upp$, 47, 1)
'worlds = VAL(MID$(upp$, 115 + DENEB + deneb2, 2))
temp$ = MID$(upp$, 67, 7)
pol$ = MID$(upp$, 79, 2)
pol$ = LEFT$(pol$, 2)
sol$ = MID$(upp$, 113, 20)
sox$ = LEFT$(sol$, 2)
so1$ = ""
FOR r = 3 TO 5
    m$ = MID$(sol$, r, 1)
    IF m$ <> " " THEN so1$ = so1$ + m$ ELSE r = 7
NEXT
IF so1$ = "II" THEN mag$ = "2"
IF so1$ = "III" THEN mag$ = "3"
IF so1$ = "V" THEN mag$ = "5"
IF so1$ = "VI" OR so1$ = "D" THEN mag$ = "6": mag$ = "5"
IF so1$ = "IV" THEN mag$ = "4"
' sox$ = sox$ + so1$
sox$ = sox$ + mag$
sunname$ = MID$(upp$, 84, 29)
4910 IF RIGHT$(sunname$, 1) = " " THEN sunname$ = LEFT$(sunname$, LEN(sunname$) - 1): GOTO 4910
'PRINT sol$, sox$; so1$, mag$, solz; s2; mag: GOSUB 1000
' PRINT sol$;
' IF RIGHT$(upp$, 1) = "ø" THEN datx$ = RIGHT$(upp$, 12): upp$ = LEFT$(upp$, LEN(upp$) - LEN(datx$)) ELSE datx$ = ""
RETURN

