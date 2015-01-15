# **Samodejno odvajanje**
###Ekipa: ZaAB
######Člana: *Andrej Borštnik* in *Barbara Bajcer*

Prvotno bi naj najin projekt obsegal implementacijo samodejnega odvajanja. Skozi raziskovanje sva zašla tudi na druga zanimiva področja in posledično je sedaj projekt sestavljen iz večih (dokaj) nepovezanih podprojektov.

##Samodejno odvajanje
Pojma samodejno odvajanje ne smemo zamenjati s pojmoma *simbolično odvajanje* in *numerično odvajanje*. Pri samodejnem odvajanju izkoristimo dejstvo, da je vsaka funkcija (v Haskellu) sestavljena iz osnovnih aritmetičnih operacij ter elementarnih funkcij (exp, log, sin, cos itd.). Z uporabo verižnega pravila tako lahko samodejno izračunamo odvod poljubnega reda v neki točki.

Vrednost funkcije in njene odvode v neki točki bomo predstavili s strukturo `D: data D a = D a (Maybe (D a))`. Na primer struktura `D 1 Just (D 2 Nothing)` predstavlja nekaj, kar ima vrednost v neki točki 1, odvod v isti točki 2 in vse nadalnje odvode enake 0.

Uporaba (overdnotimo funkcijo v točki 1):
```haskell
f :: Floating a => a -> a
f z = (exp z) / z + 2 * (sqr z) * sin z

f (idD 1) ... 
```
Rezlutat:
```
4.401223798074838, 4.4464885509678655, 8.723642245019956, 
-10.131192415931453, -2.692662103532559, -123.30646837902931, 
782.1172570013406, -5018.950908456501, 40210.42287941638, 
-362926.1809439487, 3628971.6219290467, -3.991671902855549 e7, 
4.7900135379519105 e8, -6.227020923555244 e9, 8.71782915350399 e10, ...
```


##Levi in desni odvodi
Podobno kot pri samodejnem odvajanju računamo odvode, vendar se tu ustavimo pri prvem odvodu. 

Vrednost funkcije in odvoda predstavimo s strukturo `D: data D a = D a a a`. Struktura `D 2 -1 3` predstavlja nekaj, kar ima vrednost v neki točki 2, levi odvod -1 in desni odvod 3.

Uporaba je enaka, kot pri samodejnem odvajanju, le rezultat je drugačen. Za odvedljive fukcije sta levi in desni odvod enaka, tako da je uporaba te knjižnice nesmiselna. Prav pa pride, če imamo neodvedljive funkcije (npr. definirane z dvema predpisoma, ki se ne ujemata). Deluje samo kjer je funkcija definirana (torej ne v polih).



##Lipschitzove konstante
Ideja: funkcijo lokalno omejimo z dvema premicama (predstavljenima s koeficientoma) in definiramo pripadajočo aritmetiko. S tem lahko tudi zapletenejše funkcije (lokalno) omejimo in tako jih na primer lahko integriramo.


Funkcija, v kateri evaluiramo točko je lahko poljubno blizu pola, torej bi premici rezultat omejevali samo na poljubno majhni okolici. Sledi, da je omejevanje smiselno samo za okolice, ki so zelo blizu 0. Ob tej predpostavki sva aritmetiko definirala brez težav, se pa zalomi pri aplikaciji. 

Za funkcijo `f z = z` pričakujemo, da bo omejena npr. s konstantama `1 - eps` in `1 + eps`. S tako definicijo pokrijemo vse racionalne funkcije. Prav tako za funkcijo `f z = sin z` pričakujemo, da bo omejena z `coz z - eps` in `cos z + eps`. Tu se pojavi težava v implementaciji, ki je nisva uspela razrešiti. Program namreč ne ve kakšo vrsto aplikacije opravlja in tako vrača napačne rezultate, če apliciramo kako funkcijo iz `Floating` instance na racionalni funkciji (ki ni konstanta).

Vrednost funkcije, omejujoči konsanti in okolico predstavimo s strukturo `L: L a = L a a a a`. Struktura `L 0 1 -1 0.1` predstavlja nekaj, kar ima v neki točki vrednost 0 in v 0.1 okolici zgornjo konstanto (desno od točke) 1 ter spodnjo konstanto -1.

Uporaba (je različna za racionalne in `Floating` funkcije), `eps` pove kakšna je okolica, `con` pa kako blizu
1 oz. 0 postavimo začetni omejitvi:
```haskell
f1 :: Floating a => a -> a
f1 z = cos (sin z)

f1 (constL 2 con eps) ...

f2 :: Floating a => a -> a
f2 z = sqr z + z

f2 (idL 2 con eps) ...

f3 :: Floating a => a -> a -> a
f3 x y = sqr x * sin y

f3 (idL 2 con eps) (constL 2 con eps) ...
```
Množenje funkcij, računanih v različnih točkah, je dovoljeno, vendar nima geometrijskega pomena.

Napisala sva tudi fukncijo, ki izračuna približek za določen integral poljubne funkcije. Ker uporablja Lipschitzove konsante bo pravilno delovala samo ob zgornjih predpostavkah. 
Uporaba:
```haskell
f :: Floating a => a -> a

integralR f tocka spodnjaMeja zgornjaMeja velikostKoraka ...
integralF f tocka spodnjaMeja zgornjaMeja velikostKoraka ...
```
Funkcijo `integralR` uporabljamo za racionalne funkcije, funkcijo `integralF` pa za funkcije iz `Floating` instance.

