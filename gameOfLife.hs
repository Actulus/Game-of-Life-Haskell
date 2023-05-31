import Control.Applicative
import Control.Concurrent (threadDelay)
import Data.List
import Data.List.Split
import Data.Set (Set)
import Data.Set qualified as Set
import System.Process

-- Ügyeljünk arra, hogy a koordináták előbb az y, majd az x tengelyen vett pozíciót tartalmazzák, a számítógépes grafikában megszokott módon!
type Coordinate = (Integer, Integer)

-- A játék egy állapota az aktuális generáció (az élő sejtek) celláinak megadásából áll:
type Generation = [Coordinate]

-- A játék egy állapotát egy képernyőre rajzolható, véges méretű rácsként is reprezentálhatjuk:
type Grid = [[Char]]

single :: Generation
single = [(42, 42)]

block :: Generation
block =
  [ (5, 6),
    (5, 7),
    (6, 6),
    (6, 7)
  ]

row :: Generation
row = [(10, 1), (10, 2), (10, 3)]

column :: Generation
column =
  [ (9, 2),
    (10, 2),
    (11, 2)
  ]

caterer :: Generation
caterer =
  [ (2, 4),
    (3, 2),
    (3, 6),
    (3, 7),
    (3, 8),
    (3, 9),
    (4, 2),
    (4, 6),
    (5, 2),
    (6, 5),
    (7, 3),
    (7, 4)
  ]

pulsarShuttle :: Generation
pulsarShuttle =
  [ (2, 4),
    (2, 5),
    (2, 6),
    (2, 10),
    (2, 11),
    (2, 12),
    (4, 2),
    (4, 7),
    (4, 9),
    (4, 14),
    (5, 2),
    (5, 7),
    (5, 9),
    (5, 14),
    (6, 2),
    (6, 7),
    (6, 9),
    (6, 14),
    (7, 4),
    (7, 5),
    (7, 6),
    (7, 10),
    (7, 11),
    (7, 12),
    (9, 4),
    (9, 5),
    (9, 6),
    (9, 10),
    (9, 11),
    (9, 12),
    (10, 2),
    (10, 7),
    (10, 9),
    (10, 14),
    (11, 2),
    (11, 7),
    (11, 9),
    (11, 14),
    (12, 2),
    (12, 7),
    (12, 9),
    (12, 14),
    (14, 4),
    (14, 5),
    (14, 6),
    (14, 10),
    (14, 11),
    (14, 12)
  ]

pulsar :: Generation
pulsar =
  [ (3, 5),
    (3, 6),
    (3, 7),
    (3, 11),
    (3, 12),
    (3, 13),
    (5, 3),
    (5, 8),
    (5, 10),
    (5, 15),
    (6, 3),
    (6, 8),
    (6, 10),
    (6, 15),
    (7, 3),
    (7, 8),
    (7, 10),
    (7, 15),
    (8, 5),
    (8, 6),
    (8, 7),
    (8, 11),
    (8, 12),
    (8, 13),
    (10, 5),
    (10, 6),
    (10, 7),
    (10, 11),
    (10, 12),
    (10, 13),
    (11, 3),
    (11, 8),
    (11, 10),
    (11, 15),
    (12, 3),
    (12, 8),
    (12, 10),
    (12, 15),
    (13, 3),
    (13, 8),
    (13, 10),
    (13, 15),
    (15, 5),
    (15, 6),
    (15, 7),
    (15, 11),
    (15, 12),
    (15, 13)
  ]

{-Készítsünk egy függvényt, amely egy cella koordinátái alapján megadja a vele szomszédos
cellák koordinátáit! Mivel a tér végtelen, minden cellának pontosan 8 szomszédja van.
A szomszédokat olyan sorrendben adjuk meg, hogy sorokban fentről lefelé, ezen belül pedig balról jobbra legyenek felsorolva.-}
neighbors :: Coordinate -> [Coordinate]
neighbors (y, x) = [(y - 1, x - 1), (y - 1, x), (y - 1, x + 1), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)]

{-Határozzuk meg egy generáció és egy cella alapján, hogy egy adott cellának hány élő sejtet tartalmazó szomszédja van!-}
livingNeighbors :: Generation -> Coordinate -> Int
livingNeighbors gen coord = length $ filter (`elem` gen) $ neighbors coord -- lekerjuk az adott koordinata szomszedait, majd filtereljuk az alapjan, hogy eleme-e az adott generacionak, vegul lekerjuk a kapott lista hosszat

{-Határozzuk megy egy cellához az aktuális generáció segítségével, hogy élő szomszédai száma és a fenti szabályok alapján élő sejtet fog-e tartalmazni következő generációban! Természetesen ehhez azt is figyelembe kell venni, hogy az adott generációban van-e élő sejt!-}
staysAlive :: Generation -> Coordinate -> Bool
staysAlive gen coord = livingNeighbors gen coord `elem` [2, 3] -- ha az adott generacionak 2 vagy 3 szomszedja van, akkor eletben marad

{-Alkalmazzuk a fenti szabályokat az aktuális generációra! Mivel egy generáció csak az élő sejtek celláinak koordinátáit tárolja, ezért két dolog történhet minden cellával: a benne lévő sejt vagy életben marad, vagy meghal.-}
stepLivingCells :: Generation -> Generation
stepLivingCells gen = filter (staysAlive gen) gen -- filtereljuk a generaciot az alapjan, hogy az adott koordinata eletben marad-e

{-Számítsuk ki azokat az üres cellákat, amelyek az adott generáció élő sejtjeinek szomszédai! Ügyeljünk rá, hogy minden cella csak egyszer szerepeljen!-}
deadNeighbors :: Generation -> [Coordinate]
deadNeighbors gen = nub $ concatMap (aux gen) gen -- a concatMap-pel osszefuzzuk azokat a szomszedokat, amelyek nem elemei az adott generacionak, majd a nub-bal eltavolitjuk a duplikatumokat
  where
    aux gen coord = filter (`notElem` gen) $ neighbors coord -- lekerjuk az adott koordinata szomszedait, majd filtereljuk az alapjan, hogy nem eleme-e az adott generacionak

{-Alkalmazzuk a fenti szabályokat az aktuális generáció élő sejtjei körüli üres cellákra! Természetesen ezeken a helyeken csak újabb sejtek születhetnek.-}
stepDeadCells :: Generation -> Generation
stepDeadCells gen = filter (born gen) $ deadNeighbors gen -- lekerjuk az ures szomszedokat, majd filtereljuk az alapjan, hogy az adott koordinata szuletesre alkalmas-e
  where
    born gen coord = livingNeighbors gen coord == 3 -- ha az adott generacionak 3 szomszedja van, akkor uj sejt szuletik

{-Számítsuk ki a következő generációt az aktuális generációból! Ehhez a következő lépéseket kell elvégezni:
-számítsuk ki, hogy az élő sejtek közül melyek maradnak életben,
-határozzuk meg, hogy az élő sejtek körül mely cellákban születik új sejt,
-vegyük az előző két pont eredményének unióját, majd rendezzük.-}
stepCells :: Generation -> Generation
stepCells gen = sort $ stepLivingCells gen ++ stepDeadCells gen -- osszefuzzuk az ures cellakat es az eletben maradt sejteket, majd rendezzuk oket

{-Készítsük el azt a függvényt, amely egy kezdeti generációra elvégzi az adott számú lépést! A kezdeti generációt vegyük a nulladiknak. Ha a megadott generáció száma negatív, a lenti példában található hibát váltsuk ki!-}
play :: Generation -> Int -> Generation
play gen step
  | step < 0 = error "generation number must be non-negative"
  | step == 0 = gen
  | otherwise = iterate stepCells gen !! step -- vegigiteralunk a generacion alkalmazva a stepCells fuggvenyt, majd a !! operatorral kivalasztjuk a megadott indexu elemet

-- Állapítsuk meg egy generációról, hogy stabil-e, azaz a léptetése után változatlan marad!
isStill :: Generation -> Bool
isStill gen = gen == stepCells gen -- ha a generacio megegyezik a kovetkezo generacioval, akkor stabil

{-Állapítsuk meg egy generációról, hogy oszcillátor-e, azaz visszatér-e önmagába egy megadott lépésnyi távolságon belül. Ha igen, adjuk meg azt a legkisebb pozitív számot, ahány generáció múlva ez megtörténik. Ha nem, adjuk a lenti példában megadott hibajelzést.-}
isOscillator :: Generation -> Int -> Int
isOscillator _ 0 = error "not an oscillator with the given limit"
isOscillator [] 1 = 1 -- ha ures a generacio, akkor 1-et adunk vissza
isOscillator gen limit = findOscillator gen 1 -- megkeressuk az oszcillatort
  where
    findOscillator :: Generation -> Int -> Int
    findOscillator gen' steps
      | steps > limit = error "isOscillator: not an oscillator with the given limit" -- ha a megadott limitnel tobb lepesben nem talaljuk meg az oszcillatort, akkor hibat dobunk
      | currGen == originalGen = steps -- ha a kurrens generacio megegyezik az eredetivel, akkor visszaterunk a lepesek szamaval
      | otherwise = findOscillator currGen (steps + 1) -- ha nem, akkor folytatjuk a keresest, novelve a lepesek szamat
      where
        currGen = play gen steps -- kurrens generacio kiszamitasa
        originalGen = gen -- eredeti generacio kimentese

-- ----------------------------------------------------------------------------------------
-- ascii reprezentalas terminalon valo kiiratashoz a generacionak
-- a generacio kirajzolasa
drawGeneration :: Generation -> String
drawGeneration gen
  -- ha ures a generacio, akkor ures stringet adunk vissza
  | null gen = ""
  -- ha nem ures, akkor a generacio legkisebb es legnagyobb koordinatai kozott
  -- vegigiteralunk es megnezzuk, hogy az adott koordinata szerepel-e a generacioban
  -- ha igen, akkor egy '■'-et irunk ki, ha nem, akkor egy ' '-et
  -- unlines-szal osszefuzzuk a sorokat
  | otherwise = unlines [[if (y, x) `elem` gen then '■' else ' ' | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    maxX = maximum $ map snd gen
    maxY = maximum $ map fst gen
    minX = minimum $ map snd gen
    minY = minimum $ map fst gen

-- a generacio kirajzolasa a terminalra
drawGenerationToTerminal :: Generation -> IO ()
drawGenerationToTerminal gen = putStr $ drawGeneration gen -- kiiratjuk a generaciot a terminalra

-- generacio kirajzolasa ciklikusan a terminalra a play hasznalataval, ami megkapja az aktualis generaciot es a kovetkezo generacio szamat, az n a ciklusok szamat jeloli
drawGenerationsToTerminal :: Generation -> Int -> IO ()
drawGenerationsToTerminal gen n = mapM_ draw [0 .. n] -- mapM_-el vegigiteralunk a ciklusokon es kiiratjuk a generaciot
  where
    draw n = do
      putStrLn $ "Generation " ++ show n ++ ":" -- kiirjuk a generacio szamat
      drawGenerationToTerminal $ play gen n -- kiiratjuk a generaciot
      putStrLn "" -- ures sor
      threadDelay 1000000 -- 1 masodperc varakozas

-- generacio kirajzolasa ciklukusan ugyanarra a reszre
drawGenerationsToTerminalLoop :: Generation -> Int -> IO ()
drawGenerationsToTerminalLoop gen n
  | n < 0 = error "number of generations must be non-negative"
  | null gen = error "generation must not be empty"
  | otherwise = mapM_ draw [0 .. n] -- mapM_-el vegigiteralunk a ciklusokon es kiiratjuk a generaciot
  where
    draw n = do
      clearScreen -- toroljuk a terminalt
      drawGenerationToTerminal $ play' gen n -- kiiratjuk a generaciot
      threadDelay 1000000 -- 1 masodperc varakozas
      where
        clearScreen = system "cls" -- windows alatt a terminal torlese
        play' currGen n = iterate stepCells currGen !! n -- vegigiteralunk a generacion alkalmazva a stepCells fuggvenyt, majd a !! operatorral kivalasztjuk a megadott indexu elemet
