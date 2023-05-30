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
livingNeighbors gen coord = length $ filter (`elem` gen) $ neighbors coord

{-Határozzuk megy egy cellához az aktuális generáció segítségével, hogy élő szomszédai száma és a fenti szabályok alapján élő sejtet fog-e tartalmazni következő generációban! Természetesen ehhez azt is figyelembe kell venni, hogy az adott generációban van-e élő sejt!-}
staysAlive :: Generation -> Coordinate -> Bool
staysAlive gen coord = livingNeighbors gen coord `elem` [2, 3] -- && coord `elem` gen

{-Alkalmazzuk a fenti szabályokat az aktuális generációra! Mivel egy generáció csak az élő sejtek celláinak koordinátáit tárolja, ezért két dolog történhet minden cellával: a benne lévő sejt vagy életben marad, vagy meghal.-}
stepLivingCells :: Generation -> Generation
stepLivingCells gen = filter (staysAlive gen) gen

{-Számítsuk ki azokat az üres cellákat, amelyek az adott generáció élő sejtjeinek szomszédai! Ügyeljünk rá, hogy minden cella csak egyszer szerepeljen!-}
deadNeighbors :: Generation -> [Coordinate]
deadNeighbors gen = nub $ concatMap (aux gen) gen
  where
    aux gen coord = filter (`notElem` gen) $ neighbors coord

{-Alkalmazzuk a fenti szabályokat az aktuális generáció élő sejtjei körüli üres cellákra! Természetesen ezeken a helyeken csak újabb sejtek születhetnek.-}
stepDeadCells :: Generation -> Generation
stepDeadCells gen = filter (born gen) $ deadNeighbors gen
  where
    born gen coord = livingNeighbors gen coord == 3

{-Számítsuk ki a következő generációt az aktuális generációból! Ehhez a következő lépéseket kell elvégezni:
-számítsuk ki, hogy az élő sejtek közül melyek maradnak életben,
-határozzuk meg, hogy az élő sejtek körül mely cellákban születik új sejt,
-vegyük az előző két pont eredményének unióját, majd rendezzük.-}
stepCells :: Generation -> Generation
stepCells gen = sort $ stepLivingCells gen ++ stepDeadCells gen

{-Készítsük el azt a függvényt, amely egy kezdeti generációra elvégzi az adott számú lépést! A kezdeti generációt vegyük a nulladiknak. Ha a megadott generáció száma negatív, a lenti példában található hibát váltsuk ki!-}
play :: Generation -> Int -> Generation
play gen step
  | step < 0 = error "generation number must be non-negative"
  | step == 0 = gen
  | otherwise = iterate stepCells gen !! step

-- Állapítsuk meg egy generációról, hogy stabil-e, azaz a léptetése után változatlan marad!
isStill :: Generation -> Bool
isStill gen = gen == stepCells gen

{-Állapítsuk meg egy generációról, hogy oszcillátor-e, azaz visszatér-e önmagába egy megadott lépésnyi távolságon belül. Ha igen, adjuk meg azt a legkisebb pozitív számot, ahány generáció múlva ez megtörténik. Ha nem, adjuk a lenti példában megadott hibajelzést.-}
isOscillator :: Generation -> Int -> Int
isOscillator _ 0 = error "not an oscillator with the given limit"
isOscillator [] 1 = 1
isOscillator gen limit = findOscillator gen 1
  where
    findOscillator :: Generation -> Int -> Int
    findOscillator gen' steps
      | steps > limit = error "isOscillator: not an oscillator with the given limit"
      | currGen == originalGen = steps
      | otherwise = findOscillator currGen (steps + 1)
      where
        currGen = play gen steps
        originalGen = gen

-- ----------------------------------------------------------------------------------------
-- ascii reprezentalas terminalon valo kiiratashoz a generacionak
-- a generacio kirajzolasa
drawGeneration :: Generation -> String
drawGeneration gen
  -- ha ures a generacio, akkor ures stringet adunk vissza
  | null gen = ""
  -- ha nem ures, akkor a generacio legkisebb es legnagyobb koordinatai kozott
  -- vegigiteralunk es megnezzuk, hogy az adott koordinata szerepel-e a generacioban
  -- ha igen, akkor egy 'X'-et irunk ki, ha nem, akkor egy ' '-et
  | otherwise = unlines [[if (y, x) `elem` gen then '■' else ' ' | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    maxX = maximum $ map snd gen
    maxY = maximum $ map fst gen
    minX = minimum $ map snd gen
    minY = minimum $ map fst gen

-- a generacio kirajzolasa a terminalra
drawGenerationToTerminal :: Generation -> IO ()
drawGenerationToTerminal gen = putStr $ drawGeneration gen

-- generacio kirajzolasa ciklikusan a terminalra a play hasznalataval, ami megkapja az aktualis generaciot es a kovetkezo generacio szamat, az n a ciklusok szamat jeloli
drawGenerationsToTerminal :: Generation -> Int -> IO ()
drawGenerationsToTerminal gen n = mapM_ draw [0 .. n]
  where
    draw n = do
      putStrLn $ "Generation " ++ show n ++ ":"
      drawGenerationToTerminal $ play gen n
      putStrLn ""
      threadDelay 1000000 -- 1 masodperc varakozas

-- generacio kirajzolasa ciklukusan ugyanarra a reszre
drawGenerationsToTerminalLoop :: Generation -> Int -> IO ()
drawGenerationsToTerminalLoop gen n
  | n < 0 = error "number of generations must be non-negative"
  | null gen = error "generation must not be empty"
  | otherwise = mapM_ draw [0 .. n]
  where
    draw n = do
      clearScreen
      drawGenerationToTerminal $ play' gen n
      threadDelay 1000000 -- 1 masodperc varakozas
      where
        clearScreen = system "cls"
        play' currGen n = iterate stepCells currGen !! n
