{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}
import Database.Selda
import Database.Selda.SQLite
import Control.Exception
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Map (fromListWith, toList)
import Data.Function (on)
import Data.List (sortBy)
import System.IO.Error
import Data.Time.Clock
import Data.Time.Calendar

data Employee = Employee { 
    id_emp_type :: ID Employee
  , employeFirstName :: Text
  , employeLastName :: Text
  , employeAge :: Int
  , role :: Text
  } deriving (Generic, Show)
instance SqlRow Employee
empleado :: Table Employee
empleado = table "empleado" [#id_emp_type :- autoPrimary]


data Client = Client{ 
    id_cli:: ID Client 
  , cedula :: Int
  , clientFirstName :: Text
  , clientLastName :: Text
  , mail::Text
  , clientAge :: Int
} deriving (Generic, Show)
instance SqlRow Client
cliente :: Table Client
cliente = table "cliente" [#id_cli :- autoPrimary ]



data Book = Book { 
    id_book:: ID Book 
  , name::Text 
  , isbn::Int 
  , genero :: Text
  , publication_date::Int
  , price::Double  
  , author:: Text
  , imprenta:: Text
} deriving (Generic, Show)
instance SqlRow Book
libro :: Table Book
libro = table "libro" [#id_book :- autoPrimary]


data Sell= Sell{  
   id_ven:: ID Sell
 , id_fact :: Int 
 , dia::Int
 , mes::Int
 , año::Int
 , id_clientV::Int
 , id_tienda::Int
 , id_item::Int
 , id_sell::Int} deriving (Generic, Show)
instance SqlRow Sell
venta :: Table Sell
venta = table "venta" [#id_ven :- autoPrimary] 


data Store = Store { 
id_store :: Int,
nombre :: Text,
ciudad :: Text,
direccion :: Text
} deriving (Generic, Show)
instance SqlRow Store
tienda :: Table Store
tienda = table "tienda" [#id_store :- primary] 

data Bill = Bill {
  id_bill :: Int,
  diaa :: Int,
  mess :: Int,
  ano :: Integer,
  cliente_info :: [Client],
  tienda_info :: [Store],
  items :: [(Text, Double)],
  vendedor :: [Employee],
  total :: Double
} deriving (Show)

--insert funtions
insertClient :: Int -> Text -> Text -> Text -> Int -> SeldaM b ()
insertClient ci firstN lastN mail age = insert_ cliente [Client def ci firstN lastN mail age]

insertEmployee :: Text -> Text -> Int -> Text -> SeldaM b ()
insertEmployee firstN lastN age role = insert_ empleado [Employee def firstN lastN age role]

insertBook :: Text -> Int -> Text -> Int -> Double -> Text -> Text -> SeldaM b ()
insertBook  name isbn genero publication_date price author imprenta = insert_ libro [Book def name isbn genero publication_date price author imprenta ]

insertItem :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> SeldaM b ()
insertItem id_fact dia mes year id_clientV id_tienda id_item id_emp = insert_ venta [Sell def id_fact dia mes year id_clientV id_tienda id_item id_emp]

insertStore :: Int -> Text -> Text -> Text -> SeldaM b ()
insertStore id nombre ciudad direccion = insert_ tienda [Store id nombre ciudad direccion]

showClients :: SeldaM b ()
showClients = do
  clients <- query $ do
    c <- select cliente
    return (c ! #clientFirstName :*:  c ! #clientLastName :*: c ! #cedula)
  liftIO $ print clients

getItemNames :: Col s Int ->  Query s (Col s Text  )
getItemNames factN  = do
  books <- select libro
  items <- select venta
  restrict ( items ! #id_fact .== factN)
  restrict ( books ! #isbn .== items ! #id_item )
  return (books ! #name )
  --liftIO $ print itemsFactura





getItemPrices :: Col s Int ->  Query s (Col s Double  )
getItemPrices factN  = do
  books <- select libro
  items <- select venta
  restrict ( items ! #id_fact .== factN)
  restrict ( books ! #isbn .== items ! #id_item )
  return (books ! #price )

searchClient :: Integer ->  IO [Client]
searchClient dni = withSQLite "storedb1.sqlite" $ query $ do
  client <- select cliente
  restrict ( client ! #cedula .== ( fromInteger dni))
  return (client )

searchEmployee :: Integer ->  IO [Employee]
searchEmployee id = withSQLite "storedb1.sqlite" $ query $ do
  empl <- select empleado
  restrict ( empl ! #employeAge  .==  (fromInteger id))
  return (empl )

searchStore :: String ->  IO [Store]
searchStore id = withSQLite "storedb1.sqlite" $ query $ do
  store <- select tienda
  restrict ((  store ! #direccion)  .== "123-21")
  return (store )  

addItems :: String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO [(Text,Double)]
addItems option id_fact dia mes year id_clientV id_tienda id_emp = 
  if option == "yes"
    then do
      putStr "Insert by ISBN: "
      isbn <- readLn :: IO Integer
      book <- withSQLite "storedb1.sqlite" $ query $ do
        b <- select libro
        restrict (b ! #isbn .==  (fromInteger isbn))
        return (b)

      if length book == 0
        then do
          putStr "Book not found!! \n Try to search it by name(1) or insert ISBN again (2): "         
          s <- readLn :: IO Int
          if s == 1
            then do
              putStr "Insert Book name: "
              bName <- readLn :: IO Text
              books <- withSQLite "storedb1.sqlite" $ query $ do 
                bs <- select libro
                restrict ( bs ! #name .== (text bName))
                return (bs ! #name :*: bs ! #isbn)
              liftIO $ mapM_  print  books
              addItems "yes" id_fact dia mes year id_clientV id_tienda id_emp
            else do
              addItems "yes" id_fact dia mes year id_clientV id_tienda id_emp

        else do
          withSQLite "storedb1.sqlite" $ insertItem id_fact dia mes year id_clientV id_tienda (fromIntegral isbn) id_emp
          putStr "Do you want to add a new item to the sell note? [\"yes\", \"no\"]: "
          op <- readLn :: IO String 
          addItems op id_fact dia mes year id_clientV id_tienda id_emp
    else do
      putStr "Creating sell note \n"
      a <- withSQLite "storedb1.sqlite" $ do
        itemNames <- query $ getItemNames $ fromInteger (fromIntegral id_fact)
        itemPrices <- query $ getItemPrices $ fromInteger (fromIntegral id_fact)
        return (zip itemNames itemPrices)
      return a

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

getYear :: (Integer,Int,Int) ->  IO (Integer)
getYear (a,_,_) = return a

getMonth :: (Integer,Int,Int) -> IO (Int)
getMonth (_,b,_) = return b

getDay :: (Integer,Int,Int) -> IO (Int)
getDay (_,_,b) = return b



createNewFacture :: IO(Bill)
createNewFacture = do 
  putStr "Client Ci:"
  dni <- readLn :: IO Int
  --get client info
  clientInfo <- searchClient (toInteger dni)

  --get f_id
  f_numb <- withSQLite "storedb1.sqlite" $ query $ do 
    item <- select venta
    return (item ! #id_fact)
  --get date
  ymd <- date
  year <- getYear ymd
  month <- getMonth ymd
  day <- getDay ymd
  --get employee info
  empInfo <- searchEmployee 18
  --get store info
  tInfo  <- searchStore "123-21"
  if length clientInfo == 1
    then do
      itemList <- addItems "yes" (maximum f_numb +1) day month (fromIntegral year) dni 1 1
      print $ foldl (\t (x, y) -> t + y) 0 itemList
      putStr "Factura creada!"
      return Bill {id_bill = (maximum f_numb +1), diaa = day, mess = month, ano = year, cliente_info = clientInfo, items = itemList, vendedor = empInfo,tienda_info = tInfo, total = (foldl (\t (x, y) -> t + y) 0 itemList) }

    else do
      putStr "Please create a new client:"
      putStr "Insert first name:"
      fname <- readLn :: IO Text
      putStr "Insert last name:"
      lname <- readLn :: IO Text
      putStr "Insert mail:"
      mail <- readLn :: IO Text
      putStr "Insert age:"
      age <- readLn :: IO Int
      withSQLite "storedb1.sqlite" $ do 
        insertClient dni fname lname mail age
        showClients
      itemList <- addItems "yes" (maximum f_numb +1) day month (fromIntegral year) dni 1 1
      print $ foldl (\t (x, y) -> t + y) 0 itemList
      return Bill {id_bill = (maximum f_numb +1), diaa = day, mess = month, ano = year, cliente_info = clientInfo, items = itemList, vendedor = empInfo,tienda_info = tInfo, total = (foldl (\t (x, y) -> t + y) 0 itemList) }
  
    --insertItem 3 20 2 2020 1 1 61020664
genereMostReades :: IO [Text]
genereMostReades = do
  books<-withSQLite "storedb1.sqlite" $ query $ do
    soldBs <- select venta
    bs <- select libro
    restrict(bs ! #isbn .== soldBs ! #id_item)
    return (bs ! #genero)  
  return books

sortElements :: Ord b => [(a,b)] -> [(a,b)]
sortElements = sortBy (flip compare `on` snd)



main :: IO ()
main = do
  --insertClient 41321 "Jhon" "Goyes" "jg@gmail.com" 23
  --insertClient 43121 "Pan" "Cho" "pc@gmail.com" 14
  --insertEmployee "Car" "Los" 18 "Seller" 
  --insertBook "Krondor: The Betraya" 380795272 "Comed" 1998 3.5 "Raymond E. Feis" "Blackie Book"
  --insertBook "The Dark Is Risin" 1416949658 "Scienc" 1973 13.3 "Susan Coope" "la Galer"
  --insertBook "The Black Unicorn " 1857231082 "Suspens" 1987 16.23 "Terry Brook" "Grupo Daur"
  --insertBook "I, Robo" 553803700 "Comed" 1950 20 "Isaac Asimo" "Caballo de Tro" 
  --insertBook "Love, Stargir" 375913750 "Comed" 2007 12.3 "Jerry Spinell" "El Olivo Azu"
  --insertBook "Vanishing Act" 743454553 "Comed" 2005 4.5 "Jodi Picoul" "Literatura S"
  --insertBook "Azte" 765317508 "Adventur" 1980 14.3 "Gary Jenning" "Blackie Book"
  --insertBook "Marlfo" 142501085 "Adventur" 1998 17.23 "Brian Jacque" "la Galer"
  --insertBook "Lady Midnigh" 1442468351 "Suspens" 2016 21 "Cassandra Clar" "Grupo Daur"
  --insertBook "The Secret Keepe" 1439152802 "Sci-F" 2012 24.3 "Kate Morton" "Caballo de Tro"
  --insertBook "The Afgha" 399153942 "Sci-F" 2006 3.5 "Frederick Forsyth" "Ushuaia Edicio"
  --insertBook "A Touch of Dea" 441017835 "Suspens" 2009 13.3 "Charlaine Harris" "El Olivo Azu"
  --insertBook "Daja's Boo" 590554107 "Suspens" 1998 16.23 "Tamora Pierce" "ediciones 1"
  --insertBook "Steve Job" 1451648537 "Biographi" 2011 20 "Walter Isaacson" "Literatura S"
  --insertBook "Cold Fir" 590396560 "Suspens" 2002 23.3 "Tamora Pierce" "Blackie Book"
  --insertStore 1 "Nuevoteca" "Ibarra" "123-21"
  a <- createNewFacture
  print a

  
  --createTable empleado
  --createTable cliente
  --createTable libro
  --createTable venta
  --createTable tienda
