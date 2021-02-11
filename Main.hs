{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}
import Database.Selda
import Database.Selda.SQLite
import Control.Exception
import Control.Applicative
import Data.Char
import Data.Dates
import System.IO.Error

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
 , year::Int
 , id_clientV::Int
 , id_tienda::Int
 , id_item::Int} deriving (Generic, Show)
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

stores :: Table Store
stores = table "stores" [#id_store :- primary] 



insertClient :: Int -> Text -> Text -> Text -> Int -> SeldaM b ()
insertClient ci firstN lastN mail age = insert_ cliente [Client def ci firstN lastN mail age]

insertEmployee :: Text -> Text -> Int -> Text -> SeldaM b ()
insertEmployee firstN lastN age role = insert_ empleado [Employee def firstN lastN age role]

insertBook :: Text -> Int -> Text -> Int -> Double -> Text -> Text -> SeldaM b ()
insertBook  name isbn genero publication_date price author imprenta = insert_ libro [Book def name isbn genero publication_date price author imprenta ]

insertItem :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> SeldaM b ()
insertItem id_fact dia mes year id_clientV id_tienda id_item = insert_ venta [Sell def id_fact dia mes year id_clientV id_tienda id_item]

insertStore :: Int -> Text -> Text -> Text -> SeldaM b ()
insertStore id nombre ciudad direccion = insert_ tienda [Store id nombre ciudad direccion]

insertClient :: Int -> Text -> Text -> Text -> Int -> SeldaM b ()
insertClient ci firstN lastN mail age = insert_ cliente [Client def ci firstN lastN mail age]

insertEmployee :: Text -> Text -> Int -> Text -> SeldaM b ()
insertEmployee firstN lastN age role = insert_ empleado [Employee def firstN lastN age role]

insertBook :: Text -> Int -> Text -> Int -> Double -> Text -> Text -> SeldaM b ()
insertBook  name isbn genero publication_date price author imprenta = insert_ libro [Book def name isbn genero publication_date price author imprenta ]

insertItem :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> SeldaM b ()
insertItem id_fact dia mes year id_clientV id_tienda id_item = insert_ venta [Sell def id_fact dia mes year id_clientV id_tienda id_item]

insertStore :: Int -> Text -> Text -> Text -> SeldaM b ()
insertStore id nombre ciudad direccion = insert_ tienda [Store id nombre ciudad direccion]



showClients :: SeldaM b ()
showClients = do
  clients <- query $ do
    c <- select cliente
    return (c ! #clientFirstName :*:  c ! #clientLastName)
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

main :: IO ()
main = do
  putStrLn ("NamFacture number: ")
  fact <- readLn ::  IO Integer
  withSQLite "Tienda1.sqlite" $ do
    --createTable empleado
    --createTable cliente
    --createTable libro
    --createTable venta
    
    --insertBook "My Man Jeeves" 1585678759 "Unknow" 1919 15.50 1 1 1
    --insertBook "The Sea Wolf" 1598184318 "Unknow" 1904 50.30 2 2 4
    --insertItem 2 "20 de mayo 2020" 1 1585678759
    --insertItem 2 "20 de mayo 2020" 1 1585678759

    --generateFacture 1
    --insertItem 2 "20 de mayo 2020" 1 61020664
    itemNames <- query $ getItemNames $ fromInteger fact
    itemPrices <- query $ getItemPrices $ fromInteger fact
    liftIO $ mapM_ print (zip itemNames itemPrices)
    liftIO $ print (foldl (+) 0 itemPrices)
    showClients






-- parte 2 

{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}
import Database.Selda
import Database.Selda.SQLite
import Control.Exception
import Control.Applicative
import Data.Char
import System.IO.Error
import Data.Time.Clock
import Data.Time.Calendar
--getCurrentTime >>= return . toGregorian . utctDay
--day = (\(_, _, y) -> y) <$> date

data Sell = Sell {
    id_ven      :: ID Sell,
    id_fact     :: Int,
    dia         :: Int,
    mes         :: Int,
    year        :: Int,
    id_clientV  :: Int,
    id_tienda   :: Int,
    id_item     :: Int
} deriving (Generic, Show)
instance SqlRow Sell

venta :: Table Sell
venta = table "venta" [#id_ven :- autoPrimary] 

insertSale :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> SeldaM b ()
insertSale id_fact dia mes year id_clientV id_tienda id_item = do
    --createTable venta
    insert_ venta [Sell def id_fact dia mes year id_clientV id_tienda id_item]

queryForSale :: Col b Int -> SeldaM b ()
queryForSale daySale = do
    queryForSales <- query $ do
        sale <- select venta
        restrict (sale ! #dia .== daySale)
        return (sale ! #id_fact :*: sale ! #dia :*: sale ! #mes :*: sale ! #year :*: sale ! #year id_clientV :*: sale ! #year id_tienda :*: sale ! #year id_item) 
    liftIO $ print queryForSales


data Book = Book {
    id_book :: ID Book,
    name :: Text, 
    isbn :: Int, 
    genero :: Text,
    publication_date :: Int,
    price :: Double,
    author :: Text,
    imprenta :: Text
} deriving (Generic, Show)
instance SqlRow Book

libro :: Table Book
libro = table "libro" [#id_book :- autoPrimary]

insertBook :: Text -> Int -> Text -> Int -> Double -> Text -> Text -> SeldaM b ()
insertBook name isbn genero publication_date price author imprenta = do
    --createTable libro
    insert_ libro [Book def name isbn genero publication_date price author imprenta]

queryForBook :: Col b Int -> SeldaM b ()
queryForBook bookISBN = do
    queryForBooks <- query $ do
        book <- select libro
        restrict (book ! #isbn .== bookISBN)
        return (book ! #isbn :*: book ! #name :*: book ! #author)
    liftIO $ print queryForBooks


main :: IO ()
main = do
    putStrLn ("1: Registrar Libro - 2: Consultar ventas dia - 3: Registrar Venta | quit for leave")
    options <- getLine

    if options == "1" then do 
        putStrLn ("Name book: ")
        name <- readLn :: IO Text
        putStrLn ("ISBN: ")
        isbn <- readLn :: IO Int
        putStrLn ("Genero: ")
        genero <- readLn :: IO Text
        putStrLn ("Publication Date: ")
        publication_date <- readLn :: IO Int
        putStrLn ("Price: ")
        price <- readLn :: IO Double
        putStrLn ("Author Name: ")
        author <- readLn :: IO Text
        putStrLn ("Press Name: ")
        imprenta <- readLn :: IO Text
        
        withSQLite "library.sqlite" $ do
            insertBook name isbn genero publication_date price author imprenta
        main

    else if options == "2" then do
        putStrLn ("Day: ")
        daySale <- readLn :: IO Integer
        
        withSQLite "library.sqlite" $ do
            queryForSale $ fromInteger daySale
        main

    else if options == "3" then do
        putStrLn ("ID Fact: ")
        id_fact <- readLn :: IO Int
        putStrLn ("Day: ")
        dia <- readLn :: IO Int
        putStrLn ("Month: ")
        mes <- readLn :: IO Int
        putStrLn ("Year: ")
        year <- readLn :: IO Int
        putStrLn ("Client ID: ")
        id_clientV <- readLn :: IO Int
        putStrLn ("Store ID: ")
        id_tienda <- readLn :: IO Int
        putStrLn ("Item ID: ")
        id_item <- readLn :: IO Int
        
        withSQLite "library.sqlite" $ do
            insertSale id_fact dia mes year id_clientV id_tienda id_item
        main

    else if options == "quit" then do
        return()
    else
        main