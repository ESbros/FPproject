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
 , año::Int
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
tienda :: Table Store
tienda = table "tienda" [#id_store :- primary] 



--insert funtions
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


main :: IO ()
main = withSQLite "storedb.sqlite" $ do
   
  createTable empleado
  createTable cliente
  createTable libro
  createTable venta
  createTable tienda
