
type Point = (Float,Float)

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
 deriving (Eq,Show,Ord)

data Shape = Circle Float | Square Float | Rectangle Float| Eqtrinagle Float | Trinagle Float Float 
 deriving (Eq,Show)

data Person = Person {name :: String, age :: Int, details :: String}
 deriving (Eq,Show)