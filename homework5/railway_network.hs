-- Define types for City, Road, and Location
type Location = (Float, Float)
type CityName = String
type City = (CityName, Location)
type Weight = Float
type Road = (CityName, CityName, Weight)

city_database_example :: [City]
city_database_example = [("City A", (0, 0)), ("City B", (1, 1)), ("City C", (7, 2)), ("City D", (3, 5)), ("City E", (3, 3)), ("City F", (3, 2)), ("City G", (7, 0)), ("City H", (6, 6))]

-- Example database of cities and roads (replace with your own data)
database_example :: [Road]
database_example = [
    ("City A", "City B", 0.5),
    ("City A", "City C", 0.8),
    ("City B", "City D", 0.5),
    ("City C", "City E", 0.5),
    ("City C", "City F", 0.8),
    ("City C", "City G", 0.5),
    ("City D", "City H", 0.5),
    ("City E", "City F", 0.8),
    ("City E", "City H", 0.5),
    ("City F", "City G", 0.5)

  ]



findRoutes :: CityName -> CityName -> [Road] -> [[Road]]
findRoutes start end database
  | start == end = [[]]  -- If start and end are the same, return a trivial path
  | otherwise = [ (start, next, weight) : route | (s, next, weight) <- database, s==start, route <- findRoutes next end [r | r <- database, r /= (start, next, weight)]]  --road for the first city
  ++[ (next, start, weight) : route | (next, s, weight) <- database, s==start, route <- findRoutes next end [r | r <- database, r /= (next, start, weight)]]  --road for the second city

distance :: Location -> Location -> Float
distance (x, y) (x', y') = sqrt ((x-x')^2 + (y-y')^2)

printRoad :: [[Road]] -> [City] -> ([Cityname], Distance)
printRoad answer city_database = [|(citya, cityb, weight)<-oneof,oneof<-anser]

-- Example usage
main :: IO ()
main = do
    print $ findRoutes "City A" "City E" database_example

