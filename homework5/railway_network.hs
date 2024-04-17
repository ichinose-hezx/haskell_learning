-- Define types for City, Road, and Location
type Location = (Float, Float)
type CityName = String
type City = (CityName, Location)
type Weight = Float
type Road = (CityName, CityName, Weight)

-- Example database of cities with latitudes and longitudes
city_database_example :: [City]
city_database_example = [("Shanghai", (121.47, 31.23)), ("Hangzhou", (120.21, 30.20)), ("Nanjing", (118.89, 31.32)), ("Hefei", (117.30, 31.79)), ("Wuhan", (114.02, 30.58)), ("Changsha", (112.98, 28.25)), ("Nanchang", (115.94, 28.54)),
 ("Fuzhou", (119.27, 26.04)), ("Guangzhou", (113.27, 23.15)), ("Nanning", (108.27, 22.78)), ("Guiyang", (106.62, 26.67)), ("Chongqing", (106.54, 29.40)), ("Chengdu", (104.10, 30.65)),("Lasa", (91.13, 29.65)), 
 ("Lanzhou", (103.71, 36.10)), ("Yinchuan", (106.24, 38.47)), ("Xian", (108.93, 34.23)), ("Taiyuan", (112.48, 37.94)),("Shijiazhuang", (114.53, 38.03)), ("Beijing", (116.23, 40.22)), ("Huhehaote", (111.62, 40.80)), 
 ("Jinan", (116.75, 36.55)), ("Qingdao", (123.46, 41.80)), ("Tianjin", (117.30, 39.71)), ("Shenyang", (123.46, 41.80)),("Haerbing", (126.95, 45.54)),("Changchun", (125.28, 43.83)),("Kunming", (102.82, 24.88))]

-- Example database of railway_roads (weight equals to price per kilometers)
road_database_example :: [Road]
road_database_example = [
    ("Shanghai", "Hangzhou", 0.46),
    ("Shanghai", "Nanjing", 0.46),
    ("Shanghai", "Qingdao", 0.46),
    ("Hangzhou", "Nanjing", 0.46),
    ("Hangzhou", "Nanchang", 0.46),
    ("Hangzhou", "Hefei", 0.46),  
    ("Qingdao", "Jinan", 0.46),
    ("Jinan", "Nanjing", 0.46),
    ("Jinan", "Zhengzhou", 0.31),
    ("Jinan", "Tianjin", 0.31),
    {-
    ("Jinan", "Shijiazhuang", 0.31),
    ("Nanjing", "Hefei", 0.46),
    ("Hefei","Zhengzhou",0.46),
    ("Hefei","Wuhan",0.46),
    ("Hefei","Nanchang",0.46),
    ("Nanchang","Wuhan",0.46),
    ("Nanchang","Changsha",0.46),
    ("Nanchang","Fuzhou",0.46),
    ("Fuzhou","Guangzhou",0.46),
    ("Guangzhou","Changsha",0.46),    
    ("Guangzhou","Nanning",0.46),
    ("Nanning","Changsha",0.46),
    ("Nanning","Guiyang",0.46),
    ("Nanning","Kunming",0.46),
    ("Kunming","Chengdu",0.5),
    ("Kunming","Chongqing",0.5),
    ("Kunming","Guiyang",0.5),
    ("Changsha","Wuhan",0.37),
    ("Changsha","Chongqing",0.37),
    ("Changsha","Guiyang",0.37),
    ("Wuhan","Zhengzhou",0.37),
    ("Wuhan","Chongqing",0.37),
    ("Guiyang","Chongqing",0.37),    
    ("Chongqing","Chengdu",0.37),    
    ("Chongqing","Zhengzhou",0.37),
    -}
    ("Chengdu","Lasa",0.4),
    ("Chengdu","Lanzhou",0.4),
    ("Chengdu","Xian",0.4),  
    ("Lanzhou","Yinchuan",0.31),
    ("Lanzhou","Xian",0.31),
    {-
    ("Yinchuan","Xian",0.31),
    ("Xian","Zhengzhou",0.4),
    -}
    ("Xian","Taiyuan",0.4),  
    ("Zhengzhou","Taiyuan",0.4),
    ("Zhengzhou","Shijiazhuang",0.4),
    ("Beijing","Tianjin",0.31),
    ("Beijing","Shenyang",0.31),
    ("Beijing","Huhehaote",0.31),
    {-
    ("Beijing","Shijiazhuang",0.31),
    ("Taiyuan","Shijiazhuang",0.31),
    -}
    ("Taiyuan","Huhehaote",0.31),
    ("Changchun","Shenyang",0.31),  
    ("Changchun","Haerbing",0.31)   
  ]

-- Haversine to calculate distance
distance_Road :: [City] -> [Road] -> [Road]
distance_Road city_database road_databse = 
  [(a, b, distance)
  |(a, (ax, ay))<-city_database
  ,(b, (bx, by))<-city_database
  ,(a',b', _)<-road_databse
  ,a==a',b==b'
  ,let axr = ax*pi/180.0
  ,let ayr = ay*pi/180.0
  ,let bxr = bx*pi/180.0
  ,let byr = by*pi/180.0
  ,let c = sin((bxr - axr) / 2)^2 + cos(axr) * cos(bxr) * sin((byr-ayr) / 2)^2
  ,let distance = 2 * atan2 (sqrt c) (sqrt (1 - c)) * 6371.0]

-- Price equals to distance times weight
price_Road :: [City] -> [Road] -> [Road]
price_Road city_database road_databse = 
  [(a, b, (distance*weight))
  |(a, (ax, ay))<-city_database
  ,(b, (bx, by))<-city_database
  ,(a',b',weight)<-road_databse
  ,a==a',b==b'
  ,let axr = ax*pi/180.0
  ,let ayr = ay*pi/180.0
  ,let bxr = bx*pi/180.0
  ,let byr = by*pi/180.0
  ,let c = sin((bxr - axr) / 2)^2 + cos(axr) * cos(bxr) * sin((byr-ayr) / 2)^2
  ,let distance = 2 * atan2 (sqrt c) (sqrt (1 - c)) * 6371.0]

distance_database_example :: [Road]
distance_database_example = distance_Road city_database_example road_database_example

price_database_example :: [Road]
price_database_example = price_Road city_database_example road_database_example

-- Find all routes
findRoutes :: CityName -> CityName -> [Road] -> [[Road]]
findRoutes start end database
  | start == end = [[]]
  | otherwise = [ (start, next, time') : route | (s, next, time') <- database, s==start, route <- findRoutes next end [r | r <- database, r /= (start, next, time')]]  --road for the first city
  ++[ (next, start, time') : route | (next, s, time') <- database, s==start, route <- findRoutes next end [r | r <- database, r /= (next, start, time')]]  --road for the second city

routes_distance :: [[Road]]
routes_distance = findRoutes "Shanghai" "Chengdu" distance_database_example

routes_price :: [[Road]]
routes_price = findRoutes "Shanghai" "Chengdu" price_database_example

-- Print routes & Calculate the distance or price
printRoad :: [[Road]] -> [([(CityName, CityName)], Float)]
printRoad routes = map (\route -> (map (\(a, b, _) -> (a, b)) route, sum [w | (_, _, w) <- route])) routes

-- Test the example
main :: IO ()
main = do
    print "Distance_database_example:"
    print $ distance_database_example
    print "Price_database_example:"
    print $ price_database_example
    print "Routes of distance:"
    print $ printRoad routes_distance
    print "Routes of price:"
    print $ printRoad routes_price
  

