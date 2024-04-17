node_of_ten :: [Double]
node_of_ten = [-0.9739065285171710,-0.8650633666889840,-0.6794095682990240,-0.4333953941292470,-0.1488743389816310,0.1488743389816310,0.4333953941292470,0.6794095682990240,0.8650633666889840,0.9739065285171710]

weight_of_ten :: [Double]
weight_of_ten = [0.0666713443086881,0.1494513491505800,0.2190863625159820,0.2692667193099960,0.2955242247147520,0.2955242247147520,0.2692667193099960,0.2190863625159820,0.1494513491505800,0.0666713443086881]


fx_fixed :: Double -> Double
fx_fixed x = (\x a b c -> a*x^2 + b*x + c) x 17.0 8.0 9.0

{-
fx_fixed x = fx x 17.0 8.0 9.0
    where
        --fx :: Double -> Double -> Double -> Double -> Double
        fx x a b c = a*x^2 + b*x + c
-}

gauss :: [Double] -> [Double] -> (Double -> Double) -> Double
gauss node weight f = foldr (+) 0 (zipWith (*) weight (map f node))


main :: IO()
main = do
    print $ gauss node_of_ten weight_of_ten fx_fixed