import qualified System.IO as F

-- Euler法
euler :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double
euler func h y t = y + h * func t y

-- ルンゲ・クッタ4次公式
lk4 :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double
lk4 func h y t = y + k
  where k1 = h * func t y
        k2 = h * func (t+h/2) (y+k1/2)
        k3 = h * func (t+h/2) (y+k2/2)
        k4 = h * func (t+h) (y+k3)
        k = (k1 + 2*k2 + 2*k3 + k4)/6

-- Euler法を用いてt=n*h+t0までの微分方程式の解を(t, y)のリストとして得る
solveByEuler :: (Double -> Double -> Double) -> Double -> (Double, Double) -> Integer -> [(Double, Double)]
solveByEuler func h (t0, y0) n = zip (t0:t) y
  where ns = map fromIntegral [1..n] -- ns = 1, 2, ... n 後でDoubleになるのでfromIntegralしておく
        t = map (\x -> x*h+t0) ns -- tの各ステップの値
        y = scanl (euler func h) y0 t

-- ルンゲ・クッタ4次公式を用いてt=n*h+t0までの微分方程式の解を(t, y)のリストとして得る
solveByLk4 :: (Double -> Double -> Double) -> Double -> (Double, Double) -> Integer -> [(Double, Double)]
solveByLk4 func h (t0, y0) n = zip (t0:t) y
  where ns = map fromIntegral [1..n] -- ns = 1, 2, ... n 後でDoubleになるのでfromIntegralしておく
        t = map (\x -> x*h+t0) ns -- tの各ステップの値
        y = scanl (lk4 func h) y0 t

-- Show型の要素数2のタプルをcsv形式のStringに変換する
tapleToCsv :: Show a => (a, a) -> String
tapleToCsv t = f ++ "," ++ s ++ "\n"
  where f = show $ fst t
        s = show $ snd t

-- Show型の要素数2のタプルのリストをcsv形式のStringに変換する、タプル1つ毎に改行される
tapleListToCsv :: Show a => [(a, a)] -> String
tapleListToCsv [] = []
tapleListToCsv ts = (tapleToCsv $ head ts) ++ (tapleListToCsv $ tail ts)

-- 解きたい方程式
fty :: Double -> Double -> Double
fty t y = - 0.01 * y

-- main
main = do
  let result_euler = tapleListToCsv $ solveByEuler fty 30 (0,1) 20
  let result_lk4   = tapleListToCsv $ solveByLk4 fty 30 (0,1) 20
  F.writeFile "euler.csv" result_euler
  F.writeFile "lk4.csv" result_lk4
