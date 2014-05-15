-- | Main entry point to the application.
module Main where

-- | The main entry point.
q0,c1,c2,m,sigma,tc,kappa :: Double
q0 = 342.5
c1 = 0.15
c2 = 0.7
m = 0.4
sigma = 5.6697e-8
tc = 273
kappa = 0.05

t0 :: (Fractional a, Floating a) => a
t0 = (1.9e-15)**(-1/6) -- correct
-- T0 = (1.9e-15)^(1/6) # ZG10 bug

t :: [Double]
t = [200,300,400]

--mu :: [a]
mu = 0.5

-- test results
-- [0.5,1.0,2.0]

-- mu = 0.5 ==> 159.1940519
--      1.0 ==> 21.1804830
--      ros - ris ==> [34.787581731296,21.18048301470816,579.7418534269186]
--      2.0 ==> -254.8466548
--      ros ==> [86.32443281596522,297.20762090004325,870.866121921798]
--      ris ==> [10275.00000008155,15412.500000122325,20550.0000001631]
--      ros - ris = [-16.749269353373222,-254.84665487062694,288.61758493203934]
main :: IO ()
main = do
  let ros = fmap ro t
      ir mu t = ri t mu
      ris = fmap (ir mu) t
  print $ zipWith (-) ros ris

  
-- incoming radiation
ri :: Double -> Double -> Double
ri t mu = mu * q0 * (1-alpha) where
    alpha = c1 + c2*(1-tanh(kappa*(t-tc)))/2
    
-- gaiigi.com

-- outgoing radiation
ro :: Double -> Double
ro t = sigma * g * t**4 where 
       g = 1 - m*tanh((t/t0)^6)    
   
-- plot outgoing radation as a function of temperature
-- plot(T, Ro(T),

-- xRng,yRng :: [Int]
-- xRng = [150..400] 
-- yRng = [0..630]

-- "Zaliapin and Ghil (2010), Fig. 4", 