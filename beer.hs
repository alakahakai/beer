-- Calculate the total number of beer that one can drink with n dollars.
import           Control.Applicative (liftA)

newtype Money = Money Double deriving Show
newtype Cap = Cap Int deriving Show
newtype Bottle = Bottle Int deriving Show
newtype Consumed = Consumed Int deriving Show
data Asset = Asset Money Cap Bottle deriving Show

-- Function to create Asset type for convenience
asset :: Double -> Int -> Int -> Asset
asset m c b = Asset (Money m) (Cap c) (Bottle b)

print' :: (Show a) => a -> IO ()
print' x = putStrLn ("  " ++ show x)

-- Price of a bottle of beer
price :: Double
price = 2.0

-- Number of caps that can exchange for one bottle of beer
capER :: Int
capER = 4

-- Number of empty bottles that can exchange for one bottle of beer
bottleER :: Int
bottleER = 2

-- Value of beer in the bottle
consumable :: Double
consumable = (1 - 1/fromIntegral capER - 1/fromIntegral bottleER) * price

-- Value of the Asset
value :: Asset -> Double
value (Asset (Money m) (Cap c) (Bottle bo)) =
  m + price * fromIntegral c/fromIntegral capER +
  price * fromIntegral bo/fromIntegral bottleER

-- Drink action
drink :: (Consumed, Asset) -> Bool -> IO (Consumed, Asset)
drink ret@(Consumed v, Asset (Money m) (Cap c) (Bottle bo)) f
  | m >= price = do
    let consumed = Consumed $ v + 1
        asse = Asset (Money (m - price)) (Cap $ c + 1) (Bottle $ bo + 1)
    print' (consumed, asse)
    drink (consumed, asse) f
  | c >= capER = do
    let consumed = Consumed $ v + 1
        asse = Asset (Money m) (Cap $ c - capER + 1) (Bottle $ bo + 1)
    print' (consumed, asse)
    drink (consumed, asse) f
  | bo >= bottleER = do
    let consumed = Consumed $ v + 1
        asse = Asset (Money m) (Cap $ c + 1) (Bottle $ bo - bottleER + 1)
    print' (consumed, asse)
    drink (consumed, asse) f
  | f         = return ret
  | otherwise = do
      let va = value (Asset (Money m) (Cap c) (Bottle bo))
      if va >= consumable
      then do
        let consumed = Consumed $ v + 1
            asse = Asset (Money $ m - consumable) (Cap c) (Bottle bo)
        print' (consumed, asse)
        drink (consumed, asse) f
      else
        return ret

main :: IO ()
main = do
  putStr "Input money amount:"
  m <- liftA (\x -> read x :: Double) getLine
  r <- drink (Consumed 0, asset m 0 0) True
  putStrLn $ show (fst r) ++ ", remaining: " ++ show (snd r) ++
    ", total remaining value: " ++ show (value (snd r))
  putStrLn "If borrowing money is allowed, then:"
  r' <- drink (Consumed 0, asset m 0 0) False
  putStrLn $ show (fst r') ++ ", remaining: " ++ show (snd r') ++
    ", total remaining value: " ++ show (value (snd r'))
