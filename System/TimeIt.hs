module System.TimeIt(timeIt, timeItT) where
import System.CPUTime
import Text.Printf

-- |Wrap an 'IO' computation so that it prints out the execution time.
timeIt :: IO a -> IO a
timeIt ioa = do
    (t, a) <- timeItT ioa
    printf "CPU time: %6.2fs\n" t
    return a

-- |Wrap an 'IO' computation so that it returns execution time is seconds as well as the real value.
timeItT :: IO a -> IO (Double, a)
timeItT ioa = do
    t1 <- getCPUTime
    a <- ioa
    t2 <- getCPUTime
    let t :: Double
	t = fromIntegral (t2-t1) * 1e-12
    return (t, a)
