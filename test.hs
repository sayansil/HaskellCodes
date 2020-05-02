import Math.NumberTheory.Primes

main :: IO ()
main = do
    print (takeWhile (<= 1000000) primes)