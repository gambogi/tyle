import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Control.Monad
import Safe

import Tyle.Parsers (program)

feedback result = case result of
            Left err -> print err
            Right res -> print res

arg1 = liftM (headNote "One argument must be provided.") getArgs

main = do
    filename <- arg1
    feedback =<< parseFromFile program filename
--main = putStrLn "
--    >> putStrLn "
--    >> putStrLn "
