-- Qualified import means you'll need to prepend the whole module name.
import qualified Data.Bool -- as B

--          vvvvvvvvv
example x = Data.Bool.bool 0 1 (x == 1) 