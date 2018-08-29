import System.IO
import Control.Exception

-- This is how withFile could be implemented using bracket.
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (openFile name mode) hClose f



