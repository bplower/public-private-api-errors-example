
import Backend.Api ( apiLogin, apiQuery )

-- | Call the "API handlers" pretending we're recieving HTTP requests
main :: IO ()
main = do
  apiLogin
  apiQuery
