{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Db 
import App  (app)
import Network.Wai.Handler.Warp (run)
import CommonConfig ( appjwtSettings )
-- import Mailing 
-- import Data.Text (Text)

main :: IO ()
main = do
    -- sendEmail "vladimirdoraemon@gmail.com" "http://127.0.0.1"
    pool <- createDbPool 
    jwtSettings <- appjwtSettings  
    app' <- app pool jwtSettings 
    print "server running on 3000"
    run 3000 app' 

