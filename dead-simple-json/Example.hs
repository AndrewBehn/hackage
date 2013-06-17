{-# LANGUAGE Haskell2010, TemplateHaskell, QuasiQuotes #-}

module Main where


import Text.Printf (printf)

import Text.DeadSimpleJSON
import Text.DeadSimpleJSON.TH

import Data.Ratio


main :: IO ()
main = do
    
    let jsonData = read "{\"seven\": 7, \"nine\": [1,2,4,8,16]}"
    print $ (jsonData ? "nine[3]" :: Int)

    let jone = [json|
                    [
                        {
                            "one": 0.1,
                            "two": 123
                        },
                        {
                            "one": true,
                            "two": null,
                            "three": 23412349.24e-12
                        },
                        {
                            "one": "ONE",
                            "two": null,
                            "three": "THREE"
                        },
                        {
                            "one": false,
                            "two": 14964674393249287
                        }
                    ]
                |]
        jtwo = jone -- [jsonF|json2.txt|]
        jtri = [json| ["Naks", "Noks", "Nuks"] |]
        jfrr = jone -- read [sF|json2.txt|] :: JSON

        val1 = [jsq| jone[3].two |] :: String
        val2 = [jsq| jtwo[4] |]     :: Double
        val3 = [jsq| jtri |]        :: String

    print $ length $ show jfrr
    print $ length $ show jtwo

    print val1
    print val2
    print val3



