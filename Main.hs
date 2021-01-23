{-# LANGUAGE DataKinds #-}

module Main where


-- ghcid "--command=stack ghci" --test=:main

import Vivid

theSound :: SynthDef '["note"]
theSound = sd (0 ::I "note") $ do
    wobble <- sinOsc (freq_ 5) ? KR ~* 10 ~+ 10
    s <- 0.1 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
    out 0 [s,s]

playSong :: IO ()
playSong = do
    fork $ do
        s0 <- synth theSound (36 ::I "note")
        wait 1
        free s0
    s1 <- synth theSound (60 ::I "note")
    forM_ [62,66,64] $ \note -> do
        wait (1/4)
        set s1 (note ::I "note")
    wait (1/4)
    free s1

main :: IO ()
main = do
    putStrLn "Simplest:"
    playSong