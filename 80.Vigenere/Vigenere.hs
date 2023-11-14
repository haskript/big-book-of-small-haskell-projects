{- cabal:
default-language: GHC2021
build-depends: base, Clipboard
-}

{-| Vigniere Cipher console program based on the version in _The Big Book of
Small Python Projects_ by Al Sweigart. -}
{-# LANGUAGE LambdaCase #-} -- Used to enable LambdaCase syntax to simplify a code section.
module Main where

import Data.Char (chr, isAlpha, isUpper, ord, toUpper)
import System.Clipboard (setClipboardString)
import Data.List (mapAccumL) -- used to map while holding an accumulator
import Data.Foldable (traverse_) -- used to turn a list into an fstring-like structure

type VigenereKey = [Int] -- ^ Type synonym for the key structure (list of keys) to make it a bit more readable.

makeKey :: String -> VigenereKey -- ^ Converts a string to a list of offsets
makeKey = fmap (subtract 65 . ord . toUpper) . filter isAlpha

data VigDirection = Encrypt | Decrypt -- ^ Enum type to avoid using strings; idiomatic.

encrypt, decrypt :: VigenereKey -> String -> String -- ^ Convenient synonyms to avoid directly using applyKey.
encrypt = applyKey Encrypt; decrypt = applyKey Decrypt

{-| Actual encryption decryption function, takes advantage of the fact that
encryption and decryption are just one function apart. -}
applyKey :: VigDirection -> VigenereKey -> String -> String
applyKey vigDirection [] text = text
applyKey vigDirection key text =

{- mapAccumL allows us to map with an accumulator, which is basically a mutable variable,
from left to right with the accumulator being updated by the mapping function.
The result is a tuple of the final accumulator and the mapped data structure.
Haskell's pervasive laziness allows me to use an infinite list generated by cycle as an accumulator! -}

    snd $ mapAccumL adjustAlphaNumerics (cycle key) text
  where
    adjustAlphaNumerics keySpool@(keyShift:restOfKeys) character
      | not $ isAlpha character = (keySpool, character) -- Keep the accumulator unchanged, return character.
      | otherwise =                                     -- take the first element off the accumulator, encrypt / decrypt the character
          ( restOfKeys
          , chr . (+ shiftFactor) . flip mod 26
          . shiftEncryptOrDecrypt . subtract shiftFactor
          $ ord character)
      where
        shiftFactor
            | isUpper character = 65
            | otherwise         = 97
        shiftEncryptOrDecrypt = case vigDirection of
            Encrypt -> (+ keyShift)
            Decrypt -> subtract keyShift

main :: IO () -- ^ Actual user-facing code. "Imperative shell, functional core."
main = do -- Build a basic schematic of the program in main.
    introduction
    getAndProcessInputs >>= -- Bind to allow the following function to act directly
                            -- on the output of the preceding IO action.
        showResultsAndCopyToClipboard
  where -- fill out the details in the where clause.
    introduction = putStrLn
        "Haskell Vigenere Cipher, by Liam Zhu Liam.Zhu@protonmail.com\n\
        \Adapted from Vigenere Cipher in _The Big Book of Small Python\n\
        \Projects_ by Al Sweigart al@inventwithpython.com.\n\
        \The Vigenere cipher is a polyalphabetic substitution cipher that was\n\
        \powerful enough to remain unbroken for centuries."

    getAndProcessInputs = do
        programMode <- getProgramMode
        key  <- makeKey <$> entryPrompt "Please specify the key to use.\n\
                                        \It can be a word or any combination of letters."
        text <- entryPrompt $ "Enter the message you wish to " <> case programMode of
            Encrypt -> "encrypt."
            Decrypt -> "decrypt."

        let processedMessage = (case programMode of
                Encrypt -> encrypt
                Decrypt -> decrypt) key text

        pure (programMode, processedMessage)

    {-| Here, we need a separate definition for getProgramMode since it loops/recurses into itself.
    We direct bind the LambdaCase for concision here; the LambdaCase reads the result and
    chooses either to return a Encrypt or Decrypt enum, or show an error and loop back into itself.-}
    getProgramMode = entryPrompt "Do you want to (e)ncrypt or (d)ecrypt?" >>= \case
          "e" -> pure Encrypt
          "d" -> pure Decrypt
          _   -> do
              putStrLn "Unrecognized input."
              getProgramMode

    showResultsAndCopyToClipboard (programMode, processedMessage) = do
        setClipboardString processedMessage
        traverse_ putStrLn -- traverse is used here to apply putStrLn to a list, which would
                           -- be an f-string in Python.

            [ case programMode of; Encrypt -> "Encrypted message:"; Decrypt -> "Decrypted mssage:"
            , processedMessage
            , "Full " <> (case programMode of; Encrypt -> "encrypted"; Decrypt -> "decrypted")
                      <> " text copied to clipboard."
            ]

entryPrompt :: String -> IO String -- ^ Supporting prompt function.
entryPrompt str = do
    putStrLn str
    putStr "> "
    getLine