import System.Environment
import qualified Data.Map as Map

characters = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
codes = ["-----", ".----", "..---", "...--", "....-", ".....", "-....", "--...", "---..", "----."] ++ [".-", "-...", "-.-.", "-..", ".", "..-.", "--.", "....", "..", ".---", "-.-", ".-..", "--", "-.", "---", ".--.", "--.-", ".-.", "...", "-", "..-", "...-", ".--", "-..-", "-.--", "--.."] ++ [".-", "-...", "-.-.", "-..", ".", "..-.", "--.", "....", "..", ".---", "-.-", ".-..", "--", "-.", "---", ".--.", "--.-", ".-.", "...", "-", "..-", "...-", ".--", "-..-", "-.--", "--.."]

morseToAlphaMap :: Map.Map String Char
morseToAlphaMap = Map.fromList $ zip codes characters
alphaToMorseMap :: Map.Map Char String
alphaToMorseMap = Map.fromList $ zip characters codes

alphaToMorse :: String -> String
alphaToMorse sentence = unlines $ map wordToCode $ words sentence
    where
        wordToCode word =
            unwords $ map (\c -> 
                    case Map.lookup c alphaToMorseMap of
                        Just code -> code
                        Nothing -> error "Only alphabets, numbers and spaces."
                )
                word

morseToAlpha :: String -> String
morseToAlpha code = unwords $ map codeToWord $ lines code
    where
        codeToWord code =
            map (\letterCode ->
                    case Map.lookup letterCode morseToAlphaMap of
                        Just letter -> letter
                        Nothing -> error "Incorrect Code."
                ) 
                $ words code

main = do
    args <- getArgs
    let
        output =
            if length args == 2
                then let (mode:str:[]) = args
                    in
                        case mode of
                            "-a" -> alphaToMorse str
                            "-m" -> morseToAlpha str
                            _ -> error "Allowed modes : -a (alpha to morse), -m (morse to alpha)"
                else error "Expected two arguments."
    putStrLn output