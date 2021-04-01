module Main where
import FreeType
import Codec.Picture
import Control.Monad ( when )
import Data.Char ( ord )
import Data.Word ( Word8 )
import Foreign.Marshal.Array ( peekArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( Storable(peek) )
import System.Directory ( doesFileExist, createDirectoryIfMissing )
import System.Environment ( getArgs )
import Text.Read ( readMaybe )

data FontTex = FontTex Int Int [Word8]

data TTFData = TTFData
  { chIndex ∷ Int
  , chW     ∷ Double
  , chH     ∷ Double
  , chX     ∷ Double
  , chY     ∷ Double
  } deriving (Show, Eq)

data Format = FormatPng
            | FormatBmp
            | FormatGif
            | FormatJpg
            | FormatTiff
            | FormatNULL deriving (Show, Eq)

main ∷ IO ()
main = do
  args ← getArgs
  check ← checkArgs args
  case check of
    Nothing → printUsage
    Just (fp,size,format,dir) → do
      putStrLn "generating images..."
      createDirectoryIfMissing True dir
      saveImages fp size format ['!'..'~']

saveImages ∷ String → Int → Format → [Char] → IO ()
saveImages fp size format chars = resequence_ $ map (saveCharImage fp size format) chars
saveCharImage ∷ String → Int → Format → Char → IO ()
saveCharImage fp size format char = do
  FontTex w h buff ← loadFTChar fp char size
  let genImg ∷ DynamicImage
      genImg = ImageRGBA8 (generateImage genFunc w h)
      genFunc ∷ Int → Int → PixelRGBA8
      genFunc x y = PixelRGBA8 g g g a
        where g = buff !! (x + (y*w))
              a = if g < 170 then 0 else 255
      name = findName char
  case format of
    FormatPng  → savePngImage ("out/" ++ name ++ ".png") genImg
    FormatBmp  → saveBmpImage ("out/" ++ name ++ ".bmp") genImg
    FormatGif  → case saveGifImage ("out/" ++ name ++ ".gif") genImg of
      Left err → putStrLn $ "gif error: " ++ err
      Right v  → v
    FormatJpg  → saveJpgImage 100 ("out/" ++ name ++ ".jpg") genImg
    FormatTiff → saveTiffImage ("out/" ++ name ++ ".tiff") genImg
    FormatNULL → putStrLn "unknown file format"

findName ∷ Char → String
findName '!'  = "bang"
findName '\"' = "dquote"
findName '#'  = "pound"
findName '$'  = "dollar"
findName '%'  = "percent"
findName '&'  = "ampersand"
findName '\'' = "quote"
findName '('  = "lparen"
findName ')'  = "rparen"
findName '*'  = "asterisk"
findName '+'  = "plus"
findName ','  = "comma"
findName '-'  = "minus"
findName '.'  = "period"
findName '/'  = "bslash"
findName ':'  = "colon"
findName ';'  = "semicolon"
findName '<'  = "lcarot"
findName '='  = "equals"
findName '>'  = "rcarot"
findName '?'  = "question"
findName '@'  = "at"
findName '['  = "lbracket"
findName '\\' = "fslash"
findName ']'  = "rbracket"
findName '^'  = "carot"
findName '_'  = "underscore"
findName '`'  = "tick"
findName '{'  = "lbrace"
findName '|'  = "pipe"
findName '}'  = "rbrace"
findName '~'  = "tilde"
findName ch   = [ch]

printUsage ∷ IO ()
printUsage = do
  putStrLn "usage: sermo [path] [size] [png,bmp,gif,jpg,tiff] [output directory]"
  putStrLn "the last two arguments are optional, will default to png and ./out"

checkArgs ∷ [String] → IO (Maybe (String,Int,Format,String))
checkArgs args
  | length args < 2 = return Nothing
  | otherwise       = do
      res0 ← doesFileExist (head args)
      let res1   = readMaybe (args !! 1) ∷ Maybe Int
          format = if length args > 2 then checkFormat (args !! 2) else FormatPng
          dir    = if length args > 3 then args !! 3 else "./out"
      case res1 of
        Nothing → return Nothing
        Just r0 → if res0
                    then return $ Just (head args, r0, format,dir)
                    else return Nothing

checkFormat ∷ String → Format
checkFormat "png"  = FormatPng
checkFormat "bmp"  = FormatBmp
checkFormat "gif"  = FormatGif
checkFormat "jpg"  = FormatJpg
checkFormat "tiff" = FormatTiff
checkFormat _      = FormatNULL

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

loadFTChar ∷ FilePath → Char → Int → IO FontTex
loadFTChar fp char px = do
  ft_With_FreeType $ \lib →
    ft_With_Face lib fp 0 $ \face → do
      isScalable ← FT_IS_SCALABLE face
      when isScalable $ ft_Set_Char_Size face 0 ((fromIntegral px) * 64) 0 0
      ft_Load_Char face (fromIntegral $ ord char) FT_LOAD_RENDER
      slot ← peek . frGlyph =<< peek face
      withBitmap lib (gsrBitmap slot) $ \bmap → do
        let bufferSize = fromIntegral $ (bRows bmap) * fromIntegral (bPitch bmap)
        buffr ← peekArray bufferSize $ bBuffer bmap
        return $ FontTex (fromIntegral (bPitch bmap)) (fromIntegral (bRows bmap)) buffr

withBitmap ∷ FT_Library → FT_Bitmap → (FT_Bitmap → IO a) → IO a
withBitmap lib source f =
  if any (== bPixel_mode source)
       [ FT_PIXEL_MODE_MONO, FT_PIXEL_MODE_GRAY2
       , FT_PIXEL_MODE_GRAY4, FT_PIXEL_MODE_BGRA ]
    then ft_Bitmap_With lib $ \targetPtr → do
           with source $ \sourcePtr → do
             ft_Bitmap_Convert lib sourcePtr targetPtr . fromIntegral $ bPixel_mode source
             f =<< peek targetPtr
    else f source

drawBitmap ∷ Int → [Word8] → IO ()
drawBitmap _ [] = return ()
drawBitmap n list = do
  putStrLn $ color <$> take n list
  drawBitmap n $ drop n list
  where
    color :: Word8 -> Char
    color a =
      case () of
        () | a == 0    -> ' '
           | a < 85    -> '░'
           | a < 170   -> '▒'
           | a < 255   -> '▓'
           | otherwise -> '█'
