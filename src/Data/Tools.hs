
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tools
(
   chunk,
   showByteString,
   putInt32le,
   getInt32le,
   putInt64le,
   getInt64le
) where


import Data.Word
import Text.Printf
import Data.Binary
import Data.Bits
import Data.Int



showByteString :: Int -> [Word8] -> String
showByteString lineSize str =
   showLines 0 $ chunk lineSize str
   where
      showLines :: Int -> [[Word8]] -> String
      showLines _ [] = "";
      showLines addr (line:rest) =
         printf "%04x" addr ++ showBytes line ++ " - " ++ showBytesChar line ++ "\n" ++ showLines (addr + lineSize) rest

      showBytesChar :: [Word8] -> String
      showBytesChar = foldl (\txt b -> txt ++ [byteToChar b]) ""

      showBytes :: [Word8] -> String
      showBytes = foldl (\txt b -> txt ++ printf " %02x" b) ""

      byteToChar :: Word8 -> Char
      byteToChar byte =
         let
            c = toEnum . fromEnum $ byte
         in
            if c >= ' ' && c <= '~'
               then c
               else '.'


chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs


putInt32le :: Int -> Put
putInt32le i = do
   put (fromIntegral (i .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 8) .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 16) .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 24) .&. 0xff) :: Word8)

putInt64le :: Int64 -> Put
putInt64le i = do
   put (fromIntegral (i .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 8) .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 16) .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 24) .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 32) .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 40) .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 48) .&. 0xff) :: Word8)
   put (fromIntegral ((shiftR i 56) .&. 0xff) :: Word8)


getInt32le :: Get Int
getInt32le = do
   i0 :: Word8 <- get
   i1 :: Word8 <- get
   i2 :: Word8 <- get
   i3 :: Word8 <- get
   return $ fromIntegral i0 +
            fromIntegral i1 `shiftL` 8 +
            fromIntegral i2 `shiftL` 16 +
            fromIntegral i3 `shiftL` 24

getInt64le :: Get Int64
getInt64le = do
   i0 :: Word8 <- get
   i1 :: Word8 <- get
   i2 :: Word8 <- get
   i3 :: Word8 <- get
   i4 :: Word8 <- get
   i5 :: Word8 <- get
   i6 :: Word8 <- get
   i7 :: Word8 <- get
   return $ fromIntegral i0 +
            fromIntegral i1 `shiftL` 8 +
            fromIntegral i2 `shiftL` 16 +
            fromIntegral i3 `shiftL` 24 +
            fromIntegral i4 `shiftL` 32 +
            fromIntegral i5 `shiftL` 40 +
            fromIntegral i6 `shiftL` 48 +
            fromIntegral i7 `shiftL` 56
