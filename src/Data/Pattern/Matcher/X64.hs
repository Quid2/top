{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE TupleSections             #-}
module Data.Pattern.Matcher.X64(match, matcher) where
import           CodeGen.X86
import           Control.Monad            (when)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import qualified Data.Map                 as M
import           Data.Pattern.Matcher
import           Data.Pattern.Types
import           ZM               hiding (label)
import           Data.Word
import           Foreign
import           System.IO.Unsafe         (unsafePerformIO)
-- import Data.Memory.Endian
import Data.Pattern.Bits

foreign import ccall "dynamic" callWW :: FunPtr (Int -> Int -> Ptr Word8 -> IO Word8) -> Int -> Int -> Ptr Word8 -> IO Word8
instance Callable (Int -> Int -> Ptr Word8 -> IO Word8) where dynCCall = callWW

matchPM :: PatternMatcher -> B.ByteString -> Bool
matchPM = match . matcher

match :: (Int -> Int -> Ptr Word8 -> IO Word8) -> B.ByteString -> Bool
match matcher bs =
  let (ptr,off,len) = B.toForeignPtr bs
  in trueCode == unsafePerformIO (withForeignPtr ptr (matcher off len))

retCode c | c == falseCode = Right False
          | c == trueCode = Right True
          | c== errorNotEnoughDataCode = Left "Not enough data"
          | otherwise = Left "Unknown return code"

falseCode = 0
trueCode = 1
errorNotEnoughDataCode = 2

matcher = compile . matcherCode

-- #define ZZ -- INCOMPLETE

-- CHK: stack overflow for deeply nested structures?
matcherCode
  :: (Foldable t, Functor t) =>
     (M.Map AbsType BTree, t (Match [Bit])) -> CodeM ()
matcherCode pm = mdo
  -- bt (addr8 arg3) al
  -- bt (addr16 arg3) ax
  let (tt,pat) = mapPM (listSplit 8) pm

  -- Inputs
  let bsOffset = arg1
  let bsLength = arg2
  let bsBase = arg3

  -- State
  let ptr = arg3
#ifdef ZZ
  -- INVARIANT: if totBits>0 then topByte=current byte, shifted left to have first valid bit as msb
  let topByte = cl
#else
  let topBit = cl -- must be cl to be used in shifts -- lower part of arg4 in unix or arg1 in windows
  let topBit16 = cx
  mov topBit16 7
#endif
  let totBits = arg2
  -- INVARIANT: ptr points to current byte
  add ptr bsOffset

  -- mov totBits bsLength
  shl totBits 3

  --  let ptrLast = arg2
  -- let cachedBits = dl -- arg3 -- topBit in current byte 0..8
  -- let word = al -- lower result
  -- let word = addr8 ptr
  -- let top = arg1
  -- let topl = al

  let work8 = al
  let work8h = ah
  let work16 = ax
  let work32 = eax
  let work64 = rax

  let other8 = r9b;other16 = r9w;other32 = r9d;other64 = r9

  --mov ptr bsBase
  --add ptrLast ptr
  --dec ptrLast

  --mov numBits bsLength
  -- shl topBit 3
  --dbg ptr >> dbg (addr8 ptr) >> dbg totBits >> dbg topBit

  let

    matchPattern (MatchAny t) = matchType False t

    matchPattern (MatchValue bs) = do
      -- sum lenght of all bit fields
      ensureBits $ sum $ map length bs
      mapM_ matchBits bs

    -- PRE: ensureBits
    matchBits bs = do
      let n = length bs
      -- ensureBits n
      cmpBits n bs

    matchType isTail t = do
      --dbg ax >> dbg ptr >> dbg (addr8 ptr) >> dbg totBits >> dbg topBit

      case solve t ll of
        Just l  -> (if isTail then jmp else call . ipRelValue) l
        Nothing -> return ()

      -- dbg bx >> dbg ptr >> dbg (addr8 ptr) >> dbg totBits >> dbg topBit

    -- Critical section
    matchTree (BFork l r) = do
      ensureBits 1

      -- dropBit this could be moved on both branches of if2 to avoid the need for retestBit
      -- peekBit >> dropBit >> retestBit
      peekBit >> dropBits 1
      if2 l r

    -- Tail call optimisation
    matchTree (BCon ts) = mapM_ (matchType False) (init ts) >> matchType True (last ts)

    matchTree (Skip n) = do
      ensureBits n
      dropBits n

    -- both empty branches are already converted to Skip1
    if2 (BCon []) (BCon []) = return ()
    if2 (BCon []) r         = unless bitLeft (matchTree r)
    if2 l (BCon [])         = unless bitRight (matchTree l)
    if2 l r                 = if_ bitLeft (matchTree l) (matchTree r)

    compType (t,tree) = (t,) <$> compileType (t,tree)

    compileType (t,BCon []) = return Nothing

    compileType (t,BCon [t2]) = return $ solve t2 ll

    compileType (t,tree) = do
      l <- label
      matchTree tree
      ret
      return $ Just l

    -- Lower level primitives
    -- Alternative: expand buffer to multiple of 8 bytes and just read 64 bits at a time?
    -- Is it fast?
    cmpBits n bs | n == 1 = do

                     peekBit
                     j (if bs == [V1] then bitLeft else bitRight) retFalse

                     --dbg ptr >> dbg (addr8 ptr) >> dbg totBits >> dbg topBit
                     dropBits 1

                   | n <=8 = do
                     cmp topBit (fromIntegral $ n-1)
                     if_ C (do  -- two bytes
                                --dbg ax
                                --let pat = fromIntegral . fix16 . byteSwap16 . (fromIntegral::Integer->Word16) $ asMSBits 16 bs
                                --let pat = fromIntegral . fix16 . unLE . toLE . (fromIntegral::Integer->Word16) $ asMSBits 16 bs
                                let pat = fromIntegral $ asMSBits 16 bs

                                -- get word
                                mov work16 (addr16 ptr)
                                -- big endian to little endian
                                xchg work8 work8h

                                -- shift it to MSB
                                mov other8 7
                                sub other8 topBit
                                xchg other8 topBit
                                shl work16 topBit
                                xchg other8 topBit

                                -- compare with pattern
                                and_ work16 (fromIntegral $ andMask 16 n)
                                --dbg work16 >> mov other16 pat >> dbg other16
                                xor_ work16 pat
                                j NZ retFalse

                            ) (do -- one byte
                                  --dbg bx
                                  -- pat=<101>00000
                                  let pat = fromIntegral $ asMSBits 8 bs

                                  -- get word
                                  mov work8 (addr8 ptr)
                                  --dbg work8

                                  --  and shift it to MSB
                                  mov other8 7
                                  sub other8 topBit
                                  xchg other8 topBit
                                  shl work8 topBit
                                  xchg other8 topBit
                                  --dbg work8

                                  -- compare with pattern
                                  and_ work8 (fromIntegral $ andMask 8 n)
                                  --dbg work8
                                  -- dbg work8 >> mov other8 pat >> dbg other8
                                  xor_ work8 pat
                                  j NZ retFalse
                              )
                     dropBits n
                     -- dbg ptr >> dbg (addr8 ptr) >> dbg totBits >> dbg topBit


    -- | make_mask 3 = 00000111
    -- make_mask n = (1 `shiftL` fromIntegral n) - 1 --
    -- andMask8 n = not (shiftR 255 n)

#ifdef ZZ
    nextBit = do
      -- cmp totBits (fromIntegral reqBits)
      -- reduce tot count immediately
      xor_ totBits totBits
      -- expect NC $ falseCode
      j Z retFalse
      test totBits 7
      unless NZ (mov topByte (addr8 ptr))
      shl topByte

    dropBits n | n == 1 = cmp totBits 8 >> unless NZ (inc ptr)
               | otherwise = do
      let m = n `mod` 8
      when (m > 0) $ do
        unless NC (incPtr 1)
      incPtr (n `div` 8)

    peekBit = do
      test totBits 7
      unless NZ (mov topByte (addr8 ptr))
      shl topByte

#else
    -- Preserves carry
    dropBits n | n == 1 = dec topBit >> unless NS (inc ptr >> mov topBit 7)
               | otherwise = do
      let m = n `mod` 8
      when (m > 0) $ do
        sub topBit (fromIntegral m)
        unless NC (incPtr 1 >> add topBit 8)
      incPtr (n `div` 8)
      --dbg ptr >> dbg (addr8 ptr) >> dbg totBits >> dbg topBit

      -- sub totBits (fromIntegral n)
      -- mov topBit 7
      -- mov work8 (resizeOperand totBits) -- (fromIntegral $ n `div` 8)
      -- sub topBit work8
      -- mov topBit (resizeOperand totBits) -- (fromIntegral $ n `div` 8)
      -- and_ topBit 7
      -- dec topBit

    peekBit = do
      -- PROB: we assume that this access only the required byte
      -- and unfortunately this doesn't seem to be the case
      -- also, this seems slower than the previous solution (maybe because it loads 16 bits rather than 8)
      -- TODO: use bt to access any bit using a single register (dropBit can be removed)
      -- for example totBits as the bit index can be –2^63 to +2^63 – 1
      -- also: to compare sequences of bits need to get topBit from topBits (topBit = topBits mod 8)
      -- bt (addr16 ptr) topBit16

      -- slightly faster
      mov work8 (addr8 ptr) >> bt work16 topBit16
      -- pointBit >> testBit
--      where
--        pointBit = mov work8 1 >> shl work8 topBit
--        testBit = and_ work8 (addr8 ptr)
        --testBit = test work8 (addr8 ptr)

    -- retestBit = cmp work8 0
    -- retestBit = popf
#endif

    incPtr n = i n -- (n `div` 8)
       where i 0 = return ()
             i 1 = inc ptr
             i n = add ptr (fromIntegral n)

    -- bitLeft = Z;bitRight = NZ
    bitLeft = NC;bitRight = C

    ensureBits reqBits = do
      -- cmp totBits (fromIntegral reqBits)
      -- reduce tot count immediately
      sub totBits (fromIntegral reqBits)
      -- expect NC $ falseCode
      j C retFalse

  mapM_ matchPattern pat

  -- dbg ptr >> dbg (addr8 ptr) >> dbg totBits >> dbg topBit

  -- check for correct flat encoding ending
  -- Correct endings:
  -- a) totBits<=7 and lastByte ending=0[totBits-1]1
  -- b) totBits==8 and lastByte=1
  -- dbg totBits >> dbg topBit >> dbg (addr8 ptr)
  cmp totBits 8
  j G retFalse
  if_ E (do
            --dbg ax
            mov work8 1
            xor_ work8 (addr8 ptr)
            --dbg totBits >> dbg topBit >> dbg (addr8 ptr)
            j NE retFalse
        ) (do
              --dbg bx
              mov work8 254
              -- 11111110 -> topBit 3 -> 11110000 -> not -> 00001111
              shl work8 topBit
              not_ work8
              and_ work8 (addr8 ptr)
              --dbg work8
              xor_ work8 1
              j NE retFalse
              )

  mov result trueCode
  ret

  ll <- M.fromList <$> mapM compType (M.toList tt)

  retFalse <- label
  mov result falseCode
  ret

  --failOn condition = j unless condition (jmp )

-- expect condition onFail = unless condition (retFailure onFail)

-- retFailure onFail = mov result onFail >> ret

-- dbg :: IsSize s => Operand RW s -> Code
dbg = traceReg "u"
-- dbg _ = return ()

{-
    -- | make_mask 3 = 11100000
    -- makeMask n = do
    --   mov work64 (-1) -- 255
    --   shr work64 n
    --   not_ work64
    --   ret

    -- dropBits n = do
    --   sub cachedBits (fromIntegral n)
    --   shl top (fromIntegral n)

    -- ensureBits reqBits = do
    --   cmp cachedBits (fromIntegral reqBits)
    --   -- ? call won't work with labels
    --   unless NC $ call (ipRelValue loadData)


      -- cmp topBit 8
      -- j Z noSpace

      -- inc topBit
      -- jmp done

      -- noSpace <- label
      -- mov topBit 1
      -- inc ptr
      -- cmp ptrLast ptr
      -- expect NC errorNotEnoughDataCode

      -- done <- label
      -- return ()

    -- dropBits n | n>0 && n <=64 = do
    --   sub cachedBits (fromIntegral n)
    --   shl top (fromIntegral n)

      -- faster
      -- shld work top n

      ---  top:  SSS.....
      ---  if length bits <= length topBits
      ---  bits: XXXX0000
      -- extract top bits to compare
      -- mov work bits
      -- and_ work top
      -- and_ bits top
      -- shl word (fromIntegral n)

    -- getBit = do
    --   dec cachedBits
    --   shl top 1

    --   add topBit n
    --   mov work topBit
    --   shr work 3
    --   and_ topBit 7
    --   add ptr work64
    --   cmp ptrLast ptr
    --   expect NC errorNotEnoughDataCode

    -- ensureBit = do
    --   dec memBits
    --   expect NC errorNotEnoughDataCode

    -- cacheBits: valid bits in top
    -- memBits: valid bits in mem
    -- totBits: cacheBits+totBits
    -- reqBits: requested bits (constant)
    -- top: VVV....
    -- load up to 64 bits in top, called if if reqBits <= cacheBits then done else loadDataF

        -- mov work64 topBit validBits

        -- mov work64 topBit validBits
        -- and_ work64 7
        -- unless NZ $ do
        --   mov work (addr8 ptr)
        --   inc ptr
        -- ret


  -- cacheBits: valid bits in top
  -- memBits: valid bits in mem
  -- totBits: cacheBits+totBits
  -- reqBits: requested bits (constant)
  -- top: VVV....
  -- load up to 64 bits in top, called if if reqBits <= cacheBits then done else loadDataF
  -- loadData = do
  -- let validBits = work8
  --
  -- loadData <- label
-}

{-
        -- calc bits to load = min (64-validBits) memBits
        -- or load as many bits as possible in a go?


        -- if memBits >=64
        mov work64 (addr64 ptr)
        bswap work64
        shr work64 validBits
        and_ top work64

       --load first mem byte (that might be )
       mov work8 (addr8 ptr)
       shl work8 used
       shl work64 used (validBits+usedMemBits)
       shr work64 validBits
       and_ top work64
-}
-- -- if reqBits > memBits+cacheBits then fail_no_data
  -- mov work8 cachedBits
  -- add work8 memBits
  -- cmp work8 reqBits
  -- unless NC $ do
  --   pop work64 -- go back to original caller
  --   retFailure errorNotEnoughDataCode

  -- mov top (addr64 ptr)
  --     -- -- PRE: cacheBits < reqBits <= totBits
      -- --
      -- -- so just load as much data as possible

      --   --
      --   -- cmp ptr ptrLast
      --   -- expect NC errorNotEnoughDataCode

      --   totBits
      --   mov memBytes totBits
      --   shr memBytes 3
      --   mov memHeadBits totBits
      --   and_ memHeadBits 7
      --   unless_ Z
      --   if totBits >64
      --   mov top ptr
      --   add ptr 64
      --   mov toShift (8-memHeadBits)
      --   unless Z
      --     shl top toShift
      --     shr ptr

      --   -- move min(memBits,64-cacheBits)
      --   -- memBits = totBits-cacheBits
      --   mov memBits totBits
      --   sub memBits cacheBits

      --   -- if cacheBits== just load a byte
      --   cmp cacheBits 0
      --   if_ Z (do
      --             mov a8 (add8 ptr)
      --             inc ptr
      --             mov cacheBits memBits)

      --   cmp totBits 8
      --   mov word (addr8 ptr)

      --   -- how many bits to load? 8-cacheBits
      --   mov movedBits 8
      --   sub movedBits cacheBits


      --   -- merge with top
      --   mov a8 (addr8 ptr)
      --   shr a8 movedBits
      --   and_ cache a8

      --   -- have we crossed a byte boundary?
      --   -- yes, if movedBits > totBits least significant bits 3 then
      --   mov a8 totBits
      --   and a8 7
      --   sub a8 movedBits
      --   when_ N (inc ptr)

      --   -- adjust totBits
      --   sub totBits b8





    --   cmp memBits reqBits
    --   expect NC errorNotEnoughDataCode

    --   -- if reqBits > memBits+cacheBits then fail_no_data
    --   cmp totBits reqBits
    --   expect NC errorNotEnoughDataCode

    --   -- PRE: cacheBits < reqBits <= totBits
    --   --
    --   -- so just load as much data as possible
    --   loadDataF <- label
    --     --
    --     -- cmp ptr ptrLast
    --     -- expect NC errorNotEnoughDataCode

    --     totBits
    --     mov memBytes totBits
    --     shr memBytes 3
    --     mov memHeadBits totBits
    --     and_ memHeadBits 7
    --     unless_ Z
    --     if totBits >64
    --     mov top ptr
    --     add ptr 64
    --     mov toShift (8-memHeadBits)
    --     unless Z
    --       shl top toShift
    --       shr ptr

    --     -- move min(memBits,64-cacheBits)
    --     -- memBits = totBits-cacheBits
    --     mov memBits totBits
    --     sub memBits cacheBits

    --     -- if cacheBits== just load a byte
    --     cmp cacheBits 0
    --     if_ Z (do
    --               mov a8 (add8 ptr)
    --               inc ptr
    --               mov cacheBits memBits)

    --     cmp totBits 8
    --     mov word (addr8 ptr)

    --     -- how many bits to load? 8-cacheBits
    --     mov movedBits 8
    --     sub movedBits cacheBits


    --     -- merge with top
    --     mov a8 (addr8 ptr)
    --     shr a8 movedBits
    --     and_ cache a8

    --     -- have we crossed a byte boundary?
    --     -- yes, if movedBits > totBits least significant bits 3 then
    --     mov a8 totBits
    --     and a8 7
    --     sub a8 movedBits
    --     when_ N (inc ptr)

    --     -- adjust totBits
    --     sub totBits b8



  -- failed <- label

