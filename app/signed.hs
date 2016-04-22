{-# LANGUAGE DeriveGeneric ,NoMonomorphismRestriction #-}
import Network.Quid2
import qualified Crypto.Sign.Ed25519 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Word

-- |Demonstrate the signing and verification of messages
main = do
  readerTask <- async reader
  threadDelay (seconds 3)
  john
  threadDelay (seconds 3)
  bob
  threadDelay (seconds 3)
  cancel readerTask

reader = runClient def (ByType::ByType (Signed String Ed255619)) loop
      where loop conn = do
              Signed value signature <- input conn
              let outputer = if verify johnPublicKey value signature then "John" else "Some random guy"
              putStrLn . unwords $ [outputer,"says:",value]
              loop conn

-- This is really John
john = runClient def ByType $ \conn -> let msg = "Hi from John!"
                                        in output conn (Signed msg (sign johnSecretKey msg))

-- This is the infamous prankster Bob, impersonating John
bob = runClient def ByType $ \conn -> let msg = "Hi from John!"
                                      in output conn (Signed msg (sign bobSecretKey msg))

-- |A signed value
data Signed value signature = Signed value signature deriving (Eq, Ord, Read, Show, Generic)

instance (Flat a,Flat b) => Flat (Signed a b)
instance (Model a,Model b) => Model (Signed a b)

-- |An Ed255619 signature
data Ed255619 = Ed255619 [Word8] deriving (Eq, Ord, Read, Show, Generic)
instance Flat Ed255619
instance Model Ed255619

sign :: Flat a => S.SecretKey -> a -> Ed255619
sign secretKey a = Ed255619 (B.unpack . S.unSignature . S.dsign secretKey . bflat $ a)

verify :: Flat a => S.PublicKey -> a -> Ed255619 -> Bool
verify publicKey a (Ed255619 sig) = S.dverify publicKey (bflat a) (S.Signature . B.pack $ sig)

bflat = L.toStrict . flat


-- To generate your own: printSignatures

johnPublicKey = publicKey [57,81,200,74,61,18,153,2,31,192,16,78,108,108,226,68,231,59,196,23,130,14,96,5,15,208,28,170,124,142,145,36]

johnSecretKey = secretKey [253,229,163,142,162,49,253,155,172,204,187,138,175,205,183,51,116,241,163,50,137,109,8,147,99,137,20,248,53,208,54,87,57,81,200,74,61,18,153,2,31,192,16,78,108,108,226,68,231,59,196,23,130,14,96,5,15,208,28,170,124,142,145,36]

bobSecretKey = secretKey [193,175,1,168,65,221,227,91,203,118,104,54,64,143,63,125,234,37,182,58,67,2,188,200,235,97,219,33,68,193,43,180,53,204,35,255,180,196,69,14,217,3,36,195,127,14,69,67,224,154,201,203,255,45,28,221,164,192,152,5,104,236,167,192]

publicKey = S.PublicKey . B.pack
secretKey = S.SecretKey . B.pack

printSignatures = do
   (S.PublicKey publicKey,S.SecretKey secretKey) <- S.createKeypair
   putStrLn $ "publicKey " ++ show (B.unpack publicKey)
   putStrLn $ "secretKey " ++ show (B.unpack secretKey)
