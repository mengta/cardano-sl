{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Pos.Core.Tripping
       ( runTests
       , trippingBiBuildable
       , trippingBiShow
       ) where

import           Universum

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy as LazyText
import           Data.Text.Buildable (Buildable (..))
import           Data.Text.Internal.Builder (fromText, toLazyText)

import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Show (valueDiff)
import           Hedgehog.Internal.Property (Diff (..), failWith)
import qualified Hedgehog as H

import           Pos.Binary.Class (Bi (..), serialize, decodeFull)

import qualified Prelude

import           Text.Show.Pretty (Value(..), parseValue)

runTests :: [IO Bool] -> IO ()
runTests tests = do
    result <- and <$> sequence tests
    unless result
        exitFailure

-- | Round trip test a value (any instance of both the 'Bi' and 'Show' classes)
-- by serializing it to a ByteString and back again and
--   that also has a 'Show' instance.
-- If the 'a' type has both 'Show' and 'Buildable' instances, its best to
-- use this version.
trippingBiShow :: (Bi a, Eq a, MonadTest m, Show a) => a -> m ()
trippingBiShow x =
    H.tripping x serialize decodeFull

-- | Round trip (via ByteString) any instance of the 'Bi' class
-- that also has a 'Buildable' instance.
trippingBiBuildable :: (Bi a, Buildable a, Eq a, MonadTest m) => a -> m ()
trippingBiBuildable x =
  let mx = pure x
      i = serialize x
      my = decodeFull i
  in if mx == my
        then H.success
        else case valueDiff <$> buildValue mx <*> buildValue my of
            Nothing ->
                withFrozenCallStack $
                    failWith Nothing $ Prelude.unlines
                        [ "━━━ Original ━━━"
                        , buildPretty mx
                        , "━━━ Intermediate ━━━"
                        , BS.unpack i
                        , "━━━ Roundtrip ━━━"
                        , buildPretty my
                        ]

            Just diff ->
                withFrozenCallStack $
                    failWith
                        (Just $ Diff "━━━ " "- Original" "/" "+ Roundtrip" " ━━━" diff) $
                            Prelude.unlines
                            [ "━━━ Intermediate ━━━"
                            , BS.unpack i
                            ]


instance Buildable a => Buildable (Either Text a) where
    build (Left t) = fromText t
    build (Right a) = build a

buildPretty :: Buildable a => a -> String
buildPretty = show . buildValue

buildValue :: Buildable a => a -> Maybe Value
buildValue = parseValue . stringBuild

stringBuild :: Buildable a => a -> String
stringBuild =
    LazyText.unpack . toLazyText  . build