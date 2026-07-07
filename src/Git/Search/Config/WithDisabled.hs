module Git.Search.Config.WithDisabled
  ( WithDisabled (..),
    disabledParser,
    toMaybe,
  )
where

import Git.Search.Prelude

data WithDisabled a
  = -- | The field.
    With a
  | -- | Disabled.
    Disabled
  deriving stock (Eq, Functor, Show)

instance (DecodeTOML a) => DecodeTOML (WithDisabled a) where
  tomlDecoder = parseText <|> With <$> tomlDecoder
    where
      parseText = do
        tomlDecoder @Text >>= \case
          "off" -> pure Disabled
          other -> fail $ "Expected 'off', received: " <> unpack other

disabledParser :: (Applicative f) => Text -> f a -> f (WithDisabled a)
disabledParser "off" _ = pure Disabled
disabledParser _ fx = With <$> fx

toMaybe :: Maybe (WithDisabled a) -> Maybe a
toMaybe (Just (With x)) = Just x
toMaybe _ = Nothing
