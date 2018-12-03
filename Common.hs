module Common where

import Text.Parsec
import Text.Parsec.String


int :: Parser Int
int = read <$> many1 digit
