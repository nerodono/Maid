module Maid.Parser.Helpers
( skipNewlines )
where
import Maid.Tokenizer.Span ( Spanned(..) )
import Maid.Tokenizer.Token ( Token(..) )

skipNewlines :: [Spanned Token] -> [Spanned Token]
skipNewlines =
    dropWhile isNewline
    where isNewline (Spanned _ TNewline) = True
          isNewline _ = False
