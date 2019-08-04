module SimplyTyped.Interactive where

import SimplyTyped.Back
import SimplyTyped.Blanks
import SimplyTyped.Convert
import SimplyTyped.Front
import SimplyTyped.Prelude
import SimplyTyped.Tree
import SimplyTyped.Type

readAndConvert :: Text -> IO ExpScope
readAndConvert t = do
    a <- easyReadTreeable (Proxy :: Proxy FrontFix) t
    pure (convert a)

doInfer :: ExpScope -> IO ExpScope
doInfer = foldScoped closedInfer
