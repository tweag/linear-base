import qualified OffHeap.List as List

main :: IO ()
main = do
  let _ = List.Nil -- Just forcing compilation of the module
  return ()
