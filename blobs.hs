import Data.Foldable
import Prelude hiding (foldr)

data BlobTree a = Tree [BlobTree a] | Blob a
instance Foldable BlobTree where
  foldMap f (Blob a) = f a
  foldMap f (Tree xs) = foldMap f xs

numBlobs t = foldr countBlob 0 t
  where
  countBlob (Blob a) acc = acc + 1
  countBlob _ acc    = acc


{-numBlobs Blob = 1
numBlob (Tree xs) = sum (map numBlobs xs)-}