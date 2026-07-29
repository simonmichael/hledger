@@ -100,7 +100,7 @@
 
 -- | Get all tags for a posting, both its own and inherited from its transaction.
 postingTags :: Posting -> [Tag]
-postingTags p = ptag p ++ ttags (ptransaction p)
+postingTags p = let pt = ptag p; tt = ttags (ptransaction p) in pt ++ filter (\t -> tagName t `notElem` map tagName pt) tt
 
 -- | Get all tags for a posting, as a map from tag name to list of values (including duplicates).
 -- This is used for pivoting and tag-based reports. For now we keep the legacy behavior of