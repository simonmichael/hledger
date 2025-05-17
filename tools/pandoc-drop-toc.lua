-- Remove a $TOC$ marker (cf pandoc-toc.lua) from the document.
function Para(p)
    if not  p.content[1]                  then return p end
    if not (p.content[1].t    == "Str")   then return p end
    if not (p.content[1].text == "$TOC$") then return p end
    return pandoc.Null()
end
