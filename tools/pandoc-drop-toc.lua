function Para(p)
    if not  p.content[1]                  then return p end
    if not (p.content[1].t    == "Str")   then return p end
    if not (p.content[1].text == "$toc$") then return p end
    return pandoc.Null()
end
