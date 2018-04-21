function keepBi(bi)
    if not (bi[1].t == "Plain")                             then return true end
    if not (bi[1].content[1].t == "Str")                    then return true end
    if not (string.find(bi[1].content[1].text, "toc") == 1) then return true end
    return false
end

function BulletList(bl)
    local newBl = { }
    for i,bi in pairs(bl.content) do
        if keepBi(bi)
            then table.insert(newBl, bi)
        end
    end
    return pandoc.BulletList(newBl)
end
