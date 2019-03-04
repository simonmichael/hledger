-- github wiki links -> markdown links
function Para(p)
    return pandoc.Str(p.t)
end
