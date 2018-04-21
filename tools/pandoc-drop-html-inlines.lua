function RawInline(ri)
    if ri.format == "html"
        then return pandoc.Str("")
        else return ri
    end
end
