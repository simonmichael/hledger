function RawBlock(rb)
    if rb.format == "html"
        then return pandoc.Null()
        else return rb
    end
end
