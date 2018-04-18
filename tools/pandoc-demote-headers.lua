function Header(h)
    return pandoc.Header(h.level + 1, h.content, h.attrs)
end
