-- convert code blocks to unindented plain text
function CodeBlock(b)
    return pandoc.Para{ pandoc.Str(b.text) }
end
