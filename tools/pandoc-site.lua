function gatherHeaders(bs)
  headers = {}
  for _,b in pairs(bs) do
    if b.t == "Header" then table.insert(headers, b) end
  end
  return headers
end

function isTocBlock(blk)
  if not (blk.t               == "Para")  then return false end
  if not  blk.content[1]                  then return false end
  if not (blk.content[1].t    == "Str")   then return false end
  if not (blk.content[1].text == "$toc$") then return false end
  return true
end

function markupLink(hAttr, headerText)
  local headerId         = hAttr.identifier
  local headerProperties = hAttr.attributes
  return pandoc.Link(headerText, "#" .. headerId, headerId)
end

function markupElement(elem)
  local hAttr   = elem.attr
  local hText   = elem.label
  local hNested = elem.contents
  local link    = pandoc.Plain(markupLink(hAttr, hText))
  if not hNested[1] then return {link} end
  return {link, markupElements(hNested)}
end

function markupElements(elems)
  local newElems = {}
  for _,e in pairs(elems) do
    table.insert(newElems, markupElement(e))
  end
  return pandoc.OrderedList(newElems, {"1", "Decimal", "Period"})
end

function createTable(elems)
  local navBegin  = pandoc.RawBlock("html", "<nav id=\"toc\" class=\"right-toc\">")
  local navEnd    = pandoc.RawBlock("html", "</nav>")
  local contentsP = pandoc.Para(pandoc.Str("Contents"))
  return {navBegin, contentsP, markupElements(elems), navEnd}
end

function Pandoc(doc)
  newBlocks = {}
  tocBlocks = createTable(pandoc.utils.hierarchicalize(gatherHeaders(doc.blocks)))
  for _,blk in pairs(doc.blocks) do
    if isTocBlock(blk) then
      for _,tocBlk in pairs(tocBlocks) do
        table.insert(newBlocks, tocBlk)
      end
    else
      table.insert(newBlocks, blk)
    end
  end
  return pandoc.Doc(newBlocks, doc.meta)
end
