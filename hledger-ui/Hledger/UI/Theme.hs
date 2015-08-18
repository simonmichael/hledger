----------------------------------------------------------------------
-- Theme       
-- the all-important theming engine!

-- theme = Restrained
-- -- theme = Colorful
-- -- theme = Blood

-- data UITheme = Restrained | Colorful | Blood

-- (defaultattr,
--  currentlineattr,
--  statusattr
--  ) = case theme of
--        Restrained -> (defAttr
--                     ,defAttr `withStyle` bold
--                     ,defAttr `withStyle` reverseVideo
--                     )
--        Colorful   -> (defAttr `withStyle` reverseVideo
--                     ,defAttr `withForeColor` white `withBackColor` red
--                     ,defAttr `withForeColor` black `withBackColor` green
--                     )
--        Blood      -> (defAttr `withStyle` reverseVideo
--                     ,defAttr `withForeColor` white `withBackColor` red
--                     ,defAttr `withStyle` reverseVideo
--                     )

-- -- halfbrightattr = defAttr `withStyle` dim
-- -- reverseattr = defAttr `withStyle` reverseVideo
-- -- redattr = defAttr `withForeColor` red
-- -- greenattr = defAttr `withForeColor` green
-- -- reverseredattr = defAttr `withStyle` reverseVideo `withForeColor` red
-- -- reversegreenattr= defAttr `withStyle` reverseVideo `withForeColor` green

