module Draw.MessageSelect where

import Brick

import Types

import Draw.Main

drawMessageSelect :: ChatState -> [Widget Name]
drawMessageSelect st = drawMain st
