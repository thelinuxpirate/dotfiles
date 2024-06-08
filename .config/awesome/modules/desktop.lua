statusbar_visibility = {}

function updateStatusBarVisibility()
local tag = awful.tag.selected()
if not tag then return end
mywibox[mouse.screen].visible = not statusbar_visibility[tag]
end

function toggleStatusBarVisibility()
local tag = awful.tag.selected()
if not tag then return end
statusbar_visibility[tag] = not statusbar_visibility[tag]
updateStatusBarVisibility()
end
tag.connect_signal("property::selected", updateStatusBarVisibility)
