local awful = require("awful")
local gears = require("gears")

function create_minimized_clients_menu()
	local minimized_clients = {}

	for _, c in ipairs(client.get()) do
		if c.minimized then
			table.insert(minimized_clients, c)
		end
	end

	local menu_items = {}
	for _, c in ipairs(minimized_clients) do
		table.insert(menu_items, {
			c.name,
			function()
				c.minimized = false
				client.focus = c
				c:raise()
			end
		})
	end

	local menu = awful.menu({ items = menu_items })
	menu:show({ keygrabber = true })
end
