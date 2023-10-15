#!/usr/bin/env lua

local argparse = require("argparse")
local logging = require("logging")
local GLib = require("lgi").GLib
local Playerctl = require("lgi").Playerctl
local json = require("json")

local logger = logging.logger.new("main")

function write_output(text, player)
    logger:info("Writing output")

    local output = {
        text = text,
        class = "custom-" .. player.player_name,
        alt = player.player_name
    }

    io.write(json.encode(output) .. "\n")
    io.flush()
end

function on_play(player, status, manager)
    logger:info("Received new playback status")
    on_metadata(player, player.metadata, manager)
end

function on_metadata(player, metadata, manager)
    logger:info("Received new metadata")
    local track_info = ""

    if player.player_name == "spotify" and
        metadata["mpris:trackid"] and
        string.find(metadata["mpris:trackid"], ":ad:")
    then
        track_info = "AD PLAYING"
    elseif player.artist and player.title and player.artist ~= "" and player.title ~= "" then
        track_info = player.artist .. " - " .. player.title
    else
        track_info = player.title
    end

    if player.playback_status ~= "Playing" and track_info ~= "" then
        track_info = " " .. track_info
    end

    write_output(track_info, player)
end

function on_player_appeared(manager, player, selected_player)
    if player and (not selected_player or player.player_name == selected_player) then
        init_player(manager, player)
    else
        logger:debug("New player appeared, but it's not the selected player, skipping")
    end
end

function on_player_vanished(manager, player)
    logger:info("Player has vanished")
    io.write("\n")
    io.flush()
end

function init_player(manager, name)
    logger:debug("Initialize player: " .. name.player_name)
    local player = Playerctl.Player.new_from_name(name)
    player:connect("playback-status", on_play, manager)
    player:connect("metadata", on_metadata, manager)
    manager:manage_player(player)
    on_metadata(player, player.metadata, manager)
end

function signal_handler(signum)
    logger:debug("Received signal to stop, exiting")
    io.write("\n")
    io.flush()
    os.exit(0)
end

function parse_arguments()
    local parser = argparse("lua_script", "Lua version of the script")

    parser:option("-v --verbose", "Increase verbosity with every occurrence of -v"):count("0")
    parser:option("--player", "Define for which player we're listening")

    return parser:parse()
end

function main()
    local arguments = parse_arguments()

    -- Initialize logging
    logging.basicConfig {
        level = logging.DEBUG,
        format = "%(name)s %(levelname)s %(message)s"
    }

    -- Logging is set by default to WARN and higher.
    -- With every occurrence of -v it's lowered by one
    logger:setLevel(math.max((3 - arguments.verbose) * 10, 0))

    -- Log the sent command line arguments
    logger:debug("Arguments received " .. json.encode(arguments))

    local manager = Playerctl.PlayerManager.new()
    local loop = GLib.MainLoop()

    manager:connect("name-appeared", function(...)
        on_player_appeared(..., arguments.player)
    end)

    manager:connect("player-vanished", on_player_vanished)

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    for _, player_name in ipairs(manager.player_names) do
        local player = Playerctl.Player.new_from_name(player_name)

        if arguments.player and arguments.player ~= player_name then
            logger:debug(player_name .. " is not the filtered player, skipping it")
        else
            init_player(manager, player)
        end
    end

    loop:run()
end

if arg[0] == "lua_script" then
    main()
end
