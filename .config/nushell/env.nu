use std/util "path add"

# New Variables
$env.ARCHFLAGS = "-arch x86_64"
$env.LANG = "en_US.UTF-8"

$env.DEVKITPRO = "/opt/devkitpro"
$env.DEVKITARM = $env.DEVKITPRO | path join "/devkitARM"
$env.DEVKITPPC = $env.DEVKITPRO | path join "/devkitPPC"
$env.DENO_INSTALL = $env.HOME | path join "/.deno"

# PATH Variables
#path add ($env.HOME | path join "")
path add ($env.HOME | path join "/.local/bin")
path add ($env.HOME | path join "/.config/emacs/bin")
path add ($env.HOME | path join "/.nimble/bin")
path add ($env.HOME | path join "/.cargo/bin")
path add ($env.HOME | path join "/.platformio/penv/bin")
path add ($env.DENO_INSTALL | path join "/bin")

# Programing PATH Checkers
let nixpath = $env.HOME | path join "/.nix-profile/etc/profile.d/nix.sh"
let bunpath = $env.HOME | path join "/.bun/_bun"
let haskellpath = $env.HOME | path join "/.ghcup/env"

if ($nixpath | path exists) { nu -c $"source ($nixpath)" }
if ($bunpath | path exists) { nu -c $"source ($bunpath)" }
if ($haskellpath | path exists) { nu -c $"source ($haskellpath)" }
