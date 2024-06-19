# TLP Dotfiles 2024

| Software      | Description |
| :---          |    :----:   |
| Distro     | [Gentoo](https://gentoo.org)  |
| WM     | [DWM (Xorg)](https://github.com/thelinuxpirate/sleepy-dwm)  |
| Secondary WM     | [Currently N/A](https://github.com/thelinuxpirate/)  |
| Editor           | [NvChad](https://github.com/thelinuxpirate/sleepy-nvim)    |
| Terminal         | [Currently N/A](https://github.com/thelinuxpirate/)                 |

## NixOS Struggle
I've struggled trying to use NixOS for about 7 months. 
Now I have the configuration down to contain my configurations at a reproducible level.

Would I recommend NixOS to other people? Probably not to be completely honest.
There is a lot of debugging of errors and the documentation for Nix sucks ass.
If you have no knowledge of programming you are guaranteed to struggle.

I still wonder if this is the distro for me; sometimes I just would like to go back to Gentoo.
Even if I went back to Gentoo I could still have some things be managed by Nix's Home-Manager.
My main issue is that Nix & NixOS have a heavy dependency with SystemD. So if you don't use SystemD
or plan to leave it, don't get too excited about managing your system with Nix...
