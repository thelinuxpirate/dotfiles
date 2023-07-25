#!/usr/bin/python3
# Zonaimacs Backend
# https://github.com/thelinuxpirate/dotfiles/.emacs.d/lib/backend.py

import argparse
import sys
import os
from os import environ
import subprocess
import pprint

def install_guix():
    print("Installing GNU Guix...\nPress <Enter> to continue")
    input()
    
    try:
        print("Fetching install script\n")
        os.system("mkdir temp")
        subprocess.call(["wget", "https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh"])
    except FileNotFoundError:
        os.system("rm -r temp")
        print("ERROR: Program \"wget\" is not installed\n!ABORTING!\n")
    else:
        print("\n(Created \"temp/\" directory...)\n")
        os.system("mv guix-install.sh temp && chmod +x temp/guix-install.sh")
        print("[ D O N E]\nPlease follow the installer's instructions!")
        os.system("su -c ./temp/guix-install.sh")
        print("Removing Contents of the \"temp/\" directory")
        os.system("su -c rm -r temp")

        response = input("Do you want to update GNU Guix? [y/n]\n")

        i = 1
        while i <= 2:
            if response == 'y':
                shell = os.environ['SHELL']
                print("Restarting shell so you can use guix")
                os.system(shell)

                print("Updating guix...\n(guix pull && hash guix)")
                os.system("guix pull && hash guix")
                print("Guix has been installed and updated.")
                
                print("Cleaning up...\nRemoving Contents of the \"temp/\" directory")
                os.system("sudo rm -r temp")
                print("[ D O N E ]")
                i += 1
            elif response == 'n':
                print("Cleaning up...\nRemoving Contents of the \"temp/\" directory")
                os.system("sudo rm -r temp")
                print("All done, remember to \"guix pull && hash guix\"")
                i += 1
            else:
                print("ERROR: Unable to read line; did you input \'y\' or \'n\'?")
                response = input("\nDo you want to update GNU Guix? [y/n]\n")

def install_zonaimacs():
    home = os.environ['HOME']
    print("This installer will:\nMerge Zonaimacs to \"" + home + "/.emacs.d\"")
    response = input("\nIs GNU Guix installed? [y/n]\n")

    i = 1
    while i < 2:
        if response == 'y':
            try:
                print("\nGetting latest Emacs configuration...")
                subprocess.call(["git", "clone", "https://github.com/thelinuxpirate/dotfiles", "temp"])
            except FileNotFoundError:
                os.system("sudo rm -r temp")
                print("ERROR: Program \"git\" is not installed\n!ABORTING!\n")
                i += 1
            else:
                print("Merging contents")
                os.system("cp -r temp/dots/.emacs.d ~/")
                
                print("Removing Contents of the \"temp/\" directory")
                os.system("sudo rm -r temp")
                
                print("\n[D O N E]\n")
                print("Have a good time using Zonaimacs!")
                i += 1
        elif response == 'n':
            install_guix()
            i += 1
        else:
            print("ERROR: Unable to read line; did you input \'y\' or \'n\'?")
            response = input("Is GNU Guix installed? [y/n]\n")
        
def install_sys(): # TODO Create Profiles and make pacman do its thing (dont forget to setup AUR)
    time_warn = "This might take awhile, feel free to leave this running."
    i = 1
    
    ioptions = [ "base", "desktop-minimal", "desktop-full" ]
    iop1 = ioptions[0]
    iop2 = ioptions[1]
    iop3 = ioptions[2]

    print("=+ThePirateShip Arch System Setup+=")
    print("\nDefine the Profile of the Installation:\nYour options are: \"" + iop1 + "\" " + iop2 + "\" \"" + iop3 + "\"")
    print("Press <Enter> to continue")
    input()
    response = input("Select only one option: (1, 2, 3 or \"desc\")\n")

    while i < 2:
        if response == "1":
            print("base")
            i += 1
        elif response == "2":
            print("desktop-minimal")
            i += 1
        elif response == "3":
            print("desktop-full")
            i += 1
        elif response == "desc":
            print("Help Function:\n")
            print("Base: A base installation")
            basepkgs = "{  }"
            print("Includes: " + basepkgs + "\nNo Display Manager; No Desktop/WM;")
            print("Press <Enter> to continue")
            input()

            dminimalpkgs = "\n{\n pkg,\n}\n \n| AUR |\n \n{\n aur,\n}"
            print("Desktop-Minimal: Identical to Base but includes Hyprland and some extra packages\n")
            print("Includes: " + dminimalpkgs)
            print("Press <Enter> to continue")
            input()

            desktoppkgs = "\n{\n alacritty, alsa-utils, cmake, \n discord, dolphin-emu, \n dunst, ffmpeg, file-roller, \n firefox, fuse, gcc, \n gimp, go, gzip, \n htop, hunspell-en_us, \n hyprland, intel-ucode, \n lua, lxappearance, ly, \n mesa, meson, neofetch, \n networkmanager, ninja, \n opendoas, otf-font-awesome, \n pamixer, pavucontrol, \n pipewire, playerctl, \n pulseaudio, pulseaudio-alsa, \n python, rustup, snes9x, \n tar, thunar, vlc, waybar, \n wireplumber, wlroots, \n wofi, xdg-desktop-portal-wlr, \n zig, zls, zsh, zsh-syntax-highlighting,\n}\n \n| AUR |\n \n{\n brave-bin, eww-wayland, grimshot, mpvpaper, \n pfetch, pokemon-colorscripts-git, spicetify-cli, \n spotify, swww,\n}"

            print("Desktop: Full Desktop Setup includes misc packages & Configurations\n")
            print("Includes: " + desktoppkgs)
            print("Press <Enter> to continue")
            input()
            i += 1
        else:
            print("kys")
            response = input("Select only one option: (1, 2, 3 or \"desc\")\n")
    
    print("\n[D O N E]\n")

# EXTRA
def eww():
    print("Hello World!")
    input()

def env():
    env_var = os.environ
    print("Your Environment variables:")
    pprint.pprint(dict(env_var), width = 1)

def demo():
    print("Hello, World!")


parser = argparse.ArgumentParser(description='Zonaimacs Python Backend;',
                                 epilog="Created by TheLinuxPirate")

subparsers = parser.add_subparsers()

parser_izonaimacs = subparsers.add_parser('iZonaimacs', help='Installs Zonaimacs (ROOT)')
parser_izonaimacs.set_defaults(func=install_zonaimacs)

parser_iguix = subparsers.add_parser('iGuix', help='Installs GNU Guix (ROOT)')
parser_iguix.set_defaults(func=install_guix)

parser_itps = subparsers.add_parser('iTPS', help='Turns Current System into \"ThePirateShip\" (Arch, ROOT)')
parser_itps.set_defaults(func=install_sys)

parser_getenv = subparsers.add_parser('getEnv', help='Prints Out User\'s Environment Variables')
parser_getenv.set_defaults(func=env)

parser_getenv = subparsers.add_parser('ewwSample', help='Eww bar click sample')
parser_getenv.set_defaults(func=env)

parser_demo = subparsers.add_parser('t', help='demo')
parser_demo.set_defaults(func=demo)

if len(sys.argv) <= 1:
    sys.argv.append('--help')

options = parser.parse_args()
options.func()
