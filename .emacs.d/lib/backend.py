#!/usr/bin/python3
# TLP Installers

import argparse
import sys
import os
import subprocess

def install_zonaimacs():
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

        try:
            print("\nGetting latest Emacs configuration...")
            subprocess.call(["git", "clone", "https://github.com/thelinuxpirate/dotfiles", "temp/dots"])
        except FileNotFoundError:
            os.system("rm -r temp")
            print("ERROR: Program \"git\" is not installed\n!ABORTING!\n")
        else:
            print("Merging contents")
            os.system("cp -r temp/dots/.emacs.d ~/")

            print("Removing Contents of the \"temp/\" directory")
            os.system("su -c rm -r temp")

            print("\n[D O N E]\n")
            print("Have a good time using Zonaimacs!")
    
def install_sys(): # TODO
    print("=+ThePirateShip Arch System Setup+=")
    print("\nDefine Type of Installation:\nPress <Enter> to continue")
    input()

    print("\n[D O N E]\n")

parser = argparse.ArgumentParser(description='Zonaimacs Python Backend;',
                                 epilog="Created by TheLinuxPirate")

subparsers = parser.add_subparsers()

parser_izonaimacs = subparsers.add_parser('iZonaimacs', help='Installs Zonaimacs (ROOT)')
parser_izonaimacs.set_defaults(func=install_zonaimacs)

parser_itps = subparsers.add_parser('iTPS', help='Turns System into \"ThePirateShip\" (Arch, ROOT)')
parser_itps.set_defaults(func=install_sys)

if len(sys.argv) <= 1:
    sys.argv.append('--help')
else:
    print("TOTAL BACKEND FAILURE")

options = parser.parse_args()
options.func()
