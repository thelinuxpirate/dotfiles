#!/usr/bin/perl
use strict;
use warnings;
use Cwd;
use File::Path qw(make_path remove_tree);
use File::Copy qw(move);
use File::Basename;
use Git;

my $homedir = $ENV{'HOME'};
my $repodir = cwd() . "/trongdots/";

sub main {
    check_and_install_module('Git');
    print "What would you like the clone directory to be called?\n";
    # input shi here
    clone_repository('https://github.com/thelinuxpirate/dotfiles', 'trongdots');

    print "\n=+Installer: Xorg, Wayland, or NixOnly?+=\n> ";
    chomp(my $input = <STDIN>);
    process_response($input);
}

sub process_response {
    my ($input) = @_;

    while (1) {
        if ($input =~ /^xorg$/i) {
            my $xorgapps = [
               "Awesome", 
               "Fastfetch", 
               "Krabby", 
               "Starship", 
               "Wezterm", 
               "Dunst", 
               "Flameshot", 
               "Neofetch", 
               "Picom", 
               "Polybar",
               "XMonad"
            ];
            
            print "\nApplication List:\n";
            foreach my $app (@$xorgapps) {
                print "$app\n";
            }
            print "\n=+Please input the programs' configurations youd like to have installed+=\n> "; 
            
            chomp(my $appchoice = <STDIN>);
            process_app($xorgapps, $appchoice);
            last;
        } elsif ($input =~ /^wayland$/i) {
            my $waylandapps = [
               "Hyprland", 
               "Fastfetch", 
               "Krabby", 
               "Starship", 
               "Wezterm", 
               "Dunst", 
               "Waybar", 
               "Neofetch" 
            ];

            print "\nApplication List:\n";
            foreach my $app (@$waylandapps) {
                print "$app\n";
            }
            print "\n=+Please input the programs' configurations youd like to have installed+=\n> "; 
 
            chomp(my $appchoice = <STDIN>);
            process_app($waylandapps, $appchoice);
            last;
        } elsif ($input =~ /^nixonly$/i) {
              #ifnixonly();
              last;
        } else {
            die "Invalid input. Please enter 'Xorg' or 'Wayland'.\n";
        }
    }
    # create_directory('example_dir');
    # rename_file('example_dir/old_name.txt', 'example_dir/new_name.txt');
    # move_file('example_dir/new_name.txt', 'cloned_repo/');
}

sub process_app {
    my ($applist, $usrapp) = @_;
    
    foreach my $app (@$applist) {
        if ($usrapp eq $app) {
            print "Awesome\n";
            return;
        }
    }
    die "Invalid input. Please enter one of the Xorg applications.\n";
}

sub clone_repository {
    my ($repo_url, $dir) = @_;
    
    if (-d $dir) {
        print "It seems like '$dir' already exists...\nDelete this directory? (Yes/No)\n> ";
        chomp(my $input = <STDIN>);
        if ($input =~ /^yes$/i) {
            print "\n=+Removing Directory...+=\n\n";
            remove_directory($dir); 
        } elsif ($input =~ /^no$/i) {
            die "ALERT: Exiting, no changes were made;";
        } else {
            die "ALERT: Couldnt read message...\n(Maybe try manually removing the existing clone directory?)"
        }
    } else {
        die "Please remove the directory manually or choose a different directory. Exiting...\n";
    }  

    unless(-e $dir or make_path($dir)) {
        die "\n=+Failed to create directory or '$dir' already exists+= $!\n";
    }  

    eval {
        Git::command_oneline('clone', $repo_url, $dir);
        print "\n=+Repository cloned successfully into '$dir'+=\n";
    };

    if ($@) {
        die "ALERT: Failed to clone repository: $@\n";
    }
}

sub check_and_install_module {
    my ($module) = @_;
    eval {
        (my $file = "$module.pm") =~ s|::|/|g;
        require $file;
    };
    if ($@) {
        print "Required module '$module' not found. Installing...\n";
        install_module($module);
        print "Module '$module' installed successfully. Rerunning the script...\n";
        exec($^X, $0, @ARGV);
    }
}

sub install_module {
    my ($module) = @_;
    my $cpan_install = "cpan $module";
    system($cpan_install) == 0 or die "Failed to install $module: $!\n";
}

sub create_directory {
    my ($dir) = @_;
    unless(-e $dir or make_path($dir)) {
        die "Failed to create directory '$dir': $!\n";
    }
    print "Directory '$dir' created or already exists.\n";
}

sub remove_directory {
    my ($dir) = @_;
    if (-d $dir) {
        eval {
            remove_tree($dir);
            print "Directory '$dir' removed successfully.\n";
        };
        if ($@) {
            die "Failed to remove directory '$dir': $@\n";
        }
    } else {
        print "Directory '$dir' does not exist.\n";
    }
}

sub rename_file {
    my ($old_name, $new_name) = @_;
    rename $old_name, $new_name or die "Failed to rename '$old_name' to '$new_name': $!\n";
    print "File '$old_name' renamed to '$new_name'.\n";
}

sub move_file {
    my ($file, $target_dir) = @_;
    my $file_name = basename($file);
    my $target_path = "$target_dir/$file_name";
    move($file, $target_path) or die "Failed to move '$file' to '$target_dir': $!\n";
    print "File '$file' moved to '$target_dir'.\n";
}

main();
