
#!/usr/bin/perl
use strict;
use warnings;
use Cwd;
use File::Path qw(make_path remove_tree);
use File::Copy qw(move);
use File::Basename;
use Git;

my $homedir = $ENV{'HOME'};

sub partone {
    check_and_install_module('Git');
    print "What would you like the clone directory to be called? (default: 'dotfiles')\n> ";
    chomp(my $clonedir = <STDIN>);
    $clonedir = "dotfiles" if $clonedir eq "";
    $clonedir = process_dir($clonedir);
    print "ALERT: Directory name: '$clonedir' will be used. Is this ok? (Yes/No)\n> ";
    chomp(my $input = <STDIN>);

    while (1) {
        if ($input =~ /^yes$/i) {
            print "Ok...\n";
            clone_repository('https://github.com/thelinuxpirate/dotfiles', $clonedir);
            parttwo($clonedir);
            last;
        } elsif ($input =~ /^no$/i) {
            print "Please enter a new name for the clone directory:\n> ";
            chomp($clonedir = <STDIN>);
            $clonedir = process_dir($clonedir);
            print "ALERT: Directory name: '$clonedir' will be used. Is this ok? (Yes/No)\n> ";
            chomp($input = <STDIN>);
        } else {
            print "Please enter a valid option (Yes/No), got '$input'\n> ";
            chomp($input = <STDIN>);
        }
    }
}

sub parttwo {
    my ($dir) = @_;
    print "\n=+Installer: Xorg, Wayland, or Misc?+=\n> ";
    chomp(my $input = <STDIN>);
    process_response($input, $dir);
}

sub process_dir {
    my ($str) = @_;
    $str = lc $str;
    $str =~ s/ /_/g;
    return $str;
}

sub process_response {
    my ($input, $dir) = @_;

    while (1) {
        if ($input =~ /^xorg$/i) {
            my $xorgapps = [
                "awesome",
                "fastfetch",
                "krabby",
                "starship",
                "wezterm",
                "dunst",
                "flameshot",
                "neofetch",
                "picom",
                "polybar",
                "xmonad"
            ];

            print "\nApplication List:\n";
            foreach my $app (@$xorgapps) {
                print "$app\n";
            }
            print "\n=+Please input the programs' configurations you'd like to have installed+=\n> ";
            chomp(my $appchoice = <STDIN>);
            process_app($xorgapps, $appchoice, $dir);
            last;
        } elsif ($input =~ /^wayland$/i) {
            my $waylandapps = [
                "hyprland",
                "fastfetch",
                "krabby",
                "starship",
                "wezterm",
                "dunst",
                "waybar",
                "neofetch"
            ];

            print "\nApplication List:\n";
            foreach my $app (@$waylandapps) {
                print "$app\n";
            }
            print "\n=+Please input the programs' configurations you'd like to have installed+=\n> ";
            chomp(my $appchoice = <STDIN>);
            process_app($waylandapps, $appchoice, $dir);
            last;
        } elsif ($input =~ /^misc$/i) {
            my $miscapps = [
                "home-manager",
                "fastfetch",
                "krabby",
                "starship",
                "wezterm",
                "dunst",
                "neofetch"
            ];

            print "\nConfiguration List:\n";
            foreach my $app (@$miscapps) {
                print "$app\n";
            }
            print "\n=+Please input the programs' configurations you'd like to have installed+=\n(NixOS has to be installed manually)\n> ";
            chomp(my $appchoice = <STDIN>);
            process_app($miscapps, $appchoice, $dir);
            last;
        } else {
            die "Invalid input. Please enter 'Xorg', 'Wayland', or 'Misc'.\n";
        }
    }
}

sub process_app {
    my ($applist, $usrapp, $dir) = @_;
    my $repodir = cwd() . "/" . $dir;
    print "\n$repodir\n";

    my @apps_to_install = split(/,\s*/, $usrapp);

    foreach my $app (@apps_to_install) {
        if (grep { $_ eq $app } @$applist) {
            if ($app =~ /^starship/i) {
                print "Starship \n";
            } else {
                print "Creating directory for $app...\n";
                move_file($repodir . "/.config/" . $app, $homedir . "/.config/" . $app);
            }
        } else {
            die "ALERT: Invalid input '$app'. Please enter a valid configuration title.\n";
        }
    }
    print "Hello, world!\n";
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
            print "ALERT: Installation may work incorrectly...\n";
            print "Would you like to continue installation anyways? (Yes/No)\n> ";
            chomp(my $response = <STDIN>);
            if ($response =~ /^yes$/i) {
                print "\nALERT: Using existing '$dir'!\n";
                parttwo($dir);
            } else {
                die "ALERT: Exiting, no changes were made;\n";
            }
        } else {
            die "ALERT: Couldn't read message...\n(Maybe try manually removing the existing clone directory?)";
        }
    } else {
        print "Creating '$dir'...\n";
    }

    unless (-e $dir or make_path($dir)) {
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
    unless (-e $dir or make_path($dir)) {
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

sub cleanup {
    # remove cloned repository directory logic
}

partone();

