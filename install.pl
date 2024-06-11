#!/usr/bin/perl
use strict;
use warnings;
use File::Path qw(make_path);
use File::Copy qw(move);
use File::Basename;
use Git;

sub main {
    check_and_install_module('Git');
    clone_repository('https://github.com/thelinuxpirate/dotfiles', 'trongdots');

    print "\n=+Installer: Xorg or Wayland?+=\n> ";
    chomp(my $input = <STDIN>);

    while (1) {
        if ($input =~ /^xorg$/i) {
            ifxorg();
            last;
        } elsif ($input =~ /^wayland$/i) {
            ifwayland();
            last;
        } else {
            die "Invalid input. Please enter 'Xorg' or 'Wayland'.\n";
            last;
        }
    }
   
    sub ifxorg {
        print "XORG"
    }

    sub ifwayland {
        print "WAYLAND"
    }

    # create_directory('example_dir');
    # rename_file('example_dir/old_name.txt', 'example_dir/new_name.txt');
    # move_file('example_dir/new_name.txt', 'cloned_repo/');
}

sub clone_repository {
    my ($repo_url, $dir) = @_;
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
