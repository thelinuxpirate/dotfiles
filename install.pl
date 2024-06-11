#!/usr/bin/perl
use strict;
use warnings;

eval {
    require Git;
    Git->import();
};

if ($@) {
    print "Required module 'Git' not found. Installing...\n";
    install_module('Git');
    print "Module 'Git' installed successfully. Rerunning the script...\n";
    exec($^X, $0, @ARGV);
}

my $repository_url = 'https://github.com/thelinuxpirate/dotfiles';
my $directory = 'Trong-Dots';

eval {
    Git::command_oneline('clone', $repository_url, $directory);
    print "Repository cloned successfully into '$directory'\n";
};

if ($@) {
    die "Failed to clone repository: $@\n";
}

sub install_module {
    my ($module) = @_;
    my $cpan_install = "cpan $module";
    system($cpan_install) == 0 or die "Failed to install $module: $!\n";
}
