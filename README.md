# srem

## Introduction

srem is a program for GNU/Linux and Microsoft Windows systems to
quickly set and receive pop-up reminders at times you'd like them.

I've found that often when I get engrossed in something while sitting
at my computer, I lose track of the time and this has led to burnt
food, cold cups of tea, clean laundry sitting in the machine for hours
and being late to catch the bus to meet friends.  srem ("Sean's
reminders") is a program that allows you to effortlessly request
reminders to go do things in the real world, like this:

    $ srem 20m Check on food in oven
    $ srem 3:40pm Go out to get the bus in 5 minutes

After typing one of these commands you'll get a pop-up reminder at the
right time.  It's much more convenient to just type `srem 20m Take
laundry out` into a shell, than it is to find your phone, figure out
what time it will be in 20 minutes, set an alarm to go off at that
time, make sure your phone is not silenced, et cetera.  With srem you
can just quickly set the reminder and then allow yourself to get
engrossed in whatever you're doing on your computer.

### Other features

If you use [Org-mode for GNU Emacs](http://orgmode.org/) to manage
your scheduled appointments, srem can pull appointments from your Org
agenda and show you reminders for those at the time of appointment and
at (soon to be configurable) intervals before.

### Other usage ideas

- Implement the pomodoro technique: type `srem 25m time for a short
  break`
- Use your computer more mindfully by making a decision how long
  you're going to spend on a task: something like `srem 40m spend ten
  minutes away from the screen now`

## Usage

### Setting reminders

    srem 10m do X     # reminder 10 minutes from now
    srem 10 do X      # also 10 minutes from now
    srem 1h5m do Y    # reminder 65 minutes from now
    srem 4:30am do Z  # reminder at 4:30am tomorrow morning (probably)

You can't set a reminder more than 24 hours into the future; don't try
things like `srem 26h do something`.  This restriction keeps the usage
of srem simple and quick.  Use an appointment in your Emacs agenda instead.

### Receiving reminders

#### On GNU/Linux with cron

In my crontab I have

    PATH=/home/swhitton/.local/bin:[...]

    * * * * *   srem --cron
    0 */2 * * * srem --refresh-emacs

This has srem checking if there's a reminder it needs to pop-up every
minute, and refreshing its cache of my Org-mode agenda every two
hours (this requires spawning a new Emacs instance so is a bit heavy
to be running every minute).

#### On Microsoft Windows

This command should set the scheduled tasks:

    schtasks /Create /SC MINUTE /TN srem /TR "C:\path\to\srem.exe --cron"
    schtasks /Create /SC HOURLY /MO 2 /TN srem-refresh /TR "C:\path\to\srem.exe --refresh-emacs"

To get rid of the command prompt window that momentarily flashes up
every time the commands are run, open the scheduled tasks manager,
find the srem tasks and open their properties, and select "Run whether
user is logged on or not".

## Installation

I'm currently working to make srem configurable: right now it's set up
the way I like it, but I'd like to make it so that you can change its
settings without knowing how to compile Haskell programs.  I'll
provide binaries at that point.  Until then, you'll need to build srem
yourself; see below.

## Building

I recommend [stack](https://github.com/commercialhaskell/stack):

    $ git clone https://github.com/spwhitton/srem.git
    $ cd srem
    $ stack build
    $ stack install

If you've `$HOME/.local/bin` in your PATH environment variable (which
you should if you're using stack), you should now be able to just run
srem from any shell/command prompt.

## Enhancements

I'd like to hook srem up to a mobile notifications service.  If it
detects the computer user is idle, it might send your reminder to your
phone.

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see
[<http://www.gnu.org/licenses/>](http://www.gnu.org/licenses/).
