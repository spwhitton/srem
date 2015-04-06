# srem

## Introduction

TODO

## Usage

### Setting reminders

TODO

### Receiving reminders

#### On Microsoft Windows

This command should set the scheduled task:

    schtasks /Create /SC MINUTE /TN srem /TR "C:\path\to\srem.exe --cron"

To get rid of the command prompt window that momentarily flashes up
every time the command is run, open the scheduled tasks manager, find
the srem task and open its properties, and select "Run whether user is
logged on or not".

## Building

TODO

## Installation

TODO

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
