-- Project Moonshiner-11 - PDP-11 compatible In-order superscalar core
--
-- Processor Configuration constants
--

--
-- Copyright (C) 2015
-- Alexey Shistko     alexey@kudeyar.com
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

library IEEE;
use IEEE.STD_LOGIC_1164.all;

package config is
	constant DEVICE_FAMILY: string := "Cyclone III";
	constant DCACHE_DATA_WIDTH: natural:= 16;	
	constant DCACHE_ASSOCIATIVITY: natural:= 2;
	constant DCACHE_SIZE: natural := 8192;
	constant DCACHE_LINE_SIZE: natural := 8;
	constant ICACHE_DATA_WIDTH: natural:= 32;
	constant ICACHE_ASSOCIATIVITY: natural:= 2;
	constant ICACHE_SIZE: natural := 8192;
	constant ICACHE_LINE_SIZE: natural := 8;	
end package config;

