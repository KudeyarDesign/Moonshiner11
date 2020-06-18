-- Project Moonshiner-11 - PDP-11 compatible In-order superscalar core
--
-- External Bus hangup detector
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
use IEEE.STD_LOGIC_UNSIGNED.all;

entity hangup_detector is	 
  generic
  (
    WATCHDOG_TIME: natural:= 100
  ); 	
  port
  (
	  clk : in std_logic;
		reset : in std_logic;  	
		
		inp: in std_logic;
		hangup: out std_logic
	);	
end hangup_detector;

architecture behaviour of hangup_detector is
  signal watchdog_cnt: std_logic_vector(15 downto 0);
	signal xhangup: std_logic;
begin
	process(reset, clk)
	begin
		if reset = '1' then
			watchdog_cnt <= (others => '0');
			xhangup <= '0';
		elsif rising_edge(clk) then	
			if watchdog_cnt = WATCHDOG_TIME then
				xhangup <= '1';
			end if;	
			if inp = '1' then
			  watchdog_cnt <= watchdog_cnt + 1;
			else	
				watchdog_cnt <= (others => '0');
			end if;	
		end if;
	end process;	
	
	hangup <= xhangup;
end behaviour;
