-- Project Moonshiner-11 - PDP-11 compatible In-order superscalar core
--
-- Basic constants, types and functions
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
use IEEE.STD_LOGIC_ARITH.all;
use work.config.all;

package def is
	constant WORD_SIZE: natural := 16;
	constant PHYSICAL_ADDRESS_WIDTH: natural := 16;
	constant PC_LOW: natural := 1;
	
	constant SP_REG: std_logic_vector(3 downto 0) := "0110";	-- SP register number	
	constant PC_REG: std_logic_vector(3 downto 0) := "0111";	-- PC register number
	constant TEMP1: std_logic_vector(3 downto 0) := "1000";	-- temporary register 1 number
	constant TEMP2: std_logic_vector(3 downto 0) := "1001";	-- temporary register 2 number 	
	constant PSW_REG: std_logic_vector(3 downto 0) := "1010";	-- immediate virtual register number 
	constant PSW_REG2: std_logic_vector(3 downto 0) := "1011";	-- immediate virtual register number
	constant IMM_REG: std_logic_vector(3 downto 0) := "1101";	-- immediate virtual register number 
	constant MEM_REG: std_logic_vector(3 downto 0) := "1110";	-- memory load virtual register number 
	constant ZERO_REG: std_logic_vector(3 downto 0) := "1111";	-- ZERO virtual register number 

	constant ROM_START_ADDRESS: std_logic_vector(31 downto 0) := x"00000000";
	constant RAM_START_ADDRESS: std_logic_vector(31 downto 0) := x"10000000";
	constant IO_START_ADDRESS:  std_logic_vector(31 downto 0) := x"80000000";	 
	constant ROM0_OFFSET:				std_logic_vector(31 downto 0) := x"00008000";
	constant ROM1_OFFSET:				std_logic_vector(31 downto 0) := x"00008000";
	constant ROM2_OFFSET:				std_logic_vector(31 downto 0) := x"00008000";
	constant ROM3_OFFSET:				std_logic_vector(31 downto 0) := x"00008000";
	constant SMK_RAM_OFFSET:    std_logic_vector(31 downto 0) := x"00800000";
	constant DISK_ROM_OFFSET:   std_logic_vector(31 downto 0) := x"00010000";
	
  function log2(v: in natural) return natural;
  function decode(v : std_logic_vector) return std_logic_vector;	
	
	function instruction_len(insn: std_logic_vector(15 downto 0)) return std_logic_vector;
	function has_immediate(op: std_logic_vector(5 downto 0)) return std_logic;	 
	
	type address_map_in is record
		address: std_logic_vector(WORD_SIZE-1 downto 0);
		rd: std_logic;
		wr: std_logic;												 
		RAM_window0: std_logic_vector(2 downto 0);
		RAM_window1: std_logic_vector(2 downto 0);
		ROM_page: std_logic_vector(6 downto 0);		
		smk_page: std_logic_vector(7 downto 1);
	end record;
	
	type address_map_out is record
		address: std_logic_vector(31 downto 0);
		rom_cs: std_logic;
		ram_cs: std_logic;
		io_cs:  std_logic;
		irps_cs: std_logic; 
	end record;		
	
	function address_map(ain: address_map_in) return address_map_out;
end package def;		

package body def is		
  function log2(v: in natural) return natural is
  	variable n: natural;
  	variable logn: natural;
  begin
  	n := 1;
  	for i in 0 to 128 loop
  		logn := i;
  		exit when (n>=v);
  		n := n * 2;
  	end loop;
  	return logn;
  end function log2;  
  
  function decode(v : std_logic_vector) return std_logic_vector is
    variable res : std_logic_vector((2**v'length)-1 downto 0); --'
    variable i : natural;
  begin
    res := (others => '0');
    i := conv_integer(unsigned(v));
    res(i) := '1';
    return(res);
  end;	
	
	function has_immediate(op: std_logic_vector(5 downto 0)) return std_logic is
	  alias mode: std_logic_vector(2 downto 0) is op(5 downto 3);
		alias reg:  std_logic_vector(2 downto 0) is op(2 downto 0);	
	begin														
		if (mode(2 downto 1) = "01" and reg = "111") or
			   mode(2 downto 1) = "11" then
			 return '1';
		else
			return '0';
		end if;	
	end function has_immediate;	
	
	function instruction_len(insn: std_logic_vector(15 downto 0)) return std_logic_vector	is
	  variable imm1: std_logic;
		variable imm2: std_logic;
	begin
		imm1 := '0';
		imm2 := '0';	
		case insn(14 downto 12) is
			when "000" =>
			  case insn(11 downto 9) is			 
				  when "000" =>		
					  case insn(8 downto 6) is
							when "011" =>					 
							  if insn(15) = '0' then
							    -- SWAB
								  imm1 := has_immediate(insn(5 downto 0));
								end if;	
							when "001" =>	 
							  if insn(15) = '0' then
							    -- JMP
								  imm1 := has_immediate(insn(5 downto 0));
								end if;	
							when others => null;
						end case;
					when "100" =>
					  if insn(15) = '0' then
					    -- JSR
						  imm1 := has_immediate(insn(5 downto 0));
						end if;	
					when "101" | "110" =>
					  -- One-operand instructions
						imm1 := has_immediate(insn(5 downto 0));
					when others => null;	
				end case;
			when "111" =>
			  if insn(15) = '0' then
					-- One-and-a-half-operand instructions 
					imm1 := has_immediate(insn(5 downto 0));										
				end if;
			when others =>
			  -- Two-operand instructions
				imm1 := has_immediate(insn(5 downto 0));
				imm2 := has_immediate(insn(11 downto 6));
		end case;
		if imm1 = '1' then
			if imm2 = '1' then
				return "11";
			else	
				return "10";
			end if;	
		elsif imm2 = '1' then
			return "10";
		else	
			return "01";
		end if;	
	end function instruction_len;	
	
  function address_map(ain: address_map_in) return address_map_out is
	  variable v: address_map_out;
		variable smk_mode: std_logic_vector(2 downto 0); 
		
	  procedure smk_rom(hdd: boolean) is	 
		  variable a: std_logic_vector(31 downto 0);
	  begin			
			if hdd then
				if ain.address(11 downto 4) = "11111110" then  -- 177740 - 177757
					v.io_cs := '1';
					v.address := IO_START_ADDRESS or (x"0000" & ain.address);
					return;
				end if;	
			end if;
			v.rom_cs := '1';													
			a := ROM_START_ADDRESS + DISK_ROM_OFFSET;
			v.address(31 downto 12) := a(31 downto 12);
	  end procedure smk_rom;
		
	  procedure smk_seg(page: natural range 0 to 7) is
			variable a: std_logic_vector(31 downto 0);
	  begin																				
			v.ram_cs := '1';
			a := RAM_START_ADDRESS + SMK_RAM_OFFSET;
			v.address(31 downto 19) := a(31 downto 19);
			v.address(18) := ain.smk_page(7);
			v.address(17) := ain.smk_page(6);
			v.address(16) := ain.smk_page(4);
			v.address(15) := ain.smk_page(3);
			v.address(14 downto 12) := CONV_STD_LOGIC_VECTOR(page, 3);
	  end procedure smk_seg;
		
	  procedure smk_seg_high(page: natural range 0 to 7) is
			variable a: std_logic_vector(31 downto 0);
	  begin	
			if ain.address(11 downto 9) = "111" then  -- 177000 - 177777
				v.io_cs := '1';
				v.address := IO_START_ADDRESS or (x"0000" & ain.address);
			else
				smk_seg(page);
			end if;
		end procedure smk_seg_high;	
		
		function RAM_Window_convert(win: std_logic_vector(2 downto 0)) return std_logic_vector is
			variable v: std_logic_vector(2 downto 0);
		begin
			case win is
				when "110" =>	  v := "000";	  -- page 0 (constantly mapped to address 0 ..40000)
				when "000" =>   v := "001";		-- page 1
				when "010" =>   v := "010";		-- page 2
				when "011" =>		v := "011";		-- page 3
				when "100" =>		v := "100";		-- page 4
				when "001" =>   v := "101";		-- page 5 / screen buffer 0
				when "111" =>		v := "110";		-- page 6 / screen buffer 1
				when "101" =>	  v := "111";		-- page 7 / system page
				when others => null;
			end case;	
			return v;
		end function RAM_Window_convert;	
		
		-- default BK-0011 address map for addresses 0 - 77777
		procedure default_low is	
		begin
			v.ram_cs := '1'; 
			if ain.address(14) = '0' then
				v.address := RAM_START_ADDRESS or ("000000000000000" & "000" & ain.address(13 downto 0));
			else	
				v.address := RAM_START_ADDRESS or ("000000000000000" & RAM_Window_convert(ain.RAM_Window0) & ain.address(13 downto 0));
			end if;			
		end procedure default_low;
		
		-- default BK-0011 address map for addresses 100000 - 157777
		procedure default_high is
		begin
      if ain.address(14) = '0' then
				if ain.ROM_page /= 0 then	 
					v.rom_cs := '1';
					if ain.ROM_page(0) = '1' then
						-- ROM Basic A
						v.address := (ROM_START_ADDRESS + ROM0_OFFSET) or ("000000000000000" & "000" &  ain.address(13 downto 0));
					elsif ain.ROM_page(1) = '1' then
            v.address := (ROM_START_ADDRESS + ROM1_OFFSET) or ("000000000000000" & "000" &  ain.address(13 downto 0));						
						if ain.address(13) = '0' then
							null;  -- ROM Basic B
						else
							-- ROM Ext
							v.rom_cs := '1';
						end if;
					elsif ain.ROM_page(3) = '1' then
						-- ROM 3
						v.address := (ROM_START_ADDRESS + ROM2_OFFSET) or ("000000000000000" & "000" &  ain.address(13 downto 0));
					elsif ain.ROM_page(4) = '1' then	
						-- ROM 4
						v.address := (ROM_START_ADDRESS + ROM3_OFFSET) or ("000000000000000" & "000" &  ain.address(13 downto 0));
					end if;	
				else	 
					v.ram_cs := '1';
					v.address := RAM_START_ADDRESS or ("000000000000000" & RAM_Window_convert(ain.RAM_Window1) & ain.address(13 downto 0));					
				end if;	
			else	
				v.rom_cs := '1';
				v.address := ROM_START_ADDRESS or (x"0000" & ain.address);
			end if;
		end procedure default_high;	
	begin			 
		v.rom_cs := '0';
		v.ram_cs := '0';
		v.io_cs := '0';
		v.irps_cs := '0';
		
		if ain.address(15) = '0' then
			default_low;
		else									
			v.address(11 downto 0) := ain.address(11 downto 0);
		  smk_mode := ain.smk_page(1) & ain.smk_page(5) & ain.smk_page(2);	
			case smk_mode is
				when "000" =>   -- 160  SYS
				  case ain.address(14 downto 12) is
						when "111" =>   -- 170000 - 177777 
--							smk_rom(true);
              smk_seg_high(7);
						when "110" =>   -- 160000 - 167777
						  smk_rom(false);
						when "101" =>   -- 150000 - 157777 
						  smk_seg(1);
						when "100" =>   -- 140000 - 147777
						  smk_seg(0);
						when "011" =>   -- 130000 - 137777
						  smk_seg(7);
						when "010" =>   -- 120000 - 127777
						  smk_seg(6);
						when "001" =>   -- 110000 - 117777
						  default_high;
						when "000" =>   -- 100000 - 107777
						  default_high;
						when others => null;
					end case;
				when "100" =>   -- 60	 Std10
				  case ain.address(14 downto 12) is
						when "111" =>   -- 170000 - 177777	
							smk_seg_high(7);
						when "110" =>   -- 160000 - 167777
						  smk_rom(false);
						when "101" =>   -- 150000 - 157777
						  smk_seg(5);
						when "100" =>   -- 140000 - 147777
						  smk_seg(4);
						when "011" =>   -- 130000 - 137777
						  smk_seg(3);
						when "010" =>   -- 120000 - 127777
						  smk_seg(2);
						when "001" =>   -- 110000 - 117777
						  default_high;
						when "000" =>   -- 100000 - 107777
						  default_high;
						when others => null;
					end case;				
				when "010" =>   -- 120	RAM10
				  case ain.address(14 downto 12) is
						when "111" =>   -- 170000 - 177777
						  smk_seg_high(7);
						when "110" =>   -- 160000 - 167777
						  smk_seg(6);
						when "101" =>   -- 150000 - 157777
						  smk_seg(5);
						when "100" =>   -- 140000 - 147777
						  smk_seg(4);
						when "011" =>   -- 130000 - 137777
						  smk_seg(3);
						when "010" =>   -- 120000 - 127777
						  smk_seg(2);
						when "001" =>   -- 110000 - 117777
						  smk_seg(1);
						when "000" =>   -- 100000 - 107777
						  smk_seg(0);
						when others => null;
					end case;				
				when "110" =>   -- 20   All
				  case ain.address(14 downto 12) is
						when "111" =>   -- 170000 - 177777
						  smk_seg_high(3);
						when "110" =>   -- 160000 - 167777
						  smk_seg(2);
						when "101" =>   -- 150000 - 157777
						  smk_seg(1);
						when "100" =>   -- 140000 - 147777
						  smk_seg(0);
						when "011" =>   -- 130000 - 137777
						  smk_seg(7);
						when "010" =>   -- 120000 - 127777
						  smk_seg(6);
						when "001" =>   -- 110000 - 117777
						  smk_seg(5);
						when "000" =>   -- 100000 - 107777
						  smk_seg(4);
						when others => null;
					end case;				
				when "001" =>   -- 140   Std11
				  case ain.address(14 downto 12) is
						when "111" =>   -- 170000 - 177777
						  smk_seg_high(7); 
						when "110" =>   -- 160000 - 167777
						  smk_rom(false);
						when "101" =>   -- 150000 - 157777
						  default_high;
						when "100" =>   -- 140000 - 147777
						  default_high;
						when "011" =>   -- 130000 - 137777
						  default_high;
						when "010" =>   -- 120000 - 127777
						  default_high;
						when "001" =>   -- 110000 - 117777
						  default_high;
						when "000" =>   -- 100000 - 107777
						  default_high;
						when others => null;
					end case;				
				when "101" =>   -- 40  RAM11
				  case ain.address(14 downto 12) is
						when "111" =>   -- 170000 - 177777	
						  smk_seg_high(7);
						when "110" =>   -- 160000 - 167777
						  smk_seg(6);
						when "101" =>   -- 150000 - 157777
						  smk_seg(5);
						when "100" =>   -- 140000 - 147777
						  smk_seg(4);
						when "011" =>   -- 130000 - 137777
						  default_high;
						when "010" =>   -- 120000 - 127777
						  default_high;
						when "001" =>   -- 110000 - 117777
						  default_high;
						when "000" =>   -- 100000 - 107777
						  default_high;
						when others => null;
					end case;				
				when "011" =>   -- 100   Hlt10
				  case ain.address(14 downto 12) is
						when "111" =>   -- 170000 - 177777	
						  smk_seg_high(7);
						when "110" =>   -- 160000 - 167777
						  smk_seg(6);
						when "101" =>   -- 150000 - 157777
						  smk_seg(5);
						when "100" =>   -- 140000 - 147777
						  smk_seg(4);
						when "011" =>   -- 130000 - 137777
						  smk_seg(3);
						when "010" =>   -- 120000 - 127777
						  smk_seg(2);
						when "001" =>   -- 110000 - 117777
						  smk_seg(1);
						when "000" =>   -- 100000 - 107777
						  smk_seg(0);
						when others => null;
					end case;
				when "111" =>   -- 000   Hlt11
				  case ain.address(14 downto 12) is
						when "111" =>   -- 170000 - 177777
						  smk_seg_high(7);
						when "110" =>   -- 160000 - 167777
						  smk_seg(6);
						when "101" =>   -- 150000 - 157777
						  smk_seg(5);
						when "100" =>   -- 140000 - 147777
						  smk_seg(4);
						when "011" =>   -- 130000 - 137777
						  default_high;
						when "010" =>   -- 120000 - 127777
						  default_high;
						when "001" =>   -- 110000 - 117777
						  default_high;
						when "000" =>   -- 100000 - 107777
						  default_high;
						when others => null;
					end case;					
				when others => 
				  null;
			end case;	
		end if;
		return v;
	end function address_map;	
end package body def;

