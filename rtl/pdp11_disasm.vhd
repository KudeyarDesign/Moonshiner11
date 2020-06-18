-- Project Moonshiner-11 - PDP-11 compatible In-order superscalar core
--
-- Instruction Disassembler
-- Useful for simulation
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
use std.textio.all;
use work.def.all;

package pdp11_disasm is
	function disassemble(insn: std_logic_vector(15 downto 0); immed1: std_logic_vector(15 downto 0);
	    immed2: std_logic_vector(15 downto 0); insn_rdy: std_logic; immed1_rdy: std_logic;
	    immed2_rdy: std_logic) 
	  return string;
		
	function disassembleOperand(op: std_logic_vector(5 downto 0); immed: std_logic_vector(15 downto 0);
	    immed_rdy: std_logic)
		return string;

	function getRegisterName(reg: std_logic_vector(2 downto 0)) return string;	
	
  function toDecString(num: natural) return string;
	function toOctString(num: natural) return string;	
	function toHexString(num: natural) return string;
	function toHexString(num: std_logic_vector(15 downto 0)) return string;
	
	procedure strcpy(variable dest: inout string; src: in string);		
	
	constant UNDEF_INSTR: string := "???";
	constant UNDEF_OPERAND: string := "???";
end pdp11_disasm;
    
package body pdp11_disasm is
  type char_array is array (0 to 15) of character;
  constant hex_array : char_array := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');	

  procedure strcpy(variable dest: inout string; src: in string) is
	begin
		for i in src'low to src'high loop
			dest(i+ dest'low- src'low) := src(i);
		end loop;
		for i in src'high+ 1+ dest'low- src'low to dest'high loop
			dest(i) := ' ';
		end loop;	
	end procedure strcpy;	
	
	function toHexChar(a: std_logic_vector(3 downto 0)) return character is
	begin
	  case a is
		  when "0000" => return('0');
		  when "0001" => return('1');
		  when "0010" => return('2');
		  when "0011" => return('3');
		  when "0100" => return('4');
		  when "0101" => return('5');
		  when "0110" => return('6');
		  when "0111" => return('7');
		  when "1000" => return('8');
		  when "1001" => return('9');
		  when "1010" => return('A');
		  when "1011" => return('B');
		  when "1100" => return('C');
		  when "1101" => return('D');
		  when "1110" => return('E');
		  when "1111" => return('F');
		  when others => return('X');
	  end case;
	end;	

	function toDecString(num: std_logic_vector(2 downto 0)) return string is
	  variable str: string(1 downto 1);
	begin
		str(1) := toHexChar('0' & num);
		return str;
	end function toDecString;	
	
	function toDecString(num: natural) return string is
	  variable len : integer := 0;
	  variable str: string(10 downto 1);
	  variable v : integer := num;
	begin
	  for i in 0 to 9 loop 
	     str(i+ 1) := hex_array(v mod 10);
	     if str(i+ 1) /= '0'  then
	        len := i;
	     end if;
	     v := v/ 10;
	  end loop;
	  return (str(len+ 1 downto 1));
	end function toDecString;	
	
	function toOctString(num: natural) return string is
	  variable len : integer := 0;
	  variable str: string(10 downto 1);
	  variable v : integer := num;
	begin
	  for i in 0 to 9 loop 
	     str(i+ 1) := hex_array(v mod 8);
	     if str(i+ 1) /= '0'  then
	        len := i;
	     end if;
	     v := v/ 8;
	  end loop;
	  return (str(len+ 1 downto 1));
	end function toOctString;
	
	function toHexString(num: natural) return string is
	  variable len : integer := 0;
	  variable str: string(8 downto 1);
	  variable v : integer := num;
	begin
	  for i in 0 to 7 loop 
	     str(i+ 1) := hex_array(v mod 16);
	     if str(i+ 1) /= '0'  then
	        len := i;
	     end if;
	     v := v/ 16;
	  end loop;
	  return (str(len+ 1 downto 1));
	end function toHexString;	
	
  function toHexString(num: std_logic_vector(15 downto 0)) return string is
	begin
		return toHexString(CONV_INTEGER(num));
	end function toHexString;	
	
	function getRegisterName(reg: std_logic_vector(2 downto 0)) return string is
	begin	 
		case reg is
			when "000" => return "R0";
			when "001" => return "R1";
			when "010" => return "R2";
			when "011" => return "R3";
			when "100" => return "R4";
			when "101" => return "R5";
			when "110" => return "SP";
			when "111" => return "PC";
			when others => return "";
		end case;	
	end function getRegisterName;	

	function branchOffsetToString(offset: std_logic_vector(7 downto 0)) return string is
	begin
		if offset(7) = '1' then
			return "-" & toDecString(CONV_INTEGER(("0000000"- offset) & '0'));
		else	
			return toDecString(CONV_INTEGER(offset(6 downto 0) & '0'));
		end if;	
	end function branchOffsetToString;
	
	function immediateToString(immed: std_logic_vector(15 downto 0); immed_rdy: std_logic) return string is
	begin
		if immed_rdy = '0' then
			return UNDEF_OPERAND;
		else	
		  return toOctString(CONV_INTEGER(immed));
		end if;	
	end function immediateToString;	
	
	function disassembleOperand(op: std_logic_vector(5 downto 0); immed: std_logic_vector(15 downto 0);
	    immed_rdy: std_logic)
		return string is
		
	  alias mode: std_logic_vector(2 downto 0) is op(5 downto 3);
		alias reg:  std_logic_vector(2 downto 0) is op(2 downto 0);
	begin
		case mode is
			when "000" =>				     -- register Ri
				return getRegisterName(reg);
			when "001" =>	 			     -- register deferrred (Ri)
			  return "(" & getRegisterName(reg) & ")"; 
			when "010" =>	
			  if reg = "111" then    -- immediate #n
					return "#" & immediateToString(immed, immed_rdy);				  
				else	                 -- autoincrement (Ri)+
					return "(" & getRegisterName(reg) & ")+";
				end if;			
			when "011" =>
			  if reg = "111" then    -- absolute @#n
					return "@#" & immediateToString(immed, immed_rdy);	
				else	                 -- autoincrement deferred @(Ri)+
					return "@(" & getRegisterName(reg) & ")+";
				end if;			
			when "100" =>	           -- autodecrement -(Ri)
				return "-(" & getRegisterName(reg) & ")";
			when "101" =>						 -- autodecrement deferred @-(Ri)
				return "@-(" & getRegisterName(reg) & ")";
			when "110" =>
			  if reg = "111" then	   -- PC relative
					return immediateToString(immed, immed_rdy) & "(" & getRegisterName(reg) & ")";			
				else								   -- index n(Ri) 
				  return immediateToString(immed, immed_rdy) & "(" & getRegisterName(reg) & ")";	
				end if;			
			when "111" =>
			  if reg = "111" then	   -- PC relative	deferred
					return "@" & immediateToString(immed, immed_rdy) & "(" & getRegisterName(reg) & ")";	
				else								   -- index deferred @n(Ri) 
				  return "@" & immediateToString(immed, immed_rdy) & "(" & getRegisterName(reg) & ")";	
				end if;			
			when others =>
				return UNDEF_OPERAND;
		end case;	
		
		return UNDEF_OPERAND;
	end function disassembleOperand;	
	
	function disassemble(insn: std_logic_vector(15 downto 0); immed1: std_logic_vector(15 downto 0);
	      immed2: std_logic_vector(15 downto 0); insn_rdy: std_logic; immed1_rdy: std_logic;
	      immed2_rdy: std_logic) 
	    return string	is	
			
    alias op_code: std_logic_vector(3 downto 0) is insn(15 downto 12);
    alias op_prim: std_logic_vector(2 downto 0) is insn(14 downto 12);
    alias op_byte: std_logic is insn(15);   
    alias op_ext1: std_logic_vector(2 downto 0) is insn(11 downto 9);
    alias op_ext2: std_logic_vector(2 downto 0) is insn(8 downto 6);
    alias op_ext3: std_logic_vector(2 downto 0) is insn(5 downto 3);
    alias op_ext4: std_logic_vector(2 downto 0) is insn(2 downto 0);

		function src_str return string is
		begin
			return disassembleOperand(insn(11 downto 6), immed1, immed1_rdy);
		end function src_str;	
		
		function dst_str return string is
		begin																															
			if instruction_len(insn) = "11" then
				return disassembleOperand(insn(5 downto 0), immed2, immed2_rdy);
			else	
			  return disassembleOperand(insn(5 downto 0), immed1, immed1_rdy);
			end if;	
		end function dst_str;	
	begin	
		if insn_rdy = '0' then
			return UNDEF_INSTR;
		end if;	
		
    if op_prim = "000" then
      if op_byte = '0' and op_ext1 = "000" then
        if op_ext2 = "000" and op_ext3 = "000" then -- HALT,...,RTT
					case op_ext4 is
            when "000" =>               -- HALT
						  return "HALT";
            when "001" =>               -- WAIT 
							return "WAIT"; 
            when "010" =>               -- RTI
 							return "RTI";
            when "011" =>               -- BPT (trap to 14)
							return "BPT";
            when "100" =>               -- IOT (trap to 20)
							return "IOT";
            when "101" =>               -- RESET
 							return "RESET";
            when "110" =>               -- RTT
							return "RTT";
            when others => 
						  return UNDEF_INSTR;
          end case;
        end if;
 
        if op_ext2 = "001" then          -- JMP 
					return "JMP " & dst_str;
				end if;
 
        if op_ext2 = "010" then
          if op_ext3 = "000" then        -- RTS
						return "RTS " & getRegisterName(insn(2 downto 0));
					end if;
          if op_ext3 = "011" then        -- SPL
						return UNDEF_INSTR;
					end if;
        end if;
 
        if op_ext2 = "010" then
          if op_ext3(2) = '1' then       -- SEx/CLx
           	case insn(4 downto 0) is
							when "00000" =>  -- NOP
							  return "NOP";
							when "00001" =>  -- CLC
							  return "CLC";
							when "00010" =>  -- CLV
							  return "CLV";
							when "00100" =>  -- CLZ
							  return "CLZ";
							when "01000" =>  -- CLN
							  return "CLN";
							when "01111" =>  -- CCC
							  return "CCC";
							when "10001" =>  -- SEC
							  return "SEC";
							when "10010" =>  -- SEV
							  return "SEV";
							when "10100" =>  -- SEZ
							  return "SEZ";
							when "11000" =>  -- SEN
							  return "SEN";
							when "11111" =>  -- SCC
							  return "SCC";
							when others =>  null;
						end case;	 
					end if;
        end if;
 
        if op_ext2 = "011" then          -- SWAB 
					return "SWAB " & dst_str;
				end if;
      end if;
 
      if insn(14 downto 11) = "0000" then              -- BR class instructions
				if insn(15) = '1' then
					case insn(10 downto 8) is
						when "000" =>  -- BPL
						  return "BPL " & branchOffsetToString(insn(7 downto 0));
						when "001" =>  -- BMI		
							return "BMI " & branchOffsetToString(insn(7 downto 0));
						when "010" =>	 -- BHI
						  return "BHI " & branchOffsetToString(insn(7 downto 0));
						when "011" =>	 -- BLOS	
						  return "BLOS " & branchOffsetToString(insn(7 downto 0));
						when "100" =>	 --	BVC
						  return "BVC " & branchOffsetToString(insn(7 downto 0));
						when "101" =>	 -- BVS		
						  return "BVS " & branchOffsetToString(insn(7 downto 0));
						when "110" =>	 -- BHIS/BCC
						  return "BCC " & branchOffsetToString(insn(7 downto 0));
						when "111" =>		-- BLO/ BCS
						  return "BCS " & branchOffsetToString(insn(7 downto 0));
						when others =>
						  return UNDEF_INSTR;
					end case;	
				else
					case insn(10 downto 8) is
						when "001" =>  -- BR	
						  return "BR " & branchOffsetToString(insn(7 downto 0));
						when "010" =>  -- BNE	
						  return "BNE " & branchOffsetToString(insn(7 downto 0));
						when "011" =>	 -- BEQ	
						  return "BEQ " & branchOffsetToString(insn(7 downto 0));
						when "100" =>	 -- BGE
						  return "BGE " & branchOffsetToString(insn(7 downto 0));
						when "101" =>	 -- BLT			
						  return "BLT " & branchOffsetToString(insn(7 downto 0));
						when "110" =>	 -- BGT		
						  return "BGT " & branchOffsetToString(insn(7 downto 0));
						when "111" =>	 -- BLE		
						  return "BLE " & branchOffsetToString(insn(7 downto 0));
						when others => 
						  return UNDEF_INSTR;
					end case;							
				end if;	
			end if;
 
      if op_byte ='0' and op_ext1 = "100" then -- JSR
				return "JSR " & dst_str;
			end if;
 
      if op_byte = '1' and op_ext1 = "100" then -- EMT, TRAP
        if op_ext2(2) = '0' then         -- EMT (trap tp 30)
					return "EMT " & toOctString(CONV_INTEGER(insn(7 downto 0)));					
				else                            -- TRAP (trap to 34)
					return "TRAP " & toOctString(CONV_INTEGER(insn(7 downto 0)));
				end if;
      end if;
 
      if op_ext1 = "101" then            -- CLR(B),...,TST(B)
				if op_byte = '1' then
	        case op_ext2 is
	          when "000" =>                 -- CLRB:    0 +    0 + 0   (0)
						  return "CLRB " & dst_str;
						when "001" =>                 -- COMB:    0 + ~DST + 0   (~dst)
							return "COMB " & dst_str;
						when "010" =>                 -- INCB:    0 +  DST + 1   (dst+1)
							return "INCB " & dst_str;
						when "011" =>                 -- DECB:   ~0 +  DST + 0   (dst-1)
							return "DECB " & dst_str;
						when "100" =>                 -- NEGB:    0 + ~DST + 1   (-dst)
							return "NEGB " & dst_str;
						when "101" =>                 -- ADCB:    0 +  DST + CI  (dst+ci)
							return "ADCB " & dst_str;
						when "110" =>                 -- SBCB:   ~0 +  DST + ~CI (dst-ci)
							return "SBCB " & dst_str;
						when "111" =>                 -- TSTB:    0 +  DST + 0   (dst)
							return "TSTB " & dst_str;
						when others => 
						  return UNDEF_INSTR;
	        end case;					
				else	
	        case op_ext2 is
	          when "000" =>                 -- CLR:    0 +    0 + 0   (0)
						  return "CLR " & dst_str;
						when "001" =>                 -- COM:    0 + ~DST + 0   (~dst)
							return "COM " & dst_str;
						when "010" =>                 -- INC:    0 +  DST + 1   (dst+1)
							return "INC " & dst_str;
						when "011" =>                 -- DEC:   ~0 +  DST + 0   (dst-1)
							return "DEC " & dst_str;
						when "100" =>                 -- NEG:    0 + ~DST + 1   (-dst)
							return "NEG " & dst_str;
						when "101" =>                 -- ADC:    0 +  DST + CI  (dst+ci)
							return "ADC " & dst_str;
						when "110" =>                 -- SBC:   ~0 +  DST + ~CI (dst-ci)
							return "SBC " & dst_str;
						when "111" =>                 -- TST:    0 +  DST + 0   (dst)
							return "TST " & dst_str;
						when others => 
						  return UNDEF_INSTR;
	        end case;
				end if;
      end if;
 
      if op_ext1 = "110" then
        if op_ext2(2) = '0' then         -- ROR(B),...,ASL(B)
					if op_byte = '1' then
	          case op_ext2(1 downto 0) is
	            when "00" =>                -- RORB
							  return "RORB " & dst_str;
							when "01" =>                -- ROLB
							  return "ROLB " & dst_str;
							when "10" =>                -- ASRB
							  return "ASRB " & dst_str;
							when "11" =>                -- ASLB
							  return "ASLB " & dst_str;
							when others => 
							  return UNDEF_INSTR;
	          end case;						 
					else
            case op_ext2(1 downto 0) is
	            when "00" =>                -- ROR
							  return "ROR " & dst_str;
							when "01" =>                -- ROL	 
							  return "ROL " & dst_str;
							when "10" =>                -- ASR
							  return "ASR " & dst_str;
							when "11" =>                -- ASL
							  return "ASL " & dst_str;
							when others => 
							  return UNDEF_INSTR;
	          end case;							
					end if;	
        end if;
 
        if op_byte = '0' and op_ext2 = "100" then -- MARK
					return "MARK " & toDecString(CONV_INTEGER(insn(5 downto 0) & '0'));
				end if;
 
        if op_ext2 = "101" then          -- MFP(I/D)
					return UNDEF_INSTR;
				end if;
 
        if op_ext2 = "110" then          -- MTP(I/D)
					return UNDEF_INSTR;
				end if;
				
				if op_byte = '1' and op_ext2 = "111" then  -- MFPS
					return "MFPS " & dst_str;
				end if;	
				
			  if op_byte = '1' and op_ext2 = "100" then  -- MTPS					
					return "MTPS " & dst_str;
				end if;	
 
        if op_byte = '0' and op_ext2 = "111" then -- SXT
					return "SXT " & dst_str;
				end if;
      end if;
 
    end if;
 
    if op_prim /= "000" and op_prim /= "111" then
			if op_byte = '1' then
	      case op_prim is
	        when "001" =>                   -- MOVB
					  return "MOVB " & src_str & ", " & dst_str;
					when "010" =>                   -- CMPB
					  return "CMPB " & src_str & ", " & dst_str;
					when "011" =>                   -- BITB
					  return "BITB " & src_str & ", " & dst_str;
					when "100" =>                   -- BICB		 
					  return "BICB " & src_str & ", " & dst_str;
					when "101" =>                   -- BISB
					  return "BISB " & src_str & ", " & dst_str;
					when "110" =>	                  -- SUB
					  return "SUB " & src_str & ", " & dst_str;
	        when others => 
					  return UNDEF_INSTR;
	      end case;				
			else	
	      case op_prim is
	        when "001" =>                   -- MOV
					  return "MOV " & src_str & ", " & dst_str;
					when "010" =>                   -- CMP
					  return "CMP " & src_str & ", " & dst_str;
					when "011" =>                   -- BIT
					  return "BIT " & src_str & ", " & dst_str;
					when "100" =>                   -- BIC		 
					  return "BIC " & src_str & ", " & dst_str;
					when "101" =>                   -- BIS
					  return "BIS " & src_str & ", " & dst_str;
					when "110" =>	                  -- ADD
					  return "ADD " & src_str & ", " & dst_str;
	        when others => 
					  return UNDEF_INSTR;
	      end case;
 			end if;
    end if;
 
    if op_byte = '0' and op_prim = "111" then
      case op_ext1 is
        when "000" =>                   -- MUL
					return "MUL " & getRegisterName(insn(8 downto 6)) & ", " & dst_str;
				when "001" =>                   -- DIV
					return "DIV " & getRegisterName(insn(8 downto 6)) & ", " & dst_str;
				when "010" =>                   -- ASH
					return "ASH " & getRegisterName(insn(8 downto 6)) & ", " & dst_str;
				when "011" =>                   -- ASHC
					return "ASHC " & getRegisterName(insn(8 downto 6)) & ", " & dst_str;
				when "100" =>                   -- XOR
				  return "XOR " & getRegisterName(insn(8 downto 6)) & ", " & dst_str;
				when "111" =>                   -- SOB:  SRC +   ~0 + 0   (src-1)
					return "SOB " & getRegisterName(insn(8 downto 6)) & ", " & "-" & toDecString(CONV_INTEGER(insn(5 downto 0)) * 2);				
				when others => 
				  return UNDEF_INSTR;
      end case;      
    end if;			
		return "???";
	end function disassemble;	
end package body pdp11_disasm;
