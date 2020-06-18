-- Project Moonshiner-11 - PDP-11 compatible In-order superscalar core
--
-- Instruction Timings module
-- For emulation of old processor instruction execution time
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

entity timings is
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		
		insn: in std_logic_vector(15 downto 0);
		insn_rdy: in std_logic;
		
		delay: out std_logic
	);	
end timings;

architecture behaviour of timings is
  constant TICK: time := 20 ns;						 -- processor clock period
	
	constant TIMING_REGREG: time := 4.0 us;  -- Base timing
  constant TIMING_BRANCH: time := 5.4 us;  -- BR, BEQ etc.
  constant TIMING_ILLEGAL: time := 48 us;  --
  constant TIMING_WAIT: time := 380 us;    -- WAIT and RESET
  constant TIMING_EMT: time := 22.8 us;    -- IOT, BPT, EMT, TRAP - 42+5t
  constant TIMING_RTI: time := 13.4 us;    -- RTI and RTT - 24+2t
  constant TIMING_RTS: time := 10.8 us;    -- RTS
  constant TIMING_NOP: time := 4 us;       -- NOP and all commands without operands and with register operands
  constant TIMING_SOB: time := 6.8 us;     -- 
  constant TIMING_MARK: time := 12 us;     --	 
	
	constant COUNTER_SIZE: natural := 16;
	subtype cnt_vector is std_logic_vector(COUNTER_SIZE-1 downto 0);
	
	function TimeA(mode: std_logic_vector(2 downto 0)) return cnt_vector is
	  variable v: cnt_vector;
	begin									
		case mode is
			when "000" =>	    -- register Ri
			  v := (others => '0');
			when "001" =>		  -- register deferrred (Ri)
			  v := CONV_STD_LOGIC_VECTOR(4.0 us/TICK, COUNTER_SIZE);
			when "010" =>			-- autoincrement (Ri)+ , immediate #n
			  v := CONV_STD_LOGIC_VECTOR(4.0 us/TICK, COUNTER_SIZE);
			when "011" =>			-- autoincrement deferred @(Ri)+ , absolute @#n
			  v := CONV_STD_LOGIC_VECTOR(6.8 us/TICK, COUNTER_SIZE);
			when "100" =>			-- autodecrement -(Ri)
			  v := CONV_STD_LOGIC_VECTOR(4.0 us/TICK, COUNTER_SIZE);
			when "101" =>			-- autodecrement deferred @-(Ri)
			  v := CONV_STD_LOGIC_VECTOR(6.8 us/TICK, COUNTER_SIZE);
			when "110" =>     -- index n(Ri) ,  PC relative         
			  v := CONV_STD_LOGIC_VECTOR(6.8 us/TICK, COUNTER_SIZE);
			when "111" =>			-- index deferred @n(Ri) , PC relative deferred				  
			  v := CONV_STD_LOGIC_VECTOR(9.4 us/TICK, COUNTER_SIZE);
			when others => 
			  v := (others => '0');
		end case;							 
		return v;
	end function TimeA;	
	
  function TimeA1(mode: std_logic_vector(2 downto 0)) return cnt_vector is
	begin
		return TimeA(mode);
	end function TimeA1;	
	
	function TimeAB(mode: std_logic_vector(2 downto 0)) return cnt_vector is
	  variable v: cnt_vector;
	begin									
		case mode is
			when "000" =>	    -- register Ri
			  v := (others => '0');
			when "001" =>		  -- register deferrred (Ri)
			  v := CONV_STD_LOGIC_VECTOR(5.4 us/TICK, COUNTER_SIZE);
			when "010" =>			-- autoincrement (Ri)+ , immediate #n
			  v := CONV_STD_LOGIC_VECTOR(5.4 us/TICK, COUNTER_SIZE);
			when "011" =>			-- autoincrement deferred @(Ri)+ , absolute @#n
			  v := CONV_STD_LOGIC_VECTOR(8.0 us/TICK, COUNTER_SIZE);
			when "100" =>			-- autodecrement -(Ri)
			  v := CONV_STD_LOGIC_VECTOR(5.4 us/TICK, COUNTER_SIZE);
			when "101" =>			-- autodecrement deferred @-(Ri)
			  v := CONV_STD_LOGIC_VECTOR(8.0 us/TICK, COUNTER_SIZE);
			when "110" =>     -- index n(Ri) ,  PC relative         
			  v := CONV_STD_LOGIC_VECTOR(8.0 us/TICK, COUNTER_SIZE);
			when "111" =>			-- index deferred @n(Ri) , PC relative deferred				  
			  v := CONV_STD_LOGIC_VECTOR(10.8 us/TICK, COUNTER_SIZE);
			when others => 
			  v := (others => '0');
		end case;							 
		return v;
	end function TimeAB;	
	
	function TimeB(mode: std_logic_vector(2 downto 0)) return cnt_vector is
	  variable v: cnt_vector;
	begin									
		case mode is
			when "000" =>	    -- register Ri
			  v := (others => '0');
			when "001" =>		  -- register deferrred (Ri)
			  v := CONV_STD_LOGIC_VECTOR(6.8 us/TICK, COUNTER_SIZE);
			when "010" =>			-- autoincrement (Ri)+ , immediate #n
			  v := CONV_STD_LOGIC_VECTOR(6.8 us/TICK, COUNTER_SIZE);
			when "011" =>			-- autoincrement deferred @(Ri)+ , absolute @#n
			  v := CONV_STD_LOGIC_VECTOR(10.8 us/TICK, COUNTER_SIZE);
			when "100" =>			-- autodecrement -(Ri)
			  v := CONV_STD_LOGIC_VECTOR(6.8 us/TICK, COUNTER_SIZE);
			when "101" =>			-- autodecrement deferred @-(Ri)
			  v := CONV_STD_LOGIC_VECTOR(10.8 us/TICK, COUNTER_SIZE);
			when "110" =>     -- index n(Ri) ,  PC relative         
			  v := CONV_STD_LOGIC_VECTOR(10.8 us/TICK, COUNTER_SIZE);
			when "111" =>			-- index deferred @n(Ri) , PC relative deferred				  
			  v := CONV_STD_LOGIC_VECTOR(13.4 us/TICK, COUNTER_SIZE);
			when others => 
			  v := (others => '0');
		end case;							 
		return v;
	end function TimeB;	
	
	function TimeA2(mode: std_logic_vector(2 downto 0)) return cnt_vector is
	  variable v: time;
	begin									
		case mode is
			when "000" =>	    -- register Ri
			  v := 0 us;
			when "001" =>		  -- register deferrred (Ri)
			  v := 6.8 us;
			when "010" =>			-- autoincrement (Ri)+ , immediate #n
			  v := 6.8 us;
			when "011" =>			-- autoincrement deferred @(Ri)+ , absolute @#n
			  v := 9.4 us;
			when "100" =>			-- autodecrement -(Ri)
			  v := 6.8 us;
			when "101" =>			-- autodecrement deferred @-(Ri)
			  v := 9.4 us;
			when "110" =>     -- index n(Ri) ,  PC relative         
			  v := 9.4 us;
			when "111" =>			-- index deferred @n(Ri) , PC relative deferred				  
			  v := 12.0 us;
			when others => 
			  v := 0 us;
		end case;							 
		return CONV_STD_LOGIC_VECTOR(v / TICK, COUNTER_SIZE);
	end function TimeA2;

	function TimeDst(mode: std_logic_vector(2 downto 0)) return cnt_vector is
	begin
		if mode /= "000" then
			return TimeAB(mode);
		else	
			return TimeB(mode);
		end if;	
	end function TimeDst;
	
	function TimeCmp(mode: std_logic_vector(2 downto 0)) return cnt_vector is
	begin
		if mode /= "000" then
			return TimeA1(mode);
		else	
			return TimeA2(mode);
		end if;	
	end function TimeCmp;	
	
  function TimeNj(mode: std_logic_vector(2 downto 0)) return cnt_vector is
	begin
		return TimeA2(mode);
	end function TimeNj;	
	
	function TimeNs(mode: std_logic_vector(2 downto 0)) return cnt_vector is
	  variable v: cnt_vector;
	begin									
		case mode is
			when "000" =>	    -- register Ri
			  v := (others => '0');
			when "001" =>		  -- register deferrred (Ri)
			  v := CONV_STD_LOGIC_VECTOR(10.8 us/TICK, COUNTER_SIZE);
			when "010" =>			-- autoincrement (Ri)+ , immediate #n
			  v := CONV_STD_LOGIC_VECTOR(10.8 us/TICK, COUNTER_SIZE);
			when "011" =>			-- autoincrement deferred @(Ri)+ , absolute @#n
			  v := CONV_STD_LOGIC_VECTOR(13.4 us/TICK, COUNTER_SIZE);
			when "100" =>			-- autodecrement -(Ri)
			  v := CONV_STD_LOGIC_VECTOR(10.8 us/TICK, COUNTER_SIZE);
			when "101" =>			-- autodecrement deferred @-(Ri)
			  v := CONV_STD_LOGIC_VECTOR(13.4 us/TICK, COUNTER_SIZE);
			when "110" =>     -- index n(Ri) ,  PC relative         
			  v := CONV_STD_LOGIC_VECTOR(13.4 us/TICK, COUNTER_SIZE);
			when "111" =>			-- index deferred @n(Ri) , PC relative deferred				  
			  v := CONV_STD_LOGIC_VECTOR(16.0 us/TICK, COUNTER_SIZE);
			when others => 
			  v := (others => '0');
		end case;							 
		return v;
	end function TimeNs;	

	type state_type is (st_idle, st_load_cnt, st_wait);
	signal ireg: std_logic_vector(15 downto 0);
	signal insn_time: std_logic_vector(COUNTER_SIZE-1 downto 0); 
	signal delay_cnt: cnt_vector;
	signal state, next_state: state_type;
begin	 
	decode_proc: process (ireg)
    alias op_code: std_logic_vector(3 downto 0) is ireg(15 downto 12);
    alias op_prim: std_logic_vector(2 downto 0) is ireg(14 downto 12);
    alias op_byte: std_logic is ireg(15);   
    alias op_ext1: std_logic_vector(2 downto 0) is ireg(11 downto 9);
    alias op_ext2: std_logic_vector(2 downto 0) is ireg(8 downto 6);
    alias op_ext3: std_logic_vector(2 downto 0) is ireg(5 downto 3);
    alias op_ext4: std_logic_vector(2 downto 0) is ireg(2 downto 0);	

	  alias src_mode: std_logic_vector(2 downto 0) is ireg(11 downto 9);
	  alias dst_mode: std_logic_vector(2 downto 0) is ireg(5 downto 3);
	 
	  alias src_reg: std_logic_vector(2 downto 0) is ireg(8 downto 6); 
	  alias dst_reg: std_logic_vector(2 downto 0) is ireg(2 downto 0);		
		
		constant T_REGREG: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_REGREG/TICK, COUNTER_SIZE);
		constant T_NOP: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_NOP/TICK, COUNTER_SIZE);
		constant T_BRANCH: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_BRANCH/TICK, COUNTER_SIZE);
		constant T_ILLEGAL: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_ILLEGAL/TICK, COUNTER_SIZE);
		constant T_WAIT: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_WAIT/TICK, COUNTER_SIZE);
		constant T_EMT: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_EMT/TICK, COUNTER_SIZE);
		constant T_RTI: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_RTI/TICK, COUNTER_SIZE);
		constant T_RTS: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_RTS/TICK, COUNTER_SIZE);
		constant T_SOB: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_SOB/TICK, COUNTER_SIZE);
		constant T_MARK: std_logic_vector(COUNTER_SIZE-1 downto 0) := CONV_STD_LOGIC_VECTOR(TIMING_MARK/TICK, COUNTER_SIZE);
		
		variable t1: std_logic_vector(COUNTER_SIZE-1 downto 0);
		variable t2: std_logic_vector(COUNTER_SIZE-1 downto 0);
		variable t3: std_logic_vector(COUNTER_SIZE-1 downto 0);
	begin				 
		t1 := (others => '0');
		t2 := (others => '0');
		t3 := (others => '0');
    if op_prim = "000" then
      if op_byte = '0' and op_ext1 = "000" then
        if op_ext2 = "000" and op_ext3 = "000" then -- HALT,...,RTT
					case op_ext4 is
            when "000" =>               -- HALT
						  t1 := T_NOP;
            when "001" =>               -- WAIT 
						  t1 := T_NOP;
            when "010" =>               -- RTI
						  t1 := T_RTI;
            when "011" =>               -- BPT (trap to 14)
						  t1 := T_EMT;
            when "100" =>               -- IOT (trap to 20)	 
						  t1 := T_EMT;
            when "101" =>               -- RESET
						  t1 := T_WAIT;
            when "110" =>               -- RTT
						  t1 := T_RTI;
            when others =>  null;
          end case;
        end if;
 
        if op_ext2 = "001" then          -- JMP 
					if dst_mode = "000" then
						t1 := T_EMT;
					else	
						t1 := TimeNj(dst_mode);
					end if;	
				end if;
 
        if op_ext2 = "010" then
          if op_ext3 = "000" then        -- RTS
						t1 := T_RTS;						
					end if;
          if op_ext3 = "011" then        -- SPL
						
					end if;
        end if;
 
        if op_ext2 = "010" then	 
          if op_ext3(2) = '1' then       -- SEx/CLx
						t1 := T_NOP;
           	case ireg(4 downto 0) is
							when "00001" =>  -- CLC
							when "00010" =>  -- CLV
							when "00100" =>  -- CLZ
							when "01000" =>  -- CLN
							when "01111" =>  -- CCC
							when "10001" =>  -- SEC
							when "10010" =>  -- SEV
							when "10100" =>  -- SEZ
							when "11000" =>  -- SEN
							when "11111" =>  -- SCC
							when others =>  null;
						end case;	 
					end if;
        end if;
 
        if op_ext2 = "011" then          -- SWAB 
					t1 := T_REGREG;
					t3 := TimeAB(dst_mode);
				end if;
      end if;
 
      if ireg(14 downto 11) = "0000" then              -- BR class instructions
				t1 := T_BRANCH;
				if ireg(15) = '1' then
					case ireg(10 downto 8) is
						when "000" =>  -- BPL
						when "001" =>  -- BMI		
						when "010" =>	 -- BHI
						when "011" =>	 -- BLOS	
						when "100" =>	 --	BVC
						when "101" =>	 -- BVS		
						when "110" =>	 -- BHIS/BCC
						when "111" =>		-- BLO/ BCS
						when others => null; 
					end case;	
				else
					case ireg(10 downto 8) is
						when "001" =>  -- BR	
						when "010" =>  -- BNE	
						when "011" =>	 -- BEQ	
						when "100" =>	 -- BGE
						when "101" =>	 -- BLT			
						when "110" =>	 -- BGT		
						when "111" =>	 -- BLE		
						when others => null; 
					end case;							
				end if;	
			end if;
 
      if op_byte ='0' and op_ext1 = "100" then -- JSR
				if dst_mode = "000" then
					t1 := T_EMT;
				else										 
					t3 := TimeNs(dst_mode);
				end if;	
			end if;
 
      if op_byte = '1' and op_ext1 = "100" then -- EMT, TRAP
				t1 := T_EMT;
        if op_ext2(2) = '0' then         -- EMT (trap tp 30)
					
				else                            -- TRAP (trap to 34)
					
				end if;
      end if;
 
      if op_ext1 = "101" then            -- CLR(B),...,TST(B)
				t1 := T_REGREG;
				t3 := TimeAB(dst_mode);
				if op_byte = '1' then
	        case op_ext2 is
	          when "000" =>                 -- CLRB:    0 +    0 + 0   (0)
						when "001" =>                 -- COMB:    0 + ~DST + 0   (~dst)
						when "010" =>                 -- INCB:    0 +  DST + 1   (dst+1)
						when "011" =>                 -- DECB:   ~0 +  DST + 0   (dst-1)
						when "100" =>                 -- NEGB:    0 + ~DST + 1   (-dst)
						when "101" =>                 -- ADCB:    0 +  DST + CI  (dst+ci)
						when "110" =>                 -- SBCB:   ~0 +  DST + ~CI (dst-ci)
						when "111" =>                 -- TSTB:    0 +  DST + 0   (dst)
						when others => null;
	        end case;					
				else	
	        case op_ext2 is
	          when "000" =>                 -- CLR:    0 +    0 + 0   (0)
						when "001" =>                 -- COM:    0 + ~DST + 0   (~dst)
						when "010" =>                 -- INC:    0 +  DST + 1   (dst+1)
						when "011" =>                 -- DEC:   ~0 +  DST + 0   (dst-1)
						when "100" =>                 -- NEG:    0 + ~DST + 1   (-dst)
						when "101" =>                 -- ADC:    0 +  DST + CI  (dst+ci)
						when "110" =>                 -- SBC:   ~0 +  DST + ~CI (dst-ci)
						when "111" =>                 -- TST:    0 +  DST + 0   (dst)
						when others =>  null;
	        end case;
				end if;
      end if;
 
      if op_ext1 = "110" then
        if op_ext2(2) = '0' then         -- ROR(B),...,ASL(B)	
					t1 := T_REGREG;
				  t3 := TimeAB(dst_mode);
					if op_byte = '1' then
	          case op_ext2(1 downto 0) is
	            when "00" =>                -- RORB
							when "01" =>                -- ROLB
							when "10" =>                -- ASRB
							when "11" =>                -- ASLB
							when others => null;
	          end case;						 
					else
            case op_ext2(1 downto 0) is
	            when "00" =>                -- ROR
							when "01" =>                -- ROL	 
							when "10" =>                -- ASR
							when "11" =>                -- ASL
							when others => null;
	          end case;							
					end if;	
        end if;
 
        if op_byte = '0' and op_ext2 = "100" then -- MARK
					t1 := T_MARK;
				end if;
 
        if op_ext2 = "101" then          -- MFP(I/D)
					
				end if;
 
        if op_ext2 = "110" then          -- MTP(I/D)
					
				end if;
				
				if op_byte = '1' and op_ext2 = "111" then  -- MFPS
				  t1 := T_REGREG;
				  t3 := TimeAB(dst_mode);					
				end if;	
				
			  if op_byte = '1' and op_ext2 = "100" then  -- MTPS					
				  t1 := T_REGREG;
				  t3 := TimeAB(dst_mode);					
				end if;	
 
        if op_byte = '0' and op_ext2 = "111" then -- SXT
				  t1 := T_REGREG;
				  t3 := TimeAB(dst_mode);					
				end if;
      end if;
 
    end if;
 
    if op_prim /= "000" and op_prim /= "111" then
			t1 := T_REGREG;
			if op_byte = '1' then
	      case op_prim is
	        when "001" =>                   -- MOVB
					  t2 := TimeA(src_mode);
						t3 := TimeDst(dst_mode);
					when "010" =>                   -- CMPB
					  t2 := TimeA1(src_mode);
						t3 := TimeCmp(dst_mode);					
					when "011" =>                   -- BITB	
					  t2 := TimeA1(src_mode);
						t3 := TimeCmp(dst_mode);					
					when "100" =>                   -- BICB	
					  t2 := TimeA(src_mode);
						t3 := TimeDst(dst_mode);					
					when "101" =>                   -- BISB
					  t2 := TimeA(src_mode);
						t3 := TimeDst(dst_mode);					
					when "110" =>	                  -- SUB
					  t2 := TimeA(src_mode);
						t3 := TimeDst(dst_mode);					
	        when others =>  null;
	      end case;				
			else	
	      case op_prim is
	        when "001" =>                   -- MOV
					  t2 := TimeA(src_mode);
						t3 := TimeDst(dst_mode);					
					when "010" =>                   -- CMP
					  t2 := TimeA1(src_mode);
						t3 := TimeCmp(dst_mode);					
					when "011" =>                   -- BIT
					  t2 := TimeA1(src_mode);
						t3 := TimeCmp(dst_mode);					
					when "100" =>                   -- BIC	
					  t2 := TimeA(src_mode);
						t3 := TimeDst(dst_mode);					
					when "101" =>                   -- BIS
					  t2 := TimeA(src_mode);
						t3 := TimeDst(dst_mode);					
					when "110" =>	                  -- ADD 
					  t2 := TimeA(src_mode);
						t3 := TimeDst(dst_mode);					
	        when others => null; 
	      end case;
 			end if;
    end if;
 
    if op_byte = '0' and op_prim = "111" then
      case op_ext1 is
        when "000" =>                   -- MUL
				when "001" =>                   -- DIV
				when "010" =>                   -- ASH
				when "011" =>                   -- ASHC
				when "100" =>                   -- XOR
				  t1 := T_REGREG;
					t3 := TimeA2(dst_mode);
				when "111" =>                   -- SOB:  SRC +   ~0 + 0   (src-1)
				  t1 := T_SOB;
				when others => null;
      end case;      
    end if;			
		insn_time <= t1 + t2 + t3;
	end process decode_proc;	

	next_state_proc: process (state, insn_rdy, delay_cnt)
	begin	 
		case state is
			when st_idle =>	
			  if insn_rdy = '1' then
					next_state <= st_load_cnt;
				else
					next_state <= st_idle;
				end if;
			when st_load_cnt =>
			  next_state <= st_wait;
			when st_wait =>
			  if delay_cnt(delay_cnt'high downto 1) = 0 then
					next_state <= st_idle;
				else
					next_state <= st_wait;
				end if;
		end case;
	end process next_state_proc;	
	
	ireg <= insn;
	process (reset, clk)
	begin	
		if reset = '1' then
			state <= st_idle;
			delay_cnt <= (others => '0');
		elsif rising_edge(clk) then
			state <= next_state;
			if state = st_load_cnt then
--				delay_cnt <= insn_time;
        delay_cnt <= CONV_STD_LOGIC_VECTOR(20, 16);  
			else	
				if delay_cnt /= 0 then
					delay_cnt <= delay_cnt - 1;
				end if;	
			end if;	
		end if;
	end process;	

	delay <= '0' when next_state = st_idle else '1';
end behaviour;
