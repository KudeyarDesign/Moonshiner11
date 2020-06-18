-- Project Moonshiner-11 - PDP-11 compatible In-order superscalar core
--
-- Submodule interfaces
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
use IEEE.std_logic_1164.all;
use work.def.all;

package iface is
  type icache_in_type is record	
		next_pc: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto PC_LOW);
    pc: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto PC_LOW);
		address_stall: std_logic;
		en: std_logic; 
		flush: std_logic;
		wr_address: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		wr_data: std_logic_vector(WORD_SIZE-1 downto 0);
		wr: std_logic;															 
		byte_en: std_logic_vector(WORD_SIZE/8-1 downto 0);
		RAM_window0: std_logic_vector(2 downto 0);
		RAM_window1: std_logic_vector(2 downto 0);
		ROM_page: std_logic_vector(6 downto 0);		
		smk_page: std_logic_vector(7 downto 1);
  end record;

  type icache_out_type is record
    data0: std_logic_vector(WORD_SIZE-1 downto 0);
		data0_rdy: std_logic;
    data1: std_logic_vector(WORD_SIZE-1 downto 0);
		data1_rdy: std_logic;		
  end record;	

	type dcache_in_type is record
		rd_address: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		rd: std_logic;
		rd_address_stall: std_logic;
		wr_address: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		wr_data: std_logic_vector(WORD_SIZE-1 downto 0);
		wr: std_logic;															 
		byte_en: std_logic_vector(WORD_SIZE/8-1 downto 0);
		RAM_window0: std_logic_vector(2 downto 0);
		RAM_window1: std_logic_vector(2 downto 0);
		ROM_page: std_logic_vector(6 downto 0);		
		smk_page: std_logic_vector(7 downto 1);
	end record;
	
	type dcache_out_type is record
		data: std_logic_vector(WORD_SIZE-1 downto 0);
		data_rdy: std_logic;
		wr_rdy: std_logic;
	end record;	
	
	type ibuffer_in_type is record
		pc: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		icache_data0: std_logic_vector(WORD_SIZE-1 downto 0);
		icache_data0_rdy: std_logic;
		icache_data1: std_logic_vector(WORD_SIZE-1 downto 0);
		icache_data1_rdy: std_logic;		
		insn_rd: std_logic;
		flush: std_logic;
	end record;
	
	type ibuffer_out_type is record
		pc: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		insn: std_logic_vector(WORD_SIZE-1 downto 0);
		insn_len: std_logic_vector(1 downto 0);
		insn_rdy: std_logic;
		immed1: std_logic_vector(WORD_SIZE-1 downto 0);
		immed1_pc: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		immed1_rdy: std_logic;
		immed2: std_logic_vector(WORD_SIZE-1 downto 0);
		immed2_pc: std_logic_vector(WORD_SIZE-1 downto PC_LOW);
		immed2_rdy: std_logic;
		read_num: std_logic_vector(1 downto 0);
	end record;	
	
	type gpr_in_type is record
		rd_address1: std_logic_vector(3 downto 0);
		pc1: std_logic_vector(WORD_SIZE-1 downto 0);
		immed1: std_logic_vector(WORD_SIZE-1 downto 0);
		rd_address2: std_logic_vector(3 downto 0);
		pc2: std_logic_vector(WORD_SIZE-1 downto 0);
		immed2: std_logic_vector(WORD_SIZE-1 downto 0);
		rd_address3: std_logic_vector(3 downto 0);
		pc3: std_logic_vector(WORD_SIZE-1 downto 0);
		immed3: std_logic_vector(WORD_SIZE-1 downto 0);	 
		dbg_address: std_logic_vector(3 downto 0);
		wr_address1: std_logic_vector(3 downto 0);
		wr_data1: std_logic_vector(WORD_SIZE-1 downto 0);
		wr_byte1: std_logic;
		we1: std_logic;
		wr_address2: std_logic_vector(3 downto 0);
		wr_data2: std_logic_vector(WORD_SIZE-1 downto 0);
		we2: std_logic;	
		psw: std_logic_vector(WORD_SIZE-1 downto 0);
		mem_data: std_logic_vector(WORD_SIZE-1 downto 0);
	end record;	

	type gpr_out_type is record
		rd_data1: std_logic_vector(WORD_SIZE-1 downto 0);
		rd_data2: std_logic_vector(WORD_SIZE-1 downto 0);
		rd_data3: std_logic_vector(WORD_SIZE-1 downto 0);
		dbg_data: std_logic_vector(WORD_SIZE-1 downto 0);
	end record;		

  type alu_oper_type is (alu_add, alu_adc, alu_sub, alu_rsb, alu_sbc, alu_ror,
		alu_rol, alu_asr, alu_asl, alu_sxt, alu_bit, alu_bic, alu_bis, alu_xor, alu_swab, alu_movb);	
  type flag_oper_type is (fl_clear, fl_set, fl_alu, fl_disable);
	
  type alu_in_type is record
    src: std_logic_vector(WORD_SIZE-1 downto 0);
    dst: std_logic_vector(WORD_SIZE-1 downto 0);
    oper: alu_oper_type;	
		n_oper: flag_oper_type;
		z_oper: flag_oper_type;
		v_oper: flag_oper_type;
		c_oper: flag_oper_type;		
		fbyte: std_logic;
    n, z, v, c: std_logic;		
	end record;
	
	type alu_out_type is record
    result: std_logic_vector(WORD_SIZE-1 downto 0);
    n, z, v, c: std_logic;		
	end record;

	type hw_debugger_in_type is record
		pc: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 1);
		pc_valid: std_logic;
		jump: std_logic;	
		jump_address: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 1);
		insn: std_logic_vector(WORD_SIZE-1 downto 0);
		immed1: std_logic_vector(WORD_SIZE-1 downto 0);
		immed2: std_logic_vector(WORD_SIZE-1 downto 0);
		mem_rd_address: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		mem_wr_address: std_logic_vector(PHYSICAL_ADDRESS_WIDTH-1 downto 0);
		mem_wr_data: std_logic_vector(WORD_SIZE-1 downto 0);
		load: std_logic;
		store: std_logic;
		byte_en: std_logic_vector(WORD_SIZE/8-1 downto 0);
    reg_value: std_logic_vector(WORD_SIZE-1 downto 0);
		pipe_state: std_logic_vector(WORD_SIZE-1 downto 0);
	end record;
	
	type hw_debugger_out_type is record
		reg_address: std_logic_vector(3 downto 0);
		halt: std_logic;
	end record;	
	
	function alu_oper_toString(op: alu_oper_type) return string;
	function flag_oper_toString(op: flag_oper_type) return string;
	
  component icache is
  generic
  (
    CACHE_SIZE: natural:= 2048;
    ASSOCIATIVITY: natural:= 2;
    LINE_SIZE: natural:= 8;
		DATA_WIDTH: natural:= 32
  );   
  port
  (
	  clk : in std_logic;
		reset : in std_logic;  
		
		din: in icache_in_type;
		dout: out icache_out_type;
		
		avm_address: out std_logic_vector(31 downto 0);
		avm_burstcount: out std_logic_vector(log2(LINE_SIZE) downto 0);
		avm_read: out std_logic;
		avm_flush: out std_logic;
		avm_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_readdatavalid: in std_logic;
		avm_waitrequest: in std_logic		 
	);	
  end component;
	
  component dcache is
  generic
  (
    CACHE_SIZE: natural:= 2048;
    ASSOCIATIVITY: natural:= 2;
    LINE_SIZE: natural:= 8;
		DATA_WIDTH: natural:= 16
  );   
  port
  (
	  clk : in std_logic;
		reset : in std_logic;  

		din: in dcache_in_type;
		dout: out dcache_out_type;
--		dbg_state: out dcache_debug_state;
		
		-- Avalon bus Master interface
		avm_address: out std_logic_vector(31 downto 0);
		avm_burstcount: out std_logic_vector(log2(LINE_SIZE) downto 0);
		avm_read: out std_logic;
		avm_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_readdatavalid: in std_logic;
		avm_write: out std_logic;
		avm_writedata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_byteenable: out std_logic_vector(DATA_WIDTH/8 -1 downto 0);
		avm_waitrequest: in std_logic;
		
		-- Avalon bus Master interface
		avm_io_address: out std_logic_vector(31 downto 0);
		avm_io_read: out std_logic;
		avm_io_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_io_readdatavalid: in std_logic;
		avm_io_write: out std_logic;
		avm_io_writedata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_io_byteenable: out std_logic_vector(DATA_WIDTH/8 -1 downto 0);
		avm_io_waitrequest: in std_logic		
	);	
  end component;	
	
  component ibuffer is
	port
	(
		clk : in std_logic;
		reset : in std_logic;	
	  din: in ibuffer_in_type;
		dout: out ibuffer_out_type
	);
  end component;	
	
  component gpr is
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		din: in gpr_in_type;
		dout: out gpr_out_type
	);
  end component;
	
  component alu is
	port
	(
	  din: in alu_in_type;
		dout: out alu_out_type
	);
  end component;	
	
  component timings is
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		
		insn: in std_logic_vector(15 downto 0);
		insn_rdy: in std_logic;
		
		delay: out std_logic
	);	
  end component;	

  component hw_debugger is
  generic
  (
	  INSTRUCTION_BREAKPOINT_NUM: natural range 1 to 8;
		DATA_BREAKPOINT_NUM: natural range 1 to 6;
		TRACE_BUFFER_SIZE: natural
  );   
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		
		-- Avalon bus slave
		avs_dbg_address: in std_logic_vector(8 downto 0);
		avs_dbg_writedata: in std_logic_vector(15 downto 0); 
		avs_dbg_chipselect: in std_logic;
		avs_dbg_read: in std_logic;
		avs_dbg_write: in std_logic;
		avs_dbg_readdata: out std_logic_vector(WORD_SIZE-1 downto 0);

		din: in hw_debugger_in_type;
		dout: out hw_debugger_out_type;
		
		reset_out: out std_logic;
		int_reset_out: out std_logic		
  );		
  end component;	
	
  component hangup_detector is	 
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
  end component;	
	
  component umul9x9 is
	port
	(
		dataa		: in std_logic_vector(8 downto 0);
		datab		: in std_logic_vector(8 downto 0);
		result		: out std_logic_vector(17 downto 0)
	);
  end component;	
	
  component umul16x16 is
	port
	(
		dataa		: in std_logic_vector(15 downto 0);
		datab		: in std_logic_vector(15 downto 0);
		result		: out std_logic_vector(31 downto 0)
	);
 end component;
 
  component fdd is
	port
	(
		clk: in std_logic;   
		reset: in std_logic;
		address: in std_logic_vector(0 downto 0);
		writedata: in std_logic_vector(15 downto 0); 
		chipselect: in std_logic;
		read: in std_logic;
		write: in std_logic;
		readdata: out std_logic_vector(15 downto 0);
		fdd_ds0: out std_logic;  
		fdd_ds1: out std_logic;	  
		fdd_ds2: out std_logic;
		fdd_ds3: out std_logic;
		fdd_mon: out std_logic;	 
		fdd_dir: out std_logic;	 
		fdd_st:  out std_logic;	  
		fdd_wd:  out std_logic;	 
		fdd_wen: out std_logic;	 
		fdd_sd:  out std_logic;	 
		fdd_idx: in std_logic;	 
		fdd_tr0: in std_logic;	
		fdd_wp:  in std_logic;	 
		fdd_rd:  in std_logic; 	
		fdd_rdy: in std_logic;		
		fdd_rez: out std_logic
	);
  end component; 
end package iface; 

package body iface is
	function alu_oper_toString(op: alu_oper_type) return string is
	begin	
		case op is
      when alu_add =>    return "ADD";
			when alu_adc =>	   return "ADC";
			when alu_sub =>	   return "SUB";
			when alu_rsb =>	   return "RSB";
			when alu_sbc =>	   return "SBC";
			when alu_ror =>	   return "ROR";
		  when alu_rol =>	   return "ROL";
			when alu_asr =>	   return "ASR";
			when alu_asl =>	   return "ASL";
			when alu_sxt =>	   return "SXT";
			when alu_bit =>	   return "BIT";
			when alu_bic =>	   return "BIC";
			when alu_bis =>	   return "BIS";
			when alu_xor =>	   return "XOR";
			when alu_swab =>	 return "SWAB";
			when alu_movb	=>	 return "MOVB";
			when others =>     return "???";
		end case;	
	end function alu_oper_toString;	
	
	function flag_oper_toString(op: flag_oper_type) return string is
	begin
		case op is
			when fl_clear =>    return "0";
			when fl_set => 		  return "1";
			when fl_alu =>		  return "A";
			when fl_disable =>	return "-";	
			when others =>      return "???";
		end case;	
	end function flag_oper_toString;	
	
	
end package body iface;	
