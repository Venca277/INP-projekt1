-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): jmeno <login AT stud.fit.vutbr.cz>
--
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
ENTITY cpu IS
  PORT (
    CLK : IN STD_LOGIC; -- hodinovy signal
    RESET : IN STD_LOGIC; -- asynchronni reset procesoru
    EN : IN STD_LOGIC; -- povoleni cinnosti procesoru

    -- synchronni pamet RAM
    DATA_ADDR : OUT STD_LOGIC_VECTOR(12 DOWNTO 0); -- adresa do pameti
    DATA_WDATA : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
    DATA_RDATA : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
    DATA_RDWR : OUT STD_LOGIC; -- cteni (1) / zapis (0)
    DATA_EN : OUT STD_LOGIC; -- povoleni cinnosti

    -- vstupni port
    IN_DATA : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
    IN_VLD : IN STD_LOGIC; -- data platna
    IN_REQ : OUT STD_LOGIC; -- pozadavek na vstup data

    -- vystupni port
    OUT_DATA : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- zapisovana data
    OUT_BUSY : IN STD_LOGIC; -- LCD je zaneprazdnen (1), nelze zapisovat
    OUT_INV : OUT STD_LOGIC; -- pozadavek na aktivaci inverzniho zobrazeni (1)
    OUT_WE : OUT STD_LOGIC; -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

    -- stavove signaly
    READY : OUT STD_LOGIC; -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
    DONE : OUT STD_LOGIC -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
  );
END cpu;
-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
ARCHITECTURE behavioral OF cpu IS

  CONSTANT MASK : STD_LOGIC_VECTOR(12 DOWNTO 0) := "1111111111111";

  --POINTER PART SIGNALS
  SIGNAL ptr : STD_LOGIC_VECTOR(12 DOWNTO 0);
  SIGNAL ptr_reset : STD_LOGIC;
  SIGNAL ptr_increment : STD_LOGIC;
  SIGNAL ptr_decrement : STD_LOGIC;

  --PROGRAM COUNTER SIGNALS
  SIGNAL pc : STD_LOGIC_VECTOR(12 DOWNTO 0);
  SIGNAL pc_increment : STD_LOGIC;
  SIGNAL pc_decrement : STD_LOGIC;

  --CNT SIGNALS
  SIGNAL cnt : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL cnt_increment : STD_LOGIC;
  SIGNAL cnt_decrement : STD_LOGIC;

  --TMP SIGNALS
  SIGNAL tmp : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL load : STD_LOGIC;

  TYPE t_fsm IS (
    state_reset,
    state_enable,
    state_ready,
    state_start_search,
    state_fetch,
    state_decode,
    state_inc_ptr,
    state_dec_ptr,
    state_inc_cell_path,
    state_inc_cell,
    state_inc_cell_done,
    state_dec_cell_path,
    state_dec_cell,
    state_dec_cell_done,
    state_left_brace,
    state_right_brace,

    state_left_brace_check,
    state_right_brace_check,
    state_while_skip_forward,
    state_while_check_end,
    state_while_skip_backward,
    state_while_check_start,

    state_save_tmp,
    state_save_tmp_loaded,
    state_tmp_to_cell,
    state_tmp_to_cell_loaded,
    state_tmp_to_cell_end,
    state_print_cell_set,
    state_print_cell,
    state_read_to_cell_set,
    state_read_to_cell,
    state_wait,
    state_not_vld,
    state_wait_read,
    state_busy,
    state_halt

  );
  SIGNAL curr_state, next_state : t_fsm;
  SIGNAL mx1_selection : STD_LOGIC;
  SIGNAL mx2_selection : STD_LOGIC_VECTOR(1 DOWNTO 0);

BEGIN

  fsm_logic : PROCESS (RESET, CLK, EN)
  BEGIN
    IF RESET = '1' THEN
      curr_state <= state_reset;
    ELSIF clk = '1' AND clk'event THEN
      IF EN = '1' THEN
        curr_state <= next_state;
      END IF;
    END IF;
  END PROCESS;

  final_state_machine : PROCESS (curr_state, OUT_BUSY, IN_VLD, DATA_RDATA, ptr, pc, cnt)
  BEGIN
    cnt_increment <= '0';
    cnt_decrement <= '0';
    ptr_increment <= '0';
    ptr_decrement <= '0';
    pc_decrement <= '0';
    pc_increment <= '0';
    mx2_selection <= "00";
    DATA_RDWR <= '0';
    DATA_EN <= '0';
    IN_REQ <= '0';
    OUT_WE <= '0';
    OUT_DATA <= DATA_RDATA;
    load <= '0';
    OUT_INV <= '0';

    CASE curr_state IS
      WHEN state_reset =>
        READY <= '0';
        DONE <= '0';
        mx1_selection <= '0';
        next_state <= state_enable;
      WHEN state_enable =>
        DATA_EN <= '1';
        mx1_selection <= '0';
        DATA_RDWR <= '1';
        next_state <= state_start_search;
      WHEN state_start_search =>
        IF DATA_RDATA = x"40" THEN
          ptr_increment <= '1';
          next_state <= state_ready;
        ELSE
          ptr_increment <= '1';
          next_state <= state_enable;
        END IF;
      WHEN state_ready =>
        READY <= '1';
        next_state <= state_fetch;
      WHEN state_fetch =>
        mx1_selection <= '1';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        next_state <= state_decode;

      WHEN state_decode =>
        CASE DATA_RDATA IS
          WHEN X"3E" =>
            next_state <= state_inc_ptr;
          WHEN X"3C" =>
            next_state <= state_dec_ptr;
          WHEN X"2B" =>
            next_state <= state_inc_cell_path;
          WHEN X"2D" =>
            next_state <= state_dec_cell_path;
          WHEN X"5B" =>
            next_state <= state_left_brace;
          WHEN X"5D" =>
            next_state <= state_right_brace;
          WHEN X"24" =>
            next_state <= state_save_tmp;
          WHEN X"21" =>
            next_state <= state_tmp_to_cell;
          WHEN X"2E" =>
            next_state <= state_print_cell_set;
          WHEN X"2C" =>
            next_state <= state_read_to_cell_set;
          WHEN X"40" =>
            next_state <= state_halt;
          WHEN OTHERS =>
            pc_increment <= '1';
        END CASE;

      WHEN state_inc_ptr =>
        ptr_increment <= '1';
        pc_increment <= '1';
        next_state <= state_wait;

      WHEN state_dec_ptr =>
        ptr_decrement <= '1';
        pc_increment <= '1';
        next_state <= state_wait;

      WHEN state_inc_cell_path =>
        mx1_selection <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_inc_cell;

      WHEN state_inc_cell =>
        mx2_selection <= "11";
        next_state <= state_inc_cell_done;

      WHEN state_inc_cell_done =>
        mx2_selection <= "11";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        pc_increment <= '1';
        next_state <= state_wait;

      WHEN state_dec_cell_path =>
        mx1_selection <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_dec_cell;

      WHEN state_dec_cell =>
        mx2_selection <= "10";
        next_state <= state_dec_cell_done;

      WHEN state_dec_cell_done =>
        mx2_selection <= "10";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        pc_increment <= '1';
        next_state <= state_wait;

        ------------------------------------------
      WHEN state_left_brace =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mx1_selection <= '0'; -- Set to read from PTR
        pc_increment <= '1'; -- Increment PC to move to the next instruction
        next_state <= state_left_brace_check;

      WHEN state_left_brace_check =>
        -- Check if memory at PTR is zero
        IF DATA_RDATA = X"00" THEN
          -- If zero, begin skipping forward to the first `]`
          next_state <= state_while_skip_forward;
        ELSE
          -- Otherwise, continue normal execution
          next_state <= state_fetch;
        END IF;

      WHEN state_while_skip_forward =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mx1_selection <= '1'; -- Set to read from PC address
        pc_increment <= '1'; -- Move PC forward by 1
        next_state <= state_while_check_end;

      WHEN state_while_check_end =>
        -- Check if the current instruction is `]`
        IF DATA_RDATA = X"5D" THEN -- Found `]`, end of loop
          next_state <= state_fetch; -- Resume normal execution
        ELSE
          -- Otherwise, continue moving forward
          next_state <= state_while_skip_forward;
        END IF;

        -- Handling for `]` instruction when `mem[PTR] != 0`
      WHEN state_right_brace =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mx1_selection <= '0'; -- Set to read from PTR
        next_state <= state_right_brace_check;

      WHEN state_right_brace_check =>
        -- Check if memory at PTR is non-zero
        IF DATA_RDATA /= X"00" THEN
          -- If non-zero, begin skipping backward to the first `[`
          next_state <= state_while_skip_backward;
        ELSE
          -- If zero, continue normal execution
          pc_increment <= '1'; -- Move to the next instruction
          next_state <= state_fetch;
        END IF;

      WHEN state_while_skip_backward =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mx1_selection <= '1'; -- Set to read from PC address
        pc_decrement <= '1'; -- Move PC backward by 1
        next_state <= state_while_check_start;

      WHEN state_while_check_start =>
        -- Check if the current instruction is `[`
        IF DATA_RDATA = X"5B" THEN -- Found `[`, start of loop
          pc_increment <= '1'; -- Move PC to the instruction after `[`
          next_state <= state_fetch; -- Resume normal execution
        ELSE
          -- Otherwise, continue moving backward
          next_state <= state_while_skip_backward;
        END IF;
        ------------------------------------------
      WHEN state_save_tmp =>
        mx1_selection <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_save_tmp_loaded;
      WHEN state_save_tmp_loaded =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        load <= '1';
        pc_increment <= '1';
        next_state <= state_fetch;

      WHEN state_tmp_to_cell =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mx1_selection <= '0';
        next_state <= state_tmp_to_cell_loaded;

      WHEN state_tmp_to_cell_loaded =>
        mx2_selection <= "01";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        pc_increment <= '1';
        next_state <= state_tmp_to_cell_end;

      WHEN state_tmp_to_cell_end =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_fetch;

      WHEN state_print_cell_set =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mx1_selection <= '0';
        next_state <= state_print_cell;

      WHEN state_print_cell =>
        IF OUT_BUSY = '1' THEN
          next_state <= state_busy;
        ELSIF OUT_BUSY = '0' THEN
          OUT_WE <= '1';
          OUT_DATA <= DATA_RDATA;
          pc_increment <= '1';
          next_state <= state_wait;
        END IF;

      WHEN state_read_to_cell_set =>
        mx1_selection <= '0';
        IN_REQ <= '1';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_read_to_cell;

      WHEN state_read_to_cell =>
        IF IN_VLD = '0' THEN
          IN_REQ <= '1';
          next_state <= state_read_to_cell_set;
        ELSE
          DATA_EN <= '1';
          DATA_RDWR <= '0';
          IN_REQ <= '1';
          mx2_selection <= "00";
          pc_increment <= '1';
          next_state <= state_wait_read;
        END IF;

      WHEN state_halt =>
        READY <= '1';
        DONE <= '1';
        next_state <= state_halt;
      WHEN state_wait =>
        next_state <= state_fetch;
      WHEN state_busy =>
        next_state <= state_print_cell;
      WHEN state_not_vld =>
        next_state <= state_read_to_cell_set;

      WHEN state_wait_read =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mx1_selection <= '1';
        next_state <= state_fetch;
    END CASE;
  END PROCESS;

  ------------------------------------------------------------------
  --                      function of cnt                         --
  ------------------------------------------------------------------
  counter : PROCESS (RESET, CLK, cnt_increment, cnt_decrement, cnt)
  BEGIN
    IF RESET = '1' THEN
      cnt <= "00000000";
    ELSIF clk = '1' AND clk'event THEN
      IF cnt_increment = '1' THEN
        cnt <= cnt + 1;
      ELSIF cnt_decrement = '1' THEN
        cnt <= cnt - 1;
      END IF;
    END IF;
  END PROCESS;
  ------------------------------------------------------------------
  --**************************************************************--
  ------------------------------------------------------------------

  ------------------------------------------------------------------
  --                 function of temp register                    --
  ------------------------------------------------------------------
  temporary : PROCESS (RESET, CLK, load)
  BEGIN
    IF RESET = '1' THEN
      tmp <= "00000000";
    ELSIF clk = '1' AND clk'event THEN
      IF load = '1' THEN
        tmp <= DATA_RDATA;
      END IF;
    END IF;
  END PROCESS;
  ------------------------------------------------------------------
  --**************************************************************--
  ------------------------------------------------------------------

  ---------------------------------------------------------------------
  --                 function of pointer counter                     --
  ---------------------------------------------------------------------
  ptr_counter : PROCESS (RESET, CLK, ptr, ptr_increment, ptr_decrement)
  BEGIN
    IF RESET = '1' THEN
      ptr <= "0000000000000";
    ELSIF clk = '1' AND clk'event THEN
      IF ptr_increment = '1' THEN
        ptr <= (ptr + 1) AND MASK;
      END IF;

      IF ptr_decrement = '1' THEN
        ptr <= (ptr - 1) AND MASK;
      END IF;

    END IF;

  END PROCESS;
  ------------------------------------------------------------------
  --**************************************************************--
  ------------------------------------------------------------------

  ----------------------------------------------------------------------
  --                 function of program counter                      --
  ----------------------------------------------------------------------
  program_counter : PROCESS (RESET, CLK, pc, pc_increment, pc_decrement)
  BEGIN
    IF RESET = '1' THEN
      pc <= "0000000000000";
    ELSE
      IF rising_edge(clk) THEN
        IF pc_increment = '1' THEN
          pc <= pc + 1;
        END IF;
        IF pc_decrement = '1' THEN
          pc <= pc - 1;
        END IF;
      END IF;
    END IF;
  END PROCESS;
  ----------------------------------------------------------------------
  --******************************************************************--
  ----------------------------------------------------------------------

  ----------------------------------------------------------------------
  --                 function of first multiplexor                    --
  ----------------------------------------------------------------------
  multiplexor1 : PROCESS (pc, ptr, mx1_selection)
  BEGIN
    IF mx1_selection = '0' THEN
      DATA_ADDR <= ptr;
    ELSIF mx1_selection = '1' THEN
      DATA_ADDR <= pc;
    ELSE
      DATA_ADDR <= "0000000000000";
    END IF;
  END PROCESS;
  ----------------------------------------------------------------------
  --******************************************************************--
  ----------------------------------------------------------------------

  ----------------------------------------------------------------------
  --                 function of second multiplexor                   --
  ----------------------------------------------------------------------
  multiplexor2 : PROCESS (CLK, RESET, mx2_selection)
  BEGIN
    IF (clk'event) AND (clk = '1') THEN
      IF mx2_selection = "00" THEN
        DATA_WDATA <= IN_DATA;
      ELSIF mx2_selection = "01" THEN
        DATA_WDATA <= tmp;
      ELSIF mx2_selection = "10" THEN
        DATA_WDATA <= DATA_RDATA - 1;
      ELSIF mx2_selection = "11" THEN
        DATA_WDATA <= DATA_RDATA + 1;
      ELSE
      END IF;
    END IF;
  END PROCESS;
  ----------------------------------------------------------------------
  --******************************************************************--
  ----------------------------------------------------------------------

  -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
  --   - nelze z vice procesu ovladat stejny signal,
  --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
  --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
  --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly. 

END behavioral;