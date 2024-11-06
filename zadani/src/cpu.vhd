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
  SIGNAL is_zero : STD_LOGIC;

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
  SIGNAL tmp_in : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL tmp_out : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL tmp : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL load : STD_LOGIC;

  --
  SIGNAL acc_data : STD_LOGIC_VECTOR(7 DOWNTO 0);

  TYPE t_fsm IS (
    sinit

  );
  SIGNAL curr_state, next_state : t_fsm;
  SIGNAL mx1_selection : STD_LOGIC;
  SIGNAL mx2_selection : STD_LOGIC_VECTOR(1 DOWNTO 0);

BEGIN

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
  temporary : PROCESS (RESET, CLK, load, tmp_in, tmp_out)
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
        IF ptr = "1111111111111" THEN
          ptr <= "0000000000000";
        ELSE
          ptr <= ptr + 1;
        END IF;
      END IF;

      IF ptr_decrement = '1' THEN
        IF ptr = "0000000000000" THEN
          ptr <= "1111111111111";
        ELSE
          ptr <= ptr - 1;
        END IF;
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
      DATA_RDATA <= ptr;
    ELSIF mx1_selection = '1' THEN
      DATA_RDATA <= pc;
    ELSE
      DATA_RDATA <= "0000000000000";
    END IF;
  END PROCESS;
  ----------------------------------------------------------------------
  --******************************************************************--
  ----------------------------------------------------------------------

  ----------------------------------------------------------------------
  --                 function of second multiplexor                   --
  ----------------------------------------------------------------------
  multiplexor2 : PROCESS (IN_DATA, tmp, mx2_selection)
  BEGIN
    IF clk = '1' AND clk'event THEN
      CASE mx2_selection IS
        WHEN "00" => DATA_WDATA <= IN_DATA;
        WHEN "01" => DATA_WDATA <= tmp;
        WHEN "10" => DATA_WDATA <= DATA_RDATA - 1;
        WHEN "11" => DATA_WDATA <= DATA_RDATA + 1;
      END CASE;
    END IF;
  END PROCESS;
  ----------------------------------------------------------------------
  --******************************************************************--
  ----------------------------------------------------------------------

  PROCESS (clk, ptr_reset)
  BEGIN
    IF ptr_reset = '1' THEN
      ptr <= "0000000000000";
    ELSIF rising_edge(clk) THEN
      IF ptr_increment = '1' THEN
        ptr <= ptr + 1;
      END IF;
    END IF;
  END PROCESS;
  DATA_ADDR <= ptr;

  --is_zero
  is_zero <= '1' WHEN cnt = 0 ELSE
    '0';

  -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
  --   - nelze z vice procesu ovladat stejny signal,
  --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
  --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
  --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly. 

END behavioral;