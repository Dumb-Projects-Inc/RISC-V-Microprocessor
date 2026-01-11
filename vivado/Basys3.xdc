set_property IOSTANDARD LVCMOS33 [get_ports *]


# Clock signal
set_property PACKAGE_PIN W5 [get_ports clock]							
	create_clock -add -name sys_clk_pin -period 10.00 -waveform {0 5} [get_ports clock]

# Switches
#set_property PACKAGE_PIN V17 [get_ports {io_sw[0]}]					
#set_property PACKAGE_PIN V16 [get_ports {io_sw[1]}]					
#set_property PACKAGE_PIN W16 [get_ports {io_sw[2]}]					
#set_property PACKAGE_PIN W17 [get_ports {io_sw[3]}]					
#set_property PACKAGE_PIN W15 [get_ports {io_sw[4]}]					
#set_property PACKAGE_PIN V15 [get_ports {io_sw[5]}]					
#set_property PACKAGE_PIN W14 [get_ports {io_sw[6]}]					
#set_property PACKAGE_PIN W13 [get_ports {io_sw[7]}]					
#set_property PACKAGE_PIN V2 [get_ports {io_sw[8]}]					
#set_property PACKAGE_PIN T3 [get_ports {io_sw[9]}]					
#set_property PACKAGE_PIN T2 [get_ports {io_sw[10]}]					
#set_property PACKAGE_PIN R3 [get_ports {io_sw[11]}]					
#set_property PACKAGE_PIN W2 [get_ports {io_sw[12]}]					
#set_property PACKAGE_PIN U1 [get_ports {io_sw[13]}]					
#set_property PACKAGE_PIN T1 [get_ports {io_sw[14]}]					
#set_property PACKAGE_PIN R2 [get_ports {io_sw[15]}]

# LEDs
set_property PACKAGE_PIN U16 [get_ports {io_LED[0]}]					
set_property PACKAGE_PIN E19 [get_ports {io_LED[1]}]					
set_property PACKAGE_PIN U19 [get_ports {io_LED[2]}]					
set_property PACKAGE_PIN V19 [get_ports {io_LED[3]}]					
set_property PACKAGE_PIN W18 [get_ports {io_LED[4]}]					
set_property PACKAGE_PIN U15 [get_ports {io_LED[5]}]					
set_property PACKAGE_PIN U14 [get_ports {io_LED[6]}]					
set_property PACKAGE_PIN V14 [get_ports {io_LED[7]}]					
set_property PACKAGE_PIN V13 [get_ports {io_LED[8]}]					
set_property PACKAGE_PIN V3 [get_ports {io_LED[9]}]					
set_property PACKAGE_PIN W3 [get_ports {io_LED[10]}]					
set_property PACKAGE_PIN U3 [get_ports {io_LED[11]}]					
set_property PACKAGE_PIN P3 [get_ports {io_LED[12]}]					
set_property PACKAGE_PIN N3 [get_ports {io_LED[13]}]					
set_property PACKAGE_PIN P1 [get_ports {io_LED[14]}]					
set_property PACKAGE_PIN L1 [get_ports {io_LED[15]}]

# Buttons
#set_property PACKAGE_PIN U18 [get_ports io_btnC]
set_property PACKAGE_PIN U18 [get_ports reset]                    
#set_property PACKAGE_PIN U17 [get_ports io_btnU]
#set_property PACKAGE_PIN V18 [get_ports io_btnL]
#set_property PACKAGE_PIN W19 [get_ports io_btnR]
#set_property PACKAGE_PIN V20 [get_ports io_btnD]

##USB-RS232 Interface (UART)		
#set_property PACKAGE_PIN A18 [get_ports io_txd]
#set_property PACKAGE_PIN B18 [get_ports io_rxd]	