import serial
import serial.tools.list_ports
import time
from prompt_toolkit import prompt
from prompt_toolkit.completion import PathCompleter


# small utility that acts as a UART terminal as well as detecting when the processor is asking for a program
def load_program(ser: serial.Serial, filepath: str):
    with open(filepath, "rb") as f:
        program_data = f.read()
    program_size = len(program_data)
    print(f"Loading program of size {program_size} bytes...")

    # Clear any pending output from BIOS (like the address print or newlines)
    # to ensure our first read() gets the specific handshake byte
    time.sleep(0.1)
    ser.reset_input_buffer()

    start_time = time.time()
    
    for byte in program_data:
        ser.write(bytes([byte]))
        time.sleep(0.02)  # small delay to avoid overwhelming the UART

    
    
    # Send IMDONE. The BIOS will ACK these characters too.
    for char in b'IMDONE':
        ser.write(bytes([char]))
        time.sleep(0.02)

    duration = time.time() - start_time
    print(f"Program loaded successfully in {duration:.2f}s.")
    ser.reset_input_buffer()

def main():
    print("Cougar Terminal Utility")
    print("Found UART devices:")
    device_list = []
    for i, port in enumerate(serial.tools.list_ports.comports()):
        print(f"[{i}] - {port.device}: {port.description}")
        device_list.append(port.device)
    print("Select FPGA UART port to connect to:")
    port_index = int(input("Port: "))
    port_name = device_list[port_index]
    baud_rate = int(input("Enter baud rate (e.g., 115200): "))
    with serial.Serial(port_name, baud_rate, timeout=1, stopbits=2) as ser:
        print(f"Connected to {port_name} at {baud_rate} baud.")
        while True:
            try:
                line = ser.readline().decode('utf-8').strip()
            except UnicodeDecodeError:
                print("Received undecodable bytes, skipping...")
                continue
            if line == "":
                continue
            if line.startswith("Load program"):  # assuming 0x7f is the request byte for loading a program
                print("Program load request detected.")
                filepath = prompt("Enter path to program binary: ", completer=PathCompleter())
                load_program(ser, filepath)
            elif line.startswith("Breakpoint"):
                dump_or_continue = input("Breakpoint hit. Enter 'd' to dump memory or 'c' to continue: ")
                if dump_or_continue.lower() == 'd':
                    ser.write(b'd')  # send dump command
                    print("Memory dump command sent.")
                    ser.write(b'c')
                else:
                    ser.write(b'c')  # send continue command
                    print("Continue command sent.")

            else:
                print(line, flush=True)

if __name__ == "__main__":
    main()