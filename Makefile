### Nosyn User Receiver ###
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification/example_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification/example_sender.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification/example_receiver.sv

### Sender Dynamic user Sender ###
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification/syn1_example_receiver.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification/syn1_example_receiver.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification/syn1_example_sender.sv

### Sender Dynamic user Receiver ###
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification/syn1_example1_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification/syn1_example1_sender.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification/syn1_example1_receiver.sv

### Recv Dynamic user Sender ###
ANVIL_FILE := /home/daoen/anvil_new/examples/verification/syn2_example_receiver.anvil
SV_ANVIL   := /home/daoen/anvil_new/examples/verification/syn2_example_receiver.sv
SV_USER    := /home/daoen/anvil_new/examples/verification/syn2_example_sender.sv

### Recv Dynamic user Receiver ###
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification/syn2_example1_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification/syn2_example1_sender.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification/syn2_example1_receiver.sv

.PHONY: assert
assert:
	dune exec ./bin/main.exe -- assert $(ANVIL_FILE) $(SV_ANVIL) $(SV_USER)