### Nosyn User Receiver ###
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification/example_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification/example_sender.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification/example_receiver.sv
# verilator -Wall --cc --exe --assert --trace --build -I/home/daoen/anvil_new/properties  --top-module example_sender_assert example_sender.sv example_receiver.sv example_sender_assert.sv example_main.cpp -o sim.out -Wno-UNDRIVEN -Wno-UNUSED

### Sender Dynamic user Sender ###
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification/syn1_example_receiver.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification/syn1_example_receiver.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification/syn1_example_sender.sv

### Sender Dynamic user Receiver ###
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification/syn1_example1_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification/syn1_example1_sender.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification/syn1_example1_receiver.sv

### Recv Dynamic user Sender ###
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification/syn2_example_receiver.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification/syn2_example_receiver.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification/syn2_example_sender.sv

### Recv Dynamic user Receiver ###
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification/syn2_example1_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification/syn2_example1_sender.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification/syn2_example1_receiver.sv

############### NO SYN ####################
############## User Receiver ##############
############## should_pass #################
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_pass/example1_anvil_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_pass/example1_anvil_sender.anvil.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_pass/example1_user_receiver.sv
# OUTPUT     := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_pass/example1_anvil_sender_assert.sv
# TOP_MODULE := example1_anvil_sender_assert
# verilator -Wall --binary --exe --assert --trace --build -I/home/daoen/anvil_new/properties --timing --top-module example1_anvil_sender_assert example1_anvil_sender.anvil.sv example1_user_receiver.sv example1_anvil_sender_assert.sv -o sim.out -Wno-UNDRIVEN -Wno-UNUSED

############## should_fail - property 1 #################
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_fail/ack_low_when_valid_high_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_fail/ack_low_when_valid_high_sender.anvil.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_fail/ack_low_when_valid_high_receiver.sv
# OUTPUT     := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_fail/ack_low_when_valid_high_sender_assert.sv
# TOP_MODULE := ack_low_when_valid_high_sender_assert

############## should_fail - property 2 #################
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_fail/ack_low_after_handshake_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_fail/ack_low_after_handshake_sender.anvil.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_fail/ack_low_after_handshake_receiver.sv
# OUTPUT     := /home/daoen/anvil_new/examples/verification_new/no_syn/user_receiver_should_fail/ack_low_after_handshake_sender_assert.sv
# TOP_MODULE := ack_low_after_handshake_sender_assert


############# COMPLEX ##############
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification_new/complex/example1_complex_anvil_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification_new/complex/example1_complex_anvil_sender.anvil.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification_new/complex/example1_complex_user_receiver.sv
# OUTPUT     := /home/daoen/anvil_new/examples/verification_new/complex/example1_complex_anvil_sender_assert.sv
# TOP_MODULE := example1_complex_anvil_sender_assert

# ANVIL_FILE := /home/daoen/anvil_new/examples/anvil_verification/should_pass/test1/example1_anvil_sender.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/anvil_verification/should_pass/test1/example1_anvil_sender.anvil.sv
# SV_USER    := /home/daoen/anvil_new/examples/anvil_verification/should_pass/test1/example1_user_receiver.sv
# OUTPUT     := /home/daoen/anvil_new/examples/anvil_verification/should_pass/test1/example1_anvil_sender_assert.sv
# TOP_MODULE := example1_complex_anvil_sender_assert


############### NO SYN ####################
############## User Sender ##############
############## should_pass #################
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/example1_anvil_receiver.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/example1_anvil_receiver.anvil.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/example1_user_sender.sv
# OUTPUT     := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/example1_anvil_receiver_assert.sv
# TOP_MODULE := example1_anvil_receiver_assert

############## should_fail #################
# ANVIL_FILE := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/data_stable_N_cycles_after_ack_high_receiver.anvil
# SV_ANVIL   := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/data_stable_N_cycles_after_ack_high_receiver.anvil.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/data_stable_N_cycles_after_ack_high_sender.sv
# OUTPUT     := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/data_stable_N_cycles_after_ack_high_receiver_assert.sv
# TOP_MODULE := data_stable_N_cycles_after_ack_high_receiver_assert

ANVIL_FILE := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/example1_anvil_receiver.anvil
SV_ANVIL   := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/example1_anvil_receiver.anvil.sv
# SV_USER    := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/ack_high_valid_low_sender.sv
# OUTPUT     := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/ack_high_valid_low_sender_assert.sv
# TOP_MODULE := ack_high_valid_low_sender_assert

SV_USER    := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/valid_high_until_ack_high_sender.sv
OUTPUT     := /home/daoen/anvil_new/examples/verification_new/no_syn/user_sender_should_pass/valid_high_until_ack_high_sender_assert.sv
TOP_MODULE := valid_high_until_ack_high_sender_assert

.PHONY: assert
assert:
	dune exec ./bin/main.exe -- assert $(ANVIL_FILE) $(SV_ANVIL) $(SV_USER) > $(OUTPUT)

.PHONY: run
run:
	verilator -Wall --binary -exe --assert --trace --build -I/home/daoen/anvil_new/properties --timing --top-module $(TOP_MODULE) $(SV_USER) $(SV_ANVIL) $(OUTPUT) -o sim.out -Wno-UNDRIVEN -Wno-UNUSED

.PHONY: build
build:
	./obj_dir/sim.out