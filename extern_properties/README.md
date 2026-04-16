# Assertion Properties Library

## Overview

This directory contains a collection of SystemVerilog assertion properties used to verify external user modules against communication protocols generated from Anvil channels.

These properties focus on:
- Handshake correctness (valid/ack interactions)
- Signal assertion and deassertion timing
- Data lifetime and stability guarantees

They are automatically included in generated assertion wrappers during the `assert` flow and are intended to be generic across different channel synchronization modes.

---

## Available Properties

### Handshake Behavior

#### `ack_low_when_valid_high`
Ensures that the acknowledgment (`ack`) signal is not asserted at the moment the valid signal is raised.

#### `valid_high_until_ack_high`
Ensures that the valid signal remains asserted until acknowledgment is received.

#### `ack_high_valid_low`
Ensures that the valid signal is deasserted after acknowledgment is observed.

#### `wait_req_ack_low`
Ensures that the valid signal stays asserted while acknowledgment is still low in the wait-request phase.

---

### Acknowledgment Control

#### `ack_low_after_handshake`
Ensures that the acknowledgment signal is deasserted after the handshake completes.

#### `ack_low_during_DROP_ACK`
Ensures that the acknowledgment signal remains low during the `DROP_ACK` phase of the protocol FSM.

---

### Valid Signal Control

#### `valid_low_during_DROP_VALID`
Ensures that the valid signal remains low during the `DROP_VALID` phase of the protocol FSM.

---

### Data Stability / Lifetime

#### `data_stable_valid_high`
Ensures that the data signal remains stable while valid is asserted and acknowledgment has not yet been received.

#### `data_stable_N_cycles_after_ack_high`
Ensures that the data signal remains stable for `N` cycles after acknowledgment is asserted, where `N` is defined by the channel lifetime contract.

#### `data_stable_N_cycles`
Ensures that the data signal remains stable for `N` cycles after acknowledgment (or during the protected post-event window), based on the FSM state and lifetime rules.

---

## Notes

- `N` represents the number of cycles required by the channel lifetime contract (e.g., `@#N` in Anvil).
- These properties are designed to be **generic** and work with automatically generated wrappers.
- Depending on the channel type:
  - Some properties apply only when the user controls `valid`
  - Some apply only when the user controls `ack`

These properties are included via one of the following `.svh` files, depending on the channel type and whether the user module is acting as the sender or receiver:

```systemverilog
`include "nosyn_user_sender.svh"
`include "nosyn_user_receiver.svh"
`include "syn_recv_dynamic_user_sender.svh"
`include "syn_recv_dynamic_user_receiver.svh"
`include "syn_send_dynamic_user_sender.svh"
`include "syn_send_dynamic_user_receiver.svh"
```

--- 

## Usage

To run the assertion flow, go to the `examples` directory.

First, generate the Anvil SystemVerilog file:

```bash
make MODULE_NAME=<module>
```

Then run the assertion flow with:

```bash
make assert MODULE_NAME=<module> USER_SV_MODULE=<user_module>
```

---

## Examples

You can try the assertion flow using the provided example designs in the `examples` directory. There are 6 examples, with filenames in the format:

- `extern_exampleN_sender.anvil` / `extern_exampleN_receiver.sv`
- or `extern_exampleN_receiver.anvil` / `extern_exampleN_sender.sv`

For example:

```bash
make MODULE_NAME=extern_example1_sender
make assert MODULE_NAME=extern_example1_sender USER_SV_MODULE=extern_example1_receiver
```