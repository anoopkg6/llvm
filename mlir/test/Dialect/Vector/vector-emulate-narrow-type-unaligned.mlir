// RUN: mlir-opt --test-emulate-narrow-int="arith-compute-bitwidth=1 memref-load-bitwidth=8" --cse --split-input-file %s | FileCheck %s

// TODO: remove memref.alloc() in the tests to eliminate noises.
// memref.alloc exists here because sub-byte vector data types such as i2
// are currently not supported as input arguments.


func.func @vector_load_i2() -> vector<3x3xi2> {
  %0 = memref.alloc() : memref<3x3xi2>
  %c0 = arith.constant 0 : index
  %c2 = arith.constant 2 : index
  %cst = arith.constant dense<0> : vector<3x3xi2>
  %1 = vector.load %0[%c2, %c0] : memref<3x3xi2>, vector<3xi2>
  %2 = vector.insert %1, %cst [0] : vector<3xi2> into vector<3x3xi2>
  return %2 : vector<3x3xi2>
}

// CHECK-LABEL: func @vector_load_i2
// CHECK: %[[ALLOC:.+]] = memref.alloc() : memref<3xi8>
// CHECK: %[[INDEX:.+]] = arith.constant 1 : index
// CHECK: %[[VEC:.+]] = vector.load %[[ALLOC]][%[[INDEX]]] : memref<3xi8>, vector<2xi8>
// CHECK: %[[VEC_I2:.+]] = vector.bitcast %[[VEC]] : vector<2xi8> to vector<8xi2>
// CHECK: %[[EXCTRACT:.+]] = vector.extract_strided_slice %[[VEC_I2]] {offsets = [2], sizes = [3], strides = [1]} : vector<8xi2> to vector<3xi2>

// -----

func.func @vector_transfer_read_i2() -> vector<3xi2> {
  %0 = memref.alloc() : memref<3x3xi2>
  %pad = arith.constant 0 : i2
  %c0 = arith.constant 0 : index
  %c2 = arith.constant 2 : index
  %1 = vector.transfer_read %0[%c2, %c0], %pad {in_bounds = [true]} : memref<3x3xi2>, vector<3xi2>
  return %1 : vector<3xi2>
}

// CHECK-LABEL: func @vector_transfer_read_i2
// CHECK: %[[ALLOC:.+]] = memref.alloc() : memref<3xi8>
// CHECK: %[[INDEX:.+]] = arith.constant 1 : index
// CHECK: %[[READ:.+]] = vector.transfer_read %[[ALLOC]][%[[INDEX]]], %0 : memref<3xi8>, vector<2xi8>
// CHECK: %[[BITCAST:.+]] = vector.bitcast %[[READ]] : vector<2xi8> to vector<8xi2>
// CHECK: vector.extract_strided_slice %[[BITCAST]] {offsets = [2], sizes = [3], strides = [1]} : vector<8xi2> to vector<3xi2>

// -----

func.func @vector_cst_maskedload_i2(%passthru: vector<5xi2>) -> vector<3x5xi2> {
  %0 = memref.alloc() : memref<3x5xi2>
  %cst = arith.constant dense<0> : vector<3x5xi2>
  %mask = vector.constant_mask [3] : vector<5xi1>
  %c0 = arith.constant 0 : index
  %c2 = arith.constant 2 : index
  %1 = vector.maskedload %0[%c2, %c0], %mask, %passthru :
    memref<3x5xi2>, vector<5xi1>, vector<5xi2> into vector<5xi2>
  %2 = vector.insert %1, %cst [0] : vector<5xi2> into vector<3x5xi2>
  return %2 : vector<3x5xi2>
}

// CHECK-LABEL: func @vector_cst_maskedload_i2(
// CHECK-SAME: %[[ARG0:.+]]: vector<5xi2>) -> vector<3x5xi2>
// CHECK: %[[ORIGINMASK:.+]] = vector.constant_mask [3] : vector<5xi1>
// CHECK: %[[NEWMASK:.+]] = arith.constant dense<true> : vector<2xi1>
// CHECK: %[[VESSEL:.+]] = arith.constant dense<0> : vector<8xi2>
// CHECK: %[[INSERT1:.+]] = vector.insert_strided_slice %[[ARG0]], %[[VESSEL]]
// CHECK-SAME: {offsets = [2], strides = [1]} : vector<5xi2> into vector<8xi2>
// CHECK: %[[BITCAST1:.+]] = vector.bitcast %[[INSERT1]] : vector<8xi2> to vector<2xi8>
// CHECK: %[[C2:.+]] = arith.constant 2 : index
// CHECK: %[[MASKEDLOAD:.+]] = vector.maskedload %alloc[%[[C2]]], %[[NEWMASK:.+]], %[[BITCAST1]]
// CHECK-SAME: : memref<4xi8>, vector<2xi1>, vector<2xi8> into vector<2xi8>
// CHECK: %[[BITCAST2:.+]] = vector.bitcast %[[MASKEDLOAD]] : vector<2xi8> to vector<8xi2>
// CHECK: %[[CST2:.+]] = arith.constant dense<false> : vector<8xi1>
// CHECK: %[[INSERT2:.+]] = vector.insert_strided_slice %[[ORIGINMASK]], %[[CST2]]
// CHECK-SAME: {offsets = [2], strides = [1]} : vector<5xi1> into vector<8xi1>
// CHECK: %[[SELECT:.+]] = arith.select %[[INSERT2]], %[[BITCAST2]], %[[INSERT1]] : vector<8xi1>, vector<8xi2>
// CHECK: vector.extract_strided_slice %[[SELECT]] {offsets = [2], sizes = [5], strides = [1]} : vector<8xi2> to vector<5xi2>

// -----

func.func @vector_load_i2_dynamic_indexing(%idx1: index, %idx2: index) -> vector<3xi2> {
  %0 = memref.alloc() : memref<3x3xi2>
  %cst = arith.constant dense<0> : vector<3x3xi2>
  %1 = vector.load %0[%idx1, %idx2] : memref<3x3xi2>, vector<3xi2>
  return %1 : vector<3xi2>
}

// CHECK: #[[MAP:.+]] = affine_map<()[s0, s1] -> ((s0 * 3 + s1) floordiv 4)>
// CHECK: #[[MAP1:.+]] = affine_map<()[s0, s1] -> ((s0 * 3 + s1) mod 4)>
// CHECK: func @vector_load_i2_dynamic_indexing(
// CHECK-SAME: %[[ARG0:.+]]: index, %[[ARG1:.+]]: index) -> vector<3xi2>
// CHECK: %[[ALLOC:.+]]= memref.alloc() : memref<3xi8>
// CHECK: %[[LOADADDR1:.+]] = affine.apply #[[MAP]]()[%[[ARG0]], %[[ARG1]]]
// CHECK: %[[LOADADDR2:.+]] = affine.apply #[[MAP1]]()[%[[ARG0]], %[[ARG1]]]
// CHECK: %[[EMULATED_LOAD:.+]] = vector.load %alloc[%[[LOADADDR1]]] : memref<3xi8>, vector<2xi8>
// CHECK: %[[BITCAST:.+]] = vector.bitcast %[[EMULATED_LOAD]] : vector<2xi8> to vector<8xi2>
// CHECK: %[[ZERO:.+]] = arith.constant dense<0> : vector<3xi2>
// CHECK: %[[EXTRACT:.+]] = vector.extract %[[BITCAST]][%[[LOADADDR2]]] : i2 from vector<8xi2>
// CHECK: %[[C1:.+]] = arith.constant 1 : index
// CHECK: %[[OFFSET:.+]] = arith.addi %[[LOADADDR2]], %[[C1]] : index
// CHECK: %[[EXTRACT2:.+]] = vector.extract %[[BITCAST]][%[[OFFSET]]] : i2 from vector<8xi2>
// CHECK: %[[C2:.+]] = arith.constant 2 : index
// CHECK: %[[OFFSET2:.+]] = arith.addi %1, %c2 : index
// CHECK: %[[EXTRACT3:.+]] = vector.extract %[[BITCAST]][%[[OFFSET2]]] : i2 from vector<8xi2>

// -----

func.func @vector_load_i2_dynamic_indexing_mixed(%idx: index) -> vector<3xi2> {
  %0 = memref.alloc() : memref<3x3xi2>
  %c2 = arith.constant 2 : index
  %cst = arith.constant dense<1> : vector<3x3xi2>
  %1 = vector.load %0[%idx, %c2] : memref<3x3xi2>, vector<3xi2>
  return %1 : vector<3xi2>
}

// CHECK: #[[MAP:.+]] = affine_map<()[s0] -> ((s0 * 3 + 2) floordiv 4)>
// CHECK: #[[MAP1:.+]] = affine_map<()[s0] -> (s0 * 3 - ((s0 * 3 + 2) floordiv 4) * 4 + 2)>
// CHECK: func @vector_load_i2_dynamic_indexing_mixed(
// CHECK-SAME: %[[ARG0:.+]]: index) -> vector<3xi2>
// CHECK: %[[ALLOC:.+]]= memref.alloc() : memref<3xi8>
// CHECK: %[[LOADADDR1:.+]] = affine.apply #[[MAP]]()[%[[ARG0]]]
// CHECK: %[[LOADADDR2:.+]] = affine.apply #[[MAP1]]()[%[[ARG0]]]
// CHECK: %[[EMULATED_LOAD:.+]] = vector.load %alloc[%[[LOADADDR1]]] : memref<3xi8>, vector<2xi8>
// CHECK: %[[BITCAST:.+]] = vector.bitcast %[[EMULATED_LOAD]] : vector<2xi8> to vector<8xi2>
// CHECK: %[[ZERO:.+]] = arith.constant dense<0> : vector<3xi2>
// CHECK: %[[EXTRACT:.+]] = vector.extract %[[BITCAST]][%[[LOADADDR2]]] : i2 from vector<8xi2>
// CHECK: %[[C1:.+]] = arith.constant 1 : index
// CHECK: %[[OFFSET:.+]] = arith.addi %[[LOADADDR2]], %[[C1]] : index
// CHECK: %[[EXTRACT2:.+]] = vector.extract %[[BITCAST]][%[[OFFSET]]] : i2 from vector<8xi2>
// CHECK: %[[C2:.+]] = arith.constant 2 : index
// CHECK: %[[OFFSET2:.+]] = arith.addi %1, %c2 : index
// CHECK: %[[EXTRACT3:.+]] = vector.extract %[[BITCAST]][%[[OFFSET2]]] : i2 from vector<8xi2>

// -----

func.func @vector_transfer_read_i2_dynamic_indexing(%idx1: index, %idx2: index) -> vector<3xi2> {
  %0 = memref.alloc() : memref<3x3xi2>
  %pad = arith.constant 0 : i2
  %1 = vector.transfer_read %0[%idx1, %idx2], %pad {in_bounds = [true]} : memref<3x3xi2>, vector<3xi2>
  return %1 : vector<3xi2>
}

// CHECK: #[[MAP:.+]] = affine_map<()[s0, s1] -> ((s0 * 3 + s1) floordiv 4)>
// CHECK: #[[MAP1:.+]] = affine_map<()[s0, s1] -> ((s0 * 3 + s1) mod 4)>
// CHECK: func @vector_transfer_read_i2_dynamic_indexing(
// CHECK-SAME: %[[ARG0:.+]]: index, %[[ARG1:.+]]: index) -> vector<3xi2>
// CHECK: %[[ALLOC:.+]] = memref.alloc() : memref<3xi8>
// CHECK: %[[C0:.+]] = arith.extui %c0_i2 : i2 to i8
// CHECK: %[[LOADADDR1:.+]] = affine.apply #[[MAP]]()[%[[ARG0]], %[[ARG1]]]
// CHECK: %[[LOADADDR2:.+]] = affine.apply #[[MAP1]]()[%[[ARG0]], %[[ARG1]]]
// CHECK: %[[READ:.+]] = vector.transfer_read %[[ALLOC]][%[[LOADADDR1]]], %[[C0]] : memref<3xi8>, vector<2xi8>
// CHECK: %[[BITCAST:.+]] = vector.bitcast %[[READ]] : vector<2xi8> to vector<8xi2>
// CHECK: %[[CST:.+]] = arith.constant dense<0> : vector<3xi2>
// CHECK: %[[EXTRACT:.+]] = vector.extract %[[BITCAST]][%[[LOADADDR2]]] : i2 from vector<8xi2>
// CHECK: %[[C1:.+]] = arith.constant 1 : index
// CHECK: %[[ADDI:.+]] = arith.addi %[[LOADADDR2]], %[[C1]] : index
// CHECK: %[[EXTRACT2:.+]] = vector.extract %[[BITCAST]][%[[ADDI]]] : i2 from vector<8xi2>
// CHECK: %[[C2:.+]] = arith.constant 2 : index
// CHECK: %[[ADDI2:.+]] = arith.addi %[[LOADADDR2]], %[[C2]] : index
// CHECK: %[[EXTRACT3:.+]] = vector.extract %[[BITCAST]][%[[ADDI2]]] : i2 from vector<8xi2>

// -----

func.func @vector_transfer_read_i2_dynamic_indexing_mixed(%idx1: index) -> vector<3xi2> {
  %0 = memref.alloc() : memref<3x3xi2>
  %c2 = arith.constant 2 : index
  %pad = arith.constant 0 : i2
  %1 = vector.transfer_read %0[%idx1, %c2], %pad {in_bounds = [true]} : memref<3x3xi2>, vector<3xi2>
  return %1 : vector<3xi2>
}

// CHECK: #[[MAP:.+]] = affine_map<()[s0] -> ((s0 * 3 + 2) floordiv 4)>
// CHECK: #[[MAP1:.+]] = affine_map<()[s0] -> (s0 * 3 - ((s0 * 3 + 2) floordiv 4) * 4 + 2)>
// CHECK: func @vector_transfer_read_i2_dynamic_indexing_mixed(
// CHECK-SAME: %[[ARG0:.+]]: index) -> vector<3xi2>
// CHECK: %[[ALLOC:.+]] = memref.alloc() : memref<3xi8>
// CHECK: %[[C0:.+]] = arith.extui %c0_i2 : i2 to i8
// CHECK: %[[LOADADDR1:.+]] = affine.apply #[[MAP]]()[%[[ARG0]]]
// CHECK: %[[LOADADDR2:.+]] = affine.apply #[[MAP1]]()[%[[ARG0]]]
// CHECK: %[[READ:.+]] = vector.transfer_read %[[ALLOC]][%[[LOADADDR1]]], %[[C0]] : memref<3xi8>, vector<2xi8>
// CHECK: %[[BITCAST:.+]] = vector.bitcast %[[READ]] : vector<2xi8> to vector<8xi2>
// CHECK: %[[CST:.+]] = arith.constant dense<0> : vector<3xi2>
// CHECK: %[[EXTRACT:.+]] = vector.extract %[[BITCAST]][%[[LOADADDR2]]] : i2 from vector<8xi2>
// CHECK: %[[C1:.+]] = arith.constant 1 : index
// CHECK: %[[ADDI:.+]] = arith.addi %[[LOADADDR2]], %[[C1]] : index
// CHECK: %[[EXTRACT2:.+]] = vector.extract %[[BITCAST]][%[[ADDI]]] : i2 from vector<8xi2>
// CHECK: %[[C2:.+]] = arith.constant 2 : index
// CHECK: %[[ADDI2:.+]] = arith.addi %[[LOADADDR2]], %[[C2]] : index
// CHECK: %[[EXTRACT3:.+]] = vector.extract %[[BITCAST]][%[[ADDI2]]] : i2 from vector<8xi2>
// -----

func.func @vector_maskedload_i2_dynamic_indexing_mixed(%passthru: vector<3xi2>, %idx: index) -> vector<3xi2> {
  %0 = memref.alloc() : memref<3x3xi2>
  %cst = arith.constant dense<0> : vector<3x3xi2>
  %c2 = arith.constant 2 : index
  %mask = vector.constant_mask [3] : vector<3xi1>
  %1 = vector.maskedload %0[%idx, %c2], %mask, %passthru :
    memref<3x3xi2>, vector<3xi1>, vector<3xi2> into vector<3xi2>
  return %1 : vector<3xi2>
}

// CHECK: #[[MAP:.+]] = affine_map<()[s0] -> ((s0 * 3 + 2) floordiv 4)>
// CHECK: #[[MAP1:.+]] = affine_map<()[s0] -> (s0 * 3 - ((s0 * 3 + 2) floordiv 4) * 4 + 2)>
// CHECK: func @vector_maskedload_i2_dynamic_indexing_mixed(
// CHECK-SAME: %[[PTH:.+]]: vector<3xi2>, %[[IDX:.+]]: index) -> vector<3xi2>
// CHECK: %[[ALLOC:.+]] = memref.alloc() : memref<3xi8>
// CHECK: %[[MASK:.+]] = vector.constant_mask [3] : vector<3xi1>
// CHECK: %[[LINEAR1:.+]] = affine.apply #map()[%[[IDX]]]
// CHECK: %[[LINEAR2:.+]] = affine.apply #map1()[%[[IDX]]]
// CHECK: %[[ONE:.+]] = arith.constant dense<true> : vector<2xi1>
// CHECK: %[[ZERO:.+]] = arith.constant dense<0> : vector<8xi2>

// Extract passthru vector, and insert into zero vector, this is for constructing a new passthru
// CHECK: %[[EX1:.+]] = vector.extract %[[PTH]][0] : i2 from vector<3xi2>
// CHECK: %[[IN1:.+]] = vector.insert %[[EX1]], %[[ZERO]] [%[[LINEAR2]]] : i2 into vector<8xi2>
// CHECK: %[[C1:.+]] = arith.constant 1 : index
// CHECK: %[[INCIDX:.+]] = arith.addi %[[LINEAR2]], %[[C1]] : index
// CHECK: %[[EX2:.+]] = vector.extract %[[PTH]][1] : i2 from vector<3xi2>
// CHECK: %[[IN2:.+]] = vector.insert %[[EX2]], %[[IN1]] [%[[INCIDX]]] : i2 into vector<8xi2>
// CHECK: %[[C2:.+]] = arith.constant 2 : index
// CHECK: %[[INCIDX2:.+]] = arith.addi %[[LINEAR2]], %[[C2]] : index
// CHECK: %[[EX3:.+]] = vector.extract %[[PTH]][2] : i2 from vector<3xi2>
// CHECK: %[[NEW_PASSTHRU:.+]] = vector.insert %[[EX3]], %[[IN2]] [%[[INCIDX2]]] : i2 into vector<8xi2>

// Bitcast the new passthru vector to emulated i8 vector
// CHECK: %[[BCAST_PASSTHRU:.+]] = vector.bitcast %[[NEW_PASSTHRU]] : vector<8xi2> to vector<2xi8>

// Use the emulated i8 vector for masked load from the source memory
// CHECK: %[[SOURCE:.+]] = vector.maskedload %[[ALLOC]][%[[LINEAR1]]], %[[ONE]], %[[BCAST_PASSTHRU]]
// CHECK-SAME: memref<3xi8>, vector<2xi1>, vector<2xi8> into vector<2xi8>

// Bitcast back to i2 vector
// CHECK: %[[BCAST_MASKLOAD:.+]] = vector.bitcast %[[SOURCE]] : vector<2xi8> to vector<8xi2>

// CHECK: %[[CST1:.+]] = arith.constant dense<false> : vector<8xi1>

// Create a mask vector 
// Note that if indices are known then we can fold the part generating mask.
// CHECK: %[[EX4:.+]] = vector.extract %[[MASK]][0] : i1 from vector<3xi1>
// CHECK: %[[IN4:.+]] = vector.insert %[[EX4]], %[[CST1]] [%[[LINEAR2]]] : i1 into vector<8xi1>
// CHECK: %[[EX5:.+]] = vector.extract %[[MASK]][1] : i1 from vector<3xi1>
// CHECK: %[[IN5:.+]] = vector.insert %[[EX5]], %[[IN4]] [%[[INCIDX]]] : i1 into vector<8xi1>
// CHECK: %[[EX6:.+]] = vector.extract %[[MASK]][2] : i1 from vector<3xi1>
// CHECK: %[[NEW_MASK:.+]] = vector.insert %[[EX6]], %[[IN5]] [%[[INCIDX2]]] : i1 into vector<8xi1>

// Select the effective part from the source and passthru vectors
// CHECK: %[[SELECT:.+]] = arith.select %[[NEW_MASK]], %[[BCAST_MASKLOAD]], %[[NEW_PASSTHRU]] : vector<8xi1>, vector<8xi2>

// Finally, insert the selected parts into actual passthru vector.
// CHECK: %[[EX7:.+]] = vector.extract %[[SELECT]][%[[LINEAR2]]] : i2 from vector<8xi2>
// CHECK: %[[IN7:.+]] = vector.insert %[[EX7]], %[[PTH]] [0] : i2 into vector<3xi2>
// CHECK: %[[EX8:.+]] = vector.extract %[[SELECT]][%[[INCIDX]]] : i2 from vector<8xi2>
// CHECK: %[[IN8:.+]] = vector.insert %[[EX8]], %[[IN7]] [1] : i2 into vector<3xi2>
// CHECK: %[[EX9:.+]] = vector.extract %[[SELECT]][%[[INCIDX2]]] : i2 from vector<8xi2>
// CHECK: %[[IN9:.+]] = vector.insert %[[EX9]], %[[IN8]] [2] : i2 into vector<3xi2>

// -----

func.func @vector_store_i2_const(%arg0: vector<3xi2>) {
    %0 = memref.alloc() : memref<3x3xi2>
    %c0 = arith.constant 0 : index
    %c2 = arith.constant 2 : index
    vector.store %arg0, %0[%c2, %c0] :memref<3x3xi2>, vector<3xi2>
    return
}

// in this example, emit 2 atomic stores, with the first storing 1 element and the second storing 2 elements.
// CHECK: func @vector_store_i2_const(
// CHECK-SAME: %[[ARG0:.+]]: vector<3xi2>)
// CHECK: %[[ALLOC:.+]] = memref.alloc() : memref<3xi8>
// CHECK: %[[C1:.+]] = arith.constant 1 : index

// atomic store of the first byte
// CHECK: %[[CST:.+]] = arith.constant dense<[false, false, true, true]> : vector<4xi1>
// CHECK: %[[CST0:.+]] = arith.constant dense<0> : vector<4xi2>
// CHECK: %[[EXTRACT:.+]] = vector.extract_strided_slice %[[ARG0]]
// CHECK-SAME: {offsets = [0], sizes = [2], strides = [1]} : vector<3xi2> to vector<2xi2>
// CHECK: %[[INSERT:.+]] = vector.insert_strided_slice %[[EXTRACT]], %[[CST0]]
// CHECK-SAME: {offsets = [2], strides = [1]} : vector<2xi2> into vector<4xi2>
// CHECK: %[[ATOMIC_RMW:.+]] = memref.generic_atomic_rmw %[[ALLOC]][%[[C1]]] : memref<3xi8> {
// CHECK: %[[ARG:.+]]: i8):
// CHECK: %[[FROM_ELEM:.+]] = vector.from_elements %[[ARG]] : vector<1xi8>
// CHECK: %[[BITCAST:.+]] = vector.bitcast %[[FROM_ELEM]] : vector<1xi8> to vector<4xi2>
// CHECK: %[[SELECT:.+]] = arith.select %[[CST]], %[[INSERT]], %[[BITCAST]] : vector<4xi1>, vector<4xi2>
// CHECK: %[[BITCAST2:.+]] = vector.bitcast %[[SELECT]] : vector<4xi2> to vector<1xi8>
// CHECK: %[[EXTRACT2:.+]] = vector.extract %[[BITCAST2]][0] : i8 from vector<1xi8>
// CHECK: memref.atomic_yield %[[EXTRACT2]] : i8

// atomic store of the second byte
// CHECK: %[[ADDI:.+]] = arith.addi %[[C1]], %[[C1]] : index
// CHECK: %[[EXTRACT2:.+]] = vector.extract_strided_slice %[[ARG0]]
// CHECK-SAME: {offsets = [2], sizes = [1], strides = [1]} : vector<3xi2> to vector<1xi2>
// CHECK: %[[INSERT2:.+]] = vector.insert_strided_slice %[[EXTRACT2]], %[[CST0]]
// CHECK-SAME: {offsets = [0], strides = [1]} : vector<1xi2> into vector<4xi2>
// CHECK: %[[ATOMIC_RMW2:.+]] = memref.generic_atomic_rmw %[[ALLOC]][%[[ADDI]]] : memref<3xi8> {
// CHECK: %[[ARG2:.+]]: i8):
// CHECK: %[[FROM_ELEM2:.+]] = vector.from_elements %[[ARG2]] : vector<1xi8>
// CHECK: %[[BITCAST3:.+]] = vector.bitcast %[[FROM_ELEM2]] : vector<1xi8> to vector<4xi2>
// CHECK: %[[SELECT2:.+]] = arith.select %[[CST1]], %[[INSERT2]], %[[BITCAST3]] : vector<4xi1>, vector<4xi2>
// CHECK: %[[BITCAST4:.+]] = vector.bitcast %[[SELECT2]] : vector<4xi2> to vector<1xi8>
// CHECK: %[[EXTRACT3:.+]] = vector.extract %[[BITCAST4]][0] : i8 from vector<1xi8>
// CHECK: memref.atomic_yield %[[EXTRACT3]] : i8

// -----

func.func @vector_store_i8_2(%arg0: vector<7xi2>) {
    %0 = memref.alloc() : memref<3x7xi2>
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    vector.store %arg0, %0[%c1, %c0] :memref<3x7xi2>, vector<7xi2>
    return
}

// in this example, emit 2 atomic stores and 1 non-atomic store

// CHECK: func @vector_store_i8_2(
// CHECK-SAME: %[[ARG0:.+]]: vector<7xi2>)
// CHECK: %[[ALLOC]] = memref.alloc() : memref<6xi8>
// CHECK: %[[C1:.+]] = arith.constant 1 : index
// CHECK: %[[CST:.+]] = arith.constant dense<[false, false, false, true]> : vector<4xi1>
// CHECK: %[[CST0:.+]] = arith.constant dense<0> : vector<4xi2>

// first atomic store
// CHECK: %[[EXTRACT:.+]] = vector.extract_strided_slice %[[ARG0]]
// CHECK-SAME: {offsets = [0], sizes = [1], strides = [1]} : vector<7xi2> to vector<1xi2>
// CHECK: %[[INSERT:.+]] = vector.insert_strided_slice %[[EXTRACT]], %[[CST0]]
// CHECK-SAME: {offsets = [3], strides = [1]} : vector<1xi2> into vector<4xi2>
// CHECK: %[[ATOMIC_RMW:.+]] = memref.generic_atomic_rmw %[[ALLOC]][%[[C1]]] : memref<6xi8> {
// CHECK: %[[ARG:.+]]: i8):
// CHECK: %[[FROM_ELEM:.+]] = vector.from_elements %[[ARG]] : vector<1xi8>
// CHECK: %[[BITCAST:.+]] = vector.bitcast %[[FROM_ELEM]] : vector<1xi8> to vector<4xi2>
// CHECK: %[[SELECT:.+]] = arith.select %[[CST]], %[[INSERT]], %[[BITCAST]] : vector<4xi1>, vector<4xi2>
// CHECK: %[[BITCAST2:.+]] = vector.bitcast %[[SELECT]] : vector<4xi2> to vector<1xi8>
// CHECK: %[[EXTRACT2:.+]] = vector.extract %[[BITCAST2]][0] : i8 from vector<1xi8>
// CHECK: memref.atomic_yield %[[EXTRACT2]] : i8

// non atomic store part
// CHECK: %[[ADDR:.+]] = arith.addi %[[C1]], %[[C1]] : index
// CHECK: %[[EXTRACT2:.+]] = vector.extract_strided_slice %[[ARG0]]
// CHECK-SAME: {offsets = [1], sizes = [4], strides = [1]} : vector<7xi2> to vector<4xi2>
// CHECK: %[[BITCAST3:.+]] = vector.bitcast %[[EXTRACT2]] : vector<4xi2> to vector<1xi8>
// CHECK: vector.store %[[BITCAST3]], %[[ALLOC]][%[[ADDR]]] : memref<6xi8>, vector<1xi8>

// second atomic store
// CHECK: %[[ADDR2:.+]] = arith.addi %[[ADDR]], %[[C1]] : index
// CHECK: %[[EXTRACT3:.+]] = vector.extract_strided_slice %[[ARG0]]
// CHECK-SAME: {offsets = [5], sizes = [2], strides = [1]} : vector<7xi2> to vector<2xi2>
// CHECK: %[[INSERT2:.+]] = vector.insert_strided_slice %[[EXTRACT3]], %[[CST0]]
// CHECK-SAME: {offsets = [0], strides = [1]} : vector<2xi2> into vector<4xi2>
// CHECK: %[[ATOMIC_RMW2:.+]] = memref.generic_atomic_rmw %[[ALLOC]][%[[ADDR2]]] : memref<6xi8> {
// CHECK: %[[ARG2:.+]]: i8):
// CHECK: %[[FROM_ELEM2:.+]] = vector.from_elements %[[ARG2]] : vector<1xi8>
// CHECK: %[[BITCAST4:.+]] = vector.bitcast %[[FROM_ELEM2]] : vector<1xi8> to vector<4xi2>
// CHECK: %[[SELECT2:.+]] = arith.select %[[CST1]], %[[INSERT2]], %[[BITCAST4]] :
// CHECK-SAME: vector<4xi1>, vector<4xi2>
// CHECK: %[[BITCAST5:.+]] = vector.bitcast %[[SELECT2]] : vector<4xi2> to vector<1xi8>
// CHECK: %[[EXTRACT4:.+]] = vector.extract %[[BITCAST5]][0] : i8 from vector<1xi8>
// CHECK: memref.atomic_yield %[[EXTRACT4]] : i8    

// -----

func.func @vector_store_i2_single_atomic(%arg0: vector<1xi2>) {
    %0 = memref.alloc() : memref<4x1xi2>
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    vector.store %arg0, %0[%c1, %c0] :memref<4x1xi2>, vector<1xi2>
    return
}

// in this example, only emit 1 atomic store
// CHECK: func @vector_store_i2_single_atomic(
// CHECK-SAME: %[[ARG0:.+]]: vector<1xi2>)
// CHECK: %[[ALLOC:.+]] = memref.alloc() : memref<1xi8>
// CHECK: %[[C0:.+]] = arith.constant 0 : index
// CHECK: %[[C1:.+]] = arith.constant 1 : index
// CHECK: %[[CST:.+]] = arith.constant dense<[false, true, false, false]> : vector<4xi1>
// CHECK: %[[CST0:.+]] = arith.constant dense<0> : vector<4xi2>
// CHECK: %[[INSERT:.+]] = vector.insert_strided_slice %[[ARG0]], %[[CST0]]
// CHECK-SAME: {offsets = [1], strides = [1]} : vector<1xi2> into vector<4xi2>

// CHECK: %[[ATOMIC_RMW:.+]] = memref.generic_atomic_rmw %[[ALLOC]][%[[C0]]] : memref<1xi8> {
// CHECK: %[[ARG:.+]]: i8):
// CHECK: %[[FROM_ELEM:.+]] = vector.from_elements %[[ARG]] : vector<1xi8>
// CHECK: %[[BITCAST:.+]] = vector.bitcast %[[FROM_ELEM]] : vector<1xi8> to vector<4xi2>
// CHECK: %[[SELECT:.+]] = arith.select %[[CST]], %[[INSERT]], %[[BITCAST]] : vector<4xi1>, vector<4xi2>
// CHECK: %[[BITCAST2:.+]] = vector.bitcast %[[SELECT]] : vector<4xi2> to vector<1xi8>
// CHECK: %[[EXTRACT2:.+]] = vector.extract %[[BITCAST2]][0] : i8 from vector<1xi8>
// CHECK: memref.atomic_yield %[[EXTRACT2]] : i8
