// Convert a JavaScript Number (IEEE 754 double) to its 64-bit integer representation as a string
// This is equivalent to System.BitConverter.DoubleToInt64Bits in .NET
const float64Array = new Float64Array(1);
const int32Array = new Int32Array(float64Array.buffer);

export const doubleToInt64BitsImpl = (d) => {
  float64Array[0] = d;
  // Combine the two 32-bit ints into a BigInt, then convert to string
  const lo = int32Array[0] >>> 0;  // unsigned
  const hi = int32Array[1] >>> 0;  // unsigned
  const result = BigInt(hi) * BigInt(0x100000000) + BigInt(lo);
  // Handle sign: if the high bit is set, result should be interpreted as signed
  if (hi & 0x80000000) {
    return (result - BigInt("18446744073709551616")).toString();
  }
  return result.toString();
};
