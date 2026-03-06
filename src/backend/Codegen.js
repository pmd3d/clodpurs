// Convert a JavaScript Number (IEEE 754 double) to its 64-bit integer representation as a string
// This is equivalent to System.BitConverter.DoubleToInt64Bits in .NET
const float64Array = new Float64Array(1);
const int32Array = new Int32Array(float64Array.buffer);

export const doubleToInt64BitsImpl = (d) => {
  float64Array[0] = d;
  const lo = int32Array[0] >>> 0;
  const hi = int32Array[1] >>> 0;
  const result = BigInt(hi) * BigInt(0x100000000) + BigInt(lo);
  if (hi & 0x80000000) {
    return (result - BigInt("18446744073709551616")).toString();
  }
  return result.toString();
};

// Convert a 64-bit integer (as string) back to a JavaScript Number (IEEE 754 double)
// This is equivalent to System.BitConverter.Int64BitsToDouble in .NET
export const int64BitsToDoubleImpl = (s) => {
  let n = BigInt(s);
  // Handle negative values (signed int64)
  if (n < 0n) {
    n = n + BigInt("18446744073709551616");
  }
  const lo = Number(n & BigInt(0xFFFFFFFF));
  const hi = Number((n >> BigInt(32)) & BigInt(0xFFFFFFFF));
  int32Array[0] = lo;
  int32Array[1] = hi;
  return float64Array[0];
};

// Int64.MinValue as a JavaScript Number (-(2**63) is exactly representable as a double)
export const int64MinValue = -(2**63);

// Convert BigInt to Int (JavaScript Number) without safe integer check
export const bigIntToInt = (n) => Number(n);
